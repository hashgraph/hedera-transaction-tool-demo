#!/usr/bin/env bash

#
# Hedera Transaction Tool
#
# Copyright (C) 2018 - 2021 Hedera Hashgraph, LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

export PACKAGE_DIR="tools-ui/target/package/macosx"
export JAVA_HOME
source ".env"

if ! command -v realpath > /dev/null 2>&1; then
  if command -v brew > /dev/null 2>&1; then
    brew install coreutils || exit $?
  else
    echo "[FATAL] The realpath command is not available and unable to install the necessary coreutils package via Homebrew!"
    exit 72
  fi
fi

if [[ -z "${JAVA_HOME}" ]]; then
  export JAVA_HOME="$(realpath "$(dirname "$(realpath "$(command -v java)")")/..")"
fi

JAR="${JAVA_HOME}/bin/jar"
MVN="./mvnw"

if [[ "${CI}" != true && "${CIRCLECI}" != true ]]; then
  $MVN clean install -DskipTests -Djavafx.platform=mac
fi

if [[ -d "${PACKAGE_DIR}" ]]; then
  rm -rf "${PACKAGE_DIR}" || echo "Failed to delete existing package directory: ${PACKAGE_DIR}"
fi

mkdir -p "${PACKAGE_DIR}" && echo "Using package directory: ${PACKAGE_DIR}"
pushd "${PACKAGE_DIR}" > /dev/null 2>&1 || echo "Failed to change directory: ${PACKAGE_DIR} "

file="$(basename "$(realpath ../../tools-ui-*.zip)")"
echo "Located release package archive: $file"

cp -f ../../"${file}" ./ || exit 1
echo "Copied release archive '${file}' to '$(pwd)'"

echo "Exploding zip file: ${file}"
$JAR -xvf "$file" ./

extracted_folder=${file%.zip}
echo "Using unpacked release at: ${extracted_folder}"
cd "${extracted_folder}" || exit 1

version=$(echo "${extracted_folder}" | perl -e 'chop ($ver = <STDIN>); if ($ver =~ /tools-ui-(([0-9]+\.[0-9]+\.[0-9]+){1}(-[A-Za-z0-9.]+)?){1}/i) { print "$1"; } else { print "ERROR"; }' | sed 's/^[0.]*//')
echo "Determined package version: ${version}"

mv TransactionTools.icns ../TransactionTools-volume.icns
#mv logo.png ../TransactionTools-background.png
mv resources/TransactionTools-dmg-setup.scpt  ../TransactionTools-dmg-setup.scpt


RESOURCE_DIR="../../../../../tools-ui/installation-resources"
LICENSE_FILE="../../../../../tools-ui/src/main/resources/license.txt"

if [[ "${CI}" == true || "${CIRCLECI}" == true ]]; then
  RESOURCE_DIR="../../../../../tools-ui/installation-resources"
  LICENSE_FILE="../../../../../tools-ui/src/main/resources/license.txt"
fi

codesign -vvv --options runtime --deep --force --sign "Developer ID Application: $SIGNING_KEY_USER_NAME" transactiontools.jar

TOOL_NAME="TransactionTools"
jpackage \
      --type pkg \
      --input ./ \
      --name ${TOOL_NAME} \
      --main-jar transactiontools.jar \
      --main-class com.hedera.hashgraph.client.ui.Main \
      --icon ../TransactionTools-volume.icns \
      --app-version ${version} \
      --vendor "Hedera Hashgraph LLC." \
      --dest ../../../../../Release \
      --module-path $JAVA_HOME/../javafx-jmods-20.0.1 \
      --add-modules javafx.controls,javafx.fxml,javafx.web,javafx.swing,javafx.media \
      --license-file ${LICENSE_FILE} \
      --verbose \
      --resource-dir ${RESOURCE_DIR} \
      --mac-sign \
      --mac-signing-key-user-name "$SIGNING_KEY_USER_NAME"

popd > /dev/null 2>&1 || echo "Failed to revert to previous directory path"

cd Release

xcrun notarytool submit "${TOOL_NAME}-${version}.pkg" --wait --keychain-profile $KEYCHAIN_PROFILE
xcrun stapler staple "${TOOL_NAME}-${version}.pkg"

echo "Build complete."