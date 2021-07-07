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

LIB="./lib"
MAIN_JAR_FILE="tools-cli.jar"

CLASSPATH=$(JARS=("$LIB"/*.jar); IFS=:; echo "${JARS[*]}")
JAVA="/usr/bin/env java"

$JAVA --illegal-access=deny -Xmx4g -Dlog4j.configurationFile=log4j2.xml -cp "$CLASSPATH:./$MAIN_JAR_FILE" -jar $MAIN_JAR_FILE $*