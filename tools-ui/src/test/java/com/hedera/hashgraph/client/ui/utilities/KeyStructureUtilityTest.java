/*
 * Hedera Transaction Tool
 *
 * Copyright (C) 2018 - 2021 Hedera Hashgraph, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hedera.hashgraph.client.ui.utilities;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.Mnemonic;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static junit.framework.TestCase.assertEquals;

public class KeyStructureUtilityTest {

	private static final Logger logger = LogManager.getLogger(KeyStructureUtilityTest.class);

	private static final String keyName = "src/test/resources/TempKeys/testKey-";
	private static KeyStructureUtility utility;

	@BeforeAll
	public static void beforeClass() {
		final var mnemonic = Mnemonic.generate24();
		if (new File("src/test/resources/TempKeys/").mkdirs()) {
			logger.info("Temp folder created");
		}

		for (var i = 0; i < 20; i++) {
			SecurityUtilities.generateAndStoreKey(String.format("%s%d.pem", keyName, i), "Hedera UI Tool", mnemonic, i,
					Constants.TEST_PASSWORD.toCharArray());
		}

		final Map<String, Path> pubFiles = new HashMap<>();
		final var keysFolder = new File("src/test/resources/TempKeys");
		if (!keysFolder.exists()) {
			return;
		}
		try {
			final var publicKeys = keysFolder.listFiles((dir, name) -> name.endsWith(Constants.PUB_EXTENSION));
			for (final var publicKey : Objects.requireNonNull(publicKeys)) {
				final var path = publicKey.toPath();
				pubFiles.put(new String(Files.readAllBytes(path)), path);
			}
		} catch (final Exception ex) {
			logger.error(ex);
		}
		utility = new KeyStructureUtility(pubFiles);

	}

	@AfterAll
	public static void afterClass() throws Exception {
		FileUtils.deleteDirectory(new File("src/test/resources/TempKeys/"));
	}

	@Test
	public void replaceAvailableHexfromKey_single() throws IOException {
		final var singleKeyJson = new JsonObject();
		final var pubKey = new String(Files.readAllBytes(Path.of(String.format("%s%d.pub", keyName, 0))));
		singleKeyJson.addProperty("Ed25519", pubKey);
		final var singleKey = EncryptionUtils.jsonToKey(singleKeyJson);
		final var newJson = EncryptionUtils.keyToJson(singleKey);

		final var cleanJson = utility.replaceAvailableHexfromKey(newJson).toString();
		assertEquals("{\"Ed25519\":\"testKey-0\"}", cleanJson);

	}

	@Test
	public void replaceAvailableHexfromKey_KeyList() throws IOException {
		final var keyListJson = new JsonObject();
		final var jsonArray = new JsonArray();
		for (var i = 0; i < 10; i++) {
			final var name = String.format("%s%d.pub", keyName, i);
			final var pubKey = new String(Files.readAllBytes(Path.of(name)));
			final var singleKeyJson = new JsonObject();
			singleKeyJson.addProperty("Ed25519", pubKey);
			jsonArray.add(singleKeyJson);
		}
		keyListJson.add("keyList", jsonArray);

		final var cleanJson = utility.replaceAvailableHexfromKey(keyListJson).toString();
		assertEquals("{\"keyList\":[{\"Ed25519\":\"testKey-0\"},{\"Ed25519\":\"testKey-1\"}," +
				"{\"Ed25519\":\"testKey-2\"},{\"Ed25519\":\"testKey-3\"},{\"Ed25519\":\"testKey-4\"}," +
				"{\"Ed25519\":\"testKey-5\"},{\"Ed25519\":\"testKey-6\"},{\"Ed25519\":\"testKey-7\"}," +
				"{\"Ed25519\":\"testKey-8\"},{\"Ed25519\":\"testKey-9\"}]}", cleanJson);
	}

	@Test
	public void replaceAvailableHexfromKey_ThresholdKey() throws IOException {
		final var thresholdKeyJson = new JsonObject();

		final var thresholdJson = new JsonObject();
		thresholdJson.addProperty("threshold", 8);
		final var jsonArray = new JsonArray();
		for (var i = 0; i < 10; i++) {
			final var name = String.format("%s%d.pub", keyName, i);
			final var pubKey = new String(Files.readAllBytes(Path.of(name)));
			final var singleKeyJson = new JsonObject();
			singleKeyJson.addProperty("Ed25519", pubKey);
			jsonArray.add(singleKeyJson);
		}
		thresholdJson.add("keyList", jsonArray);
		thresholdKeyJson.add("thresholdKey", thresholdJson);

		final var cleanJson = utility.replaceAvailableHexfromKey(thresholdKeyJson).toString();
		assertEquals("{\"threshold\":8,\"keyList\":{\"keyList\":[{\"Ed25519\":\"testKey-0\"}," +
				"{\"Ed25519\":\"testKey-1\"},{\"Ed25519\":\"testKey-2\"},{\"Ed25519\":\"testKey-3\"}," +
				"{\"Ed25519\":\"testKey-4\"},{\"Ed25519\":\"testKey-5\"},{\"Ed25519\":\"testKey-6\"}," +
				"{\"Ed25519\":\"testKey-7\"},{\"Ed25519\":\"testKey-8\"},{\"Ed25519\":\"testKey-9\"}]}}", cleanJson);

	}
}
