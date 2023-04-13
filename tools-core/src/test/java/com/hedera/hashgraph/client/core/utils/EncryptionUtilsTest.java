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

package com.hedera.hashgraph.client.core.utils;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.Mnemonic;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class EncryptionUtilsTest implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(EncryptionUtilsTest.class);
	Mnemonic mnemonic = Mnemonic.generate24();
	String keyName = "src/test/resources/testKey-";

	@BeforeEach
	void setUp() {
		for (var i = 0; i < 20; i++) {
			SecurityUtilities.generateAndStoreKey(String.format("%s%d.pem", keyName, i), "Hedera CLI Tool", mnemonic, i,
					Constants.TEST_PASSWORD.toCharArray());
		}

	}

	@AfterEach
	void tearDown() {
		for (var i = 0; i < 20; i++) {
			final var name = String.format("%s%d.pem", keyName, i);
			if (new File(name).exists()) {
				new File(name).deleteOnExit();
			}
			if (new File(name.replace(Constants.PK_EXTENSION, Constants.PUB_EXTENSION)).exists()) {
				new File(name.replace(Constants.PK_EXTENSION, Constants.PUB_EXTENSION)).deleteOnExit();
			}
		}
	}

	@Test
	void jsonToKeyAndKeyToJson_single_test() throws IOException {
		final var singleKeyJson = new JsonObject();
		final var pubKey = new String(Files.readAllBytes(Path.of(String.format("%s%d.pub", keyName, 0))));
		singleKeyJson.addProperty("Ed25519", pubKey);
		final var singleKey = EncryptionUtils.jsonToKey(singleKeyJson);
		final var newJson = EncryptionUtils.keyToJson(singleKey);
		assertEquals(pubKey, newJson.get("Ed25519").getAsString());
		logger.info("Finished single key test");
	}

	@Test
	void jsonToKeyAndKeyToJson_singleBadFile_test() {
		final var singleKeyJson = new JsonObject();
		singleKeyJson.addProperty("key", String.format("%s%d.pub", keyName, 20));
		final Exception e =
				assertThrows(HederaClientRuntimeException.class, () -> EncryptionUtils.jsonToKey(singleKeyJson));
		assertEquals("Hedera Client Runtime: Public key file src/test/resources/testKey-20.pub cannot be found",
				e.getMessage());

		logger.info("Finished single key test");
	}

	@Test
	void jsonToKeyAndKeyToJson_KeyList_test() throws IOException {
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
		final var keyList = EncryptionUtils.jsonToKey(keyListJson);

//		for (final var key : keyList) {
//			assertEquals(1, ((KeyList) key).size());
//		}

		final var newJson = EncryptionUtils.keyToJson(keyList);
		assertTrue(newJson.has("keyList"));
		final var newListJson = newJson.getAsJsonArray("keyList");
		assertEquals(10, newListJson.size());

		for (final var element : newListJson) {
			assertTrue(element.isJsonObject());
			final var jsonObject = element.getAsJsonObject();
			assertTrue(jsonObject.has("Ed25519"));
			assertEquals(1, jsonObject.entrySet().size());
		}

		assertEquals(jsonArray, newListJson);

		logger.info("Finished key list test");
	}

	@Test
	void jsonToKeyAndKeyToJson_thresholdKey_test() throws IOException {
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
		final var keyList = EncryptionUtils.jsonToKey(thresholdKeyJson);

//		assertEquals(10, keyList.size());

		final var newJson = EncryptionUtils.keyToJson(keyList);
		assertTrue(newJson.has("thresholdKey"));
		final var newThresholdKeyJson = newJson.get("thresholdKey").getAsJsonObject();
		assertTrue(newThresholdKeyJson.has("threshold"));
		assertTrue(newThresholdKeyJson.has("keyList"));
		assertEquals(8, newThresholdKeyJson.get("threshold").getAsInt());

		final var newListJson = newThresholdKeyJson.getAsJsonArray("keyList");
		assertEquals(10, newListJson.size());

		for (final var element : newListJson) {
			assertTrue(element.isJsonObject());
			final var jsonObject = element.getAsJsonObject();
			assertTrue(jsonObject.has("Ed25519"));
			assertEquals(1, jsonObject.entrySet().size());
		}

		assertEquals(jsonArray, newListJson);

		logger.info("Finished key list test");
	}

	@Test
	void jsonToKeyAndKeyToJson_complexKey_test() throws IOException {
		final var complexKeyJson = complexKeyFromBytes(0);

		final var complexKey = EncryptionUtils.jsonToKey(complexKeyJson);

//		assertEquals(3, complexKey.size());
//
//		for (final var key : complexKey) {
//			if (((KeyList) key).size() > 1 && ((KeyList) key).getThreshold() == null) {
//				assertEquals(5, ((KeyList) key).size());
//			} else if (((KeyList) key).size() > 1 && ((KeyList) key).getThreshold() != null) {
//				assertEquals(4, ((KeyList) key).size());
//				assertEquals(2, ((KeyList) key).getThreshold());
//			} else {
//				assertTrue(key.toString().contains(
//						new String(Files.readAllBytes(Path.of(String.format("%s0.pub", keyName))))));
//			}
//		}

		assertEquals(complexKeyJson, EncryptionUtils.keyToJson(complexKey));
		logger.info("Finished complex key test");
	}

	@Test
	void jsonToKeyAndKeyToJson_complexKeyFromFiles_test() throws IOException {
		final var complexKeyJson = complexKeyFromFiles();
		final var complexKey = EncryptionUtils.jsonToKey(complexKeyJson);
//		assertEquals(3, complexKey.size());
//
//		for (final var key : complexKey) {
//			if (((KeyList) key).size() > 1 && ((KeyList) key).getThreshold() == null) {
//				assertEquals(5, ((KeyList) key).size());
//			} else if (((KeyList) key).size() > 1 && ((KeyList) key).getThreshold() != null) {
//				assertEquals(4, ((KeyList) key).size());
//				assertEquals(2, ((KeyList) key).getThreshold());
//			} else {
//				assertTrue(key.toString().contains(
//						new String(Files.readAllBytes(Path.of(String.format("%s0.pub", keyName))))));
//			}
//		}

		assertEquals(complexKeyFromBytes(0), EncryptionUtils.keyToJson(complexKey));
		logger.info("Finished complex key test");
	}

	@Test
	void keyConversionExceptions_test() throws IOException {
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

		thresholdKeyJson.add("thresholdKey", thresholdJson);
		final Exception exception0 =
				assertThrows(HederaClientRuntimeException.class, () -> EncryptionUtils.jsonToKey(thresholdKeyJson));
		assertEquals("Hedera Client Runtime: Missing keyList in threshold key", exception0.getMessage());

		thresholdJson.remove("threshold");
		thresholdJson.add("keyList", jsonArray);
		thresholdKeyJson.add("thresholdKey", thresholdJson);
		final Exception exception1 =
				assertThrows(HederaClientRuntimeException.class, () -> EncryptionUtils.jsonToKey(thresholdKeyJson));
		assertEquals("Hedera Client Runtime: Missing threshold in threshold key", exception1.getMessage());

		thresholdJson.addProperty("threshold", 15);
		thresholdKeyJson.add("thresholdKey", thresholdJson);
		final Exception exception2 =
				assertThrows(HederaClientRuntimeException.class, () -> EncryptionUtils.jsonToKey(thresholdKeyJson));
		assertEquals("Hedera Client Runtime: Threshold cannot be larger than the number of keys",
				exception2.getMessage());
	}

	@Test
	void getChecksum_test() {
		var checksum = EncryptionUtils.getChecksum("src/test/resources/Files/0.0.2.info");
		assertEquals("3cadadbbc958bfefcf101c40fddef0ca20756f2c4933500d4df9490fee9a173ef7709f4bac10ab2575b5fce99adf5d7d",
				checksum);
		checksum = EncryptionUtils.getChecksum("src/test/resources/Files/0.0.3.info");
		assertEquals(ErrorMessages.COULD_NOT_CALCULATE_HASH_OF_THE_FILE, checksum);

	}

	@Test
	void getFileDigest_test() {
		final var digest = EncryptionUtils.getFileDigest(new File("src/test/resources/Files/0.0.2.info")).split("[ ]");
		assertEquals(24, digest.length);
		for (final var s : digest) {
			assertEquals(4, s.length());
		}

		final var digest2 = EncryptionUtils.getFileDigest(new File("src/test/resources/Files/0.0.3.info"));
		assertEquals("", digest2);
	}

	@Test
	void flatPublicKeys_test() throws IOException {
		final var complexKeyJson0 = complexKeyFromBytes(0);
		final var complexKey0 = EncryptionUtils.jsonToKey(complexKeyJson0);

		final var flat0 = EncryptionUtils.flatPubKeys(Collections.singletonList(complexKey0));
		assertNotNull(flat0);
		assertEquals(10, flat0.size());

		final List<Key> keyLists = new ArrayList<>();
		keyLists.add(complexKey0);

		final var complexKeyJson3 = complexKeyFromBytes(3);
		final var complexKey3 = EncryptionUtils.jsonToKey(complexKeyJson3);

		keyLists.add(complexKey3);
		final var flat2 = EncryptionUtils.flatPubKeys(keyLists);
		assertNotNull(flat0);
		assertEquals(13, flat2.size());


		final var complexKeyJson7 = complexKeyFromBytes(7);
		final var complexKey7 = EncryptionUtils.jsonToKey(complexKeyJson7);

		keyLists.add(complexKey7);
		final var flat3 = EncryptionUtils.flatPubKeys(keyLists);
		assertNotNull(flat0);
		assertEquals(17, flat3.size());
	}

	private JsonObject complexKeyFromBytes(final int start) throws IOException {
		final var complexKeyJson = new JsonObject();
		final var jsonArray0 = new JsonArray();

		// A single Key
		final var pubKey0 = new String(Files.readAllBytes(Path.of(String.format("%s%d.pub", keyName, start))));
		var singleKeyJson = new JsonObject();
		singleKeyJson.addProperty("Ed25519", pubKey0);
		jsonArray0.add(singleKeyJson);

		// A KeyList
		final var keyListJson = new JsonObject();
		final var jsonArray1 = new JsonArray();
		for (var i = 1; i < 6; i++) {
			final var name = String.format("%s%d.pub", keyName, i + start);
			final var pubKey = new String(Files.readAllBytes(Path.of(name)));
			singleKeyJson = new JsonObject();
			singleKeyJson.addProperty("Ed25519", pubKey);
			jsonArray1.add(singleKeyJson);
		}
		keyListJson.add("keyList", jsonArray1);
		jsonArray0.add(keyListJson);

		// A threshold key
		final var thresholdKeyJson = new JsonObject();
		final var jsonArray2 = new JsonArray();
		for (var i = 6; i < 10; i++) {
			final var name = String.format("%s%d.pub", keyName, i + start);
			final var pubKey = new String(Files.readAllBytes(Path.of(name)));
			singleKeyJson = new JsonObject();
			singleKeyJson.addProperty("Ed25519", pubKey);
			jsonArray2.add(singleKeyJson);
		}
		thresholdKeyJson.add("keyList", jsonArray2);
		thresholdKeyJson.addProperty("threshold", 2);
		final var jsonObject = new JsonObject();
		jsonObject.add("thresholdKey", thresholdKeyJson);
		jsonArray0.add(jsonObject);

		complexKeyJson.add("keyList", jsonArray0);
		return complexKeyJson;
	}

	private JsonObject complexKeyFromFiles() {
		final var complexKeyJson = new JsonObject();
		final var jsonArray0 = new JsonArray();

		// A single Key
		final var pubKey0 = String.format("%s0.pub", keyName);
		var singleKeyJson = new JsonObject();
		singleKeyJson.addProperty("key", pubKey0);
		jsonArray0.add(singleKeyJson);

		// A KeyList
		final var keyListJson = new JsonObject();
		final var jsonArray1 = new JsonArray();
		for (var i = 1; i < 6; i++) {
			final var name = String.format("%s%d.pub", keyName, i);
			singleKeyJson = new JsonObject();
			singleKeyJson.addProperty("key", name);
			jsonArray1.add(singleKeyJson);
		}
		keyListJson.add("keyList", jsonArray1);
		jsonArray0.add(keyListJson);

		// A threshold key
		final var thresholdKeyJson = new JsonObject();
		final var jsonArray2 = new JsonArray();
		for (var i = 6; i < 10; i++) {
			final var name = String.format("%s%d.pub", keyName, i);
			singleKeyJson = new JsonObject();
			singleKeyJson.addProperty("key", name);
			jsonArray2.add(singleKeyJson);
		}
		thresholdKeyJson.add("keyList", jsonArray2);
		thresholdKeyJson.addProperty("threshold", 2);
		final var jsonObject = new JsonObject();
		jsonObject.add("thresholdKey", thresholdKeyJson);
		jsonArray0.add(jsonObject);

		complexKeyJson.add("keyList", jsonArray0);
		return complexKeyJson;
	}


}
