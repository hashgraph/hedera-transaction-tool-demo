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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.constants.JsonConstants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Disabled("Temporarily disabling tests")
class CommonMethodsTest implements GenericFileReadWriteAware {
	private final static Logger logger = LogManager.getLogger(CommonMethodsTest.class);

	@BeforeEach
	void setUp() {
		if (new File("src/test/resources/out").mkdirs()) {
			logger.info("Output directory created");
		}
		final var file = new File("recovery.aes");
		if (file.exists() && file.delete()) {
			logger.info("File deleted");
		}
	}

	@AfterEach
	void tearDown() throws IOException {
		FileUtils.deleteDirectory(new File("src/test/resources/out"));
		new File("recovery.aes").deleteOnExit();
	}

	@Test
	void getClient() {
		final var integration = CommonMethods.getClient(NetworkEnum.INTEGRATION);
		assertEquals(4, integration.getNetwork().size());
		final var mainNet = CommonMethods.getClient(NetworkEnum.MAINNET);
		assertTrue(mainNet.getNetwork().size() >= 10);
		final var testNet = CommonMethods.getClient(NetworkEnum.TESTNET);
		assertTrue(testNet.getNetwork().size() >= 4);
		final var previewNet = CommonMethods.getClient(NetworkEnum.PREVIEWNET);
		assertTrue(previewNet.getNetwork().size() >= 4);
	}


	@Test
	void longestCommonPrefix_test() {
		final List<String> words = new ArrayList<>();
		assertEquals("", CommonMethods.longestCommonPrefix(words));
		words.add("test");
		assertEquals("test", CommonMethods.longestCommonPrefix(words));
		words.add("test1");
		assertEquals("test", CommonMethods.longestCommonPrefix(words));
		words.add("tes");
		assertEquals("tes", CommonMethods.longestCommonPrefix(words));
		words.add("dog");
		assertEquals("", CommonMethods.longestCommonPrefix(words));
	}

	@Test
	void setupClient() throws HederaClientException {
		final var testInput = new JsonObject();
		final Exception exception0 =
				assertThrows(HederaClientException.class, () -> CommonMethods.setupClient(testInput));
		assertEquals("Hedera Client: Missing critical fields in the JSON input to set up the client",
				exception0.getMessage());

		testInput.addProperty(JsonConstants.NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());
		final Exception exception1 =
				assertThrows(HederaClientException.class, () -> CommonMethods.setupClient(testInput));
		assertEquals("Hedera Client: Missing critical fields in the JSON input to set up the client",
				exception1.getMessage());

		testInput.remove(JsonConstants.NETWORK_FIELD_NAME);
		testInput.addProperty(JsonConstants.TRANSACTION_FEE_FIELD_NAME, 1000);
		final Exception exception2 =
				assertThrows(HederaClientException.class, () -> CommonMethods.setupClient(testInput));
		assertEquals("Hedera Client: Missing critical fields in the JSON input to set up the client",
				exception2.getMessage());

		testInput.addProperty(JsonConstants.NETWORK_FIELD_NAME, "not_a_network");
		final Exception exception3 =
				assertThrows(IllegalArgumentException.class, () -> CommonMethods.setupClient(testInput));
		assertEquals("No enum constant com.hedera.hashgraph.client.core.enums.NetworkEnum.not_a_network",
				exception3.getMessage());

		testInput.addProperty(JsonConstants.NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());

		final var feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 700000000);
		testInput.add(JsonConstants.TRANSACTION_FEE_FIELD_NAME, feeJson);

		final var client = CommonMethods.setupClient(testInput);
		final Map<String, AccountId> network = new HashMap<>();
		network.put("34.74.191.8:50211", new AccountId(3L));
		network.put("35.245.150.69:50211", new AccountId(4L));
		network.put("34.70.193.123:50211", new AccountId(5L));
		network.put("35.197.75.89:50211", new AccountId(6L));
		assertEquals(network, client.getNetwork());

		assertEquals(new Hbar(7), client.getDefaultMaxQueryPayment());

	}

	@Test
	void setupRecoveryPhrase() throws HederaClientException {
		// create output folder if it didn't exist
		if (new File("src/test/resources/out/").mkdirs()) {
			logger.info("Output folder created");
		}

		final Exception exception0 =
				assertThrows(HederaClientException.class, () -> CommonMethods.setupRecoveryPhrase("recovery.aes"));
		assertEquals("Hedera Client: Cannot open file recovery.aes", exception0.getMessage());

		final var mnemonic0 = CommonMethods.setupRecoveryPhrase("src/test/resources/recovery.aes");
		assertNotNull(mnemonic0);

		final var mnemonic1 = CommonMethods.setupRecoveryPhrase("src/test/resources/out");
		assertNotNull(mnemonic1);
		assertTrue(new File("src/test/resources/out/recovery.aes").exists());
		final var mnemonic2 =
				SecurityUtilities.fromEncryptedFile(
						SecurityUtilities.keyFromPasswordLegacy(Constants.TEST_PASSWORD.toCharArray()),
						"src/test/resources/out/recovery.aes");
		assertEquals(mnemonic1.words, mnemonic2.words);
	}

	@Test
	void buildKeyName_test() throws IOException {

		if (new File("src/test/resources/keyStores").mkdirs()) {
			logger.info("Output folder created");
		}
		final var keyName0 = CommonMethods.buildKeyName("KeyStore", "src/test/resources/keyStores");
		assertFalse(keyName0.isEmpty());
		assertEquals("src/test/resources/keyStores/KeyStore-0.pem", keyName0);
		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pem"), new File(keyName0));


		final var keyName1 = CommonMethods.buildKeyName("KeyStore", "src/test/resources/keyStores");
		assertEquals("src/test/resources/keyStores/KeyStore-1.pem", keyName1);
		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pem"), new File(keyName1));

		final var keyName2 = CommonMethods.buildKeyName("KeyStore", "src/test/resources/keyStores");
		assertEquals("src/test/resources/keyStores/KeyStore-2.pem", keyName2);
		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pem"), new File(keyName2));

		final var keyName3 = CommonMethods.buildKeyName("KeyStore", "src/test/resources/keyStores");
		assertEquals("src/test/resources/keyStores/KeyStore-3.pem", keyName3);
		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pem"), new File(keyName3));

		final var keyName4 = CommonMethods.buildKeyName("KeyStore", "src/test/resources/keyStores");
		assertEquals("src/test/resources/keyStores/KeyStore-4.pem", keyName4);
		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pem"), new File(keyName4));

		final var keyName5 = CommonMethods.buildKeyName("KeyStore", "src/test/resources/keyStores");
		assertEquals("src/test/resources/keyStores/KeyStore-5.pem", keyName5);
		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pem"), new File(keyName5));

		FileUtils.deleteDirectory(new File("src/test/resources/keyStores"));


	}

	@Test
	void createSingleKeyAsJson_test() throws HederaClientException, IOException {
		if (new File("src/test/resources/Keys/Temp").mkdirs()) {
			logger.info("Output folder created");
		}

		final var mnemonic = CommonMethods.setupRecoveryPhrase("src/test/resources/recovery.aes");
		final var singleKeyAsJson =
				CommonMethods.createSingleKeyAsJson(mnemonic, Constants.TEST_PASSWORD.toCharArray(), 7,
						"src/test/resources/Keys/Temp");

		assertNotNull(singleKeyAsJson);
		assertEquals("2b4fb0101d1f2d405feba7187ae2fd7bba8db91741d66e0c00599df69109be28",
				singleKeyAsJson.get("Ed25519").getAsString());

		final Exception e1 = assertThrows(HederaClientException.class,
				() -> CommonMethods.createSingleKeyAsJson(mnemonic, null, 7, "src/test/resources/Keys/Temp"));
		assertEquals("Hedera Client: " + ErrorMessages.PASSWORD_CANNOT_BE_EMPTY_ERROR_MESSAGE, e1.getMessage());

		final Exception e2 = assertThrows(HederaClientException.class,
				() -> CommonMethods.createSingleKeyAsJson(null, Constants.TEST_PASSWORD.toCharArray(), 7,
						"src/test/resources/Keys/Temp"));
		assertEquals("Hedera Client: " + ErrorMessages.MNEMONIC_CANNOT_BE_NULL_ERROR_MESSAGE, e2.getMessage());

		FileUtils.deleteDirectory(new File("src/test/resources/Keys/Temp"));
	}

	@Test
	void getLCSubStr_test() {

		var lcSubstring =
				CommonMethods.getLCSubStr("src/test/resources/Keys/genesis.pem", "src/test/resources/Files/0.0.2.info");
		assertEquals("src/test/resources/", lcSubstring);

		lcSubstring =
				CommonMethods.getLCSubStr("src/test/resources/Keys/genesis.pem", "/src/test/resources/Files/0.0.2" +
						".info");
		assertEquals("src/test/resources/", lcSubstring);

		lcSubstring = CommonMethods.getLCSubStr("src/test/resources/Keys/genesis.pem", "lorem ipsum");
		assertEquals("re", lcSubstring);

		lcSubstring = CommonMethods.getLCSubStr("xyz", "ipsum");
		assertEquals("", lcSubstring);
	}

	@Test
	void splitStringDigest_test() {
		final var words =
				"Lorem ipsum dolor sit amet consectetur adipiscing elit Aliquam tincidunt sapien eu neque malesuada " +
						"ultrices Etiam ultrices venenatis ex at imperdiet nisi dapibus ac Integer sem magna " +
						"ultricies a mi vitae sodales hendrerit leo Cras a";
		var split = CommonMethods.splitStringDigest(words, 9);
		var pieces = split.split("\n");
		assertEquals(4, pieces.length);
		for (final var piece : pieces) {
			assertEquals(9, piece.split("\u00A0").length);
		}

		split = CommonMethods.splitStringDigest(words, 3);
		pieces = split.split("\n");
		assertEquals(12, pieces.length);
		for (final var piece : pieces) {
			assertEquals(3, piece.split("\u00A0").length);
		}
	}

	@Test
	void nickNameOrNumber_Test() throws HederaClientException {
		final var nicknames = readJsonObject("src/test/resources/accountMapFile.json");

		var testString = CommonMethods.nicknameOrNumber(new Identifier(0, 0, 1), nicknames);
		assertEquals("zero1 (0.0.1-dfkxr)", testString);

		testString = CommonMethods.nicknameOrNumber(new Identifier(0, 0, 2), nicknames);
		assertEquals("treasury (0.0.2-lpifi)", testString);

		testString = CommonMethods.nicknameOrNumber(new Identifier(0, 0, 39), nicknames);
		assertEquals("0.0.39-dtqcs", testString);

		testString = CommonMethods.nicknameOrNumber(new Identifier(0, 0, 390), nicknames);
		assertEquals("0.0.390-kvffm", testString);
	}

	@Test
	void checkJsonInput_test() {
		final var input = new JsonObject();
		input.addProperty("testPropInt", 42);
		input.addProperty("testPropString", "forty two");
		final var fortyTwo = new JsonObject();
		fortyTwo.addProperty("tens", 4);
		fortyTwo.addProperty("units", "two");
		input.add("testPropJson", fortyTwo);
		final List<String> props = new ArrayList<>();
		props.add("testPropInt");
		props.add("testPropMap");
		props.add("testPropString");
		props.add("testPropLong");
		props.add("testPropJson");
		final var missing = CommonMethods.checkJsonInput(input, props);
		assertEquals(2, missing.size());
		assertTrue(missing.contains("testPropMap"));
		assertTrue(missing.contains("testPropLong"));
	}

	@Test
	void verifyFieldExist_test() {
		final var input = new JsonObject();
		input.addProperty("testPropInt", 42);
		input.addProperty("testPropString", "forty two");
		final var fortyTwo = new JsonObject();
		fortyTwo.addProperty("tens", 4);
		fortyTwo.addProperty("units", "two");
		input.add("testPropJson", fortyTwo);
		assertFalse(CommonMethods.verifyFieldExist(input, "testPropInt", "testPropMap", "testPropString",
				"testPropLong", "testPropJson"));
		assertTrue(CommonMethods.verifyFieldExist(input, "testPropInt", "testPropString", "testPropJson"));
	}

	@Test
	void badPassword() {
		assertFalse(CommonMethods.badPassword("tempura test coin".toCharArray()));
		assertTrue(CommonMethods.badPassword("tempura".toCharArray()));
		assertTrue(CommonMethods.badPassword("1234567890".toCharArray()));
		assertTrue(CommonMethods.badPassword(RandomStringUtils.random(1025, true, true).toCharArray()));

	}

	@Test
	void fromString() throws HederaClientException {
		final var bar = new Hbar(1);
		assertEquals(bar, CommonMethods.fromString(bar.toString()));
		final var tinyBar = Hbar.fromTinybars(1);
		assertEquals(tinyBar, CommonMethods.fromString(tinyBar.toString()));

		final var barTinyBar = Hbar.fromTinybars(110000020);
		assertEquals(barTinyBar, CommonMethods.fromString(barTinyBar.toString()));

		assertEquals(Hbar.from(1), CommonMethods.fromString("1"));

		final Exception exception0 =
				assertThrows(NumberFormatException.class, () -> CommonMethods.fromString("not an hbar"));
		assertEquals("For input string: \"not\"", exception0.getMessage());

		final Exception exception1 = assertThrows(HederaClientException.class, () -> CommonMethods.fromString("1.1.1"));
		assertEquals("Hedera Client: Cannot parse String \"1.1.1\" to hbars", exception1.getMessage());

	}

	@Test
	void removeNickname_test() {
		assertEquals("654654.9946546.3345667-atest",
				CommonMethods.removeNickname("nickname (654654.9946546.3345667-atest)"));
		assertEquals("0.0.34-godql", CommonMethods.removeNickname("0.0.34"));
		assertEquals("0.0.34-godql", CommonMethods.removeNickname("34"));
		assertEquals("", CommonMethods.removeNickname("0.34"));
		assertEquals("", CommonMethods.removeNickname("a.b.c"));
		assertEquals("", CommonMethods.removeNickname("a.1.2"));
		assertEquals("", CommonMethods.removeNickname("1.oio.2"));

		final var id = new Identifier(78098098, 987987, 987987);
		assertEquals("78098098.987987.987987-fydsl", CommonMethods.removeNickname(id.toReadableStringAndChecksum()));
	}

	@Test
	void trimString_test() {
		final String aString =
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore " +
						"et dolore magna aliqua. Morbi tempus iaculis urna id volutpat. Ac turpis egestas sed tempus. " +
						"Id volutpat lacus laoreet non curabitur gravida arcu ac tortor. Amet mauris commodo quis " +
						"imperdiet. Neque volutpat ac tincidunt vitae semper quis lectus nulla. Aliquam purus sit " +
						"amet luctus venenatis lectus. Massa vitae tortor condimentum lacinia quis. Faucibus nisl " +
						"tincidunt eget nullam non nisi est. Vulputate eu scelerisque felis imperdiet proin fermentum" +
						" " +
						"leo. Pharetra pharetra massa massa ultricies. Scelerisque purus semper eget duis at tellus " +
						"at. Adipiscing bibendum est ultricies integer quis auctor elit.";

		assertEquals(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore ",
				CommonMethods.trimString(aString, 100));

		assertEquals(100, CommonMethods.trimString(aString, 100).getBytes(StandardCharsets.UTF_8).length);

		assertEquals("Lorem ipsum dolor sit amet", CommonMethods.trimString("Lorem ipsum dolor sit amet", 100));

	}
}