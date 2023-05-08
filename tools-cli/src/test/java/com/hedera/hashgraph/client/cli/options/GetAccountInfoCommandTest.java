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

package com.hedera.hashgraph.client.cli.options;

import com.hedera.hashgraph.client.cli.ToolsMain;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.Ed25519PrivateKey;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import io.github.cdimascio.dotenv.Dotenv;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStoreException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BooleanSupplier;

import static java.lang.Boolean.parseBoolean;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GetAccountInfoCommandTest implements GenericFileReadWriteAware {

	private final static Logger logger = LogManager.getLogger(GetAccountInfoCommandTest.class);
	public static BooleanSupplier isInCircleCi = () ->
			parseBoolean(Optional.ofNullable(System.getenv("IN_CIRCLE_CI")).orElse("false"));
	public static final String RESOURCES_DIRECTORY =
			((isInCircleCi.getAsBoolean()) ? "/repo/tools-cli/" : "") + "src/test/resources/";

	private static AccountId myAccountId;
	private static File tempKey;

	@BeforeAll
	static void beforeAll() throws KeyStoreException {
		var dotenv = Dotenv.configure().directory("../").load();
		myAccountId = AccountId.fromString(dotenv.get("TEST_ACCOUNT_ID"));
		final var privateKey = dotenv.get("TEST_PRIVATE_KEY");
		final var myPrivateKey = Ed25519PrivateKey.fromBytes(Hex.decode(privateKey.startsWith("0x") ? privateKey.substring(2) : privateKey));
		final var keyStore = new Ed25519KeyStore.Builder().withPassword(Constants.TEST_PASSWORD.toCharArray()).build();
		keyStore.insertNewKeyPair(myPrivateKey);
		tempKey = new File("tempKey.pem");
		keyStore.write(tempKey);
	}

	@AfterAll
	static void afterAll() throws IOException {
		Files.deleteIfExists(tempKey.toPath());
	}

	@BeforeEach
	void setUp() {
		if (new File(RESOURCES_DIRECTORY).mkdirs()) {
			logger.info("Resources directory created");
		}

	}

	@AfterEach
	void tearDown() throws IOException {
		if (new File("src/test/resources/infos_temp").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/infos_temp"));
		}

		if (new File(System.getProperty("user.dir") + "/user.properties").exists()) {
			new File(System.getProperty("user.dir") + "/user.properties").deleteOnExit();
		}

		Files.deleteIfExists(Path.of(System.getProperty("user.dir"), "0.0.50.info"));
		Files.deleteIfExists(Path.of(System.getProperty("user.dir"), "0.0.50.json"));
	}


	@Test
	void happyPath_test() throws Exception {
		final String[] args =
				{ "get-account-info", "-a", "50", "-p", myAccountId.toString(), "-k", tempKey.getPath(), "-o", "src" +
						"/test/resources/infos_temp", "-n", NetworkEnum.TESTNET.getName() };

		ToolsMain.main(args);

		final var infos = Objects.requireNonNull(new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.INFO_EXTENSION)),
				"src/test/resources/infos_temp directory does not exist.");
		final var jsons = Objects.requireNonNull(new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.JSON_EXTENSION)),
				"src/test/resources/infos_temp directory does not exist.");

		Arrays.sort(infos, Comparator.comparing(File::getName));
		Arrays.sort(jsons, Comparator.comparing(File::getName));

		assertEquals(1, infos.length);
		assertEquals(1, jsons.length);

		final var info = AccountInfo.fromBytes(readBytes(infos[0]));
		assertEquals(50, info.accountId.num);

		final var accountId = readJsonObject(jsons[0]).get("accountID").getAsJsonObject();
		assertEquals("0.0.50", accountId.get("realmNum") +
				"." + accountId.get("shardNum") +
				"." + accountId.get("accountNum"));
	}

	@Test
	void happyPathStrings_test() throws Exception {
		final String[] args =
				{ "get-account-info", "-a", "0.0.50, 0.0.51", "-p", myAccountId.toString(), "-k", tempKey.getPath(), "-o", "src" +
						"/test/resources/infos_temp", "-n", NetworkEnum.TESTNET.getName() };
		ToolsMain.main(args);

		final var infos = Objects.requireNonNull(new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.INFO_EXTENSION)),
				"src/test/resources/infos_temp directory does not exist.");
		final var jsons = Objects.requireNonNull(new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.JSON_EXTENSION)),
				"src/test/resources/infos_temp directory does not exist.");

		Arrays.sort(infos, Comparator.comparing(File::getName));
		Arrays.sort(jsons, Comparator.comparing(File::getName));

		assertEquals(2, infos.length);
		assertEquals(2, jsons.length);

		var info = AccountInfo.fromBytes(readBytes(infos[0]));
		assertEquals(50, info.accountId.num);
		info = AccountInfo.fromBytes(readBytes(infos[1]));
		assertEquals(51, info.accountId.num);

		var json = readJsonObject(jsons[0]);
		var accountId = json.get("accountID").getAsJsonObject();
		assertEquals("0.0.50", accountId.get("realmNum") +
				"." + accountId.get("shardNum") +
				"." + accountId.get("accountNum"));
		json = readJsonObject(jsons[1]);
		accountId = json.get("accountID").getAsJsonObject();
		assertEquals("0.0.51", accountId.get("realmNum") +
				"." + accountId.get("shardNum") +
				"." + accountId.get("accountNum"));
	}

	@Test
	void badString_test() {
		final String[] args1 =
				{ "get-account-info", "-a", "0.50", "-p", myAccountId.toString(), "-k", tempKey.getPath(), "-o",
						"src" +
						"/test/resources/infos_temp", "-n", NetworkEnum.TESTNET.getName() };
		final Exception exception1 = assertThrows(HederaClientRuntimeException.class, () -> ToolsMain.main(args1));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"0.50\" cannot be parsed",
				exception1.getMessage());

		final String[] args2 =
				{ "get-account-info", "-a", "0.0.50", "-p", "0.02", "-k", tempKey.getPath(), "-o",
						"src" +
						"/test/resources/infos_temp", "-n", NetworkEnum.TESTNET.getName() };
		final Exception exception2 = assertThrows(HederaClientRuntimeException.class, () -> ToolsMain.main(args2));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"0.02\" cannot be parsed",
				exception2.getMessage());
	}


	@Test
	void happyPathMultipleAccounts_test() throws Exception {
		final String[] args =
				{ "get-account-info", "-a", "50, 75, 98", "-p", myAccountId.toString(), "-k", tempKey.getPath(), "-o",
						"src/test/resources/infos_temp", "-n", NetworkEnum.TESTNET.getName() };
		ToolsMain.main(args);

		final var infos = new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.INFO_EXTENSION));
		assert infos != null;
		assertEquals(3, infos.length);
	}

	@Test
	void badNetwork_test() {
		final String[] args =
				{ "get-account-info", "-a", "50, 75, 98", "-p", myAccountId.toString(), "-k", tempKey.getPath(), "-o",
						"src/test/resources/infos_temp", "-n", "INGRATE" };

		final Exception e = assertThrows(IllegalArgumentException.class, () -> ToolsMain.main(args));
		assertEquals("No enum constant com.hedera.hashgraph.client.core.enums.NetworkEnum.INGRATE", e.getMessage());
	}

	@Test
	void badKey_test() {
		final String[] args =
				{ "get-account-info", "-a", "50, 75, 98", "-p", myAccountId.toString(), "-k",
						"src/test/resources/Keys/ttt.pem", "-o",
						"src/test/resources/infos_temp", "-n", NetworkEnum.TESTNET.getName() };
		final Exception e = assertThrows(HederaClientException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client: The file src/test/resources/Keys/ttt.pem doesn't exist", e.getMessage());
	}

	@Test
	void badAccount_test() {
		final String[] args =
				{ "get-account-info", "-a", "account", "-p", myAccountId.toString(), "-k", tempKey.getPath(), "-o",
						"src/test/resources/infos_temp", "-n", NetworkEnum.TESTNET.getName() };
		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"account\" cannot be parsed", e.getMessage());
	}

	@Test
	void badPayer_test() {
		final String[] args =
				{ "get-account-info", "-a", "50", "-p", "account", "-k", tempKey.getPath(), "-o",
						"src/test/resources/infos_temp", "-n", NetworkEnum.TESTNET.getName() };

		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"account\" cannot be parsed", e.getMessage());
	}

	@Test
	void noOutput_test() throws Exception {
		final String[] args =
				{ "get-account-info", "-a", "50", "-p", myAccountId.toString(), "-k", tempKey.getPath(), "-n",
						NetworkEnum.TESTNET.getName() };

		ToolsMain.main(args);
		final var infoFile = new File(System.getProperty("user.dir"), "0.0.50.info");
		assertTrue(infoFile.exists());
		final var jsonFile = new File(System.getProperty("user.dir"), "0.0.50.json");
		assertTrue(jsonFile.exists());
		Files.deleteIfExists(infoFile.toPath());
		Files.deleteIfExists(jsonFile.toPath());
	}

	@Test
	void preCheckFailure_test() {
		final String[] args =
				{ "get-account-info", "-a", "50", "-p", myAccountId.toString(),
						"-k", "src/test/resources/Keys/KeyStore-0.pem",
						"-n", NetworkEnum.TESTNET.getName() };

		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> ToolsMain.main(args));
		assertTrue(e.getMessage().contains("failed pre-check with the status `INVALID_SIGNATURE`"));
	}

	@Test
	@Disabled("Custom network nodes not working, need to look into this more.")
	void customNetwork_test() throws Exception {
		final String[] args =
				{ "get-account-info", "-a", "50", "-p", "2", "-k", "src/test/resources/Keys/genesis.pem", "-n",
						"src/test/resources/testNetwork.json" };

		ToolsMain.main(args);
		final var infoFile = new File(System.getProperty("user.dir"), "0.0.50.info");
		assertTrue(infoFile.exists());
		final var jsonFile = new File(System.getProperty("user.dir"), "0.0.50.json");
		assertTrue(jsonFile.exists());
		Files.deleteIfExists(infoFile.toPath());
		Files.deleteIfExists(jsonFile.toPath());
	}

}
