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
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.sdk.AccountInfo;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
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
				{ "get-account-info", "-a", "50", "-p", "2", "-k", "src/test/resources/Keys/genesis.pem", "-o", "src" +
						"/test/resources/infos_temp", "-n", "integration" };

		ToolsMain.main(args);

		final var infos = new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.INFO_EXTENSION));
		final var jsons = new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.JSON_EXTENSION));

		assert infos != null;
		assertEquals(1, infos.length);
		assert jsons != null;
		assertEquals(1, jsons.length);

		final var info = AccountInfo.fromBytes(readBytes(infos[0]));
		assertEquals(50, info.accountId.num);

		final var json = readJsonObject(jsons[0]);
		assertEquals("0.0.50", json.get("accountId").getAsString());
	}

	@Test
	void happyPathStrings_test() throws Exception {
		final String[] args =
				{ "get-account-info", "-a", "0.0.50, 0.0.51", "-p", "0.0.2", "-k", "src/test/resources/Keys/genesis.pem", "-o", "src" +
						"/test/resources/infos_temp", "-n", "integration" };
		ToolsMain.main(args);

		final var infos = new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.INFO_EXTENSION));
		final var jsons = new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.JSON_EXTENSION));

		assert infos != null;
		assertEquals(2, infos.length);
		assert jsons != null;
		assertEquals(2, jsons.length);

		var info = AccountInfo.fromBytes(readBytes(infos[0]));
		assertEquals(50, info.accountId.num);
		info = AccountInfo.fromBytes(readBytes(infos[1]));
		assertEquals(51, info.accountId.num);

		var json = readJsonObject(jsons[0]);
		assertEquals("0.0.50", json.get("accountId").getAsString());
		json = readJsonObject(jsons[1]);
		assertEquals("0.0.51", json.get("accountId").getAsString());
	}

	@Test
	void badString_test() {
		final String[] args1 =
				{ "get-account-info", "-a", "0.50", "-p", "0.0.2", "-k", "src/test/resources/Keys/genesis.pem", "-o", "src" +
						"/test/resources/infos_temp", "-n", "integration" };
		final Exception exception1 = assertThrows(HederaClientRuntimeException.class, ()->ToolsMain.main(args1));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"0.50\" cannot be parsed", exception1.getMessage());

		final String[] args2 =
				{ "get-account-info", "-a", "0.0.50", "-p", "0.02", "-k", "src/test/resources/Keys/genesis.pem", "-o", "src" +
						"/test/resources/infos_temp", "-n", "integration" };
		final Exception exception2 = assertThrows(HederaClientRuntimeException.class, ()->ToolsMain.main(args2));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"0.02\" cannot be parsed", exception2.getMessage());
	}


	@Test
	void happyPathMultipleAccounts_test() throws Exception {
		final String[] args =
				{ "get-account-info", "-a", "50, 75, 98", "-p", "2", "-k", "src/test/resources/Keys/genesis.pem", "-o",
						"src/test/resources/infos_temp", "-n", "integration" };
		ToolsMain.main(args);

		final var infos = new File("src/test/resources/infos_temp").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(Constants.INFO_EXTENSION));
		assert infos != null;
		assertEquals(3, infos.length);
	}

	@Test
	void badNetwork_test() {
		final String[] args =
				{ "get-account-info", "-a", "50, 75, 98", "-p", "2", "-k", "src/test/resources/Keys/genesis.pem", "-o",
						"src/test/resources/infos_temp", "-n", "ingrate" };

		final Exception e = assertThrows(IllegalArgumentException.class, () -> ToolsMain.main(args));
		assertEquals("No enum constant com.hedera.hashgraph.client.core.enums.NetworkEnum.INGRATE", e.getMessage());
	}

	@Test
	void badKey_test() {
		final String[] args =
				{ "get-account-info", "-a", "50, 75, 98", "-p", "2", "-k", "src/test/resources/Keys/gene.pem", "-o",
						"src/test/resources/infos_temp", "-n", "integration" };
		final Exception e = assertThrows(HederaClientException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client: The file src/test/resources/Keys/gene.pem doesn't exist", e.getMessage());
	}

	@Test
	void badAccount_test() {
		final String[] args =
				{ "get-account-info", "-a", "account", "-p", "2", "-k", "src/test/resources/Keys/genesis.pem", "-o",
						"src/test/resources/infos_temp", "-n", "integration" };
		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"account\" cannot be parsed", e.getMessage());
	}

	@Test
	void badPayer_test() {
		final String[] args =
				{ "get-account-info", "-a", "50", "-p", "account", "-k", "src/test/resources/Keys/genesis.pem", "-o",
						"src/test/resources/infos_temp", "-n", "integration" };

		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"account\" cannot be parsed", e.getMessage());
	}

	@Test
	void noOutput_test() throws Exception {
		final String[] args =
				{ "get-account-info", "-a", "50", "-p", "2", "-k", "src/test/resources/Keys/genesis.pem", "-n",
						"integration" };

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
				{ "get-account-info", "-a", "50", "-p", "2", "-k", "src/test/resources/Keys/KeyStore-0.pem", "-n",
						"integration" };

		final Exception e = assertThrows(HederaClientRuntimeException.class,()-> ToolsMain.main(args));
		assertTrue(e.getMessage().contains("failed pre-check with the status `INVALID_SIGNATURE`"));
	}

	@Test
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