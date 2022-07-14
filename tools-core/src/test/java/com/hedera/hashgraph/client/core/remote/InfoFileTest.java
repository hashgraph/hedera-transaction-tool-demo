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

package com.hedera.hashgraph.client.core.remote;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrivateKey;
import javafx.scene.control.Label;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.core.utils.EncryptionUtils.publicKeyFromFile;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class InfoFileTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(InfoFileTest.class);

	@Before
	public void setUp() throws Exception {
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
		FileUtils.copyFile(new File("src/test/resources/Files/0.0.2.meta"),
				new File(DEFAULT_HISTORY, "0.0.2.meta"));

		if (new File(Constants.DEFAULT_ACCOUNTS).mkdirs()) {
			logger.info("Accounts folder created");
		}
		Files.deleteIfExists(new File(DEFAULT_ACCOUNTS, "0.0.2.info").toPath());
	}

	@Test
	public void constructor_test() throws IOException, HederaClientException {
		final var file = new File("src/test/resources/Files/0.0.2.info");
		final var info = FileDetails.parse(file);

		final var infoFile = new InfoFile(info);
		assertNotNull(infoFile);
		assertTrue(infoFile.isValid());
		assertEquals("0.0.2", infoFile.getAccountID().toReadableString());
		assertEquals("0.0.2", infoFile.getBaseName());
		assertEquals(FilenameUtils.getFullPath(file.getAbsolutePath()), infoFile.getParentPath() + File.separator);
		assertEquals(28, EncryptionUtils.flatPubKeys(Collections.singletonList(infoFile.getKey())).size());
		assertTrue(infoFile.getActions().contains(FileActions.ACCEPT));
		assertTrue(infoFile.getActions().contains(FileActions.DECLINE));

		final var badFile = new File("src/test/resources/Files/0.0.2.meta");
		final var badInfo = FileDetails.parse(badFile);
		assertFalse(new InfoFile(badInfo).isValid());

	}

	@Test
	public void buildGridPane_test() throws IOException, HederaClientException {
		final var file = new File("src/test/resources/Files/0.0.2.info");
		final var info = FileDetails.parse(file);

		var infoFile = new InfoFile(info);
		var gridPane = infoFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		var label = (Label) gridPane.getChildren().get(0);
		assertEquals("Would you like to import information regarding account 0.0.2 to your records?", label.getText());

		FileUtils.copyFile(new File("src/test/resources/Files/0.0.2.info"),
				new File(DEFAULT_ACCOUNTS, "0.0.2.info"));

		infoFile = new InfoFile(info);
		gridPane = infoFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		label = (Label) gridPane.getChildren().get(0);
		assertEquals(
				"We have found new information regarding account 0.0.2. Would you like to import it to your records?",
				label.getText());

		assertTrue(infoFile.duplicate());
		assertTrue(infoFile.isExpired());

		infoFile.setHistory(true);
		gridPane = infoFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		label = (Label) gridPane.getChildren().get(0);
		assertTrue(label.getText().contains("Information regarding account 0.0.2.info was accepted on 2021-01-07 "));

	}

	@After
	public void tearDown() throws Exception {
		Files.delete(new File(DEFAULT_HISTORY + File.separator + "0.0.2.meta").toPath());
	}

	@Test
	public void canSignThreshold_test() throws Exception {
		final var infoFile = createAccountInfo("src/test/resources/KeyFiles/jsonKeySimpleThreshold.json");

		final var file = new InfoFile(FileDetails.parse(new File(infoFile)));

		final var keyFiles =
				new File("src/test/resources/KeyFiles").listFiles((dir, name) -> name.endsWith(PUB_EXTENSION));
		assert keyFiles != null;
		final var keys = Arrays.stream(keyFiles).map(
				keyFile -> publicKeyFromFile(keyFile.getAbsolutePath())).collect(Collectors.toSet());

		assertTrue(file.canSign(keys));

		final var smallSet = ImmutableSet.copyOf(Iterables.limit(keys, 5));

		assertFalse(file.canSign(smallSet));

		Files.deleteIfExists(Path.of(infoFile));
		logger.info("Deleted {}", infoFile);
	}

	@Test
	public void canSignList_test() throws Exception {
		final var infoFile = createAccountInfo("src/test/resources/KeyFiles/jsonKeyList.json");

		final var file = new InfoFile(FileDetails.parse(new File(infoFile)));

		final var keyFiles =
				new File("src/test/resources/KeyFiles").listFiles((dir, name) -> name.endsWith(PUB_EXTENSION));
		assert keyFiles != null;
		final var keys = Arrays.stream(keyFiles).map(
				keyFile -> publicKeyFromFile(keyFile.getAbsolutePath())).collect(Collectors.toSet());

		assertTrue(file.canSign(keys));

		final var smallSet = ImmutableSet.copyOf(Iterables.limit(keys, 5));

		assertFalse(file.canSign(smallSet));

		Files.deleteIfExists(Path.of(infoFile));
		logger.info("Deleted {}", infoFile);
	}

	@Test
	public void canSignSingle_test() throws Exception {
		final var infoFile = createAccountInfo("src/test/resources/KeyFiles/jsonKeySingle.json");

		final var file = new InfoFile(FileDetails.parse(new File(infoFile)));

		final var keyFiles =
				new File("src/test/resources/KeyFiles").listFiles((dir, name) -> name.endsWith(PUB_EXTENSION));
		assert keyFiles != null;

		final var small = Arrays.stream(keyFiles).filter(keyFile -> !keyFile.getName().contains("0")).map(
				keyFile -> publicKeyFromFile(keyFile.getAbsolutePath())).collect(Collectors.toSet());

		final var keys = Arrays.stream(keyFiles).map(
				keyFile -> publicKeyFromFile(keyFile.getAbsolutePath())).collect(Collectors.toSet());

		assertTrue(file.canSign(keys));


		assertFalse(file.canSign(small));

		Files.deleteIfExists(Path.of(infoFile));
		logger.info("Deleted {}", infoFile);
	}


	private String createAccountInfo(final String filePath) throws Exception {
		final var keyStore =
				Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");
		final var genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());


		final var client = CommonMethods.getClient(NetworkEnum.INTEGRATION);
		client.setOperator(new AccountId(0, 0, 2), genesisKey);
		final var key = EncryptionUtils.jsonToKey(readJsonObject(filePath));
		final var transactionResponse = new AccountCreateTransaction()
				.setKey(key)
				.setInitialBalance(new Hbar(1))
				.setAccountMemo("Test payer account")
				.execute(client);

		final var account = transactionResponse.getReceipt(client).accountId;


		sleep(500);
		final var accountInfo = new AccountInfoQuery()
				.setAccountId(account)
				.execute(client);

		final var infoFile = String.format("%s/%s.info", "src/test/resources/AccountInfos", account);
		writeBytes(infoFile, accountInfo.toBytes());
		logger.info("Account {} created and info stored to {}", account, infoFile);
		return infoFile;
	}
}
