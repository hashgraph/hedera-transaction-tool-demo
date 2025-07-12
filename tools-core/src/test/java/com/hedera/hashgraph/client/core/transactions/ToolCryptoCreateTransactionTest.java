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

package com.hedera.hashgraph.client.core.transactions;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.JsonConstants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransferTransaction;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Collections;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_KEYS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.testHelpers.TestHelpers.getJsonInputCA;
import static com.hedera.hashgraph.client.core.testHelpers.TestHelpers.getJsonInputCT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolCryptoCreateTransactionTest implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(ToolCryptoCreateTransactionTest.class);

	@BeforeEach
	void setUp() throws IOException {
		final var storage = new File(DEFAULT_STORAGE);
		if (storage.exists()) {
			FileUtils.deleteDirectory(storage);
		}

		if (storage.mkdirs()) {
			logger.info("Tools folder created");
		}

		if (new File(DEFAULT_KEYS).mkdirs()) {
			logger.info("Keys folder created");
		}
		final var files = new File("src/test/resources/PublicKeys").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(PUB_EXTENSION));
		assert files != null;
		for (final File file : files) {
			final var destFile = new File(DEFAULT_KEYS, file.getName());
			if (!destFile.exists()) {
				FileUtils.copyFile(file, destFile);
			}
		}
		if (new File(DEFAULT_ACCOUNTS).mkdirs()) {
			logger.info("Keys folder created");
		}
		final var accounts = new File("src/test/resources/AccountInfos").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(INFO_EXTENSION));
		assert accounts != null;
		for (final File file : accounts) {
			final var destFile = new File(DEFAULT_ACCOUNTS, file.getName());
			if (!destFile.exists()) {
				FileUtils.copyFile(file, destFile);
			}
		}
	}

	@Test
	void build_test() throws HederaClientException {
		final long sender = 2;
		final long receiver = 50;
		final JsonObject testJson = getJsonInputCT(50, sender, receiver, new Timestamp(20).asInstant());

		final ToolTransferTransaction transaction = new ToolTransferTransaction(testJson);
		assertTrue(transaction.getTransaction() instanceof TransferTransaction);
		final TransferTransaction transfer = (TransferTransaction) transaction.getTransaction();
		assertEquals(2, transfer.getHbarTransfers().size());


	}

	@Test
	void create_test() throws HederaClientException {
		final JsonObject jsonObject = getJsonInputCA(50);
		final String keyName = "src/test/resources/Keys/genesis.pub";
		final JsonObject singleKeyJson = new JsonObject();
		final String pubKey;
		try {
			pubKey = new String(
					Files.readAllBytes(Path.of(keyName.replace(Constants.PK_EXTENSION, Constants.PUB_EXTENSION))));
		} catch (final IOException e) {
			throw new HederaClientException("Could not load public key from file");
		}
		singleKeyJson.addProperty("Ed25519", pubKey);


		jsonObject.add(JsonConstants.NEW_KEY_FIELD_NAME, singleKeyJson);
		jsonObject.addProperty(JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, false);
		final ToolCryptoCreateTransaction transaction = new ToolCryptoCreateTransaction(jsonObject);
		assertTrue(transaction.getTransaction() instanceof AccountCreateTransaction);
		final AccountCreateTransaction create = (AccountCreateTransaction) transaction.getTransaction();

		assertEquals(Hbar.fromTinybars(50), create.getInitialBalance());

	}

	@Test
	void receiverSigRequired_test() throws HederaClientException, IOException {
		final var transactionValidStart = new Timestamp(5).asInstant();
		final var transactionId =
				new TransactionId(new Identifier(0, 0, 2).asAccount(), transactionValidStart);

		final var transferTransaction = new TransferTransaction();

		final Duration transactionValidDuration = Duration.ZERO.withSeconds(179);
		transferTransaction.setMaxTransactionFee(new Hbar(1))
				.setTransactionId(transactionId)
				.setNodeAccountIds(Collections.singletonList(new Identifier(0, 0, 3).asAccount()))
				.setTransactionValidDuration(transactionValidDuration);

		// Add transfers

		transferTransaction.addHbarTransfer(new Identifier(0, 0, 2).asAccount(), new Hbar(-3));
		transferTransaction.addHbarTransfer(new Identifier(0, 0, 1469).asAccount(), new Hbar(1));
		transferTransaction.addHbarTransfer(new Identifier(0, 0, 1470).asAccount(), new Hbar(2));

		transferTransaction.freeze();

		final var transactionBytes = transferTransaction.toBytes();
		writeBytes("src/test/resources/Files/testTransaction.tx", transactionBytes);


		final var toolTransaction = new ToolTransaction();
		final var transaction = toolTransaction.parseFile(new File("src/test/resources/Files/testTransaction.tx"));

		// Accounts 1469 and 1470 have the Receiver Sig Required flag

		final var signers = transaction.getSigningAccountIds();
		assertEquals(3, signers.size());
		assertTrue(signers.contains(new AccountId(0, 0, 2)));
		assertTrue(signers.contains(new AccountId(0, 0, 1469)));
		assertTrue(signers.contains(new AccountId(0, 0, 1470)));

		Files.deleteIfExists(Path.of("src/test/resources/Files/testTransaction.tx"));
	}

	@Test
	void getSigners_test() throws HederaClientException, IOException {
		final var transactionValidStart = new Timestamp(5).asInstant();
		final var transactionId =
				new TransactionId(new Identifier(0, 0, 2).asAccount(), transactionValidStart);

		final var transferTransaction = new TransferTransaction();

		final Duration transactionValidDuration = Duration.ZERO.withSeconds(179);
		transferTransaction.setMaxTransactionFee(new Hbar(1))
				.setTransactionId(transactionId)
				.setNodeAccountIds(Collections.singletonList(new Identifier(0, 0, 3).asAccount()))
				.setTransactionValidDuration(transactionValidDuration);

		// Add transfers

		transferTransaction.addHbarTransfer(new Identifier(0, 0, 2).asAccount(), new Hbar(-3));
		transferTransaction.addHbarTransfer(new Identifier(0, 0, 1057).asAccount(), new Hbar(1));
		transferTransaction.addHbarTransfer(new Identifier(0, 0, 29255).asAccount(), new Hbar(2));

		transferTransaction.freeze();

		final var transactionBytes = transferTransaction.toBytes();
		writeBytes("src/test/resources/Files/testTransaction.tx", transactionBytes);


		final var toolTransaction = new ToolTransaction();
		final var transaction = toolTransaction.parseFile(new File("src/test/resources/Files/testTransaction.tx"));

		final var signers = transaction.getSigningAccountIds();
		assertEquals(1, signers.size());
		assertTrue(signers.contains(new AccountId(0, 0, 2)));

		Files.deleteIfExists(Path.of("src/test/resources/Files/testTransaction.tx"));
	}
}