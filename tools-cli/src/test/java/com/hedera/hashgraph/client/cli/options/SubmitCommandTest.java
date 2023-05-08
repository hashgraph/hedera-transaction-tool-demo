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
import com.hedera.hashgraph.sdk.AccountBalanceQuery;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransactionResponse;
import com.hedera.hashgraph.sdk.TransferTransaction;
import io.github.cdimascio.dotenv.Dotenv;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SubmitCommandTest implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(SubmitCommandTest.class);
	public static final String TRANSACTIONS = "src/test/resources/TempTransactions";
	public static final String RECEIPTS = "src/test/resources/TempReceipts";
	public static final int TEST_SIZE = 10;

	private static PrivateKey generalPrivateKey;
	private static Client client;
	private static final Map<AccountId, TransactionReceipt> receipts = new HashMap<>();

	@BeforeAll
	static void beforeAll() throws ReceiptStatusException, IOException {
		var dotenv = Dotenv.configure().directory("../").load();
		final var myAccountId = AccountId.fromString(dotenv.get("TEST_ACCOUNT_ID"));
		final var myPrivateKey = PrivateKey.fromString(dotenv.get("TEST_PRIVATE_KEY"));

		client = Client.forTestnet();
		client.setOperator(myAccountId, myPrivateKey);

		generalPrivateKey = PrivateKey.generateED25519();
		final PublicKey generalPublicKey = generalPrivateKey.getPublicKey();

		for (int i = 0; i < 2; i++) {
			final TransactionResponse transactionResponse;
			try {
				transactionResponse = new AccountCreateTransaction()
						// The only _required_ property here is `key`
						.setKey(generalPublicKey)
						.setInitialBalance(Hbar.fromTinybars(1000000000))
						.execute(client);

				// This will wait for the receipt to become available
				final TransactionReceipt receipt = transactionResponse.getReceipt(client);
				final AccountId newAccountId = receipt.accountId;
				receipts.put(newAccountId, receipt);

			} catch (final TimeoutException | PrecheckStatusException e) {
				logger.error(e.getMessage());
			}

		}
		logger.info("{} accounts created", receipts.size());

		if (new File(TRANSACTIONS).mkdirs()) {
			logger.info("Transactions folder created");
		}
		FileUtils.cleanDirectory(new File(TRANSACTIONS));
		if (new File(RECEIPTS).mkdirs()) {
			logger.info("Receipts folder created");
		}
		FileUtils.cleanDirectory(new File(RECEIPTS));
	}

	@AfterEach
	void tearDown() throws IOException {
		FileUtils.cleanDirectory(new File(TRANSACTIONS));
		FileUtils.cleanDirectory(new File(RECEIPTS));
	}

	@AfterAll
	static void afterAll() throws IOException {
		FileUtils.deleteDirectory(new File(TRANSACTIONS));
		FileUtils.deleteDirectory(new File(RECEIPTS));

	}

	@Test
	void submitOneTransaction_test() throws Exception {
		logger.info("Creating transaction");

		final var iterator = receipts.entrySet().iterator();
		final Map.Entry<AccountId, TransactionReceipt> sender = iterator.next();
		final Map.Entry<AccountId, TransactionReceipt> receiver = iterator.next();
		final String transactionLocation =
				createTransfer(3, sender.getKey(), receiver.getKey(), Instant.now());

		final Hbar initialBalance = new AccountBalanceQuery()
				.setAccountId(receiver.getKey())
				.execute(client)
				.hbars;

		logger.info("Submitting {}", transactionLocation);
		final String[] args =
				{ "submit", "-t", transactionLocation, "-n", NetworkEnum.TESTNET.getName(), "-o", RECEIPTS };
		ToolsMain.main(args);

		final File[] receipts = new File(RECEIPTS).listFiles(
				(dir, name) -> Constants.RECEIPT_EXTENSION.equals(FilenameUtils.getExtension(name)));
		assert receipts != null;
		assertTrue(receipts.length > 0);
		final var receipt = TransactionReceipt.fromBytes(readBytes(receipts[0]));
		assertEquals(Status.SUCCESS, receipt.status);

		final Hbar finalBalance = new AccountBalanceQuery()
				.setAccountId(receiver.getKey())
				.execute(client)
				.hbars;

		assertEquals(1000, finalBalance.toTinybars() - initialBalance.toTinybars());

	}

	@Test
	void submitOneTransactionWildCards_test() throws Exception {
		logger.info("Creating transaction");

		final var iterator = receipts.entrySet().iterator();
		final Map.Entry<AccountId, TransactionReceipt> sender = iterator.next();
		final Map.Entry<AccountId, TransactionReceipt> receiver = iterator.next();
		final String transactionLocation =
				createTransfer(3, sender.getKey(), receiver.getKey(), Instant.now());
		final Hbar initialBalance = new AccountBalanceQuery()
				.setAccountId(receiver.getKey())
				.execute(client)
				.hbars;

		final var wildcardName = FilenameUtils.getBaseName(transactionLocation).substring(0, 5) + "*";
		logger.info("Submitting {}", wildcardName);
		final String[] args =
				{ "submit", "-t", TRANSACTIONS + File.separator + wildcardName, "-n", NetworkEnum.TESTNET.getName(), "-o", RECEIPTS };
		ToolsMain.main(args);

		final File[] receipts = new File(RECEIPTS).listFiles(
				(dir, name) -> Constants.RECEIPT_EXTENSION.equals(FilenameUtils.getExtension(name)));
		assert receipts != null;
		assertTrue(receipts.length > 0);
		final var receipt = TransactionReceipt.fromBytes(readBytes(receipts[0]));
		assertEquals(Status.SUCCESS, receipt.status);
		final Hbar finalBalance = new AccountBalanceQuery()
				.setAccountId(receiver.getKey())
				.execute(client)
				.hbars;

		assertEquals(1000, finalBalance.toTinybars() - initialBalance.toTinybars());

	}

	@Test
	void submitMultipleTransactions_test() throws Exception {
		int count = 3;
		final Random rand = new Random();
		final var iterator = receipts.entrySet().iterator();
		final List<String> transactions = new ArrayList<>();
		final Map.Entry<AccountId, TransactionReceipt> sender = iterator.next();
		final Map.Entry<AccountId, TransactionReceipt> receiver = iterator.next();
		final Hbar initialBalance = new AccountBalanceQuery()
				.setAccountId(receiver.getKey())
				.execute(client)
				.hbars;

		for (int i = 0; i < TEST_SIZE; i++) {
			final var transactionValidStart = Instant.now().plusSeconds(count);
			transactions.add(createTransfer(3, sender.getKey(), receiver.getKey(), transactionValidStart));

			if (i == TEST_SIZE / 2) {
				for (int j = 1; j <= TEST_SIZE; j++) {
					transactions.add(createTransfer(3, sender.getKey(), receiver.getKey(), transactionValidStart.plusNanos(j)));
				}
			}

			count += rand.nextInt(3) + 1;
		}

		final String[] args =
				{ "submit", "-t", TRANSACTIONS, "-n", NetworkEnum.TESTNET.getName(), "-o", RECEIPTS };
		ToolsMain.main(args);

		final File[] receipts = new File(RECEIPTS).listFiles(
				(dir, name) -> Constants.RECEIPT_EXTENSION.equals(FilenameUtils.getExtension(name)));
		assert receipts != null;
		assertEquals(transactions.size(), receipts.length);

		for (final File receipt : receipts) {
			assertEquals(Status.SUCCESS, TransactionReceipt.fromBytes(readBytes(receipt)).status);
		}
		final Hbar finalBalance = new AccountBalanceQuery()
				.setAccountId(receiver.getKey())
				.execute(client)
				.hbars;

		assertEquals(TEST_SIZE * 2000, finalBalance.toTinybars() - initialBalance.toTinybars());
	}

	@Test
	void submitPastTransaction_test() {
		final String[] args =
				{ "submit", "-t", "src/test/resources/transactions/1625538609-0_0_9401-2092093589.txsig", "-n",
						"TESTNET", "-o", RECEIPTS };
		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client Runtime: No valid transactions found.", e.getMessage());
	}

	@Test
	void submitDuplicatedTransaction_test() throws Exception {
		logger.info("Creating transaction");

		final var iterator = receipts.entrySet().iterator();
		final Map.Entry<AccountId, TransactionReceipt> sender = iterator.next();
		final Map.Entry<AccountId, TransactionReceipt> receiver = iterator.next();
		final Hbar initialBalance = new AccountBalanceQuery()
				.setAccountId(receiver.getKey())
				.execute(client)
				.hbars;

		final var transactionValidStart = Instant.now();
		final String transactionLocation0 = createTransfer(3, sender.getKey(), receiver.getKey(),
				transactionValidStart);
		final String transactionLocation1 = createTransfer(4, sender.getKey(), receiver.getKey(),
				transactionValidStart);

		logger.info("Submitting transactions");

		final String[] args0 =
				{ "submit", "-t", transactionLocation0, "-n", NetworkEnum.TESTNET.getName(), "-o", RECEIPTS };
		ToolsMain.main(args0);

		final String[] args1 =
				{ "submit", "-t", transactionLocation1, "-n", NetworkEnum.TESTNET.getName(), "-o", RECEIPTS };
		ToolsMain.main(args1);
		logger.info("Transactions submitted");

		final Hbar finalBalance = new AccountBalanceQuery()
				.setAccountId(receiver.getKey())
				.execute(client)
				.hbars;

		assertEquals(1000, finalBalance.toTinybars() - initialBalance.toTinybars());
	}

	private String createTransfer(final int node, final AccountId sender, final AccountId receiver,
			final Instant transactionValidStart) throws HederaClientException {

		final var transactionId =
				new TransactionId(sender, transactionValidStart);

		final var transferTransaction = new TransferTransaction();

		transferTransaction.setMaxTransactionFee(Hbar.fromTinybars(1000000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("test transfer")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, node)))
				.setTransactionValidDuration(Duration.ofSeconds(175));

		// Add transfers
		transferTransaction.addHbarTransfer(sender, Hbar.fromTinybars(-1000));
		transferTransaction.addHbarTransfer(receiver, Hbar.fromTinybars(1000));
		transferTransaction.freeze();
		transferTransaction.sign(generalPrivateKey);

		final var filePath =
				TRANSACTIONS + File.separator + transactionId.toString() +
						Constants.FILE_NAME_GROUP_SEPARATOR + node +
						"." + Constants.SIGNED_TRANSACTION_EXTENSION;
		writeBytes(filePath, transferTransaction.toBytes());
		return filePath;
	}
}
