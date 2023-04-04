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

package com.hedera.hashgraph.client.core.helpers;

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.Ed25519PrivateKey;
import com.hedera.hashgraph.client.core.security.KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransferTransaction;
import io.github.cdimascio.dotenv.Dotenv;
import org.apache.commons.io.FilenameUtils;
import org.bouncycastle.util.encoders.Hex;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.security.KeyStoreException;
import java.time.Duration;
import java.time.Instant;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TransactionCallableWorkerTest implements GenericFileReadWriteAware {

	private AccountId myAccountId;
	private PrivateKey myPrivateKey;
	private Ed25519KeyStore myKeyStore;
	@BeforeEach
	void setUp() throws KeyStoreException {
		myAccountId = AccountId.fromString(Dotenv.configure().directory("../").load().get("MY_ACCOUNT_ID"));
		final var privateKey = Dotenv.configure().directory("../").load().get("MY_PRIVATE_KEY");
		myPrivateKey = PrivateKey.fromString(Dotenv.configure()
				.directory("../").load().get("MY_PRIVATE_KEY"));
		myKeyStore = new Ed25519KeyStore.Builder()
				.withPassword(Constants.TEST_PASSWORD.toCharArray()).build();
		myKeyStore.insertNewKeyPair(Ed25519PrivateKey.fromBytes(Hex.decode(privateKey.startsWith("0x") ?
				privateKey.substring(2) : privateKey)));

	}

	@AfterEach
	void tearDown() throws IOException {
		final File[] receipts = new File("src/test/resources/Worker_Test").listFiles(
				(dir, name) -> Constants.RECEIPT_EXTENSION.equals(FilenameUtils.getExtension(name)));
		for (final File receipt : receipts) {
			Files.deleteIfExists(receipt.toPath());
		}

	}

	@Test
	void call_test() throws Exception {
		final var transactionId =
				new TransactionId(myAccountId, Instant.now().plusSeconds(5));

		final var transferTransaction = new TransferTransaction();

		transferTransaction.setMaxTransactionFee(new Hbar(1000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("memo")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.setTransactionValidDuration(Duration.ofSeconds(175));

		transferTransaction.addHbarTransfer(myAccountId, new Hbar(-1));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 75), new Hbar(1));
		transferTransaction.freeze();

		transferTransaction.sign(myPrivateKey);

		final var client = CommonMethods.getClient(NetworkEnum.TESTNET);

		final var worker =
				new TransactionCallableWorker(transferTransaction, 0, "src/test/resources/Worker_Test", client);
		final var x = worker.call();
		worker.doneSleeping.await();
		final File[] receipts = new File("src/test/resources/Worker_Test").listFiles(
				(dir, name) -> Constants.RECEIPT_EXTENSION.equals(FilenameUtils.getExtension(name)));
		assert receipts != null;
		assertEquals(1, receipts.length);
		final var receipt = TransactionReceipt.fromBytes(readBytes(receipts[0]));
		assertNotNull(receipt);
		assertEquals(Status.SUCCESS, receipt.status);
	}

	@Test
	void callExpiredTransaction_test() throws Exception {
		var transactionId =
				new TransactionId(myAccountId, Instant.now().minusSeconds(60));

		var transferTransaction = new TransferTransaction();

		transferTransaction.setMaxTransactionFee(new Hbar(1000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("memo")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.setTransactionValidDuration(Duration.ofSeconds(175));
		transferTransaction.addHbarTransfer(myAccountId, new Hbar(-1));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 75), new Hbar(1));
		transferTransaction.freeze();

		transferTransaction.sign(myPrivateKey);

		final var client = CommonMethods.getClient(NetworkEnum.TESTNET);

		final var worker =
				new TransactionCallableWorker(transferTransaction, 10, "src/test/resources/Worker_Test", client);
		worker.call();
		worker.doneSleeping.await();
		final File[] receipts = new File("src/test/resources/Worker_Test").listFiles(
				(dir, name) -> Constants.RECEIPT_EXTENSION.equals(FilenameUtils.getExtension(name)));
		assert receipts != null;
		assertEquals(1, receipts.length);
		final var receipt = TransactionReceipt.fromBytes(readBytes(receipts[0]));
		assertNotNull(receipt);
		assertEquals(Status.SUCCESS, receipt.status);

		transactionId =
				new TransactionId(new AccountId(0, 0, 2), Instant.now().minusSeconds(181));

		transferTransaction = new TransferTransaction();

		transferTransaction.setMaxTransactionFee(new Hbar(1000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("memo")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.setTransactionValidDuration(Duration.ofSeconds(175));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 2), new Hbar(-1));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 75), new Hbar(1));
		transferTransaction.freeze();

		transferTransaction.sign(myPrivateKey);

		final TransactionCallableWorker finalWorker =
				new TransactionCallableWorker(transferTransaction, 10, "src/test/resources/Worker_Test", client);
		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> finalWorker.call());
		finalWorker.doneSleeping.await();
		assertEquals("Hedera Client Runtime: Transaction happens in the past.", e.getMessage());

	}
}