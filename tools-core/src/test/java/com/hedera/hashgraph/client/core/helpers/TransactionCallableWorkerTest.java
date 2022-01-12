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
import com.hedera.hashgraph.client.core.security.KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransferTransaction;
import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Duration;
import java.time.Instant;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TransactionCallableWorkerTest implements GenericFileReadWriteAware {

	@BeforeEach
	void setUp() {
	}

	@AfterEach
	void tearDown() throws IOException {
		final File[] receipts = new File("src/test/resources/Worker_Test").listFiles(
				(dir, name) -> Constants.RECEIPT_EXTENSION.equals(FilenameUtils.getExtension(name)));
		assert receipts != null;
		for (final File receipt : receipts) {
			Files.deleteIfExists(receipt.toPath());
		}

	}

	@Test
	void call_test() throws Exception {
		final var transactionId =
				new TransactionId(new AccountId(0, 0, 2), Instant.now().plusSeconds(10));

		final var transferTransaction = new TransferTransaction();

		transferTransaction.setMaxTransactionFee(new Hbar(1000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("memo")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.setTransactionValidDuration(Duration.ofSeconds(175));

		transferTransaction.addHbarTransfer(new AccountId(0, 0, 2), new Hbar(-1));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 75), new Hbar(1));
		transferTransaction.freeze();

		final KeyStore keyPairs =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");

		transferTransaction.sign(PrivateKey.fromBytes(keyPairs.get(0).getPrivate().getEncoded()));

		final var client = CommonMethods.getClient(NetworkEnum.INTEGRATION);

		final var worker =
				new TransactionCallableWorker(transferTransaction, 10, "src/test/resources/Worker_Test", client);
		worker.call();
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
				new TransactionId(new AccountId(0, 0, 2), Instant.now().minusSeconds(60));

		var transferTransaction = new TransferTransaction();

		transferTransaction.setMaxTransactionFee(new Hbar(1000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("memo")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.setTransactionValidDuration(Duration.ofSeconds(175));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 2), new Hbar(-1));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 75), new Hbar(1));
		transferTransaction.freeze();

		final KeyStore keyPairs =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");

		transferTransaction.sign(PrivateKey.fromBytes(keyPairs.get(0).getPrivate().getEncoded()));

		final var client = CommonMethods.getClient(NetworkEnum.INTEGRATION);

		final var worker =
				new TransactionCallableWorker(transferTransaction, 10, "src/test/resources/Worker_Test", client);
		worker.call();
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

		transferTransaction.sign(PrivateKey.fromBytes(keyPairs.get(0).getPrivate().getEncoded()));

		final TransactionCallableWorker finalWorker =
				new TransactionCallableWorker(transferTransaction, 10, "src/test/resources/Worker_Test", client);
		final Exception e = assertThrows(HederaClientRuntimeException.class, finalWorker::call);
		assertEquals("Hedera Client Runtime: Transaction happens in the past.", e.getMessage());

	}
}
