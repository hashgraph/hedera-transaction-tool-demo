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

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.AccountUpdateTransaction;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.SystemDeleteTransaction;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransactionResponse;
import com.hedera.hashgraph.sdk.TransferTransaction;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStoreException;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_READABLE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.testHelpers.TestHelpers.getJsonInputCT;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolTransactionTest {
	private final long sender = 2;
	private final long receiver = 50;

	@BeforeEach
	void setUp() throws IOException {
		Files.deleteIfExists(Path.of("src/test/resources/Files/TransactionFileTests/temp.tx"));

	}

	@AfterEach
	void tearDown() throws IOException {
		Files.deleteIfExists(Path.of("src/test/resources/Files/TransactionFileTests/temp.tx"));

	}

	@Test
	void checkInput_test() {
		final var startTime = new Timestamp(20).asInstant();
		final var testJsonMissingInput = getJsonInputCT(50, sender, receiver, startTime);
		testJsonMissingInput.remove(FEE_PAYER_ACCOUNT_FIELD_NAME);
		final Exception e0 = assertThrows(HederaClientException.class, () -> new ToolTransaction(testJsonMissingInput));
		assertEquals("Hedera Client: Cannot validate input", e0.getMessage());

		final var badPayerId = getJsonInputCT(50, sender, receiver, startTime);
		badPayerId.addProperty(FEE_PAYER_ACCOUNT_FIELD_NAME, "bad");
		final Exception e1 = assertThrows(HederaClientException.class, () -> new ToolTransaction(badPayerId));
		assertEquals("Hedera Client: Cannot validate input", e1.getMessage());

		final var badNodeId = getJsonInputCT(50, sender, receiver, startTime);
		badNodeId.addProperty(NODE_ID_FIELD_NAME, "bad");
		final Exception e2 = assertThrows(HederaClientException.class, () -> new ToolTransaction(badNodeId));
		assertEquals("Hedera Client: Cannot validate input", e2.getMessage());

		final var badFee = getJsonInputCT(50, sender, receiver, startTime);
		badFee.addProperty(TRANSACTION_FEE_FIELD_NAME, "bad");
		final Exception e3 = assertThrows(HederaClientException.class, () -> new ToolTransaction(badFee));
		assertEquals("Hedera Client: Cannot validate input", e3.getMessage());

		final var badNetwork = getJsonInputCT(50, sender, receiver, startTime);
		badNetwork.addProperty(NETWORK_FIELD_NAME, "bad");
		final Exception e4 = assertThrows(HederaClientException.class, () -> new ToolTransaction(badNetwork));
		assertEquals("Hedera Client: Cannot validate input", e4.getMessage());

		final var badStart = getJsonInputCT(50, sender, receiver, startTime);
		badStart.addProperty(TRANSACTION_VALID_START_FIELD_NAME, "bad");
		final Exception e5 = assertThrows(HederaClientException.class, () -> new ToolTransaction(badStart));
		assertEquals("Hedera Client: Cannot validate input", e5.getMessage());

		final var badDuration = getJsonInputCT(50, sender, receiver, startTime);
		badDuration.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, "bad");
		final Exception e6 = assertThrows(HederaClientException.class, () -> new ToolTransaction(badDuration));
		assertEquals("Hedera Client: Cannot validate input", e6.getMessage());

	}

	@Test
	void checkEquals_test() throws HederaClientException {
		final var transaction0 = new ToolTransaction();

		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);
		final var transaction1 = new ToolTransaction(testJson);
		final var transaction2 = new ToolTransaction(testJson);
		assertNotEquals(transaction0, transaction1);
		assertEquals(transaction1, transaction2);

		final var update = new ToolCryptoUpdateTransaction(
				new File("src/test/resources/Files/TransactionFileTests/updateAccount.tx"));
		final var transfer1 = new ToolTransferTransaction(
				new File("src/test/resources/Files/TransactionFileTests/transferTransaction.tx"));
		final var transfer2 = new ToolTransferTransaction(
				new File("src/test/resources/Files/TransactionFileTests/transferTransaction.tx"));
		assertTrue(transfer1.equals(transfer2));
		assertFalse(transfer1.equals(update));

	}

	@Test
	void getTransactionFields_test() throws HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final var emptyTransaction = new ToolTransaction();
		assertNull(emptyTransaction.getMemo());
		assertNull(emptyTransaction.getTransactionFee());
		assertNull(emptyTransaction.getTransactionValidDuration());
		assertNull(emptyTransaction.getTransactionValidStart());
		assertNull(emptyTransaction.getFeePayerID());
		assertNull(emptyTransaction.getNodeID());
		assertNull(emptyTransaction.getNetwork());

		final var testJsonBad = getJsonInputCT(50, sender, receiver, startTime);
		testJsonBad.remove(FEE_PAYER_ACCOUNT_FIELD_NAME);
		final Exception e = assertThrows(HederaClientException.class, () -> new ToolTransaction(testJsonBad));
		assertEquals("Hedera Client: Cannot validate input", e.getMessage());

		final var testJson = getJsonInputCT(50, sender, receiver, startTime);
		final var transaction = new ToolTransaction(testJson);

		assertEquals("a memo to go with the transaction", transaction.getMemo());
		assertEquals(Hbar.fromTinybars(100000000), transaction.getTransactionFee());
		assertEquals(Duration.ofSeconds(120), transaction.getTransactionValidDuration());
		assertEquals(startTime.truncatedTo(ChronoUnit.SECONDS), transaction.getTransactionValidStart());
		assertEquals(sender, transaction.getFeePayerID().getAccountNum());
		assertEquals(new Identifier(0, 0, 3, "Mainnet"), transaction.getNodeID());
		assertEquals(NetworkEnum.INTEGRATION, transaction.getNetwork());
	}

	@Test
	void transferTransaction_test() throws HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);

		final var transfer = new ToolTransferTransaction(testJson);
		final var transactionId = transfer.getTransactionId();
		assertEquals(new Identifier(0, 0, sender).asAccount(), transactionId.accountId);
		assertEquals(startTime.truncatedTo(ChronoUnit.SECONDS), transactionId.validStart);

		assertEquals("a memo to go with the transaction", transfer.getMemo());
		assertEquals(Hbar.fromTinybars(100000000), transfer.getTransactionFee());
		assertEquals(Duration.ofSeconds(120), transfer.getTransactionValidDuration());
		assertEquals(startTime.truncatedTo(ChronoUnit.SECONDS), transfer.getTransactionValidStart());
		assertEquals(sender, transfer.getFeePayerID().getAccountNum());
		assertEquals(new Identifier(0, 0, 3, "mainnet"), transfer.getNodeID());
		assertEquals(NetworkEnum.INTEGRATION, transfer.getNetwork());

		final var actualTransfer = transfer.getTransaction();
		assertTrue(actualTransfer instanceof TransferTransaction);

		final var transferList = ((TransferTransaction) actualTransfer).getHbarTransfers();
		assertNotNull(transferList);
		assertEquals(2, transferList.size());
		assertTrue(transferList.containsKey(new Identifier(0, 0, sender).asAccount()));
		assertEquals(Hbar.fromTinybars(-50), transferList.get(new Identifier(0, 0, sender).asAccount()));
		assertTrue(transferList.containsKey(new Identifier(0, 0, receiver).asAccount()));
		assertEquals(Hbar.fromTinybars(50), transferList.get(new Identifier(0, 0, receiver).asAccount()));

		assertEquals(TransactionType.CRYPTO_TRANSFER, transfer.getTransactionType());

		final var signers = transfer.getSigningAccounts();
		assertEquals(1, signers.size());
		assertTrue(signers.contains(new Identifier(0, 0, sender).asAccount()));

	}

	@Test
	void constructorFromFile_test() throws HederaClientException {
		final var transfer = new ToolTransferTransaction(
				new File("src/test/resources/Files/TransactionFileTests/transferTransaction.tx"));

		final var transactionId = transfer.getTransactionId();
		assertEquals(new Identifier(0, 0, 76).asAccount(), transactionId.accountId);
		assert transactionId.validStart != null;
		assertEquals("2029-05-05T22:10:07", new Timestamp(transactionId.validStart).asRFCString());

		assertEquals("", transfer.getMemo());
		assertEquals(Hbar.fromTinybars(100000000), transfer.getTransactionFee());
		assertEquals(Duration.ofSeconds(180), transfer.getTransactionValidDuration());
		assertEquals("2029-05-05T22:10:07", new Timestamp(transfer.getTransactionValidStart()).asRFCString());
		assertEquals(76, transfer.getFeePayerID().getAccountNum());
		assertEquals(new Identifier(0, 0, 3), transfer.getNodeID());
		assertNull(transfer.getNetwork());

		final var actualTransfer = transfer.getTransaction();
		assertTrue(actualTransfer instanceof TransferTransaction);

		final var transferList = ((TransferTransaction) actualTransfer).getHbarTransfers();
		assertNotNull(transferList);
		assertEquals(2, transferList.size());
		assertTrue(transferList.containsKey(new Identifier(0, 0, 50).asAccount()));
		assertEquals(Hbar.from(-100), transferList.get(new Identifier(0, 0, 50).asAccount()));
		assertTrue(transferList.containsKey(new Identifier(0, 0, 94).asAccount()));
		assertEquals(Hbar.from(100), transferList.get(new Identifier(0, 0, 94).asAccount()));

		assertEquals(TransactionType.CRYPTO_TRANSFER, transfer.getTransactionType());

		final var signers = transfer.getSigningAccounts();
		assertEquals(2, signers.size());
		assertTrue(signers.contains(new Identifier(0, 0, 76).asAccount()));
		assertTrue(signers.contains(new Identifier(0, 0, 50).asAccount()));
	}

	@Test
	void parseFile_test() throws HederaClientException {
		final var transfer = new ToolTransaction().parseFile(
				new File("src/test/resources/Files/TransactionFileTests/transferTransaction.tx"));

		assertTrue(transfer instanceof ToolTransferTransaction);
		assertTrue(transfer.transaction instanceof TransferTransaction);
		assertEquals(TransactionType.CRYPTO_TRANSFER, transfer.getTransactionType());

		final var create = new ToolTransaction().parseFile(
				new File("src/test/resources/Files/TransactionFileTests/createAccount.tx"));

		assertTrue(create instanceof ToolCryptoCreateTransaction);
		assertTrue(create.transaction instanceof AccountCreateTransaction);
		assertEquals(TransactionType.CRYPTO_CREATE, create.getTransactionType());

		final var update = new ToolTransaction().parseFile(
				new File("src/test/resources/Files/TransactionFileTests/updateAccount.tx"));

		assertTrue(update instanceof ToolCryptoUpdateTransaction);
		assertTrue(update.transaction instanceof AccountUpdateTransaction);
		assertEquals(TransactionType.CRYPTO_UPDATE, update.getTransactionType());

		final var system = new ToolTransaction().parseFile(
				new File("src/test/resources/Files/TransactionFileTests/systemDelete.tx"));

		assertTrue(system instanceof ToolSystemTransaction);
		assertTrue(system.transaction instanceof SystemDeleteTransaction);
		assertEquals(TransactionType.SYSTEM_DELETE_UNDELETE, system.getTransactionType());
	}

	@Test
	void storeRead_test() throws HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);

		final var transfer = new ToolTransferTransaction(testJson);

		transfer.store("src/test/resources/Files/TransactionFileTests/temp.tx");

		final ToolTransaction transaction = new ToolTransaction();
		assertFalse(transaction.read("src/test/resources/Files/0.0.2.meta"));
		assertTrue(transaction.read("src/test/resources/Files/TransactionFileTests/temp.tx"));

		assertEquals("a memo to go with the transaction", transaction.getMemo());
		assertEquals(Hbar.fromTinybars(100000000), transaction.getTransactionFee());
		assertEquals(Duration.ofSeconds(120), transaction.getTransactionValidDuration());
		assertEquals(startTime.truncatedTo(ChronoUnit.SECONDS), transaction.getTransactionValidStart());
		assertEquals(sender, transaction.getFeePayerID().getAccountNum());
		assertEquals(new Identifier(0, 0, 3), transaction.getNodeID());
		assertNull(transaction.getNetwork());
	}

	@Test
	void asJson_test() throws HederaClientException {
		final ToolTransaction transaction = new ToolTransaction();
		assertTrue(transaction.read("src/test/resources/Files/TransactionFileTests/transferTransaction.tx"));
		final var jsonTransaction = transaction.asJson();
		assertTrue(jsonTransaction.has(FEE_PAYER_ACCOUNT_FIELD_NAME));
		assertTrue(jsonTransaction.has(NODE_ID_FIELD_NAME));
		assertTrue(jsonTransaction.has(TRANSACTION_FEE_FIELD_NAME));
		assertTrue(jsonTransaction.has(TRANSACTION_VALID_START_FIELD_NAME));
		assertTrue(jsonTransaction.has(TRANSACTION_VALID_START_READABLE_FIELD_NAME));
		assertTrue(jsonTransaction.has(TRANSACTION_VALID_DURATION_FIELD_NAME));
		assertTrue(jsonTransaction.has(MEMO_FIELD_NAME));

		assertEquals(new Identifier(0, 0, 76).asJSON(),
				jsonTransaction.get(FEE_PAYER_ACCOUNT_FIELD_NAME).getAsJsonObject());
		assertEquals(new Identifier(0, 0, 3).asJSON(), jsonTransaction.get(NODE_ID_FIELD_NAME));
		assertEquals(100000000L, jsonTransaction.get(TRANSACTION_FEE_FIELD_NAME).getAsLong());
		assertEquals(new Timestamp(transaction.transactionValidStart).asJSON(),
				jsonTransaction.get(TRANSACTION_VALID_START_FIELD_NAME));
		assertEquals(new Timestamp(transaction.transactionValidStart).asRFCString(),
				jsonTransaction.get(TRANSACTION_VALID_START_READABLE_FIELD_NAME).getAsString());
		assertEquals(180, jsonTransaction.get(TRANSACTION_VALID_DURATION_FIELD_NAME).getAsInt());
		assertEquals("", jsonTransaction.get(MEMO_FIELD_NAME).getAsString());

	}

	@Test
	void testSigningKeys() throws HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);

		final var transfer = new ToolTransferTransaction(testJson);
		var keys = transfer.getSigningKeys("src/test/resources/BadInfo");
		assertEquals(0, keys.size());

		keys = transfer.getSigningKeys("src/test/resources/AccountInfos");
		assertEquals(28, keys.size());
	}


	@Test
	void sign_test() throws KeyStoreException, HederaClientException {

		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);

		final var transfer = new ToolTransferTransaction(testJson);


		final var keyStore =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");

		final var privateKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());
		final var signature = transfer.sign(privateKey);
		assertNotNull(signature);
		assertEquals(64, signature.length);
		var nonZero = false;
		for (final byte b : signature) {
			if (b != 0) {
				nonZero = true;
				break;
			}
		}
		assertTrue(nonZero);
	}

	@Test
	void collate_test() throws KeyStoreException, HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);
		final var nodeId = new Identifier(0, 0, 3).asAccount();

		final Map<PublicKey, byte[]> signatures = new HashMap<>();

		final var transfer0 = new ToolTransferTransaction(testJson);
		final var privateKey0 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-0.pem").get(0).getPrivate().getEncoded());
		signatures.put(privateKey0.getPublicKey(), transfer0.sign(privateKey0));

		final var transfer1 = new ToolTransferTransaction(testJson);
		final var privateKey1 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-1.pem").get(0).getPrivate().getEncoded());
		signatures.put(privateKey1.getPublicKey(), transfer1.sign(privateKey1));

		final var transfer2 = new ToolTransferTransaction(testJson);
		final var privateKey2 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-2.pem").get(0).getPrivate().getEncoded());
		signatures.put(privateKey2.getPublicKey(), transfer2.sign(privateKey2));

		final var transfer3 = new ToolTransferTransaction(testJson);
		final var privateKey3 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-3.pem").get(0).getPrivate().getEncoded());
		signatures.put(privateKey3.getPublicKey(), transfer3.sign(privateKey3));

		final var transfer = new ToolTransferTransaction(testJson);
		final var sigMapBefore = transfer.getTransaction().getSignatures();
		assertFalse(sigMapBefore.containsKey(nodeId));

		transfer.collate(signatures);
		final var sigMapAfter = transfer.getTransaction().getSignatures();
		assertTrue(sigMapAfter.containsKey(nodeId));
		final var collatedMap = sigMapAfter.get(nodeId);
		assertEquals(4, collatedMap.size());

		for (final Map.Entry<PublicKey, byte[]> entry : signatures.entrySet()) {
			assertTrue(collatedMap.containsKey(entry.getKey()));
			assertArrayEquals(collatedMap.get(entry.getKey()), entry.getValue());
			entry.getKey().verifyTransaction(transfer.getTransaction());
		}


	}

	@Test
	void collateTransactions_test() throws KeyStoreException, HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);
		final var nodeId = new Identifier(0, 0, 3).asAccount();

		final var transfer0 = new ToolTransferTransaction(testJson);
		final var privateKey0 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-0.pem").get(0).getPrivate().getEncoded());
		final var signature0 = transfer0.sign(privateKey0);

		final var transfer1 = new ToolTransferTransaction(testJson);
		final var privateKey1 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-1.pem").get(0).getPrivate().getEncoded());
		final var signature1 = transfer1.sign(privateKey1);

		final var sigMapBefore = transfer0.getTransaction().getSignatures();
		assertTrue(sigMapBefore.containsKey(nodeId));
		assertEquals(1, sigMapBefore.get(nodeId).size());
		assertArrayEquals(signature0, sigMapBefore.get(nodeId).get(privateKey0.getPublicKey()));

		transfer0.collate(transfer1.getTransaction());

		final var sigMapAfter = transfer0.getTransaction().getSignatures();
		assertTrue(sigMapAfter.containsKey(nodeId));
		assertEquals(2, sigMapAfter.get(nodeId).size());
		assertArrayEquals(signature0, sigMapAfter.get(nodeId).get(privateKey0.getPublicKey()));
		assertArrayEquals(signature1, sigMapAfter.get(nodeId).get(privateKey1.getPublicKey()));

	}

	@Test
	void collateSignaturePairs_test() throws KeyStoreException, HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);
		final var nodeId = new Identifier(0, 0, 3).asAccount();

		final Set<SignaturePair> pairs = new HashSet<>();

		final var transfer0 = new ToolTransferTransaction(testJson);
		final var privateKey0 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-0.pem").get(0).getPrivate().getEncoded());
		pairs.add(new SignaturePair(privateKey0.getPublicKey(), transfer0.sign(privateKey0)));

		final var transfer1 = new ToolTransferTransaction(testJson);
		final var privateKey1 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-1.pem").get(0).getPrivate().getEncoded());
		pairs.add(new SignaturePair(privateKey1.getPublicKey(), transfer1.sign(privateKey1)));

		final var transfer2 = new ToolTransferTransaction(testJson);
		final var privateKey2 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-2.pem").get(0).getPrivate().getEncoded());
		pairs.add(new SignaturePair(privateKey2.getPublicKey(), transfer2.sign(privateKey2)));

		final var transfer3 = new ToolTransferTransaction(testJson);
		final var privateKey3 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-3.pem").get(0).getPrivate().getEncoded());
		pairs.add(new SignaturePair(privateKey3.getPublicKey(), transfer3.sign(privateKey3)));

		final var transfer = new ToolTransferTransaction(testJson);
		final var sigMapBefore = transfer.getTransaction().getSignatures();
		assertFalse(sigMapBefore.containsKey(nodeId));

		transfer.collate(pairs);
		final var sigMapAfter = transfer.getTransaction().getSignatures();
		assertTrue(sigMapAfter.containsKey(nodeId));
		final var collatedMap = sigMapAfter.get(nodeId);
		assertEquals(4, collatedMap.size());

		for (final SignaturePair pair : pairs) {
			assertTrue(collatedMap.containsKey(pair.getPublicKey()));
			assertArrayEquals(pair.getSignature(), collatedMap.get(pair.getPublicKey()));
			pair.getPublicKey().verifyTransaction(transfer.getTransaction());
		}
	}

	@Test
	void verify_test() throws KeyStoreException, HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, sender, receiver, startTime);

		final Set<SignaturePair> pairs = new HashSet<>();
		final List<PublicKey> publicKeys = new ArrayList<>();

		final var transfer0 = new ToolTransferTransaction(testJson);
		final var privateKey0 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-0.pem").get(0).getPrivate().getEncoded());
		pairs.add(new SignaturePair(privateKey0.getPublicKey(), transfer0.sign(privateKey0)));
		publicKeys.add(privateKey0.getPublicKey());

		final var transfer1 = new ToolTransferTransaction(testJson);
		final var privateKey1 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-1.pem").get(0).getPrivate().getEncoded());
		pairs.add(new SignaturePair(privateKey1.getPublicKey(), transfer1.sign(privateKey1)));
		publicKeys.add(privateKey1.getPublicKey());

		final var transfer2 = new ToolTransferTransaction(testJson);
		final var privateKey2 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-2.pem").get(0).getPrivate().getEncoded());
		pairs.add(new SignaturePair(privateKey2.getPublicKey(), transfer2.sign(privateKey2)));
		publicKeys.add(privateKey2.getPublicKey());

		final var transfer3 = new ToolTransferTransaction(testJson);
		final var privateKey3 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-3.pem").get(0).getPrivate().getEncoded());
		pairs.add(new SignaturePair(privateKey3.getPublicKey(), transfer3.sign(privateKey3)));
		publicKeys.add(privateKey3.getPublicKey());

		final var transfer = new ToolTransferTransaction(testJson);
		transfer.collate(pairs);

		for (final PublicKey publicKey : publicKeys) {
			assertTrue(transfer.verify(publicKey));
		}

	}

	@Test
	void verifyWithInfoAndSubmit() throws KeyStoreException, PrecheckStatusException, TimeoutException,
			ReceiptStatusException, HederaClientException, InterruptedException {
		final List<PublicKey> publicKeys = new ArrayList<>();
		final var privateKey0 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-0.pem").get(0).getPrivate().getEncoded());
		publicKeys.add(privateKey0.getPublicKey());
		final var privateKey1 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-1.pem").get(0).getPrivate().getEncoded());
		publicKeys.add(privateKey1.getPublicKey());
		final var privateKey2 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-2.pem").get(0).getPrivate().getEncoded());
		publicKeys.add(privateKey2.getPublicKey());
		final var privateKey3 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-3.pem").get(0).getPrivate().getEncoded());
		publicKeys.add(privateKey3.getPublicKey());

		final KeyList keys = new KeyList();
		keys.addAll(publicKeys);
		keys.setThreshold(2);

		assertEquals(4, keys.size());

		final AccountInfo info = createAccount(keys);
		final var startTime = new Timestamp(20).asInstant();
		final var testJson = getJsonInputCT(50, info.accountId.num, receiver, startTime);

		final Set<SignaturePair> pairs = new HashSet<>();

		final var transfer0 = new ToolTransferTransaction(testJson);
		pairs.add(new SignaturePair(privateKey0.getPublicKey(), transfer0.sign(privateKey0)));
		assertFalse(transfer0.verify(info));

		final var transfer1 = new ToolTransferTransaction(testJson);
		pairs.add(new SignaturePair(privateKey1.getPublicKey(), transfer1.sign(privateKey1)));
		assertFalse(transfer1.verify(info));

		final var transfer2 = new ToolTransferTransaction(testJson);
		pairs.add(new SignaturePair(privateKey2.getPublicKey(), transfer2.sign(privateKey2)));
		assertFalse(transfer2.verify(info));

		final var transfer = new ToolTransferTransaction(testJson);

		transfer.collate(pairs);
		assertTrue(transfer.verify(info));

		final var receipt = transfer.submit();
		assertEquals(Status.SUCCESS, receipt.status);
		final var endTime = new Timestamp().asInstant();
		assertTrue(endTime.isAfter(startTime));
	}

	@Test
	void badTransactionSubmit_test() throws KeyStoreException, ReceiptStatusException, PrecheckStatusException,
			TimeoutException, HederaClientException {
		final List<PublicKey> publicKeys = new ArrayList<>();
		final var privateKey0 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-0.pem").get(0).getPrivate().getEncoded());
		publicKeys.add(privateKey0.getPublicKey());
		final var privateKey1 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-1.pem").get(0).getPrivate().getEncoded());
		publicKeys.add(privateKey1.getPublicKey());
		final var privateKey2 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-2.pem").get(0).getPrivate().getEncoded());
		publicKeys.add(privateKey2.getPublicKey());
		final var privateKey3 = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/KeyStore-3.pem").get(0).getPrivate().getEncoded());
		publicKeys.add(privateKey3.getPublicKey());

		final KeyList keys = new KeyList();
		keys.addAll(publicKeys);
		keys.setThreshold(2);

		assertEquals(4, keys.size());

		final AccountInfo info = createAccount(keys);
		final var startTime = new Timestamp(1).asInstant();
		final var testJson = getJsonInputCT(50, info.accountId.num, receiver, startTime);

		final var transfer = new ToolTransferTransaction(testJson);

		final Exception e = assertThrows(HederaClientRuntimeException.class, transfer::submit);
		assertTrue(e.getMessage().contains("failed pre-check with the status `INVALID_SIGNATURE`"));
	}

	private AccountInfo createAccount(
			final KeyList keys) throws KeyStoreException, TimeoutException, PrecheckStatusException,
			ReceiptStatusException {
		final Client client = CommonMethods.getClient(NetworkEnum.INTEGRATION);
		final PrivateKey genesisKey = PrivateKey.fromBytes(Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(),
				"src/test/resources/Keys/genesis.pem").get(0).getPrivate().getEncoded());
		client.setOperator(new AccountId(0, 0, 2), genesisKey);

		final TransactionResponse response = new AccountCreateTransaction()
				.setKey(keys)
				.setInitialBalance(Hbar.fromTinybars(1000000))
				.execute(client);
		final TransactionReceipt receipt = response.getReceipt(client);

		final AccountId newAccountId = receipt.accountId;
		assert newAccountId != null;

		return new AccountInfoQuery().setAccountId(newAccountId).execute(client);
	}
}