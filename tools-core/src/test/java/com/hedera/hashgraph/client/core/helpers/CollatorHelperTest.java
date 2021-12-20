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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransferTransaction;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CollatorHelperTest implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(CollatorHelperTest.class);

	@AfterEach
	void tearDown() throws IOException {
		Files.deleteIfExists(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.txsig").toPath());
	}

	@Test
	void constructor_test() throws HederaClientException {
		var helper = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig"));
		assertNull(helper.getTransaction()); // empty transaction
		assertEquals("0-0-2_1678312256-0", helper.getBaseName());
		assertEquals("", helper.getTransactionFile());
		assertEquals(1, helper.getSignaturePairs().size());
		assertEquals(new JsonArray(), helper.getComments().get("comments").getAsJsonArray());

		helper = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));
		assertNotNull(helper.getTransaction().getTransaction());
		assertEquals("0-0-2_1678312256-0", helper.getBaseName());
		assertEquals(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx").getAbsolutePath(),
				helper.getTransactionFile());
		assertEquals(0, helper.getSignaturePairs().size());
		assertEquals(new JsonArray(), helper.getComments().get("comments").getAsJsonArray());

		helper = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/testCSV.txt"));
		assertNull(helper.getTransaction());
		assertEquals("testCSV", helper.getBaseName());
		assertEquals("", helper.getTransactionFile());
		assertEquals(0, helper.getSignaturePairs().size());
		var array = helper.getComments().get("comments").getAsJsonArray();
		assertEquals(1, array.size());
		JsonObject comment = array.get(0).getAsJsonObject();
		assertEquals("This comment has a title", comment.get("Title").getAsString());
		assertEquals("test comment for test csv", comment.get("Contents").getAsString());

		Exception e = assertThrows(HederaClientException.class, () -> new CollatorHelper(
				new File("src/test/resources/collation_test/testCSV_aharris-saft_Node-0-0-4_summary.csv")));
		assertEquals("Hedera Client: Cannot parse: testCSV_aharris-saft_Node-0-0-4_summary.csv", e.getMessage());
	}

	@Test
	void add_test() throws HederaClientException {
		var helper1 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig"));
		assertNull(helper1.getTransaction()); // empty transaction
		assertFalse(helper1.hasTransaction());

		helper1.addTransaction(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));
		assertTrue(helper1.hasTransaction());
		assertNotNull(helper1.getTransaction().getTransaction());
		assertEquals(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx").getAbsolutePath(),
				helper1.getTransactionFile());
		assertEquals(1, helper1.getSignaturePairs().size());
		assertEquals(new JsonArray(), helper1.getComments().get("comments").getAsJsonArray());

		var helper2 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));
		assertNotNull(helper2.getTransaction()); // empty transaction

		final var signaturePair = new SignaturePair("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig");
		helper2.addSignature(signaturePair.getPublicKey(), signaturePair.getSignature());
		assertNotNull(helper2.getTransaction().getTransaction());
		assertEquals(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx").getAbsolutePath(),
				helper2.getTransactionFile());
		assertEquals(1, helper2.getSignaturePairs().size());
		assertEquals(new JsonArray(), helper2.getComments().get("comments").getAsJsonArray());

		assertEquals(helper1, helper2);

		helper1.addComments(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.txt"));
		helper2.addComments(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.txt"));

		assertEquals(helper1, helper2);

	}

	@Test
	void verify_test() throws HederaClientException, InvalidProtocolBufferException {
		final var info = AccountInfo.fromBytes(readBytes("src/test/resources/infos/0.0.2.info"));

		var helper1 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig"));
		helper1.addTransaction(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));
		var tx = helper1.collate();
		assertEquals(1, tx.getSignatures().size());
		var node3 = tx.getSignatures().get(new AccountId(0, 0, 3));
		assertEquals(1, node3.size());
		assertFalse(helper1.verify(info));

		var signer = new SignaturePair("src/test/resources/CollatorHelperFiles/main_signer.sig");
		helper1.addSignature(signer.getPublicKey(), signer.getSignature());
		signer = new SignaturePair("src/test/resources/CollatorHelperFiles/signer1.sig");
		helper1.addSignature(signer.getPublicKey(), signer.getSignature());
		signer = new SignaturePair("src/test/resources/CollatorHelperFiles/signer2.sig");
		helper1.addSignature(signer.getPublicKey(), signer.getSignature());
		tx = helper1.collate();
		node3 = tx.getSignatures().get(new AccountId(0, 0, 3));
		assertEquals(4, node3.size());
		assertFalse(helper1.verify(info));
		// Since the public keys don't correspond to the private keys, the signature verification fails

	}

	@Test
	void addHelper_test() throws HederaClientException {
		var helper1 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig"));
		var helper2 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));

		assertNull(helper1.getTransaction()); // empty transaction
		assertFalse(helper1.hasTransaction());

		helper1.addHelper(helper2);
		assertNotNull(helper1.getTransaction()); // empty transaction
		assertTrue(helper1.hasTransaction());

	}

	@Test
	void addHelper_differentTx() throws HederaClientException {
		var helper1 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/Transactions/Signer1/0_0_76@1639746360_10000-0_0_10749.tx"));
		var helper2 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/Transactions/Signer2/0_0_76@1639746360_10000-0_0_10749.tx"));

		assertNotNull(helper1.getTransaction()); // empty transaction
		assertTrue(helper1.hasTransaction());
		assertNotNull(helper2.getTransaction()); // empty transaction
		assertTrue(helper2.hasTransaction());

		var exception = assertThrows(HederaClientException.class, () -> helper1.addHelper(helper2));
		assertEquals("Hedera Client: Transactions don't match", exception.getMessage());
	}
	@Test
	void addAllSignatures_test() throws HederaClientException {
		var helper1 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig"));
		var helper2 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));

		helper1.addSignature(new SignaturePair("src/test/resources/CollatorHelperFiles/main_signer.sig"));
		helper1.addSignature(new SignaturePair("src/test/resources/CollatorHelperFiles/signer1.sig"));
		helper1.addSignature(new SignaturePair("src/test/resources/CollatorHelperFiles/signer2.sig"));

		assertEquals(4, helper1.getSignaturePairs().size());
		assertEquals(0, helper2.getSignaturePairs().size());

		helper2.addAllSignatures(helper1);
		assertEquals(4, helper2.getSignaturePairs().size());
	}

	@Test
	void store_test() throws HederaClientException, InvalidProtocolBufferException {
		var helper = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));
		helper.addSignature(new SignaturePair("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig"));
		helper.addSignature(new SignaturePair("src/test/resources/CollatorHelperFiles/main_signer.sig"));
		helper.addSignature(new SignaturePair("src/test/resources/CollatorHelperFiles/signer1.sig"));
		helper.addSignature(new SignaturePair("src/test/resources/CollatorHelperFiles/signer2.sig"));

		var signed = new File(helper.store("key"));
		assertTrue(signed.exists());

		Transaction<?> transaction =
				Transaction.fromBytes(readBytes(signed + File.separator + "0-0-2_1678312256-0.txsig"));
		assertNotNull(transaction);
		assertTrue(transaction instanceof TransferTransaction);
		assertTrue(transaction.getSignatures().isEmpty());

		var signedTransaction = helper.collate();
		var signatures = signedTransaction.getSignatures().get(new AccountId(0, 0, 3));
		assertEquals(4, signatures.size());

		signed = new File(helper.store("key"));
		assertTrue(signed.exists());
		transaction =
				Transaction.fromBytes(readBytes(signed + File.separator + "0-0-2_1678312256-0.txsig"));
		assertNotNull(transaction);
		assertTrue(transaction instanceof TransferTransaction);
		signatures = transaction.getSignatures().get(new AccountId(0, 0, 3));
		assertEquals(4, signatures.size());
		for (SignaturePair signaturePair : helper.getSignaturePairs()) {
			assertTrue(signaturePair.getPublicKey().verifyTransaction(transaction));
		}
	}

	@Test
	void hashCode_test() throws HederaClientException {
		var helper1 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig"));
		var helper2 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));

		var helper3 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.sig"));
		var helper4 = new CollatorHelper(new File("src/test/resources/CollatorHelperFiles/0-0-2_1678312256-0.tx"));

		assertEquals(helper1.hashCode(), helper3.hashCode());
		assertEquals(helper2.hashCode(), helper4.hashCode());
	}
}