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
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransactionResponse;
import com.hedera.hashgraph.sdk.TransferTransaction;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStoreException;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.helpers.TestHelpers.getJsonInputCA;
import static com.hedera.hashgraph.client.core.helpers.TestHelpers.getJsonInputCT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolTransferTransactionTest implements GenericFileReadWriteAware {

	private final long sender = 2;
	private final long receiver = 50;

	@Test
	void build_test() throws HederaClientException {
		JsonObject testJson = getJsonInputCT(50, sender, receiver, new Timestamp(20).asInstant());

		ToolTransferTransaction transaction = new ToolTransferTransaction(testJson);
		assertTrue(transaction.getTransaction() instanceof TransferTransaction);
		TransferTransaction transfer = (TransferTransaction) transaction.getTransaction();
		assertEquals(2, transfer.getHbarTransfers().size());


	}

	@Test
	void create_test() throws HederaClientException {
		JsonObject jsonObject = getJsonInputCA(50);
		final String keyName = "src/test/resources/Keys/genesis.pub";
		JsonObject singleKeyJson = new JsonObject();
		String pubKey;
		try {
			pubKey = new String(
					Files.readAllBytes(Path.of(keyName.replace(Constants.PK_EXTENSION, Constants.PUB_EXTENSION))));
		} catch (IOException e) {
			throw new HederaClientException("Could not load public key from file");
		}
		singleKeyJson.addProperty("Ed25519", pubKey);


		jsonObject.add(JsonConstants.NEW_KEY_FIELD_NAME, singleKeyJson);
		jsonObject.addProperty(JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, false);
		ToolCryptoCreateTransaction transaction = new ToolCryptoCreateTransaction(jsonObject);
		assertTrue(transaction.getTransaction() instanceof AccountCreateTransaction);
		AccountCreateTransaction create = (AccountCreateTransaction) transaction.getTransaction();

		assertEquals(Hbar.fromTinybars(50), create.getInitialBalance());

	}

	@Test
	void receiverRequiredAccounts_test() throws KeyStoreException, PrecheckStatusException, TimeoutException,
			ReceiptStatusException, HederaClientException {
		var generalPrivateKey = PrivateKey.generate();
		var generalPublicKey = generalPrivateKey.getPublicKey();
		var client = CommonMethods.getClient(NetworkEnum.INTEGRATION);
		var keyStore =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");
		var genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());
		client.setOperator(new AccountId(0, 0, 2), genesisKey);


		for (int i = 0; i < 2; i++) {
			TransactionResponse transactionResponse = new AccountCreateTransaction()
					// The only _required_ property here is `key`
					.setKey(generalPublicKey)
					.setInitialBalance(Hbar.fromTinybars(1000000000))
					.execute(client);

			// This will wait for the receipt to become available
			TransactionReceipt receipt = transactionResponse.getReceipt(client);

			AccountId newAccountId = receipt.accountId;
			assert newAccountId != null;
			AccountInfo info = new AccountInfoQuery().setAccountId(newAccountId).execute(client);
			writeBytes("src/test/resources/AccountInfos/" + newAccountId + ".info", info.toBytes());
		}

	}
}