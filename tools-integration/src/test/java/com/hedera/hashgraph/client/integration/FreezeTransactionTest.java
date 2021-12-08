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

package com.hedera.hashgraph.client.integration;

import com.google.gson.JsonArray;
import com.google.gson.JsonIOException;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.sdk.AccountBalanceQuery;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.FreezeTransaction;
import com.hedera.hashgraph.sdk.FreezeType;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransferTransaction;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.FileReader;
import java.io.IOException;
import java.security.KeyStoreException;
import java.time.Duration;
import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.REALM_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.SHARD_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static org.junit.Assert.assertTrue;

/**
 * WARNING: THIS TEST CLASS SHOULD NOT BE RUN DURING AUTOMATIC TESTING. CLASS DEPENDS ON A LOCAL NETWORK THAT IS NOT
 * GUARANTEED TO EXIST
 */

public class FreezeTransactionTest {
	private static final Logger logger = LogManager.getLogger(FreezeTransactionTest.class);

	private static Client client;
	private static PrivateKey genesisKey;

	@Before
	public void setUp() throws Exception {
		// Setup the client for network
		setupClient();
		// Fund account 50 if needed
		var transferTransaction = new TransferTransaction();
		final var transactionValidStart = Instant.now();
		TransactionId transactionId = new TransactionId(new AccountId(0, 0, 2), transactionValidStart);
		transferTransaction.setMaxTransactionFee(Hbar.fromTinybars(1000000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("test transfer")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.setTransactionValidDuration(Duration.ofSeconds(175));

		// Add transfers
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 2), Hbar.fromTinybars(-100000000));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 50), Hbar.fromTinybars(100000000));
		transferTransaction.freeze();
		transferTransaction.sign(genesisKey);
		var response = transferTransaction.execute(client);
		assert response != null;

		final var status = response.getReceipt(client).status;
		logger.info("Worker: Transaction: {}, final status: {}", transactionId.toString(), status);

		// Update file 150
		// Calculate file 150's checksum
	}

	@NotNull
	private void setupClient() throws KeyStoreException {
		var keyStore =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/KeyFiles/genesis.pem");
		genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());

		Map<String, AccountId> network = new HashMap<>();
		var jsonArray = getIPS("src/test/resources/homeNodes.json");
		for (var jsonElement : jsonArray) {
			var node = jsonElement.getAsJsonObject();
			network.put(
					String.format("%s:%s", node.get("ipAddress").getAsString(), node.get("port").getAsString()),
					new AccountId(node.get("accountID").getAsInt()));
		}
		client = Client.forNetwork(network);
		final var treasury = new AccountId(0, 0, 2);
		client.setOperator(treasury, genesisKey);
	}

	@Test
	public void freezeTransactionBuild_test() throws HederaClientException, PrecheckStatusException, TimeoutException {
		// Check 50 has a balance
		Hbar balance = new AccountBalanceQuery()
				.setAccountId(new AccountId(0, 0, 50))
				.execute(client)
				.hbars;

		assertTrue(balance.toTinybars() > 0);

		// Create a FREEZE transaction
		final var transactionValidStart = Instant.now();
		final var startFreeze = new Timestamp(Instant.now().plusSeconds(100));

		TransactionId transactionId = new TransactionId(new AccountId(0, 0, 50), transactionValidStart);
		FreezeTransaction freezeTransaction = new FreezeTransaction().setTransactionId(transactionId)
				.setFreezeType(FreezeType.FREEZE_ONLY)
				.setStartTime(startFreeze.asInstant())
				.setMaxTransactionFee(Hbar.fromTinybars(100000000))
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.freeze();

		freezeTransaction.sign(genesisKey);

		client.setOperator(new AccountId(0, 0, 50), genesisKey);
		var response = freezeTransaction.execute(client);
		assert response != null;
		// Assert network is down
		balance = new AccountBalanceQuery()
				.setAccountId(new AccountId(0, 0, 50))
				.execute(client)
				.hbars;
	}

	@After
	public void tearDown() throws Exception {

	}

	/**
	 * Read the integration file into a json array
	 *
	 * @return a json array that contains the IPs of the integration network
	 */
	private static JsonArray getIPS(String nodes) {
		// Read file into object
		try (var file = new FileReader(nodes)) {
			return JsonParser.parseReader(file).getAsJsonArray();
		} catch (JsonIOException | JsonSyntaxException | IOException cause) {
			logger.error(cause);
		}
		return new JsonArray();
	}

	@NotNull
	private JsonObject getBasicJsonObject() {
		JsonObject testJson = new JsonObject();
		JsonObject feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 100000000);

		JsonObject feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, 50);

		testJson.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccount);

		JsonObject node = new JsonObject();
		node.addProperty(REALM_NUMBER, 0);
		node.addProperty(SHARD_NUMBER, 0);
		node.addProperty(ACCOUNT_NUMBER, 3);

		testJson.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		testJson.add(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp().asJSON());

		testJson.add(NODE_ID_FIELD_NAME, node);
		testJson.addProperty(NETWORK_FIELD_NAME, NetworkEnum.CUSTOM.toString());

		testJson.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, 120);

		testJson.addProperty(MEMO_FIELD_NAME, "a memo to go with the transaction");
		return testJson;
	}


}
