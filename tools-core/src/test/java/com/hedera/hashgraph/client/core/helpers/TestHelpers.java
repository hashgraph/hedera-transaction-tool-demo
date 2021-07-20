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
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.helpers.TestConstants;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.sdk.Key;

import java.io.File;
import java.time.Instant;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AMOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AUTO_RENEW_PERIOD_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_KEY_LOCATION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.INITIAL_BALANCE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.REALM_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.SHARD_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSFERS;

public class TestHelpers {

	/**
	 * Create a json input for testing crypto create accounts
	 *
	 * @param tinyBars
	 * 		initial balance
	 * @return a Json object
	 */
	public static JsonObject getJsonInputCA(long tinyBars) {
		JsonObject testJson = new JsonObject();

		File key = new File(TestConstants.RESOURCES_DIRECTORY + "/Keys/genesis.pem");

		JsonObject balanceJson = new JsonObject();
		balanceJson.addProperty(H_BARS, 0);
		balanceJson.addProperty(TINY_BARS, tinyBars);

		JsonObject feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 100000000);

		JsonObject feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, 2);

		JsonObject node = new JsonObject();
		node.addProperty(REALM_NUMBER, 0);
		node.addProperty(SHARD_NUMBER, 0);
		node.addProperty(ACCOUNT_NUMBER, 3);

		testJson.addProperty(FEE_PAYER_KEY_LOCATION, key.getAbsolutePath());
		testJson.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccount);
		testJson.add(TRANSACTION_FEE_FIELD_NAME, feeJson);
		testJson.add(INITIAL_BALANCE_FIELD_NAME, balanceJson);

		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(10).asRFCString());

		testJson.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, 120);

		testJson.addProperty(AUTO_RENEW_PERIOD_FIELD_NAME, 7776000);

		testJson.add(NODE_ID_FIELD_NAME, node);
		testJson.addProperty(NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());
		return testJson;
	}

	/**
	 * Create a json input for testing single transfers
	 *
	 * @param tinyBars
	 * 		amount to be transferred between accounts
	 * @param fromAccount
	 * 		sender
	 * @param toAccount
	 * 		receiver
	 * @param startInstant
	 * 		start Instant
	 * @return a json object
	 */
	public static JsonObject getJsonInputCT(long tinyBars, long fromAccount, long toAccount, Instant startInstant) {
		JsonObject testJson = new JsonObject();
		File key = new File(TestConstants.RESOURCES_DIRECTORY + "/Keys/genesis.pem");

		JsonObject feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 100000000);

		JsonObject feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, fromAccount);

		JsonObject node = new JsonObject();
		node.addProperty(REALM_NUMBER, 0);
		node.addProperty(SHARD_NUMBER, 0);
		node.addProperty(ACCOUNT_NUMBER, 3);

		testJson.addProperty(FEE_PAYER_KEY_LOCATION, key.getAbsolutePath());
		testJson.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccount);
		testJson.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(10).asRFCString());

		testJson.add(NODE_ID_FIELD_NAME, node);
		testJson.addProperty(NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());

		JsonArray jsonArray = new JsonArray();
		JsonObject from = new JsonObject();
		from.add(ACCOUNT, new Identifier(0, 0, fromAccount).asJSON());
		from.addProperty(AMOUNT, -tinyBars);

		JsonObject to = new JsonObject();
		to.add(ACCOUNT, new Identifier(0, 0, toAccount).asJSON());
		to.addProperty(AMOUNT, tinyBars);
		jsonArray.add(from);
		jsonArray.add(to);

		testJson.add(TRANSFERS, jsonArray);

		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(startInstant).asRFCString());

		testJson.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, 120);

		testJson.addProperty(MEMO_FIELD_NAME, "a memo to go with the transaction");

		return testJson;

	}

	public static JsonObject getJsonInput(long... accountNums) {
		JsonObject testJson = new JsonObject();

		File key = new File(TestConstants.RESOURCES_DIRECTORY + "/Keys/genesis.pem");

		JsonObject feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, 2);

		JsonArray accountArray = new JsonArray();
		for (long num : accountNums) {
			JsonObject account = new JsonObject();
			account.addProperty(REALM_NUMBER, 0);
			account.addProperty(SHARD_NUMBER, 0);
			account.addProperty(ACCOUNT_NUMBER, num);
			accountArray.add(account);
		}

		JsonObject feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 10000000);

		JsonObject node = new JsonObject();
		node.addProperty(REALM_NUMBER, 0);
		node.addProperty(SHARD_NUMBER, 0);
		node.addProperty(ACCOUNT_NUMBER, 3);

		testJson.addProperty(FEE_PAYER_KEY_LOCATION, key.getAbsolutePath());
		testJson.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccount);
		testJson.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		testJson.add(ACCOUNT_ID_FIELD_NAME, accountArray);
		testJson.add(NODE_ID_FIELD_NAME, node);
		testJson.addProperty(NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());
		return testJson;
	}

	public static JsonObject buildUpdateJson(int feePayer, Instant startInstant) {
		JsonObject testJson = new JsonObject();
		File key = new File(TestConstants.RESOURCES_DIRECTORY + "/Keys/genesis.pem");

		JsonObject feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 100000000);

		JsonObject feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, feePayer);

		JsonObject node = new JsonObject();
		node.addProperty(REALM_NUMBER, 0);
		node.addProperty(SHARD_NUMBER, 0);
		node.addProperty(ACCOUNT_NUMBER, 3);

		testJson.addProperty(FEE_PAYER_KEY_LOCATION, key.getAbsolutePath());
		testJson.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccount);
		testJson.add(TRANSACTION_FEE_FIELD_NAME, feeJson);
		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(10).asRFCString());
		testJson.add(NODE_ID_FIELD_NAME, node);
		testJson.addProperty(NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());
		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(startInstant).asRFCString());
		testJson.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, 120);
		testJson.addProperty(MEMO_FIELD_NAME, "a memo to go with the transaction");
		return testJson;
	}
}
