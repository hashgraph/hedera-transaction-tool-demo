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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import helpers.TestConstants;

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
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.REALM_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME;
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
	public static JsonObject getJsonInputCA(final long tinyBars) {
		final var testJson = new JsonObject();

		final var key = new File(TestConstants.RESOURCES_DIRECTORY + "/Keys/genesis.pem");

		final var balanceJson = new JsonObject();
		balanceJson.addProperty(H_BARS, 0);
		balanceJson.addProperty(TINY_BARS, tinyBars);

		final var feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 100000000);

		final var feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, 2);

		final var node = new JsonObject();
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

		testJson.addProperty(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, false);
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
	public static JsonObject getJsonInputCT(
			final long tinyBars, final long fromAccount, final long toAccount, final Instant startInstant) {
		final var testJson = new JsonObject();
		final var key = new File(TestConstants.RESOURCES_DIRECTORY + "/Keys/genesis.pem");

		final var feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 100000000);

		final var feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, 2);

		final var node = new JsonObject();
		node.addProperty(REALM_NUMBER, 0);
		node.addProperty(SHARD_NUMBER, 0);
		node.addProperty(ACCOUNT_NUMBER, 3);

		testJson.addProperty(FEE_PAYER_KEY_LOCATION, key.getAbsolutePath());
		testJson.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccount);
		testJson.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(10).asRFCString());

		testJson.add(NODE_ID_FIELD_NAME, node);
		testJson.addProperty(NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());

		final var jsonArray = new JsonArray();
		final var from = new JsonObject();
		from.add(ACCOUNT, new Identifier(0, 0, fromAccount).asJSON());
		from.addProperty(AMOUNT, -tinyBars);

		final var to = new JsonObject();
		to.add(ACCOUNT, new Identifier(0, 0, toAccount).asJSON());
		to.addProperty(AMOUNT, tinyBars);
		jsonArray.add(from);
		jsonArray.add(to);

		testJson.add(TRANSFERS, jsonArray);

		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(startInstant).asRFCString());

		testJson.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, 120);

		return testJson;

	}

	public static JsonObject getJsonInput(final long... accountNums) {
		final var testJson = new JsonObject();

		final var key = new File(TestConstants.RESOURCES_DIRECTORY + "/Keys/genesis.pem");

		final var feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, 2);

		final var accountArray = new JsonArray();
		for (final var num : accountNums) {
			final var account = new JsonObject();
			account.addProperty(REALM_NUMBER, 0);
			account.addProperty(SHARD_NUMBER, 0);
			account.addProperty(ACCOUNT_NUMBER, num);
			accountArray.add(account);
		}

		final var feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 10000000);

		final var node = new JsonObject();
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
}
