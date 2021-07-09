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

package com.hedera.hashgraph.client.core.queries;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Query;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Collections;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.FILE_DOES_NOT_EXIST_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.MISSING_FIELD_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_KEY_LOCATION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.utils.JsonUtils.jsonToHBars;

public class GetAccountInfoQuery extends ToolQuery {
	Logger logger = LogManager.getLogger(GetAccountInfoQuery.class);

	public GetAccountInfoQuery(JsonObject input) throws HederaClientException {
		super(input);
	}

	@Override
	public Query build() throws HederaClientRuntimeException, HederaClientException {
		super.query =
				new AccountInfoQuery().setAccountId(jsonToAccountId(input.get(ACCOUNT_ID_FIELD_NAME).getAsJsonObject()))
						.setNodeAccountIds(Collections.singletonList(
								jsonToAccountId(input.get(NODE_ID_FIELD_NAME).getAsJsonObject())))
						.setMaxQueryPayment(jsonToHBars(input.getAsJsonObject(TRANSACTION_FEE_FIELD_NAME)));
		return query;
	}

	AccountId jsonToAccountId(JsonObject accountJson) throws HederaClientException {
		return Identifier.parse(accountJson).asAccount();
	}


	@Override
	public TransactionReceipt submit() throws HederaClientRuntimeException {
		return super.submit();
	}

	@Override
	public boolean checkInput(JsonObject input) {
		return isIdentifier(input, FEE_PAYER_ACCOUNT_FIELD_NAME) &&
				isIdentifier(input, NODE_ID_FIELD_NAME) &&
				isAccountArray(input) &&
				isKeyLocation(input) &&
				isTransactionFee(input) &&
				isNetwork(input);
	}

	private boolean isNetwork(JsonObject input) {
		try {
			if (input.has(NETWORK_FIELD_NAME)) {
				NetworkEnum.valueOf(input.get(NETWORK_FIELD_NAME).getAsString());
				return true;
			}
			logger.error(MISSING_FIELD_ERROR_MESSAGE, NETWORK_FIELD_NAME);
			return false;
		} catch (IllegalArgumentException e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, NETWORK_FIELD_NAME);
			return false;
		}
	}

	private boolean isTransactionFee(JsonObject input) {
		try {
			if (input.has(TRANSACTION_FEE_FIELD_NAME)) {
				jsonToHBars(input.getAsJsonObject(TRANSACTION_FEE_FIELD_NAME));
				return true;
			}
			logger.error(MISSING_FIELD_ERROR_MESSAGE, TRANSACTION_FEE_FIELD_NAME);
			return false;
		} catch (ClassCastException e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, TRANSACTION_FEE_FIELD_NAME);
			return false;
		}
	}

	private boolean isKeyLocation(JsonObject input) {
		if (!input.has(FEE_PAYER_KEY_LOCATION)) {
			logger.error(MISSING_FIELD_ERROR_MESSAGE, FEE_PAYER_KEY_LOCATION);
			return false;
		}
		var keyLocation = input.get(FEE_PAYER_KEY_LOCATION).getAsString();

		if (new File(keyLocation).exists()) {
			return true;
		}
		logger.error(FILE_DOES_NOT_EXIST_ERROR_MESSAGE, FEE_PAYER_KEY_LOCATION);
		return false;
	}

	private boolean isAccountArray(JsonObject input) {
		try {
			if (!input.has(ACCOUNT_ID_FIELD_NAME)) {
				logger.error(MISSING_FIELD_ERROR_MESSAGE, ACCOUNT_ID_FIELD_NAME);
				return false;
			}
			var accounts = input.getAsJsonArray(ACCOUNT_ID_FIELD_NAME);
			for (var account : accounts) {
				Identifier.parse(account.getAsJsonObject());
			}
		} catch (HederaClientException e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, NODE_ID_FIELD_NAME);
			return false;
		}
		return true;
	}

	private boolean isIdentifier(JsonObject input, String fieldName) {
		try {
			if (!input.has(fieldName)) {
				logger.error(MISSING_FIELD_ERROR_MESSAGE, fieldName);
				return false;
			}
			var feePayer = input.getAsJsonObject(fieldName);
			Identifier.parse(feePayer);
		} catch (Exception e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, fieldName);
			return false;
		}
		return true;
	}
}
