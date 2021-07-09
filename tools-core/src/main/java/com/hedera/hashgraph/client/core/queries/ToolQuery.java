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
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.interfaces.SDKInterface;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.Query;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.ParseException;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_VALIDATE_INPUT_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.utils.JsonUtils.jsonToHBars;

public class ToolQuery implements SDKInterface, GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(ToolQuery.class);

	JsonObject input;
	Query<?, ?> query;

	// Common fields to build the query
	Identifier feePayerID;
	Identifier nodeID;
	Hbar transactionFee;
	Instant transactionValidStart;
	Duration transactionValidDuration;
	NetworkEnum network;

	public ToolQuery(JsonObject input) throws HederaClientException {
		if (checkInput(input)) {
			this.input = input;
		} else {
			throw new HederaClientException(CANNOT_VALIDATE_INPUT_ERROR_MESSAGE);
		}
		query = null;
	}


	@Override
	public byte[] sign(PrivateKey key) throws HederaClientRuntimeException {
		return new byte[0];
	}

	@Override
	public Transaction<?> collate(Map<PublicKey, byte[]> signatures) throws HederaClientRuntimeException {
		return null;
	}

	@Override
	public Transaction<?> collate(Set<SignaturePair> signaturePairs) throws HederaClientRuntimeException {
		return null;
	}

	@Override
	public Transaction<?> collate(Transaction<?> otherTransaction) throws HederaClientRuntimeException {
		return null;
	}

	@Override
	public boolean verify(PublicKey publicKey) throws HederaClientRuntimeException {
		return false;
	}

	@Override
	public boolean verify(AccountInfo info) throws HederaClientException {
		return false;
	}

	@Override
	public TransactionReceipt submit() throws HederaClientRuntimeException {
		return null;
	}

	@Override
	public boolean checkInput(JsonObject input) {
		var answer = true;

		// Checks the common fields in the input
		if (!CommonMethods.verifyFieldExist(input, FEE_PAYER_ACCOUNT_FIELD_NAME, NODE_ID_FIELD_NAME,
				TRANSACTION_FEE_FIELD_NAME,
				NETWORK_FIELD_NAME,
				TRANSACTION_VALID_START_FIELD_NAME, TRANSACTION_VALID_DURATION_FIELD_NAME)) {
			return false;
		}

		try {
			var feePayer = input.getAsJsonObject(FEE_PAYER_ACCOUNT_FIELD_NAME);
			feePayerID = Identifier.parse(feePayer);
		} catch (HederaClientException e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, FEE_PAYER_ACCOUNT_FIELD_NAME);
			answer = false;
		}
		try {
			var node = input.getAsJsonObject(NODE_ID_FIELD_NAME);
			nodeID = Identifier.parse(node);
		} catch (HederaClientException e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, NODE_ID_FIELD_NAME);
			answer = false;
		}

		try {
			transactionFee = jsonToHBars(input.get(TRANSACTION_FEE_FIELD_NAME).getAsJsonObject());
		} catch (NumberFormatException e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, TRANSACTION_FEE_FIELD_NAME);
			answer = false;
		}

		try {
			network = NetworkEnum.valueOf(input.get(NETWORK_FIELD_NAME).getAsString());
		} catch (IllegalArgumentException e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, NETWORK_FIELD_NAME);
			answer = false;
		}

		try {
			transactionValidStart = new Timestamp(input.get(TRANSACTION_VALID_START_FIELD_NAME)).asInstant();
		} catch (NumberFormatException | ParseException e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, TRANSACTION_VALID_START_FIELD_NAME);
			answer = false;
		}

		try {
			transactionValidDuration = Duration.ofSeconds(input.get(TRANSACTION_VALID_DURATION_FIELD_NAME).getAsLong());
		} catch (Exception e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, TRANSACTION_VALID_DURATION_FIELD_NAME);
		}

		return answer;
	}

	@Override
	public TransactionId getTransactionId() {
		return null;
	}

	@Override
	public String store(String location) {
		return location;
	}

	@Override
	public boolean read(String location) throws HederaClientException {
		return false;
	}

	@Override
	public JsonObject asJson() {
		return null;
	}

	@Override
	public byte[] toBytes() {
		return new byte[0];
	}

	/**
	 * Uses a verified json input to build a query
	 */
	public Query<?, ?> build() throws HederaClientException {
		return null;
	}

}
