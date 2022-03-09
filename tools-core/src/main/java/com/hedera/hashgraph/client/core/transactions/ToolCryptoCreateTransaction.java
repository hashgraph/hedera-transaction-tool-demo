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
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.time.Duration;
import java.util.Collections;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.AUTO_RENEW_PERIOD_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.INITIAL_BALANCE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NEW_KEY_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME;
import static com.hedera.hashgraph.client.core.utils.JsonUtils.jsonToHBars;

public class ToolCryptoCreateTransaction extends ToolTransaction {

	private Hbar initialBalance;
	private KeyList key;
	private Duration autoRenewDuration;
	private boolean receiverSignatureRequired;
	private static final Logger logger = LogManager.getLogger(ToolCryptoCreateTransaction.class);

	public ToolCryptoCreateTransaction(final JsonObject input) throws HederaClientException {
		super(input);
		this.transactionType = TransactionType.CRYPTO_CREATE;
	}

	public ToolCryptoCreateTransaction(final File inputFile) throws HederaClientException {
		super(inputFile);

		this.initialBalance = ((AccountCreateTransaction) transaction).getInitialBalance();
		this.key = (KeyList) ((AccountCreateTransaction) transaction).getKey();
		this.autoRenewDuration = ((AccountCreateTransaction) transaction).getAutoRenewPeriod();
		this.receiverSignatureRequired = ((AccountCreateTransaction) transaction).getReceiverSignatureRequired();
		setTransactionType(TransactionType.CRYPTO_CREATE);
	}

	public Hbar getInitialBalance() {
		return initialBalance;
	}

	public KeyList getKey() {
		return key;
	}

	public Duration getAutoRenewDuration() {
		return autoRenewDuration;
	}

	public boolean isReceiverSignatureRequired() {
		return receiverSignatureRequired;
	}

	@Override
	public boolean checkInput(final JsonObject input) {
		var answer = super.checkInput(input);

		if (!CommonMethods.verifyFieldExist(input, NEW_KEY_FIELD_NAME, AUTO_RENEW_PERIOD_FIELD_NAME,
				INITIAL_BALANCE_FIELD_NAME, RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME)) {
			return false;
		}

		try {
			final var keyAsJsonObject = input.getAsJsonObject(NEW_KEY_FIELD_NAME);
			this.key = EncryptionUtils.jsonToKey(keyAsJsonObject);
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, NEW_KEY_FIELD_NAME);
			answer = false;
		}


		try {
			this.autoRenewDuration = Duration.ofSeconds(input.get(AUTO_RENEW_PERIOD_FIELD_NAME).getAsLong());
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, AUTO_RENEW_PERIOD_FIELD_NAME);
			answer = false;
		}


		try {
			this.initialBalance = jsonToHBars(input.get(INITIAL_BALANCE_FIELD_NAME).getAsJsonObject());
		} catch (final NumberFormatException e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, INITIAL_BALANCE_FIELD_NAME);
			answer = false;
		}

		try {
			this.receiverSignatureRequired = input.get(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME).getAsBoolean();
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME);
			answer = false;
		}

		return answer;
	}

	@Override
	public Transaction<?> build() throws HederaClientRuntimeException {
		final var transactionId =
				new TransactionId(feePayerID.asAccount(), transactionValidStart);

		final var accountCreateTransaction = new AccountCreateTransaction();

		return accountCreateTransaction
				.setKey(key)
				.setInitialBalance(initialBalance)
				.setAutoRenewPeriod(autoRenewDuration)
				.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
				.setTransactionId(transactionId)
				.setMaxTransactionFee(transactionFee)
				.setTransactionMemo(memo)
				.setTransactionValidDuration(transactionValidDuration)
				.setReceiverSignatureRequired(receiverSignatureRequired)
				.freeze();
	}

	@Override
	public Set<ByteString> getSigningKeys(final String accountsInfoFolder) {
		final var keysSet = super.getSigningKeys(accountsInfoFolder);
		keysSet.addAll(EncryptionUtils.flatPubKeys(
				Collections.singletonList(((AccountCreateTransaction) transaction).getKey())));
		return keysSet;
	}

	@Override
	public boolean equals(final Object obj) {
		return super.equals(obj);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public JsonObject asJson() {
		final var asJson = super.asJson();
		asJson.addProperty("initialBalance", initialBalance.toTinybars());
		asJson.add("key", EncryptionUtils.keyToJson(key));
		asJson.addProperty("autoRenewDuration", autoRenewDuration.getSeconds());
		asJson.addProperty("receiverSignatureRequired", receiverSignatureRequired);
		return asJson;
	}
}
