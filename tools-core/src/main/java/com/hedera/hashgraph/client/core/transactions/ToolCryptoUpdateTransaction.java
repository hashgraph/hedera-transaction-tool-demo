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
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountUpdateTransaction;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.time.Duration;
import java.util.Collections;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_TO_UPDATE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AUTO_RENEW_PERIOD_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NEW_KEY_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME;

public class ToolCryptoUpdateTransaction extends ToolTransaction {

	private Identifier account;
	private KeyList key;
	private Duration autoRenewDuration;
	private Boolean receiverSignatureRequired;
	private static final Logger logger = LogManager.getLogger(ToolCryptoUpdateTransaction.class);

	public ToolCryptoUpdateTransaction(final JsonObject input) throws HederaClientException {
		super(input);
		this.transactionType = TransactionType.CRYPTO_UPDATE;
	}

	public ToolCryptoUpdateTransaction(final File inputFile) throws HederaClientException {
		super(inputFile);
		this.account = new Identifier(((AccountUpdateTransaction) transaction).getAccountId());
		this.key = (KeyList) ((AccountUpdateTransaction) transaction).getKey();
		this.autoRenewDuration = ((AccountUpdateTransaction) transaction).getAutoRenewPeriod();
		this.receiverSignatureRequired = ((AccountUpdateTransaction) transaction).getReceiverSignatureRequired();
		setTransactionType(TransactionType.CRYPTO_UPDATE);
	}

	public KeyList getKey() {
		return key;
	}

	public Duration getAutoRenewDuration() {
		return autoRenewDuration;
	}

	public Boolean isReceiverSignatureRequired() {
		return receiverSignatureRequired;
	}

	public Identifier getAccount() {
		return account;
	}

	@Override
	public boolean checkInput(final JsonObject input) {
		var answer = super.checkInput(input);
		if (!input.has(ACCOUNT_TO_UPDATE)) {
			logger.error("The input json does not contain the account ID to update");
			return false;
		}

		if (!CommonMethods.verifyOneOfExists(input, NEW_KEY_FIELD_NAME, AUTO_RENEW_PERIOD_FIELD_NAME,
				RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME)) {
			return false;
		}

		try {
			final var accountIdJson = input.getAsJsonObject(ACCOUNT_TO_UPDATE);
			account = Identifier.parse(accountIdJson);
		} catch (final HederaClientException | ClassCastException e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, ACCOUNT_TO_UPDATE);
			answer = false;
		}

		try {
			if (input.has(NEW_KEY_FIELD_NAME)) {
				final var keyAsJsonObject = input.getAsJsonObject(NEW_KEY_FIELD_NAME);
				this.key = EncryptionUtils.jsonToKey(keyAsJsonObject);
			}
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, NEW_KEY_FIELD_NAME);
			answer = false;
		}


		try {
			if (input.has(AUTO_RENEW_PERIOD_FIELD_NAME)) {
				this.autoRenewDuration = Duration.ofSeconds(input.get(AUTO_RENEW_PERIOD_FIELD_NAME).getAsLong());
			}
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, AUTO_RENEW_PERIOD_FIELD_NAME);
			answer = false;
		}

		try {
			if (input.has(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME)) {
				this.receiverSignatureRequired = input.get(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME).getAsBoolean();
			}
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

		final var accountUpdateTransaction = new AccountUpdateTransaction().setAccountId(account.asAccount());

		if (key != null) {
			accountUpdateTransaction.setKey(key);
		}
		if (autoRenewDuration != null) {
			accountUpdateTransaction.setAutoRenewPeriod(autoRenewDuration);
		}
		if (receiverSignatureRequired != null) {
			accountUpdateTransaction.setReceiverSignatureRequired(receiverSignatureRequired);
		}

		return accountUpdateTransaction
				.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
				.setTransactionId(transactionId)
				.setMaxTransactionFee(transactionFee)
				.setTransactionMemo(memo)
				.setTransactionValidDuration(transactionValidDuration)
				.freeze();
	}

	@Override
	public Set<ByteString> getSigningKeys(final String accountsInfoFolder) {
		final var keysSet = super.getSigningKeys(accountsInfoFolder);
		final var keyFromTransaction = ((AccountUpdateTransaction) transaction).getKey();
		if (keyFromTransaction != null) {
			keysSet.addAll(EncryptionUtils.flatPubKeys(Collections.singletonList(keyFromTransaction)));
		}
		return keysSet;
	}

	@Override
	public Set<AccountId> getSigningAccounts() {
		final var accountsSet = super.getSigningAccounts();
		accountsSet.add(((AccountUpdateTransaction) transaction).getAccountId());
		return accountsSet;
	}

	@Override
	public boolean equals(final Object obj) {
		return super.equals(obj);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
