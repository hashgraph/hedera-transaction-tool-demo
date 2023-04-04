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
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_TO_UPDATE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AUTO_RENEW_PERIOD_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DECLINE_STAKING_REWARDS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MAX_TOKEN_ASSOCIATIONS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NEW_KEY_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.STAKED_ACCOUNT_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.STAKED_NODE_ID_FIELD_NAME;

public class ToolCryptoUpdateTransaction extends ToolTransaction {

	private Identifier account;
	private Key key;
	private Duration autoRenewDuration;
	private Boolean receiverSignatureRequired;
	private Integer maxTokenAssociations;
	private String accountMemo;
	private Set<String> updateList;
	private Identifier stakedAccountId;
	private Long stakedNodeId;
	private Boolean declineStakingRewards;

	private static final Logger logger = LogManager.getLogger(ToolCryptoUpdateTransaction.class);

	public ToolCryptoUpdateTransaction(final JsonObject input) throws HederaClientException {
		super(input);
		this.transactionType = TransactionType.CRYPTO_UPDATE;
	}

	public ToolCryptoUpdateTransaction(final File inputFile) throws HederaClientException {
		super(inputFile);
		this.account = new Identifier(((AccountUpdateTransaction) transaction).getAccountId());
		this.key = ((AccountUpdateTransaction) transaction).getKey();
		this.autoRenewDuration = ((AccountUpdateTransaction) transaction).getAutoRenewPeriod();
		this.receiverSignatureRequired = ((AccountUpdateTransaction) transaction).getReceiverSignatureRequired();
		this.maxTokenAssociations = ((AccountUpdateTransaction) transaction).getMaxAutomaticTokenAssociations();
		this.accountMemo = ((AccountUpdateTransaction) transaction).getAccountMemo();
		this.stakedAccountId = ((AccountUpdateTransaction) transaction).getStakedAccountId() == null ? null :
				new Identifier(((AccountUpdateTransaction) transaction).getStakedAccountId());
		this.stakedNodeId = ((AccountUpdateTransaction) transaction).getStakedNodeId();
		try {
			this.declineStakingRewards = ((AccountUpdateTransaction) transaction).getDeclineStakingReward();
		} catch (NullPointerException npe) {
			this.declineStakingRewards = null;
		}
		setTransactionType(TransactionType.CRYPTO_UPDATE);
	}

	public Key getKey() {
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

	public Integer getMaxTokenAssociations() {
		return maxTokenAssociations;
	}

	public String getAccountMemo() {
		return accountMemo;
	}

	public Identifier getStakedAccountId() {
		return stakedAccountId;
	}

	public Long getStakedNodeId() {
		return stakedNodeId;
	}

	public Boolean isDeclineStakingRewards() {
		return declineStakingRewards;
	}

	@Override
	protected KeyList buildKeyList(String accountsInfoFolder,
								   final Map<PublicKey, byte[]> signatures) throws HederaClientRuntimeException {
		// Get the keyList
		final var keyList = super.buildKeyList(accountsInfoFolder, signatures);
		// For every key in this.key that isn't in keyList, sign the transaction
		for (final var k : convertKeyToList(this.key)) {
			if (!keyListContainsKey(keyList, k)) {
				transaction.addSignature(k, signatures.get(k));
			}
		}
		// Return the keyList
		return keyList;
	}

	private List<PublicKey> convertKeyToList(final Key key) {
		final var newList = new ArrayList<PublicKey>();
		if (key instanceof PublicKey) {
			newList.add((PublicKey)key);
		}
		else {
			for (final var k : (KeyList)key) {
				newList.addAll(convertKeyToList(k));
			}
		}
		return newList;
	}

	private boolean keyListContainsKey(KeyList keyList, Key key) {
		// If the keyList contains the key, return true
		if (keyList.contains(key)) {
			return true;
		}
		// Otherwise, go through each key in the keyList. If it is a keyList,
		// then recall this method and determine if it contains the key
		for (final var k : keyList) {
			if (k instanceof KeyList && keyListContainsKey((KeyList)k, key)) {
				return true;
			}
		}
		// The key was not found, return false
		return false;
	}

	@Override
	public boolean checkInput(final JsonObject input) {
		var answer = super.checkInput(input);
		updateList = new HashSet<>();
		if (!input.has(ACCOUNT_TO_UPDATE)) {
			logger.error("The input json does not contain the account ID to update");
			return false;
		}

		if (!CommonMethods.verifyOneOfExists(input, NEW_KEY_FIELD_NAME, AUTO_RENEW_PERIOD_FIELD_NAME,
				RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, ACCOUNT_MEMO_FIELD_NAME, MAX_TOKEN_ASSOCIATIONS_FIELD_NAME,
				STAKED_NODE_ID_FIELD_NAME, STAKED_ACCOUNT_ID_FIELD_NAME, DECLINE_STAKING_REWARDS_FIELD_NAME)) {
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
				updateList.add("key");
			}
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, NEW_KEY_FIELD_NAME);
			answer = false;
		}


		try {
			if (input.has(AUTO_RENEW_PERIOD_FIELD_NAME)) {
				this.autoRenewDuration = Duration.ofSeconds(input.get(AUTO_RENEW_PERIOD_FIELD_NAME).getAsLong());
				updateList.add("auto renew duration");
			}
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, AUTO_RENEW_PERIOD_FIELD_NAME);
			answer = false;
		}

		try {
			if (input.has(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME)) {
				this.receiverSignatureRequired = input.get(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME).getAsBoolean();
				updateList.add("receiver signature required");
			}
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME);
			answer = false;
		}

		try {
			if (input.has(MAX_TOKEN_ASSOCIATIONS_FIELD_NAME)) {
				this.maxTokenAssociations = input.get(MAX_TOKEN_ASSOCIATIONS_FIELD_NAME).getAsInt();
				updateList.add("maximum number of token associations");
			}
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, MAX_TOKEN_ASSOCIATIONS_FIELD_NAME);
			answer = false;
		}

		try {
			if (input.has(ACCOUNT_MEMO_FIELD_NAME)) {
				this.accountMemo = input.get(ACCOUNT_MEMO_FIELD_NAME).getAsString();
				updateList.add("account memo");
			}
		} catch (final Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, ACCOUNT_MEMO_FIELD_NAME);
			answer = false;
		}

		try {
			stakedNodeId = input.has(STAKED_NODE_ID_FIELD_NAME) ? input.get(STAKED_NODE_ID_FIELD_NAME).getAsLong() : null;
			if (stakedNodeId != null) {
				updateList.add("staked node id");
			}
		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, STAKED_NODE_ID_FIELD_NAME);
			answer = false;
		}

		try {
			stakedAccountId = input.has(STAKED_ACCOUNT_ID_FIELD_NAME)
					? Identifier.parse(input.getAsJsonObject(STAKED_ACCOUNT_ID_FIELD_NAME)) : null;
			if (stakedAccountId != null) {
				updateList.add("staked account id");
			}
		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, STAKED_ACCOUNT_ID_FIELD_NAME);
			answer = false;
		}

		try {
			declineStakingRewards = input.has(DECLINE_STAKING_REWARDS_FIELD_NAME) ? input.get(DECLINE_STAKING_REWARDS_FIELD_NAME)
					.getAsBoolean() : null;
			if (declineStakingRewards != null) {
				updateList.add("decline staking rewards");
			}
		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, DECLINE_STAKING_REWARDS_FIELD_NAME);
			answer = false;
		}

		return answer;
	}

	/**
	 * Returns an ordered list of the fields updated
	 *
	 * @return a list
	 */
	public List<String> getUpdateList() {
		final var list = new ArrayList<>(updateList);
		Collections.sort(list);
		return list;
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
		if (maxTokenAssociations != null) {
			accountUpdateTransaction.setMaxAutomaticTokenAssociations(maxTokenAssociations);
		}
		if (accountMemo != null) {
			accountUpdateTransaction.setAccountMemo(accountMemo);
		}
		if (stakedAccountId != null) {
			accountUpdateTransaction
				.setStakedAccountId(stakedAccountId.asAccount());
		}
		if (stakedNodeId != null) {
			accountUpdateTransaction
					.setStakedNodeId(stakedNodeId);
		}
		if (declineStakingRewards != null) {
			accountUpdateTransaction
					.setDeclineStakingReward(declineStakingRewards);
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
		//TODO Does this include new accounts/keys?
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

	@Override
	public JsonObject asJson() {
		final var asJson = super.asJson();
		if (key != null) {
			asJson.add("key", EncryptionUtils.keyToJson(key));
		}
		if (autoRenewDuration != null) {
			asJson.addProperty("autoRenewDuration", autoRenewDuration.getSeconds());
		}
		if (receiverSignatureRequired != null) {
			asJson.addProperty("receiverSignatureRequired", receiverSignatureRequired);
		}
		if (stakedAccountId != null) {
			asJson.add(STAKED_ACCOUNT_ID_FIELD_NAME, stakedAccountId.asJSON());
		}
		if (stakedNodeId != null) {
			asJson.addProperty(STAKED_NODE_ID_FIELD_NAME, stakedNodeId);
		}
		if (declineStakingRewards != null) {
			asJson.addProperty(DECLINE_STAKING_REWARDS_FIELD_NAME, declineStakingRewards);
		}
		return asJson;
	}

}
