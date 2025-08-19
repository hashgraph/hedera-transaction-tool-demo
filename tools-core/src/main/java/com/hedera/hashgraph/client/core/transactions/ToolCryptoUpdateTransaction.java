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
import java.util.Objects;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_CREATION_METADATA_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_TO_UPDATE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_TO_UPDATE_INPUT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AUTO_RENEW_PERIOD_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DECLINE_STAKING_REWARDS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MAX_TOKEN_ASSOCIATIONS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NEW_KEY_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.STAKED_ACCOUNT_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.STAKED_NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.remote.TransactionCreationMetadataFile.ACCOUNTS_STRING;
import static com.hedera.hashgraph.client.core.remote.TransactionCreationMetadataFile.IS_UPDATE_ACCOUNT_FEE_PAYER;
import static com.hedera.hashgraph.client.core.remote.helpers.AccountList.INPUT_STRING;

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
	private String accountInput;
	private boolean isUpdateAccountFeePayer = false;

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
		final var tcm = new File(inputFile.getAbsolutePath()
				.replace(TRANSACTION_EXTENSION, TRANSACTION_CREATION_METADATA_EXTENSION));
		if (tcm.exists()) {
			var contents = readJsonObject(tcm.getPath());
			if (contents.has(ACCOUNTS_STRING)) {
				contents = contents.get(ACCOUNTS_STRING).getAsJsonObject();
				if (contents.has(INPUT_STRING)) {
					accountInput = contents.get(INPUT_STRING).getAsString();
				}
			}
			if (contents.has(IS_UPDATE_ACCOUNT_FEE_PAYER)) {
				isUpdateAccountFeePayer = contents.get(IS_UPDATE_ACCOUNT_FEE_PAYER).getAsBoolean();
			}
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

	public String getAccountInput() {
		return accountInput;
	}

	public boolean isUpdateAccountFeePayer() {
		return isUpdateAccountFeePayer;
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
			if (input.has(ACCOUNT_TO_UPDATE_INPUT)) {
				accountInput = input.get(ACCOUNT_TO_UPDATE_INPUT).getAsString();
			}
		} catch (final Exception ex) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, ACCOUNT_TO_UPDATE_INPUT);
			answer = false;
		}

		try {
			if (input.has(IS_UPDATE_ACCOUNT_FEE_PAYER)) {
				isUpdateAccountFeePayer = input.get(IS_UPDATE_ACCOUNT_FEE_PAYER).getAsBoolean();
			}
		} catch (final Exception ex) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, ACCOUNT_TO_UPDATE_INPUT);
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

	// According to documentation, if account is between 3 and 1000 inclusive,
	// the key is not required IF the fee payer is 2 or 50. In all other cases, the key is required.
	// https://github.com/hiero-ledger/hiero-consensus-node/blob/main/hedera-node/docs/privileged-transactions.md#waived-signing-requirements-for-crypto-updates
	private boolean requireSigningRequirements() {
		final var feePayerNum = feePayerID.getAccountNum();
		final var accountId = ((AccountUpdateTransaction) transaction).getAccountId();
		final var accountNum = accountId != null ? accountId.num : 0L;
		return (accountNum < 3 || accountNum > 1000) || (feePayerNum != 2 && feePayerNum != 50);
	}

	@Override
	public Set<AccountId> getSigningAccountIds() {
		// This will return the Fee Payer account and the account to be updated, no new signatures
		// will be referenced at this point.
		final var accountsSet = super.getSigningAccountIds();
		if (requireSigningRequirements()) {
			accountsSet.add(((AccountUpdateTransaction) transaction).getAccountId());
		}
		return accountsSet;
	}

	@Override
	public KeyList getSigningAccountKeys(KeyList accountKeyList) throws HederaClientRuntimeException {
		final var requiredKeyList = new KeyList();
		requiredKeyList.add(accountKeyList);
		if (requireSigningRequirements()) {
			// New key is required:
			// https://docs.hedera.com/hedera/sdks-and-apis/sdks/accounts-and-hbar/update-an-account
			final var keyFromTransaction = ((AccountUpdateTransaction) transaction).getKey();
			if (keyFromTransaction != null) {
				requiredKeyList.add(keyFromTransaction);
			}
		}
		return requiredKeyList;
	}

	@Override
	public boolean equals(final Object obj) {
		if (!super.equals(obj) || !(obj instanceof ToolCryptoUpdateTransaction)) {
			return false;
		}

		final var other = (ToolCryptoUpdateTransaction) obj;
		return Objects.equals(account, other.account) &&
				Objects.equals(key, other.key) &&
				Objects.equals(autoRenewDuration, other.autoRenewDuration) &&
				Objects.equals(receiverSignatureRequired, other.receiverSignatureRequired) &&
				Objects.equals(maxTokenAssociations, other.maxTokenAssociations) &&
				Objects.equals(accountMemo, other.accountMemo) &&
				Objects.equals(stakedAccountId, other.stakedAccountId) &&
				Objects.equals(stakedNodeId, other.stakedNodeId) &&
				Objects.equals(declineStakingRewards, other.declineStakingRewards) &&
				isUpdateAccountFeePayer == other.isUpdateAccountFeePayer;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public JsonObject asJson() {
		final var asJson = super.asJson();
		if (account != null) {
			asJson.add(ACCOUNT_TO_UPDATE, account.asJSON());
		}
		if (accountInput != null) {
			asJson.addProperty(ACCOUNT_TO_UPDATE_INPUT, accountInput);
		}
		if (isUpdateAccountFeePayer) {
			asJson.addProperty(IS_UPDATE_ACCOUNT_FEE_PAYER, isUpdateAccountFeePayer);
		}
		if (key != null) {
			asJson.add(NEW_KEY_FIELD_NAME, EncryptionUtils.keyToJson(key));
		}
		if (accountMemo != null) {
			asJson.addProperty(ACCOUNT_MEMO_FIELD_NAME, accountMemo);
		}
		if (maxTokenAssociations != null) {
			asJson.addProperty(MAX_TOKEN_ASSOCIATIONS_FIELD_NAME, maxTokenAssociations);
		}
		if (autoRenewDuration != null) {
			asJson.addProperty(AUTO_RENEW_PERIOD_FIELD_NAME, autoRenewDuration.getSeconds());
		}
		if (receiverSignatureRequired != null) {
			asJson.addProperty(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, receiverSignatureRequired);
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
