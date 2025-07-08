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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransferTransaction;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AMOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSFERS;

public class ToolTransferTransaction extends ToolTransaction {

	private static final Logger logger = LogManager.getLogger(ToolTransferTransaction.class);

	private Map<Identifier, Hbar> accountAmountMap;

	public ToolTransferTransaction(final JsonObject input) throws HederaClientException {
		super(input);
		this.transactionType = TransactionType.CRYPTO_TRANSFER;
	}


	public ToolTransferTransaction(final File location) throws HederaClientException {
		super(location);

		if (!(transaction instanceof TransferTransaction)) {
			throw new HederaClientException("Not a transfer transaction");
		}

		accountAmountMap = new HashMap<>();
		final var transfers = ((TransferTransaction) transaction).getHbarTransfers();
		transfers.keySet().forEach(
				accountId -> accountAmountMap.put(new Identifier(accountId), transfers.get(accountId)));
		setTransactionType(TransactionType.CRYPTO_TRANSFER);
	}

	public Map<Identifier, Hbar> getAccountAmountMap() {
		return accountAmountMap;
	}

	/**
	 * Return the total number of Hbars transferred (sum of all deductions from the senders accounts)
	 *
	 * @return an Hbar value
	 */
	public Hbar getHbarsTransferred() {
		var total = 0L;
		for (Hbar value : accountAmountMap.values()) {
			if (value.toTinybars() > 0) {
				total += value.toTinybars();
			}
		}
		return Hbar.fromTinybars(total);
	}

	@Override
	public boolean checkInput(final JsonObject input) throws HederaClientRuntimeException {

		// Check common fields first
		var answer = super.checkInput(input);

		// Check transfers
		if (!input.has(TRANSFERS)) {
			logger.error(ErrorMessages.MISSING_FIELD_ERROR_MESSAGE, TRANSFERS);
			return false;
		}

		long total = 0;
		final var transfers = input.getAsJsonArray(TRANSFERS);
		accountAmountMap = new HashMap<>();
		for (final var transfer : transfers) {
			final var jsonObject = transfer.getAsJsonObject();
			final Identifier identifier;
			final Hbar amount;
			try {
				identifier = Identifier.parse(jsonObject.get(ACCOUNT).getAsJsonObject());
				final var tinyBars = jsonObject.get(AMOUNT).getAsLong();
				amount = Hbar.fromTinybars(tinyBars);
				total += tinyBars;
			} catch (final HederaClientException e) {
				logger.error(ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE,
						jsonObject.get(ACCOUNT).getAsJsonObject().toString());
				answer = false;
				continue;
			}
			accountAmountMap.put(identifier, amount);
		}

		if (total != 0) {
			answer = false;
			logger.error(ErrorMessages.NON_ZERO_TOTAL_ERROR_MESSAGE, total);
		}

		return answer;
	}

	@Override
	public Transaction<?> build() throws HederaClientRuntimeException {
		// Set common fields
		final var transactionId =
				new TransactionId(feePayerID.asAccount(), transactionValidStart);

		final var transferTransaction = new TransferTransaction();

		transferTransaction.setMaxTransactionFee(transactionFee)
				.setTransactionId(transactionId)
				.setTransactionMemo(memo)
				.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
				.setTransactionValidDuration(transactionValidDuration);

		// Add transfers
		for (final Map.Entry<Identifier, Hbar> entry : accountAmountMap.entrySet()) {
			transferTransaction.addHbarTransfer(entry.getKey().asAccount(), entry.getValue());
		}
		return transferTransaction.freeze();
	}

	@Override
	public Set<AccountId> getSigningAccountIds() {
		final var accountsSet = super.getSigningAccountIds();
		Map<AccountId, AccountInfo> infos = new HashMap<>();
		try {
			infos = loadAccountInfos();
		} catch (final HederaClientException | InvalidProtocolBufferException e) {
			logger.warn("Unable to load account information, some required receiver signatures may be omitted", e);
		}

		for (final Map.Entry<Identifier, Hbar> entry : accountAmountMap.entrySet()) {
			final var accountId = entry.getKey().asAccount();
			if (entry.getValue().toTinybars() < 0) {
				accountsSet.add(accountId);
				continue;
			}
			if (infos.containsKey(accountId)) {
				final var info = infos.get(accountId);
				if (info.isReceiverSignatureRequired) {
					accountsSet.add(accountId);
				}
			}
		}
		return accountsSet;
	}

	@Override
	public JsonObject asJson() {
		final var output = super.asJson();
		final var array = new JsonArray();
		for (final Map.Entry<Identifier, Hbar> entry : accountAmountMap.entrySet()) {
			final var line = new JsonObject();
			line.add(ACCOUNT, entry.getKey().asJSON());
			line.addProperty(AMOUNT, entry.getValue().toTinybars());
			array.add(line);
		}
		output.add(TRANSFERS, array);
		return output;
	}

	@Override
	public boolean equals(final Object obj) {
		if (!super.equals(obj) || !(obj instanceof ToolTransferTransaction)) {
			return false;
		}

		final var other = (ToolTransferTransaction) obj;
		return Objects.equals(this.accountAmountMap, other.accountAmountMap);
	}

	@Override
	public int hashCode() {
		return super.hashCode() + accountAmountMap.hashCode();
	}

	/**
	 * Load accounts into a map
	 *
	 * @return a Map<AccountId, AccountInfo>
	 */
	private Map<AccountId, AccountInfo> loadAccountInfos() throws HederaClientException,
			InvalidProtocolBufferException {
		final Map<AccountId, AccountInfo> map = new HashMap<>();
		final var files = new File(Constants.ACCOUNTS_INFO_FOLDER).listFiles(
				(dir, name) -> INFO_EXTENSION.equals(FilenameUtils.getExtension(name)));
		if (files != null) {
			for (final File file : files) {
				final var info = AccountInfo.fromBytes(readBytes(file.getAbsolutePath()));
				map.put(info.accountId, info);
			}
		}
		return map;
	}
}
