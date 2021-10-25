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
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.constants.JsonConstants;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.FreezeTransaction;
import com.hedera.hashgraph.sdk.FreezeType;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;

import java.io.File;
import java.text.ParseException;
import java.time.Instant;
import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_START_TIME_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_TYPE_FIELD_NAME;

public class ToolFreezeTransaction extends ToolTransaction {
	private static final Logger logger = LogManager.getLogger(ToolFreezeTransaction.class);

	private Instant startTime;
	private FreezeType freezeType;
	private Identifier fileID;
	private byte[] fileHash;

	public ToolFreezeTransaction(JsonObject input) throws HederaClientException {
		super(input);
		this.transactionType = TransactionType.FREEZE;
	}

	public ToolFreezeTransaction(File inputFile) throws HederaClientException {
		super(inputFile);
		this.startTime = ((FreezeTransaction) transaction).getStartTime();
		this.freezeType = ((FreezeTransaction) transaction).getFreezeType();
		if (((FreezeTransaction) transaction).getFileId() != null) {
			this.fileID = new Identifier(Objects.requireNonNull(((FreezeTransaction) transaction).getFileId()));
		}
		if (((FreezeTransaction) transaction).getFileHash() != null) {
			this.fileHash = ((FreezeTransaction) transaction).getFileHash();
		}
		this.transactionType = TransactionType.FREEZE;
	}

	public Timestamp getStartTime() {
		return new Timestamp(startTime);
	}

	public FreezeType getFreezeType() {
		return freezeType;
	}

	public Identifier getFileID() {
		return fileID;
	}

	public String getFileHash() {
		return Hex.toHexString(fileHash);
	}

	private FreezeType parseType(String asString) {
		switch (asString) {
			case "FREEZE_ONLY":
				return FreezeType.FREEZE_ONLY;
			case "PREPARE_UPGRADE":
				return FreezeType.PREPARE_UPGRADE;
			case "FREEZE_UPGRADE":
				return FreezeType.FREEZE_UPGRADE;
			case "FREEZE_ABORT":
				return FreezeType.FREEZE_ABORT;
			case "TELEMETRY_UPGRADE":
				return FreezeType.TELEMETRY_UPGRADE;
			default:
				return FreezeType.UNKNOWN_FREEZE_TYPE;
		}
	}

	@Override
	public boolean checkInput(JsonObject input) {
		var answer = super.checkInput(input);
		if (!CommonMethods.verifyFieldExist(input, FREEZE_TYPE_FIELD_NAME)) {
			return false;
		}

		freezeType = parseType(input.get(FREEZE_TYPE_FIELD_NAME).getAsString());
		if (FreezeType.UNKNOWN_FREEZE_TYPE.equals(freezeType)) {
			logger.error("Unknown freeze type");
			answer = false;
		}

		if (input.has(JsonConstants.FREEZE_FILE_ID_FIELD_NAME)) {
			if (!input.has(JsonConstants.FREEZE_FILE_HASH_FIELD_NAME)) {
				logger.error("Missing {} in input", JsonConstants.FREEZE_FILE_HASH_FIELD_NAME);
				return false;
			}
			try {
				fileID = Identifier.parse(input.getAsJsonObject(JsonConstants.FREEZE_FILE_ID_FIELD_NAME));
				fileHash = Hex.decode(input.get(JsonConstants.FREEZE_FILE_HASH_FIELD_NAME).getAsString());
			} catch (HederaClientException e) {
				logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, JsonConstants.FREEZE_FILE_ID_FIELD_NAME);
				answer = false;
			}
		}

		if (input.has(FREEZE_START_TIME_FIELD_NAME)) {
			try {
				startTime = new Timestamp(input.get(FREEZE_START_TIME_FIELD_NAME)).asInstant();
			} catch (ParseException e) {
				logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, FREEZE_START_TIME_FIELD_NAME);
				answer = false;
			}
		}

		return answer;
	}

	@Override
	public Transaction<? extends Transaction<?>> build() throws HederaClientRuntimeException {
		var transactionId = new TransactionId(feePayerID.asAccount(), transactionValidStart);
		FreezeTransaction transaction = new FreezeTransaction();
		switch (freezeType) {
			case FREEZE_ONLY:
				if (startTime == null) {
					throw new HederaClientRuntimeException("Start time must be specified");
				}
				if (startTime.isBefore(Instant.now())) {
					throw new HederaClientRuntimeException("Start time cannot be in the past");
				}
				return transaction.setTransactionId(transactionId)
						.setFreezeType(FreezeType.FREEZE_ONLY)
						.setStartTime(startTime)
						.setMaxTransactionFee(transactionFee)
						.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
						.freeze();
			case PREPARE_UPGRADE:
				if (fileID == null) {
					throw new HederaClientRuntimeException("File ID must be specified");
				}
				if (fileHash.length == 0) {
					throw new HederaClientRuntimeException("Empty file hash");
				}
				return transaction.setFreezeType(FreezeType.PREPARE_UPGRADE)
						.setFileId(fileID.asFile())
						.setFileHash(fileHash)
						.setTransactionId(transactionId)
						.setMaxTransactionFee(transactionFee)
						.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
						.freeze();
			case FREEZE_UPGRADE:
				if (startTime == null) {
					throw new HederaClientRuntimeException("Start time must be specified");
				}
				if (startTime.isBefore(Instant.now())) {
					throw new HederaClientRuntimeException("Start time cannot be in the past");
				}
				if (fileID == null) {
					throw new HederaClientRuntimeException("File ID must be specified");
				}
				if (fileHash.length == 0) {
					throw new HederaClientRuntimeException("Empty file hash");
				}
				return transaction.setFreezeType(FreezeType.FREEZE_UPGRADE)
						.setFileId(fileID.asFile())
						.setFileHash(fileHash)
						.setStartTime(startTime)
						.setTransactionId(transactionId)
						.setMaxTransactionFee(transactionFee)
						.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
						.freeze();

			case FREEZE_ABORT:
				return transaction.setFreezeType(FreezeType.FREEZE_ABORT)
						.setTransactionId(transactionId)
						.setMaxTransactionFee(transactionFee)
						.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
						.freeze();

			case TELEMETRY_UPGRADE:
				if (startTime == null) {
					throw new HederaClientRuntimeException("Start time must be specified");
				}
				if (startTime.isBefore(Instant.now())) {
					throw new HederaClientRuntimeException("Start time cannot be in the past");
				}
				if (fileID == null) {
					throw new HederaClientRuntimeException("File ID must be specified");
				}
				if (fileHash.length == 0) {
					throw new HederaClientRuntimeException("Empty file hash");
				}
				return transaction.setFreezeType(FreezeType.TELEMETRY_UPGRADE)
						.setStartTime(startTime)
						.setFileHash(fileHash)
						.setFileId(fileID.asFile())
						.setTransactionId(transactionId)
						.setMaxTransactionFee(transactionFee)
						.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
						.freeze();

		}

		return transaction;
	}

	@Override
	public Set<AccountId> getSigningAccounts() {
		Set<AccountId> accountIds = new HashSet<>();
		accountIds.add(new Identifier(0, 0, 2).asAccount());
		accountIds.add(new Identifier(0, 0, 50).asAccount());
		return accountIds;
	}

	@Override
	public JsonObject asJson() {
		var output = super.asJson();
		output.addProperty(FREEZE_TYPE_FIELD_NAME, freezeType.toString());
		output.add(FREEZE_START_TIME_FIELD_NAME, new Timestamp(startTime).asJSON());
		output.add(JsonConstants.FREEZE_FILE_ID_FIELD_NAME, fileID.asJSON());
		output.addProperty(JsonConstants.FREEZE_FILE_HASH_FIELD_NAME, Hex.toHexString(fileHash));
		return output;
	}

	@Override
	public boolean equals(Object obj) {
		return super.equals(obj);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
