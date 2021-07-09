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
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.SystemDeleteTransaction;
import com.hedera.hashgraph.sdk.SystemUndeleteTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.text.ParseException;
import java.time.Instant;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DEL_UNDEL_SWITCH;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ENTITY_TO_DEL_UNDEL;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.EXPIRATION_DATE_TIME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FILE_CONTRACT_SWITCH;

public class ToolSystemTransaction extends ToolTransaction {
	private Identifier entity;
	private Instant expiration;
	private boolean isFile;
	private boolean isDelete;
	private static final Logger logger = LogManager.getLogger(ToolSystemTransaction.class);

	public ToolSystemTransaction(JsonObject input) throws HederaClientException {
		super(input);
		this.transactionType = TransactionType.SYSTEM_DELETE_UNDELETE;
	}

	@Override
	public boolean equals(Object obj) {
		return super.equals(obj);
	}

	public ToolSystemTransaction(File inputFile) throws HederaClientException {
		super(inputFile);
		this.isDelete = transaction instanceof SystemDeleteTransaction;
		var contract =
				(isDelete) ? ((SystemDeleteTransaction) transaction).getContractId() :
						((SystemUndeleteTransaction) transaction).getContractId();
		var file =
				(isDelete) ? ((SystemDeleteTransaction) transaction).getFileId() :
						((SystemUndeleteTransaction) transaction).getFileId();

		if (file == null && contract == null) {
			throw new HederaClientException("Both contract and file IDs cannot be null");
		}
		if (file != null && contract != null) {
			throw new HederaClientException("Cannot determine if the entity is a contract or a file");
		}
		this.isFile = (file != null);
		this.entity = isFile ? new Identifier(file) : new Identifier(contract);

		this.expiration = (isDelete) ? ((SystemDeleteTransaction) transaction).getExpirationTime() : null;
		this.transactionType = TransactionType.SYSTEM_DELETE_UNDELETE;
	}

	@Override
	public boolean checkInput(JsonObject input) {
		var answer = super.checkInput(input);
		if (!CommonMethods.verifyFieldExist(input, ENTITY_TO_DEL_UNDEL, DEL_UNDEL_SWITCH,
				FILE_CONTRACT_SWITCH)) {
			return false;
		}


		try {
			entity = Identifier.parse(input.get(ENTITY_TO_DEL_UNDEL).getAsJsonObject());
		} catch (HederaClientException e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, ENTITY_TO_DEL_UNDEL);
			answer = false;
		}

		try {
			isDelete = input.get(DEL_UNDEL_SWITCH).getAsString().contains("Remove");
		} catch (Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, DEL_UNDEL_SWITCH);
			answer = false;
		}

		try {
			isFile = input.get(FILE_CONTRACT_SWITCH).getAsString().contains("File");
		} catch (Exception e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, FILE_CONTRACT_SWITCH);
			answer = false;
		}

		if (isDelete && !input.has(EXPIRATION_DATE_TIME)) {
			logger.error("Missing expiration for deletion");
			return false;
		}

		try {
			expiration = new Timestamp(input.get(EXPIRATION_DATE_TIME)).asInstant();
		} catch (NumberFormatException | ParseException e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, EXPIRATION_DATE_TIME);
			answer = false;
		}

		return answer;
	}

	@Override
	public Transaction<?> build() throws HederaClientRuntimeException {
		var transactionId = new TransactionId(feePayerID.asAccount(), transactionValidStart);

		if (isDelete) {
			var systemTransaction = new SystemDeleteTransaction()
					.setExpirationTime(expiration)
					.setTransactionId(transactionId)
					.setTransactionMemo(memo)
					.setMaxTransactionFee(transactionFee)
					.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
					.setTransactionValidDuration(transactionValidDuration);

			if (isFile) {
				return systemTransaction.setFileId(entity.asFile()).freeze();
			}
			return systemTransaction.setContractId(entity.asContract()).freeze();
		}
		var systemTransaction = new SystemUndeleteTransaction()
				.setTransactionId(transactionId)
				.setTransactionMemo(memo)
				.setMaxTransactionFee(transactionFee)
				.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
				.setTransactionValidDuration(transactionValidDuration);

		if (isFile) {
			return systemTransaction.setFileId(entity.asFile()).freeze();
		}
		return systemTransaction.setContractId(entity.asContract()).freeze();


	}

	public Identifier getEntity() {
		return entity;
	}

	public Instant getExpiration() {
		return expiration;
	}

	public boolean isFile() {
		return isFile;
	}

	public boolean isDelete() {
		return isDelete;
	}

	@Override
	public JsonObject asJson() {
		var asJson = super.asJson();
		asJson.add(ENTITY_TO_DEL_UNDEL, entity.asJSON());
		asJson.addProperty(FILE_CONTRACT_SWITCH, isFile);
		asJson.addProperty(DEL_UNDEL_SWITCH, isDelete);
		if (isDelete) {
			asJson.add(EXPIRATION_DATE_TIME, new Timestamp(expiration).asJSON());
		}
		return asJson;
	}

	@Override
	public Set<AccountId> getSigningAccounts() {
		Set<AccountId> accountIds = new HashSet<>();
		accountIds.add(new Identifier(0, 0, 2).asAccount());
		accountIds.add(new Identifier(0, 0, 50).asAccount());
		return accountIds;
	}
}
