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
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.FileUpdateTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collections;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.CONTENTS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FILE_ID_FIELD_NAME;

public class ToolFileUpdateTransaction extends ToolTransaction {

	private Identifier file;
	private byte[] bytes;
	private static final Logger logger = LogManager.getLogger(ToolFileUpdateTransaction.class);

	public ToolFileUpdateTransaction(JsonObject input) throws HederaClientException {
		super(input);
		this.transactionType = TransactionType.FILE_UPDATE;
	}


	@Override
	public boolean checkInput(JsonObject input) {
		var answer = super.checkInput(input);
		if (!CommonMethods.verifyFieldExist(input, FILE_ID_FIELD_NAME, CONTENTS_FIELD_NAME)) {
			return false;
		}

		try {
			file = Identifier.parse(input.get(FILE_ID_FIELD_NAME).getAsJsonObject());
		} catch (HederaClientException e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, FILE_ID_FIELD_NAME);
			answer = false;
		}

		try {
			bytes = readBytes(input.get(CONTENTS_FIELD_NAME).getAsString());
		} catch (HederaClientException e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, CONTENTS_FIELD_NAME);
			answer = false;
		}

		if (bytes.length > 4095) {
			logger.error("File contents exceed the transaction limit: {}", bytes.length);
			answer = false;
		}

		return answer;
	}

	@Override
	public Transaction<?> build() throws HederaClientRuntimeException {
		var transactionId =
				new TransactionId(feePayerID.asAccount(), transactionValidStart);

		var fileUpdateTransaction = new FileUpdateTransaction();
		return fileUpdateTransaction.setFileId(file.asFile())
				.setContents(bytes)
				.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
				.setTransactionId(transactionId)
				.setMaxTransactionFee(transactionFee)
				.setTransactionMemo(memo)
				.setTransactionValidDuration(transactionValidDuration)
				.freeze();

	}

}
