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
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.CONTENTS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FILE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_READABLE_FIELD_NAME;

public class ToolFileAppendTransaction extends ToolTransaction {
	private Identifier file;
	private byte[] bytes;
	private String location;
	private static final Logger logger = LogManager.getLogger(ToolFileAppendTransaction.class);

	public ToolFileAppendTransaction(final JsonObject input) throws HederaClientException {
		super(input);
		this.transactionType = TransactionType.FILE_APPEND;
	}

	public ToolFileAppendTransaction(final File inputFile) throws HederaClientException {
		super(inputFile);
		this.file = new Identifier(((FileAppendTransaction) transaction).getFileId(), "");
		this.bytes = ((FileAppendTransaction) transaction).getContents().toByteArray();
//		this.fileMemo = ((FileUpdateTransaction) transaction).getFileMemo();
		setTransactionType(TransactionType.FILE_APPEND);
	}

	@Override
	public ToolFileAppendTransaction atNow() throws HederaClientException {
		final var json = this.asJson();
		final var now = new Timestamp();
		writeBytes(location, bytes);
		json.addProperty(TRANSACTION_VALID_START_READABLE_FIELD_NAME, now.asRFCString());
		json.add(TRANSACTION_VALID_START_FIELD_NAME, now.asJSON());
		return new ToolFileAppendTransaction(json);
	}

	public Identifier getFile() {
		return file;
	}

	@Override
	public boolean equals(final Object obj) {
		if (!super.equals(obj) || !(obj instanceof ToolFileAppendTransaction)) {
			return false;
		}

		final var other = (ToolFileAppendTransaction) obj;
		return Objects.equals(this.file, other.file) &&
				Objects.equals(this.location, other.location) &&
				Arrays.equals(this.bytes, other.bytes);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public boolean checkInput(final JsonObject input) {
		var answer = super.checkInput(input);
		if (!CommonMethods.verifyFieldExist(input, FILE_ID_FIELD_NAME, CONTENTS_FIELD_NAME)) {
			return false;
		}

		try {
			file = Identifier.parse(input.get(FILE_ID_FIELD_NAME).getAsJsonObject());
		} catch (final HederaClientException e) {
			logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, FILE_ID_FIELD_NAME);
			answer = false;
		}

		try {
			this.location = input.get(CONTENTS_FIELD_NAME).getAsString();
			bytes = readBytes(this.location);
		} catch (final HederaClientException e) {
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
		final var transactionId =
				new TransactionId(feePayerID.asAccount(), transactionValidStart);

		final var fileAppendTransaction = new FileAppendTransaction();
		return fileAppendTransaction.setFileId(file.asFile())
				.setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
				.setContents(bytes)
				.setTransactionId(transactionId)
				.setMaxTransactionFee(transactionFee)
				.setTransactionMemo(memo)
				.setTransactionValidDuration(transactionValidDuration)
				.freeze();

	}

	@Override
	public JsonObject asJson() {
		final var input = super.asJson();
		input.add(FILE_ID_FIELD_NAME, file.asJSON());
		input.addProperty(CONTENTS_FIELD_NAME, location);
		return input;
	}

}


