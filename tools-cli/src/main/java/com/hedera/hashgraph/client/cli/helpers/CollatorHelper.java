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

package com.hedera.hashgraph.client.cli.helpers;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.Transaction;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.SIGNATURE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.NOT_EMPTY_FIELD_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.Messages.OUTPUT_FILE_CREATED_MESSAGE;

public class CollatorHelper implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(CollatorHelper.class);

	private final Set<SignaturePair> signaturePairs = new HashSet<>();
	private ToolTransaction transaction;
	private String transactionFile = "";
	private String baseName = "";
	private final JsonArray comments = new JsonArray();

	public CollatorHelper(File file) throws HederaClientException {
		switch (FilenameUtils.getExtension(file.getName())) {
			case TRANSACTION_EXTENSION:
				this.transaction = new ToolTransaction(file);
				this.transactionFile = setFileOutput(file.getAbsolutePath());
				this.baseName = FilenameUtils.getBaseName(file.getAbsolutePath());
				addSignature(this.transaction.getTransaction());
				break;
			case SIGNATURE_EXTENSION:
				this.transaction = null;
				this.transactionFile = "";
				this.baseName = FilenameUtils.getBaseName(file.getAbsolutePath());
				signaturePairs.add(new SignaturePair(file.getAbsolutePath()));
				break;
			case TXT_EXTENSION:
				this.transaction = null;
				this.transactionFile = "";
				this.baseName = FilenameUtils.getBaseName(file.getAbsolutePath());
				comments.add(readJsonObject(file));
				break;
			default:
				throw new HederaClientException(CANNOT_PARSE_ERROR_MESSAGE.replace("{}", file.getName()));
		}
	}

	private String setFileOutput(String absolutePath) {
		if ("".equals(absolutePath)) {
			return this.transactionFile;
		}
		if ("".equals(this.transactionFile)) {
			return absolutePath;
		}
		var lcSubstring = CommonMethods.getLCSubStr(this.transactionFile, absolutePath);
		if (lcSubstring.endsWith("_") || lcSubstring.endsWith("-")) {
			lcSubstring = lcSubstring.substring(0, lcSubstring.length() - 1);
		}
		return lcSubstring;
	}

	public Set<SignaturePair> getSignaturePairs() {
		return signaturePairs;
	}

	public ToolTransaction getTransaction() {
		return transaction;
	}

	public void setTransaction(ToolTransaction transaction) {
		this.transaction = transaction;
	}

	public String getTransactionFile() {
		return transactionFile;
	}

	public void setTransactionFile(String transactionFile) {
		this.transactionFile = transactionFile;
	}

	public String getBaseName() {
		return baseName;
	}

	public void addSignature(Transaction<?> transaction) {
		var signatures = transaction.getSignatures();
		assert signatures.size() < 2;
		for (var entry : signatures.entrySet()) {
			var map = entry.getValue();
			for (var mapEntry : map.entrySet()) {
				var pair = new SignaturePair(mapEntry.getKey(), mapEntry.getValue());
				signaturePairs.add(pair);
			}
		}
	}

	public void addTransaction(File file) throws HederaClientException {
		if (this.transaction != null) {
			throw new HederaClientException(NOT_EMPTY_FIELD_ERROR_MESSAGE);
		}
		if (!TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(file.getName()))) {
			throw new HederaClientException(CANNOT_PARSE_ERROR_MESSAGE);
		}
		this.transaction = new ToolTransaction(file);
		this.transactionFile = file.getAbsolutePath();
		addSignature(this.transaction.getTransaction());
	}

	public void addSignature(PublicKey publicKey, byte[] signature) {
		var newSignature = new SignaturePair(publicKey, signature);
		signaturePairs.add(newSignature);
	}

	public void addSignature(SignaturePair signaturePair) {
		signaturePairs.add(signaturePair);
	}

	public Transaction<?> collate() {
		return transaction.collate(signaturePairs);
	}

	public void addComments(File commentFile) throws HederaClientException {
		comments.add(readJsonObject(commentFile));
	}

	public JsonObject getComments() {
		var object = new JsonObject();
		object.add("comments", comments);
		return object;
	}

	public boolean verify(AccountInfo info) throws HederaClientException {
		var signed = transaction;
		signed.collate(signaturePairs);
		return signed.verify(info);
	}

	public boolean verify(PublicKey publicKey) {
		var signed = transaction;
		signed.collate(signaturePairs);
		return signed.verify(publicKey);
	}

	public Set<AccountId> getSigningAccounts() {
		return transaction.getSigningAccounts();
	}

	public boolean hasTransaction() {
		return !transactionFile.equals("");
	}

	/**
	 * Loads transaction from a file and adds all the signatures to the map
	 *
	 * @param helper
	 * 		a collator helper
	 */
	public void addAllSignatures(CollatorHelper helper) {
		signaturePairs.addAll(helper.getSignaturePairs());
	}

	public void addHelper(CollatorHelper helper) throws HederaClientException {
		if (!this.baseName.equals(helper.baseName)) {
			throw new HederaClientException("Transactions don't match");
		}
		if (this.transactionFile.equals("")) {
			this.transactionFile = helper.transactionFile;
			this.transaction = helper.transaction;
			this.comments.addAll(helper.comments);
		} else {
			this.transactionFile = setFileOutput(helper.transactionFile);
		}
		signaturePairs.addAll(helper.getSignaturePairs());
	}

	public String store(String key) throws HederaClientException {
		var output = this.transactionFile;
		final var outFile = new File(output);
		if (outFile.isFile()) {
			output = outFile.getParent();
		}
		if (key.contains("Node")) {
			output = output + "_" + key.substring(0, key.indexOf("_"));
		}
		if (new File(output).mkdirs()) {
			logger.info(OUTPUT_FILE_CREATED_MESSAGE, output);
		}
		var transactionBytes = transaction.toBytes();
		writeBytes(output + File.separator + this.baseName + "." + SIGNED_TRANSACTION_EXTENSION, transactionBytes);
		return output;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof CollatorHelper)) {
			return false;
		}

		final var objTransaction = ((CollatorHelper) obj).getTransaction().getTransaction();
		if (this.transaction.getTransaction() == null ^ objTransaction == null) {
			return false;
		}

		final var objSignaturePairs = ((CollatorHelper) obj).getSignaturePairs();
		if (this.signaturePairs.isEmpty() ^ objSignaturePairs.isEmpty()) {
			return false;
		}

		final var objComments = ((CollatorHelper) obj).getComments().get("comments").getAsJsonArray();
		if (this.comments.size() == 0 ^ objComments.size() == 0) {
			return false;
		}

		var transactionBoolean = true;
		if (this.transaction.getTransaction() != null && ((CollatorHelper) obj).getTransaction() != null) {
			assert objTransaction != null;
			transactionBoolean = Arrays.equals(this.transaction.getTransaction().toBytes(), objTransaction.toBytes());
		}

		var signaturesBoolean = true;
		if (this.signaturePairs.size() == objSignaturePairs.size()) {
			for (var signaturePair : this.signaturePairs) {
				if (!objSignaturePairs.contains(signaturePair)) {
					signaturesBoolean = false;
					break;
				}
			}
		}

		var commentsBoolean = true;
		if (this.comments.size() == 0 && objComments.size() == 0) {
			commentsBoolean = this.comments.equals(objComments);
		}

		return transactionBoolean && signaturesBoolean && commentsBoolean;
	}

	@Override
	public int hashCode() {
		final var prime = 37;
		var hash = 1;
		hash = (prime * hash) + signaturePairs.hashCode();
		hash = (prime * hash) + ((transaction != null) ? Arrays.hashCode(transaction.getTransaction().toBytes()) : 0);
		hash = (prime * hash) + ((!"".equals(transactionFile)) ? transactionFile.hashCode() : 0);
		hash = (prime * hash) + ((!"".equals(baseName)) ? baseName.hashCode() : 0);
		hash = (prime * hash) + ((comments.isJsonNull()) ? comments.hashCode() : 0);
		return hash;
	}
}
