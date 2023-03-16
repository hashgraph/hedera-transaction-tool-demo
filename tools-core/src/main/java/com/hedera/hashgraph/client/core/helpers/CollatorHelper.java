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

package com.hedera.hashgraph.client.core.helpers;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.Transaction;
import org.apache.commons.io.FileUtils;
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

/**
 * This helper class is used to gather all the .tx, .sig, and .txt files into one place
 * to prepare for collation.
 */
public class CollatorHelper implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(CollatorHelper.class);

	private final Set<SignaturePair> signaturePairs = new HashSet<>();
	private ToolTransaction transaction;
	private String transactionFileName = "";
	private final String baseName;
	private final JsonArray comments = new JsonArray();

	public CollatorHelper(final File file) throws HederaClientException {
		switch (FilenameUtils.getExtension(file.getName())) {
			case TRANSACTION_EXTENSION:
				this.transaction = new ToolTransaction().parseFile(file);
				this.transactionFileName = setFileOutput(file);
				this.baseName = FilenameUtils.getBaseName(file.getAbsolutePath());
				break;
			case SIGNATURE_EXTENSION:
				this.transaction = null;
				this.transactionFileName = "";
				this.baseName = FilenameUtils.getBaseName(file.getAbsolutePath());
				signaturePairs.add(new SignaturePair(file.getAbsolutePath()));
				break;
			case TXT_EXTENSION:
				this.transaction = null;
				this.transactionFileName = "";
				this.baseName = FilenameUtils.getBaseName(file.getAbsolutePath());
				comments.add(readJsonObject(file));
				break;
			default:
				throw new HederaClientException(CANNOT_PARSE_ERROR_MESSAGE.replace("{}", file.getName()));
		}
	}

	private String setFileOutput(final File file) {
		// If absolutePath is empty, return the transactionFileName (which might also be empty)
		if (file == null) {
			return this.transactionFileName;
		}

		// Ensure that the file is a file (exists and not a directory)
		if (file.isFile()) {
			var parent = file.getParentFile();
			var parentName = parent.getName();

			// If 'Node' is in the name of the transaction AND the node label is not used, this will be an issue
			// If Node exists, remove it, otherwise find the suffix and remove it (if applicable).
			if (parentName.contains("Node")) {
				parentName = parentName.substring(0, parentName.lastIndexOf("_Node"));
			} else {
				final var suffix0 = parentName.contains("signatures") ? "_signatures" : "";
				final var suffix = parentName.contains("transactions") ? "_transactions" : suffix0;
				parentName = parentName.substring(0, parentName.lastIndexOf(suffix));
			}
			// Remove the key name
			parentName = parentName.substring(0, parentName.lastIndexOf("_"));

			// Return the path of the parent's parent, plus the parentName
			return parent.getParentFile().getAbsolutePath() + File.separator + parentName;
		}
		// Return an empty string
		return "";
	}

	public Set<SignaturePair> getSignaturePairs() {
		return signaturePairs;
	}

	public ToolTransaction getTransaction() {
		return transaction;
	}

	public void setTransaction(final ToolTransaction transaction) {
		this.transaction = transaction;
	}

	public String getTransactionFile() {
		return transactionFileName;
	}

	public void setTransactionFile(final String transactionFile) {
		this.transactionFileName = transactionFile;
	}

	public String getBaseName() {
		return baseName;
	}

	public void addSignature(final Transaction<?> transaction) {
		final var signatures = transaction.getSignatures();
		if (signatures.size() > 1) {
			throw new HederaClientRuntimeException("Too many signatures");
		}
		for (final var entry : signatures.entrySet()) {
			final var map = entry.getValue();
			for (final var mapEntry : map.entrySet()) {
				final var pair = new SignaturePair(mapEntry.getKey(), mapEntry.getValue());
				signaturePairs.add(pair);
			}
		}
	}

	public void addTransaction(final File file) throws HederaClientException {
		if (this.transaction != null) {
			throw new HederaClientException(NOT_EMPTY_FIELD_ERROR_MESSAGE);
		}
		if (!TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(file.getName()))) {
			throw new HederaClientException(CANNOT_PARSE_ERROR_MESSAGE);
		}
		this.transaction = new ToolTransaction(file);
		this.transactionFileName = setFileOutput(file);
		addSignature(this.transaction.getTransaction());
	}

	public void addSignature(final PublicKey publicKey, final byte[] signature) {
		final var newSignature = new SignaturePair(publicKey, signature);
		signaturePairs.add(newSignature);
	}

	public void addSignature(final SignaturePair signaturePair) {
		signaturePairs.add(signaturePair);
	}

	public final Transaction<?> collate() {
		return transaction.collate(signaturePairs);
	}

	public void addComments(final File commentFile) throws HederaClientException {
		comments.add(readJsonObject(commentFile));
	}

	public JsonObject getComments() {
		final var object = new JsonObject();
		object.add("comments", comments);
		return object;
	}

	/**
	 * Verify that the provided account has the proper amount of signatures on the transaction.
	 *
	 * @param info
	 * @return
	 * @throws HederaClientException
	 */
	public boolean verify(final AccountInfo info) throws HederaClientException {
		final var signed = transaction;
		signed.collate(signaturePairs);
		return signed.verify(info);
	}

	public boolean verify(final PublicKey publicKey) {
		final var signed = transaction;
		signed.collate(signaturePairs);
		return signed.verify(publicKey);
	}

	public Set<AccountId> getSigningAccounts() {
		return transaction.getSigningAccounts();
	}

	public boolean hasTransaction() {
		return !transactionFileName.equals("");
	}

	/**
	 * Loads transaction from a file and adds all the signatures to the map
	 *
	 * @param helper
	 * 		a collator helper
	 */
	public void addAllSignatures(final CollatorHelper helper) {
		signaturePairs.addAll(helper.getSignaturePairs());
	}

	public void addHelper(final CollatorHelper helper) throws HederaClientException {
		if (!this.baseName.equals(helper.baseName)) {
			throw new HederaClientException("Transactions don't match");
		}
		if (this.transactionFileName.equals("")) {
			this.transactionFileName = helper.transactionFileName;
			this.transaction = helper.transaction;
			this.comments.addAll(helper.comments);
		} else if (helper.hasTransaction()) {
			if (!this.transaction.equals(helper.getTransaction())) {
				logger.error("Transaction {} does not match transaction {}", this.getTransactionFile(),
						helper.getTransactionFile());
				throw new HederaClientException("Transactions don't match");
			}
			this.transactionFileName = setFileOutput(helper.transactionFileName);
			// If the transactions are the same (only checks memo, maxTransactionFee, validDuration, and nodeAccountId)
			// then keep the smaller one (will have fewer signatures on it)
			if (helper.getTransaction().toBytes().length < this.transaction.toBytes().length) {
				this.transaction = helper.getTransaction();
			}
		}
		signaturePairs.addAll(helper.getSignaturePairs());
	}

	public String store(final String key) throws HederaClientException {
		var output = this.transactionFileName;

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
		final var transactionBytes = transaction.toBytes();
		// Write the bytes of the signed transaction to file
		writeBytes(output + File.separator + this.baseName + "." + SIGNED_TRANSACTION_EXTENSION, transactionBytes);
		// Return the enclosing directory
		return output;
	}

	@Override
	public boolean equals(final Object obj) {
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
			if (objTransaction == null) {
				throw new HederaClientRuntimeException("Invalid transaction");
			}
			transactionBoolean = Arrays.equals(this.transaction.getTransaction().toBytes(), objTransaction.toBytes());
		}

		var signaturesBoolean = true;
		if (this.signaturePairs.size() == objSignaturePairs.size()) {
			for (final var signaturePair : this.signaturePairs) {
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
		hash = (prime * hash) + ((!"".equals(transactionFileName)) ? transactionFileName.hashCode() : 0);
		hash = (prime * hash) + ((!"".equals(baseName)) ? baseName.hashCode() : 0);
		hash = (prime * hash) + ((comments.isJsonNull()) ? comments.hashCode() : 0);
		return hash;
	}
}
