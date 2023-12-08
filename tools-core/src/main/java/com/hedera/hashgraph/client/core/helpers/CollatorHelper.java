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
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.Transaction;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import static com.hedera.hashgraph.client.core.constants.Constants.FILE_NAME_GROUP_SEPARATOR;
import static com.hedera.hashgraph.client.core.constants.Constants.FILE_NAME_INTERNAL_SEPARATOR;
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
	private final Map<PublicKey, String> publicKeys = new HashMap<>();
	private ToolTransaction transaction;
	// The directory name where the file will be stored
	private String transactionFile;
	// The actual file name used to store the signed transaction
	private final String baseName;
	private final JsonArray comments = new JsonArray();

	public CollatorHelper(final File file) throws HederaClientException {
		switch (FilenameUtils.getExtension(file.getName())) {
			case TRANSACTION_EXTENSION:
				this.transaction = new ToolTransaction().parseFile(file);
				this.transactionFile = getFileOutput(file);
				this.baseName = buildBaseName(file);
				break;
			case SIGNATURE_EXTENSION:
				this.transaction = null;
				this.transactionFile = "";
				this.baseName = buildBaseName(file);
				var pair = new SignaturePair(file.getAbsolutePath());
				signaturePairs.add(pair);
				// If the same Public key is used multiple times with different names, this
				// could be an issue.
				publicKeys.put(pair.getPublicKey(), getKeyName(file));
				break;
			case TXT_EXTENSION:
				this.transaction = null;
				this.transactionFile = "";
				this.baseName = buildBaseName(file);
				comments.add(readJsonObject(file));
				break;
			default:
				throw new HederaClientException(CANNOT_PARSE_ERROR_MESSAGE.replace("{}", file.getName()));
		}
	}

	private String getFileOutput(final File file) {
		// If file is null, return the transactionFile (which might also be empty)
		if (file == null) {
			return this.transactionFile;
		}

		// Ensure that the file is a file (exists and not a directory)
		if (file.isFile()) {
			// Get the parent directory name
			var parentName = formatName(file.getParentFile().getName());
			var parentNameParts = parentName.split(FILE_NAME_GROUP_SEPARATOR);
			// All files to be collated should be zipped. These files will now have been unzipped into a directory
			// that ends in _unzipped. Whenever the submission node is included in the naming (due to multiple
			// submission nodes), then both node and a suffix is included.
			if (parentNameParts.length < 3) {
				// If there are fewer parts than 3, which should not happen, then
				// combine what is there as a new name, and return it.
				return String.join(FILE_NAME_INTERNAL_SEPARATOR, parentNameParts);
			} else {
				// Always start with 2 items before the last, as the last is 'unzipped' and the one before that is
				// either the key, or suffix.
				int lastFileNameIndex = parentNameParts.length-3;
				// If 'Node' exists, it will be the 3rd from the back (the currently marked location).
				if (parentNameParts[lastFileNameIndex].startsWith("Node")) {
					// Node exists, go back two parts.
					lastFileNameIndex-=2;
				}
				// Get the remaining parts and recombine it into a standard name.
				parentNameParts = Arrays.copyOf(parentNameParts, lastFileNameIndex+1);
				return String.join(FILE_NAME_GROUP_SEPARATOR, parentNameParts);
			}
		}
		// Return an empty string
		return "";
	}

	private String getKeyName(final File file) {
		// If file is null, ensure that the file is a file (exists and not a directory)
		if (file != null && file.isFile()) {
			// Get the parent directory name
			var parentName = formatName(file.getParentFile().getName());
			var parentNameParts = parentName.split(FILE_NAME_GROUP_SEPARATOR);
			// All files to be collated should be zipped. These files will now have been unzipped into a directory
			// that ends in _unzipped. Whenever the submission node is included in the naming (due to multiple
			// submission nodes), then both node and a suffix is included.
			if (parentNameParts.length < 3) {
				// If there are fewer parts than 3, which should not happen, then
				// combine what is there as a new name, and return it.
				return String.join(FILE_NAME_INTERNAL_SEPARATOR, parentNameParts);
			} else {
				// Always start with the item before the last, as the last is 'unzipped'
				int keyNameIndex = parentNameParts.length-2;
				// If 'Node' exists, it will be the 3rd from the back (the one before the currently marked index).
				if (parentNameParts[keyNameIndex-1].startsWith("Node")) {
					// Node exists, go back two parts.
					keyNameIndex-=2;
				}
				return parentNameParts[keyNameIndex];
			}
		}
		return "";
	}

	public Set<SignaturePair> getSignaturePairs() {
		return signaturePairs;
	}

	public Map<PublicKey, String> getPublicKeys() {
		return publicKeys;
	}

	public ToolTransaction getTransaction() {
		return transaction;
	}

	public void setTransaction(final ToolTransaction transaction) {
		this.transaction = transaction;
	}

	public String getTransactionFile() {
		return transactionFile;
	}

	public void setTransactionFile(final String transactionFile) {
		this.transactionFile = transactionFile;
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
		this.transaction = new ToolTransaction().parseFile(file);
		this.transactionFile = getFileOutput(file);
		addSignature(this.transaction.getTransaction());
	}

	public void addSignature(final PublicKey publicKey, final byte[] signature) {
		final var newSignature = new SignaturePair(publicKey, signature);
		signaturePairs.add(newSignature);
	}

	public void addSignature(final SignaturePair signaturePair) {
		signaturePairs.add(signaturePair);
	}

	public final Transaction<?> collate() throws HederaClientRuntimeException {
		return transaction.collate(signaturePairs);
	}

	public final Transaction<?> collate(final String... folders) throws HederaClientRuntimeException {
		if (folders == null || folders.length == 0) {
			return collate();
		}
		return transaction.collate(signaturePairs, folders);
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
		return transaction.verify(info);
	}

	public boolean verify(final PublicKey publicKey) {
		return transaction.verify(publicKey);
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
	public void addAllSignatures(final CollatorHelper helper) {
		signaturePairs.addAll(helper.getSignaturePairs());
	}

	public void addHelper(final CollatorHelper helper) throws HederaClientException {
		if (!this.baseName.equals(helper.baseName)) {
			throw new HederaClientException("Transactions don't match");
		}
		if (this.transactionFile.equals("")) {
			this.transactionFile = helper.transactionFile;
			this.transaction = helper.transaction;
			this.comments.addAll(helper.comments);
		} else if (helper.hasTransaction()) {
			if (!this.transaction.equals(helper.getTransaction())) {
				logger.error("Transaction {} does not match transaction {}", this.getTransactionFile(),
						helper.getTransactionFile());
				throw new HederaClientException("Transactions don't match");
			}
			// If the transactions are the same (only checks memo, maxTransactionFee, validDuration, and nodeAccountId)
			// then keep the smaller one (will have fewer signatures on it)
			if (helper.getTransaction().toBytes().length < this.transaction.toBytes().length) {
				this.transaction = helper.getTransaction();
			}
		}
		signaturePairs.addAll(helper.getSignaturePairs());
		publicKeys.putAll(helper.publicKeys);
	}

	public String store(final String key) throws HederaClientException {
		var output = this.transactionFile;
		final var outFile = new File(output);
		// If the output is a file, set the output as the parent
		if (outFile.isFile()) {
			output = outFile.getParent();
		}
		// If the key contains "Node", get the Node-nodeId, remove the _basename stuff, add to the output
		if (key.contains("Node")) {
			output = output + "_" + key.substring(0, key.indexOf("_"));
		}
		// Add the temporary directory prefix
		output = "./Temp/" + output;
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
		hash = (prime * hash) + ((!"".equals(transactionFile)) ? transactionFile.hashCode() : 0);
		hash = (prime * hash) + ((!"".equals(baseName)) ? baseName.hashCode() : 0);
		hash = (prime * hash) + ((comments.isJsonNull()) ? comments.hashCode() : 0);
		return hash;
	}

	public static String buildBaseName(final File file) {
		final var pathName = file.getAbsolutePath();
		var fileBaseName = FilenameUtils.getBaseName(pathName);

		return formatName(fileBaseName);
	}

	private static String formatName(@NotNull final String name) {
		// First, determine if the naming convention is the new or old version
		// Old convention does not use '.'
		if (!name.contains(".")) {
			// If the fileName has '_unzipped', then this should follow the form of:
			// 'somefilename-keyname_{Node-0-0-4_transaction}_unzipped
			// where {} is optional.
			// The trick is that a key name can also have '-' in it. So it is hard to tell where the keyName starts.
			// Where this is an issue (somefilename-keyname), they do follow the pattern digits-0_0_digits-{-}digits.
			// When that pattern is not followed, the separator between somefilename and keyname is '_', so it is not
			// an issue.
			if (name.endsWith("_unzipped")) {
				// Ignore the '_unzipped' portion and the suffix portion (if it exists)
				// Get the Node (if it exists) and replace '-' with '.'
				// Get the key (the part right before Node) and ensure '_' is replaced with '-'
				// Combine all parts and put into one name
				final var pattern = Pattern.compile("^\\d+-0_0_\\d+-(-?)\\d+-");
				final var matcher = pattern.matcher(name);
				if (matcher.find()) {
					final var fileName = matcher.group();
					final var splitIndex = matcher.end();
					final var keyName = name.substring(splitIndex);

					return fileName.replace("_", ".")
							.replaceAll("(?<!-)-", FILE_NAME_GROUP_SEPARATOR)
							.concat(keyName);
				}

				return name.replace("Node-0-0-", "Node-0.0.");
			} else {
				// Otherwise replace all '_' with '.' and all non double '--' with '_' (double '--' is replaced with '_-')
				// The file name's form should be similar to:
				//1613195463-0_0_3-1820596828.tx
				// or
				//0_0_9@382308_0-0_0_3498.tx

				// Replace any '-' with the FILE_NAME_GROUP_SEPARATOR, as long as it isn't preceded by what was a '-'
				// If the hash is negative, retain the negative sign
				return name.replace("_", ".")
						.replaceAll("(?<!-)-", FILE_NAME_GROUP_SEPARATOR);
			}
		} else {
			// If using the newer version, there is a chance that some trailing 0s might be present
			// at the end of the transactionId's timestamp portion. Those will be replaced here.
			return name.replaceAll("\\.0{0,9}(?=_)", ".0");
		}
	}
}
