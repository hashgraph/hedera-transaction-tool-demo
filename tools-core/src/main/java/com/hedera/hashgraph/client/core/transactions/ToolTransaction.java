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
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.Messages;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.interfaces.SDKInterface;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.AccountUpdateTransaction;
import com.hedera.hashgraph.sdk.FreezeTransaction;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.HbarUnit;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.hedera.hashgraph.sdk.SystemDeleteTransaction;
import com.hedera.hashgraph.sdk.SystemUndeleteTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransferTransaction;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.Arrays;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_LOAD_TRANSACTION_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_VALIDATE_INPUT_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_READABLE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.setupClient;
import static com.hedera.hashgraph.client.core.utils.JsonUtils.jsonToHBars;
import static java.lang.Thread.sleep;


public class ToolTransaction implements SDKInterface, GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(ToolTransaction.class);
	JsonObject input;
	Transaction<? extends Transaction<?>> transaction;

	TransactionType transactionType;

	// Common fields to build the transaction
	Identifier feePayerID;
	Identifier nodeID;
	Hbar transactionFee;
	Instant transactionValidStart;
	Duration transactionValidDuration;
	NetworkEnum network;
	String memo;

	private enum CollateAndVerifyStatus {
		SUCCESSFUL, NOT_VERIFIABLE, OVER_SIZE_LIMIT;
	}

	public ToolTransaction() {
	}

	public ToolTransaction(final JsonObject input) throws HederaClientException {
		if (checkInput(input)) {
			this.input = input;
			this.transaction = build();
		} else {
			throw new HederaClientException(CANNOT_VALIDATE_INPUT_ERROR_MESSAGE);
		}
	}

	public void setNetwork(final String networkName) {
		this.network = NetworkEnum.valueOf(networkName);
		input.addProperty(NETWORK_FIELD_NAME, networkName);
	}

	public ToolTransaction(final File inputFile) throws HederaClientException {
		switch (FilenameUtils.getExtension(inputFile.getName())) {
			case TRANSACTION_EXTENSION:
				try {
					this.transaction = Transaction.fromBytes(readBytes(inputFile.getAbsolutePath()));
					this.feePayerID = new Identifier(Objects.requireNonNull(transaction.getTransactionId()).accountId);
					this.nodeID = new Identifier((Objects.requireNonNull(transaction.getNodeAccountIds()).get(0)));
					this.transactionFee = transaction.getMaxTransactionFee();
					this.transactionValidStart = Objects.requireNonNull(transaction.getTransactionId().validStart);
					this.transactionValidDuration = transaction.getTransactionValidDuration();
					this.memo = transaction.getTransactionMemo();
				} catch (final InvalidProtocolBufferException e) {
					logger.error(e);
					throw new HederaClientException(CANNOT_LOAD_TRANSACTION_ERROR_MESSAGE);
				}
				break;
			case JSON_EXTENSION:
				final var jsonObject = readJsonObject(inputFile);
				if (checkInput(jsonObject)) {
					this.input = jsonObject;
					this.transaction = build();
				} else {
					throw new HederaClientException(CANNOT_VALIDATE_INPUT_ERROR_MESSAGE);
				}
				break;
			default:
				throw new HederaClientException(CANNOT_VALIDATE_INPUT_ERROR_MESSAGE);
		}
	}

	public ToolTransaction parseFile(final File inputFile) throws HederaClientException {
		try {
			transaction = Transaction.fromBytes(readBytes(inputFile.getAbsolutePath()));
		} catch (final InvalidProtocolBufferException e) {
			throw new HederaClientException(e);
		}
		if (transaction instanceof TransferTransaction) {
			return new ToolTransferTransaction(inputFile);
		}
		if (transaction instanceof AccountCreateTransaction) {
			return new ToolCryptoCreateTransaction(inputFile);
		}
		if (transaction instanceof AccountUpdateTransaction) {
			return new ToolCryptoUpdateTransaction(inputFile);
		}
		if (transaction instanceof SystemDeleteTransaction || transaction instanceof SystemUndeleteTransaction) {
			return new ToolSystemTransaction(inputFile);
		}
		if (transaction instanceof FreezeTransaction) {
			return new ToolFreezeTransaction(inputFile);
		}
		return new ToolTransaction(inputFile);
	}

	public Identifier getFeePayerID() {
		return feePayerID;
	}

	public Identifier getNodeID() {
		return nodeID;
	}

	public Hbar getTransactionFee() {
		return transactionFee;
	}

	public Instant getTransactionValidStart() {
		return transactionValidStart;
	}

	public Duration getTransactionValidDuration() {
		return transactionValidDuration;
	}

	public NetworkEnum getNetwork() {
		return network;
	}

	public String getMemo() {
		return memo;
	}

	public Transaction<?> getTransaction() {
		return transaction;
	}

	public TransactionType getTransactionType() {
		return transactionType;
	}

	public void setTransactionType(final TransactionType transactionType) {
		this.transactionType = transactionType;
	}

	@Override
	public byte[] sign(final PrivateKey key) {
		return key.signTransaction(transaction);
	}

	/**
	 * Creates the signature for the given key, only. No signature is added
	 * to the transaction. When a transaction is signed, and only requires one
	 * signature, then {@link #sign(PrivateKey) signing} should be used. If multiple
	 * signatures from multiple users will be required, then the transaction should
	 * not be signed, but a signature should be created.
	 *
	 * @param key
	 * @return
	 * @throws HederaClientRuntimeException
	 */
	//TODO Either this approach, or move the copy concept into the ToolTransaction and save a .txsig with every execute
	public byte[] createSignature(final PrivateKey key) throws HederaClientRuntimeException {
		try {
			var transactionCopy = Transaction.fromBytes(transaction.toBytes());
			return key.signTransaction(transactionCopy);
		} catch (InvalidProtocolBufferException e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/**
	 * Adds a list of signatures to the transaction. Determines if the resulting
	 * transaction is valid (within size limitations and contains all necessary signatures).
	 *
	 * @param signatures
	 * 		Signatures to be added to the transaction.
	 * @return
	 * 		Boolean indicating if the resulting signed transaction is valid (within size limitations
	 * 		and contains all necessary signatures).
	 */
	private boolean addSignature(final Map<PublicKey, byte[]> signatures) {
		// Add all signatures to the transaction
		for (final var entry : signatures.entrySet()) {
			transaction.addSignature(entry.getKey(), entry.getValue());
		}

		// Check the size of the transaction, if too large
		return (transaction.toBytes().length <= Constants.MAX_TRANSACTION_LENGTH);
	}

	public Transaction<? extends Transaction<?>> collate(final String accountsInfoFolder,
									 final Map<PublicKey, byte[]> signatures) throws HederaClientRuntimeException {
		try {
			// Before anything happens, make sure the transaction is still within size limitations
			var transactionSize = transaction.toBytes().length;
			if (transactionSize > Constants.MAX_TRANSACTION_LENGTH) {
				throw new HederaClientRuntimeException("Transaction size (" +
						transactionSize + ") is over the maximum limit.");
			}

			// Build the list of keys that are required for a valid transaction
			final var keyList = buildKeyList(accountsInfoFolder, signatures);
			if (keyList.isEmpty()) {
				throw new HederaClientRuntimeException("Account information is missing, cannot determine " +
						"the keys required for signing.");
			}

			// Remove any keys from the list of signatures to collate that are already present on the transaction.
			// These keys cannot be removed, and don't need to be re-added, and so don't need to be a part
			// of this process.
			transaction.getSignatures().values().forEach(map -> map.keySet().forEach(signatures::remove));

			// Collate and verify the resulting transaction is within size limitations
			var result = collateAndVerify(keyList, signatures);
			// If the result is OVER_SIZE_LIMIT, that means that the required number of signatures is too great and
			// cannot result in a valid transaction.
			// If the result is NOT_VERIFIABLE, that means that there are required signatures missing.
			if (result == CollateAndVerifyStatus.OVER_SIZE_LIMIT) {
				transactionSize = transaction.toBytes().length;
				throw new HederaClientRuntimeException("Too many signatures are required for this transaction, " +
						"resulting in the transaction size (" +	transactionSize + ") being over the maximum limit.");
			} else if (result == CollateAndVerifyStatus.NOT_VERIFIABLE) {
				throw new HederaClientRuntimeException("Required signatures are still missing and the transaction " +
						"cannot be verified.");
			}
		} catch (IOException e) {
			throw new HederaClientRuntimeException(e);
		}

		// Return the transaction
		return transaction;
	}

	public Transaction<?> collate(final String accountsInfoFolder,
								  final Transaction<?> otherTransaction) throws HederaClientRuntimeException {
		final var signatures = otherTransaction.getSignatures();
		if (signatures.size() != 1) {
			throw new HederaClientRuntimeException("Invalid signature map size");
		}
		for (final var entry : signatures.entrySet()) {
			final var nodeSignatures = entry.getValue();
			collate(accountsInfoFolder, nodeSignatures);
		}
		return transaction;
	}

	public Transaction<?> collate(final String accountsInfoFolder,
								  final Set<SignaturePair> signaturePairs) throws HederaClientRuntimeException {
		// In order to consolidate similar work, do a bit extra work now and take all pairs and create a map
		var signatures = new HashMap<PublicKey, byte[]>();
		for (final var signaturePair : signaturePairs) {
			final var publicKey = signaturePair.getPublicKey();
			final var signature = signaturePair.getSignature();
			signatures.put(publicKey, signature);
		}

		return collate(accountsInfoFolder, signatures);
	}

	@Override
	public Transaction<? extends Transaction<?>> collate(
			final Map<PublicKey, byte[]> signatures) throws HederaClientRuntimeException {
		return collate(Constants.ACCOUNTS_INFO_FOLDER, signatures);
	}

	@Override
	public Transaction<?> collate(final Transaction<?> otherTransaction) throws HederaClientRuntimeException {
		return collate(Constants.ACCOUNTS_INFO_FOLDER, otherTransaction);
	}

	@Override
	public Transaction<?> collate(final Set<SignaturePair> signaturePairs) throws HederaClientRuntimeException {
		return collate(Constants.ACCOUNTS_INFO_FOLDER, signaturePairs);
	}

	/**
	 * Build a keyList of the keys that are a part of the required keyLists. If multiple accounts are
	 * involved, each of the accounts' keyList will be added to this new list.
	 *
	 * @param accountsInfoFolder
	 * 		The location string of the folder containing the account.info files
	 * @param signatures
	 * 		The map of the signatures that are being added to the transaction which are needed in some situations
	 * @return
	 * 		The new keyList containing all keys from any required account involved in this transaction.
	 * @throws IOException
	 */
	protected KeyList buildKeyList(final String accountsInfoFolder,
								   final Map<PublicKey, byte[]> signatures) throws HederaClientRuntimeException {
		// Determine all the accounts that need to be involved in the signing
		final var accounts = getSigningAccounts();
		final var fileSet = accounts.stream()
				.map(account -> CommonMethods.getInfoFiles(accountsInfoFolder, account))
				.filter(files -> files != null && files.length == 1)
				.map(fileArray -> fileArray[0])
				.collect(Collectors.toSet());

		// Build the list of keys that are required for a valid transaction
		final var keyList = KeyList.withThreshold(fileSet.size());
		for (final var file : fileSet) {
			try (final var fis = new FileInputStream((file))) {
				keyList.add(AccountInfo.fromBytes(fis.readAllBytes()).key);
			} catch (IOException e) {
				throw new HederaClientRuntimeException(e);
			}
		}
		return keyList;
	}

	// In order to collate and have a valid signed transaction, verification needs to happen alongside the collating.
	// This method will ensure that the collating does not result in a transaction that exceeds the maximum
	// transaction size limit, if possible.
	private CollateAndVerifyStatus collateAndVerify(final KeyList keyList, Map<PublicKey, byte[]> signatures) throws InvalidProtocolBufferException {
		// Create a backup of the transaction
		final var backupTransaction = Transaction.fromBytes(transaction.toBytes());

		// First, sign the transaction and determine if the resulting transaction is too large
		final var transactionTooLarge = !addSignature(signatures);
		// Second, verify if the transaction, too large or not, is valid after signing
		final var verifiedTransaction = verifyWithKeyList(keyList);
		// If the transaction is valid
		if (verifiedTransaction) {
			// If signed transaction is too large, remove a signature and try again, otherwise return true
			if (transactionTooLarge) {
				// Create the map copy, it will have entries added/removed, and will be used for the next attempt
				var signaturesCopy = new HashMap<>(signatures);
				// For every key in newKeys
				for (final var key : signatures.keySet()) {
					// Reset the transaction
					transaction = Transaction.fromBytes(backupTransaction.toBytes());
					// Remove the key from the signatures
					final var signature = signaturesCopy.remove(key);
					// Try again with the new list of signatures
					if (collateAndVerify(keyList, signaturesCopy) == CollateAndVerifyStatus.SUCCESSFUL) {
						return CollateAndVerifyStatus.SUCCESSFUL;
					}
					// If it didn't work, put the signature back into the list and loop
					signaturesCopy.put(key, signature);
				}
			} else {
				return CollateAndVerifyStatus.SUCCESSFUL;
			}
		}

		// Return why it failed.
		// The transaction is now signed, even those it failed, in order to help
		// with the message to be sent to the user
		return verifiedTransaction ? CollateAndVerifyStatus.OVER_SIZE_LIMIT : CollateAndVerifyStatus.NOT_VERIFIABLE;
	}

	/**
	 * Verify that the supplied key exists in the transaction signature list.
	 *
	 * @param publicKey
	 * 		a public key
	 * @return
	 * @throws HederaClientRuntimeException
	 */
	@Override
	public boolean verify(final PublicKey publicKey) throws HederaClientRuntimeException {
		final var signatures = transaction.getSignatures();
		for (final var entry : signatures.entrySet()) {
			if (entry.getValue().containsKey(publicKey)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Verify that the supplied account's key exists, and passes any
	 * required thresholds, in the transaction signature list.
	 *
	 * @param info
	 * 		account info of the account to test
	 * @return
	 * @throws HederaClientException
	 */
	@Override
	public boolean verify(final AccountInfo info) throws HederaClientException {
		return verifyWithKey(info.key);
	}

	private boolean verifyWithKey(final Key key) {
		if (key instanceof PublicKey) {
			return verify((PublicKey) key);
		}
		return verifyWithKeyList((KeyList) key);
	}

	private boolean verifyWithKeyList(final KeyList keyList) {
		final var threshold = (keyList.threshold != null) ? keyList.threshold : keyList.size();
		var count = 0;
		for (final var key : keyList) {
			if (verifyWithKey(key)) {
				count++;
			}
			if (count >= threshold) {
				return true;
			}
		}
		return false;
	}

	@Override
	public TransactionReceipt submit() throws HederaClientRuntimeException, InterruptedException,
			PrecheckStatusException, ReceiptStatusException {

		final TransactionReceipt receipt;
		try (final var client = setupClient(input)) {
			if (transaction.getTransactionId() == null) {
				throw new HederaClientRuntimeException("Invalid transaction ID");
			}
			if (transaction.getTransactionId().validStart == null) {
				throw new HederaClientRuntimeException("Invalid transaction start");
			}
			final var start = (Objects.requireNonNull(transaction.getTransactionId()).validStart);
			final var delay = start.getEpochSecond() - (new Timestamp().asInstant()).getEpochSecond();
			if (delay > 0) {
				logger.info(Messages.DELAY_MESSAGE, delay);
				sleep(delay * 1000);
			}
			final var transactionResponse = transaction.execute(client);
			receipt = transactionResponse.getReceipt(client);
		} catch (final HederaClientException | TimeoutException e) {
			logger.error(e);
			throw new HederaClientRuntimeException(e);
		}
		logger.info(Messages.TRANSACTION_STATUS_MESSAGE, receipt.status);
		return receipt;
	}

	@Override
	public boolean checkInput(final JsonObject input) {
		var answer = true;

		// Checks the common fields in the input
		if (!CommonMethods.verifyFieldExist(input, FEE_PAYER_ACCOUNT_FIELD_NAME, NODE_ID_FIELD_NAME,
				TRANSACTION_FEE_FIELD_NAME, TRANSACTION_VALID_START_FIELD_NAME,
				TRANSACTION_VALID_DURATION_FIELD_NAME)) {
			return false;
		}

		try {
			final var feePayer = input.getAsJsonObject(FEE_PAYER_ACCOUNT_FIELD_NAME);
			feePayerID = Identifier.parse(feePayer);
		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, FEE_PAYER_ACCOUNT_FIELD_NAME);
			answer = false;
		}

		try {
			final var node = input.getAsJsonObject(NODE_ID_FIELD_NAME);
			nodeID = Identifier.parse(node);
		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, NODE_ID_FIELD_NAME);
			answer = false;
		}

		try {
			final var element = input.get(TRANSACTION_FEE_FIELD_NAME);
			transactionFee =
					(element.isJsonPrimitive()) ? Hbar.from(element.getAsLong(), HbarUnit.TINYBAR) : jsonToHBars(
							element.getAsJsonObject());

		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, TRANSACTION_FEE_FIELD_NAME);
			answer = false;
		}

		try {
			if (input.has(NETWORK_FIELD_NAME)) {
				final var networkName = input.get(NETWORK_FIELD_NAME).getAsString();
				network = NetworkEnum.valueOf(networkName);
			}
		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, NETWORK_FIELD_NAME);
			answer = false;
		}

		try {
			transactionValidStart = new Timestamp(input.get(TRANSACTION_VALID_START_FIELD_NAME)).asInstant();
		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, TRANSACTION_VALID_START_FIELD_NAME);
			answer = false;
		}

		try {
			transactionValidDuration = Duration.ofSeconds(input.get(TRANSACTION_VALID_DURATION_FIELD_NAME).getAsLong());
		} catch (final Exception e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, TRANSACTION_VALID_DURATION_FIELD_NAME);
			answer = false;
		}

		memo = input.has(MEMO_FIELD_NAME) ? input.get(MEMO_FIELD_NAME).getAsString() : "";

		return answer;
	}

	@Override
	public TransactionId getTransactionId() {
		return (transaction != null) ? transaction.getTransactionId() : null;
	}

	@Override
	public String store(final String location) throws HederaClientException {
		final var transactionBytes = transaction.toBytes();
		final var name = Objects.requireNonNull(transaction.getTransactionId()).toString().replace("@",
				"_").replace(".", "-");

		final String filePath;
		if (new File(location).isDirectory()) {
			filePath = location + File.separator + name + "." + TRANSACTION_EXTENSION;
		} else {
			filePath = location;
		}
		try {
			Files.deleteIfExists(Path.of(filePath));
		} catch (final IOException e) {
			logger.error(e.getMessage());
			throw new HederaClientException(e);
		}
		writeBytes(filePath, transactionBytes);
		return filePath;
	}

	@Override
	public boolean read(final String location) throws HederaClientException {
		if (FilenameUtils.getExtension(location).equalsIgnoreCase(TRANSACTION_EXTENSION)) {
			parseFile(new File(location));
			this.memo = transaction.getTransactionMemo();
			this.transactionFee = transaction.getMaxTransactionFee();
			this.transactionValidDuration = transaction.getTransactionValidDuration();
			this.transactionValidStart = Objects.requireNonNull(transaction.getTransactionId()).validStart;
			this.feePayerID = new Identifier(transaction.getTransactionId().accountId);
			this.nodeID = new Identifier(Objects.requireNonNull(transaction.getNodeAccountIds()).get(0));
			return true;
		}
		return false;
	}

	@Override
	public JsonObject asJson() {
		final var jsonTransaction = new JsonObject();
		if (feePayerID != null) {
			jsonTransaction.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerID.asJSON());
		}
		if (nodeID != null) {
			jsonTransaction.add(NODE_ID_FIELD_NAME, nodeID.asJSON());
		}
		if (transactionFee != null) {
			jsonTransaction.addProperty(TRANSACTION_FEE_FIELD_NAME, transactionFee.toTinybars());
		}
		if (transactionFee != null) {
			jsonTransaction.add(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(transactionValidStart).asJSON());
		}
		if (transactionValidStart != null) {
			jsonTransaction.addProperty(TRANSACTION_VALID_START_READABLE_FIELD_NAME,
					new Timestamp(transactionValidStart).asRFCString());
		}
		if (transactionValidDuration != null) {
			jsonTransaction.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, transactionValidDuration.getSeconds());
		}
		jsonTransaction.addProperty(MEMO_FIELD_NAME, memo);
		return jsonTransaction;
	}

	@Override
	public int hashCode() {
		return Arrays.hashCode(transaction.toBytes());
	}

	@Override
	public byte[] toBytes() {
		return (transaction != null) ? transaction.toBytes() : new byte[0];
	}


	/**
	 * Uses a verified json input to build a transaction
	 *
	 * @return a transaction
	 */
	public Transaction<? extends Transaction<?>> build() throws HederaClientRuntimeException {
		return null;
	}

	/**
	 * Determines the public keys that are involved in the transactions.
	 *
	 * @return a list of ByteStrings
	 */
	public Set<ByteString> getSigningKeys(final String accountsInfoFolder) {
		final var accounts = getSigningAccounts();
		return accounts.stream()
				.map(account -> CommonMethods.getInfoFiles(accountsInfoFolder, account))
				.filter(files -> files != null && files.length == 1)
				.flatMap(files -> addToKeySet(files).stream())
				.collect(Collectors.toSet());
	}

	/**
	 * Determines the public keys that are involved in the transactions.
	 *
	 * @return a list of ByteStrings
	 */
	public Set<ByteString> getSigningKeys(){
		return getSigningKeys(Constants.ACCOUNTS_INFO_FOLDER);
	}

	private Set<ByteString> addToKeySet(final File[] files) {
		final Set<ByteString> keysSet = new HashSet<>();
		java.util.Arrays.stream(files).filter(File::exists).forEachOrdered(accountFile -> {
			try {
				final var accountInfo = AccountInfo.fromBytes(readBytes(accountFile.getAbsolutePath()));
				keysSet.addAll(EncryptionUtils.flatPubKeys(Collections.singletonList(accountInfo.key)));
			} catch (final InvalidProtocolBufferException | HederaClientException e) {
				logger.error(e);
			}
		});
		return keysSet;
	}

	/**
	 * Determines the IDs of the accounts that should sign the transaction (does NOT include references to new keys in
	 * entity CREATION or UPDATE transactions)
	 *
	 * @return a Set of AccountId
	 */
	public Set<AccountId> getSigningAccounts() {
		final Set<AccountId> signingAccounts = new HashSet<>();
		signingAccounts.add(Objects.requireNonNull(transaction.getTransactionId()).accountId);
		return signingAccounts;
	}

	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof ToolTransaction)) {
			return false;
		}

		final var tx = ((ToolTransaction) obj).getTransaction();
		if (tx == null && this.transaction == null) {
			return this.asJson().equals(((ToolTransaction) obj).asJson());
		}
		if (tx != null && this.transaction != null) {
			if (!this.transaction.getTransactionMemo().equals(tx.getTransactionMemo()) ||
					!Objects.equals(this.transaction.getTransactionId(), tx.getTransactionId())) {
				return false;
			}
			if (this.transaction.getMaxTransactionFee() == null || tx.getMaxTransactionFee() == null) {
				throw new HederaClientRuntimeException("Invalid transaction max fee");
			}
			if (!this.transaction.getMaxTransactionFee().equals(tx.getMaxTransactionFee())) {
				return false;
			}
			if (this.transaction.getTransactionValidDuration() == null || tx.getTransactionValidDuration() == null) {
				throw new HederaClientRuntimeException("Invalid transaction valid duration");
			}
			return this.transaction.getTransactionValidDuration().equals(tx.getTransactionValidDuration()) &&
					Objects.equals(this.transaction.getNodeAccountIds(), tx.getNodeAccountIds());
		}
		return false;
	}

	public ToolTransaction atNow() throws HederaClientException {
		final var json = this.asJson();
		final var now = new Timestamp();
		json.addProperty(TRANSACTION_VALID_START_READABLE_FIELD_NAME, now.asRFCString());
		json.add(TRANSACTION_VALID_START_FIELD_NAME, now.asJSON());
		return new ToolTransaction(json);
	}
}
