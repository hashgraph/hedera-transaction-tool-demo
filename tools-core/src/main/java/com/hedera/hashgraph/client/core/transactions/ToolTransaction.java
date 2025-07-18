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
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.interfaces.SDKInterface;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.core.utils.JsonUtils;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.AccountUpdateTransaction;
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.FileUpdateTransaction;
import com.hedera.hashgraph.sdk.FreezeTransaction;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.HbarUnit;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.NodeCreateTransaction;
import com.hedera.hashgraph.sdk.NodeDeleteTransaction;
import com.hedera.hashgraph.sdk.NodeUpdateTransaction;
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

import static com.hedera.hashgraph.client.core.constants.Constants.FILE_NAME_GROUP_SEPARATOR;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_CREATION_METADATA_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_LOAD_TRANSACTION_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_VALIDATE_INPUT_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_FIELD_INPUT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_READABLE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.remote.TransactionCreationMetadataFile.NODES_STRING;
import static com.hedera.hashgraph.client.core.remote.helpers.AccountList.INPUT_STRING;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.setupClient;
import static com.hedera.hashgraph.client.core.utils.JsonUtils.jsonToHBars;
import static com.hedera.hashgraph.client.core.utils.ValidationUtils.validateSignature;
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
	String network;
	String memo;
	String nodeInput;

	private enum CollateAndVerifyStatus {
		SUCCESSFUL, NOT_VERIFIABLE, OVER_SIZE_LIMIT
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
		this.network = networkName;
		if (input != null) {
			input.addProperty(NETWORK_FIELD_NAME, networkName);
		}
	}

	public ToolTransaction(final File inputFile) throws HederaClientException {
		switch (FilenameUtils.getExtension(inputFile.getName())) {
			case SIGNED_TRANSACTION_EXTENSION:
			case TRANSACTION_EXTENSION:
				try {
					this.transaction = CommonMethods.getTransaction(readBytes(inputFile.getAbsolutePath()));
					this.feePayerID = new Identifier(Objects.requireNonNull(transaction.getTransactionId()).accountId);
					this.nodeID = new Identifier((Objects.requireNonNull(transaction.getNodeAccountIds()).get(0)));
					this.transactionFee = transaction.getMaxTransactionFee();
					this.transactionValidStart = Objects.requireNonNull(transaction.getTransactionId().validStart);
					this.transactionValidDuration = transaction.getTransactionValidDuration();
					this.memo = transaction.getTransactionMemo();
					final var tcm = new File(inputFile.getAbsolutePath()
							.replace(TRANSACTION_EXTENSION, TRANSACTION_CREATION_METADATA_EXTENSION));
					if (tcm.exists()) {
						var contents = readJsonObject(tcm.getPath());
						if (contents.has(NODES_STRING)) {
							contents = contents.get(NODES_STRING).getAsJsonObject();
							if (contents.has(INPUT_STRING)) {
								nodeInput = contents.get(INPUT_STRING).getAsString();
							}
						}
					}
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
			transaction = CommonMethods.getTransaction(readBytes(inputFile.getAbsolutePath()));
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
		if (transaction instanceof FileUpdateTransaction) {
			return new ToolFileUpdateTransaction(inputFile);
		}
		if (transaction instanceof FileAppendTransaction) {
			return new ToolFileAppendTransaction(inputFile);
		}
		if (transaction instanceof NodeCreateTransaction) {
			return new ToolNodeCreateTransaction(inputFile);
		}
		if (transaction instanceof NodeUpdateTransaction) {
			return new ToolNodeUpdateTransaction(inputFile);
		}
		if (transaction instanceof NodeDeleteTransaction) {
			return new ToolNodeDeleteTransaction(inputFile);
		}
		return new ToolTransaction(inputFile);
	}

	public Identifier getFeePayerID() {
		return feePayerID;
	}

	public Identifier getNodeID() {
		return nodeID;
	}

	public String getNodeInput() {
		return nodeInput;
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

	public String getNetwork() {
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
	public byte[] createSignature(final PrivateKey key) throws HederaClientRuntimeException {
		try {
			var transactionCopy = CommonMethods.getTransaction(transaction.toBytes());
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
	private boolean addSignatures(final Map<PublicKey, byte[]> signatures) {
		// Add all signatures to the transaction
		try {
			for (final var entry : signatures.entrySet()) {
				if (!validateSignature(transaction, entry.getKey(), entry.getValue())) {
					logger.warn("The Public Key ({}) and signature did not match the transaction, it will not be added.", entry.getKey());
				} else {
					transaction.addSignature(entry.getKey(), entry.getValue());
				}
			}
		} catch (final IllegalAccessException e) {
			logger.error("Failed to get transaction body bytes: {}", e.getMessage());
		}

		// Check the size of the transaction, if too large
		// NOTE: if using SDK controlled, multi-node transactions, this will not work.
		// This will be the size of all the transactions, not just the one being signed.
		return (transaction.toBytes().length <= Constants.MAX_TRANSACTION_LENGTH);
	}

	public Transaction<? extends Transaction<?>> collate(final Map<PublicKey, byte[]> signatures,
										 final String... accountsInfoFolders) throws HederaClientRuntimeException {
		try {
			// Before anything happens, make sure the transaction is within size limitations
			final var transactionSize = transaction.toBytes().length;
			if (transactionSize > Constants.MAX_TRANSACTION_LENGTH) {
				throw new HederaClientRuntimeException("Transaction size (" +
						transactionSize + ") is over the maximum limit.");
			}

			// Get the keys from the accountInfoFolders list, based on the required keys
			// Build the list of keys that are required for a valid transaction, any additional keys
			// used/needed will not be present in this keyList, but will still be required in the signing.
			// This canNOT include the new keys in the key list because all new keys are required but the
			// key list would allow for thresholds to bypass this requirement.
			final var requiredKeyList = getSigningAccountKeys(buildKeyList(accountsInfoFolders));

			// For every key in this.key that isn't in requiredKeyList, sign the transaction.
			// This could be because the account info is missing, or outdated,
			// or is being updated, or it could be due to extra unneeded keys.
			// In all cases, the user will need to manually verify the keys in use.
			signatures.entrySet().forEach(entry -> {
				try {
					// Double check that the signature belongs to the transaction before applying
					if (validateSignature(transaction, entry.getKey(), entry.getValue()) &&
							!keyListContainsKey(requiredKeyList, entry.getKey())) {
						transaction.addSignature(entry.getKey(), entry.getValue());
					}
				} catch (final IllegalAccessException e) {
					logger.error("Failed to get transaction body bytes: {}", e.getMessage());
				}
			});

			// Remove any keys from the list of signatures to collate that are already present on the transaction.
			// These keys cannot be removed, don't need to be re-added, and so don't need to be a part
			// of this process.
			final var currentSignatures = transaction.getSignatures().values();
			if (currentSignatures.isEmpty() && signatures.isEmpty()) {
				throw new HederaClientRuntimeException("No signatures were supplied for this transaction.");
			}
			currentSignatures.forEach(map -> map.keySet().forEach(signatures::remove));

			// Make a backup copy of the transaction. If it fails collation, we will reset the transaction
			// and add all signatures in order to run the verification tree and display the results before discarding
			// the transaction. We will use the bytes of the transaction at this point, which may already include
			// some signatures.
			final var backupTransactionBytes = transaction.toBytes();

			CollateAndVerifyStatus result;
			// Now check if the requiredKeyList is empty. If empty, then all signatures supplied are already added to
			// the transaction, no additional verification is possible. Check the size of the transaction.
			// Otherwise, proceed with the smart collate.
			if (requiredKeyList.isEmpty()) {
				result = (transaction.toBytes().length <= Constants.MAX_TRANSACTION_LENGTH) ?
						CollateAndVerifyStatus.SUCCESSFUL : CollateAndVerifyStatus.OVER_SIZE_LIMIT;
			} else {
				result = collateAndVerify(requiredKeyList, signatures);
			}

			if (result != CollateAndVerifyStatus.SUCCESSFUL) {
				// If the result is not successful, reset the transaction to the backup copy
				transaction = CommonMethods.getTransaction(backupTransactionBytes);
				addSignatures(signatures);

				// If the result is OVER_SIZE_LIMIT, that means that the required number of signatures is too great and
				// cannot result in a valid transaction.
				// If the result is NOT_VERIFIABLE, that means that there are required signatures missing.
				if (result == CollateAndVerifyStatus.OVER_SIZE_LIMIT) {
					logger.error(
							"Unable to remove unnecessary signatures to produce a valid transaction within the size limit. " +
									"All attempted combinations exceeded the maximum allowed size."
					);
					throw new HederaClientRuntimeException("Transaction over size limit.");
				} else if (result == CollateAndVerifyStatus.NOT_VERIFIABLE) {
					logger.error("Required signatures are missing and the transaction " +
							"cannot be verified.");
					throw new HederaClientRuntimeException("Transaction not verifiable.");
				}
			}
		} catch (IOException e) {
			throw new HederaClientRuntimeException(e);
		}

		// Return the transaction
		return transaction;
	}

	public Transaction<?> collate(final Transaction<?> otherTransaction,
								  final String... accountsInfoFolders) throws HederaClientRuntimeException {
		final var signatures = otherTransaction.getSignatures();
		if (signatures.size() != 1) {
			throw new HederaClientRuntimeException("Invalid signature map size");
		}
		for (final var entry : signatures.entrySet()) {
			final var nodeSignatures = entry.getValue();
			collate(nodeSignatures, accountsInfoFolders);

		}
		return transaction;
	}

	public Transaction<?> collate(final Set<SignaturePair> signaturePairs,
								  final String... accountsInfoFolders) throws HederaClientRuntimeException {
		// In order to consolidate similar work, do a bit extra work now and take all pairs and create a map
		var signatures = new HashMap<PublicKey, byte[]>();
		for (final var signaturePair : signaturePairs) {
			final var publicKey = signaturePair.getPublicKey();
			final var signature = signaturePair.getSignature();
			signatures.put(publicKey, signature);
		}

		return collate(signatures, accountsInfoFolders);
	}

	@Override
	public Transaction<? extends Transaction<?>> collate(
			final Map<PublicKey, byte[]> signatures) throws HederaClientRuntimeException {
		return collate(signatures, Constants.ACCOUNTS_INFO_FOLDER);
	}

	@Override
	public Transaction<?> collate(final Transaction<?> otherTransaction) throws HederaClientRuntimeException {
		return collate(otherTransaction, Constants.ACCOUNTS_INFO_FOLDER);
	}

	@Override
	public Transaction<?> collate(final Set<SignaturePair> signaturePairs) throws HederaClientRuntimeException {
		return collate(signaturePairs, Constants.ACCOUNTS_INFO_FOLDER);
	}

	/**
	 * Build a keyList of the keys that are a part of the required keyLists. If multiple accounts are
	 * involved, each of the accounts' keyList will be added to this new list. This differs from
	 * getSigningKeys in that this returns a KeyList, while getSigningKeys returns a list of all keys in byte form.
	 * In addition, getSigningKeys will include the new keys in the case of a CryptoUpdateTransaction, whereas
	 * buildKeyList is strictly the key structure that is currently required.
	 *
	 * @param accountsInfoFolders
	 * 		The location string(s) of the folder(s) containing the account.info files
	 * @return
	 * 		The new keyList containing all keys from any required account involved in this transaction.
	 * @throws HederaClientRuntimeException
	 */
	public KeyList buildKeyList(final String[] accountsInfoFolders) throws HederaClientRuntimeException {
		// Determine all the accounts that need to be involved in the signing
		final var accounts = getSigningAccountIds();
		final var fileSet = accounts.stream()
				.flatMap(account -> java.util.Arrays.stream(accountsInfoFolders)
							.map(a -> CommonMethods.getInfoFiles(a, account)))
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

		// Return the keyList
		return keyList;
	}

	private boolean keyListContainsKey(KeyList keyList, Key key) {
		// If the keyList contains the key, return true
		if (keyList.contains(key)) {
			return true;
		}
		// Otherwise, go through each key in the keyList. If it is a keyList,
		// then recall this method and determine if it contains the key
		for (final var k : keyList) {
			if (k instanceof KeyList && keyListContainsKey((KeyList)k, key)) {
				return true;
			}
		}
		// The key was not found, return false
		return false;
	}

	// In order to collate and have a valid signed transaction, verification needs to happen alongside the collating.
	// This method will ensure that the collating does not result in a transaction that exceeds the maximum
	// transaction size limit, if possible.
	private CollateAndVerifyStatus collateAndVerify(final KeyList keyList, Map<PublicKey, byte[]> signatures) throws InvalidProtocolBufferException {
		// Create a backup of the transaction
		final var backupTransactionBytes = transaction.toBytes();

		// First, sign the transaction and determine if the resulting transaction is too large
		final var transactionTooLarge = !addSignatures(signatures);
		// Second, verify if the transaction, too large or not, is valid after signing
		final var verifiedTransaction = verifyWithKeyList(keyList);
		// If the transaction is valid
		if (verifiedTransaction) {
			// If signed transaction is too large, remove a signature and try again, otherwise return true
			if (transactionTooLarge) {
				// Create the map copy, it will have entries added/removed, and will be used for the next attempt
				var signaturesCopy = new HashMap<>(signatures);
				for (final var key : signatures.keySet()) {
					// Reset the transaction
					transaction = CommonMethods.getTransaction(backupTransactionBytes);
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
			if (input.has(NODE_FIELD_INPUT)) {
				nodeInput = input.get(NODE_FIELD_INPUT).getAsString();
			}
		} catch (final Exception ex) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, NODE_FIELD_INPUT);
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
				CommonMethods.getClient(networkName);
				network = networkName;
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

	public String buildFileName() {
		final var accountId = getFeePayerID();
		final var seconds = getTransactionValidStart().getEpochSecond();
		return String.join(FILE_NAME_GROUP_SEPARATOR, seconds+"",
				accountId.toReadableString(), hashCode()+"");
	}

	@Override
	public String store(final String location) throws HederaClientException {
		final var transactionBytes = transaction.toBytes();

		final String filePath;
		if (new File(location).isDirectory()) {
			filePath = location + File.separator + buildFileName() + "." + TRANSACTION_EXTENSION;
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
		if (nodeInput != null) {
			jsonTransaction.addProperty(NODE_FIELD_INPUT, nodeInput);
		}
		if (transactionFee != null) {
			jsonTransaction.add(TRANSACTION_FEE_FIELD_NAME, JsonUtils.hBarsToJsonObject(transactionFee));
		}
		if (transactionValidStart != null) {
			jsonTransaction.add(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(transactionValidStart).asJSON());
			jsonTransaction.addProperty(TRANSACTION_VALID_START_READABLE_FIELD_NAME,
					new Timestamp(transactionValidStart).asRFCString());
		}
		if (transactionValidDuration != null) {
			jsonTransaction.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, transactionValidDuration.getSeconds());
		}
		if (network != null) {
			jsonTransaction.addProperty(NETWORK_FIELD_NAME, network);
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
	 * @return a set of ByteStrings
	 */
	public Set<ByteString> getSigningKeys(final String accountsInfoFolder) {
		final var accounts = getSigningAccountIds();
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
	public Set<AccountId> getSigningAccountIds() {
		final Set<AccountId> signingAccounts = new HashSet<>();
		signingAccounts.add(Objects.requireNonNull(transaction.getTransactionId()).accountId);
		return signingAccounts;
	}


	public KeyList getSigningAccountKeys(KeyList accountKeyList) {
		return accountKeyList;
	}

	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof ToolTransaction)) {
			return false;
		}

		final var other = (ToolTransaction) obj;
		return Objects.equals(this.transactionType, other.transactionType) &&
				Objects.equals(this.feePayerID, other.feePayerID) &&
				Objects.equals(this.nodeID, other.nodeID) &&
				Objects.equals(this.transactionFee, other.transactionFee) &&
				Objects.equals(this.transactionValidStart, other.transactionValidStart) &&
				Objects.equals(this.transactionValidDuration, other.transactionValidDuration) &&
				Objects.equals(this.network, other.network) &&
				Objects.equals(this.memo, other.memo) &&
				Objects.equals(this.nodeInput, other.nodeInput);
	}

	public ToolTransaction atNow() throws HederaClientException {
		final var json = this.asJson();
		final var now = new Timestamp();
		json.addProperty(TRANSACTION_VALID_START_READABLE_FIELD_NAME, now.asRFCString());
		json.add(TRANSACTION_VALID_START_FIELD_NAME, now.asJSON());
		return new ToolTransaction(json);
	}
}
