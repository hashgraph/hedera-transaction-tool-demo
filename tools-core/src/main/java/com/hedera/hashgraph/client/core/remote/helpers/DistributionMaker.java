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

package com.hedera.hashgraph.client.core.remote.helpers;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.opencsv.CSVWriter;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.zeroturnaround.zip.ZipUtil;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyPair;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.hedera.hashgraph.client.core.constants.Constants.SIGNATURE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AMOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSFERS;

public class DistributionMaker implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(DistributionMaker.class);
	public static final String TRANSACTIONS_SUFFIX = "_transactions";
	public static final String SIGNATURES_SUFFIX = "_signatures";
	private final AccountId sender;
	private final AccountId feePayer;
	private final AccountId node;
	private final Timestamp transactionValidDuration;
	private final long transactionFee;
	private final String storageLocation;
	private final String output;
	private final String memo;

	/**
	 * Constructor
	 *
	 * @param sender
	 * 		account that will be the sender
	 * @param feePayer
	 * 		account that will be the fee payer
	 * @param node
	 * 		node account where transactions will be sent
	 * @param transactionValidDuration
	 * 		duration of the transaction
	 * @param transactionFee
	 * 		maximum transaction fee
	 * @param memo
	 * 		the transactions memo
	 * @param storageLocation
	 * 		location where the created transactions will be temporarily stored
	 * @param output
	 * 		the location where the zip files will be exported
	 */
	public DistributionMaker(final AccountId sender, final AccountId feePayer, final AccountId node,
			final Timestamp transactionValidDuration, final long transactionFee, final String memo,
			final String storageLocation, final String output) {
		this.sender = sender;
		this.feePayer = feePayer;
		this.node = node;
		this.transactionValidDuration = transactionValidDuration;
		this.transactionFee = transactionFee;
		this.memo = memo;
		this.storageLocation = storageLocation;
		if (new File(output).mkdirs()) {
			logger.info("Output folder {} created", output);
		}
		this.output = output;
	}

	/**
	 * Builds a transaction and a signature files
	 *
	 * @param distributionData
	 * 		the data required to build a transaction
	 * @param keyPair
	 * 		the key that will sign the transaction
	 */
	public void buildBundle(final BatchLine distributionData, final KeyPair keyPair) throws HederaClientException {
		final var tx = buildTransfer(distributionData);
		final var stx = buildSignature(tx, keyPair);
		final var signaturePair =
				new SignaturePair(PrivateKey.fromBytes(keyPair.getPrivate().getEncoded()).getPublicKey(), stx);
		final var transactionsStorage = String.format("%s_transactions", storageLocation);
		if (new File(transactionsStorage).mkdirs()) {
			logger.info("Folder {} created", transactionsStorage);
		}
		final var signaturesStorage = String.format("%s_signatures", storageLocation);
		if (new File(signaturesStorage).mkdirs()) {
			logger.info("Folder {} created", signaturesStorage);
		}

		final var name = getFullTransactionName(distributionData, tx.getTransactionId().toString());
		final var transactionFilePath =
				String.format("%s_transactions/%s.%s", storageLocation, name, TRANSACTION_EXTENSION);
		writeBytes(transactionFilePath, tx.toBytes());
		logger.info("Writing: {}", transactionFilePath);
		final var signatureFilePath = String.format("%s_signatures/%s.%s", storageLocation, name, SIGNATURE_EXTENSION);
		signaturePair.write(signatureFilePath);
		logger.info("Writing: {}", signatureFilePath);
	}

	/**
	 * Summarizes the transactions and zips the transactions and signatures separately
	 */
	public void pack() throws HederaClientException {
		try {
			summarizeTransactions();
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}
		zipMessages(TRANSACTIONS_SUFFIX, TRANSACTION_EXTENSION);
		zipMessages(SIGNATURES_SUFFIX, SIGNATURE_EXTENSION);
	}

	private ToolTransaction buildTransfer(final BatchLine distributionData) throws HederaClientException {
		final var input = new JsonObject();
		input.add(TRANSACTION_VALID_START_FIELD_NAME, distributionData.getDate().asJSON());

		// Fee payer
		input.add(FEE_PAYER_ACCOUNT_FIELD_NAME, new Identifier(feePayer).asJSON());

		// Use default fee for transactions (note: Large binary files might override this)
		final var feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, transactionFee);
		input.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		// Use default for transaction valid duration
		input.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, transactionValidDuration.getSeconds());

		// Node ID
		input.add(NODE_ID_FIELD_NAME, new Identifier(node).asJSON());

		// Memo
		input.addProperty(MEMO_FIELD_NAME, memo);

		// Transfer
		final var jsonArray = new JsonArray();
		final var senderPair = new JsonObject();
		senderPair.add(ACCOUNT, new Identifier(sender).asJSON());
		senderPair.addProperty(AMOUNT, -distributionData.getAmount());
		jsonArray.add(senderPair);
		final var receiverPair = new JsonObject();
		receiverPair.add(ACCOUNT, distributionData.getReceiverAccountID().asJSON());
		receiverPair.addProperty(AMOUNT, distributionData.getAmount());
		jsonArray.add(receiverPair);

		input.addProperty(NETWORK_FIELD_NAME, String.valueOf(NetworkEnum.MAINNET));

		input.add(TRANSFERS, jsonArray);

		return new ToolTransferTransaction(input);
	}

	private void summarizeTransactions() throws HederaClientException, IOException {
		final var transactions =
				new File(storageLocation + TRANSACTIONS_SUFFIX).listFiles(
						(dir, name) -> name.endsWith(TRANSACTION_EXTENSION));
		assert transactions != null;
		final var files = Arrays.asList(transactions);
		Collections.sort(files);

		final var summary =
				new File(output + File.separator + FilenameUtils.getBaseName(storageLocation) + "_summary.csv");

		Files.deleteIfExists(summary.toPath());
		final PrintWriter printWriter;
		try {
			printWriter = new PrintWriter(summary);
		} catch (final FileNotFoundException e) {
			throw new HederaClientException(e);
		}
		try (final var writer = new CSVWriter(printWriter)) {
			final var header = new String[] { "Filename", "Transaction Json" };
			writer.writeNext(header);
			for (final var file : files) {
				final List<String> dataList = new ArrayList<>();
				final var transaction = new ToolTransferTransaction(file);
				dataList.add(file.getName());
				dataList.add(transaction.asJson().toString().replace(",", ";"));
				var data = new String[2];
				data = dataList.toArray(data);
				writer.writeNext(data);
			}
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}
	}

	private void zipMessages(final String messages, final String ext) throws HederaClientException {
		try {
			final var txToPack = new File(storageLocation + messages).listFiles((dir, name) -> name.endsWith(ext));
			assert txToPack != null;

			final var zipFile = new File(storageLocation + messages.concat(".zip"));

			Files.deleteIfExists(zipFile.toPath());

			ZipUtil.packEntries(txToPack, zipFile);

			if (new File(storageLocation).exists()) {
				Files.deleteIfExists(zipFile.toPath());
				ZipUtil.packEntries(txToPack, zipFile);
			}

			// Delete unzipped transactions
			for (final File file : txToPack) {
				Files.deleteIfExists(file.toPath());
			}

			Files.deleteIfExists(Path.of(storageLocation, messages));
			FileUtils.moveFile(zipFile, new File(output, zipFile.getName()));
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}
	}

	private byte[] buildSignature(final ToolTransaction tx, final KeyPair keyPair) {
		return tx.sign(PrivateKey.fromBytes(keyPair.getPrivate().getEncoded()));
	}

	private String getFullTransactionName(final BatchLine distributionData, final String id) {
		return (id + "-" + distributionData.getReceiverAccountID().toReadableString()).replace(".", "_");
	}


}
