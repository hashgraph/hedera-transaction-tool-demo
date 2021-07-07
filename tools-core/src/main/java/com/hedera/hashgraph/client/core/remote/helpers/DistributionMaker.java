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
	private final AccountId node;
	private final Timestamp transactionValidDuration;
	private final long transactionFee;
	private final String storageLocation;
	private final String output;

	/**
	 * Constructor
	 *
	 * @param sender
	 * 		account that will be the fee payer and sender
	 * @param node
	 * 		node account where transactions will be sent
	 * @param transactionValidDuration
	 * 		duration of the transaction
	 * @param transactionFee
	 * 		maximum transaction fee
	 * @param storageLocation
	 * 		location where the created transactions will be temporarily stored
	 * @param output
	 * 		the location where the zip files will be exported
	 */
	public DistributionMaker(AccountId sender, AccountId node, Timestamp transactionValidDuration, long transactionFee,
			String storageLocation, String output) {
		this.sender = sender;
		this.node = node;
		this.transactionValidDuration = transactionValidDuration;
		this.transactionFee = transactionFee;
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
	 * @throws HederaClientException
	 */
	public void buildBundle(BatchLine distributionData, KeyPair keyPair) throws HederaClientException {
		var tx = buildTransfer(distributionData);
		var stx = buildSignature(tx, keyPair);
		var signaturePair =
				new SignaturePair(PrivateKey.fromBytes(keyPair.getPrivate().getEncoded()).getPublicKey(), stx);
		if (new File(String.format("%s_transactions/", storageLocation)).mkdirs()) {
			logger.info(String.format("Folder %s_transactions/ created", storageLocation));
		}
		if (new File(String.format("%s_signatures/", storageLocation)).mkdirs()) {
			logger.info(String.format("Folder %s_signatures/ created", storageLocation));
		}

		var name = getFullTransactionName(distributionData, tx.getTransactionId().toString());
		writeBytes(String.format("%s_transactions/%s.%s", storageLocation, name, TRANSACTION_EXTENSION), tx.toBytes());
		logger.info(String.format("Writing: %s_transactions/%s.%s", storageLocation, name, TRANSACTION_EXTENSION));
		signaturePair.write(String.format("%s_signatures/%s.%s", storageLocation, name, SIGNATURE_EXTENSION));
		logger.info(String.format("Writing: %s_signatures/%s.%s", storageLocation, name, SIGNATURE_EXTENSION));
	}

	/**
	 * Summarizes the transactions and zips the transactions and signatures separately
	 *
	 * @throws HederaClientException
	 */
	public void pack() throws HederaClientException {
		summarizeTransactions();
		zipMessages(TRANSACTIONS_SUFFIX, TRANSACTION_EXTENSION);
		zipMessages(SIGNATURES_SUFFIX, SIGNATURE_EXTENSION);
	}

	private ToolTransaction buildTransfer(BatchLine distributionData) throws HederaClientException {
		var input = new JsonObject();
		input.add(TRANSACTION_VALID_START_FIELD_NAME, distributionData.getDate().asJSON());

		// Fee payer
		input.add(FEE_PAYER_ACCOUNT_FIELD_NAME, new Identifier(sender).asJSON());

		// Use default fee for transactions (note: Large binary files might override this)
		var feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, transactionFee);
		input.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		// Use default for transaction valid duration
		input.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, transactionValidDuration.getSeconds());

		// Node ID
		input.add(NODE_ID_FIELD_NAME, new Identifier(node).asJSON());

		// Transfer
		var jsonArray = new JsonArray();
		var senderPair = new JsonObject();
		senderPair.add(ACCOUNT, new Identifier(sender).asJSON());
		senderPair.addProperty(AMOUNT, -distributionData.getAmount());
		jsonArray.add(senderPair);
		var receiverPair = new JsonObject();
		receiverPair.add(ACCOUNT, distributionData.getReceiverAccountID().asJSON());
		receiverPair.addProperty(AMOUNT, distributionData.getAmount());
		jsonArray.add(receiverPair);

		input.addProperty(NETWORK_FIELD_NAME, String.valueOf(NetworkEnum.MAINNET));

		input.add(TRANSFERS, jsonArray);

		return new ToolTransferTransaction(input);
	}

	private void summarizeTransactions() throws HederaClientException {
		var transactions =
				new File(storageLocation + TRANSACTIONS_SUFFIX).listFiles(
						(dir, name) -> name.endsWith(TRANSACTION_EXTENSION));
		assert transactions != null;
		var files = Arrays.asList(transactions);
		Collections.sort(files);

		var summary = new File(output + File.separator + FilenameUtils.getBaseName(storageLocation) + "_summary.csv");

		if (summary.exists() && summary.delete()) {
			logger.info("Summary file deleted");
		}
		PrintWriter printWriter;
		try {
			printWriter = new PrintWriter(summary);
		} catch (FileNotFoundException e) {
			throw new HederaClientException(e);
		}
		try (var writer = new CSVWriter(printWriter)) {
			var header = new String[] { "Filename", "Transaction Json" };
			writer.writeNext(header);
			for (var file : files) {
				List<String> dataList = new ArrayList<>();
				var transaction = new ToolTransferTransaction(file);
				dataList.add(file.getName());
				dataList.add(transaction.asJson().toString().replace(",", ";"));
				var data = new String[2];
				data = dataList.toArray(data);
				writer.writeNext(data);
			}
		} catch (IOException e) {
			throw new HederaClientException(e);
		}
	}

	private void zipMessages(String messages, String ext) throws HederaClientException {
		try {
			var txToPack = new File(storageLocation + messages).listFiles((dir, name) -> name.endsWith(ext));
			assert txToPack != null;

			final var zipFile = new File(storageLocation + messages.concat(".zip"));

			Files.deleteIfExists(zipFile.toPath());

			ZipUtil.packEntries(txToPack, zipFile);

			if (new File(storageLocation).exists()) {
				Files.deleteIfExists(zipFile.toPath());
				ZipUtil.packEntries(txToPack, zipFile);
			}

			// Delete unzipped transactions
			Arrays.stream(txToPack).forEach(File::delete);

			if (new File(storageLocation + messages).delete()) {
				logger.info(String.format("Folder %s deleted", messages));
			}

			FileUtils.moveFile(zipFile, new File(output, zipFile.getName()));
		} catch (IOException e) {
			throw new HederaClientException(e);
		}
	}

	private byte[] buildSignature(ToolTransaction tx, KeyPair keyPair) {
		return tx.sign(PrivateKey.fromBytes(keyPair.getPrivate().getEncoded()));
	}

	private String getFullTransactionName(BatchLine distributionData, String id) {
		return (id + "-" + distributionData.getReceiverAccountID().toReadableString()).replace(".", "_");
	}



}
