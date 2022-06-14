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

package com.hedera.hashgraph.client.cli.options;

import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.helpers.CollatorHelper;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.PublicKey;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import picocli.CommandLine;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.SIGNATURE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.ZIP_EXTENSION;

@CommandLine.Command(name = "collate", aliases = { "col" }, description = "Collate a transaction and its corresponding" +
		" signature files for submission to a Hedera network.")
public class CollateCommand implements ToolCommand, GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(CollateCommand.class);

	@CommandLine.Option(names = { "-f", "--folders" }, description = "The path to the root folder containing the " +
			"transaction and signature files to collate",
			required = true)
	private String rootFolder;

	@CommandLine.Option(names = { "-a", "--account-info" }, description = "The path to the account info files for " +
			"the account(s) corresponding to the transaction", split = ",")
	private String[] infoFiles;

	@CommandLine.Option(names = { "-k", "--public-key" }, description = "The path to the public key files that " +
			"correspond with the transaction's required signatures", split = ",")
	private String[] keyFiles;

	@CommandLine.Option(names = { "-o", "--output-directory" }, description = "The path to the folder where the " +
			"collated transaction files will be stored")
	private String out = "";

	private final Map<String, CollatorHelper> transactions = new HashMap<>();
	private final Map<File, AccountInfo> infos = new HashMap<>();
	private final Map<File, PublicKey> publicKeys = new HashMap<>();
	private final List<File> unzips = new ArrayList<>();
	private final Set<AccountId> knownIds = new HashSet<>();

	@Override
	public void execute() throws HederaClientException, IOException {
		if ("".equals(out)) {
			out = rootFolder;
		}

		final var root = new File(rootFolder);
		if (!root.exists()) {
			throw new HederaClientException("Cannot find the transactions root folder");
		}
		// Parse account info files form inputs
		loadVerificationFiles(infoFiles, Constants.INFO_EXTENSION);

		// Parse public key files from inputs
		loadVerificationFiles(keyFiles, Constants.PUB_EXTENSION);

		// Parse transactions
		loadTransactions(root);

		for (final var unzip : unzips) {
			FileUtils.deleteDirectory(unzip);
		}

		final var verification = verifyTransactions();

		if (verification == null) {
			logger.info("Transactions not verified. Terminating");
			return;
		}

		writeCSV(out + File.separator + "verification.csv", verification);
		logger.info("Verification done");

		final Set<String> outputs = new HashSet<>();
		for (final var entry : transactions.entrySet()) {
			final var helper = entry.getValue();
			helper.collate();
			outputs.add(helper.store(entry.getKey()));
		}
		logger.info("Transactions collated and stored");

		moveToOutput(outputs);

		logger.info("Collation done");
	}


	/**
	 * Moves the files to the designate output directory
	 *
	 * @param outputs
	 * 		the output files produced by the collator
	 * @throws IOException
	 * 		if the file move or deletion fails
	 */
	private void moveToOutput(final Set<String> outputs) throws IOException {
		for (final var output : outputs) {
			final var files = Objects.requireNonNull(new File(output).listFiles());
			if (moreThanOneFile(output, files)) {
				continue;
			}

			final var prefix = (output.contains("Node")) ? output.substring(output.lastIndexOf("_") + 1) + "_" : "";
			final var destination = new File(out + File.separator + prefix + files[0].getName());

			if (Files.deleteIfExists(destination.toPath())) {
				logger.info("Destination file deleted");
			}

			FileUtils.moveFile(files[0], destination);
			FileUtils.deleteDirectory(new File(output));
		}
	}

	/**
	 * Loads the files used for verification
	 *
	 * @param files
	 * 		An array of file paths
	 * @param extension
	 * 		the extension of the files to load.
	 * @throws HederaClientException
	 * 		if an incorrect extension is found or the app encounters an IOException
	 */
	private void loadVerificationFiles(final String[] files, final String extension) throws HederaClientException {
		if (files != null && files.length > 0) {
			final var infoArray = Arrays.stream(files).map(File::new).toArray(File[]::new);
			parseFiles(infoArray, extension);
		}
	}

	private boolean moreThanOneFile(final String output, final File[] files) throws IOException {
		if (files.length <= 1) {
			return false;
		}
		final var zippedOutput = zipFolder(output);
		if (!rootFolder.equals(out)) {
			final var destination = new File(out, zippedOutput.getName());
			FileUtils.moveFile(zippedOutput, destination);
		}
		FileUtils.deleteDirectory(new File(output));
		return true;
	}

	private Map<String, List<String>> verifyTransactions() throws HederaClientException {
		final Map<String, Set<String>> verifyWithFiles = new HashMap<>();

		final Set<AccountId> requiredIds = new HashSet<>();
		Set<String> ids = new HashSet<>();


		for (final var entry : transactions.entrySet()) {
			if (verifyWithFiles.containsKey(entry.getKey())) {
				ids = verifyWithFiles.get(entry.getKey());
			}

			final var helper = entry.getValue();
			requiredIds.addAll(helper.getSigningAccounts());

			ids.addAll(getAccountIds(helper));
			ids.addAll(getPublicKeyNames(helper));

			final List<String> sortedIDs = new ArrayList<>(ids);
			Collections.sort(sortedIDs);
			verifyWithFiles.put(FilenameUtils.getBaseName(helper.getTransactionFile()), new HashSet<>(sortedIDs));
		}

		for (final AccountId requiredId : requiredIds) {
			final var requiredIdString = requiredId.toString();
			if (knownIds.contains(requiredId) && !ids.contains(requiredIdString)) {
				logger.info("Transactions have not been signed by required account {}", requiredIdString);
				return null;
			}
		}

		final Map<String, List<String>> verifyTransactions = new HashMap<>();
		for (final Map.Entry<String, Set<String>> entry : verifyWithFiles.entrySet()) {
			final var key = entry.getKey();
			final var value = entry.getValue();
			final List<String> sortedIDs = new ArrayList<>(value);
			Collections.sort(sortedIDs);
			verifyTransactions.put(key, sortedIDs);
		}


		return verifyTransactions;
	}

	private List<String> getPublicKeyNames(final CollatorHelper helper) {
		final List<String> idList = new ArrayList<>();
		for (final Map.Entry<File, PublicKey> keyEntry : publicKeys.entrySet()) {
			if (helper.verify(keyEntry.getValue())) {
				idList.add(FilenameUtils.getBaseName(keyEntry.getKey().getName()));
			}
		}
		return idList;
	}

	private List<String> getAccountIds(final CollatorHelper helper) throws HederaClientException {
		final List<String> idList = new ArrayList<>();
		for (final var info : infos.entrySet()) {
			if (helper.verify(info.getValue())) {
				idList.add(info.getValue().accountId.toString());
			}
		}
		return idList;
	}

	private void loadTransactions(final File root) throws HederaClientException {
		if (root.isFile()) {
			handle(root);
			return;
		}
		if (root.isDirectory()) {
			final var files = root.listFiles();
			if (files == null) {
				throw new HederaClientRuntimeException("Unable to read files from root");
			}
			for (final var file : files) {
				loadTransactions(file);
			}
		}
	}

	private void handle(final File file) throws HederaClientException {
		if (isZip(file)) {
			// unzip and handle
			handleZip(file);
			return;
		}
		if (isTransaction(file) || isSigPair(file) || isComment(file)) {
			// add to map
			handleFile(file);
		}
	}

	private void handleFile(final File file) throws HederaClientException {
		final var key = buildBaseName(file);
		final var helper = new CollatorHelper(file);
		if (transactions.containsKey(key)) {
			helper.addHelper(transactions.get(key));
		}
		transactions.put(key, helper);
	}

	private String buildBaseName(final File file) {
		final var pathName = file.getAbsolutePath();
		final var baseName = FilenameUtils.getBaseName(pathName);

		final var suffix0 = pathName.contains("signatures") ? "_signatures" : "";
		final var suffix = pathName.contains("transactions") ? "_transactions" : suffix0;

		if (pathName.contains("Node")) {
			return pathName.substring(pathName.indexOf("Node"), pathName.indexOf(suffix)) + "_" + baseName;
		}

		return baseName;

	}

	private void handleZip(final File file) throws HederaClientException {

		final var destination = file.getAbsolutePath().replace(".zip", "_unzipped");
		final var unzipped = unZip(file.getAbsolutePath(), destination);
		loadTransactions(unzipped);
		if (!new File(destination).exists()) {
			throw new HederaClientException("Files were not unzipped");
		}
		unzips.add(new File(destination));
	}

	private boolean isZip(final File file) {
		return file.getName().endsWith(ZIP_EXTENSION);
	}

	private boolean isTransaction(final File file) {
		return file.getName().endsWith(Constants.TRANSACTION_EXTENSION);
	}

	private boolean isSigPair(final File file) {
		return SIGNATURE_EXTENSION.equals(FilenameUtils.getExtension(file.getName()));
	}

	private boolean isComment(final File file) {
		return file.getName().endsWith(Constants.TXT_EXTENSION);
	}

	private void parseFiles(final File[] files, final String extension) throws HederaClientException {
		for (final var file : files) {
			if (!file.exists()) {
				throw new HederaClientException(String.format("The file %s does not exist", file.getName()));
			}
			if (file.isDirectory()) {
				final var inner = file.listFiles((dir, name) -> name.endsWith(extension));
				if (inner == null) {
					throw new HederaClientRuntimeException("Unable to read files from directory");
				}
				parseFiles(inner, extension);
				return;
			}
			try {
				switch (extension) {
					case Constants.INFO_EXTENSION:
						infos.put(file, AccountInfo.fromBytes(readBytes(file)));
						knownIds.add(infos.get(file).accountId);
						break;
					case Constants.PUB_EXTENSION:
						publicKeys.put(file, EncryptionUtils.publicKeyFromFile(file.getAbsolutePath()));
						break;
					default:
						throw new HederaClientException("Not implemented");
				}

			} catch (final InvalidProtocolBufferException e) {
				logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, file.getName());
				throw new HederaClientException(e);
			}
		}
	}

}
