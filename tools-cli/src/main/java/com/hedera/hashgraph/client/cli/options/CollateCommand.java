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
import com.hedera.hashgraph.client.cli.helpers.CollatorHelper;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
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

	@CommandLine.Option(names = { "-a", "--account-info" }, description = "The path to the account info files for the" +
			" " +
			"account(s) corresponding to the transaction", split = ",")
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

		var root = new File(rootFolder);
		if (!root.exists()) {
			throw new HederaClientException("Cannot find the transactions root folder");
		}
		// Parse account info files form inputs
		if (infoFiles != null && infoFiles.length > 0) {
			var infoArray = Arrays.stream(infoFiles).map(File::new).toArray(File[]::new);
			parseFiles(infoArray, Constants.INFO_EXTENSION);
		}

		// Parse public key files from inputs
		if (keyFiles != null && keyFiles.length > 0) {
			var keys = Arrays.stream(keyFiles).map(File::new).toArray(File[]::new);
			parseFiles(keys, Constants.PUB_EXTENSION);
		}
		loadTransactions(root);

		for (var unzip : unzips) {
			FileUtils.deleteDirectory(unzip);
		}

		final var verification = verifyTransactions();

		if (verification == null) {
			logger.info("Transactions not verified. Terminating");
			return;
		}

		writeCSV(out + File.separator + "verification.csv", verification);
		logger.info("Verification done");

		Set<String> outputs = new HashSet<>();
		for (var entry : transactions.entrySet()) {
			var helper = entry.getValue();
			helper.collate();
			outputs.add(helper.store(entry.getKey()));
		}
		logger.info("Transactions collated and stored");


		for (var output : outputs) {
			final var files = Objects.requireNonNull(new File(output).listFiles());
			if (onlyOneFile(output, files)) {
				continue;
			}

			var destination = new File(out + File.separator + files[0].getName());
			if (Files.deleteIfExists(destination.toPath())) {
				logger.info("Destination file deleted");
			}

			FileUtils.moveFile(files[0], destination);
			FileUtils.deleteDirectory(new File(output));
		}

		logger.info("Collation done");
	}

	private boolean onlyOneFile(String output, File[] files) throws IOException {
		if (files.length <= 1) {
			return false;
		}
		var zippedOutput = zipFolder(output);
		if (!rootFolder.equals(out)) {
			var destination = new File(out, zippedOutput.getName());
			FileUtils.moveFile(zippedOutput, destination);
		}
		FileUtils.deleteDirectory(new File(output));
		return true;
	}

	private Map<String, List<String>> verifyTransactions() throws HederaClientException {
		Map<String, Set<String>> verifyWithFiles = new HashMap<>();

		Set<AccountId> requiredIds = new HashSet<>();
		Set<String> ids = new HashSet<>();


		for (var entry : transactions.entrySet()) {
			if (verifyWithFiles.containsKey(entry.getKey())) {
				ids = verifyWithFiles.get(entry.getKey());
			}

			var helper = entry.getValue();
			requiredIds.addAll(helper.getSigningAccounts());

			ids.addAll(getAccountIds(helper));
			ids.addAll(getPublicKeyNames(helper));

			List<String> sortedIDs = new ArrayList<>(ids);
			Collections.sort(sortedIDs);
			verifyWithFiles.put(FilenameUtils.getBaseName(helper.getTransactionFile()), ids);
		}

		for (AccountId requiredId : requiredIds) {
			final var requiredIdString = requiredId.toString();
			if (knownIds.contains(requiredId) && !ids.contains(requiredIdString)) {
				logger.info("Transactions have not been signed by required account {}", requiredIdString);
				return null;
			}
		}

		Map<String, List<String>> verifyTransactions = new HashMap<>();
		for (Map.Entry<String, Set<String>> entry : verifyWithFiles.entrySet()) {
			var key = entry.getKey();
			var value = entry.getValue();
			List<String> sortedIDs = new ArrayList<>(value);
			Collections.sort(sortedIDs);
			verifyTransactions.put(key, sortedIDs);
		}


		return verifyTransactions;
	}

	private List<String> getPublicKeyNames(CollatorHelper helper) {
		List<String> idList = new ArrayList<>();
		for (Map.Entry<File, PublicKey> keyEntry : publicKeys.entrySet()) {
			if (helper.verify(keyEntry.getValue())) {
				idList.add(FilenameUtils.getBaseName(keyEntry.getKey().getName()));
			}
		}
		return idList;
	}

	private List<String> getAccountIds(CollatorHelper helper) throws HederaClientException {
		List<String> idList = new ArrayList<>();
		for (var info : infos.entrySet()) {
			if (helper.verify(info.getValue())) {
				idList.add(info.getValue().accountId.toString());
			}
		}
		return idList;
	}

	private void loadTransactions(File root) throws HederaClientException {
		if (root.isFile()) {
			handle(root);
			return;
		}
		if (root.isDirectory()) {
			var files = root.listFiles();
			assert files != null;
			for (var file : files) {
				loadTransactions(file);
			}
		}
	}

	private void handle(File file) throws HederaClientException {
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

	private void handleFile(File file) throws HederaClientException {
		var key = buildBaseName(file);
		var helper = new CollatorHelper(file);
		if (transactions.containsKey(key)) {
			helper.addHelper(transactions.get(key));
		}
		transactions.put(key, helper);
	}

	private String buildBaseName(File file) {
		final var pathName = file.getAbsolutePath();
		final var baseName = FilenameUtils.getBaseName(pathName);

		var suffix0 = pathName.contains("signatures") ? "_signatures" : "";
		var suffix = pathName.contains("transactions") ? "_transactions" : suffix0;

		if (pathName.contains("Node")) {
			return pathName.substring(pathName.indexOf("Node"), pathName.indexOf(suffix)) + "_" + baseName;
		}

		return baseName;

	}

	private void handleZip(File file) throws HederaClientException {

		final var destination = file.getAbsolutePath().replace(".zip", "_unzipped");
		var unzipped = unZip(file.getAbsolutePath(), destination);
		loadTransactions(unzipped);
		if (!new File(destination).exists()) {
			throw new HederaClientException("Files were not unzipped");
		}
		unzips.add(new File(destination));
	}

	private boolean isZip(File file) {
		return file.getName().endsWith(ZIP_EXTENSION);
	}

	private boolean isTransaction(File file) {
		return file.getName().endsWith(Constants.TRANSACTION_EXTENSION);
	}

	private boolean isSigPair(File file) {
		return SIGNATURE_EXTENSION.equals(FilenameUtils.getExtension(file.getName()));
	}

	private boolean isComment(File file) {
		return file.getName().endsWith(Constants.TXT_EXTENSION);
	}

	private void parseFiles(File[] files, String extension) throws HederaClientException {
		for (var file : files) {
			if (!file.exists()) {
				throw new HederaClientException(String.format("The file %s does not exist", file.getName()));
			}
			if (file.isDirectory()) {
				var inner = file.listFiles((dir, name) -> name.endsWith(extension));
				assert inner != null;
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

			} catch (InvalidProtocolBufferException e) {
				logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, file.getName());
				throw new HederaClientException(e);
			}
		}
	}

}
