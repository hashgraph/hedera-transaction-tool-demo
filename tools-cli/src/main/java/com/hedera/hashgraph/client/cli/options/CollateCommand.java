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

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.helpers.CollatorHelper;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.PublicKey;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import picocli.CommandLine;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.FILE_NAME_GROUP_SEPARATOR;
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

	@CommandLine.Option(names = { "-p", "--prefix-label" }, description = "The label used as a prefix for the " +
			"name of the verification.csv file")
	private String prefix = "";

	// The key is the name of the original transaction file name + node being submitted to.
	// This is a best guess approach, for a quick fix, as the key is deduced based on the
	// zip's file name.
	private final Map<String, CollatorHelper> transactions = new HashMap<>();

	private final Map<PublicKey, String> publicKeys = new HashMap<>();
	private final List<File> unzips = new ArrayList<>();
	@Override
	public void execute() throws HederaClientException, IOException {
		if ("".equals(out)) {
			out = rootFolder;
		}

		final var root = new File(rootFolder);
		if (!root.exists()) {
			throw new HederaClientException("Cannot find the transactions root folder");
		}

		// Parse public key files from inputs
		loadVerificationFiles(Constants.PUB_EXTENSION, keyFiles);

		// Parse transactions
		loadTransactions(root);

		// Verify the transactions have the proper signatures,
		// collate the signature files,
		// and create the info for the verification.csv,
		final var verification = verifyTransactions();

		if (verification.isEmpty() || transactions.isEmpty()) {
			logger.error("All transactions failed verification. Terminating");
			cleanup();
			return;
		}

		// If the prefix option is used, add the separator.
		if (!"".equals(prefix)) {
			prefix = prefix + FILE_NAME_GROUP_SEPARATOR;
		}

		writeCSV(out + File.separator + prefix + "verification.csv", verification);
		logger.info("Verification done");

		final Set<String> outputs = new HashSet<>();
		// For each transaction found in the root folder
		for (final var entry : transactions.entrySet()) {
			// Get the helper that will perform the work
			final var helper = entry.getValue();
			// Group up the files in preparation to be moved
			outputs.add(helper.store(entry.getKey()));
		}

		logger.info("Transactions collated and stored");

		moveToOutput(outputs);

		cleanup();

		logger.info("Collation done");
	}

	private void cleanup() throws IOException {
		// Clean up all the unzipped directories
		for (final var unzip : unzips) {
			FileUtils.deleteDirectory(unzip);
		}
		FileUtils.deleteDirectory(new File("./Temp"));
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
		// For each output directory in outputs
		for (final var output : outputs) {
			// Output is a directory, return the list of files
			final var files = Objects.requireNonNull(new File(output).listFiles());
			// If more than one file, zip it up, move it to the destination, and continue
			// This occurs after all the files below are done.
			if (moreThanOneFile(output, files)) {
				continue;
			}

			// If only a single file, then rename as needed, and move to the new location
			final var filenamePrefix = (output.contains("Node")) ?
					output.substring(output.lastIndexOf(FILE_NAME_GROUP_SEPARATOR) + 1)
							+ FILE_NAME_GROUP_SEPARATOR : "";
			final var destination = new File(out + File.separator + filenamePrefix + files[0].getName());

			// Overwriting the file, if it exists
			Files.deleteIfExists(destination.toPath());
			FileUtils.moveFile(files[0], destination);
			// Delete the temporary output
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
	private void loadVerificationFiles(final String extension, final String... files) throws HederaClientException {
		if (files != null && files.length > 0) {
			final var infoArray = Arrays.stream(files)
					.filter(Objects::nonNull)
					.map(File::new)
					.toArray(File[]::new);
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
			Files.move(zippedOutput.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING);
		}
		FileUtils.deleteDirectory(new File(output));
		return true;
	}

	private List<List<String>> verifyTransactions() {
		// Create the map, the key being the transactionId, the list is all the fields for the verification of
		// that transaction.
		final Map<String, List<String>> verifyWithFiles = new HashMap<>();

		final var iterator = transactions.entrySet().iterator();
		while (iterator.hasNext()) {
			final var entry = iterator.next();
			var helper = entry.getValue();
			// Get the transactionId and use that as the key for the verification map
			var transactionId = helper.getBaseName();
			var fileName = helper.getTransactionFile();
			// Collate all signatures, this process will also verify the signatures,
			// ensuring the required signatures are present.
			try {
				helper.collate(infoFiles);
			} catch (HederaClientRuntimeException e) {
				// If collating failed, add an item to the verification list
				// Make sure this transaction isn't already in the map
				// (Multi-node submission will have duplicate transactionId entries)
				if (addItemListToVerification(verifyWithFiles, fileName, transactionId)) {
					logger.error("Collation and Verification of " + transactionId
							+ " failed due to the following error: "
							+ e.getMessage().replace("Hedera Client Runtime: ", ""));
				}
				iterator.remove();
				continue;
			}

			// Get the accounts associated with the transaction. This would include
			// the fee payer, and accounts to be updated, or accounts with balances changing
			// due to transfer, etc.
			final var accounts = helper.getSigningAccounts();
			final var requiredIdsInUse = accounts.stream()
					.filter(account -> !java.util.Arrays.stream(infoFiles)
							.filter(a -> CommonMethods.getInfoFiles(a, account).length > 0)
							.collect(Collectors.toList())
							.isEmpty())
					.map(AccountId::toString)
					.collect(Collectors.toList());

			// Get the list of public key names used to sign the transaction (if the key is a required key)
			var publicKeyNames = getPublicKeyNames(helper);
			// Sort the list of ids
			Collections.sort(requiredIdsInUse);
			// Sort the list of public keys
			Collections.sort(publicKeyNames);

			// Now put everything into the list for the csv
			// transactionFileName, transactionId, list of accounts (requiredIds), list of keys used (getPublicKeyNames)
			// Make sure this transaction isn't already in the map
			// (Multi-node submission will have duplicate transactionId entries)
			addItemListToVerification(verifyWithFiles, fileName, transactionId,
					"\"" + String.join(",", requiredIdsInUse) + "\"",
					"\"" + String.join(",", publicKeyNames) + "\"");
		}

		final var listOfVerifiedFiles =  new ArrayList<>(verifyWithFiles.values());
		Collections.sort(listOfVerifiedFiles, (list1, list2) -> {
			if (list1 == null || list1.isEmpty() || list1.get(0) == null) {
				return 1;
			} else if (list2 == null || list2.isEmpty() || list2.get(0) == null) {
				return -1;
			}
			return list1.get(0).compareTo(list2.get(0));
		});
		return listOfVerifiedFiles;
	}

	/**
	 * Return a list of key names for keys used to sign the transaction.
	 *
	 * @param helper
	 * @return
	 */
	private List<String> getPublicKeyNames(final CollatorHelper helper) {
		final List<String> idList = new ArrayList<>();
		for (final Map.Entry<PublicKey, String> keyEntry : helper.getPublicKeys().entrySet()) {
			if (helper.verify(keyEntry.getKey())) {
				var keyName = publicKeys.get(keyEntry.getKey());
				if (keyName != null) {
					idList.add(keyName);
				} else {
					idList.add("unknown-key");
				}
			}
		}
		return idList;
	}

	@NotNull
	private boolean addItemListToVerification(@NotNull Map<String, List<String>> map,
											  @NotNull String fileName,
											  @NotNull String transactionId,
											  @NotNull String... details) {
		if (!map.containsKey(transactionId)) {
			var verificationItemList = new ArrayList<String>();
			verificationItemList.add(fileName);
			verificationItemList.add(transactionId);
			verificationItemList.addAll(Arrays.stream(details).collect(Collectors.toList()));
			map.put(transactionId, verificationItemList);
			return true;
		}
		return false;
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
		// Create the key for the map. The key should be the
		// transaction file name + key name + node
		final var key = buildBaseName(file);
		final var helper = new CollatorHelper(file);

		if (transactions.containsKey(key)) {
			helper.addHelper(transactions.get(key));
		}
		transactions.put(key, helper);
	}

	private String buildBaseName(final File file) {
		var baseName = CollatorHelper.buildBaseName(file);
		final var pathName = file.getAbsolutePath();

		if (pathName.contains("Node")) {
			final var suffix0 = pathName.contains("signatures") ? "_signatures" : "";
			final var suffix = pathName.contains("transactions") ? "_transactions" : suffix0;

			// This won't affect the new naming convention, only the old version
			final var nodeNumber = pathName.substring(pathName.indexOf("Node")+5, pathName.indexOf(suffix));
			final var nodeName = "Node-" + nodeNumber.replace("-", ".");

			return nodeName + FILE_NAME_GROUP_SEPARATOR + baseName;
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
			if (Constants.PUB_EXTENSION.equals(extension)) {
				publicKeys.put(EncryptionUtils.publicKeyFromFile(file.getAbsolutePath()),
						FilenameUtils.getBaseName(file.getName()));
			} else {
				throw new HederaClientException("Not implemented");
			}
		}
	}

}
