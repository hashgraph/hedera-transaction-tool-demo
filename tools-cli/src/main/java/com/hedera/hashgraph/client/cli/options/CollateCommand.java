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

import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.helpers.CollatorHelper;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.PublicKey;
import io.grpc.internal.ClientStream;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import picocli.CommandLine;

import javax.swing.border.Border;
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

		// Parse account info files from inputs
		//TODO this only grabs stuff in the folders, but what if there is a key missing
		// from the folder, but exists in the transaction (like account update?)
		loadVerificationFiles(infoFiles, Constants.INFO_EXTENSION);

		// Parse public key files from inputs
		loadVerificationFiles(keyFiles, Constants.PUB_EXTENSION);

		// Parse transactions
		loadTransactions(root);

		// or something, and filename can have an _ in it
		final var verification = verifyTransactions();

		if (verification == null) {
			logger.info("Transactions not verified. Terminating");
			cleanup();
			return;
		}

		// If the prefix option is used, add the separator.
		if (!"".equals(prefix)) {
			prefix = prefix + ".";
		}

		writeCSV(out + File.separator + prefix + "verification.csv", verification);
		logger.info("Verification done");

		final Set<String> outputs = new HashSet<>();
		// For each transaction found in the root folder
		for (final var entry : transactions.entrySet()) {
			// Get the helper that will perform the work
			final var helper = entry.getValue();
			// Collate all signature key pairs
			//TODO this happens in the verify step
			helper.collate();
			//
			outputs.add(helper.store(entry.getKey()));
		}
		logger.info("Transactions collated and stored");

		moveToOutput(outputs);

		cleanup();

		logger.info("Collation done");
	}

	private final void cleanup() throws IOException {
		// Clean up all the unzipped directories
		for (final var unzip : unzips) {
			FileUtils.deleteDirectory(unzip);
		}
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
			final var filenamePrefix = (output.contains("Node")) ? output.substring(output.lastIndexOf("_") + 1) + "_" : "";
			final var destination = new File(out + File.separator + filenamePrefix + files[0].getName());

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
			Files.move(zippedOutput.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING);
		}
		FileUtils.deleteDirectory(new File(output));
		return true;
	}

	//transaction file nsme, transaction id, all account involved in signing (if they matter),
	// list of keys used in signing (if they matter) derive key nsmed from pub key file names
	// put the list in quotes ",,,,"

//	i need to get all accounts and keys that are needed, compare to the ones that exist,
//	if passes, create the csv, if not, throw an error
	private List<List<String>> verifyTransactions() throws HederaClientException {
		// Create the map, the key being the transactionId, the list is all of the fields for the vefification of
		// that transaction.
		final Map<String, List<String>> verifyWithFiles = new HashMap<>();

		for (final var entry : transactions.entrySet()) {
			var helper = entry.getValue();
			// Get the transactionId and use that as the key for the verification map
			var transactionId = helper.getBaseName();
			var fileName = helper.getTransactionFile();
			// Make sure this transaction isn't already in the map
			// (Multi-node submission will have duplicate transactionId entries)
			if (verifyWithFiles.containsKey(transactionId)) {
				continue;
			}

			// Get the accounts associated with the transaction. This would include
			// the fee payer, and accounts to be updated, or accounts with balances changing
			// due to transfer, etc.
			var requiredIds = helper.getSigningAccounts().stream()
					.map(accountId -> accountId.toString())
					.collect(Collectors.toList());
			// Get all accounts that have valid signatures
			// (exist in the info folder AND pass verification which includes threshold checks)
			var verifiedIds = getAccountIds(helper);

			// For every required account, ensure that it was verified
			for (var requiredId : requiredIds) {
				if (!verifiedIds.contains(requiredId)) {
					logger.info("Transaction ({}) has not been signed by required account {}",
							fileName, requiredId);
					return null;
				}
			}

			// Get the list of public key names used to sign the transaction (if the key is a required key)
			var publicKeyNames = getPublicKeyNames(helper);
			// Sort the list of ids
			Collections.sort(requiredIds);
			// Sort the list of public keys
			Collections.sort(publicKeyNames);

			// Now put everything into the list for the csv
			// transactionFileName, transactionId, list of accounts (requiredIds), list of keys used (getPublicKeyNames)
			var verificationItemList = new ArrayList<String>();
			verificationItemList.add(fileName);
			verificationItemList.add(transactionId);
			verificationItemList.add("\"" + String.join(",", requiredIds) + "\"");
			verificationItemList.add("\"" + String.join(",", publicKeyNames) + "\"");


			// Put the list of strings into the map
			verifyWithFiles.put(transactionId, verificationItemList);
//
//			helper.getsigningaccounts returns all accounts needed (be sure to include new keys)
//			helper.getsignaturepairs only returns sigs collected
//			getaccountid(helper) gets all accounts that have valid sigs (exist and pass threshold tests)
//			getpublickeynames(helper) returns names of all keys used to sign if they are needed to sign (be sure to include new keys)
//
//			make sure all accounts returned in getsigningaccounts exist in getaccountid list
//			if that is true, then it is verified, add to the list along with all publickeynames
//
//			check if the accountinfo file is present (knownids)? what good does that do? nothing,
//
//
//
//
//
//					the reason it was done all at once was because the requiredids could be the same ids fro multiple transactions
//					and the ids that passed (present and threshold) could be the same for multiple transactions
//					so unless I want to specify which trnsaction failed, group them up. but why woulnd't I want to say which transaction
//		failed? specify transactionfilename, or transactionid, or both probably just filename'
//
//
//
//
//
//
//
//
//
//
//			// This should get the accounts needed.
//			helper.getSigningAccounts();
//			// This should get the keys used to sign
//			this is the list of all signatures used, how is this different than publickeynames? its not, other than
//					publickeynames returns the key names
//					actually this gets all sigs, where getpublickeynames only gets key names of sigs that are required
//					so make sure that both getsigningaccounts and getpublickeynames will also return new keys in the case
//				of accountupdate
//			helper.getSignaturePairs();
//
//			gets all accounts that have valid signatures on the transaction, compare this to the list of
//			required accounts
//					this needs to also make sure accountupdate new key stuff is taken into account
//			getAccountIds(helper);
//			// Go through all the public keys in the supplied 'public keys' folder,
//			// add all of the keys that apply (with verify)
//			gets all keys that signed the transaction, compare this to the list of required signatures?
//			getPublicKeyNames(helper);
		}
		return new ArrayList<>(verifyWithFiles.values());
//
//
//
////		the idea is verifywithfiles will allow me to group stuff, i just need to be sure to filter out stuff by node
////		which means transactionid? or filename+id? transactionid, nothing shouldn't have more than one entry per transaction id
////	    but can i have multiple entries in transaction for the same id that need to be collated? or is that already done?'
////		by this point it should already be done
////		I either need a set of verification objects, or a map that I will convert to a list (map.values().aslist)
////		at the end
//		// TODO I don't think I fully understand what is going on here, what is verifyWithFiles?
//
//// TODO start in here, i need the transaction name/id thing (testCSV_3 copy 2) as first column
////not sure on the group account stuff yet
////also, remove all accounts not in requiredids from verification
//		final Set<AccountId> requiredAccountIds = new HashSet<>();
//		final Set<ByteString> requiredSigningKeys = new HashSet<>();
//		Set<String> ids = new HashSet<>();
//
////TODO
////		ok, this needs to go through all the transactions and have an entry for each:
////		transactionfilename, .txsig filename, list of accounts/keys (not ',' separated?)
////		I only want this for 1 node, meaning the same data will be present for each node
////				group by node (meaning I only need full data from 1 node, as all are the same?)
////		then, for each
////		in the case of an accountupdate, the new key structure can be different than the one in .info
////		and if new keys are added, they will be required to be signers
////		which means that they will get left off IF we don't check the transaction.keystructure (not sure how to do that yet)'
////		and if I can do that easily enough here, maybe I can do that in the home page stuff so the new user doesn't
////	    have to manually add their key'
////should just be in entry.getvalue().gettransaction().getKey()
//// TODO ids never gets reset? is that right? if verifyWtihFiles contains key, it gets the set, otherwise
//// it just uses the previous set? doesn't sound right. works if only 1 thing is involved though
//		//todo
////		should transactions have an entry for every transaction for every node? or just one for all ndes that gets
////				copied at the end
//		for (final var entry : transactions.entrySet()) {
//			//get the transactionId and use that as the key for the verify
//			var fileName = entry.getValue().getBaseName();
//			if (verifyWithFiles.containsKey(fileName)) {
//				continue;
//			}
//
//			final var helper = entry.getValue();
////			this is the secret, the account.info has the key structure, so I need the .info in order to
////					get keys, but in the case of an cryptoyupdatetransaction, I need to get the key list as well
//			requiredAccountIds.addAll(helper.getSigningAccounts());
////			which should be in here, in signingkeys, no this is the same kinda thing as signing accounts, I think
////			i think i need to specifically check for transaction instanceof accountupdate, then get key
////
////						but for both types, what about threshold?
////				does verification even bother with threshold? who does? other than the network
////
////						transaction.getsigningkeys for an update should be where the new keys are added to the list
//			requiredSigningKeys.addAll(helper.getTransaction().getSigningKeys());
//
//			// TODO this seems to only pull the keys that match, nothing else
//			// Go through all the accounts in the supplied 'info' folder,
//			// add all of the accounts that apply (with verify)
//			// TODO does verify get all the new keys not already in account.key? for an account update that is
//			ids.addAll(getAccountIds(helper));
//			// Go through all the public keys in the supplied 'public keys' folder,
//			// add all of the keys that apply (with verify)
//			ids.addAll(getPublicKeyNames(helper));
//
//			// TODO it gets sorted every time, and new stuff is added to this set for each different file
//			// the sorting seems unnecessary as it is done down below before the results are returned
//			// and I don't see it helping much in the check below
////			final List<String> sortedIDs = new ArrayList<>(ids);
////			Collections.sort(sortedIDs);
////			i think i want the helper.gettransactionfilename to be wihtout node, then add node as a separate field
////					so the fields would be
////					txfilename, node, .txsig name without node prefix, list of accounts/keys used to sign (change delimiter?)
////			except, I don't think they care about node, because it will look the same for all nodes, '
//			verifyWithFiles.put(fileName, new HashSet<>(ids));
//		}
//
////		it would seem to me that this should be done for each file, not afterwards for all of them, right?
////		or maybe as it is a group.... no it should be able to collate a bunch of stuff at the same time and this might be
////		one of the issues they were having.
//
////	TODO	try and find an example of a multi signing collate
//		for (final AccountId requiredId : requiredAccountIds) {
//			final var requiredIdString = requiredId.toString();
//			if (knownIds.contains(requiredId) && !ids.contains(requiredIdString)) {
//				logger.info("Transactions have not been signed by required account {}", requiredIdString);
//				return null;
//			}
//		}
//
//		final Map<String, List<String>> verifyTransactions = new HashMap<>();
//		for (final Map.Entry<String, Set<String>> entry : verifyWithFiles.entrySet()) {
//			final var key = entry.getKey();
//			final var value = entry.getValue();
//			final List<String> sortedIDs = new ArrayList<>(value);
//			Collections.sort(sortedIDs);
//			verifyTransactions.put(key, sortedIDs);
//		}
//
////		this should really be a key (transactionfilename), with a list of transactions, each having a list of keys
////				except, the transactionfilename will always be the same (unless lots of different transactions are all collated at the same time)
////		so, it's not a map so much as a list of verification items that need to be written'
////		return verifyTransactions;
//		return new ArrayList<>();
	}

	/**
	 * Return a list of key names for keys used to sign the transaction.
	 *
	 * @param helper
	 * @return
	 */
	private List<String> getPublicKeyNames(final CollatorHelper helper) {
		final List<String> idList = new ArrayList<>();
		for (final Map.Entry<File, PublicKey> keyEntry : publicKeys.entrySet()) {
			if (helper.verify(keyEntry.getValue())) {
				idList.add(FilenameUtils.getBaseName(keyEntry.getKey().getName()));
			}
		}
		return idList;
	}

	/**
	 * Return a list of accountIds that have signatures on the transaction.
	 *
	 * @param helper
	 * @return
	 * @throws HederaClientException
	 */
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
		final var pathName = file.getAbsolutePath();
		final var baseName = FilenameUtils.getBaseName(pathName);

		if (pathName.contains("Node")) {
			final var suffix0 = pathName.contains("signatures") ? "_signatures" : "";
			final var suffix = pathName.contains("transactions") ? "_transactions" : suffix0;

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
