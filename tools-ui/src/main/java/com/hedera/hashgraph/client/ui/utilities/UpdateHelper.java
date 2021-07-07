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

package com.hedera.hashgraph.client.ui.utilities;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountInfo;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.zeroturnaround.zip.ZipUtil;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.Constants.AES_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.ZIP_EXTENSION;
import static org.apache.commons.io.FileUtils.copyFile;
import static org.apache.commons.io.FileUtils.deleteDirectory;
import static org.apache.commons.io.FileUtils.moveFile;
import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getExtension;

public class UpdateHelper implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(UpdateHelper.class);

	private String toolFolder;
	private boolean updated;
	private boolean valid;

	public UpdateHelper(String... paths) {
		var folder = new StringBuilder();
		var separator = "";

		if (paths == null) {
			return;
		}

		for (var s : paths) {
			folder.append(separator);
			folder.append(s);
			separator = File.separator;
		}

		var toolDirectory = new File(folder.toString());
		if (!toolDirectory.exists() || !toolDirectory.isDirectory()) {
			this.toolFolder = null;
			this.valid = false;
			this.updated = false;
			return;
		}

		this.toolFolder = folder.toString();
		this.valid = true;
		this.updated = new File(this.toolFolder, "Files/.System").exists();
	}

	public boolean isUpdated() {
		return updated;
	}

	public boolean isValid() {
		return valid;
	}

	/**
	 * Translates the old style directory (v2.x) to the new one (v0.x). Creates the nicknames json file, stores files
	 * for each account containing info and json and zips the old folders into an Archive.zip
	 *
	 * @throws HederaClientException
	 * @throws IOException
	 */
	public void handleAccounts() throws HederaClientException, IOException {
		// if the folder is not valid, nothing to do. return
		if (!isValid()) {
			return;
		}
		// if the folder has been updated, nothing to do. return
		if (isUpdated()) {
			return;
		}
		// Create the system folder, if not yet created
		if (new File(toolFolder + "/Files/.System/").mkdirs()) {
			logger.info("System files folder created");
		}

		var accountsJson = new JsonObject();

		final var accountsDirectory = new File(toolFolder, "Accounts");
		var directories = accountsDirectory.listFiles(File::isDirectory);

		if (directories != null) {
			for (var directory : directories) {
				var object = handleDirectory(directory);
				if (object != null) {
					accountsJson.addProperty(directory.getName(), object.get(directory.getName()).getAsString());
				}
			}

			// Zip all old folders
			zipAllOldFolders(accountsDirectory, directories);

			writeJsonObject(toolFolder + "/Files/.System/accountMapFile.json", accountsJson);

			logger.info("Done updating accounts folder");
		}
	}

	private void zipAllOldFolders(File accountsDirectory, File[] directories) throws IOException {
		for (var directory : directories) {
			zipDir(directory.toPath());
			deleteDirectory(directory);
		}
		var toPack =
				accountsDirectory.listFiles((dir, name) -> ZIP_EXTENSION.equals(getExtension(name)));

		assert toPack != null;
		ZipUtil.packEntries(toPack, new File(accountsDirectory.getAbsolutePath(), "Archive.zip"));
		for (var file : toPack) {
			Files.delete(file.toPath());
		}
	}

	/**
	 * Moves the recovery phrase to the new location in Files/.System
	 *
	 * @throws IOException
	 */
	public void handleKeys() throws IOException {
		// if the folder is not valid, nothing to do. return
		if (!isValid()) {
			return;
		}
		// if the folder has been updated, nothing to do. return
		if (isUpdated()) {
			return;
		}
		// Create the system folder, if not yet created
		if (new File(toolFolder + "/Files/.System/").mkdirs()) {
			logger.info("System files folder created");
		}

		var mnemonicFiles = new File(toolFolder + "/Keys").listFiles(
				(dir, name) -> AES_EXTENSION.equals(getExtension(name)));

		if (mnemonicFiles == null || mnemonicFiles.length != 1) {
			return;
		}

		moveFile(mnemonicFiles[0], new File(toolFolder, MNEMONIC_PATH));


	}

	/**
	 * Removes all transaction files that cannot be read from the History and moves them to a zipped archive in the
	 * Files folder.
	 *
	 * @throws HederaClientException
	 * @throws IOException
	 */
	public void handleHistory() throws HederaClientException, IOException {
		// if the folder is not valid, nothing to do. return
		if (!isValid()) {
			return;
		}
		// if the folder has been updated, nothing to do. return
		if (isUpdated()) {
			return;
		}
		final var historyFolder = new File(toolFolder, "History");
		if (historyFolder.mkdirs()) {
			logger.info("History folder created");
		}
		var transactions = historyFolder.listFiles(
				(dir, name) -> TRANSACTION_EXTENSION.equals(getExtension(name)));

		if (transactions == null) {
			throw new HederaClientException("Cannot load transaction files");
		}


		for (var transaction : transactions) {
			try {
				var toolTransaction = new ToolTransaction(transaction);
				final var transactionId = toolTransaction.getTransactionId();
				logger.info("Transaction {} loaded", transactionId);
			} catch (Exception e) {
				if (e.getMessage().contains("parsed transaction body has no data")) {
					moveFileAndAssociates(transaction);
				}
			}
		}

		var archive = new File(toolFolder, "Files/History_Archive");
		if (archive.mkdirs()) {
			logger.info("Archive folder created");
		}
		if (archive.exists()) {
			zipDir(archive.toPath());
			FileUtils.deleteDirectory(archive);
		}
	}

	public void handleMigration() throws HederaClientException, IOException {


		logger.info("Handling accounts");
		handleAccounts();

		logger.info("Handling keys");
		handleKeys();

		logger.info("Handling history");
		handleHistory();

	}


	/**
	 * Given a file in the history folder, move it to the archive, as well as any other files associated with it.
	 *
	 * @param transaction
	 * @throws IOException
	 */
	private void moveFileAndAssociates(File transaction) throws IOException {
		final var historyFolder = new File(toolFolder, "History");
		final var archive = new File(toolFolder, "Files/History_Archive");
		if (archive.mkdirs()) {
			logger.info("Archive folder created");
		}

		moveFile(transaction, new File(archive.getAbsolutePath(), transaction.getName()));

		var otherFiles = historyFolder.listFiles(
				(dir, name) -> Objects.equals(getBaseName(transaction.getName()), getBaseName(name)));

		if (otherFiles == null) {
			return;
		}

		for (var otherFile : otherFiles) {
			moveFile(otherFile, new File(archive.getAbsolutePath(), otherFile.getName()));
		}
	}

	/**
	 * Handles a single account directory. Reads the account info and nickname, stores the info as a binary and as a
	 * json file in the correct folder and returns an object that can be added to the nicknames json
	 *
	 * @param directory
	 * 		the account directory
	 * @return a json object that contains the account number and nickname.
	 * @throws HederaClientException
	 * @throws IOException
	 */
	private JsonObject handleDirectory(File directory) throws HederaClientException, IOException {
		var object = new JsonObject();
		if (directory.getName().contains("KeysOnly")) {
			return null;
		}
		// Deal with the info files and nicknames
		var dir = directory.getAbsolutePath();
		var account = readJsonObject(new File(dir, "AccountInfo/account.json"));
		var accountInfo = AccountInfo.fromBytes(readBytes(new File(dir, "AccountInfo/accountInfo.info")));
		var accountId = new Identifier(accountInfo.accountId).toReadableString();
		if (!account.get("accountID").getAsString().equals(accountId)) {
			logger.error("Unrecognized account in folder {}", dir);
			return null;
		}

		object.addProperty(accountId, account.get("nickname").getAsString());

		var infoJson = EncryptionUtils.info2Json(accountInfo);
		final var fileName = toolFolder + File.separator + "Accounts" + File.separator + accountId + ".";

		writeBytes(fileName + INFO_EXTENSION, accountInfo.toBytes());
		writeJsonObject(fileName + JSON_EXTENSION, infoJson);


		// If there are any keys associated with the accounts (V1.0 of the tool compatibility)
		var keys = new File(directory.getAbsolutePath() + "/AccountKeys/Keys").listFiles(
				(dir1, name) -> !name.startsWith("."));
		var stores = new File(directory.getAbsolutePath() + "/AccountKeys/KeyStores").listFiles(
				(dir1, name) -> !name.startsWith("."));

		if (keys != null) {
			// attempt to move the key to the keys folder
			for (var key : keys) {
				copyFile(key, new File(getIncrementedName(toolFolder + "/Keys/" + key.getName())));
			}
		}

		if (stores != null) {
			for (var key : stores) {
				copyFile(key, new File(getIncrementedName(toolFolder + "/Keys/" + key.getName())));
			}
		}
		return object;
	}

	/**
	 * Given a file path, it checks if a file of the same name exists. If it does returns a new path with a subscript
	 *
	 * @param path
	 * 		the path to a file
	 * @return the path to a file with no repeats
	 */
	private String getIncrementedName(String path) {
		var counter = 0;
		var candidate = path;
		var parent = new File(path).getParent();
		var name = getBaseName(path);
		var extension = getExtension(path);

		while (Files.exists(Paths.get(candidate))) {
			candidate = String.format("%s/%s_%d.%s", parent, name, ++counter, extension);
		}
		return candidate;
	}
}
