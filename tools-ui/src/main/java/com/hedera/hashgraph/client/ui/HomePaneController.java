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

package com.hedera.hashgraph.client.ui;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.Expiry;
import com.github.benmanes.caffeine.cache.Scheduler;
import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.fileservices.FileAdapterFactory;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.BatchFile;
import com.hedera.hashgraph.client.core.remote.BundleFile;
import com.hedera.hashgraph.client.core.remote.InfoFile;
import com.hedera.hashgraph.client.core.remote.PublicKeyFile;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.RemoteFilesMap;
import com.hedera.hashgraph.client.core.remote.SoftwareUpdateFile;
import com.hedera.hashgraph.client.core.remote.TransactionFile;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.UserComments;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.ui.popups.ExtraKeysSelectorPopup;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.sdk.Key;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextArea;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyPair;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_INTERNAL_FILES;
import static com.hedera.hashgraph.client.core.constants.Constants.INPUT_FILES;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_COLUMNS;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.OUTPUT_FILES;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.enums.Actions.ACCEPT;
import static com.hedera.hashgraph.client.core.enums.Actions.DECLINE;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.verifySignature;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.checkBoxListener;


@SuppressWarnings({ "ResultOfMethodCallIgnored" })
public class HomePaneController implements SubController {

	private static final Logger logger = LogManager.getLogger(HomePaneController.class);
	private static final double VBOX_SPACING = 20;

	// region FXML

	public VBox defaultViewVBox;
	public VBox newFilesViewVBox;
	public ScrollPane homeFilesScrollPane;
	// A Bi-directional Map. Sorting is done by comparing the RemoteFiles behind the VBox (which is actually what
	// gets sorted). Removing items from this map, however, is done by providing the RemoteFile.
	// Thus, the relationship is two-way.
	private final BiMap<RemoteFile, VBox> fileBoxes = HashBiMap.create();

	@FXML
	private Controller controller;
	// endregion

	// The cache tracking the expiration of the transactions.
	// Upon expiry, the files will no longer be displayed in this pane.
	private Cache<String, RemoteFile> remoteFilesCache;
	private String output = "";
	private String user = "";

	private final Label noTasksLabel = new Label("No new tasks.");

	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	@Override
	public void initializePane() {
		// Configure the components
		newFilesViewVBox.prefWidthProperty().bind(homeFilesScrollPane.widthProperty());
		newFilesViewVBox.setSpacing(VBOX_SPACING);
		noTasksLabel.setPadding(new Insets(15, 0, 5, 15));
		// Recreate the cache, this also only needs to be done once
		remoteFilesCache = Caffeine.newBuilder()
				.scheduler(Scheduler.systemScheduler())
				.expireAfter(new Expiry<String, RemoteFile>() {
					@Override
					public long expireAfterCreate(
							String key, RemoteFile value, long currentTime) {
						// Get the expiration of the RemoteFile
						final var expiration = value.getExpiration();
						// If it is 'MAX', it should never expire, return the non-expiring
						// value of Long.MAX_VALUE
						if (expiration.equals(Timestamp.MAX)) {
							return Long.MAX_VALUE;
						}
						// Get the duration until the file expires
						final var diff = Duration.between(Instant.now(), value.getExpiration().asInstant());
						// This should never happen, but if it does, return 0 to immediately expire
						if (diff.isNegative()) {
							return 0;
						}
						logger.info("{} will expire in: {} seconds", value.getName(), diff.toSeconds());
						// Return the time until expires as nanos
						return diff.toNanos();
					}
					@Override
					public long expireAfterUpdate(
							String key, RemoteFile value, long currentTime, long currentDuration) {
						// return currentDuration to signify to not modify the expiration time
						return currentDuration;
					}
					@Override
					public long expireAfterRead(
							String key, RemoteFile value, long currentTime, long currentDuration) {
						// return currentDuration to signify to not modify the expiration time
						return currentDuration;
					}
				})
				.removalListener((key, value, cause) -> {
					// If the cause indicates this RemoteFile was evicted due to expiry
					if (cause.wasEvicted()) {
						Platform.runLater(() -> {
							// Move the file to history
							try {
								value.moveToHistory(Actions.EXPIRE, "", "");
								controller.historyPaneController.addToHistory(value);
							} catch (final HederaClientException e) {
								logger.error("moving expired transaction failed", e);
								controller.displaySystemMessage(e.getMessage());
							}
							removeFile(value);
						});
					}
				})
				.build();

		newFilesViewVBox.setVisible(true);
		newFilesViewVBox.setDisable(false);

		populatePane();
	}

	public void populatePane() {
		try {
			// Clear the list of RemoteFiles to be displayed
			newFilesViewVBox.getChildren().clear();
			// Invalidate and clean up the cache
			remoteFilesCache.invalidateAll();
			remoteFilesCache.cleanUp();
			// Remove all files from the fileBoxes
			fileBoxes.clear();

			// Load the RemoteFiles from disk
			loadRemoteFilesMap();

			// Remove any RemoteFiles that have been invalidated
			remoteFilesCache.cleanUp();
			if (remoteFilesCache.estimatedSize() > 0) {
				// Show new transactions by hiding the defaultViewBox
				defaultViewVBox.setDisable(true);
				defaultViewVBox.setVisible(false);
			} else {
				defaultViewVBox.setDisable(false);
				defaultViewVBox.setVisible(true);
				newFilesViewVBox.getChildren().add(noTasksLabel);
			}
		} catch (final HederaClientException e) {
			logger.error("initializeHomePane error", e);
			controller.displaySystemMessage(e.getCause().toString());
		}
		logger.info("Home pane initialized");
	}

	/**
	 * Load all remote files into the map
	 */
	private void loadRemoteFilesMap() throws HederaClientException {
		if (controller.getOneDriveCredentials() != null && !controller.getOneDriveCredentials().isEmpty()) {
			// Load remote files
			final var emailMap = controller.getOneDriveCredentials();
			final var keyMap = emailMap.keySet();
			final List<String> inputFolder = new ArrayList<>(keyMap);
			inputFolder.add("USB");
			ensureInternalInputExists();
			inputFolder.add(DEFAULT_INTERNAL_FILES);
			loadInputFolderIntoMap(inputFolder);
			sortFiles();
		}
		logger.debug("Done loading remote files");
	}

	/**
	 * Load the not expired files from input from the input folder into the Remote Files Map
	 *
	 * @param inputFolder
	 * 		the folder where transactions are stored
	 */
	private void loadInputFolderIntoMap(final List<String> inputFolder) throws HederaClientException {
		final var version = controller.getVersion();
		for (final var s : inputFolder) {
			final var fileService = FileAdapterFactory.getAdapter(s);
			if (fileService != null && fileService.exists()) {
				final var remoteFilesMap = new RemoteFilesMap(version).fromFile(fileService);

				for (final var file : remoteFilesMap.getFiles()) {
					if (shouldCreateBox(file)) {
						// Do an extra check if the file is history (as the RemoteFile.isHistory isn't saved to disk
						// and therefore can be false here). If it is history, set it on the RemoteFile, otherwise
						// finish building the box.
						if (controller.historyPaneController.isHistory(file.hashCode())) {
							file.setHistory(true);
						} else {
							if (file instanceof TransactionFile) {
								setupKeyTree((TransactionFile) file);
							}
							buildAndAddBox(file);
						}
					}
				}
			}
		}
	}

	private boolean shouldCreateBox(RemoteFile file) {
		// if software update, update.getTimestamp() != null was checked previously,
		// when would it be null? and does that matter?
		return file.isValid() && !file.isExpired() &&
				!file.getType().equals(FileType.METADATA) &&
				!file.getType().equals(FileType.COMMENT) &&
				!file.getType().equals(FileType.TRANSACTION_CREATION_METADATA) &&
				!file.isHistory();
	}

	public void addFile(final String filePath) {
		try {
			// Create a RemoteFile by filePath and add it to the list
			addFile(new RemoteFile().getSingleRemoteFile(FileDetails.parse(new File(filePath))));
		} catch (HederaClientException e) {
			logger.error("adding file error: ", e);
			controller.displaySystemMessage(e.getMessage());
		}
	}

	public void addFile(final String filePath, final boolean history) {
		try {
			// Create the RemoteFile
			final var remoteFile = new RemoteFile().getSingleRemoteFile(
					FileDetails.parse(new File(filePath)));
			// Set its history property - even those RemoteFile.history never gets saved, currently.
			remoteFile.setHistory(history);
			// Add it to the list
			addFile(remoteFile);
		} catch (HederaClientException e) {
			logger.error("adding file error: ", e);
			controller.displaySystemMessage(e.getMessage());
		}
	}

	public void addFile(final RemoteFile file) {
		try {
			// Check if the box should be created
			if (shouldCreateBox(file)) {
				// If so, build the box and add it to the list of files
				buildAndAddBox(file);
				// Sort the list of files
				sortFiles();
			}
		} catch (HederaClientException e) {
			logger.error("adding file error: ", e);
			controller.displaySystemMessage(e.getCause().toString());
		}
	}

	private void sortFiles() {
		FXCollections.sort(newFilesViewVBox.getChildren(), (t1,t2) -> {
			final var file1 = fileBoxes.inverse().get(t1);
			final var file2 = fileBoxes.inverse().get(t2);
			// Should never be null
			if (file1 == null || file1.getExpiration() == null) {
				return -1;
			} else if (file2 == null) {
				return 1;
			}
			// Always keep the software updates on top
			if (file1 instanceof SoftwareUpdateFile) {
				return -1;
			} else if (file2 instanceof SoftwareUpdateFile) {
				return 1;
			}
			// Sort the files, soonest to expire on top
			return file1.getExpiration().compareTo(file2.getExpiration());
		});

	}

	public void removeFile(final String filePath) {
		// The file (on disk) has already been moved, but the RemoteFile object still exists
		final var remoteFile = remoteFilesCache.getIfPresent(filePath);
		if (remoteFile != null) {
			// Remove it from the list
			removeFile(remoteFile);
		}
	}

	private void removeFile(@NotNull final RemoteFile file) {
		// Remove the VBox from the map of fileBoxes
		final var box = fileBoxes.remove(file);
		// Invalidate the RemoteFile from the cache, to allow it to be removed
		remoteFilesCache.invalidate(file.getPath());
		// If a box was found
		if (box != null) {
			// Request focus on the newFilesViewBox to prevent scrolling to the top
			newFilesViewVBox.requestFocus();
			// Remove the box for the RemoteFile from the newFilesViewBox
			newFilesViewVBox.getChildren().remove(box);
			// If the newFilesViewBox is now empty, add the noTasksLabel
			if (newFilesViewVBox.getChildren().isEmpty()) {
				newFilesViewVBox.getChildren().add(noTasksLabel);
			}
		}
	}

	private void buildAndAddBox(final RemoteFile file) throws HederaClientException {
		// First check if the fileBox exists already for the file.
		// If it does contain the fileBox, check the actions of the file.
		// If the action count is not 4, just re-add the file to the remoteFiles.
		// Otherwise, recreate the fileBox and then add it.
		if (fileBoxes.containsKey(file)) {
			final var actions = file.getActions();
			switch (actions.size()) {
			case 0,1,2:
					remoteFilesCache.put(file.getPath(), file);
					return;
				case 4:
					break;
				default:
					logger.error("Unexpected value for number of actions");
					return;
			}

			return;
		}

		// Build the details box for the RemoteFile
		final var fileBox = file.buildDetailsBox();
		// Should throw an error if the fileBox is null
		if (fileBox == null) return;
		// Build the buttons box for the RemoteFile
		final var buttonsBox = getButtonsBox(file);
		if (buttonsBox != null) {
			// Add it to the fileBox
			fileBox.getChildren().add(buttonsBox);
		}

		// If anything was added to the fileBox, display it
		if (!fileBox.getChildren().isEmpty()) {
			addFileBox(file, fileBox);
		}
	}

	private void addFileBox(final RemoteFile file, final VBox fileBox) {
		final var children = newFilesViewVBox.getChildren();
		children.remove(noTasksLabel);
		// Keep SoftwareUpdateFiles on top, this needs to be done as SoftwareUpdateFiles are
		// added, in order to ensure only the newest SoftwareUpdateFiles are kept in the list
		if (file instanceof SoftwareUpdateFile) {
			if (!fileBoxes.isEmpty() && !children.isEmpty() &&
					fileBoxes.inverse().get(children.get(0)) instanceof SoftwareUpdateFile) {
				final var currentUpdate = fileBoxes.inverse().get(children.get(0));
				// The first file is a SoftwareUpdateFile, compare it to the new file, keep the most up-to-date
				// SoftwareUpdateFile
				if (file.compareTo(currentUpdate) > 0) {
					children.set(0, fileBox);
				}
			} else {
				// Otherwise just add the SoftwareUpdateFile to the beginning of the list
				children.add(0, fileBox);
			}
		} else {
			// Add the fileBox to the newFilesViewVBox
			children.add(fileBox);
		}
		// Add the file to the cache and map
		fileBoxes.put(file, fileBox);
		remoteFilesCache.put(file.getPath(), file);
	}

	private void setupKeyTree(final TransactionFile rf) {
		final var transactionType = rf.getTransaction().getTransactionType();
		if (transactionType == null) {
			// old style transaction
			return;
		}

		JsonObject oldInfo = null;
		JsonObject oldKey = null;
		controller.loadPubKeys();
		Key key = null;
		if (transactionType.equals(TransactionType.CRYPTO_CREATE)) {
			key = ((ToolCryptoCreateTransaction) rf.getTransaction()).getKey();
		}
		if (transactionType.equals(TransactionType.CRYPTO_UPDATE)) {
			final var transaction = (ToolCryptoUpdateTransaction) rf.getTransaction();
			oldInfo = getOldInfo(transaction.getAccount());
			if (oldInfo != null) {
				oldKey = oldInfo.get("key").getAsJsonObject();
			}
			key = transaction.getKey();
		}
		if (key != null) {
			rf.setTreeView(controller.buildKeyTreeView(key));
		}
		if (oldKey != null) {
			rf.setOldInfo(oldInfo);
			rf.setOldKey(controller.buildKeyTreeView(oldKey));
		}
	}

	private JsonObject getOldInfo(final Identifier account) {
		final File[] accounts = getFiles(account);
		if (accounts.length != 1) {
			logger.error("Cannot determine old account");
			return null;
		}

		try {
			return readJsonObject(accounts[0]);
		} catch (final HederaClientException e) {
			logger.error("readJsonObject failed", e);
			return null;
		}

	}

	@Nullable
	private File[] getFiles(final Identifier account) {
		return new File(DEFAULT_ACCOUNTS).listFiles((dir, name) -> {
			final var stringAccount = account.toReadableString();
			return JSON_EXTENSION.equals(FilenameUtils.getExtension(name)) && (name.contains(
					stringAccount + ".") || name.contains(stringAccount + "-"));
		});
	}

	private VBox getButtonsBox(final RemoteFile rf) {
		final var buttonsBox = new VBox();
		buttonsBox.setAlignment(Pos.CENTER_RIGHT);

		final var actions = rf.getActions();

		switch (actions.size()) {
			case 0:
				break;
			case 1:
				if (rf.isHistory()) {
					return buttonsBox;
				}
				final var updateButton = buildUpdateButton(rf);
				final var updateButtonBar = new ButtonBar();
				updateButtonBar.getButtons().addAll(updateButton);
				updateButtonBar.setButtonMinWidth(150);
				buttonsBox.getChildren().add(updateButton);
				break;
			case 2:
				if (rf.isHistory()) {
					if (!rf.accepted()) {
						final var undoButton = buildUndoButton(rf);
						final var historyPublicKeyButtonBar = new ButtonBar();
						historyPublicKeyButtonBar.getButtons().addAll(undoButton);
						historyPublicKeyButtonBar.setButtonMinWidth(250);
						buttonsBox.getChildren().add(historyPublicKeyButtonBar);
					}
					break;
				}
				final var acceptButton = buildAcceptButton(rf);
				final var declineButton = buildDeclineButton(rf);
				final var publicKeyButtonBar = new ButtonBar();
				publicKeyButtonBar.getButtons().addAll(acceptButton, declineButton);
				publicKeyButtonBar.setButtonMinWidth(150);
				buttonsBox.getChildren().add(publicKeyButtonBar);
				break;
			case 4:
				if (rf.isHistory() && !rf.isExpired()) {
					final var undoButton = buildUndoButton(rf);
					final var historyPublicKeyButtonBar = new ButtonBar();
					historyPublicKeyButtonBar.getButtons().addAll(undoButton);
					historyPublicKeyButtonBar.setButtonMinWidth(250);
					buttonsBox.getChildren().add(historyPublicKeyButtonBar);
					break;
				}
				final var extraSignersGridPane = formatExtraSignersGridPane(KEYS_COLUMNS);
				final var signingBar = new ButtonBar();
				signingBar.setButtonMinWidth(150);
				signingBar.getButtons().addAll(buildSignButton(rf), buildDeclineButton(rf));

				if (rf.hasHistory()) {
					signingBar.getButtons().add(buildCancelButton(rf));
				}

				final var addMoreKeysBar = new ButtonBar();
				addMoreKeysBar.setButtonMinWidth(150);
				addMoreKeysBar.getButtons().addAll(buildAddMoreButton(rf, extraSignersGridPane),
						buildBrowseButton(rf, extraSignersGridPane));
				buttonsBox.getChildren().add(
						formatRequiredSignersBox(rf, signingBar, addMoreKeysBar));

				final var extraSignersVBox = formatExtraSignersVBox(extraSignersGridPane);
				buttonsBox.getChildren().add(extraSignersVBox);
				break;
			default:
				logger.error("Unexpected value for number of actions");
				return null;

		}

		return buttonsBox;
	}

	private Button buildSignButton(final RemoteFile rf) {
		final var signButton = buildBlueButton("SIGN\u2026");
		signButton.setOnAction(actionEvent -> {
			final List<File> signers = new ArrayList<>(rf.getSignerSet());
			if (!signers.isEmpty()) {
				try {
					sign(rf);
				} catch (final HederaClientException exception) {
					logger.error("sign failed", exception);
				}
			} else {
				PopupMessage.display("Missing key", "Please select a key to sign the transaction");
			}
		});
		return signButton;
	}

	private Button buildAddMoreButton(final RemoteFile rf, final GridPane extraSignersGridPane) {
		final var addMoreButton = buildWhiteButton("ADD MORE");
		addMoreButton.setOnAction(actionEvent -> {
			rf.addExtraSigners(ExtraKeysSelectorPopup.display(rf.getSignerSet()));
			fillKeysGridPane(extraSignersGridPane, new ArrayList<>(rf.getExtraSigners()), rf.getSignerSet());
		});
		return addMoreButton;
	}

	private Button buildBrowseButton(final RemoteFile rf, final GridPane extraSignersGridPane) {
		final var browseButton = buildWhiteButton("BROWSE");
		browseButton.setOnAction(actionEvent -> {
			rf.addExtraSigners(loadKeyFiles());
			fillKeysGridPane(extraSignersGridPane, new ArrayList<>(rf.getExtraSigners()), rf.getSignerSet());
		});
		return browseButton;
	}

	private Button buildAcceptButton(final RemoteFile rf) {
		final var acceptButton = buildBlueButton("ACCEPT");
		acceptButton.setOnAction(event -> {
			try {
				switch (rf.getType()) {
					case PUBLIC_KEY:
						final var keysFile = new File(KEYS_FOLDER + rf.getName());
						Files.deleteIfExists(keysFile.toPath());
						FileUtils.copyFile(new File(rf.getPath()), keysFile);
						break;
					case ACCOUNT_INFO:
						final var importCount = controller.accountsPaneController.importInfoFiles(
								Collections.singletonList(new File(rf.getPath())));
						if (importCount <= 0) {
							return;
						}
						break;
					case BUNDLE:
						for (final Map.Entry<BundleFile.InfoKey, File> entry :
								((BundleFile) rf).getAccountInfoMap().entrySet()) {
							controller.accountsPaneController.importFromFile(entry.getValue(),
									entry.getKey().getNickname());
						}
						for (final Map.Entry<BundleFile.PubInfoKey, File> entry :
								((BundleFile) rf).getPublicKeyMap().entrySet()) {
							final var oldLocation =
									new File(KEYS_FOLDER, entry.getKey().getOldNickname() + "." + PUB_EXTENSION);
							Files.deleteIfExists(oldLocation.toPath());
							final var destination =
									new File(KEYS_FOLDER, entry.getKey().getNickname() + "." + PUB_EXTENSION);
							FileUtils.copyFile(entry.getValue(), destination);
						}
						break;
					default:
						logger.info("No action taken: {}", rf.getType());
				}
				exportComments(rf, rf.getCommentArea(), rf.getName());
				rf.moveToHistory(ACCEPT, rf.getCommentArea().getText(), "");
				controller.historyPaneController.addToHistory(rf);
				controller.loadPubKeys();
				controller.accountsPaneController.initializePane();
				controller.keysPaneController.initializePane();
				removeFile(rf);
			} catch (final IOException | HederaClientException e) {
				logger.error("buildAcceptButton failed", e);
				controller.displaySystemMessage(e.getMessage());
			}
		});
		return acceptButton;
	}

	private Button buildDeclineButton(final RemoteFile rf) {
		final var declineButton = buildWhiteButton("DECLINE");
		declineButton.setOnAction(event -> {
			try {
				exportComments(rf, rf.getCommentArea(), rf.getName());
				rf.moveToHistory(DECLINE, rf.getCommentArea().getText(), "");
				controller.historyPaneController.addToHistory(rf);
			} catch (final HederaClientException e) {
				logger.error("buildDeclineButton failed", e);
				controller.displaySystemMessage(e.getMessage());
			}
			removeFile(rf);
		});
		return declineButton;
	}

	private Button buildCancelButton(final RemoteFile rf) {
		final var cancelButton = buildWhiteButton("CANCEL");
		cancelButton.setOnAction(event -> {
			try {
				rf.moveToHistory();
				controller.historyPaneController.addToHistory(rf);
			} catch (final HederaClientException e) {
				logger.error("buildCancelButton failed", e);
				controller.displaySystemMessage(e.getMessage());
			}
			removeFile(rf);
		});
		return cancelButton;
	}

	private Button buildUndoButton(final RemoteFile rf) {
		final var legend =
				(rf instanceof PublicKeyFile || rf instanceof InfoFile || rf instanceof BundleFile) ?
						"UNDO" :
						"ADD MORE SIGNATURES";
		final var undoButton = buildBlueButton(legend);
		undoButton.setPrefWidth(Region.USE_COMPUTED_SIZE);
		undoButton.setMinWidth(250);
		undoButton.setOnAction(actionEvent -> {
			try {
				rf.moveFromHistory();
				controller.historyPaneController.removeFromHistory(rf);
				addFile(rf);
			} catch (final HederaClientException e) {
				logger.error("buildUndoButton failed", e);
				controller.displaySystemMessage(e.getCause().toString());
			}
		});
		return undoButton;
	}

	private Button buildUpdateButton(final RemoteFile rf) {
		final var button = buildBlueButton("UPDATE");
		button.setOnAction(actionEvent -> {
			var answer = false;
			if (!verifySignature(rf.getPath())) {
				answer = PopupMessage.display("Unverified Update",
						"The update package cannot be verified if you want to continue the update press the CONTINUE " +
								"button, otherwise CANCEL",
						true, "CANCEL", "CONTINUE");
			}
			if (answer) {
				return;
			}
			try {
				rf.moveToHistory(ACCEPT, ((SoftwareUpdateFile) rf).getDigest(), "");
				controller.historyPaneController.addToHistory(rf);
			} catch (final HederaClientException e) {
				logger.error("buildUpdateButton failed", e);
			}

			runUpdate(rf.getPath());
		});
		return button;
	}

	private void sign(final RemoteFile rf) throws HederaClientException {
		final List<Pair<String, KeyPair>> pairs = new ArrayList<>();
		final List<File> signers = new ArrayList<>(rf.getSignerSet());
		Collections.sort(signers);
		for (final var signer : signers) {
			pairs.add(controller.getAccountKeyPair(signer));
		}

		output = rf.getParentPath().replace(INPUT_FILES, OUTPUT_FILES);
		user = controller.getEmailFromMap(new File(rf.getParentPath()).getParent());
		for (final var pair : pairs) {
			try {
				signTransactionAndComment(rf, pair);
			} catch (final Exception exception) {
				logger.error("Transaction {} could not be signed with key {}.", rf.getName(),
						FilenameUtils.getBaseName(pair.getLeft()));
				logger.error("signTransactionAndComment failed", exception);
			}
		}
	}

	private void runUpdate(final String localLocation) {
		try {
			final var processBuilder = new ProcessBuilder("/usr/bin/open", localLocation);
			final var process = processBuilder.start();
			final var exitCode = process.waitFor();

			if (exitCode == 0) {
				System.exit(0);
			} else {
				logger.error("The update finished with exit code {}", exitCode);
				PopupMessage.display("Error opening update file",
						"The software update file cannot be opened.\nPlease contact the administrator.", "CLOSE");
			}
		} catch (final IOException e) {
			logger.error("runUpdate failed", e);
			PopupMessage.display("Error opening update file",
					"The software update file cannot be opened.\nPlease contact the administrator.", "CLOSE");
		} catch (final InterruptedException e) {
			logger.error("Interrupted exception", e);
			// Restore interrupted state
			Thread.currentThread().interrupt();
		}

	}

	private HBox formatRequiredSignersBox(final RemoteFile rf, final ButtonBar buttonBar, final ButtonBar extraBar) {
		final var signingKeys = new HBox();
		signingKeys.setAlignment(Pos.TOP_RIGHT);
		signingKeys.setSpacing(10);
		signingKeys.managedProperty().bind(signingKeys.visibleProperty());
		signingKeys.setVisible(!rf.getSigningPublicKeys().isEmpty() && !rf.isHistory());

		final var signerLabel = new Label("Keys: ");
		signerLabel.setPadding(new Insets(2));
		signingKeys.getChildren().add(signerLabel);


		final var keysPane = formatExtraSignersGridPane(KEYS_COLUMNS - 1);

		final var signers = rf.getSigningPublicKeys();
		final List<File> requiredKeys = controller.extractRequiredKeys(signers);
		requiredKeys.forEach(rf::addToSignerSet);

		keysPane.managedProperty().bind(keysPane.visibleProperty());
		keysPane.visibleProperty().bind(Bindings.size(keysPane.getChildren()).greaterThan(0));

		signerLabel.setVisible(!requiredKeys.isEmpty() && !rf.isHistory());

		final var keysHBox = new HBox();
		keysHBox.setSpacing(10);

		keysHBox.managedProperty().bind(keysHBox.visibleProperty());
		keysHBox.visibleProperty().bind(Bindings.size(keysHBox.getChildren()).greaterThan(0));

		var counter = 0;
		for (final var requiredKey : requiredKeys) {
			final var checkBox = formatCheckBox(rf.getSignerSet(), requiredKey);
			checkBox.setSelected(!rf.getOldSigners().contains(requiredKey));
			if (requiredKeys.size() < KEYS_COLUMNS) {
				keysHBox.getChildren().add(checkBox);
			} else {
				keysPane.add(checkBox, counter % (KEYS_COLUMNS - 1), counter / (KEYS_COLUMNS - 1));
			}
			counter++;
		}

		signingKeys.getChildren().addAll(keysPane, keysHBox);
		signingKeys.setPadding(new Insets(5));

		final var signBox = new HBox();
		signBox.setAlignment(Pos.TOP_RIGHT);
		signBox.getChildren().addAll(setupFiller(), formatNeededSignersGridPane(buttonBar, extraBar, signingKeys));
		return signBox;
	}

	private CheckBox formatCheckBox(final Set<File> signersSet, final File keyFile) {
		final var baseName = FilenameUtils.getBaseName(keyFile.getName());
		final var checkBox = new CheckBox(baseName);
		checkBox.setSelected(true);
		checkBoxListener(signersSet, keyFile, baseName, checkBox);
		return checkBox;
	}

	private GridPane formatNeededSignersGridPane(final ButtonBar batchButtonBar, final ButtonBar extraBar,
			final HBox signingKeys) {
		final var signGrid = new GridPane();
		final var vBox = new VBox();
		vBox.setPrefHeight(Region.USE_COMPUTED_SIZE);
		vBox.setPrefWidth(Region.USE_COMPUTED_SIZE);
		vBox.getChildren().add(batchButtonBar);
		vBox.setAlignment(Pos.TOP_CENTER);
		signGrid.setVgap(10);
		signGrid.setHgap(10);
		signGrid.add(signingKeys, 0, 0);
		signGrid.add(vBox, 1, 0);
		signGrid.add(extraBar, 1, 1);
		return signGrid;
	}

	private GridPane formatExtraSignersGridPane(final int columns) {
		final var extraSignersGridPane = new GridPane();
		extraSignersGridPane.setVgap(10);
		extraSignersGridPane.setStyle("-fx-border-color: darkgray; -fx-border-radius: 5");
		extraSignersGridPane.setPadding(new Insets(5));

		for (var i = 0; i < columns; i++) {
			final var column = new ColumnConstraints();
			column.setPercentWidth(100 / (double) columns);
			extraSignersGridPane.getColumnConstraints().add(column);
		}
		return extraSignersGridPane;
	}

	private VBox formatExtraSignersVBox(final GridPane extraSignersGridPane) {
		final var extraSignersVBox = new VBox();
		extraSignersVBox.getChildren().add(new Label("More keys"));
		extraSignersVBox.managedProperty().bind(extraSignersVBox.visibleProperty());
		extraSignersVBox.getChildren().add(extraSignersGridPane);
		extraSignersVBox.setSpacing(10);

		extraSignersVBox.visibleProperty().bind(Bindings.size(extraSignersGridPane.getChildren()).greaterThan(0));
		return extraSignersVBox;
	}

	private List<File> loadKeyFiles() {
		final var files =
				BrowserUtilities.browseMultiFiles(controller.getLastTransactionsDirectory(), controller.homePane,
						"KeyStore", "pem");

		if (files == null) {
			return new ArrayList<>();
		}

		controller.setLastBrowsedDirectory(files.get(0));
		return files;
	}

	private void fillKeysGridPane(final GridPane extraSignersGridPane,
			final List<File> extraSignersList, final Set<File> signersSet) {
		extraSignersList.sort(new SortByFileBaseName());
		extraSignersGridPane.getChildren().clear();
		var counter = 0;
		for (final var file : extraSignersList) {
			final var checkBox = formatCheckBox(signersSet, file);
			final var tooltip = new Tooltip(file.getPath());
			tooltip.setStyle("-fx-background-color: white; -fx-text-fill: black;");
			checkBox.setTooltip(tooltip);
			extraSignersGridPane.add(checkBox, counter % KEYS_COLUMNS, counter / KEYS_COLUMNS);
			counter++;
		}
	}

	private Region setupFiller() {
		final var filler = new Region();
		filler.setPrefHeight(Region.USE_COMPUTED_SIZE);
		filler.setPrefWidth(Region.USE_COMPUTED_SIZE);
		HBox.setHgrow(filler, Priority.ALWAYS);
		return filler;
	}


	public String buildCommentFile(final RemoteFile rf, final TextArea comment, final String name) throws IOException {
		final var userComments = new UserComments.Builder()
				.withAuthor(controller.getEmailFromMap(rf.getParentPath()))
				.withComment(comment.getText())
				.build();


		final var userCommentLocation =
				rf.getParentPath().replace(INPUT_FILES, OUTPUT_FILES) + File.separator + FilenameUtils.getBaseName(
						name) + ".txt";
		if (new File(userCommentLocation).isFile()) {
			Files.deleteIfExists(Path.of(userCommentLocation));
		}

		userComments.toFile(userCommentLocation);
		return userCommentLocation;
	}

	private void signTransactionAndComment(final RemoteFile rf, final Pair<String, KeyPair> pair) {
		switch (rf.getType()) {
		case TRANSACTION, LARGE_BINARY:
				createSignedTransaction(rf, pair);
				break;
			case BATCH:
				if (!(rf instanceof BatchFile)) {
					throw new HederaClientRuntimeException("Remote file is not a batch file");
				}
				createSignedTransaction(rf, pair);
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + rf.getType());
		}
	}

	private void createSignedTransaction(final RemoteFile rf, final Pair<String, KeyPair> pair) {
		try {
			//When a RemoteFile executes, it will moveToHistory and setHistory
			rf.execute(pair, user, output, () -> {
				controller.historyPaneController.addToHistory(rf);
				removeFile(rf);
				try {
					exportComments(rf, rf.getCommentArea(), rf.getName());
				} catch (HederaClientException e) {
					logger.error("exporting comments failed", e);
					controller.displaySystemMessage(e.getCause().toString());
				}
			});
		} catch (final Exception e) {
			logger.error("createSignedTransaction failed", e);
			controller.displaySystemMessage(e.getCause().toString());
		}

	}

	private Button buildWhiteButton(final String decline) {
		final var declineButton = new Button(decline);
		declineButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: " +
						"10; -fx-background-radius: 10;");
		declineButton.setMinWidth(150);
		return declineButton;
	}

	private Button buildBlueButton(final String legend) {
		final var acceptButton = new Button(legend);
		acceptButton.setStyle(
				"-fx-background-color: #0b9dfd; -fx-border-color: #0b9dfd; -fx-text-fill: white; -fx-border-radius: " +
						"10; -fx-background-radius: 10;");
		acceptButton.setMinWidth(150);
		return acceptButton;
	}

	private void exportComments(final RemoteFile rf, final TextArea comment,
			final String name) throws HederaClientException {
		if ("".equals(comment.getText())) {
			return;
		}
		final String userCommentLocation;
		try {
			userCommentLocation = buildCommentFile(rf, comment, name);
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}
		final var remoteLocation = rf.getParentPath();
		final var zipFiles = Collections.singletonList(new File(userCommentLocation));
		moveToOutput(zipFiles, remoteLocation);
	}

	private void moveToOutput(final List<File> zipFiles, final String remoteLocation) throws HederaClientException {
		final var emailFromMap = controller.getEmailFromMap(remoteLocation);
		final var outputFolder =
				("".equals(emailFromMap)) ? File.separator : File.separator + OUTPUT_FILES + File.separator;
		var fileService = FileAdapterFactory.getAdapter(remoteLocation);
		if (fileService == null) {
			throw new HederaClientRuntimeException("Error creating file service");
		}
		final var remoteDestination = outputFolder + ((fileService.getPath().contains("Volumes")) ? "" : user);

		if (remoteDestination.contains("Volumes")) {
			// USB is special
			if (!fileService.exists(File.separator + OUTPUT_FILES)) {
				new File(fileService.getPath() + File.separator + OUTPUT_FILES).mkdirs();
			}
		} else if (!fileService.exists(File.separator + OUTPUT_FILES + File.separator + emailFromMap)) {
			// If the user doesn't exist his signature is kept locally
			fileService = FileAdapterFactory.getAdapter(controller.getPreferredStorageDirectory());
			new File(controller.getPreferredStorageDirectory() + remoteDestination).mkdirs();
		}


		for (final var zip : zipFiles) {
			if (fileService != null) {
				try {
					fileService.upload(zip.getAbsolutePath(), remoteDestination);
				} catch (final HederaClientException e) {
					PopupMessage.display("Unable to upload",
							"Could not upload the file to the specified folder. Please check you have the appropriate" +
									" permissions",
							"OK");
					logger.error("fileService.upload failed", e);
					controller.displaySystemMessage(e.getCause().toString());
				}
			}
		}
	}

	private void ensureInternalInputExists() {
		var f = new File(DEFAULT_INTERNAL_FILES, INPUT_FILES);
		if (f.mkdirs()) {
			logger.info("created {}", f.getAbsolutePath());
		}
	}

	class SortByFileBaseName implements Comparator<File> {
		@Override
		public int compare(final File o1, final File o2) {
			final var path1 = o1.getAbsolutePath();
			final var path2 = o2.getAbsolutePath();
			final var toolsPath = controller.getPreferredStorageDirectory();
			if (path1.contains(toolsPath) && path2.contains(toolsPath)) {
				return (FilenameUtils.getBaseName(o1.getName()).toLowerCase()).compareTo(
						FilenameUtils.getBaseName(o2.getName()).toLowerCase());
			}
			if (path1.contains(toolsPath)) {
				return -1;
			}
			if (path2.contains(toolsPath)) {
				return 1;
			}
			return (FilenameUtils.getBaseName(o1.getName()).toLowerCase()).compareTo(
					FilenameUtils.getBaseName(o2.getName()).toLowerCase());
		}
	}
}
