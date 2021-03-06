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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.fileservices.FileAdapterFactory;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.remote.BatchFile;
import com.hedera.hashgraph.client.core.remote.BundleFile;
import com.hedera.hashgraph.client.core.remote.InfoFile;
import com.hedera.hashgraph.client.core.remote.PublicKeyFile;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.RemoteFilesMap;
import com.hedera.hashgraph.client.core.remote.SoftwareUpdateFile;
import com.hedera.hashgraph.client.core.remote.TransactionFile;
import com.hedera.hashgraph.client.core.remote.helpers.UserComments;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.ui.popups.ExtraKeysSelectorPopup;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.sdk.KeyList;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
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

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyPair;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.GPG_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_COLUMNS;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.PUBLIC_KEY_LOCATION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.enums.Actions.ACCEPT;
import static com.hedera.hashgraph.client.core.enums.Actions.DECLINE;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.checkBoxListener;


@SuppressWarnings({ "ResultOfMethodCallIgnored" })
public class HomePaneController implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(HomePaneController.class);
	private static final String OUTPUT_FILES = "OutputFiles";
	private static final String INPUT_FILES = "InputFiles";
	private static final double VBOX_SPACING = 20;
	private boolean badDrive = false;

	// region FXML

	public VBox defaultViewVBox;
	public VBox newFilesViewVBox;
	public ScrollPane homeFilesScrollPane;

	@FXML
	private Controller controller;
	// endregion

	private long lastModified = 0;
	private int lastCount = 0;
	private boolean historyChanged = false;
	private boolean forceUpdate = false;
	private final RemoteFilesMap remoteFilesMap = new RemoteFilesMap();
	private String output = "";
	private String user = "";

	public void setForceUpdate(final boolean force) {
		forceUpdate = force;
	}

	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	public void initializeHomePane() {
		newFilesViewVBox.prefWidthProperty().bind(homeFilesScrollPane.widthProperty());
		newFilesViewVBox.setSpacing(VBOX_SPACING);

		try {
			// Only refresh if there have been changes in the remotes or the history
			final var countFiles = countTotalFiles();
			if (updateNotNeeded(countFiles)) {
				setForceUpdate(false);
				return;
			}
			setForceUpdate(true);
			remoteFilesMap.clearMap();
			loadRemoteFilesMap();

			lastCount = countFiles;

			loadNewFilesBox(remoteFilesMap);
			newFilesViewVBox.setVisible(true);
			newFilesViewVBox.setDisable(false);

			// Show new transactions
			if (remoteFilesMap.size() > 0) {
				defaultViewVBox.setDisable(true);
				defaultViewVBox.setVisible(false);
				if (newFilesViewVBox.getChildren().isEmpty()) {
					final var noTasksLabel = new Label("No new tasks.");
					noTasksLabel.setPadding(new Insets(15, 0, 5, 15));
					newFilesViewVBox.getChildren().add(noTasksLabel);
				}
			} else {
				defaultViewVBox.setDisable(false);
				defaultViewVBox.setVisible(true);
			}

			if (remoteFilesMap.size() > 0) {
				defaultViewVBox.setVisible(false);
			}

		} catch (final HederaClientException e) {
			logger.error("initializeHomePane error", e);
			controller.displaySystemMessage(e.getCause().toString());
		}
		Platform.runLater(() -> homeFilesScrollPane.setVvalue(0.0));
		logger.info("Home pane initialized");
		historyChanged = false;
		setForceUpdate(false);
	}

	private int countTotalFiles() {
		var count = 0;
		final var outs = controller.getOneDriveCredentials();
		for (final var inputLocation : outs.keySet()) {
			final var filelist = new File(inputLocation, INPUT_FILES).listFiles();
			if (filelist == null) {
				if (!badDrive) {
					badDrive = true;
					Platform.runLater(() -> PopupMessage.display("Error reading files", String.format(
							"The application was unable to read files from the remote location: %s. Please make sure " +
									"that the application is able to read the drive.", inputLocation)));
				}
				continue;
			}
			count += Objects.requireNonNull(filelist).length;
		}
		return count;
	}

	public boolean updateNotNeeded(final int countFiles) {
		return (lastCount == countFiles) && !historyChanged && !forceUpdate;
	}

	private void loadNewFilesBox(final RemoteFilesMap remoteFilesMap) throws HederaClientException {
		newFilesViewVBox.getChildren().clear();
		final var newFiles = displayFiles(remoteFilesMap);
		if (!newFiles.isEmpty()) {
			newFilesViewVBox.getChildren().addAll(newFiles);
		}
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
			if (forceUpdate) {
				remoteFilesMap.clearMap();
			}
			loadInputFolderIntoMap(inputFolder);

			// before showing the transactions need to check the history
			removeHistoryFiles();
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
			final var count = remoteFilesMap.size();
			final var fileService = FileAdapterFactory.getAdapter(s);
			if (fileService != null && fileService.exists() && (fileService.lastModified() > lastModified || forceUpdate)) {
				final var remoteFiles = new RemoteFilesMap(version).fromFile(fileService);
				this.remoteFilesMap.addAllNotExpired(remoteFiles);
				lastModified = fileService.lastModified();
				logger.info("{} Files loaded from {}", this.remoteFilesMap.size() - count, s);
			}
		}
	}

	/**
	 * Remove the files from the map that are also in the history
	 */
	private void removeHistoryFiles() {
		for (final RemoteFile rf : remoteFilesMap.getFiles()) {
			if (rf.getType().equals(FileType.SOFTWARE_UPDATE)) {
				final var su = (SoftwareUpdateFile) rf;
				final var currentVersion = controller.getVersion().split(" ");
				if (su.compareVersion(currentVersion[1]) > 0 && controller.historyPaneController.isHistory(rf.hashCode())) {
					controller.historyPaneController.removeFromHistory(rf);
				}
			}
			if (controller.historyPaneController.isHistory(rf.hashCode())) {
				logger.info("Removing {}", rf.getName());
				remoteFilesMap.remove(rf.getName());
			}
		}
		logger.info("Done removing history");
	}


	private List<VBox> displayFiles(final RemoteFilesMap remoteFilesMap) throws HederaClientException {

		// Filter all but the last software update
		final List<RemoteFile> files = new ArrayList<>();

		var sFile = new SoftwareUpdateFile();
		for (final var file : remoteFilesMap.getFiles()) {
			if (!file.getType().equals(FileType.SOFTWARE_UPDATE)) {
				files.add(file);
				continue;
			}
			if (file.compareTo(sFile) > 0) {
				sFile = (SoftwareUpdateFile) file;
			}
		}
		if (sFile.isValid()) {
			files.add(sFile);
		}
		Collections.sort(files);
		return getFileBoxes(files);
	}

	private List<VBox> getFileBoxes(final List<RemoteFile> fileList) throws HederaClientException {
		final List<VBox> boxes = new ArrayList<>();
		Collections.sort(fileList);
		for (final var rf : fileList) {
			if (rf.getType().equals(FileType.METADATA) ||
					rf.getType().equals(FileType.COMMENT) ||
					controller.historyPaneController.isHistory(rf.hashCode())) {
				continue;
			}

			if (rf instanceof TransactionFile) {
				setupKeyTree((TransactionFile) rf);
			}

			final var fileBox = rf.buildDetailsBox();
			final var buttonsBox = getButtonsBox(rf);
			if (buttonsBox != null) {
				fileBox.getChildren().add(buttonsBox);
			}
			if (fileBox != null && !fileBox.getChildren().isEmpty()) {
				boxes.add(fileBox);
			}
		}
		return boxes;
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
		var key = new KeyList();
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
			logger.error(e.getMessage());
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
					logger.error(exception);
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
				historyChanged = true;
				controller.loadPubKeys();
				controller.accountsPaneController.initializeAccountPane();
				controller.keysPaneController.initializeKeysPane();
				initializeHomePane();
			} catch (final IOException | HederaClientException e) {
				logger.error(e);
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
				historyChanged = true;
			} catch (final HederaClientException e) {
				logger.error(e);
				controller.displaySystemMessage(e.getMessage());
			}
			initializeHomePane();
		});
		return declineButton;
	}

	private Button buildCancelButton(final RemoteFile rf) {
		final var cancelButton = buildWhiteButton("CANCEL");
		cancelButton.setOnAction(event -> {
			try {
				rf.moveToHistory();
				controller.historyPaneController.addToHistory(rf);
				historyChanged = true;
			} catch (final HederaClientException e) {
				logger.error(e);
				controller.displaySystemMessage(e.getMessage());
			}
			initializeHomePane();
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
				historyChanged = true;
				forceUpdate = true;
				initializeHomePane();
			} catch (final HederaClientException e) {
				logger.error(e);
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
				logger.error(e);
			}
			historyChanged = true;

			runUpdate(rf.getPath());
		});
		return button;
	}

	private boolean verifySignature(final String filePath) {
		try {
			final var signaturePath = filePath + "." + GPG_EXTENSION;
			if (!new File(signaturePath).exists()) {
				logger.info("Cannot find signature file");
				return false;
			}
			if (!new File(DEFAULT_STORAGE + PUBLIC_KEY_LOCATION).exists()) {
				logger.error("Cannot find gpg public key file");
				return false;
			}
			return SecurityUtilities.verifyFile(filePath, signaturePath, DEFAULT_STORAGE + PUBLIC_KEY_LOCATION);
		} catch (final Exception e) {
			logger.error(e);
			return false;
		}
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
				logger.error(exception);
			}
		}
		historyChanged = true;
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
			logger.error(e);
			PopupMessage.display("Error opening update file",
					"The software update file cannot be opened.\nPlease contact the administrator.", "CLOSE");
		} catch (final InterruptedException e) {
			logger.error("Interrupted exception: {}", e.getMessage());
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
			case TRANSACTION:
			case LARGE_BINARY:
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
			rf.execute(pair, user, output);
			exportComments(rf, rf.getCommentArea(), rf.getName());
			rf.setHistory(true);
			controller.historyPaneController.addToHistory(rf);
			historyChanged = true;
			initializeHomePane();
		} catch (final Exception e) {
			logger.error(e);
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
					logger.error(e);
					controller.displaySystemMessage(e.getCause().toString());
				}
			}
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
