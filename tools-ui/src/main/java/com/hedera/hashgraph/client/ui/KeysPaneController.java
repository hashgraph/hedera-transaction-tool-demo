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

import com.google.common.collect.Sets;
import com.google.common.primitives.Chars;
import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.constants.Messages;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.Ed25519PrivateKey;
import com.hedera.hashgraph.client.core.security.PasswordAuthenticator;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.popups.CompleteKeysPopup;
import com.hedera.hashgraph.client.ui.popups.FinishBox;
import com.hedera.hashgraph.client.ui.popups.GenericPopup;
import com.hedera.hashgraph.client.ui.popups.NewPasswordPopup;
import com.hedera.hashgraph.client.ui.popups.PasswordBox;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.popups.ThreeButtonPopup;
import com.hedera.hashgraph.client.ui.utilities.KeysTableRow;
import com.hedera.hashgraph.client.ui.utilities.ResponseEnum;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.BadMnemonicException;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.ScrollEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.TextAlignment;
import net.i2p.crypto.eddsa.EdDSAPublicKey;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyPair;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_SIZE;
import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SALT_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static java.util.Arrays.fill;
import static java.util.Arrays.stream;
import static org.apache.commons.io.FileUtils.contentEquals;
import static org.apache.commons.io.FileUtils.copyFile;
import static org.apache.commons.io.FileUtils.moveFile;

public class KeysPaneController implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(KeysPaneController.class);
	private static final String MISSING_HASHCODE_MESSAGE =
			"The application has detected the presence of private keys that are not associated with any " +
					"recovery phrases and will attempt to associate them with the current phrase.\n\nYou " +
					"will be prompted for your password.";
	private static final String MISSING_PUBLIC_KEY_MESSAGE =
			"The application has detected the presence of private keys without corresponding public keys" +
					".\n\nYou will be prompted for your password to generate a public key file.";
	private static final String KEYS_GENERATED_MESSAGE =
			"A private and public key pair has been created, with index %d.\nIt can be found at: ";

	private static final String DUPLICATED_KEY_NAME_MESSAGE =
			"Cannot replace a public key that is associated with an existing private key.";
	private static final String KEYS_STRING = "/Keys/";
	private static final String ACCEPT_MESSAGE = "CONTINUE";
	private static final String MNEMONIC_IS_NULL = "Mnemonic is null";

	public StackPane keysPane;
	public ScrollPane mainKeysScrollPane;

	public VBox mainVBox;
	public VBox signingKeysVBox;
	public VBox btnRecoverKey;
	public VBox btnRecoveryPhrase;
	public VBox createKeysVBox;
	public VBox reGenerateKeysVBox;
	public VBox mnemonicWordsVBox;
	public VBox recoveryVBox;

	public HBox phraseHBox;

	public Button btnCreateKeys;
	public Button btnImportKeys;
	public Button btnRegenerateKeys;
	public Button btnShowMnemonicWords;
	public Button createKeysButton;
	public Button cancelCreationButton;
	public Button recoverKeysButton;
	public Button cancelRecoverKeyButton;
	public Button copyMnemonicToClipboard;
	public Button cancelEditMnemonicButton;

	public TextField nicknameTextBox;
	public TextField recoverNicknameField;
	public TextField recoverIndexField;

	public Label dummy;
	public Label nicknameErrorLabel;
	public Label phrasePasswordErrorLabel;

	public Button publicKeyToolTip;
	public Button linkedPrivateToolTip;
	public Button unlinkedPrivateToolTip;
	public Button changePasswordKP;

	@FXML
	private Controller controller;

	// key: Key name; value: location of the file
	private final Map<String, String> publicKeysMap = new HashMap<>();

	// key: Key name; value: list of accounts that contain this public key
	private final Map<String, List<String>> keysAccountsMap = new HashMap<>();

	// key: Key name; value: location of the file
	private final Map<String, String> orphanPEMs = new HashMap<>();

	// key: Key name; value: index used to generate the pem
	private final Map<String, Integer> indexMap = new HashMap<>();

	// key: Key name; value: hash code of the mnemonic used to generate the pem
	private final Map<String, String> pemMnemonicMap = new HashMap<>();

	// key: Key name; value: location of the file
	private Map<String, String> privateKeysMap = new HashMap<>();

	private String currentHashCode = null;

	// region INITIALIZATION
	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	public void initializeKeysPane() {
		try {
			currentHashCode = String.valueOf(controller.getMnemonicHashCode());

			mainKeysScrollPane.setFitToWidth(true);

			closeBoxes();
			cleanTextFields();
			signingKeysVBox.prefWidthProperty().bind(mainKeysScrollPane.widthProperty());

			setupBindings(btnCreateKeys, createKeysVBox, btnImportKeys, btnRegenerateKeys,
					reGenerateKeysVBox, btnShowMnemonicWords, mnemonicWordsVBox, copyMnemonicToClipboard, recoveryVBox,
					signingKeysVBox);

			removeKeysTables(signingKeysVBox);

			populatePrivateKeysMap();
			populatePublicKeysMap();
			populateKeysTables();
			initializeIndexMap();

			createKeysVBox.setVisible(false);
			reGenerateKeysVBox.setVisible(false);
			mnemonicWordsVBox.setVisible(false);
			copyMnemonicToClipboard.setVisible(false);


			// Events
			nicknameTextBox.setOnKeyReleased(this::nickNameTextBoxEvent);

			recoverIndexField.textProperty().addListener(this::recoverIndexFieldListenerAction);

			recoverIndexField.focusedProperty().addListener(this::recoverIndexFieldFocusedAction);

			recoverNicknameField.focusedProperty().addListener(this::recoverNickNameFieldFocusedAction);

			recoverNicknameField.setOnKeyReleased(this::recoverNicknameFieldKeyAction);

			recoveryVBox.managedProperty().bind(recoveryVBox.visibleProperty());

			// region Tooltips
			publicKeyToolTip.setOnAction(actionEvent -> Utilities.showTooltip(controller.settingsPane, publicKeyToolTip,
					Messages.UNLINKED_PUBLIC_KEY_TOOLTIP_TEXT));

			linkedPrivateToolTip.setOnAction(
					actionEvent -> Utilities.showTooltip(controller.settingsPane, linkedPrivateToolTip,
							Messages.LINKED_PUBLIC_KEY_TOOLTIP_TEXT));

			unlinkedPrivateToolTip.setOnAction(
					actionEvent -> Utilities.showTooltip(controller.settingsPane, unlinkedPrivateToolTip,
							Messages.UNLINKED_PK_TOOLTIP_TEXT));

			controller.homePaneController.setForceUpdate(true);

			//endregion
		} catch (final HederaClientException e) {
			logger.error(e);
		}

	}

	private void nickNameTextBoxEvent(final KeyEvent keyEvent) {
		if (nicknameTextBox.getText().equals("")) {
			nicknameErrorLabel.setVisible(false);
			createKeysButton.setDisable(true);
			return;
		}

		final var pathToKeys = controller.getPreferredStorageDirectory() + KEYS_STRING + nicknameTextBox.getText();
		final var exists =
				new File(pathToKeys + "." + PK_EXTENSION).exists() || new File(
						pathToKeys + "." + PUB_EXTENSION).exists();

		if (exists) {
			nicknameErrorLabel.setVisible(true);
			createKeysButton.setDisable(true);
		} else {
			createKeysButton.setDisable(false);
			nicknameErrorLabel.setVisible(false);
		}

		if (keyEvent.getCode() == KeyCode.ENTER) {
			if (!exists) {
				try {
					generateKeysEvent();
				} catch (final HederaClientException e) {
					logger.error(e);
					controller.displaySystemMessage(e);
				}
			}
		} else if (keyEvent.getCode() == KeyCode.TAB) {
			createKeysButton.requestFocus();
		}
	}

	public Map<String, String> getPublicKeysMap() {
		return publicKeysMap;
	}

	private void cleanTextFields() {
		nicknameTextBox.clear();
		recoverNicknameField.clear();
		recoverIndexField.clear();
	}

	private void removeKeysTables(final VBox... keysVBox) {
		for (final var vBox : keysVBox) {
			final var children = vBox.getChildren();
			for (final var child : children) {
				if (child instanceof TableView) {
					vBox.getChildren().remove(child);
					break;
				}
			}
		}
	}

	private void populatePublicKeysMap() {
		final Map<String, List<String>> accountKeyMap = new HashMap<>();
		try {
			controller.loadPubKeys();
			if (new File(controller.getPreferredStorageDirectory() + "/Accounts").mkdirs()) {
				logger.info("Accounts folder created");
			}
			final var accounts = new File(controller.getPreferredStorageDirectory() + "/Accounts").listFiles(
					(dir, name) -> name.endsWith(INFO_EXTENSION));
			assert accounts != null;
			final var nicknames =
					new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

			for (final var account : accounts) {
				final var name = FilenameUtils.getBaseName(account.getName());
				if (nicknames.has(name)) {
					accountKeyMap.put(nicknames.get(name).getAsString(), getKnownKeysFromAccountInfo(account.toPath()));
				}
			}
		} catch (final Exception ex) {
			logger.error(ex);
		}

		publicKeysMap.clear();
		try {
			final var pubKeyFiles = new File(controller.getPreferredStorageDirectory() + KEYS_STRING).listFiles(
					(dir, name) -> isPUBFile(new File(name).toPath()));
			assert pubKeyFiles != null;
			stream(pubKeyFiles).forEachOrdered(
					pubKeyFile -> publicKeysMap.put(pubKeyFile.getName(), pubKeyFile.getAbsolutePath()));
		} catch (final Exception ex) {
			logger.error(ex);
		}

		keysAccountsMap.clear();
		for (final var publicKey : publicKeysMap.keySet()) {
			final List<String> accounts = new ArrayList<>();
			for (final var entry : accountKeyMap.entrySet()) {
				if (entry.getValue().contains(publicKey)) {
					accounts.add(entry.getKey());
				}
			}
			keysAccountsMap.put(publicKey, accounts);
		}
	}

	public void populateKeysTables() throws HederaClientException {

		try {
			final List<KeysTableRow> keysTableRows = new ArrayList<>();
			final List<String> keys = new ArrayList<>(publicKeysMap.keySet());
			Collections.sort(keys);

			for (final var key : keys) {
				addKeyToTableRowsList(keysTableRows, key);
			}

			// Add PEMs without public key
			for (final var entry : privateKeysMap.entrySet()) {
				addOrphanPEMSToTable(keysTableRows, entry);
			}

			keysTableRows.sort(Comparator.comparing(KeysTableRow::getKeyName));

			final var signingKeysTableView = new TableView<KeysTableRow>();

			final var iconsColumn = getKeysIconsColumn(signingKeysTableView);

			final var nameColumn = getNamesTableColumn(signingKeysTableView);

			final var linkedAccountsColumn = getLinkedAccountsTableColumn(signingKeysTableView);

			signingKeysTableView.getColumns().addAll(iconsColumn, nameColumn, linkedAccountsColumn);

			setSigningKeysRowFactory(signingKeysTableView);

			signingKeysTableView.getItems().addAll(keysTableRows);

			signingKeysTableView.setFixedCellSize(30);
			signingKeysTableView.prefHeightProperty().bind(signingKeysTableView.fixedCellSizeProperty().multiply(
					Bindings.size(signingKeysTableView.getItems()).add(2.01)));
			signingKeysTableView.minHeightProperty().bind(signingKeysTableView.prefHeightProperty());
			signingKeysTableView.maxHeightProperty().bind(signingKeysTableView.prefHeightProperty());

			signingKeysVBox.addEventFilter(ScrollEvent.ANY, event -> {
				if (event.getDeltaX() != 0) {
					event.consume();
				}
			});

			if (!signingKeysTableView.getItems().isEmpty()) {
				signingKeysVBox.getChildren().add(signingKeysTableView);
			}

		} catch (final KeyStoreException e) {
			throw new HederaClientException(e);
		}
	}

	private void setSigningKeysRowFactory(final TableView<KeysTableRow> signingKeysTableView) {
		signingKeysTableView.setRowFactory(
				signingKeyTableRowTableView -> {
					final TableRow<KeysTableRow> row = new TableRow<>() {
					};

					row.setOnMouseClicked(mouseEvent -> {
						if (mouseEvent.getClickCount() == 2 && !row.isEmpty()) {
							final var rowData = row.getItem();
							showPrivateKeyCompletePopup(rowData);
						}
					});
					return row;
				});
	}

	@NotNull
	private TableColumn<KeysTableRow, String> getLinkedAccountsTableColumn(
			final TableView<KeysTableRow> signingKeysTableView) {
		final var linkedAccountsColumn = new TableColumn<KeysTableRow, String>("Associated accounts");
		linkedAccountsColumn.setCellValueFactory(new PropertyValueFactory<>("accountList"));
		linkedAccountsColumn.prefWidthProperty().bind(
				signingKeysTableView.widthProperty().divide(4).multiply(3).subtract(5));
		return linkedAccountsColumn;
	}

	@NotNull
	private TableColumn<KeysTableRow, String> getNamesTableColumn(
			final TableView<KeysTableRow> signingKeysTableView) {
		final var nameColumn = new TableColumn<KeysTableRow, String>("Key nickname");
		nameColumn.setCellValueFactory(new PropertyValueFactory<>("keyName"));
		nameColumn.prefWidthProperty().bind(signingKeysTableView.widthProperty().divide(5));
		return nameColumn;
	}

	@NotNull
	private TableColumn<KeysTableRow, String> getKeysIconsColumn(
			final TableView<KeysTableRow> signingKeysTableView) {
		final var iconsColumn = new TableColumn<KeysTableRow, String>("");
		iconsColumn.setCellValueFactory(new PropertyValueFactory<>("iconFile"));
		iconsColumn.prefWidthProperty().bind(signingKeysTableView.widthProperty().divide(20).multiply(1));

		iconsColumn.setCellFactory(
				publicKeysTableRowStringTableColumn -> new TableCell<>() {
					@Override
					public void updateItem(final String item, final boolean empty) {
						if (item != null) {
							final var imageView = new ImageView();
							imageView.setFitHeight(20);
							imageView.setPreserveRatio(true);
							imageView.setImage(new Image(item));
							setGraphic(imageView);
						}
					}
				});

		iconsColumn.setStyle("-fx-alignment: CENTER");
		return iconsColumn;
	}

	private void addOrphanPEMSToTable(final List<KeysTableRow> keysTableRows,
			final Map.Entry<String, String> entry) throws KeyStoreException {
		final var key = entry.getKey();
		final var value = entry.getValue();
		if (publicKeysMap.containsKey(key.replace(PK_EXTENSION, PUB_EXTENSION))) {
			return;
		}
		final var index = getIndex(value);
		final var mnemonicBoolean = currentHashCode != null && currentHashCode.equals(hashAsString(value));
		keysTableRows.add(
				new KeysTableRow(FilenameUtils.removeExtension(key), "Missing public key", index, true,
						mnemonicBoolean));
	}

	private void addKeyToTableRowsList(final List<KeysTableRow> keysTableRows,
			final String key) throws KeyStoreException {
		final var pemLocation = publicKeysMap.get(key).replace(PUB_EXTENSION, PK_EXTENSION);
		// if pemLocation points to a pem file and it exists
		var signer = false;
		var mnemonicBoolean = false;
		var index = "public key";
		if (new File(pemLocation).exists() && isPEMFile(new File(pemLocation).toPath())) {
			index = getIndex(pemLocation);
			mnemonicBoolean = currentHashCode != null && currentHashCode.equals(hashAsString(pemLocation));
			signer = true;
		}

		keysTableRows.add(
				new KeysTableRow(FilenameUtils.removeExtension(key), keysAccountsMap.get(key),
						index, signer, mnemonicBoolean));
	}

	private String hashAsString(final String pemLocation) throws KeyStoreException {
		return String.valueOf(Ed25519KeyStore.getMnemonicHashCode(pemLocation));
	}

	/**
	 * Assign an index to a row. If the index is negative, returns an empty string
	 *
	 * @param pemFileLocation
	 * 		location of the pem file
	 * @return an integer as a string
	 * @throws KeyStoreException
	 * 		if loafing the key fails
	 */
	private String getIndex(final String pemFileLocation) throws KeyStoreException {
		return Ed25519KeyStore.getIndex(pemFileLocation) < 0 ? "none" : String.valueOf(
				Ed25519KeyStore.getIndex(pemFileLocation));
	}

	private void showPrivateKeyCompletePopup(final KeysTableRow rowData) {
		final var pubKeyAddress = publicKeysMap.get(rowData.getKeyName() + "." + PUB_EXTENSION);
		final var answer = CompleteKeysPopup.display(pubKeyAddress, true);
		if (Boolean.TRUE.equals(answer)) {
			initializeKeysPane();
		}
	}

	private List<String> getKnownKeysFromAccountInfo(
			final Path path) throws InvalidProtocolBufferException, HederaClientException {
		final var info = AccountInfo.fromBytes(readBytes(path.toString()));
		return Utilities.getKeysFromInfo(info, controller);
	}

	private void populatePrivateKeysMap() {
		privateKeysMap = new HashMap<>();

		final var keysDirectory = new File(controller.getPreferredStorageDirectory(), KEYS_STRING);
		if (keysDirectory.mkdirs()) {
			logger.info("Keys folder created");
		}
		final var pemFiles = keysDirectory.listFiles((dir, name) -> isPEMFile(new File(name).toPath()));
		if (pemFiles != null) {
			stream(pemFiles).forEach(pem -> privateKeysMap.put(pem.getName(), pem.getAbsolutePath()));
		}


		populatePemMaps();

		final var missingHashPemList =
				pemMnemonicMap.keySet().stream().filter(key -> "".equals(pemMnemonicMap.get(key))).collect(
						Collectors.toList());

		if (orphanPEMs.size() > 0 && mainKeysScrollPane.isVisible()) {
			handleOrphanPem();
		}

		if (!missingHashPemList.isEmpty()) {
			handleMissingHashPem(missingHashPemList);
			missingHashPemList.clear();
		}

	}

	private void handleMissingHashPem(final List<String> missingHashPemList) {
		try {
			GenericPopup.display("Unknown recovery phrase", ACCEPT_MESSAGE, "", false, false,
					MISSING_HASHCODE_MESSAGE);
			final var password = getPassword();
			if (password.length == 0) {
				return;
			}
			final var mnemonic = getMnemonicFromFile(password);
			if (mnemonic == null) {
				return;
			}
			if (currentHashCode == null) {
				currentHashCode = String.valueOf(mnemonic.words.hashCode());
			}
			handleMissingHashPemList(missingHashPemList, password, mnemonic);
			populatePemMaps();
			Arrays.fill(password, 'x');
		} catch (final HederaClientException | KeyStoreException | IOException exception) {
			logger.error(exception);
		}
	}

	private void handleOrphanPem() {
		GenericPopup.display("Missing Public Keys", ACCEPT_MESSAGE, "", false, false,
				MISSING_PUBLIC_KEY_MESSAGE);

		for (final var entry : orphanPEMs.entrySet()) {
			final var key = entry.getKey();
			final var value = entry.getValue();
			final var keyPair = getKeyPair(key);
			final var pubKey_filename = value.replace(PK_EXTENSION, PUB_EXTENSION);
			if (keyPair == null) {
				handleNullKeyPair(pubKey_filename);
				continue;
			}
			handleExistingKeyPair(keyPair, pubKey_filename);
		}
		orphanPEMs.clear();
	}

	private void handleMissingHashPemList(final List<String> missingHashPemList, final char[] password,
			final Mnemonic mnemonic) throws KeyStoreException, IOException {
		for (final var key : missingHashPemList) {
			final var keyStore = new Ed25519KeyStore.Builder().withPassword(password).build();
			final var originalPath = privateKeysMap.get(key);
			final var pubName0 = originalPath.replace(PK_EXTENSION, PUB_EXTENSION);
			final var index = Ed25519KeyStore.getIndex(originalPath);
			if (index < 0) {
				addEmptyHashToPem(originalPath);
				continue;
			}
			final var pk = Ed25519PrivateKey.fromMnemonic(mnemonic).derive(index);
			final var keyPair = keyStore.insertNewKeyPair(pk);
			if (verifyWithPublicKey(pubName0, (EdDSAPublicKey) keyPair.getPublic())) {
				moveFile(new File(originalPath), new File(getArchivePathname(key)));
				keyStore.write(originalPath, "Transaction Tool UI", index, controller.getVersion(),
						mnemonic.words.hashCode());
			} else {
				addEmptyHashToPem(originalPath);
			}
		}
	}

	private void handleExistingKeyPair(final KeyPair keyPair, final String pubKeyFilename) {
		try {
			EncryptionUtils.storePubKey(pubKeyFilename, (EdDSAPublicKey) keyPair.getPublic());
		} catch (final IOException e) {
			logger.error(e);
			logger.error("Cannot store the public key");
		}
	}

	private void handleNullKeyPair(final String pubKeyFilename) {
		try {
			final var empty = new File(pubKeyFilename);
			if (empty.createNewFile()) {
				logger.info("Created empty file {}", pubKeyFilename);
			}
		} catch (final IOException e) {
			logger.error(e);
			logger.error("Cannot create empty file");
		}
	}

	private void populatePemMaps() {
		for (final var entry : privateKeysMap.entrySet()) {
			try {
				final var key = entry.getKey();
				final var value = entry.getValue();
				final var hashCode = Ed25519KeyStore.getMnemonicHashCode(value);

				if (hashCode != null) {
					pemMnemonicMap.put(key, hashCode.toString());
				} else {
					pemMnemonicMap.put(key, "");
				}

				if (!new File(value.replace(PK_EXTENSION, PUB_EXTENSION)).exists()) {
					orphanPEMs.put(key, value);
				}
			} catch (final KeyStoreException e) {
				logger.error(e);
			}
		}
	}

	/**
	 * Returns a unique file name for the archived key.
	 *
	 * @param key
	 * 		original name of the key
	 * @return complete path to a new, unique filename for the key in the archives
	 */
	private String getArchivePathname(final String key) {
		var pathname = controller.getPreferredStorageDirectory() + "/Archive/" + key;
		var count = 0;

		while (new File(pathname).exists()) {
			count++;
			pathname = String.format("%s/Archive/%s_%d.pem",
					controller.getPreferredStorageDirectory(), getBaseName(key),
					count);
		}
		return pathname;
	}

	/**
	 * This method will be used if the pem file was generated prior to version 2.0 of the tools (no mnemonic) or if the
	 * user declines to decode the mnemonic when the key is detected. The empty hasCode will prevent the app to ask for
	 * the password every time the key pane is reloaded
	 *
	 * @param s
	 * 		key location
	 * @throws IOException
	 * 		if there are failures accessing the key location
	 */
	private void addEmptyHashToPem(final String s) throws IOException {
		try (final var output = new BufferedWriter(new FileWriter(s, true))) {
			output.append("Recovery Phrase Hash: 0");
		}
	}

	private KeyPair getKeyPair(final String key) {
		return controller.getKeyPairUtility().getKeyPairFromPEM(new File(orphanPEMs.get(key)),
				String.format("Please enter the password for key %s", key));
	}

	private void initializeIndexMap() throws HederaClientException {
		var maxIndex = -1;
		for (final var entry : privateKeysMap.entrySet()) {
			try {
				final var index = Ed25519KeyStore.getIndex(entry.getValue());
				indexMap.put(entry.getKey(), index);
				if (index > maxIndex) {
					maxIndex = index;
				}
			} catch (final KeyStoreException e) {
				logger.error(e);
				controller.displaySystemMessage(e);
				throw new HederaClientException(e);
			}
		}
		controller.setLastIndex(maxIndex + 1);

	}

	// endregion

	// region EVENTS
	public void generateKeys() {
		btnCreateKeys.setVisible(false);
		btnRegenerateKeys.setVisible(false);
		btnShowMnemonicWords.setVisible(false);
		createKeysVBox.setVisible(true);
	}

	public void reGenerateKeys() {
		btnCreateKeys.setVisible(false);
		btnRegenerateKeys.setVisible(false);
		btnShowMnemonicWords.setVisible(false);
		reGenerateKeysVBox.setVisible(true);
	}

	public void showMnemonic() throws HederaClientException {
		final var password = getPassword();
		if (password.length == 0 || passwordIdentical(password)) {
			return;
		}
		showMnemonicPhrase(password);
		Arrays.fill(password, 'x');
		phrasePasswordErrorLabel.setVisible(false);
		btnCreateKeys.setVisible(false);
		btnRegenerateKeys.setVisible(false);
		btnShowMnemonicWords.setVisible(false);
		mnemonicWordsVBox.setVisible(true);
	}

	private boolean passwordIdentical(final char[] password) {
		return Sets.newHashSet(Chars.asList(password)).size() == 1;
	}

	public void generateKeysEvent() throws HederaClientException {
		final var password = getPassword();
		if (password.length == 0) {
			return;
		}

		final var mnemonic = getMnemonicFromFile(password);
		if (mnemonic == null) {
			logger.error(MNEMONIC_IS_NULL);
			return;
		}
		setupMnemonicHBox(mnemonic);

		// Create key store for account
		try {
			final var nickname = nicknameTextBox.getText();
			if (nickname.equals("")) {
				return;
			}

			var lastIndex = -1;
			for (final var entry : pemMnemonicMap.entrySet()) {
				if (currentHashCode.equals(entry.getValue())) {
					if (!privateKeysMap.containsKey(entry.getKey())) {
						throw new HederaClientRuntimeException("Could not find key in map");
					}
					final var k = Ed25519KeyStore.getIndex(privateKeysMap.get(entry.getKey()));
					if (k > lastIndex) {
						lastIndex = k;
					}
				}
			}

			final var keyStoreName = generateAndStoreKeyPair(password, nickname, lastIndex + 1, false);

			if (!"".equals(keyStoreName)) {

				controller.incrementIndex();

				FinishBox.display(new File(keyStoreName), "Keys Generated", String.format(KEYS_GENERATED_MESSAGE,
						lastIndex + 1));
				indexMap.put(nickname + PK_EXTENSION, lastIndex + 1);
			}
		} catch (final Exception e) {
			logger.error(e);
		}
		populatePublicKeysMap();
		populatePrivateKeysMap();
		initializeKeysPane();
		closeGenerateKeys();
		fill(password, 'x');
	}

	public void closeGenerateKeys() {
		closeBoxes();
		resetKeyGenerationBox();
	}

	public void recoverKeysEvent() throws HederaClientException {
		if (indexMap.isEmpty()) {
			initializeIndexMap();
		}

		final var index = !"".equals(recoverIndexField.getText()) ? Integer.parseInt(recoverIndexField.getText()) : -1;
		final var nick = recoverNicknameField.getText() + "." + PK_EXTENSION;

		if (!indexMap.containsKey(nick) && index == -1) {
			PopupMessage.display("Missing Index", "Cannot recover a key without an index.", "OK");
			return;
		}

		if (handleOverwrite(index, nick)) {
			return;
		}

		final var password = getPassword();
		if (password.length == 0) {
			return;
		}
		final var mnemonic = getMnemonicFromFile(password);
		if (mnemonic == null) {
			PopupMessage.display("Error in recovery phrase", "The recovery phrase could not be found.", ACCEPT_MESSAGE);
			return;
		}

		// Create key store for account
		if (createKeyStoreForAccount(index, password)) {
			return;
		}

		fill(password, 'x');
		resetPanes();
	}

	private void resetPanes() throws HederaClientException {
		closeRecoverKeys();
		closeBoxes();
		resetKeyRecoveryBox();
		populatePrivateKeysMap();
		initializeIndexMap();
		initializeKeysPane();
	}

	private boolean createKeyStoreForAccount(final int index, final char[] password) {
		try {
			final var nickname = recoverNicknameField.getText();
			if (nickname.equals("")) {
				PopupMessage.display("Missing nickname",
						"Please provide a nickname for the key, for identification purposes");
				return true;
			}

			final var keyStoreName = generateAndStoreKeyPair(password, getBaseName(nickname), index, true);

			if ("".equals(keyStoreName)) {
				return false;
			}

			// Update the current index in preferences
			if (controller.getLastIndex() < index) {
				controller.setLastIndex(index + 1);
			}

			FinishBox.display(new File(keyStoreName), "Keys Recovered",
					"The private and public key pair has been recovered. It can be found at...");

		} catch (final Exception e) {
			logger.error(e);
		}
		return false;
	}

	private boolean handleOverwrite(final int index, final String nick) {
		var overwrite = false;
		if (indexMap.containsKey(nick)) {
			overwrite = true;
		} else {
			indexMap.put(nick, index);
		}

		if (indexMap.get(nick) == -1) {
			PopupMessage.display("Wrong version",
					"The private key specified was created with a previous version of the app and cannot be recovered",
					"OK");
			return true;
		}

		if (indexMap.get(nick) != index) {
			PopupMessage.display("Wrong index", "The index specified does not correspond to the private key.", "OK");
			return true;
		}

		if (overwrite) {
			final var answer = PopupMessage.display("Alert!",
					"This operation will overwrite the existing KeyPair. Are you sure you want to do that? This " +
							"operation is irreversible", true, ACCEPT_MESSAGE, "CANCEL");

			return Boolean.FALSE.equals(answer);
		}
		return false;
	}

	public void closeRecoverKeys() {
		closeBoxes();
		resetKeyRecoveryBox();
	}

	public void copyPhraseToClipBoard() {

		final var phraseLabel = (Label) phraseHBox.getChildren().get(0);

		final var mnemonic = phraseLabel.getText().replace("\n", "   ");


		Toolkit.getDefaultToolkit()
				.getSystemClipboard()
				.setContents(new StringSelection(mnemonic),
						null);
		PopupMessage.display("Recovery phrase", "The recovery phrase has been copied to the clipboard.", "OK");

	}

	private void showMnemonicPhrase(final char[] password) throws HederaClientException {
		phrasePasswordErrorLabel.setVisible(false);
		recoveryVBox.setVisible(true);
		copyMnemonicToClipboard.setVisible(true);
		final var mnemonic = getMnemonicFromFile(password);
		if (controller.isLegacyMnemonic() && mnemonic != null) {
			logger.info("Handling legacy mnemonic");
			final var passwordBytes = SecurityUtilities.keyFromPassword(password, controller.getSalt());
			SecurityUtilities.toEncryptedFile(passwordBytes, DEFAULT_STORAGE + File.separator + MNEMONIC_PATH,
					mnemonic.toString());
			controller.setLegacy(false);
		}
		setupMnemonicHBox(mnemonic);
	}


	private void setupMnemonicHBox(final Mnemonic mnemonic) {
		if (mnemonic == null) {
			logger.error(MNEMONIC_IS_NULL);
			return;
		}
		final var mnemonicLabel = new Label();
		var counter = 0;
		var phrase = "";
		for (final var word : mnemonic.toString().split(" ")) {
			phrase = phrase.concat(word.toUpperCase());
			if (counter < 23) {
				phrase = counter % 4 == 3 ? phrase.concat("\n") : phrase.concat("   ");
			}
			counter++;
		}

		mnemonicLabel.setText(phrase);
		mnemonicLabel.setWrapText(true);
		mnemonicLabel.setTextAlignment(TextAlignment.CENTER);
		mnemonicLabel.setPrefHeight(Region.USE_COMPUTED_SIZE);
		mnemonicLabel.setStyle("-fx-font-size: 22");
		phraseHBox.getChildren().clear();
		phraseHBox.getChildren().add(mnemonicLabel);
	}

	public void closeMnemonicBox() {
		phrasePasswordErrorLabel.setVisible(false);
		recoveryVBox.setVisible(false);
		copyMnemonicToClipboard.setVisible(false);
		phraseHBox.getChildren().clear();
		closeBoxes();
	}

	// endregion

	// region STYLING
	private void setupBindings(final Node... nodes) {
		for (final var n : nodes) {
			n.managedProperty().bind(n.visibleProperty());
		}
	}

	// endregion

	// region HELPERS
	private boolean isPEMFile(final Path path) {
		return path.getFileName().toString().endsWith(PK_EXTENSION);
	}

	private boolean isPUBFile(final Path path) {
		return path.getFileName().toString().endsWith(PUB_EXTENSION) || path.getFileName().toString().endsWith(
				TXT_EXTENSION);
	}

	private char[] getPassword() throws HederaClientException {
		final var passwordAuthenticator = new PasswordAuthenticator();
		char[] password;
		while (true) {
			password = PasswordBox.display("Password", "Please enter your password", "", false);
			if (password == null || passwordIdentical(password)) {
				return new char[0];
			}
			try {
				final var authenticate = controller.hasSalt() ?
						passwordAuthenticator.authenticate(password, controller.getHash()) :
						passwordAuthenticator.authenticateLegacy(password, controller.getHash());

				if (authenticate) {
					if (!controller.hasSalt()) {
						// recoverNicknameFieldKeyAction password migration
						logger.info("Handling password hash migration");
						controller.setHash(password);
						controller.setSalt(true);
					}
					break;
				} else {
					PopupMessage.display("Incorrect Password",
							"The password you entered does not match our records. Please try again.", "OK");
				}
			} catch (final InvalidKeySpecException | NoSuchAlgorithmException e) {
				logger.error(e);
				controller.displaySystemMessage(e);
				throw new HederaClientException(e);
			}
		}

		return password;
	}

	private String generateAndStoreKeyPair(final char[] password, final String nickname, final int index,
			final boolean overwrite) throws HederaClientException {
		final var prefDir = controller.getPreferredStorageDirectory();
		final var keysDir = prefDir + KEYS_STRING;

		// Create Keys/ if it doesn't exist
		if (new File(keysDir).mkdirs()) {
			logger.info("Keys folder has been created");
		}
		final Ed25519KeyStore keyStore;
		final KeyPair keyPair;
		final var keyStoreName =
				overwrite ? String.format("%s%s.pem", keysDir, nickname) : String.valueOf(findFileName(
						Paths.get(prefDir, "Keys"), nickname, PK_EXTENSION));

		try {
			keyStore = new Ed25519KeyStore.Builder().withPassword(password).build();
			final var mnemonic = getMnemonicFromFile(password);
			if (mnemonic == null) {
				controller.displaySystemMessage(MNEMONIC_IS_NULL);
				throw new HederaClientException(MNEMONIC_IS_NULL);
			}
			final var pk = Ed25519PrivateKey.fromMnemonic(mnemonic, index);

			final var pubName = String.valueOf(findFileName(Paths.get(prefDir, "Keys"), nickname, PUB_EXTENSION));
			keyPair = keyStore.insertNewKeyPair(pk);
			if (!verifyWithPublicKey(pubName, (EdDSAPublicKey) keyPair.getPublic())) {
				PopupMessage.display("Public Keys Mismatch", ErrorMessages.PUBLIC_KEY_MISMATCH_MESSAGE, ACCEPT_MESSAGE);
				return "";
			}
			keyStore.write(keyStoreName, "Transaction Tool UI", index, controller.getVersion(),
					mnemonic.words.hashCode());

			// Only write the pub key if it doesn't already exist
			EncryptionUtils.storePubKey(pubName, (EdDSAPublicKey) keyPair.getPublic());

		} catch (final KeyStoreException | IOException e) {
			logger.error(e);
			controller.displaySystemMessage(e);
			throw new HederaClientException(e);
		}
		return keyStoreName;
	}

	private boolean verifyWithPublicKey(final String pubName, final EdDSAPublicKey publicKey) throws IOException {
		if (!new File(pubName).exists()) {
			return true;
		}
		final var tempPub = System.getProperty("java.io.tmpdir") + "/tempPublic.pub";
		EncryptionUtils.storePubKey(tempPub, publicKey);

		return contentEquals(new File(pubName), new File(tempPub));
	}

	private Mnemonic getMnemonicFromFile(final char[] password) {
		final var mnemonicFile = new File(controller.getPreferredStorageDirectory(), MNEMONIC_PATH);
		Mnemonic mnemonic = null;
		try {
			if (mnemonicFile.exists()) {
				final var salt = controller.isLegacyMnemonic() ? new byte[SALT_LENGTH] : controller.getSalt();
				final var path = new File(controller.getPreferredStorageDirectory(), MNEMONIC_PATH);
				mnemonic = SecurityUtilities.fromEncryptedFile(password, salt, path.getAbsolutePath());
			}
		} catch (final HederaClientException e) {
			logger.error(e);
			controller.displaySystemMessage(e);
		}
		return mnemonic;
	}

	// endregion

	// region RESETS
	private void closeBoxes() {
		createKeysVBox.setVisible(false);
		reGenerateKeysVBox.setVisible(false);
		mnemonicWordsVBox.setVisible(false);
		btnCreateKeys.setVisible(true);
		btnRegenerateKeys.setVisible(true);
		btnShowMnemonicWords.setVisible(true);
		btnImportKeys.setVisible(true);
	}

	private void resetKeyGenerationBox() {
		nicknameTextBox.clear();
		createKeysButton.setDisable(true);
		mainKeysScrollPane.setVvalue(0.0);
	}

	private void resetKeyRecoveryBox() {
		recoverIndexField.clear();
		recoverNicknameField.clear();
		mainKeysScrollPane.setVvalue(0.0);
	}

	public void importKeys() throws IOException, KeyStoreException {
		final var pubKeys = new File(controller.getPreferredStorageDirectory() + KEYS_STRING).listFiles(
				(dir, name) -> name.endsWith(PUB_EXTENSION) || name.endsWith(TXT_EXTENSION));

		final var pemKeys = new File(controller.getPreferredStorageDirectory() + KEYS_STRING).listFiles(
				(dir, name) -> name.endsWith(PK_EXTENSION));

		final var importedKeys =
				BrowserUtilities.browseMultiFiles(controller.getLastTransactionsDirectory(), controller.keysPane,
						"Keys");
		if (importedKeys == null) {
			return;
		}

		controller.setLastBrowsedDirectory(importedKeys.get(0));
		final List<File> publicKeys = new ArrayList<>();
		final List<File> privateKeys = new ArrayList<>();

		importedKeys.forEach(importedKey -> importSingleKey(publicKeys, privateKeys, importedKey));

		var counter = 0;

		assert pubKeys != null;
		logger.info("Importing public keys first");
		final Map<File, String> duplicates = new HashMap<>();
		for (final var publicKey : publicKeys) {
			counter += handlePublicKeys(duplicates, publicKey, checkIfDuplicate(publicKey, pubKeys));
		}

		assert pemKeys != null;
		logger.info("Importing private keys second");
		for (final var importedKey : privateKeys) {
			counter += handlePublicKeys(duplicates, importedKey, checkIfPEMDuplicate(importedKey, pemKeys));
		}

		counter += handleDuplicates(duplicates);

		if (counter > 0) {
			initializeKeysPane();
		}
	}

	private int handleDuplicates(final Map<File, String> duplicates) throws IOException {
		var popupResponse = ResponseEnum.UNKNOWN;
		var keepAsking = true;
		var counter = 0;
		if (duplicates.size() > 0) {
			for (final var entry : duplicates.entrySet()) {
				final var key = entry.getKey();
				final var value = entry.getValue();
				if (keepAsking) {
					popupResponse = ThreeButtonPopup.display(key, duplicates.size() > 1);
				}
				switch (popupResponse) {
					case KEEP_BOTH_ONCE:
						keepBoth(key, value);
						counter++;
						break;
					case IGNORE_ONCE:
						break;
					case REPLACE_ONCE:
						replaceOnce(key, value);
						counter++;
						break;
					case KEEP_BOTH_ALWAYS:
						keepBoth(key, value);
						keepAsking = false;
						counter++;
						break;
					case IGNORE_ALWAYS:
						keepAsking = false;
						break;
					case REPLACE_ALWAYS:
						replaceOnce(key, value);
						counter++;
						keepAsking = false;
						break;
					case UNKNOWN:
				}
			}
		}
		return counter;
	}

	private int handlePublicKeys(final Map<File, String> duplicates, final File publicKey,
			final String duplicate) throws IOException {
		if ("".equals(duplicate)) {
			copyFile(publicKey, new File(KEYS_FOLDER, publicKey.getName()));
			return 1;
		}
		duplicates.put(publicKey, duplicate);
		return 0;
	}

	private void importSingleKey(final List<File> publicKeys, final List<File> privateKeys, final File importedKey) {
		if (isPEMFile(importedKey.toPath())) {
			privateKeys.add(importedKey);
		}
		if (isPUBFile(importedKey.toPath())) {
			final var ext = importedKey.getAbsolutePath().endsWith(PUB_EXTENSION) ? PUB_EXTENSION : TXT_EXTENSION;

			final var pemFileName = importedKey.getName().replace(ext, PK_EXTENSION);

			if (new File(KEYS_FOLDER, pemFileName).exists()) {
				PopupMessage.display("Duplicate", DUPLICATED_KEY_NAME_MESSAGE);
				logger.info("Public key not imported because there is a pem file with the same name");
				return;
			}
			publicKeys.add(importedKey);
		}
	}

	private void replaceOnce(final File duplicate, final String oldFile) throws IOException {
		if (!duplicate.getName().equals(FilenameUtils.getName(oldFile))) {
			Files.deleteIfExists(Path.of(oldFile));
			logger.info("Old file {} deleted", oldFile);

			copyFile(duplicate,
					new File(
							controller.getPreferredStorageDirectory() + KEYS_STRING + duplicate.getName()));
		}
	}

	private void keepBoth(final File duplicate, final String duplicatePath) throws IOException {
		if (getBaseName(duplicatePath).equals(getBaseName(duplicate.getAbsolutePath()))) {
			final var newName = getBaseName(duplicate.getAbsolutePath()) + "_0";
			copyFile(duplicate,
					new File(String.format("%s/Keys/%s.%s", controller.getPreferredStorageDirectory(), newName,
							FilenameUtils.getExtension(duplicate.getAbsolutePath()))));
		} else {
			copyFile(duplicate, new File(
					String.format("%s/Keys/%s", controller.getPreferredStorageDirectory(),
							FilenameUtils.getName(duplicate.getAbsolutePath()))));
		}
	}

	private String getBaseName(final String absolutePath) {
		return FilenameUtils.getBaseName(absolutePath);
	}

	private String checkIfPEMDuplicate(final File pemFile, final File[] pemKeys) throws KeyStoreException {
		for (final var pemKey : pemKeys) {
			final var indexBoolean = getIndex(pemFile.getAbsolutePath()).equals(getIndex(pemKey.getAbsolutePath()));
			final var hashBoolean = Objects.equals(Ed25519KeyStore.getMnemonicHashCode(pemFile.getAbsolutePath()),
					Ed25519KeyStore.getMnemonicHashCode(pemKey.getAbsolutePath()));
			if (indexBoolean && hashBoolean) {
				logger.info("Found duplicated private key: PEMs {} and {} have the same mnemonic hash and index",
						pemFile.getAbsolutePath(), pemKey.getAbsolutePath());
				return pemKey.getAbsolutePath();
			}
		}
		return "";
	}

	private String checkIfDuplicate(final File publicKey, final File[] pubKeys) throws IOException {
		for (final var pubKey : pubKeys) {
			if (contentEquals(publicKey, pubKey)) {
				logger.info("Found duplicated public key: Contents of {} identical to {}",
						publicKey.getAbsolutePath(), pubKey.getAbsolutePath());
				return pubKey.getAbsolutePath();
			}
		}
		return "";
	}

	private void recoverIndexFieldListenerAction(final ObservableValue<? extends String> observable,
			final String oldValue,
			final String newValue) {
		if (!newValue.matches("\\d*")) {
			recoverIndexField.setText(newValue.replaceAll("[^\\d]", ""));
		}
	}

	private void recoverIndexFieldFocusedAction(final ObservableValue<? extends Boolean> observable,
			final Boolean oldValue,
			final Boolean newValue) {
		if (Boolean.FALSE.equals(newValue) && !recoverIndexField.getText().isEmpty()) {
			final var index = Integer.parseInt(recoverIndexField.getText());

			final List<String> values = new ArrayList<>();
			for (final var entry : indexMap.entrySet()) {
				if (entry.getValue() == index) {
					values.add(entry.getKey());
				}
			}

			// We will only populate the nickname field if the key index is not duplicated
			if (values.size() == 1 && recoverNicknameField.getText().isEmpty()) {
				recoverNicknameField.setText(values.get(0).replace("." + PK_EXTENSION, ""));
			}
		}
	}

	private void recoverNickNameFieldFocusedAction(final ObservableValue<? extends Boolean> observable,
			final Boolean oldValue,
			final Boolean newValue) {
		if (Boolean.FALSE.equals(newValue) &&
				!recoverNicknameField.getText().isEmpty() &&
				indexMap.containsKey(recoverNicknameField.getText() + "." + PK_EXTENSION) &&
				recoverIndexField.getText().isEmpty() &&
				indexMap.get(recoverNicknameField.getText() + "." + PK_EXTENSION) >= 0) {
			recoverIndexField.setText(
					indexMap.get(recoverNicknameField.getText() + "." + PK_EXTENSION).toString());
		}
	}

	private void recoverNicknameFieldKeyAction(final KeyEvent keyEvent) {
		if (!recoverNicknameField.getText().isEmpty() &&
				indexMap.containsKey(recoverNicknameField.getText() + PK_EXTENSION) &&
				recoverIndexField.getText().isEmpty() &&
				indexMap.get(recoverNicknameField.getText() + PK_EXTENSION) >= 0) {
			recoverIndexField.setText(indexMap.get(recoverNicknameField.getText() + PK_EXTENSION).toString());
		}
	}

	public void changePasswordAction() throws HederaClientException, BadMnemonicException {
		final var mnemonicFromBox = getMnemonicFromBox();
		if (mnemonicFromBox == null || mnemonicFromBox.size() < MNEMONIC_SIZE) {
			return;
		}

		final var password = NewPasswordPopup.display();
		if (password == null || password.length == 0) {
			return;
		}
		controller.setHash(password);
		final var salt = controller.getSalt();

		final Mnemonic mnemonic = Mnemonic.fromWords(mnemonicFromBox);
		final var passwordBytes = SecurityUtilities.keyFromPassword(password, salt);
		SecurityUtilities.toEncryptedFile(passwordBytes, DEFAULT_STORAGE + File.separator + MNEMONIC_PATH,
				mnemonic.toString());
		closeBoxes();

	}

	private List<String> getMnemonicFromBox() {
		if (phraseHBox.getChildren().size() != 1) {
			return new ArrayList<>();
		}
		final Label phrase = (Label) phraseHBox.getChildren().get(0);
		final var text = phrase.getText().toLowerCase(Locale.ROOT).split("[ \n]");
		return stream(text).filter(s -> !"".equals(s)).collect(Collectors.toCollection(ArrayList::new));
	}

	// endregion
}
