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

package com.hedera.hashgraph.client.ui.popups;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.fileservices.FileAdapterFactory;
import com.hedera.hashgraph.client.core.interfaces.FileService;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.ui.utilities.KeyPairUtility;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;

import javax.swing.filechooser.FileSystemView;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.utils.BrowserUtilities.browseDirectories;
import static java.nio.file.Files.readAllBytes;

/**
 * This class introduces a popup with three distinct sections, where details of a key are displayed.
 * The first section shows the key's nickname and gives the user the option of changing the nickname (button).
 * The second section shows the public key, and allows the user to export it to one of the remote drives (or a place of
 * the user's choosing).
 * The last section will only be shown in the case the key has a PEM file. By default, the private key will be hidden. If
 * the user presses the "SHOW" button, a second popup will appear asking for the user's password to decrypt the PEM and
 * show it in the window.
 */
public class CompleteKeysPopup {
	private static final String NICKNAME_EXPLANATION = "Key Name";
	private static final String PUBLIC_KEY_EXPLANATION = "Public Key";
	private static final String PRIVATE_KEY_EXPLANATION = "Private Key";
	private static final UserAccessibleProperties properties =
			new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");
	private static final String WHITE_WITH_BLUE_BORDER_STYLE = "-fx-background-color: white; -fx-border-color: " +
			"#0b9dfd;";
	private static final String WHITE_BUTTON_STYLE = WHITE_WITH_BLUE_BORDER_STYLE +
			" -fx-border-radius: 10; -fx-background-radius: 10;";
	private static final String FX_FONT_SIZE = "-fx-font-size: 16";
	public static final String ASSOCIATED_ACCOUNTS = "Associated accounts";
	public static final String NO_ACCOUNT_FOUND_TEXT = "No account found";

	private static Boolean reloadTable = false;
	private static List<FileService> outputDirectories = new ArrayList<>();
	private static String publicKey = "";
	private static String privateKey = "";
	private static Label address = new Label();

	private static final Logger logger = LogManager.getLogger(CompleteKeysPopup.class);
	private static String keyName = "";

	private CompleteKeysPopup() {
		throw new IllegalStateException("Utility class");
	}

	public static Boolean display(final String pubKeyAddres, final boolean showNicknameEdit) {
		return display(pubKeyAddres, "", showNicknameEdit);
	}

	public static Boolean display(final String pubKeyAddress, final String accounts, final boolean showNicknameEdit) {
		initializeOutputDirectories();
		publicKey = pubKeyAddress;
		privateKey = pubKeyAddress.replace(PUB_EXTENSION, PK_EXTENSION).replace(TXT_EXTENSION, PK_EXTENSION);
		keyName = FilenameUtils.getBaseName(publicKey);

		final var pemExists = new File(privateKey).exists();

		final var window = new Stage();

		window.setTitle("Key details");
		window.sizeToScene();
		window.setMaxWidth(600);
		window.initModality(Modality.APPLICATION_MODAL);

		final var chars = new char[96];
		Arrays.fill(chars, '\u2022');


		TextArea publicKeyLabel = null;
		try {
			publicKeyLabel = new TextArea(new String(readAllBytes(Paths.get(pubKeyAddress))));
			publicKeyLabel.setWrapText(true);
			publicKeyLabel.setStyle(WHITE_WITH_BLUE_BORDER_STYLE);
			publicKeyLabel.setEditable(false);
			publicKeyLabel.setPrefRowCount(3);
		} catch (final IOException e) {
			logger.error(e);
		}

		final var nick = new TextField();
		nick.setStyle(WHITE_WITH_BLUE_BORDER_STYLE + " -fx-padding: 5,5,5,5");
		HBox.setHgrow(nick, Priority.ALWAYS);
		nick.setText(keyName);

		final var privateKeyLabel = new TextArea(new String(chars));
		privateKeyLabel.setWrapText(true);
		privateKeyLabel.setStyle(WHITE_WITH_BLUE_BORDER_STYLE + " -fx-opacity: 1;");
		privateKeyLabel.setEditable(false);
		privateKeyLabel.setDisable(true);
		privateKeyLabel.setPrefRowCount(3);

		final var layout = new VBox();

		// Buttons: Continue, export and view pk
		final var continueButton = new Button("CLOSE");
		continueButton.setStyle(WHITE_BUTTON_STYLE);
		continueButton.setPrefWidth(200);
		continueButton.setOnAction(event -> window.close());

		final var showPrivateButton = new Button("SHOW");
		showPrivateButton.setVisible(true);
		final var hidePrivateButton = new Button("HIDE");
		hidePrivateButton.setVisible(false);
		final var changePasswordButton = new Button("CHANGE PASSWORD");

		showPrivateButton.setStyle(WHITE_BUTTON_STYLE);
		showPrivateButton.setMinWidth(200);
		showPrivateButton.setOnAction(actionEvent -> {
			final var utility = new KeyPairUtility();
			final var keyPair = utility.getKeyPairFromPEM(new File(privateKey),
					String.format("Please enter the password for key %s", keyName));
			if (keyPair != null) {
				privateKeyLabel.setText(Hex.toHexString(keyPair.getPrivate().getEncoded()));
				privateKeyLabel.setDisable(false);
				showPrivateButton.setVisible(false);
				hidePrivateButton.setVisible(true);
			}
		});
		showPrivateButton.managedProperty().bind(showPrivateButton.visibleProperty());

		hidePrivateButton.setStyle(WHITE_BUTTON_STYLE);
		hidePrivateButton.setMinWidth(200);
		hidePrivateButton.setOnAction(actionEvent -> {
			privateKeyLabel.setText(new String(chars));
			privateKeyLabel.setDisable(true);
			showPrivateButton.setVisible(true);
			hidePrivateButton.setVisible(false);
		});
		hidePrivateButton.managedProperty().bind(hidePrivateButton.visibleProperty());
		showPrivateButton.setMinWidth(200);

		changePasswordButton.setStyle(WHITE_BUTTON_STYLE);
		changePasswordButton.setMinWidth(200);
		changePasswordButton.setOnAction(actionEvent -> {
			final var answer =
					PopupMessage.display("Change password", "This will change the key's password.", true, "CONTINUE",
							"DECLINE");
			if (Boolean.TRUE.equals(answer)) {
				changeKeyPassword();

			}
		});

		final var updateButton = new Button("CHANGE");
		updateButton.setStyle(WHITE_BUTTON_STYLE);
		updateButton.setMinWidth(200);
		updateButton.setOnAction(event -> updateNickname(nick.getText()));
		updateButton.setVisible(showNicknameEdit);

		final var buttonBox = new VBox();
		buttonBox.getChildren().addAll(showPrivateButton, hidePrivateButton, changePasswordButton);
		buttonBox.setSpacing(5);

		final var menuButton = new MenuButton("EXPORT");
		menuButton.setAlignment(Pos.CENTER);
		menuButton.setStyle(WHITE_BUTTON_STYLE);
		menuButton.setMinWidth(200);
		initializeExportPublicKeysMenuButton(menuButton, layout);

		final var nicknameBox = new HBox();
		nicknameBox.getChildren().addAll(nick, updateButton);
		nicknameBox.setSpacing(10);
		nicknameBox.setMinWidth(500);
		nicknameBox.setMaxWidth(500);

		final var publicKeyBox = new HBox();
		publicKeyBox.getChildren().addAll(publicKeyLabel, menuButton);
		publicKeyBox.setSpacing(10);
		publicKeyBox.setMinWidth(500);
		publicKeyBox.setMaxWidth(500);

		final var privateKeyBox = new HBox();
		privateKeyBox.getChildren().addAll(privateKeyLabel, buttonBox);
		privateKeyBox.setSpacing(10);
		privateKeyBox.setMinWidth(500);
		privateKeyBox.setMaxWidth(500);

		final var nickTitle = new Label(NICKNAME_EXPLANATION);
		nickTitle.setStyle(FX_FONT_SIZE);
		nickTitle.setMaxWidth(500);
		nickTitle.setWrapText(true);

		final var publicKeyTitle = new Label(PUBLIC_KEY_EXPLANATION);
		publicKeyTitle.setStyle(FX_FONT_SIZE);
		publicKeyTitle.setMaxWidth(500);
		publicKeyTitle.setWrapText(true);

		final var privateKeyTitle = new Label(PRIVATE_KEY_EXPLANATION);
		privateKeyTitle.setStyle(FX_FONT_SIZE);
		privateKeyTitle.setMaxWidth(500);
		privateKeyTitle.setWrapText(true);


		final var indexLabel = new Label();
		try {
			if (pemExists) {
				final var index = (Ed25519KeyStore.getIndex(privateKey)) < 0 ? "not available" : String.valueOf(
						Ed25519KeyStore.getIndex(privateKey));
				indexLabel.setText(String.format("Index: %s", index));
			}
		} catch (final KeyStoreException e) {
			logger.error(e);
		}

		final var nickNameVBox = new VBox();
		nickNameVBox.getChildren().addAll(nickTitle, nicknameBox);
		nickNameVBox.setAlignment(Pos.CENTER_LEFT);
		nickNameVBox.setSpacing(5);

		final var publicKeyVBox = new VBox();
		publicKeyVBox.getChildren().addAll(publicKeyTitle, publicKeyBox);
		publicKeyVBox.setAlignment(Pos.CENTER_LEFT);
		publicKeyVBox.setSpacing(5);

		final var privateKeyVBox = new VBox();
		privateKeyVBox.getChildren().addAll(privateKeyTitle, indexLabel, privateKeyBox);
		privateKeyVBox.setVisible(pemExists);
		privateKeyVBox.managedProperty().bind(privateKeyVBox.visibleProperty());
		privateKeyVBox.setAlignment(Pos.CENTER_LEFT);
		privateKeyVBox.setSpacing(5);

		address = new Label(pubKeyAddress);
		address.setMaxWidth(500);
		address.setWrapText(true);

		final var accountsText = new TextArea(accounts);
		accountsText.setWrapText(true);
		accountsText.setStyle(WHITE_WITH_BLUE_BORDER_STYLE + " -fx-opacity: 1;");
		accountsText.setEditable(false);
		accountsText.setDisable(true);
		accountsText.setPrefRowCount(3);

		final var accountsBox = new HBox();
		accountsBox.getChildren().add(accountsText);
		accountsBox.setSpacing(10);
		accountsBox.setMinWidth(500);
		accountsBox.setMaxWidth(500);

		final var accountsTitle = new Label(ASSOCIATED_ACCOUNTS);
		accountsTitle.setStyle(FX_FONT_SIZE);
		accountsTitle.setMaxWidth(500);
		accountsTitle.setWrapText(true);


		final var associatedAccountsVBox = new VBox();
		associatedAccountsVBox.getChildren().addAll(accountsTitle, accountsBox);
		associatedAccountsVBox.setAlignment(Pos.CENTER_LEFT);
		associatedAccountsVBox.setSpacing(5);
		associatedAccountsVBox.setVisible(!("".equals(accounts) || NO_ACCOUNT_FOUND_TEXT.equals(accounts)));
		associatedAccountsVBox.managedProperty().bind(associatedAccountsVBox.visibleProperty());


		layout.getChildren().addAll(nickNameVBox, address, publicKeyVBox, privateKeyVBox, associatedAccountsVBox,
				continueButton);
		layout.setSpacing(20);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setAlignment(Pos.CENTER);


		layout.setStyle("-fx-font-size: 14");

		final var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();

		return reloadTable;
	}

	private static void changeKeyPassword() {
		logger.info("Changing the password for key \"{}\"", privateKey);
		final var utility = new KeyPairUtility();
		final var keyPair = utility.getKeyPairFromPEM(new File(privateKey),
				String.format("Please enter the password for key %s", keyName));
		if (keyPair == null) {
			return;
		}
		final char[] password = NewPasswordPopup.display();

		if (password == null || Arrays.equals(password, new char[0])) {
			PopupMessage.display("Password",
					String.format("The password for %s has not been changed", FilenameUtils.getBaseName(privateKey)));
			return;
		}

		try {
			final Ed25519KeyStore store = new Ed25519KeyStore.Builder().withPassword(password).build();
			final var index = Ed25519KeyStore.getIndex(privateKey);
			final var version = Ed25519KeyStore.getVersion(privateKey);
			final var hashCode = Ed25519KeyStore.getMnemonicHashCode(privateKey);
			store.add(keyPair);
			store.write(privateKey, "Transaction Tool UI", index, version, hashCode);
			PopupMessage.display("Password",
					String.format("The password for %s has been changed", FilenameUtils.getBaseName(privateKey)));
		} catch (final KeyStoreException e) {
			logger.error(e.getMessage());
		}
	}

	private static void updateNickname(final String text) {
		if (text.equals(keyName)) {
			reloadTable = false;
		} else {
			final var oldPem = privateKey;
			final var oldPub = publicKey;
			final var newPem = privateKey.replace(keyName, text);
			final var newPub = publicKey.replace(keyName, text);
			if (new File(newPem).exists() || new File(newPub).exists()) {
				reloadTable = false;
				PopupMessage.display("Duplicated key name",
						String.format("The chosen nickname, %s, already exists. Nickname will not be updated", text));
			} else {
				try {
					if (new File(oldPem).exists()) {
						FileUtils.moveFile(new File(oldPem), new File(newPem));
					}
					if (new File(oldPub).exists()) {
						FileUtils.moveFile(new File(oldPub), new File(newPub));
					}
					privateKey = newPem;
					publicKey = newPub;
					keyName = text;
					PopupMessage.display("Key name updated",
							String.format("The nickname for this key has been updated to: %s", text));
					reloadTable = true;
					address.setText(publicKey);
				} catch (final IOException e) {
					logger.error(e);
					PopupMessage.display("Error updating nickname",
							String.format(
									"The nickname for this key could not be updated to: %s. See the error log for " +
											"details",
									text));
					reloadTable = false;
				}
			}
		}
	}

	private static void initializeExportPublicKeysMenuButton(final MenuButton menuButton, final Pane pane) {

		menuButton.getItems().clear();
		// setup button text
		if (outputDirectories != null) {
			for (final var s :
					outputDirectories) {
				final var menuItem =
						new MenuItem(s.getPath().replace(System.getProperty("user.home") + File.separator, ""));
				menuItem.setOnAction(actionEvent -> exportKeysToFileService(s));
				menuButton.getItems().add(menuItem);
			}
		} else {
			menuButton.setDisable(true);
		}

		final var menuItem = new MenuItem("browse for directory");
		menuItem.setOnAction(actionEvent -> {
			FileService fs = null;
			try {
				final var s =
						browseDirectories(FileSystemView.getFileSystemView().getDefaultDirectory().getPath(), pane);
				if (!"".equals(s)) {
					fs = FileAdapterFactory.getAdapter(s);
				} else {
					properties.setLastBrowsedDirectory(new File(s));
				}

			} catch (final Exception e) {
				logger.error(e);

			}
			if (fs != null) {
				exportKeysToFileService(fs);
			}
		});
		menuButton.getItems().add(menuItem);
	}

	private static void exportKeysToFileService(final FileService fs) {
		final var path = publicKey;
		assert new File(path).exists();
		try {
			if (fs.exists("/OutputFiles")) {
				final var user = properties.getOneDriveCredentials().get(fs.getPath());
				final var remote = "/OutputFiles/" + ((fs.getPath().contains("Volumes")) ? "" : user);
				fs.upload(path, remote);
				logger.info("Key {} uploaded to {}", keyName, remote);
			} else {
				fs.upload(path, "/");
				logger.info("Key {} uploaded", keyName);
			}
		} catch (final HederaClientException e) {
			logger.error(e);
		}
	}

	private static void initializeOutputDirectories() {
		try {
			if (properties.getOneDriveCredentials() != null) {
				final var inputs = properties.getOneDriveCredentials().keySet();
				outputDirectories = new ArrayList<>();
				for (final var s :
						inputs) {
					final var fs = FileAdapterFactory.getAdapter(s);
					assert fs != null;
					if (fs.exists()) {
						outputDirectories.add(fs);
					}
				}
			}
		} catch (final HederaClientException e) {
			logger.error(e);
		}
	}
}
