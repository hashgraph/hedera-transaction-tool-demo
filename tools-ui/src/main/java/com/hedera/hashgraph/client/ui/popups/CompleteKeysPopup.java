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
import com.hedera.hashgraph.client.core.fileServices.FileAdapterFactory;
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
 * The last section will only be shown in the case the key has a PEM file. By default the private key will be hidden. If
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


	private static Boolean reloadTable = false;
	private static List<FileService> outputDirectories = new ArrayList<>();
	private static String publicKey = "";
	private static String privateKey = "";
	private static Label address = new Label();

	private static final Logger logger = LogManager.getLogger(CompleteKeysPopup.class);
	private static String keyName = "";


	public static Boolean display(String pubKeyAddress, boolean showNicknameEdit) {
		initializeOutputDirectories();
		publicKey = pubKeyAddress;
		privateKey = pubKeyAddress.replace(PUB_EXTENSION, PK_EXTENSION).replace(TXT_EXTENSION, PK_EXTENSION);
		keyName = FilenameUtils.getBaseName(publicKey);

		var pemExists = new File(privateKey).exists();

		var window = new Stage();

		window.setTitle("Key details");
		window.sizeToScene();
		window.setMaxWidth(500);
		window.initModality(Modality.APPLICATION_MODAL);

		var chars = new char[96];
		Arrays.fill(chars, '\u2022');


		TextArea publicKeyLabel = null;
		try {
			publicKeyLabel = new TextArea(new String(readAllBytes(Paths.get(pubKeyAddress))));
			publicKeyLabel.setWrapText(true);
			publicKeyLabel.setStyle(WHITE_WITH_BLUE_BORDER_STYLE);
			publicKeyLabel.setEditable(false);
			publicKeyLabel.setPrefRowCount(3);
		} catch (IOException e) {
			logger.error(e);
		}

		var nick = new TextField();
		nick.setStyle(WHITE_WITH_BLUE_BORDER_STYLE + " -fx-padding: 5,5,5,5");
		HBox.setHgrow(nick, Priority.ALWAYS);
		nick.setText(keyName);

		var privateKeyLabel = new TextArea(new String(chars));
		privateKeyLabel.setWrapText(true);
		privateKeyLabel.setStyle(WHITE_WITH_BLUE_BORDER_STYLE + " -fx-opacity: 1;");
		privateKeyLabel.setEditable(false);
		privateKeyLabel.setDisable(true);
		privateKeyLabel.setPrefRowCount(3);

		var layout = new VBox();

		// Buttons: Continue, export and view pk
		var continueButton = new Button("CLOSE");
		continueButton.setStyle(WHITE_BUTTON_STYLE);
		continueButton.setPrefWidth(200);
		continueButton.setOnAction(event -> window.close());

		var showPrivateButton = new Button("SHOW");
		showPrivateButton.setVisible(true);
		var hidePrivateButton = new Button("HIDE");
		hidePrivateButton.setVisible(false);

		showPrivateButton.setStyle(WHITE_BUTTON_STYLE);
		showPrivateButton.setMinWidth(110);
		showPrivateButton.setOnAction(actionEvent -> {
			var utility = new KeyPairUtility();
			var keyPair = utility.getKeyPairFromPEM(new File(privateKey),
					String.format("Please enter the password for key %s", keyName));
			privateKeyLabel.setText(Hex.toHexString(keyPair.getPrivate().getEncoded()));
			privateKeyLabel.setDisable(false);
			showPrivateButton.setVisible(false);
			hidePrivateButton.setVisible(true);
		});
		showPrivateButton.managedProperty().bind(showPrivateButton.visibleProperty());

		hidePrivateButton.setStyle(WHITE_BUTTON_STYLE);
		hidePrivateButton.setMinWidth(110);
		hidePrivateButton.setOnAction(actionEvent -> {
			privateKeyLabel.setText(new String(chars));
			privateKeyLabel.setDisable(true);
			showPrivateButton.setVisible(true);
			hidePrivateButton.setVisible(false);
		});
		hidePrivateButton.managedProperty().bind(hidePrivateButton.visibleProperty());

		var updateButton = new Button("CHANGE");
		updateButton.setStyle(WHITE_BUTTON_STYLE);
		updateButton.setMinWidth(110);
		updateButton.setOnAction(event -> updateNickname(nick.getText()));
		updateButton.setVisible(showNicknameEdit);

		var buttonBox = new VBox();
		buttonBox.getChildren().addAll(showPrivateButton, hidePrivateButton);

		var menuButton = new MenuButton("EXPORT");
		menuButton.setStyle(WHITE_BUTTON_STYLE);
		menuButton.setMinWidth(110);
		initializeExportPublicKeysMenuButton(menuButton, layout);

		var nicknameBox = new HBox();
		nicknameBox.getChildren().addAll(nick, updateButton);
		nicknameBox.setSpacing(10);
		nicknameBox.setPrefWidth(450);

		var publicKeyBox = new HBox();
		publicKeyBox.getChildren().addAll(publicKeyLabel, menuButton);
		publicKeyBox.setSpacing(10);
		publicKeyBox.setPrefWidth(450);

		var privateKeyBox = new HBox();
		privateKeyBox.getChildren().addAll(privateKeyLabel, buttonBox);
		privateKeyBox.setSpacing(10);
		privateKeyBox.setPrefWidth(450);

		var nickTitle = new Label(NICKNAME_EXPLANATION);
		nickTitle.setStyle(FX_FONT_SIZE);
		nickTitle.setMaxWidth(450);
		nickTitle.setWrapText(true);

		var publicKeyTitle = new Label(PUBLIC_KEY_EXPLANATION);
		publicKeyTitle.setStyle(FX_FONT_SIZE);
		publicKeyTitle.setMaxWidth(450);
		publicKeyTitle.setWrapText(true);

		var privateKeyTitle = new Label(PRIVATE_KEY_EXPLANATION);
		privateKeyTitle.setStyle(FX_FONT_SIZE);
		privateKeyTitle.setMaxWidth(450);
		privateKeyTitle.setWrapText(true);


		var indexLabel = new Label();
		try {
			if (pemExists) {
				var index = (Ed25519KeyStore.getIndex(privateKey)) < 0 ? "not available" : String.valueOf(
						Ed25519KeyStore.getIndex(privateKey));
				indexLabel.setText(String.format("Index: %s", index));
			}
		} catch (KeyStoreException e) {
			logger.error(e);
		}

		var nickNameVBox = new VBox();
		nickNameVBox.getChildren().addAll(nickTitle, nicknameBox);
		nickNameVBox.setAlignment(Pos.CENTER_LEFT);
		nickNameVBox.setSpacing(5);

		var publicKeyVBox = new VBox();
		publicKeyVBox.getChildren().addAll(publicKeyTitle, publicKeyBox);
		publicKeyVBox.setAlignment(Pos.CENTER_LEFT);
		publicKeyVBox.setSpacing(5);

		var privateKeyVBox = new VBox();
		privateKeyVBox.getChildren().addAll(privateKeyTitle, indexLabel, privateKeyBox);
		privateKeyVBox.setVisible(pemExists);
		privateKeyVBox.managedProperty().bind(privateKeyVBox.visibleProperty());
		privateKeyVBox.setAlignment(Pos.CENTER_LEFT);
		privateKeyVBox.setSpacing(5);

		address = new Label(pubKeyAddress);
		address.setMaxWidth(450);
		address.setWrapText(true);

		layout.getChildren().addAll(nickNameVBox, address, publicKeyVBox, privateKeyVBox, continueButton);
		layout.setSpacing(20);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setAlignment(Pos.CENTER);


		layout.setStyle("-fx-font-size: 14");

		var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();

		return reloadTable;
	}

	private static void updateNickname(String text) {
		if (text.equals(keyName)) {
			reloadTable = false;
		} else {
			var oldPem = privateKey;
			var oldPub = publicKey;
			var newPem = privateKey.replace(keyName, text);
			var newPub = publicKey.replace(keyName, text);
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
				} catch (IOException e) {
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

	private static void initializeExportPublicKeysMenuButton(MenuButton menuButton, Pane pane) {

		menuButton.getItems().clear();
		// setup button text
		if (outputDirectories != null) {
			for (var s :
					outputDirectories) {
				var menuItem =
						new MenuItem(s.getPath().replace(System.getProperty("user.home") + File.separator, ""));
				menuItem.setOnAction(actionEvent -> exportKeysToFileService(s));
				menuButton.getItems().add(menuItem);
			}
		} else {
			menuButton.setDisable(true);
		}

		var menuItem = new MenuItem("browse for directory");
		menuItem.setOnAction(actionEvent -> {
			FileService fs = null;
			try {
				var s =
						browseDirectories(FileSystemView.getFileSystemView().getDefaultDirectory().getPath(), pane);
				if (!"".equals(s)) {
					fs = FileAdapterFactory.getAdapter(s);
				} else {
					properties.setLastBrowsedDirectory(new File(s));
				}

			} catch (Exception e) {
				logger.error(e);

			}
			if (fs != null) {
				exportKeysToFileService(fs);
			}
		});
		menuButton.getItems().add(menuItem);
	}

	private static void exportKeysToFileService(FileService fs) {
		var path = publicKey;
		assert new File(path).exists();
		try {
			if (fs.exists("/OutputFiles")) {
				var user = properties.getOneDriveCredentials().get(fs.getPath());
				var remote = "/OutputFiles/" + ((fs.getPath().contains("Volumes")) ? "" : user);
				fs.upload(path, remote);
				logger.info(String.format("Key %s uploaded to %s", keyName, remote));
			} else {
				fs.upload(path, "/");
				logger.info(String.format("Key %s uploaded", keyName));
			}
		} catch (HederaClientException e) {
			logger.error(e);
		}
	}

	private static void initializeOutputDirectories() {
		try {
			if (properties.getOneDriveCredentials() != null) {
				var inputs = properties.getOneDriveCredentials().keySet();
				outputDirectories = new ArrayList<>();
				for (var s :
						inputs) {
					var fs = FileAdapterFactory.getAdapter(s);
					assert fs != null;
					if (fs.exists()) {
						outputDirectories.add(fs);
					}
				}
			}
		} catch (HederaClientException e) {
			logger.error(e);
		}
	}
}
