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

import com.codahale.passpol.BreachDatabase;
import com.codahale.passpol.PasswordPolicy;
import com.codahale.passpol.Status;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.Ed25519PrivateKey;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.popups.FinishBox;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.utilities.MnemonicPhraseHelper;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import net.i2p.crypto.eddsa.EdDSAPublicKey;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.checkPasswordPolicy;
import static java.lang.System.exit;
import static java.util.Arrays.stream;

public class RecoverPasswordPaneController {

	private static final Logger logger = LogManager.getLogger(RecoverPasswordPaneController.class);
	private static final int MIN_PASSWORD_LENGTH = 10;
	private static final int MAX_PASSWORD_LENGTH = 1024;

	Map<String, String> privateKeysMap = new HashMap<>();
	Map<Integer, String> indexMap = new HashMap<>();

	public ScrollPane recoverScrollPane;

	public Label recoverMnemonicErrorMessage;
	public Label recoverPasswordErrorLabel;
	public Label recoverCharacterCount;

	public GridPane recoverKeysGridPane;
	public GridPane recoverPasswordMainGridPane;
	public GridPane recoverKeysMainGridPane;
	public GridPane recoverKeysListGridPane;
	public GridPane recoverNoKeysMainGridPane;

	public PasswordField recoverAppPasswordField;
	public PasswordField recoverReEnterPasswordField;

	public ImageView recoverCheckPassword;
	public ImageView recoverReCheckPassword;

	public Button recoverPhraseButton;
	public Button recoverChangePasswordButton;
	public Button recoverFinishButton;
	public Button pasteFromClipBoardButton;

	public VBox recoverMnemonicPhraseVBbox;
	public VBox recoverPasswordVBox;
	public VBox recoverSelectedKeysVBox;
	public VBox recoverFinishVBox;
	public VBox recoverNoKeysVBox;
	public VBox recoverInnerVBox;
	public VBox recoverPhraseBox;

	@FXML
	private Controller controller;
	private char[] password;

	private MnemonicPhraseHelper mnemonicPhraseHelper;

	void injectMainController(Controller controller) {
		this.controller = controller;
	}

	public void initializeRecoveryPane() {
		mnemonicPhraseHelper = MnemonicPhraseHelper.Builder.aMnemonicPhraseHelper()
				.withMnemonicErrorMessage(recoverMnemonicErrorMessage)
				.withPhraseBox(recoverPhraseBox)
				.withStorageDirectory(controller.getPreferredStorageDirectory())
				.withPasteFromClipBoardButton(pasteFromClipBoardButton)
				.withGenerateKeys(recoverPhraseButton)
				.withFinishBox(recoverPasswordVBox)
				.build();

		controller.menuButtonBar.setVisible(false);
		initializeWordsVBox();
		initializeKeysVBox();
		initializePasswordVBox();
		recoverScrollPane.vvalueProperty().bind(recoverInnerVBox.heightProperty());

		recoverPhraseButton.setDisable(true);
		recoverMnemonicPhraseVBbox.setVisible(true);
		recoverPasswordVBox.setVisible(false);
		recoverSelectedKeysVBox.setVisible(false);
		recoverNoKeysVBox.setVisible(false);
		recoverFinishVBox.setVisible(false);

		recoverPasswordVBox.managedProperty().bind(recoverPasswordVBox.visibleProperty());
		recoverSelectedKeysVBox.managedProperty().bind(recoverSelectedKeysVBox.visibleProperty());
		recoverNoKeysVBox.managedProperty().bind(recoverNoKeysVBox.visibleProperty());
		recoverFinishVBox.managedProperty().bind(recoverFinishVBox.visibleProperty());
		pasteFromClipBoardButton.visibleProperty().bind(recoverPasswordVBox.visibleProperty().not());

		recoverPhraseButton.setOnKeyPressed(keyEvent -> getMnemonicWords());
	}

	public void pastePhraseFromClipBoard() {
		mnemonicPhraseHelper.pastePhraseFromClipBoard();
	}

	private void initializePasswordVBox() {
		var policy = new PasswordPolicy(BreachDatabase.top100K(), MIN_PASSWORD_LENGTH, MAX_PASSWORD_LENGTH);
		recoverAppPasswordField.setOnKeyReleased(keyEvent -> {
			final var length = recoverAppPasswordField.getText().length();
			recoverCharacterCount.setText(String.valueOf(length));
			recoverCharacterCount.setStyle(Constants.RED_STYLE);
			recoverCheckPassword.setVisible(false);
			if (Status.OK.equals(policy.check(recoverAppPasswordField.getText()))) {
				recoverCharacterCount.setStyle(Constants.GREEN_STYLE);
				recoverCheckPassword.setVisible(true);
				recoverPasswordErrorLabel.setVisible(false);
				recoverReEnterPasswordField.setDisable(false);
			}
			if (!keyEvent.getCode().equals(KeyCode.TAB) && !keyEvent.getCode().equals(KeyCode.ENTER)) {
				recoverPasswordErrorLabel.setVisible(false);
				return;
			}
			checkPasswordPolicy(policy, recoverAppPasswordField, recoverCheckPassword, recoverPasswordErrorLabel,
					recoverReEnterPasswordField);
		});


		recoverReEnterPasswordField.textProperty().addListener(((observableValue, s, t1) -> {
			if (t1.equals(recoverAppPasswordField.getText())) {
				recoverReCheckPassword.setVisible(true);
				recoverChangePasswordButton.setVisible(true);
			} else {
				recoverReCheckPassword.setVisible(false);
				recoverChangePasswordButton.setVisible(false);
			}
		}));

		recoverReEnterPasswordField.setOnKeyReleased(keyEvent -> {
			if ((keyEvent.getCode().equals(KeyCode.ENTER) || keyEvent.getCode().equals(
					KeyCode.TAB)) && recoverChangePasswordButton.isVisible()) {
				acceptPassword();
			}
		});
	}

	private void initializeKeysVBox() {
		populatePrivateKeysMap();
		if (privateKeysMap.size() == 0) {
			logger.info("No keys to recover");
		}


		List<String> keys = new ArrayList<>(privateKeysMap.keySet());
		Collections.sort(keys);

		for (var key : keys) {
			var index = getIndex(key);

			if (!indexMap.containsKey(index) && index != -1) {
				indexMap.put(index, FilenameUtils.removeExtension(key));
			} else {
				logger.info("We have found keys with duplicated indexes. Keys will not be changed");
				indexMap.clear();
				return;
			}

		}

		for (var key : keys) {
			var index = getIndex(key);
			if (index >= 0) {
				logger.info(String.format("%s -> %d", key, index));
				var row = recoverKeysListGridPane.getRowCount();
				recoverKeysListGridPane.add(new Label(FilenameUtils.removeExtension(key)), 0, row);
				recoverKeysListGridPane.add(new Label(Integer.toString(index)), 1, row);
			}
		}
	}

	private int getIndex(String key) {
		var index = 0;
		try {
			index = Ed25519KeyStore.getIndex(privateKeysMap.get(key));
		} catch (KeyStoreException e) {
			logger.error(String.format("Cannot read index from %s", privateKeysMap.get(key)));
			controller.displaySystemMessage(e);
			controller.displaySystemMessage(String.format("Cannot read index from %s", privateKeysMap.get(key)));
		}
		return index;
	}

	private void populatePrivateKeysMap() {
		privateKeysMap = new HashMap<>();
		try {
			var pKeys = new File(KEYS_FOLDER).listFiles((dir, name) -> name.endsWith(PK_EXTENSION));
			assert pKeys != null;
			stream(pKeys).forEachOrdered(pKey -> privateKeysMap.put(pKey.getName(), pKey.getAbsolutePath()));
		} catch (Exception ex) {
			logger.error(ex);
		}
	}

	private void initializeWordsVBox() {
		var mnemonicGridPane = new GridPane();
		mnemonicPhraseHelper.setupEmptyMnemonicBox(mnemonicGridPane);
		recoverPhraseBox.getChildren().add(mnemonicGridPane);
	}

	public void acceptPassword(){
		password = recoverAppPasswordField.getText().toCharArray();
		recoverChangePasswordButton.setVisible(false);
		recoverChangePasswordButton.setDisable(true);
		var filler = new char[password.length];
		Arrays.fill(filler, 'x');
		recoverAppPasswordField.clear();
		recoverReEnterPasswordField.clear();
		recoverAppPasswordField.setText(String.valueOf(filler));
		recoverReEnterPasswordField.setText(String.valueOf(filler));
		recoverAppPasswordField.setDisable(true);
		recoverReEnterPasswordField.setDisable(true);
		recoverChangePasswordButton.setDisable(true);
		controller.setLegacy(false);

		// Store the mnemonic and password hash
		Platform.runLater(() -> {
			recoverChangePasswordButton.setVisible(false);
			try {
				controller.setHash(password);
				mnemonicPhraseHelper.generatePassphraseEvent(password, controller.getSalt(), false);
			} catch (HederaClientException e) {
				logger.error(e.getMessage());
			}
		});


		// Show the next box and recover keys
		recoverFinishVBox.setVisible(true);

		if (privateKeysMap.size() != 0) {
			if (indexMap.size() > 0) {
				recoverSelectedKeysVBox.setVisible(true);
				if (indexMap.size() > 0) {
					for (int index : indexMap.keySet()) {
						recoverKey(index, indexMap.get(index));
					}
				}
			} else {
				recoverNoKeysVBox.setVisible(true);
			}
		}
	}

	public void finishSetup() {
		PopupMessage.display("Password Reset Finished",
				"The application will now close. Please restart the application and log in using your new password.",
				"CONTINUE");

		if (!controller.getSetupPhase().equals(SetupPhase.TEST_PHASE)) {
			controller.setSetupPhase(SetupPhase.NORMAL_OPERATION_PHASE);
			exit(0);
		}
	}

	private void recoverKey(int index, String nickname) {

		var answer = PopupMessage.display("Alert!",
				String.format(
						"This operation will overwrite the original private key password %s. Are you sure you want to" +
								" reset the password? This operation is irreversible.",
						nickname),
				true,
				"CONTINUE", "CANCEL");

		if (!answer) {
			return;
		}


		if (password == null) {
			return;
		}


		// Create key store for account
		try {
			var keyStoreName = generateAndStoreKeyPair(password, FilenameUtils.getBaseName(nickname), index);

			if ("".equals(keyStoreName)) {
				PopupMessage.display("Key Recovery Error", String.format(
						"The key recovery could not be verified using the public key. Key %s will not be recovered",
						nickname));

			} else {
				// Update the current index in preferences
				if (controller.getLastIndex() < index) {
					controller.setLastIndex(index + 1);
				}
				FinishBox.display(new File(keyStoreName), "Keys Recovered",
						"The private key has been recovered. It can be found at...");
			}

		} catch (Exception e) {
			logger.error(e);
			controller.displaySystemMessage(e);
		}

	}

	private String generateAndStoreKeyPair(char[] password, String nickname, int index) throws HederaClientException {
		var prefDir = controller.getPreferredStorageDirectory();
		var keysDir = prefDir + "/Keys/";

		// Create Keys/ if it doesn't exist
		if (new File(keysDir).mkdirs()) {
			logger.info("Keys folder has been created");
		}
		final Ed25519KeyStore keyStore;
		KeyPair keyPair;
		var keyStoreName = String.format("%s%s.pem", keysDir, nickname);

		try {

			keyStore = new Ed25519KeyStore.Builder().withPassword(password).build();
			var pk = Ed25519PrivateKey.fromMnemonic(mnemonicPhraseHelper.getMnemonic()).derive(index);
			keyPair = keyStore.insertNewKeyPair(pk);

			var pubName = keyStoreName.replace(".pem", ".pub");

			if (verifyWithPublicKey(pubName, (EdDSAPublicKey) keyPair.getPublic())) {
				keyStore.write(keyStoreName, "Transaction Tool UI", index, controller.getVersion(),
						mnemonicPhraseHelper.getMnemonic().words.hashCode());
			} else {
				return "";
			}

		} catch (KeyStoreException | IOException e) {
			logger.error(e);
			controller.displaySystemMessage(e);
			throw new HederaClientException(e);
		}
		return keyStoreName;
	}

	private boolean verifyWithPublicKey(String pubName, EdDSAPublicKey publicKey) throws IOException {
		var tempPub = System.getProperty("java.io.tmpdir") + "/tempPublic.pub";
		EncryptionUtils.storePubKey(tempPub, publicKey);

		return FileUtils.contentEquals(new File(pubName), new File(tempPub));
	}

	public void getMnemonicWords() {
		recoverPhraseButton.setVisible(false);
		recoverPasswordVBox.setVisible(true);
		recoverAppPasswordField.requestFocus();
	}

}
