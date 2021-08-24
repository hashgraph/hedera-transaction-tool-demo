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

import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
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
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static com.hedera.hashgraph.client.ui.utilities.Utilities.checkPasswordPolicy;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.clearPasswordFields;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.setupCharacterCount;
import static java.lang.System.exit;

public class RecoverPasswordPaneController {

	private static final Logger logger = LogManager.getLogger(RecoverPasswordPaneController.class);

	public ScrollPane recoverScrollPane;

	public Label recoverMnemonicErrorMessage;
	public Label recoverPasswordErrorLabel;
	public Label recoverCharacterCount;

	public GridPane recoverKeysGridPane;
	public GridPane recoverPasswordMainGridPane;

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
	public VBox recoverFinishVBox;
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
				.withGenerateKeys(recoverPhraseButton)
				.withFinishBox(recoverPasswordVBox)
				.build();

		controller.menuButtonBar.setVisible(false);
		initializeWordsVBox();
		initializePasswordVBox();
		recoverScrollPane.vvalueProperty().bind(recoverInnerVBox.heightProperty());

		recoverChangePasswordButton.setOnAction(actionEvent -> acceptPassword());

		recoverPhraseButton.setDisable(true);
		recoverMnemonicPhraseVBbox.setVisible(true);
		recoverPasswordVBox.setVisible(false);
		recoverFinishVBox.setVisible(false);

		recoverPasswordVBox.managedProperty().bind(recoverPasswordVBox.visibleProperty());
		recoverFinishVBox.managedProperty().bind(recoverFinishVBox.visibleProperty());
		pasteFromClipBoardButton.visibleProperty().bind(recoverPasswordVBox.visibleProperty().not());

		recoverPhraseButton.setOnKeyPressed(keyEvent -> getMnemonicWords());
	}

	public void pastePhraseFromClipBoard() {
		mnemonicPhraseHelper.pastePhraseFromClipBoard();
	}

	private void initializePasswordVBox() {
		recoverAppPasswordField.setOnKeyReleased(keyEvent -> {
			recoverReCheckPassword.setVisible(false);
			recoverReEnterPasswordField.setText("");
			setupCharacterCount(recoverAppPasswordField, recoverCharacterCount, recoverCheckPassword,
					recoverPasswordErrorLabel, recoverReEnterPasswordField);
			if (!keyEvent.getCode().equals(KeyCode.TAB) && !keyEvent.getCode().equals(KeyCode.ENTER)) {
				recoverPasswordErrorLabel.setVisible(false);
				return;
			}
			checkPasswordPolicy(recoverAppPasswordField, recoverCheckPassword, recoverPasswordErrorLabel,
					recoverReEnterPasswordField);
		});

		recoverReEnterPasswordField.textProperty().addListener(
				((observableValue, s, t1) -> recoverReCheckPassword.setVisible(
						t1.equals(recoverAppPasswordField.getText()))));

		recoverReEnterPasswordField.setOnKeyReleased(keyEvent -> {
			if ((keyEvent.getCode().equals(KeyCode.ENTER) || keyEvent.getCode().equals(
					KeyCode.TAB)) && recoverChangePasswordButton.isVisible()) {
				acceptPassword();
			}
		});

		recoverChangePasswordButton.visibleProperty().bind(
				recoverCheckPassword.visibleProperty().and(recoverReCheckPassword.visibleProperty()));
	}

	private void initializeWordsVBox() {
		var mnemonicGridPane = new GridPane();
		mnemonicPhraseHelper.setupEmptyMnemonicBox(mnemonicGridPane);
		recoverPhraseBox.getChildren().add(mnemonicGridPane);
	}

	public void acceptPassword() {
		password = recoverAppPasswordField.getText().toCharArray();
		clearPasswordFields(recoverChangePasswordButton, password, recoverAppPasswordField, recoverReEnterPasswordField);
		recoverChangePasswordButton.setDisable(true);
		controller.setLegacy(false);

		// Store the mnemonic and password hash
		Platform.runLater(() -> {
			try {
				controller.setHash(password);
				mnemonicPhraseHelper.generatePassphraseEvent(password, controller.getSalt(), false);
			} catch (HederaClientException e) {
				logger.error(e.getMessage());
			}
		});

		// Show the next box and recover keys
		recoverFinishVBox.setVisible(true);
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

	public void getMnemonicWords() {
		recoverPhraseButton.setVisible(false);
		recoverPasswordVBox.setVisible(true);
		recoverAppPasswordField.requestFocus();
	}

}
