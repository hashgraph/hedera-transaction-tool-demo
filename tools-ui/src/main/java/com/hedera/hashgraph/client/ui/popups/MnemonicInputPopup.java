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

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.utilities.MnemonicPhraseHelper;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.BadMnemonicException;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.keyFromPassword;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.toEncryptedFile;

public class MnemonicInputPopup {
	public static final String ERROR_MESSAGE =
			"The recovery phrase entered is incorrect. Please check your recovery phrase and try again.";
	public static final String MESSAGE_LABEL =
			"Please use the spaces below to enter your recovery phrase. The recovery phrase is the list of 24 words" +
					" that was created during the application initial setup.";
	private static MnemonicPhraseHelper mnemonicPhraseHelper;

	private static final Logger logger = LogManager.getLogger(MnemonicInputPopup.class);

	private static char[] answer;

	public static char[] display(String storageDirectory) {
		var window = new Stage();
		window.setTitle("Recovery phrase reset");
		window.sizeToScene();
		window.initModality(Modality.APPLICATION_MODAL);

		Label recoverMnemonicErrorMessage = new Label(ERROR_MESSAGE);
		recoverMnemonicErrorMessage.setStyle("-fx-text-fill: red");
		recoverMnemonicErrorMessage.setVisible(false);

		Label explanation = new Label(MESSAGE_LABEL);
		explanation.setMaxWidth(450);
		explanation.setWrapText(true);

		VBox recoverPhraseBox = new VBox();
		VBox recoverPasswordVBox = new VBox();

		Button recoverPhraseButton = new Button("RECOVER");
		recoverPhraseButton.setStyle(Constants.WHITE_BUTTON_STYLE);
		recoverPhraseButton.setVisible(false);
		recoverPhraseButton.managedProperty().bind(recoverPhraseButton.visibleProperty());

		Button pasteFromClipboard = new Button("PASTE");
		pasteFromClipboard.setStyle(Constants.WHITE_BUTTON_STYLE);
		pasteFromClipboard.setOnAction(actionEvent -> pastePhraseFromClipBoard());

		Button cancelButton = new Button("CANCEL");
		cancelButton.setStyle(Constants.BLUE_BUTTON_STYLE);
		cancelButton.setOnAction(actionEvent -> window.close());


		HBox buttonBox = new HBox();
		pasteFromClipboard.setPrefWidth(150);
		recoverPhraseButton.setPrefWidth(150);
		cancelButton.setPrefWidth(150);
		buttonBox.getChildren().addAll(pasteFromClipboard, recoverPhraseButton, cancelButton);
		buttonBox.setSpacing(20);
		buttonBox.setAlignment(Pos.CENTER);

		mnemonicPhraseHelper = MnemonicPhraseHelper.Builder.aMnemonicPhraseHelper()
				.withMnemonicErrorMessage(recoverMnemonicErrorMessage)
				.withPhraseBox(recoverPhraseBox)
				.withStorageDirectory(storageDirectory)
				.withGenerateKeys(recoverPhraseButton)
				.withFinishBox(recoverPasswordVBox)
				.build();

		var mnemonicGridPane = new GridPane();
		mnemonicPhraseHelper.setupEmptyMnemonicBox(mnemonicGridPane);
		recoverPhraseBox.getChildren().add(mnemonicGridPane);

		VBox layout = new VBox();
		layout.setPadding(new Insets(20, 20, 20, 20));

		layout.setAlignment(Pos.CENTER);
		layout.setSpacing(20);

		layout.setStyle("-fx-font-size: 14");
		layout.getChildren().addAll(explanation, recoverPhraseBox, recoverMnemonicErrorMessage, buttonBox);

		recoverPhraseButton.setOnAction(
				actionEvent -> recoverEvent(storageDirectory, window, recoverMnemonicErrorMessage));

		var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		window.showAndWait();
		return answer;
	}

	private static void recoverEvent(String storageDirectory, Stage window, Label recoverMnemonicErrorMessage) {
		var password = NewPasswordPopup.display();
		try {
			setPassword(password);
		} catch (HederaClientException e) {
			logger.error(e.getMessage());
		}
		var words = mnemonicPhraseHelper.getWordsFromGridPane();

		if (words.size() < Constants.MNEMONIC_SIZE) {
			recoverMnemonicErrorMessage.setVisible(true);
			return;
		}

		try {
			Mnemonic mnemonic = Mnemonic.fromWords(words);
			recoverMnemonicErrorMessage.setVisible(false);
			var properties = new UserAccessibleProperties(storageDirectory + "/Files/user.properties", "");
			properties.setMnemonicHashCode(mnemonic.words.hashCode());
			var passwordBytes = keyFromPassword(password, getSalt());
			toEncryptedFile(passwordBytes, storageDirectory + File.separator + Constants.MNEMONIC_PATH,
					mnemonic.toString());
			window.close();
		} catch (BadMnemonicException | HederaClientException e) {
			logger.error(e);
			recoverMnemonicErrorMessage.setVisible(true);
		}
	}

	public static void pastePhraseFromClipBoard() {
		mnemonicPhraseHelper.pastePhraseFromClipBoard();
	}

	private static byte[] getSalt() {
		UserAccessibleProperties properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");
		properties.setLegacy(false);
		return Utilities.getSaltBytes(properties);
	}

	private static void setPassword(char[] password) throws HederaClientException {
		UserAccessibleProperties properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");
		properties.setHash(password);
		answer = password;
	}
}
