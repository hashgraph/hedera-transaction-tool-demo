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
import com.hedera.hashgraph.client.ui.utilities.KeyPairUtility;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.security.KeyStoreException;
import java.util.Arrays;

public class PasswordBox {
	private static final Logger logger = LogManager.getLogger(PasswordBox.class);
	private static final String WARNING_MESSAGE =
			"This process will change your password. You will need to have your recovery phrase at hand to enter it at " +
					"the prompt. Please be advised that the Keys that were generated and stored with the old password " +
					"will have to be recovered. Please press the \"Recover Key\" button in the \"Accounts and Keys\" " +
					"page to change the password on your Keys";
	public static final String NO_STRING = "CANCEL";
	public static final String RESET = "RESET";
	public static final String RESET_PASSWORD_TITLE = "Reset Password";
	private static char[] answer;


	private PasswordBox() {
		throw new IllegalStateException("Utility class");
	}

	public static char[] display(String title, String message, String pemFile, boolean isKey) {
		var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle(title);

		window.sizeToScene();
		window.setMaxWidth(400);

		var label = new Label();
		label.setMaxWidth(360);
		label.setWrapText(true);
		label.setText(message);

		TextField answerField = new PasswordField();

		answerField.setOnKeyPressed(event -> {
			if (event.getCode().equals(KeyCode.ENTER)) {
				answer = answerField.getText().toCharArray();
				window.close();
			}
		});

		var button = new Button("CONFIRM");
		button.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: " +
						"10;" +
						" -fx-background-radius: 10;");
		button.setPrefWidth(150);
		button.setOnAction(event -> {
			answer = answerField.getText().toCharArray();
			window.close();
		});

		var cancelButton = new Button(NO_STRING);
		cancelButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: " +
						"10; -fx-background-radius: 10;");
		cancelButton.setPrefWidth(150);
		cancelButton.setOnAction(event -> {
			answer = null;
			window.close();
		});

		var hbox = new HBox();
		hbox.getChildren().addAll(button, cancelButton);
		hbox.setSpacing(20);
		hbox.setAlignment(Pos.CENTER);
		hbox.setPadding(new Insets(20, 20, 20, 20));

		var linkBox = new HBox();
		label.setAlignment(Pos.CENTER_LEFT);
		var hyperlink = new Hyperlink("Forgot your password?");
		linkBox.getChildren().add(hyperlink);

		hyperlink.setOnAction(actionEvent -> {
			try {
				resetPassword(isKey, pemFile);
				window.close();
			} catch (HederaClientException | KeyStoreException e) {
				PopupMessage.display("Error in password reset",
						"The password could not be reset at this time. Please check the application log", "CONTINUE");
				logger.error(e.getMessage());
			}
		});

		var vBox = new VBox();
		vBox.getChildren().addAll(label, answerField, linkBox, hbox);
		vBox.setSpacing(20);
		vBox.setAlignment(Pos.CENTER);
		vBox.setPadding(new Insets(20, 20, 20, 20));

		var scene = new Scene(vBox);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();

		return answer;

	}

	private static void resetPassword(boolean isKey, String pemFile) throws HederaClientException, KeyStoreException {
		var resetPassword = PopupMessage.display(RESET_PASSWORD_TITLE, WARNING_MESSAGE, true, RESET, NO_STRING);

		if (Boolean.TRUE.equals(resetPassword)) {
			var confirm = PopupMessage.display(RESET_PASSWORD_TITLE, "Are you sure?", true, RESET, NO_STRING);
			if (Boolean.TRUE.equals(confirm)) {
				if (!isKey) {
					var defaultStorage = System.getProperty(
							"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
					var newPwd = MnemonicInputPopup.display(defaultStorage);
					if (newPwd == null || newPwd.length == 0) {
						return;
					}
					answer = Arrays.copyOf(newPwd, newPwd.length);
					logger.info("Mnemonic password reset");
				} else {
					answer = KeyPairUtility.resetPassword(pemFile);
					logger.info("Key password reset");
				}
			}
		}
	}
}
