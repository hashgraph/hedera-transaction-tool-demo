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

import com.codahale.passpol.BreachDatabase;
import com.codahale.passpol.PasswordPolicy;
import com.codahale.passpol.Status;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.util.Arrays;

import static com.hedera.hashgraph.client.core.constants.Constants.MAX_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MIN_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;

public class NewPasswordPopup {
	private static char[] answer;

	public static char[] display() {

		var window = new Stage();
		var policy = new PasswordPolicy(BreachDatabase.top100K(), MIN_PASSWORD_LENGTH, MAX_PASSWORD_LENGTH);

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle("New Password");

		window.sizeToScene();
		window.setMaxWidth(400);

		var titleLabel = new Label("Change your key's password");
		titleLabel.setStyle("-fx-font-size: 20");

		var warningLabel = new Label(
				"Your new password will be checked against a list of common passwords, to increase your security.");
		warningLabel.setMaxWidth(360);
		warningLabel.setWrapText(true);
		VBox passwordBox = new VBox();
		Label firstTitle = new Label("Password");
		Label firstExplanation = new Label("Must be at least 10 characters.");

		VBox confirmBox = new VBox();
		Label secondTitle = new Label("Confirm password");
		Label secondExplanation = new Label("Both passwords must match.");

		var passwordField1 = new PasswordField();
		var passwordField2 = new PasswordField();

		HBox.setHgrow(passwordField1, Priority.ALWAYS);
		HBox.setHgrow(passwordField2, Priority.ALWAYS);

		passwordField2.setDisable(true);

		Label check1 = new Label("✓");
		check1.setStyle("-fx-text-fill: green");
		check1.setVisible(false);

		HBox box1 = new HBox();
		box1.setSpacing(10);
		box1.getChildren().addAll(passwordField1, check1);

		passwordBox.getChildren().addAll(firstTitle, box1, firstExplanation);
		passwordBox.setSpacing(3);

		Label check2 = new Label("✓");
		check2.setStyle("-fx-text-fill: green");
		check2.setVisible(false);

		HBox box2 = new HBox();
		box2.setSpacing(10);
		box2.getChildren().addAll(passwordField2, check2);

		Button continueButton = new Button("CONTINUE");
		continueButton.setMinWidth(150);
		continueButton.setStyle(WHITE_BUTTON_STYLE);
		Button cancelButton = new Button("CANCEL");
		cancelButton.setMinWidth(150);
		cancelButton.setStyle(WHITE_BUTTON_STYLE);

		confirmBox.getChildren().addAll(secondTitle, box2, secondExplanation);
		confirmBox.setSpacing(3);

		var vBox = new VBox();
		HBox buttonBar = new HBox();
		buttonBar.getChildren().addAll(cancelButton, continueButton);
		HBox.setHgrow(buttonBar, Priority.ALWAYS);
		buttonBar.setSpacing(20);
		buttonBar.setAlignment(Pos.CENTER);


		continueButton.visibleProperty().bind(check2.visibleProperty());
		continueButton.managedProperty().bind(continueButton.visibleProperty());

		vBox.getChildren().addAll(titleLabel, warningLabel, passwordBox, confirmBox, buttonBar);
		vBox.setSpacing(20);
		vBox.setAlignment(Pos.CENTER);
		vBox.setPadding(new Insets(20, 20, 20, 20));

		// region EVENTS
		passwordField1.setOnKeyPressed(event -> {
			check1.setVisible(policy.check(passwordField1.getText()).equals(Status.OK));
			if (check1.isVisible()) {
				passwordField2.setDisable(false);
			}
			if (event.getCode().equals(KeyCode.ENTER) || event.getCode().equals(KeyCode.TAB)) {
				answer = passwordField1.getText().toCharArray();
				passwordField1.setDisable(true);
			}
		});
		passwordField2.setOnKeyReleased(event -> {
			check2.setVisible(check1.isVisible() && passwordField1.getText().equals(passwordField2.getText()));
			if (event.getCode().equals(KeyCode.ENTER)) {
				if (Arrays.equals(answer, passwordField2.getText().toCharArray())) {
					window.close();
				}
			}
		});
		continueButton.setOnAction(actionEvent -> {
			answer = passwordField1.getText().toCharArray();
			if (Arrays.equals(answer, passwordField2.getText().toCharArray())) {
				window.close();
			} else {
				PopupMessage.display("Password mismatch", "The passwords don't match. Please try again.");
			}
		});
		cancelButton.setOnAction(actionEvent -> {
			answer = new char[0];
			window.close();
		});
		// endregion

		var scene = new Scene(vBox);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();
		return answer;
	}
}
