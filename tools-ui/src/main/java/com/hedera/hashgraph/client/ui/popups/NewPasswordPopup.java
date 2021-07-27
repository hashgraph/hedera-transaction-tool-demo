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
import com.hedera.hashgraph.client.core.constants.Constants;
import javafx.beans.binding.Bindings;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Arrays;

import static com.hedera.hashgraph.client.core.constants.Constants.MAX_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MIN_PASSWORD_LENGTH;

public class NewPasswordPopup {
	private static char[] answer;
	private static final Logger logger = LogManager.getLogger(NewPasswordPopup.class);

	public static char[] display() {

		var window = new Stage();
		var policy = new PasswordPolicy(BreachDatabase.top100K(), MIN_PASSWORD_LENGTH, MAX_PASSWORD_LENGTH);

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle("Password reset");

		window.sizeToScene();
		window.setMaxWidth(400);

		var label1 = new Label("Please enter the new password. The password must be at least 10 characters long.");
		label1.setMaxWidth(360);
		label1.setWrapText(true);

		var label2 = new Label("Please verify the password");
		label2.setMaxWidth(360);
		label2.setWrapText(true);

		var passwordField1 = new PasswordField();
		Label check1 = new Label("✓");
		check1.setStyle("-fx-text-fill: green");
		check1.setVisible(false);

		passwordField1.setOnKeyPressed(event -> {
			check1.setVisible(policy.check(passwordField1.getText()).equals(Status.OK));
			if (event.getCode().equals(KeyCode.ENTER)) {
				answer = passwordField1.getText().toCharArray();
			}
		});

		HBox box1 = new HBox();
		box1.setSpacing(10);
		box1.getChildren().addAll(passwordField1, check1);

		var passwordField2 = new PasswordField();


		Label check2 = new Label("✓");
		check2.setStyle("-fx-text-fill: green");
		check2.setVisible(false);
		passwordField2.setOnKeyReleased(event -> {
			check2.setVisible(check1.isVisible() && passwordField1.getText().equals(passwordField2.getText()));
			if (event.getCode().equals(KeyCode.ENTER)) {
				if (Arrays.equals(answer, passwordField2.getText().toCharArray())) {
					window.close();
				}
			}
		});

		HBox box2 = new HBox();
		box2.setSpacing(10);
		box2.getChildren().addAll(passwordField2, check2);

		box2.disableProperty().bind(
				Bindings.createBooleanBinding(() -> Status.OK.equals(policy.check(passwordField1.getText()))));


		Button continueButton = new Button("CONTINUE");
		continueButton.setStyle(Constants.WHITE_BUTTON_STYLE);

		var vBox = new VBox();
		vBox.getChildren().addAll(label1, box1, label2, box2, continueButton);
		vBox.setSpacing(20);
		vBox.setAlignment(Pos.CENTER);
		vBox.setPadding(new Insets(20, 20, 20, 20));

		var scene = new Scene(vBox);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();
		return answer;
	}
}
