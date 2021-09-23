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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.OverrunStyle;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.io.File;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;

public class NewNetworkPopup implements GenericFileReadWriteAware {
	private static final UserAccessibleProperties properties =
			new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");

	private static final JsonObject customNetwork = new JsonObject();
	private static final VBox mainBox = new VBox();

	private NewNetworkPopup() {
		throw new IllegalStateException("Utility class");
	}

	public static JsonObject display() {
		var window = new Stage();
		window.setTitle("Add Custom Network");
		window.sizeToScene();
		window.setMaxWidth(600);
		window.initModality(Modality.APPLICATION_MODAL);

		final var errorLabel = new Label();
		errorLabel.setStyle("-fx-text-fill: red");
		errorLabel.setWrapText(true);
		errorLabel.setVisible(false);

		final var nicknameTextField = formatTextField();
		final var locationTextField = formatTextField();
		final var locationLabel = formatLabel();
		locationLabel.setVisible(false);

		nicknameTextField.setOnKeyPressed(
				keyEvent -> handleNickname(errorLabel, nicknameTextField, locationTextField, keyEvent));
		nicknameTextField.focusedProperty().addListener((observableValue, aBoolean, t1) -> {
			if (!t1) {
				removeFocusAction(errorLabel, nicknameTextField);
			}
		});
		locationTextField.setOnKeyPressed(
				keyEvent -> handleLocation(errorLabel, locationTextField, locationLabel, keyEvent));

		final var locationHBox = new HBox();
		locationHBox.getChildren().addAll(locationTextField, locationLabel);

		final var browseButton = formatButton("BROWSE");
		browseButton.setOnAction(actionEvent -> browseAction(errorLabel, locationTextField, locationLabel));
		final var continueButton = formatButton("CONTINUE");
		final var cancelButton = formatButton("CANCEL");
		continueButton.setOnAction(actionEvent -> {
			if (checkFields(errorLabel, nicknameTextField, locationLabel)) {
				return;
			}
			window.close();
		});
		cancelButton.setOnAction(actionEvent -> window.close());

		mainBox.setPadding(new Insets(10));
		mainBox.setSpacing(10);

		GridPane gridPane = new GridPane();
		gridPane.add(new Label("Nickname"), 0, 0);
		gridPane.add(nicknameTextField, 1, 0);
		gridPane.add(new Label("Location"), 0, 1);
		gridPane.add(locationHBox, 1, 1);
		gridPane.add(browseButton, 2, 1);

		gridPane.setHgap(20);
		gridPane.setVgap(10);
		mainBox.getChildren().add(gridPane);

		mainBox.getChildren().add(errorLabel);

		HBox continueBox = new HBox();
		continueBox.getChildren().addAll(cancelButton, continueButton);
		continueBox.setSpacing(20);
		continueBox.setAlignment(Pos.CENTER);

		mainBox.getChildren().add(continueBox);

		var scene = new Scene(mainBox);
		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		window.showAndWait();

		return customNetwork;
	}

	private static void browseAction(Label errorLabel, TextField locationTextField, Label locationLabel) {
		errorLabel.setVisible(false);
		File file = browse();
		if (file == null) {
			return;
		}
		locationLabel.setText(file.getAbsolutePath());
		locationLabel.setVisible(true);
		locationTextField.setVisible(false);
	}

	private static boolean checkFields(Label errorLabel, TextField nicknameTextField, Label locationLabel) {
		if (locationLabel.getText().isEmpty() || (nicknameTextField.getText().isEmpty() || locationLabel.getText().isEmpty())) {
			errorLabel.setText("Both a nickname and a file must be specified.");
			errorLabel.setVisible(true);
			return true;
		}
		if (!new File(locationLabel.getText()).exists()) {
			errorLabel.setText("The file specified does not exist");
			errorLabel.setVisible(true);
		}
		customNetwork.addProperty("nickname", nicknameTextField.getText());
		customNetwork.addProperty("file", locationLabel.getText());
		return false;
	}

	private static void handleLocation(Label errorLabel, TextField locationTextField, Label locationLabel,
			KeyEvent keyEvent) {
		errorLabel.setVisible(false);
		var code = keyEvent.getCode();
		if (code.equals(KeyCode.ENTER) || code.equals(KeyCode.TAB)) {
			final var text = locationTextField.getText();
			if (!new File(text).exists()) {
				errorLabel.setText("The location specified does not exist. Please try again.");
				errorLabel.setVisible(true);
				return;
			}
			errorLabel.setVisible(false);
			locationLabel.setText(text);
			locationLabel.setVisible(true);
			locationTextField.setVisible(false);
		}
	}

	private static void handleNickname(Label errorLabel, TextField nicknameTextField, TextField locationTextField,
			KeyEvent keyEvent) {
		errorLabel.setVisible(false);
		final var code = keyEvent.getCode();
		if (code.equals(KeyCode.ENTER) || code.equals(KeyCode.TAB)) {
			locationTextField.requestFocus();
		}
	}

	private static boolean removeFocusAction(Label errorLabel, TextField nicknameTextField) {
		String fileName = nicknameTextField.getText() + ".json";
		if (new File(Constants.CUSTOM_NETWORK_FOLDER, fileName).exists()) {
			errorLabel.setText("The network nickname already exists.");
			errorLabel.setVisible(true);
			nicknameTextField.requestFocus();
			return true;
		}
		return false;
	}

	private static Label formatLabel() {
		var label = new Label();
		label.setStyle("-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-border-radius: 10; " +
				"-fx-background-radius: 10;");

		label.managedProperty().bind(label.visibleProperty());
		label.setTextOverrun(OverrunStyle.LEADING_ELLIPSIS);
		label.setMaxWidth(250);
		label.setMinWidth(250);
		label.setPrefWidth(250);
		label.setPrefHeight(30);
		label.setPadding(new Insets(5));
		return label;
	}

	private static Button formatButton(String legend) {
		var button = new Button(legend);
		button.setMinWidth(150);
		button.setStyle(WHITE_BUTTON_STYLE);
		return button;
	}

	private static TextField formatTextField() {
		var textField = new TextField();
		textField.setStyle("-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-border-radius: 10; " +
				"-fx-background-radius: 10;");
		textField.managedProperty().bind(textField.visibleProperty());
		textField.setMaxWidth(250);
		textField.setMinWidth(250);
		textField.setPrefWidth(250);
		textField.setPrefHeight(30);
		return textField;
	}

	private static File browse() {
		var file = BrowserUtilities.browseFiles(properties.getLastBrowsedDirectory(), mainBox,
				"Json", Constants.JSON_EXTENSION);
		properties.setLastBrowsedDirectory(file);
		return file;
	}
}
