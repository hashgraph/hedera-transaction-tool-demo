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
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

import static com.hedera.hashgraph.client.core.constants.Constants.MAX_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MIN_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;

public class NewPasswordPopup {
	public static final String TITLE_LABEL = "Change your password";
	public static final String WARNING_LABEL =
			"Your new password will be checked against a list of common passwords, to increase your security.";
	public static final String PASSWORD_LENGTH_LABEL = "Must be at least 10 characters.";
	public static final String MATCH_LABEL = "Both passwords must match.";
	private static char[] answer;

	private NewPasswordPopup() {
		throw new IllegalStateException("Utility class");
	}

	public static char[] display() {
		return display(TITLE_LABEL, WARNING_LABEL);
	}

	public static char[] display(final String title, final String message) {
		final Stage window = getStage();

		// Setup labels
		final var titleLabel = new Label(title);
		titleLabel.setStyle("-fx-font-size: 20");

		final Label warningLabel = getLabel(message);
		final var firstTitle = new Label("Password");

		final var secondTitle = new Label("Confirm password");

		// Error Labels
		final Label error1 = getLabel(PASSWORD_LENGTH_LABEL);
		error1.setStyle("-fx-text-fill: red");
		error1.setWrapText(true);
		error1.setVisible(false);

		final Label error2 = getLabel(MATCH_LABEL);
		error2.setStyle("-fx-text-fill: red");
		error2.setWrapText(true);
		error2.setVisible(false);

		// Setup enter password box and contents
		final var passwordField1 = new PasswordField();
		HBox.setHgrow(passwordField1, Priority.ALWAYS);
		final var check1 = getCheck();
		final var bundlePassword = bundlePasswordAndCheck(passwordField1, check1);
		final var passwordBox = new VBox();
		passwordBox.getChildren().addAll(firstTitle, bundlePassword, error1);
		passwordBox.setSpacing(3);

		// Setup confirm password box and contents
		final var passwordField2 = new PasswordField();
		HBox.setHgrow(passwordField2, Priority.ALWAYS);
		passwordField2.setDisable(true);
		final var check2 = getCheck();
		final var bundleConfirmation = bundlePasswordAndCheck(passwordField2, check2);
		final var confirmBox = new VBox();
		confirmBox.getChildren().addAll(secondTitle, bundleConfirmation, error2);
		confirmBox.setSpacing(3);


		// Setup buttons
		final var continueButton = setupButton("CONTINUE");
		final var cancelButton = setupButton("CANCEL");
		final HBox buttonBar = getButtonBar(continueButton, cancelButton);
		continueButton.visibleProperty().bind(check2.visibleProperty());
		continueButton.managedProperty().bind(continueButton.visibleProperty());


		// region EVENTS

		passwordField1.setOnKeyReleased(
				event -> keyPressedEvent(passwordField1, passwordField2, check1, error1, check2, error2, event));
		passwordField2.setOnKeyReleased(
				event -> keyReleasedEvent(window, passwordField1, passwordField2, check1, check2, error2, event));
		continueButton.setOnAction(actionEvent -> continueActionEvent(window, passwordField1, passwordField2));
		cancelButton.setOnAction(actionEvent -> cancelActionEvent(window));
		// endregion


		final var scene = new Scene(getMainBox(titleLabel, warningLabel, passwordBox, confirmBox, buttonBar));
		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		window.showAndWait();
		return answer;
	}

	@NotNull
	private static Label getLabel(final String legend) {
		final var label = new Label(legend);
		label.setMaxWidth(360);
		label.setWrapText(true);
		return label;
	}

	@NotNull
	private static VBox getMainBox(final Label titleLabel, final Label warningLabel, final VBox passwordBox,
			final VBox confirmBox,
			final HBox buttonBar) {
		final var vBox = new VBox();

		vBox.getChildren().addAll(titleLabel, warningLabel, passwordBox, confirmBox, buttonBar);
		vBox.setSpacing(10);
		vBox.setAlignment(Pos.CENTER);
		vBox.setPadding(new Insets(20, 20, 20, 20));
		return vBox;
	}

	@NotNull
	private static Stage getStage() {
		final var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle("New Password");

		window.sizeToScene();
		window.setMaxWidth(400);
		return window;
	}

	@NotNull
	private static HBox getButtonBar(final Button continueButton, final Button cancelButton) {
		final var buttonBar = new HBox();
		buttonBar.getChildren().addAll(cancelButton, continueButton);
		HBox.setHgrow(buttonBar, Priority.ALWAYS);
		buttonBar.setSpacing(20);
		buttonBar.setAlignment(Pos.CENTER);
		return buttonBar;
	}

	@NotNull
	private static Label getCheck() {
		final var check2 = new Label("✓");
		check2.setStyle("-fx-text-fill: green");
		check2.setVisible(false);
		return check2;
	}

	@NotNull
	private static HBox bundlePasswordAndCheck(final PasswordField passwordField1, final Label check1) {
		final var box1 = new HBox();
		box1.setSpacing(10);
		box1.getChildren().addAll(passwordField1, check1);
		return box1;
	}

	@NotNull
	private static Button setupButton(final String legend) {
		final var continueButton = new Button(legend);
		continueButton.setMinWidth(150);
		continueButton.setStyle(WHITE_BUTTON_STYLE);
		return continueButton;
	}

	private static void cancelActionEvent(final Stage window) {
		answer = new char[0];
		window.close();
	}

	private static void continueActionEvent(final Stage window, final PasswordField passwordField1,
			final PasswordField passwordField2) {
		answer = passwordField1.getText().toCharArray();
		if (Arrays.equals(answer, passwordField2.getText().toCharArray())) {
			window.close();
		} else {
			PopupMessage.display("Password mismatch", "The passwords don't match. Please try again.");
		}
	}

	private static void keyReleasedEvent(final Stage window, final PasswordField passwordField1,
			final PasswordField passwordField2,
			final Label check1, final Label check2, final Label error, final KeyEvent event) {
		check2.setVisible(check1.isVisible() && passwordField1.getText().equals(passwordField2.getText()));
		error.setVisible(!check2.isVisible());
		if (event.getCode().equals(KeyCode.ENTER) &&
				Arrays.equals(answer, passwordField2.getText().toCharArray())) {
			window.close();
		}
	}

	private static void keyPressedEvent(
			final PasswordField passwordField1, final PasswordField passwordField2, final Label check1,
			final Label error1, final Label check2, final Label error2, final KeyEvent event) {
		final var policy = new PasswordPolicy(BreachDatabase.top100K(), MIN_PASSWORD_LENGTH, MAX_PASSWORD_LENGTH);
		final var status = policy.check(passwordField1.getText());
		check1.setVisible(status.equals(Status.OK));
		if (check1.isVisible()) {
			error1.setVisible(false);
			passwordField2.setDisable(false);
		} else {
			error1.setText(getLegend(status));
			error1.setWrapText(true);
			error1.setVisible(true);
			check2.setVisible(false);
		}
		if ((event.getCode().equals(KeyCode.ENTER) || event.getCode().equals(
				KeyCode.TAB)) && check1.isVisible()) {
			answer = passwordField1.getText().toCharArray();
		}

		check2.setVisible(passwordField1.getText().equals(passwordField2.getText()));

		if (check1.isVisible() && check2.isVisible()) {
			error2.setVisible(false);
		}

	}

	private static String getLegend(final Status status) {
		var legend = "";
		switch (status) {
			case OK:
				break;
			case TOO_SHORT:
				legend = PASSWORD_LENGTH_LABEL;
				break;
			case TOO_LONG:
				legend = "The password must be at most 1024 characters long.";
				break;
			case BREACHED:
				legend = "The password has been breached.";
				break;
		}
		return legend;
	}
}
