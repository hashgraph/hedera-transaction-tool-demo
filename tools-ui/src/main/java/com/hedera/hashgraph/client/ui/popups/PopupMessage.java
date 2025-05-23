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

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.scene.text.TextAlignment;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class PopupMessage {

	private static final String BUTTON_CONTINUE = "CONTINUE";
	public static final int PREF_BUTTON_WIDTH = 250;

	static Boolean confirmation = null;

	public static Boolean display(final String title, final String message) {
		return display(title, message, false, "Yes", "No");
	}

	public static Boolean display(final String title, final String message, final boolean scrollable) {
		return display(title, message, scrollable, false, "Yes", "No");
	}

	public static Boolean display(final String title, final String message, final String buttonName) {
		return display(title, message, false, false, "Yes", "No", buttonName);
	}

	public static Boolean display(final String title, final String message, final boolean scrollable,
								  final String buttonName) {
		return display(title, message, scrollable, false, "Yes", "No", buttonName);
	}

	public static Boolean display(final String title, final String message, final boolean confirm,
			final String yesString, final String noString) {
		return display(title, message, false, confirm, yesString, noString);
	}

	public static Boolean display(final String title, final String message, final boolean scrollable,
								  final boolean confirm, final String yesString, final String noString) {
		return display(title, message, scrollable, confirm, yesString, noString, BUTTON_CONTINUE);
	}

	private PopupMessage() {
		throw new IllegalStateException("Utility class");
	}

	private static Boolean display(final String title, final String message, final boolean scrollable,
								   final boolean confirm, final String yesString, final String noString,
								   final String buttonName) {
		final var window = new Stage();

		window.setTitle(title);
		window.sizeToScene();
		window.setMaxWidth(600);
		window.initModality(Modality.APPLICATION_MODAL);


		final var vBox = new VBox();
		if (scrollable) {
			final var textArea = new TextArea(message);
			textArea.setEditable(false);
			textArea.setFocusTraversable(false);
			textArea.setMaxWidth(550);
			textArea.setPrefHeight(Region.USE_COMPUTED_SIZE);

			vBox.getChildren().add(textArea);
		} else {
			final var label = new Label();
			label.setText(message);
			label.setWrapText(true);
			label.setTextAlignment(TextAlignment.LEFT);
			label.setPrefHeight(Region.USE_COMPUTED_SIZE);
			label.setMaxWidth(550);
			label.setStyle("-fx-font-size: 16");

			vBox.getChildren().add(label);
		}

		final var button =
				(buttonName == null || buttonName.isEmpty() || buttonName.equalsIgnoreCase(BUTTON_CONTINUE)) ?
				new Button(BUTTON_CONTINUE) :
				new Button(buttonName.toUpperCase());
		button.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: " +
						"10; -fx-background-radius: 10;");

		button.setPrefWidth(PREF_BUTTON_WIDTH);
		button.setOnAction(event -> window.close());

		final var centerButton = new HBox();
		final var spacer1 = new Region();
		spacer1.setMinWidth(20);
		spacer1.setPrefHeight(Region.USE_COMPUTED_SIZE);
		spacer1.setPrefWidth(Region.USE_COMPUTED_SIZE);
		HBox.setHgrow(spacer1, Priority.ALWAYS);

		final var spacer2 = new Region();
		spacer2.setPrefWidth(Region.USE_COMPUTED_SIZE);
		spacer2.setPrefHeight(Region.USE_COMPUTED_SIZE);
		spacer2.setMinWidth(20);
		HBox.setHgrow(spacer2, Priority.ALWAYS);

		final var yesButton = new Button(yesString);
		yesButton.setStyle(
				"-fx-background-color: #0b9dfd; -fx-border-color: #0b9dfd; -fx-text-fill: white; -fx-border-radius: " +
						"10;" +
						" -fx-background-radius: 10;");
		yesButton.setPrefWidth(150);

		final var noButton = new Button(noString);
		noButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: " +
						"10;" +
						" -fx-background-radius: 10;");
		noButton.setPrefWidth(150);
		yesButton.setOnAction(event -> {
			confirmation = true;
			window.close();
		});

		noButton.setOnAction(event -> {
			confirmation = false;
			window.close();
		});

		final var buttonBar = new HBox();

		buttonBar.getChildren().addAll(yesButton, noButton);
		buttonBar.setSpacing(10);
		buttonBar.setAlignment(Pos.CENTER);


		if (confirm) {
			centerButton.getChildren().addAll(spacer1, buttonBar, spacer2);
		} else {
			centerButton.getChildren().addAll(spacer1, button, spacer2);
		}

		vBox.getChildren().add(centerButton);
		vBox.setSpacing(20);
		vBox.setPadding(new Insets(20, 20, 20, 20));

		final var layout = new HBox();

		vBox.setOnKeyPressed(event -> {
			if (event.getCode() == KeyCode.ENTER) {
				window.close();
			}
		});

		layout.getChildren().addAll(vBox);

		layout.setStyle("-fx-font-size: 14");

		final var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();

		return confirmation;
	}
}
