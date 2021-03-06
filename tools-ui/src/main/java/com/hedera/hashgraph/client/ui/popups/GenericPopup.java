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
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

// I want to deprecate all other popup classes and replace it with this one.
public class GenericPopup {

	private static final Logger logger = LogManager.getLogger(GenericPopup.class);
	private static String answer = "";

	private GenericPopup() {
		throw new IllegalStateException("Utility class");
	}

	public static String display(
			final String title, final String acceptMessage, final String cancelMessage, final boolean showTextField,
			final boolean isInteger, final String... message) {
		if ("".equals(acceptMessage) && "".equals(cancelMessage)) {
			logger.error("At least one button must have a legend");
			return answer;
		}

		final var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle(title);
		window.sizeToScene();

		final var layout = new VBox();
		layout.setStyle("-fx-font-size: 16");
		layout.setPadding(new Insets(20, 30, 20, 30));
		layout.setSpacing(20);
		layout.setMaxWidth(450);
		layout.setAlignment(Pos.CENTER);

		if (!"".equals(title)) {
			final var titleLabel = new Label();
			titleLabel.setText(title);
			titleLabel.setStyle("-fx-font-size: 22");
			layout.getChildren().add(titleLabel);
		}

		// Setup Text
		for (final var s : message) {
			final var label = new Label();
			label.setText(s);
			label.setWrapText(true);
			layout.getChildren().add(label);
		}

		final var answerField = new TextField();
		if (showTextField) {

			if (isInteger) {
				answerField.textProperty().addListener((observable, oldValue, newValue) -> {
					if (!newValue.matches("\\d*")) {
						answerField.setText(newValue.replaceAll("[^\\d]", ""));
					}
				});
			}
			layout.getChildren().add(answerField);
		}


		final var buttonBar = new ButtonBar();
		buttonBar.setButtonMinWidth(75);

		// Buttons setup
		if (!"".equals(acceptMessage)) {
			final var acceptButton = new Button(acceptMessage);
			acceptButton.setStyle(
					"-fx-background-color: #0b9dfd ; -fx-border-color: #0b9dfd; -fx-text-fill: white; " +
							"-fx-border-radius: 10; -fx-background-radius: 10;");
			acceptButton.setPrefWidth(200);

			acceptButton.setOnAction(event -> {
				answer = answerField.getText();
				window.close();
			});
			buttonBar.getButtons().add(acceptButton);
		}

		if (!"".equals(cancelMessage)) {
			final var cancelButton = new Button(cancelMessage);
			cancelButton.setStyle("-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
					"-fx-border-radius: 10; -fx-background-radius: 10;");

			cancelButton.setOnAction(event -> {
				answer = "";
				window.close();
			});
			buttonBar.getButtons().add(cancelButton);
		}

		layout.getChildren().add(buttonBar);

		final var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		window.showAndWait();

		return answer;
	}


}
