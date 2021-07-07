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
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class AlertBox {
	private static String answer;

	public static String display(String title, String message) {
		return display(title, message, "");

	}

	public static String display(String title, String message, String defaultMessage) {
		return display(title, message, defaultMessage, false, false, "");
	}


	private static String display(String title, String message, String defaultMessage, boolean inputPwd,
			boolean showTitle, String mainTitle) {
		var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle(title);

		window.sizeToScene();
		var titleLabel = new Label();
		titleLabel.setText(mainTitle);
		titleLabel.setStyle("-fx-font-size: 22");

		var label = new Label();
		label.setText(message);
		label.setWrapText(true);

		TextField answerField;
		if (inputPwd) {
			answerField = new PasswordField();
		} else {
			answerField = new TextField(defaultMessage);
		}

		answerField.setPrefWidth(300);
		answerField.setOnKeyPressed(event -> {
			if (event.getCode().equals(KeyCode.ENTER)) {
				answer = answerField.getText();
				window.close();
			}
		});


		var hBox = new HBox();

		var continueButton = new Button("CONTINUE");
		continueButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
						"-fx-border-radius: 10; -fx-background-radius: 10;");
		continueButton.setPrefWidth(200);

		continueButton.setOnAction(event -> {
			answer = answerField.getText();
			window.close();
		});

		var region = new Region();
		region.setPrefWidth(Region.USE_COMPUTED_SIZE);
		region.setPrefHeight(Region.USE_COMPUTED_SIZE);

		hBox.getChildren().addAll(region, continueButton);
		HBox.setHgrow(region, Priority.ALWAYS);

		var layout = new VBox();

		if (showTitle) {
			layout.getChildren().add(titleLabel);
		}

		layout.getChildren().addAll(label, answerField, hBox);


		layout.setStyle("-fx-font-size: 12");

		layout.setPadding(new Insets(20, 30, 20, 30));
		layout.setSpacing(20);
		layout.setMaxWidth(450);
		layout.setAlignment(Pos.CENTER);

		var scene = new Scene(layout);

		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();

		return answer;

	}

}
