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

import com.hedera.hashgraph.client.ui.Style;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.scene.text.TextAlignment;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class MnemonicBox {
	static Boolean confirmation = null;

	private MnemonicBox() {
		throw new IllegalStateException("Utility class");
	}

	public static Boolean display(final String title, final String message, final String[] words,
			final String buttonName) {
		final var window = new Stage();

		window.setTitle(title);
		window.sizeToScene();

		window.initModality(Modality.APPLICATION_MODAL);

		final var label = new Label();
		label.setWrapText(true);
		label.setText(message);
		label.setWrapText(true);
		label.setTextAlignment(TextAlignment.CENTER);
		label.setPrefHeight(Region.USE_COMPUTED_SIZE);
		label.setMaxWidth(400);
		label.setStyle("-fx-font-size: 16");

		final var yesButton = new Button(buttonName.toUpperCase());
		yesButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: " +
						"10;" +
						" -fx-background-radius: 10;");
		yesButton.setPrefWidth(150);


		final var mnemonic = new Label();
		var counter = 0;
		var phrase = "";
		for (final var word :
				words) {
			phrase = phrase.concat(word.toUpperCase());
			if (counter < 23) {
				if (counter % 4 == 3) {
					phrase = phrase.concat("\n");
				} else {
					phrase = phrase.concat("  ");
				}
			}
			counter++;
		}

		mnemonic.setText(phrase);
		mnemonic.setWrapText(true);
		mnemonic.setTextAlignment(TextAlignment.CENTER);
		mnemonic.setPrefHeight(Region.USE_COMPUTED_SIZE);
		mnemonic.setStyle("-fx-font-size: 18");

		final var layout = new VBox();
		layout.setPadding(new Insets(20, 20, 20, 20));

		layout.setOnKeyPressed(event -> {
			if (event.getCode() == KeyCode.ENTER) {
				window.close();
			}
		});

		layout.getChildren().addAll(label, mnemonic, yesButton);
		layout.setAlignment(Pos.CENTER);
		layout.setSpacing(20);
		yesButton.setOnAction(actionEvent -> window.close());

		layout.setStyle("-fx-font-size: 14");

		final var scene = new Scene(layout);
		Style.addStylesheets(scene);

		window.setScene(scene);

		window.showAndWait();

		return confirmation;
	}
}
