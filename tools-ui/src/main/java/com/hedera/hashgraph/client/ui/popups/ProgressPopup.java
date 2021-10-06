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

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class ProgressPopup {
	private ProgressPopup() {
		throw new IllegalStateException("Popup class");
	}


	public static Stage setupProgressPopup(ProgressBar bar, Button cancelButton, String title, String message,
			long size) {
		var window = new Stage();
		var layout = new VBox();
		layout.setAlignment(Pos.CENTER);
		layout.setSpacing(10);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setMaxWidth(400);

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle(title);
		window.sizeToScene();
		window.setWidth(450);

		final var titleLabel = new Label();
		titleLabel.setText(title);
		titleLabel.setStyle("-fx-font-size: 20");

		final var messageLabel = new Label(message);
		messageLabel.setWrapText(true);
		messageLabel.setStyle("-fx-font-size: 16");

		cancelButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
						"-fx-border-radius: 10; -fx-background-radius: 10;");
		cancelButton.setMinWidth(200);

		bar.setPrefWidth(375);

		final var text = new Text();
		bar.progressProperty().addListener((observableValue, number, t1) -> text.setText(
				String.format("Requesting %d of %d", Math.round(number.doubleValue() * size), size)));

		layout.getChildren().addAll(titleLabel, messageLabel, bar, text, cancelButton);

		var scene = new Scene(layout);

		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		window.show();
		return window;
	}


}
