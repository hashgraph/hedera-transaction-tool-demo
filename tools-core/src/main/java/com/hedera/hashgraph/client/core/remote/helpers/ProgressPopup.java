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

package com.hedera.hashgraph.client.core.remote.helpers;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class ProgressPopup {
	private ProgressPopup() {
		throw new IllegalStateException("Popup class");
	}

	public static Stage setupProgressPopup(final ProgressBar bar, final ProgressBar bar2, final Button cancelButton) {
		final var layout = new VBox();
		layout.setAlignment(Pos.CENTER);
		layout.setSpacing(10);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setMaxWidth(400);

		final var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle("Batch Transactions");

		window.sizeToScene();
		window.setWidth(450);


		final var label1 = new Label();
		label1.setText("Batch Transactions");
		label1.setStyle("-fx-font-size: 20");

		final var label2 = new Label(
				"Please wait while the transactions are being created and signed.");
		label2.setWrapText(true);
		label2.setStyle("-fx-font-size: 16");

		final var label3 =
				new Label("Transactions are now being compressed and exported to the output folder");
		label3.setWrapText(true);
		label3.setStyle("-fx-font-size: 16");
		label3.setVisible(false);

		final var box = new HBox();
		box.setPrefWidth(Region.USE_COMPUTED_SIZE);
		box.setPrefHeight(Region.USE_COMPUTED_SIZE);
		box.setAlignment(Pos.CENTER);
		box.getChildren().addAll(label2, label3);
		label2.managedProperty().bind(label2.visibleProperty());
		label3.managedProperty().bind(label3.visibleProperty());

		cancelButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
						"-fx-border-radius: 10; -fx-background-radius: 10;");
		cancelButton.setMinWidth(200);

		bar.setPrefWidth(375);
		bar2.setPrefWidth(375);

		bar.managedProperty().bind(bar.visibleProperty());
		bar2.managedProperty().bind(bar2.visibleProperty());
		bar2.visibleProperty().bind(bar.visibleProperty().not());

		label2.visibleProperty().bind(bar.visibleProperty());
		label3.visibleProperty().bind(bar.visibleProperty().not());

		layout.getChildren().addAll(label1, box, bar, bar2, cancelButton);

		final var scene = new Scene(layout);

		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.show();

		return window;
	}
}
