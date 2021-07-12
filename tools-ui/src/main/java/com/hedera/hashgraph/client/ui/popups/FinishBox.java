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
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.util.List;

public class FinishBox {

	private static final Logger logger = LogManager.getLogger(FinishBox.class);

	public static void display(List<String> transactions) {
		var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle("Setup Finished");

		window.sizeToScene();
		window.setWidth(450);


		var label1 = new Label();
		label1.setText("Setup Complete");
		label1.setStyle("-fx-font-size: 20");

		var vBox = new VBox();
		vBox.getChildren().add(label1);

		if (!transactions.isEmpty()) {
			var label2 = new Label("The following keys were created as part of the setup");
			label2.setWrapText(true);
			label2.setStyle("-fx-font-size: 16");
			vBox.getChildren().add(label2);

			for (var t : transactions) {
				var name = t.substring(t.lastIndexOf('/'));
				var file = new Hyperlink(name);
				file.setStyle("-fx-font-size: 16");
				file.setOnAction(event -> {
					try {
						openFolder(t);
					} catch (IOException e) {
						logger.error(e);
					}
				});
				vBox.getChildren().add(file);
			}

			var label3 = new Label(
					"Please copy the text file into a clean flash drive and send it by secure email to your " +
							"administrator");
			label3.setWrapText(true);
			label3.setStyle("-fx-font-size: 16");
			vBox.getChildren().add(label3);
		}

		var label4 = new Label("Visit the Settings page to make any changes to the configuration");
		label4.setWrapText(true);
		label4.setStyle("-fx-font-size: 16");
		vBox.getChildren().add(label4);

		var close = new Button("CONTINUE");
		close.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
						"-fx-border-radius: 10; -fx-background-radius: 10;");
		close.setMinWidth(150);
		var hBox = new HBox();
		hBox.getChildren().add(close);
		hBox.setAlignment(Pos.CENTER);
		close.setOnAction(event -> window.close());

		vBox.getChildren().add(hBox);

		vBox.setAlignment(Pos.CENTER);
		vBox.setSpacing(10);
		vBox.setPadding(new Insets(20, 20, 20, 20));


		var layout = new ScrollPane();
		layout.setContent(vBox);

		var scene = new Scene(layout);

		window.setScene(scene);

		window.showAndWait();
	}

	public static void display(final File createdKeysFileName, String titleLabel, String displayMessage) {
		var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle(titleLabel);

		window.sizeToScene();
		window.setWidth(450);


		var label1 = new Label();
		label1.setText(titleLabel);
		label1.setStyle("-fx-font-size: 20");

		var vBox = new VBox();
		vBox.getChildren().add(label1);


		var label2 = new Label(displayMessage);
		label2.setWrapText(true);
		label2.setStyle("-fx-font-size: 16");
		vBox.getChildren().add(label2);


//		String name = createdKeysFileName.substring(createdKeysFileName.lastIndexOf('/'));

		var path = createdKeysFileName.getParentFile().getPath();
		var file = new Hyperlink(path);
		if (path.length() > 50) {
			file.setText(String.format("...%s", path.substring(path.length() - 50)));
		}
		file.setStyle("-fx-font-size: 16");
		file.setOnAction(event -> {
			try {
				openFolder(createdKeysFileName.getPath());
			} catch (IOException e) {
				logger.error(e);
			}
		});
		vBox.getChildren().add(file);

		var close = new Button("CONTINUE");
		close.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
						"-fx-border-radius: 10; -fx-background-radius: 10;");
		close.setMinWidth(150);
		var hBox = new HBox();
		hBox.getChildren().add(close);
		hBox.setAlignment(Pos.CENTER);
		close.setOnAction(event -> window.close());

		vBox.getChildren().add(hBox);

		vBox.setAlignment(Pos.CENTER);
		vBox.setSpacing(10);
		vBox.setPadding(new Insets(20, 20, 20, 20));


		//	ScrollPane layout = new ScrollPane();
		//	layout.setContent(vBox);

		var scene = new Scene(vBox);

		window.setScene(scene);

		window.showAndWait();
	}

	private static void openFolder(String filename) throws IOException {
		Desktop.getDesktop().open(new File(filename.substring(0, filename.lastIndexOf('/'))));
	}
}