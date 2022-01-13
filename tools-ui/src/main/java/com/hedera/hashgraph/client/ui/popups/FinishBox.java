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
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;

public class FinishBox {

	private static final Logger logger = LogManager.getLogger(FinishBox.class);
	public static final String FX_FONT_SIZE_16 = "-fx-font-size: 16";
	public static final String BUTTON_STYLE_STRING =
			"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
					"-fx-border-radius: 10; -fx-background-radius: 10;";

	private FinishBox() {
		throw new IllegalStateException("Utility class");
	}

	public static void display(final File createdKeysFileName, final String titleLabel, final String displayMessage) {
		final var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle(titleLabel);

		window.sizeToScene();
		window.setWidth(450);


		final var label1 = new Label();
		label1.setText(titleLabel);
		label1.setStyle("-fx-font-size: 20");

		final var vBox = new VBox();
		vBox.getChildren().add(label1);


		final var label2 = new Label(displayMessage);
		label2.setWrapText(true);
		label2.setStyle(FX_FONT_SIZE_16);
		vBox.getChildren().add(label2);

		final var path = createdKeysFileName.getParentFile().getPath();
		final var file = new Hyperlink(path);
		if (path.length() > 50) {
			file.setText(String.format("...%s", path.substring(path.length() - 50)));
		}
		file.setStyle(FX_FONT_SIZE_16);
		file.setOnAction(event -> {
			try {
				openFolder(createdKeysFileName.getPath());
			} catch (final IOException e) {
				logger.error(e);
			}
		});
		vBox.getChildren().add(file);

		buildContinueButton(window, vBox);

		final var scene = new Scene(vBox);

		window.setScene(scene);

		window.showAndWait();
	}

	private static void openFolder(final String filename) throws IOException {
		Desktop.getDesktop().open(new File(filename.substring(0, filename.lastIndexOf('/'))));
	}

	private static void buildContinueButton(final Stage window, final VBox vBox) {
		final var close = new Button("CONTINUE");
		close.setStyle(BUTTON_STYLE_STRING);
		close.setMinWidth(150);
		final var hBox = new HBox();
		hBox.getChildren().add(close);
		hBox.setAlignment(Pos.CENTER);
		close.setOnAction(event -> window.close());

		vBox.getChildren().add(hBox);

		vBox.setAlignment(Pos.CENTER);
		vBox.setSpacing(10);
		vBox.setPadding(new Insets(20, 20, 20, 20));
	}

}