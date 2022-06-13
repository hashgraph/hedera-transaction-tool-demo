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

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.ui.HomePaneController;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import static java.lang.Math.max;
import static java.lang.Math.sqrt;

public class ExtraKeysSelectorPopup {
	protected static final String PK_EXTENSION = ".pem";
	private static final Logger logger = LogManager.getLogger(ExtraKeysSelectorPopup.class);

	private ExtraKeysSelectorPopup() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Displays a list of checkboxes corresponding to the keys that have been imported to the app.
	 * Allows the user to choose a subset of the keys that will be used for signing the transaction.
	 *
	 * @param signingKeys
	 * 		keys that are already identified as necessary
	 * @return a list of keys that will be added to the set of signing keys
	 */
	public static List<File> display(final Set<File> signingKeys) {
		final var window = new Stage();
		final var layout = new VBox();

		final var cancelBoolean = new AtomicBoolean(true);

		final Set<File> selectedSet = new HashSet<>();

		final var knownKeys =
				new File(Constants.KEYS_FOLDER).listFiles(pathname -> pathname.isFile() && isPEM(pathname));
		if (knownKeys == null) {
			throw new HederaClientRuntimeException("Error reading known keys");
		}
		Arrays.sort(knownKeys);
		final var columns = (int) max(3, sqrt(knownKeys.length));
		logger.info("Grid panes will have {} columns", columns);


		final var nonSignersVBox = new VBox();
		nonSignersVBox.setStyle("-fx-border-color: #0b9dfd; -fx-background-color: white");
		nonSignersVBox.setSpacing(5);
		nonSignersVBox.setPadding(new Insets(10));
		nonSignersVBox.managedProperty().bind(nonSignersVBox.visibleProperty());

		final var nonSignersGridPane = new GridPane();
		nonSignersGridPane.setHgap(10);
		nonSignersGridPane.setVgap(10);


		final var extraSignersGridPane = new GridPane();
		extraSignersGridPane.setHgap(10);
		extraSignersGridPane.setVgap(10);


		var nonSignNumber = 0;
		for (final var knownKey : knownKeys) {
			final var checkBox = getCheckBox(selectedSet, knownKey);
			if (!signingKeys.contains(knownKey)) {
				checkBox.setSelected(false);
				nonSignersGridPane.add(checkBox, nonSignNumber % columns, nonSignNumber / columns);
				nonSignNumber++;
			}
		}

		nonSignersVBox.getChildren().add(nonSignersGridPane);

		nonSignersVBox.setVisible(nonSignNumber > 0);

		final var acceptButton = new Button("ACCEPT");
		acceptButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd;  -fx-border-radius: 10; " +
						"-fx-background-radius: 10;");
		acceptButton.setPrefWidth(200);
		acceptButton.setOnAction(event -> {
			cancelBoolean.set(false);
			window.close();
		});

		final var cancelButton = new Button("CANCEL");
		cancelButton.setStyle(
				"-fx-background-color: #0b9dfd; -fx-border-radius: 10; -fx-background-radius: 10; -fx-text-fill: " +
						"white");
		cancelButton.setPrefWidth(200);
		cancelButton.setOnAction(event -> {
			cancelBoolean.set(true);
			window.close();
		});


		final var buttonBox = new HBox();
		buttonBox.setAlignment(Pos.CENTER);
		buttonBox.setSpacing(10);
		buttonBox.getChildren().addAll(acceptButton, cancelButton);

		window.setTitle("Additional Keys");
		window.sizeToScene();
		window.initModality(Modality.APPLICATION_MODAL);


		layout.setSpacing(20);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setAlignment(Pos.CENTER);


		layout.setStyle("-fx-font-size: 14");

		if (nonSignersVBox.isVisible()) {
			layout.getChildren().addAll(nonSignersVBox, buttonBox);
		} else {
			final var label =
					new Label("The application has not detected any keys. Please select keys using the BROWSE button");
			label.setWrapText(true);
			label.setMaxWidth(400);
			cancelButton.setText("CONTINUE");
			acceptButton.setVisible(false);
			layout.getChildren().addAll(label, buttonBox);
		}

		final var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();

		return (cancelBoolean.get()) ? new ArrayList<>() : new ArrayList<>(selectedSet);
	}

	private static CheckBox getCheckBox(final Set<File> selectedSet, final File knownKey) {
		final var baseName = FilenameUtils.getBaseName(knownKey.getName());
		final var checkBox = new CheckBox(baseName);
		HomePaneController.checkBoxListener(selectedSet, knownKey, baseName, checkBox, logger);
		return checkBox;
	}

	private static boolean isPEM(final File pathname) {
		return pathname.getAbsolutePath().endsWith(PK_EXTENSION);
	}

}
