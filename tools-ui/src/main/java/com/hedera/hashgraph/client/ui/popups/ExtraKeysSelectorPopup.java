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
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.ui.components.SigningKeyCheckBoxListener;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import javafx.beans.binding.Bindings;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.BLUE_BUTTON_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;
import static java.lang.Math.max;
import static java.lang.Math.min;
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

		final Set<File> selectedSet = new HashSet<>(signingKeys);

		final var knownKeys =
				new File(Constants.KEYS_FOLDER).listFiles(pathname -> pathname.isFile() && isPEM(pathname));
		if (knownKeys == null) {
			throw new HederaClientRuntimeException("Error reading known keys");
		}
		Arrays.sort(knownKeys);
		final var columns = min((int) max(3, sqrt(knownKeys.length)), 9);
		logger.info("Grid panes will have {} columns", columns);

		final var nonSignersVBox = getvBox();
		final var browsedSignersVBox = getvBox();

		final var nonSignersGridPane = getGridPane(columns);
		final var browsedSignersGridPane = getGridPane(columns);
		browsedSignersVBox.getChildren().add(browsedSignersGridPane);
		browsedSignersVBox.visibleProperty().bind(Bindings.size(browsedSignersGridPane.getChildren()).greaterThan(0));

		var nonSignNumber = 0;
		for (final var knownKey : knownKeys) {
			final var checkBox = getCheckBox(selectedSet, knownKey);
			checkBox.setSelected(signingKeys.contains(knownKey));
			nonSignersGridPane.add(checkBox, nonSignNumber % columns, nonSignNumber / columns);
			nonSignNumber++;
		}

		// If there are foreign keys in the list, from a previous browse action
		updateBrowsedGrid(selectedSet
						.stream()
						.filter(file -> !Arrays.asList(knownKeys).contains(file))
						.collect(Collectors.toList()),
				selectedSet, columns, browsedSignersGridPane);
		window.sizeToScene();


		nonSignersVBox.getChildren().add(nonSignersGridPane);
		nonSignersVBox.setVisible(nonSignNumber > 0);

		final var acceptButton = new Button("ACCEPT");
		acceptButton.setStyle(WHITE_BUTTON_STYLE);
		acceptButton.setPrefWidth(200);
		acceptButton.setOnAction(event -> {
			cancelBoolean.set(false);
			window.close();
		});


		final var cancelButton = new Button("CANCEL");
		cancelButton.setStyle(BLUE_BUTTON_STYLE);
		cancelButton.setPrefWidth(200);
		cancelButton.setOnAction(event -> {
			cancelBoolean.set(true);
			window.close();
		});

		final var browseButton = new Button(("BROWSE"));
		browseButton.setStyle(WHITE_BUTTON_STYLE);
		browseButton.setPrefWidth(200);
		browseButton.setOnAction(actionEvent -> {
			updateBrowsedGrid(browsedFileAction(layout), selectedSet, columns, browsedSignersGridPane);
			window.sizeToScene();
		});

		final var buttonBox = new HBox();
		buttonBox.setAlignment(Pos.CENTER);
		buttonBox.setSpacing(10);
		buttonBox.getChildren().addAll(acceptButton, browseButton, cancelButton);

		window.setTitle("Additional Keys");
		window.sizeToScene();
		window.initModality(Modality.APPLICATION_MODAL);


		layout.setSpacing(20);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setAlignment(Pos.CENTER);
		VBox.setVgrow(layout, Priority.ALWAYS);


		layout.setStyle("-fx-font-size: 14");

		if (nonSignersVBox.isVisible()) {
			layout.getChildren().addAll(nonSignersVBox, browsedSignersVBox, buttonBox);
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

	@NotNull
	private static GridPane getGridPane(final int columns) {
		final var grid = new GridPane();
		grid.setHgap(10);
		grid.setVgap(10);
		for (var i = 0; i < columns; i++) {
			final var constraints = new ColumnConstraints();
			constraints.setPercentWidth(100.0 / columns);
			grid.getColumnConstraints().add(constraints);
		}
		return grid;
	}

	@NotNull
	private static VBox getvBox() {
		final var nonSignersVBox = new VBox();
		nonSignersVBox.setStyle("-fx-border-color: #0b9dfd; -fx-background-color: white");
		nonSignersVBox.setSpacing(5);
		nonSignersVBox.setPadding(new Insets(10));
		nonSignersVBox.managedProperty().bind(nonSignersVBox.visibleProperty());
		return nonSignersVBox;
	}

	private static void updateBrowsedGrid(final List<File> files, final Set<File> selectedSet, final int columns,
			final GridPane grid) {

		final var children = new ArrayList<>(grid.getChildren());
		for (final var file : files) {
			selectedSet.add(file);
			final var checkBox = getCheckBox(selectedSet, file);
			checkBox.setSelected(true);
			children.add(checkBox);
		}

		grid.getChildren().clear();
		var number = 0;
		for (final var checkBox : children) {
			grid.add(checkBox, (number) % columns, number / columns);
			number++;
		}
	}

	private static List<File> browsedFileAction(final Node node) {
		final var properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");
		final var keys = BrowserUtilities.browseMultiFiles(properties.getLastBrowsedDirectory(), (Pane) node, "Key",
				PK_EXTENSION);
		if (!keys.isEmpty()) {
			properties.setLastBrowsedDirectory(keys.get(0).getParentFile());
		}
		return keys;
	}

	private static CheckBox getCheckBox(final Set<File> selectedSet, final File knownKey) {
		final var baseName = FilenameUtils.getBaseName(knownKey.getName());
		final var checkBox = new CheckBox(baseName);
		checkBox.selectedProperty().addListener(new SigningKeyCheckBoxListener(selectedSet, knownKey, baseName));
		return checkBox;
	}

	private static boolean isPEM(final File pathname) {
		return pathname.getAbsolutePath().endsWith(PK_EXTENSION);
	}

}
