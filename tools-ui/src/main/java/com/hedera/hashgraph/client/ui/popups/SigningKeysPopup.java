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
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Set;
import java.util.stream.Collectors;

public class SigningKeysPopup {

	private static final Logger logger = LogManager.getLogger(SigningKeysPopup.class);

	private static final int PREF_BUTTON_WIDTH = 100;
	public static final String LEGEND_1 = "The following keys will be used to sign the transaction.";
	public static final String LEGEND_2 =
			"Please review this list and if it is correct, please click \"CONTINUE\". If keys are missing, please " +
					"click \"ADD MORE\". Lastly, click \"CANCEL\" to go back to the transaction creation page.";

	private static Boolean answer = null;

	private SigningKeysPopup() {
		throw new IllegalStateException("Popup class");
	}

	public static Boolean display(final Set<File> keys) {
		final var window = new Stage();
		window.setTitle("Signing keys");
		window.sizeToScene();
		window.setMaxWidth(500);
		window.initModality(Modality.APPLICATION_MODAL);
		final var layout = new VBox();
		layout.setMaxWidth(460);
		layout.setSpacing(20);
		layout.setPadding(new Insets(20));
		layout.getChildren().add(formatLabel(LEGEND_1, 16));
		final var signerTable = new VBox();
		signerTable.getChildren().clear();
		final var names =
				keys.stream().map(key -> FilenameUtils.getBaseName(key.getName())).sorted().collect(Collectors.toList());
		for (final var name : names) {
			signerTable.getChildren().add(formatLabel("\t\u2022 " + name, 14));
		}
		layout.getChildren().add(signerTable);
		layout.getChildren().add(formatLabel(LEGEND_2, 16));
		// Reset the answer everytime the popup is used
		answer = null;
		final var continueButton = new Button(Constants.BUTTON_CONTINUE);
		continueButton.setStyle(Constants.WHITE_BUTTON_STYLE);
		continueButton.setOnAction(actionEvent -> {
			logger.info("Keys accepted");
			answer = Boolean.TRUE;
			window.close();
		});
		final var addButton = new Button(Constants.BUTTON_ADD);
		addButton.setStyle(Constants.WHITE_BUTTON_STYLE);
		addButton.setOnAction(actionEvent -> {
			logger.info("Add more keys");
			answer = Boolean.FALSE;
			window.close();
		});
		final var cancelButton = new Button(Constants.BUTTON_CANCEL);
		cancelButton.setStyle(Constants.WHITE_BUTTON_STYLE);
		cancelButton.setOnAction(actionEvent -> {
			logger.info("Keys rejected");
			answer = null;
			window.close();
		});

		final var buttonBar = new ButtonBar();
		buttonBar.getButtons().addAll(cancelButton, addButton, continueButton);
		buttonBar.setButtonMinWidth(PREF_BUTTON_WIDTH);
		layout.getChildren().add(buttonBar);

		final var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		window.showAndWait();
		return answer;
	}

	private static Label formatLabel(final String legend, final int size) {
		final var label0 = new Label(legend);
		label0.setWrapText(true);
		label0.setStyle("-fx-font-size: " + size);
		return label0;
	}

}
