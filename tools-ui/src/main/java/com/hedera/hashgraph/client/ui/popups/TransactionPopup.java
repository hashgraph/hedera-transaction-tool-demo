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
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static com.hedera.hashgraph.client.core.constants.Constants.BUTTON_CANCEL;

public class TransactionPopup {
	private static final Logger logger = LogManager.getLogger(TransactionPopup.class);
	private static boolean response;

	private TransactionPopup() {
		throw new IllegalStateException("Popup class");
	}

	public static boolean display(final RemoteFile remoteFile) {
		final var window = new Stage();
		window.setTitle("Transaction");
		window.sizeToScene();
		window.initModality(Modality.APPLICATION_MODAL);
		final var layout = new VBox();
		layout.setPadding(new Insets(20));
		layout.setSpacing(20);

		final var label = new Label("The following transaction is ready to be submitted. Please review and continue.");
		label.setWrapText(true);
		layout.getChildren().add(label);

		final var gridPane = remoteFile.buildGridPane();
		gridPane.setMinWidth(550);
		final var frame = remoteFile.buildFileVBox(false);
		frame.getChildren().add(gridPane);
		layout.getChildren().add(frame);

		final var continueButton = new Button("SUBMIT");
		continueButton.setStyle(Constants.WHITE_BUTTON_STYLE);
		continueButton.setOnAction(actionEvent -> {
			logger.info("Transaction accepted");
			response = true;
			window.close();
		});
		final var cancelButton = new Button(BUTTON_CANCEL);
		cancelButton.setStyle(Constants.WHITE_BUTTON_STYLE);
		cancelButton.setOnAction(actionEvent -> {
			logger.info("Transaction rejected");
			response = false;
			window.close();
		});

		final var buttonBar = new ButtonBar();
		buttonBar.setButtonMinWidth(100);
		buttonBar.getButtons().addAll(cancelButton, continueButton);

		layout.getChildren().add(buttonBar);

		final var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		window.showAndWait();
		return response;
	}


}
