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

import com.hedera.hashgraph.client.ui.utilities.ResponseEnum;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.commons.io.FilenameUtils;

import java.io.File;

/**
 * This class introduces a popup with 3 buttons, to be used when replacing a file.
 * The options are: IGNORE (do nothing), KEEP BOTH (keeps the old file and the new file, renaming the new one if
 * necessary), and REPLACE (deletes the old file and keeps the new one)
 * The popup also contains a checkbox that will only be shown if there are more of one file that needs to be acted upon.
 * If the checkbox is checked, then all files will have the same action.
 */
public class ThreeButtonPopup {
	protected static final String MESSAGE =
			"A key named %s has a duplicate in the Keys folder.\nDo you want to replace it with the one " +
					"you're importing?";
	protected static final String PUB_EXTENSION = "pub";
	protected static final String INFO_EXTENSION = "info";
	private static ResponseEnum responseEnum = ResponseEnum.UNKNOWN;

	public static ResponseEnum display(final File file, final boolean multipleFiles) {
		return display(file, MESSAGE, true, multipleFiles);
	}

	private ThreeButtonPopup() {
		throw new IllegalStateException("Utility class");
	}

	public static ResponseEnum display(final File file, final String message, final boolean allowKeep,
			final boolean multipleFiles) {

		final var window = new Stage();

		final var extension = FilenameUtils.getExtension(file.getName());
		final String itemName;

		switch (extension) {
			case PUB_EXTENSION:
				itemName = " key";
				break;
			case INFO_EXTENSION:
				itemName = " account";
				break;
			default:
				itemName = "";
				break;
		}

		window.setTitle("Replace duplicate" + itemName);
		window.sizeToScene();
		window.setMaxWidth(500);
		window.initModality(Modality.APPLICATION_MODAL);
		final var layout = new VBox();
		layout.setSpacing(20);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setAlignment(Pos.CENTER);
		layout.setStyle("-fx-font-size: 14");

		final var checkBox = new CheckBox("Apply to all");

		final var buttonBox = new HBox();
		final var keepBoth = new Button("Keep both");
		keepBoth.setOnAction(actionEvent -> {
			responseEnum = checkBox.isSelected() ? ResponseEnum.KEEP_BOTH_ALWAYS : ResponseEnum.KEEP_BOTH_ONCE;
			window.close();
		});
		final var ignore = new Button("Ignore");
		ignore.setOnAction(actionEvent -> {
			responseEnum = checkBox.isSelected() ? ResponseEnum.IGNORE_ALWAYS : ResponseEnum.IGNORE_ONCE;
			window.close();
		});
		final var replace = new Button("Replace");
		replace.setOnAction(actionEvent -> {
			responseEnum = checkBox.isSelected() ? ResponseEnum.REPLACE_ALWAYS : ResponseEnum.REPLACE_ONCE;
			window.close();
		});

		setButtonStyle(keepBoth, ignore, replace);

		buttonBox.setSpacing(5);
		buttonBox.setAlignment(Pos.CENTER);


		if (allowKeep) {
			buttonBox.getChildren().add(keepBoth);
		}
		buttonBox.getChildren().add(ignore);
		buttonBox.getChildren().add(replace);

		final var choiceBox = new HBox();
		choiceBox.setSpacing(20);
		choiceBox.getChildren().addAll(checkBox, buttonBox);
		choiceBox.setAlignment(Pos.CENTER);

		final var explanation = new Label(String.format(message, FilenameUtils.getBaseName(file.getName())));
		explanation.setWrapText(true);

		if (multipleFiles) {
			layout.getChildren().addAll(explanation, choiceBox);
		} else {
			layout.getChildren().addAll(explanation, buttonBox);
		}

		final var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();

		return responseEnum;
	}

	private static void setButtonStyle(final Button... buttons) {
		for (final var button : buttons) {
			button.setStyle(
					"-fx-background-color: white; -fx-border-color: #0b9dfd;  -fx-border-radius: 10; " +
							"-fx-background-radius: 10;");
			button.setPrefWidth(100);
		}
	}

}
