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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.ui.popups;

import com.hedera.hashgraph.client.ui.utilities.ResponseEnum;
import com.hedera.hashgraph.client.ui.utilities.ResponseTuple;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.util.concurrent.atomic.AtomicReference;

/**
 * This class introduces a popup with 2 buttons, to be used when loading info files.
 * The options are ACCEPT (accepts the file and uses the nickname chosen in the text field), DECLINE (does not import the
 * file)
 * The popup also contains a checkbox that will only be shown if there are more of one file that needs to be acted upon.
 * If the checkbox is checked, then all files will have the same action.
 */
public class TwoButtonPopup {
	private static ResponseEnum responseEnum = ResponseEnum.UNKNOWN;
	protected static final String MESSAGE = "Please enter a nickname for account %s";

	public static ResponseTuple display(final String accountIDString, boolean multiple) {
		var proposedNick = new AtomicReference<String>();
		proposedNick.set(accountIDString);

		var window = new Stage();

		window.setTitle("Import new account");
		window.sizeToScene();
		window.setMaxWidth(500);
		window.initModality(Modality.APPLICATION_MODAL);
		var layout = new VBox();
		layout.setSpacing(20);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setAlignment(Pos.CENTER);
		layout.setStyle("-fx-font-size: 14");

		var text = new Label(String.format(MESSAGE, accountIDString));

		var checkBox = new CheckBox("Apply to all");
		checkBox.setVisible(multiple);

		var answerField = new TextField(proposedNick.get());


		answerField.setPrefWidth(300);
		answerField.setOnKeyPressed(event -> {
			if (event.getCode().equals(KeyCode.ENTER)) {
				responseEnum = checkBox.isSelected() ? ResponseEnum.REPLACE_ALWAYS : ResponseEnum.REPLACE_ONCE;
				proposedNick.set(answerField.getText());
				window.close();
			}
		});

		var buttonBox = new HBox();

		var ignore = new Button("IGNORE");
		ignore.setOnAction(actionEvent -> {
			responseEnum = checkBox.isSelected() ? ResponseEnum.IGNORE_ALWAYS : ResponseEnum.IGNORE_ONCE;
			window.close();
		});
		var replace = new Button("ACCEPT");
		replace.setOnAction(actionEvent -> {
			responseEnum = checkBox.isSelected() ? ResponseEnum.REPLACE_ALWAYS : ResponseEnum.REPLACE_ONCE;
			proposedNick.set(answerField.getText());
			window.close();
		});


		setButtonStyle(ignore, replace);

		buttonBox.setSpacing(5);
		buttonBox.setAlignment(Pos.CENTER);
		buttonBox.getChildren().addAll(ignore, replace);

		var choiceBox = new HBox();
		choiceBox.setSpacing(20);
		choiceBox.getChildren().addAll(checkBox, buttonBox);
		choiceBox.setAlignment(Pos.CENTER);

		layout.getChildren().addAll(text, answerField, choiceBox);

		var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();

		return new ResponseTuple(responseEnum, proposedNick.get());
	}

	private static void setButtonStyle(Button... buttons) {
		for (var button : buttons) {
			button.setStyle(
					"-fx-background-color: white; -fx-border-color: #0b9dfd;  -fx-border-radius: 10; " +
							"-fx-background-radius: 10;");
			button.setPrefWidth(100);
		}
	}

}
