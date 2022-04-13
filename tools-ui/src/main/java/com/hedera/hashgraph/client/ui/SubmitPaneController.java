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

package com.hedera.hashgraph.client.ui;

import com.google.gson.JsonArray;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import javafx.fxml.FXML;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;

import static com.hedera.hashgraph.client.core.constants.Constants.SELECT_STRING;

public class SubmitPaneController implements GenericFileReadWriteAware {

	public static final String TRANSACTION_PROPERTIES =
			"tools-ui/src/main/resources/transactionProperties.json";
	private static final Logger logger = LogManager.getLogger(SubmitPaneController.class);

	@FXML
	public Controller controller;

	public GridPane mainGrid;
	public ChoiceBox<String> typeChoiceBox;
	public ComboBox<String> feePayerCombobox;
	public TextArea memoTextArea;
	public TextField transactionFeeTextField;

	private JsonArray transactionTypes;
	private boolean noise = false;

	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	void initializeSubmitPane() {
		try {
			transactionTypes = (new File(TRANSACTION_PROPERTIES).exists()) ?
					readJsonArray(TRANSACTION_PROPERTIES) :
					new JsonArray();

			setupSelectTransaction();
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}


	}


	private void setupSelectTransaction() {
		noise = true;
		typeChoiceBox.getItems().clear();
		typeChoiceBox.setItems(CreateTransactionType.names());
		typeChoiceBox.setValue(SELECT_STRING);
		noise = false;
	}
}
