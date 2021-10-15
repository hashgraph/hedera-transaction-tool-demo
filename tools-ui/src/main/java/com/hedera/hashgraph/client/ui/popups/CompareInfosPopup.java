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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.hedera.hashgraph.client.core.enums.AccountInfoFields;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import javafx.event.Event;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.jetbrains.annotations.NotNull;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;

public class CompareInfosPopup {
	public static final String ACCOUNT_ID = "accountID";
	private static Controller controller;
	private static final List<TableLine> lines = new ArrayList<>();
	private static final JsonObject accounts = readJsonObject();

	public static void setController(Controller controller) {
		CompareInfosPopup.controller = controller;
	}

	private CompareInfosPopup() {
		throw new IllegalStateException("Popup class");
	}

	public static void display(JsonObject current, JsonObject old, Controller controller) throws HederaClientException {
		var window = new Stage();
		lines.clear();
		setController(controller);
		var diff = Utilities.difference(old, current);

		var id = Identifier.parse(current.get(ACCOUNT_ID).getAsJsonObject()).toNicknameAndChecksum(accounts);

		if (!current.get(ACCOUNT_ID).getAsJsonObject().equals(old.get(ACCOUNT_ID).getAsJsonObject())) {
			throw new HederaClientException("Account ids don't match.");
		}

		window.setTitle(id);
		window.sizeToScene();
		window.initModality(Modality.APPLICATION_MODAL);

		parse(current, old);

		var continueButton = new Button("CLOSE");
		continueButton.setStyle(WHITE_BUTTON_STYLE);
		continueButton.setPrefWidth(200);
		continueButton.setOnAction(event -> window.close());
		var layout = new VBox();

		TableView<TableLine> tableView = new TableView<>();
		TableColumn<TableLine, String> keyColumn = new TableColumn<>("Info Field");
		keyColumn.setCellValueFactory(new PropertyValueFactory<>("key"));
		keyColumn.prefWidthProperty().bind(tableView.widthProperty().divide(40).multiply(7));

		TableColumn<TableLine, String> currentColumn = new TableColumn<>("Current");
		currentColumn.setCellValueFactory(new PropertyValueFactory<>("current"));
		currentColumn.prefWidthProperty().bind(tableView.widthProperty().divide(80).multiply(33));
		currentColumn.setCellFactory(tv -> getCell());

		TableColumn<TableLine, String> oldColumn = new TableColumn<>("Old");
		oldColumn.setCellValueFactory(new PropertyValueFactory<>("old"));
		oldColumn.prefWidthProperty().bind(tableView.widthProperty().divide(80).multiply(33));
		oldColumn.setCellFactory(tv -> getCell());

		tableView.setRowFactory(tableLineTableView -> getTableRow(diff));

		tableView.setSelectionModel(null);
		tableView.getColumns().addAll(keyColumn, currentColumn, oldColumn);
		tableView.getItems().addAll(lines);
		tableView.setOnSort(Event::consume);
		layout.setSpacing(20);
		layout.getChildren().addAll(tableView, continueButton);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setAlignment(Pos.CENTER);
		layout.setStyle("-fx-font-size: 14");
		layout.setMinWidth(1600);

		var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.showAndWait();
	}

	@NotNull
	private static TableRow<TableLine> getTableRow(List<String> diff) {
		return new TableRow<>() {
			@Override
			protected void updateItem(TableLine tableLine, boolean b) {
				super.updateItem(tableLine, b);
				if (tableLine == null || b) {
					setStyle("");
				} else {
					setStyle(diff.contains(tableLine.getField()) ? "-fx-background-color: lightskyblue; " : "");
				}
			}
		};
	}

	@NotNull
	private static TableCell<TableLine, String> getCell() {
		return new TableCell<>() {
			@Override
			protected void updateItem(String s, boolean b) {
				super.updateItem(s, b);
				if (s != null && !b) {
					VBox vBox = new VBox();
					String[] textList = s.split(",");
					Arrays.stream(textList)
							.filter(s1 -> !"".equals(s1.replaceAll("\\s", "")))
							.forEach(s1 -> vBox.getChildren().add(new Label(s1)));
					setGraphic(vBox);
				} else {
					setGraphic(null);
				}
			}
		};
	}


	private static void parse(JsonObject current, JsonObject old) throws HederaClientException {
		Set<String> combined = Stream.concat(current.keySet().stream(), old.keySet().stream())
				.collect(Collectors.toSet());
		for (String s : combined) {
			String oldBox = getString(old, s);
			String currentBox = getString(current, s);

			if (!"".equals(currentBox) || !"".equals(oldBox)) {
				lines.add(new TableLine(s, currentBox, oldBox));
			}
		}
	}

	private static String getString(JsonObject jsonObject, String key) throws HederaClientException {
		String currentBox = "";
		if (!jsonObject.has(key)) {
			return currentBox;
		}
		if (jsonObject.get(key).isJsonPrimitive()) {
			return jsonObject.get(key).getAsString();
		}
		if (ACCOUNT_ID.equals(key)) {
			return Identifier.parse(jsonObject.getAsJsonObject(key)).toNicknameAndChecksum(accounts);
		}
		if ("autoRenewPeriod".equals(key)) {
			return String.valueOf(new Timestamp(jsonObject.getAsJsonObject(key)).getSeconds());
		}
		if ("expirationTime".equals(key)) {
			return new Timestamp(jsonObject.getAsJsonObject(key)).asReadableLocalString();
		}
		if ("key".equals(key)) {
			final var keyJson = jsonObject.getAsJsonObject(key);
			final var printJson = controller.jsonKeyToPrettyString(keyJson);
			return printJson.replace("{", "")
					.replace("}", "")
					.replace("[", "")
					.replace("]", "")
					.replace("\n", ",");
		}
		return currentBox;
	}

	private static JsonObject readJsonObject() {
		final FileReader file;
		try {
			file = new FileReader(ACCOUNTS_MAP_FILE);
		} catch (FileNotFoundException e) {
			return new JsonObject();
		}
		return JsonParser.parseReader(file).getAsJsonObject();
	}

	public static class TableLine {
		private String key;
		private String current;
		private String old;
		private final String field;

		public TableLine(String key, String current, String old) {
			this.field = key;
			this.key = AccountInfoFields.valueOf(key).getName();
			this.current = current;
			this.old = old;
		}

		public String getField() {
			return field;
		}

		public String getKey() {
			return key;
		}

		public void setKey(String key) {
			this.key = key;
		}

		public String getCurrent() {
			return current;
		}

		public void setCurrent(String current) {
			this.current = current;
		}

		public String getOld() {
			return old;
		}

		public void setOld(String old) {
			this.old = old;
		}
	}
}
