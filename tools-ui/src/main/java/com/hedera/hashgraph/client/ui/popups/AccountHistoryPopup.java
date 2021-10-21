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
import com.hedera.hashgraph.client.core.enums.AccountInfoFields;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.AccountId;
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
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.Format;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TreeMap;
import java.util.stream.Collectors;

import static com.google.gson.JsonParser.parseReader;
import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;

public class AccountHistoryPopup {
	private static final Logger logger = LogManager.getLogger(AccountHistoryPopup.class);
	private static final TreeMap<Long, JsonObject> history = new TreeMap<>();

	private static Controller controller;

	private AccountHistoryPopup() {
		throw new IllegalStateException("Popup class");
	}

	public static void setController(Controller controller) {
		AccountHistoryPopup.controller = controller;
	}

	public static void display(AccountId accountId, Controller controller) {
		history.clear();
		setController(controller);
		try {
			final var name = new Identifier(accountId).toReadableString();
			final var current = readJson(new File(ACCOUNTS_INFO_FOLDER, name + "." + JSON_EXTENSION));
			final var lines = getTableLines(accountId, current);

			var window = new Stage();

			window.setTitle(String.format("Account %s history", name));
			window.sizeToScene();
			window.setMaxWidth(600);
			window.initModality(Modality.APPLICATION_MODAL);

			TableView<TableLine> tableView = getTableLineTableView(current, lines);


			var continueButton = new Button("CLOSE");
			continueButton.setStyle(WHITE_BUTTON_STYLE);
			continueButton.setPrefWidth(200);
			continueButton.setOnAction(event -> window.close());
			var layout = new VBox();

			layout.setSpacing(20);
			layout.getChildren().add(tableView);
			layout.getChildren().add(continueButton);
			layout.setPadding(new Insets(20, 20, 20, 20));
			layout.setAlignment(Pos.CENTER);
			layout.setStyle("-fx-font-size: 14");
			layout.setMinWidth(500);

			var scene = new Scene(layout);
			scene.getStylesheets().add("tools.css");

			window.setScene(scene);

			window.showAndWait();
		} catch (IOException e) {
			logger.error(e.getMessage());
		}


	}

	@NotNull
	private static TableView<TableLine> getTableLineTableView(JsonObject current, List<TableLine> lines) {
		TableView<TableLine> tableView = new TableView<>();
		TableColumn<TableLine, String> dateColumn = new TableColumn<>("Date");
		dateColumn.setCellValueFactory(new PropertyValueFactory<>("date"));
		dateColumn.prefWidthProperty().bind(tableView.widthProperty().divide(3));
		TableColumn<TableLine, String> differencesColumn = new TableColumn<>("Fields changed");
		differencesColumn.setCellValueFactory(new PropertyValueFactory<>("differences"));
		differencesColumn.setCellFactory(tv -> getTableCell());
		differencesColumn.prefWidthProperty().bind(tableView.widthProperty().divide(3).multiply(2));

		tableView.setRowFactory(r -> {
			final TableRow<TableLine> row = new TableRow<>() {
			};

			row.setOnMouseClicked(mouseEvent -> doubleClickEvent(current, row, mouseEvent));
			return row;
		});

		tableView.getColumns().add(dateColumn);
		tableView.getColumns().add(differencesColumn);
		tableView.getItems().addAll(lines);
		tableView.setPrefWidth(Region.USE_COMPUTED_SIZE);
		return tableView;
	}

	private static void doubleClickEvent(JsonObject current, TableRow<TableLine> row, MouseEvent mouseEvent) {
		if (mouseEvent.getClickCount() != 2 || row.isEmpty()) {
			return;
		}
		var rowData = row.getItem();
		try {
			CompareInfosPopup.display(current, history.get(rowData.getSeconds()), controller);
		} catch (HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	@NotNull
	private static TableCell<TableLine, String> getTableCell() {
		return new TableCell<>() {
			@Override
			protected void updateItem(String s, boolean b) {
				super.updateItem(s, b);
				if (s != null && !b) {
					VBox vBox = new VBox();
					String[] textList = s.split(",");
					for (String s1 : textList) {
						vBox.getChildren().add(new Label(s1));
					}
					setGraphic(vBox);
				} else {
					setGraphic(null);
				}
			}
		};
	}

	@NotNull
	private static List<TableLine> getTableLines(AccountId accountId, JsonObject current) throws IOException {
		getHistory(accountId);

		List<TableLine> lines = new ArrayList<>();
		for (var entry : history.descendingKeySet()) {
			var oldInfo = history.get(entry);
			var diff = Utilities.difference(oldInfo, current);
			diff.remove("balance");
			List<String> titles =
					diff.stream().map(s -> AccountInfoFields.valueOf(s.toUpperCase(Locale.ROOT)).getName()).collect(
							Collectors.toList());
			var message = diff.isEmpty() ? "No difference" : String.join(",", titles);
			lines.add(new TableLine(entry, message));
		}
		return lines;
	}

	private static void getHistory(AccountId accountId) throws IOException {
		var name = new Identifier(accountId).toReadableString();
		var archive = new File(ACCOUNTS_INFO_FOLDER, "Archive").listFiles(
				(dir, filename) -> filename.contains(name + "_") && filename.endsWith(JSON_EXTENSION));

		if (archive != null && archive.length > 0) {
			for (var file : archive) {
				final var seconds = getSeconds(file);
				if (seconds > 0) {
					history.put(seconds, readJson(file));
				}
			}
		}
	}

	private static long getSeconds(File file) {
		final var archiveName = FilenameUtils.getBaseName(file.getName());
		try {
			final String substring = archiveName.substring(archiveName.lastIndexOf("_") + 1);
			return Long.parseLong(substring);
		} catch (Exception e) {
			logger.error(e.getMessage());
		}
		return -1;
	}

	private static JsonObject readJson(File file) throws IOException {
		final var reader = new FileReader(file);
		return parseReader(reader).getAsJsonObject();
	}

	public static class TableLine {
		private String date;
		private String differences;
		private final Long seconds;

		public TableLine(Long entry, String message) {
			var entryDate = new Date(entry);
			Format format = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
			this.seconds = entry;
			this.date = format.format(entryDate);
			this.differences = message;
		}

		public String getDate() {
			return date;
		}

		public void setDate(String date) {
			this.date = date;
		}

		public String getDifferences() {
			return differences;
		}

		public void setDifferences(String differences) {
			this.differences = differences;
		}

		public Long getSeconds() {
			return seconds;
		}
	}
}
