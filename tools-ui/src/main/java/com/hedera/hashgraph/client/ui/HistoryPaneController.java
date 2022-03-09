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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.google.gson.JsonArray;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.ui.utilities.HistoryData;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Control;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.util.Callback;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.table.TableRowExpanderColumn;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Map;
import java.util.function.Predicate;

import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.FILTER_TOOLTIP_TEXT;
import static com.hedera.hashgraph.client.core.enums.FileType.COMMENT;
import static com.hedera.hashgraph.client.core.enums.FileType.METADATA;

public class HistoryPaneController implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(HistoryPaneController.class);
	private static final ObservableMap<Integer, HistoryData> historyMap = FXCollections.observableHashMap();
	private static final String HISTORY_MAP = Constants.SYSTEM_FOLDER + File.separator + "historyMap.json";
	public static final String RESET_ICON = "icons/icons8-reset-48.png";
	public static final String FILTER_ICON = "icons/filter.png";
	private final ObservableList<FileType> filterOut = FXCollections.observableArrayList();
	private final ObservableList<HistoryData> tableList = FXCollections.observableArrayList();
	private final FilteredList<HistoryData> filteredList = new FilteredList<>(tableList, p -> true);

	private boolean noise = true;

	public StackPane historyPane;
	public ScrollPane contentScrollPane;
	public VBox filterVBox;

	@FXML
	private Controller controller;

	// region INITIALIZATION
	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	void initializeHistoryPane() {
		try {
			setupMapListener();
			loadMap();
			setupFilterBox();
			contentScrollPane.setContent(setupTable());
			contentScrollPane.setFitToWidth(true);
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	private void setupFilterBox() throws HederaClientException {
		filterVBox.managedProperty().bind(filterVBox.visibleProperty());
		filterVBox.setVisible(false);
		final var size = filterOut.size();
		final var filterTitle = size > 0 ? String.format("filters (%d)", size) : "filters";
		final var title = new Label(filterTitle);
		title.setStyle("-fx-border-color: transparent;-fx-background-color: transparent");
		title.setPadding(new Insets(5));

		final var image = new Image("icons" + File.separator + "helpIcon.png");
		final var imageView = new ImageView(image);
		imageView.setPreserveRatio(true);
		imageView.setFitHeight(15);
		final var toolTipButton = getToolTipButton(imageView);

		final var titleBox = new HBox();
		titleBox.getChildren().addAll(title, toolTipButton);

		final var vBox = new VBox();
		vBox.setPadding(new Insets(5));
		vBox.setVisible(true);

		vBox.managedProperty().bind(vBox.visibleProperty());
		final var gridPane = getCheckboxesGridPane();
		vBox.getChildren().add(gridPane);

		filterVBox.getChildren().clear();
		filterVBox.getChildren().addAll(titleBox, vBox);
		filterVBox.setStyle("-fx-border-color: gray; -fx-border-radius: 10");
	}

	@NotNull
	private GridPane getCheckboxesGridPane() throws HederaClientException {
		final var gridPane = new GridPane();
		gridPane.setHgap(5);
		gridPane.setVgap(5);
		var counter = 0;
		noise = true;
		for (final var type : EnumSet.allOf(FileType.class)) {

			final var typeCounter = countType(type);
			final var typeString = type.toKind().toLowerCase();
			if (!"".equals(typeString)) {
				final var checkBox = new CheckBox(String.format("%s (%d)", typeString, typeCounter));
				checkBox.setSelected(true);
				filterOut.add(type);

//				checkBox.selectedProperty().addListener(
//						(observableValue, oldValue, newValue) -> {
//							filteredList.setPredicate(new Predicate<HistoryData>() {
//								@Override
//								public boolean test(HistoryData historyData) {
//									return newValue && historyData.getType().equals(type);
//								}
//							});
//						});
				checkBox.selectedProperty().addListener((observableValue, aBoolean, t1) -> {
					if (Boolean.FALSE.equals(t1)) {
						filterOut.remove(type);
					} else {
						filterOut.add(type);
					}
				});


				gridPane.add(checkBox, counter % 3, counter / 3);
				counter++;
			}
		}
		gridPane.setVgap(10);
		gridPane.setHgap(10);
		noise = false;
		return gridPane;
	}

	private void checkBoxListenerAction(final FileType type, final Boolean newValue) {

	}

	private int countType(final FileType type) throws HederaClientException {
		var count = 0;
		for (final var value : historyMap.values()) {
			if (type.equals(value.getType())) {
				count++;
			}
		}
		return count;
	}

	@NotNull
	private Button getToolTipButton(final ImageView imageView) {
		final var toolTipButton = new Button();
		toolTipButton.setGraphic(imageView);
		toolTipButton.setStyle("-fx-background-color: transparent; -fx-border-color: transparent");
		toolTipButton.setPadding(new Insets(0, 0, 10, 0));
		toolTipButton.setMinWidth(25);

		toolTipButton.setOnAction(
				actionEvent -> Utilities.showTooltip(controller.homePane, toolTipButton, FILTER_TOOLTIP_TEXT));
		return toolTipButton;
	}

	private TableView<HistoryData> setupTable() {
		final var table = new TableView<HistoryData>();
		final var refresh = formatButton(FILTER_ICON);
		refresh.setOnAction(actionEvent -> showFilters());
		final var title = new HBox();
		title.getChildren().add(new Label("Title"));
		title.getChildren().add(refresh);
		title.setSpacing(5);
		title.setAlignment(Pos.CENTER_RIGHT);
		final var titleColumn = new TableColumn<HistoryData, String>("");
		titleColumn.setGraphic(title);
		titleColumn.setCellValueFactory(new PropertyValueFactory<>("title"));
		titleColumn.prefWidthProperty().bind(table.widthProperty().divide(5));

		final var commentColumn = new TableColumn<HistoryData, String>("Comment");
		commentColumn.setCellValueFactory(new PropertyValueFactory<>("lastComment"));
		commentColumn.prefWidthProperty().bind(table.widthProperty().divide(5));
		commentColumn.setCellFactory(historyDataStringTableColumn -> setWrapping());

		final var lastAction = new TableColumn<HistoryData, String>("Latest Action");
		lastAction.setCellValueFactory(new PropertyValueFactory<>("lastAction"));
		lastAction.prefWidthProperty().bind(table.widthProperty().divide(5));
		lastAction.setCellFactory(historyDataStringTableColumn -> setWrapping());

		final var prepared = new TableColumn<HistoryData, String>("Prepared by:");
		prepared.setCellValueFactory(new PropertyValueFactory<>("preparedBy"));
		prepared.prefWidthProperty().bind(table.widthProperty().divide(5));
		prepared.setCellFactory(historyDataStringTableColumn -> setWrapping());

		final var expanderColumn = getExpanderColumn();
		final var reSignColumn = getReSignColumn(table);

		table.getColumns().add(expanderColumn);
		table.getColumns().add(titleColumn);
		table.getColumns().add(prepared);
		table.getColumns().add(commentColumn);
		table.getColumns().add(lastAction);
		table.getColumns().add(reSignColumn);

		filterOut.addListener((ListChangeListener<FileType>) change -> {
			while (change.next()) {
				if (!noise && (change.wasAdded() || change.wasRemoved())) {
					filteredList.setPredicate(statesModel -> {
						if (!noise) {
							if (change.wasAdded() || change.wasRemoved()) {
								return filterOut.contains(statesModel.getType());
							}
						}
						return false;
					});
				}
			}
		});

		tableList.addAll(historyMap.values());
		SortedList<HistoryData> sortedList = new SortedList<>(filteredList);
		table.setItems(sortedList);

		return table;

	}

	private void showFilters() {
		filterVBox.setVisible(!filterVBox.isVisible());
	}

	@NotNull
	private TableCell<HistoryData, String> setWrapping() {
		final var cell = new TableCell<HistoryData, String>();
		final var text = new Text();
		cell.setGraphic(text);
		cell.setPrefHeight(Control.USE_COMPUTED_SIZE);
		text.wrappingWidthProperty().bind(cell.widthProperty());
		text.textProperty().bind(cell.itemProperty());
		return cell;
	}

	private TableRowExpanderColumn<HistoryData> getExpanderColumn() {
		final var expanderColumn = new TableRowExpanderColumn<>(this::buildAccountVBox);
		expanderColumn.setStyle("-fx-alignment: TOP-CENTER; -fx-padding: 10");
		return expanderColumn;

	}

	private VBox buildAccountVBox(final TableRowExpanderColumn.TableRowDataFeatures<HistoryData> parameter) {
		final var data = parameter.getValue();
		final var returnBox = new VBox();
		try {
			final var remoteFile =
					new RemoteFile().getSingleRemoteFile(FileDetails.parse(new File(data.getRemoteFilePath())));
			remoteFile.setHistory(true);
			return remoteFile.buildDetailsBox();
		} catch (final HederaClientException | IOException e) {
			logger.error(e);
		}
		final var l = new Label(data.getFileName());
		returnBox.getChildren().add(l);
		return returnBox;
	}

	private void setupMapListener() {
		historyMap.addListener((MapChangeListener<Integer, HistoryData>) change -> {
			if (!noise) {
				storeMap();
			}
		});
	}

	private void storeMap() {
		// Store map
		logger.info("Storing map to {}", HISTORY_MAP);
		final var array = new JsonArray();
		for (final var entry : historyMap.entrySet()) {
			array.add(entry.getValue().asJson());
		}

		try {
			writeJsonObject(HISTORY_MAP, array);
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	private void loadMap() {
		try {
			final var mapFile = new File(HISTORY_MAP);
			if (!mapFile.exists()) {
				logger.info("Parsing history folder");
				parseHistoryFolder();
				return;
			}
			final var array = readJsonArray(mapFile.getAbsolutePath());
			for (final var element : array) {
				final var historyData = new HistoryData(element.getAsJsonObject());
				historyMap.put(historyData.getCode(), historyData);
			}
		} catch (final FileNotFoundException | HederaClientException | JsonProcessingException e) {
			logger.error(e.getMessage());
		}
	}

	private void parseHistoryFolder() throws FileNotFoundException {
		noise = true;
		final var files = getRemoteFiles();
		for (final var file : files) {
			if (METADATA.equals(file.getType()) || COMMENT.equals(file.getType())) {
				continue;
			}
			logger.info("Parsing file {}", file.getName());
			final var data = new HistoryData(file);
			data.setHistory(true);
			historyMap.put(data.getCode(), data);
		}
		storeMap();
		noise = false;
	}

	@NotNull
	private ArrayList<RemoteFile> getRemoteFiles() {
		final var filenames = new File(Constants.DEFAULT_HISTORY).listFiles((dir, name) -> isAllowedFile(name));
		final var remoteFiles = new ArrayList<RemoteFile>();
		for (final var filename : filenames) {
			FileDetails details = null;
			try {
				details = FileDetails.parse(filename);
			} catch (final IOException e) {
				logger.error(e.getMessage());
			}
			try {
				remoteFiles.add(new RemoteFile().getSingleRemoteFile(details));
			} catch (final HederaClientException e) {
				logger.error(e.getMessage());
			}
		}
		return remoteFiles;
	}

	private boolean isAllowedFile(final String name) {
		try {
			return !FileType.getType(FilenameUtils.getExtension(name)).equals(FileType.UNKNOWN);
		} catch (final HederaClientException e) {
			return false;
		}
	}
	// endregion

	// region GETTERS
	public Map<Integer, HistoryData> getHistoryMap() {
		return historyMap;
	}

	public boolean inMap(final int code) {
		return historyMap.containsKey(code);
	}
	// endregion

	private TableColumn<HistoryData, String> getReSignColumn(final TableView<HistoryData> table) {
		final var actionColumn = new TableColumn<HistoryData, String>("");
		actionColumn.setCellValueFactory(new PropertyValueFactory<>(""));
		final var cellFactory =
				(Callback<TableColumn<HistoryData, String>, TableCell<HistoryData, String>>) column -> new TableCell<>() {
					final Button button = formatButton(RESET_ICON);

					@Override
					public void updateItem(final String item, final boolean empty) {
						setText(null);
						if (!empty) {
							if (controller.getSetupPhase().equals(SetupPhase.TEST_PHASE)) {
								final var x = table.getItems().get(getIndex());
								button.setText(x.getFileName() + "T");
								button.setStyle("-fx-font-size: 2");
							}
							final var historyData = getTableView().getItems().get(getIndex());
							button.setVisible(!historyData.isExpired());
							button.setOnAction(actionEvent -> setHistoryDataAction(historyData));
							setGraphic(button);
							return;
						}
						setGraphic(null);
					}

					private void setHistoryDataAction(final HistoryData historyData) {
						logger.info("Deleting {}", historyData.getFileName());
						historyData.setHistory(false);
						noise = false;
						historyMap.remove(historyData.getCode());
						historyMap.put(historyData.getCode(), historyData);
						table.getItems().remove(getIndex());
						noise = true;
						controller.homePaneController.setForceUpdate(true);
					}
				};
		actionColumn.setCellFactory(cellFactory);
		return actionColumn;
	}

	/**
	 * Setups a button with an icon
	 *
	 * @param imageLocation
	 * 		the relative location of the image of the icon to be used
	 * @return a button with the image provided
	 */
	private Button formatButton(final String imageLocation) {
		final var button = new Button();
		final var imageView = new ImageView(new Image(imageLocation));
		imageView.setFitHeight(20);
		imageView.setPreserveRatio(true);
		button.setGraphic(imageView);
		button.setStyle("-fx-background-color: transparent; -fx-border-color: transparent");
		return button;
	}

}
