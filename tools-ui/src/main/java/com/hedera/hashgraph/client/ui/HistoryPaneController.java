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
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.ui.utilities.HistoryData;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleStringProperty;
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
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.util.Callback;
import javafx.util.StringConverter;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.table.TableRowExpanderColumn;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Predicate;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.HISTORY_MAP_JSON;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_SYSTEM_FOLDER;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.FILTER_TOOLTIP_TEXT;
import static com.hedera.hashgraph.client.core.enums.FileType.COMMENT;
import static com.hedera.hashgraph.client.core.enums.FileType.METADATA;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.parseAccountNumbers;

public class HistoryPaneController implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(HistoryPaneController.class);
	private static final ObservableMap<Integer, HistoryData> historyMap = FXCollections.observableHashMap();
	private static final String HISTORY_MAP = DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON;
	public static final String RESET_ICON = "icons/sign-back.png";
	public static final String FILTER_ICON = "icons/filter.png";
	private final ObservableList<FileType> typeFilter = FXCollections.observableArrayList();
	private final ObservableList<Actions> actionsFilter = FXCollections.observableArrayList();
	private final ObservableList<HistoryData> tableList = FXCollections.observableArrayList();
	private final FilteredList<HistoryData> filteredList = new FilteredList<>(tableList, p -> true);

	private boolean noise = true;

	public StackPane historyPane;
	public ScrollPane contentScrollPane;

	public Button feePayerIDTooltip;
	public TextField feePayerTextField;

	public CheckBox acceptedCheckBox;
	public CheckBox declinedCheckBox;

	public VBox typeFilterVBox;
	public VBox feePayerFilterVBox;
	public VBox actedDateFilterVBox;
	public VBox expirationDateFilterVBox;

	public DatePicker expirationStartDatePicker;
	public DatePicker expirationEndDatePicker;
	public DatePicker actionsStartDatePicker;
	public DatePicker actionsEndDatePicker;

	public Button rebuild;


	public LocalDate start = LocalDate.now();
	public LocalDate end = LocalDate.now();

	private final ObservableList<Predicate<HistoryData>> filters = FXCollections.observableArrayList();
	private Predicate<HistoryData> feePayerPredicate;
	private Predicate<HistoryData> typePredicate;
	private Predicate<HistoryData> actionDatePredicate;
	private Predicate<HistoryData> actionTypePredicate;
	private Predicate<HistoryData> expirationDatePredicate;

	private final String dateFormat = Locale.getDefault().equals(Locale.US) ? "MM/dd/yyyy" : "dd/MM/yyyy";

	@FXML
	private Controller controller;

	// region GETTERS and SETTERS

	/**
	 * Get the history file map
	 *
	 * @return the history map
	 */
	public Map<Integer, HistoryData> getHistoryMap() {
		if (historyMap.isEmpty()) {
			loadMap();
		}
		return historyMap;
	}

	/**
	 * Clears the contents of the history map only valid on TEST
	 */
	public void clearHistoryMap() {
		if (controller.getSetupPhase().equals(SetupPhase.TEST_PHASE)) {
			noise= true;
			historyMap.clear();
			noise= false;
		}
		storeMap();
	}

	/**
	 * Adds a file to the map
	 *
	 * @param path
	 * 		the path to the new file
	 */
	public void addToHistory(final String path) {
		try {
			noise = false;
			final var addition = new HistoryData(path);
			addition.setHistory(true);
			historyMap.remove(addition.getCode());
			historyMap.put(addition.getCode(), addition);
		} catch (final IOException | HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * Loads all the remote files from the history folder.
	 *
	 * @return an array of remote files
	 */
	@NotNull
	private ArrayList<RemoteFile> getRemoteFiles() {
		final var filenames = new File(DEFAULT_HISTORY).listFiles((dir, name) -> isAllowedFile(name));
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

	// endregion

	// region INITIALIZATION
	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	void initializeHistoryPane() {
		rebuild.managedProperty().bind(rebuild.visibleProperty());
		typeFilter.addAll(EnumSet.allOf(FileType.class));
		actionsFilter.addAll(Actions.ACCEPT, Actions.DECLINE);

		setupMapListener();
		setupPredicates();
		if (historyMap.isEmpty()) {
			loadMap();
		}
		setupTypeFilterBox();
		setupFeePayerFilterBox();
		setupExpirationDateBox();
		setupActionDateBox();
		contentScrollPane.setContent(setupTable());
		contentScrollPane.setFitToWidth(true);

		formatDatePicker(expirationStartDatePicker, expirationEndDatePicker, actionsStartDatePicker,
				actionsEndDatePicker);
	}

	// endregion

	// region FILTERS

	/**
	 * Set up the predicates
	 */
	private void setupPredicates() {
		feePayerPredicate = historyData -> {
			final var accounts = parseAccountNumbers(feePayerTextField.getText(), controller.getCurrentNetwork());
			return accounts.isEmpty() || accounts.contains(Identifier.parse(historyData.getFeePayer()).asAccount());
		};

		expirationDatePredicate = historyData -> {
			final var date = historyData.getExpirationLocalDate();
			final var b1 = date.isEqual(start) || date.isAfter(start);
			final var b2 = date.isEqual(end) || date.isBefore(end);
			return b1 && b2;
		};

		actionDatePredicate = historyData -> {
			final var date = historyData.getActionLocalDate();
			final var b1 = date.isEqual(start) || date.isAfter(start);
			final var b2 = date.isEqual(end) || date.isBefore(end);
			return b1 && b2;
		};
	}

	private void setupTypeFilterBox() {
		typeFilterVBox.managedProperty().bind(typeFilterVBox.visibleProperty());
		typeFilterVBox.setVisible(false);
		final var size = typeFilter.size();
		final var filterTitle = size > 0 ? String.format("filters (%d)", size) : "filters";
		final var title = new Label(filterTitle);
		title.setStyle("-fx-border-color: transparent;-fx-background-color: transparent");
		title.setPadding(new Insets(5));

		final var toolTipButton = getToolTipButton(FILTER_TOOLTIP_TEXT);

		final var titleBox = new HBox();
		titleBox.getChildren().addAll(title, toolTipButton);

		final var vBox = new VBox();
		vBox.setPadding(new Insets(5));
		vBox.setVisible(true);

		vBox.managedProperty().bind(vBox.visibleProperty());
		final var gridPane = getCheckboxesGridPane();
		vBox.getChildren().add(gridPane);

		typeFilterVBox.getChildren().clear();
		typeFilterVBox.getChildren().addAll(titleBox, vBox);
		typeFilterVBox.setStyle("-fx-border-color: gray; -fx-border-radius: 10");
	}

	private void setupFeePayerFilterBox() {
		feePayerFilterVBox.managedProperty().bind(feePayerFilterVBox.visibleProperty());
		feePayerFilterVBox.setVisible(false);
	}

	private void setupExpirationDateBox() {
		expirationDateFilterVBox.managedProperty().bind(expirationDateFilterVBox.visibleProperty());
		expirationDateFilterVBox.setVisible(false);
	}

	private void setupActionDateBox() {
		actedDateFilterVBox.managedProperty().bind(actedDateFilterVBox.visibleProperty());
		actedDateFilterVBox.setVisible(false);
	}

	@NotNull
	private GridPane getCheckboxesGridPane() {
		final var gridPane = new GridPane();
		gridPane.setHgap(5);
		gridPane.setVgap(5);
		noise = true;

		final var selectAll = formatButton("icons/icons8-check-all-50.png", 25);
		selectAll.setText("Select all");
		final var selectNone = formatButton("icons/icons8-uncheck-all-50.png", 25);
		selectNone.setText("Select none");

		final List<CheckBox> checkBoxes = new ArrayList<>();

		for (final var type : EnumSet.allOf(FileType.class)) {
			final var typeCounter = countType(type);
			final var typeString = type.toKind().toLowerCase();
			if (!"".equals(typeString)) {
				final var checkBox = new CheckBox(String.format("%s (%d)", typeString, typeCounter));
				checkBox.setSelected(typeFilter.contains(type));
				checkBox.selectedProperty().addListener((observableValue, aBoolean, t1) -> {
					if (Boolean.FALSE.equals(t1)) {
						typeFilter.remove(type);
					} else {
						typeFilter.add(type);

					}
				});
				checkBoxes.add(checkBox);
			}
		}

		selectAll.setOnAction(actionEvent -> checkBoxes.forEach(checkBox -> checkBox.setSelected(true)));

		selectNone.setOnAction(actionEvent -> checkBoxes.forEach(checkBox -> checkBox.setSelected(false)));

		var counter = 0;
		for (final var checkBox : checkBoxes) {
			gridPane.add(checkBox, counter % 3, counter / 3);
			counter++;
		}

		gridPane.add(selectAll, 0, counter / 3 + 1);
		gridPane.add(selectNone, 1, counter / 3 + 1);

		gridPane.setVgap(10);
		gridPane.setHgap(10);
		noise = false;
		return gridPane;
	}

	private int countType(final FileType type) {
		var count = 0;
		for (final var value : historyMap.values()) {
			if (type.equals(value.getType())) {
				count++;
			}
		}
		return count;
	}

	// endregion

	// region TABLE
	private void refreshTable(){
		tableList.clear();
		final List<HistoryData> historyData = new ArrayList<>(historyMap.values());
		Collections.sort(historyData);
		tableList.addAll(historyData);
	}

	private TableView<HistoryData> setupTable() {
		final var table = new TableView<HistoryData>();
		final var typeColumn = getTypeColumn(table);
		final var feePayerColumn = getFeePayerColumn(table);
		final var lastAction = getLastActionColumn(table);
		final var expirationDateColumn = getExpirationColumn(table);
		final var expanderColumn = getExpanderColumn();
		final var reSignColumn = getReSignColumn(table);

		table.getColumns().add(expanderColumn);
		table.getColumns().add(typeColumn);
		table.getColumns().add(expirationDateColumn);
		table.getColumns().add(feePayerColumn);
		table.getColumns().add(lastAction);
		table.getColumns().add(reSignColumn);
		table.getSortOrder().add(lastAction);

		typeFilter.addListener((ListChangeListener<FileType>) change -> {
			while (change.next()) {
				if (!noise && (change.wasAdded() || change.wasRemoved())) {
					typePredicate = statesModel -> {
						if (!noise && (change.wasAdded() || change.wasRemoved())) {
							return typeFilter.contains(statesModel.getType());
						}
						return false;
					};
					filters.add(typePredicate);
				}
			}
		});

		actionsFilter.addListener((ListChangeListener<Actions>) change -> {
			while (change.next()) {
				if (!noise && (change.wasAdded() || change.wasRemoved())) {
					actionTypePredicate = historyData -> {
						if (!noise && (change.wasAdded() || change.wasRemoved())) {
							return actionsFilter.contains(historyData.getActions());
						}
						return false;
					};
					filters.add(actionTypePredicate);
				}
			}
		});

		acceptedCheckBox.selectedProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.TRUE.equals(t1)) {
				actionsFilter.add(Actions.ACCEPT);
			} else {
				actionsFilter.remove(Actions.ACCEPT);
			}
		});

		declinedCheckBox.selectedProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.TRUE.equals(t1)) {
				actionsFilter.add(Actions.DECLINE);
			} else {
				actionsFilter.remove(Actions.DECLINE);
			}
		});

		feePayerTextField.setOnKeyReleased(event -> {
			if (event.getCode() != KeyCode.ENTER || event.getCode() != KeyCode.TAB) {
				return;
			}
			feePayerFilterAccept();
		});

		filteredList.predicateProperty().bind(
				Bindings.createObjectBinding(() -> filters.stream().reduce(x -> true, Predicate::and), filters));

		tableList.clear();
		final List<HistoryData> historyData = new ArrayList<>(historyMap.values());
		Collections.sort(historyData);
		tableList.addAll(historyData);
		final var sortedList = new SortedList<>(filteredList);

		sortedList.comparatorProperty().bind(table.comparatorProperty());
		table.setItems(sortedList);
		table.sort();
		setupTypeFilterBox();
		return table;
	}

	@NotNull
	private TableColumn<HistoryData, String> getExpirationColumn(final TableView<HistoryData> table) {
		final var expirationDateColumn = new TableColumn<HistoryData, String>("");
		expirationDateColumn.setCellValueFactory(new PropertyValueFactory<>("expirationDate"));
		expirationDateColumn.prefWidthProperty().bind(table.widthProperty().divide(5));
		expirationDateColumn.setCellFactory(historyDataStringTableColumn -> setWrapping());
		final HBox lastActionBox = getTitleBox("Expiration Date:", expirationDateFilterVBox);
		expirationDateColumn.setGraphic(lastActionBox);
		return expirationDateColumn;
	}

	@NotNull
	private TableColumn<HistoryData, String> getLastActionColumn(final TableView<HistoryData> table) {
		final var lastAction = new TableColumn<HistoryData, String>("");
		lastAction.setCellValueFactory(new PropertyValueFactory<>("lastAction"));
		lastAction.prefWidthProperty().bind(table.widthProperty().divide(5));
		lastAction.setCellFactory(historyDataStringTableColumn -> setWrapping());
		lastAction.setSortType(TableColumn.SortType.DESCENDING);
		final HBox lastActionBox = getTitleBox("Last acted on:", actedDateFilterVBox);
		lastAction.setGraphic(lastActionBox);

		return lastAction;
	}

	@NotNull
	private TableColumn<HistoryData, String> getFeePayerColumn(final TableView<HistoryData> table) {
		final var feePayerColumn = new TableColumn<HistoryData, String>("");
		feePayerColumn.setCellValueFactory(new PropertyValueFactory<>("feePayer"));
		feePayerColumn.prefWidthProperty().bind(table.widthProperty().divide(5));
		feePayerColumn.setCellFactory(historyDataStringTableColumn -> setWrapping());
		feePayerColumn.setCellValueFactory(this::setupFeePayerCell);
		final var feePayerHBox = getTitleBox("Fee Payer Account", feePayerFilterVBox);
		feePayerColumn.setGraphic(feePayerHBox);
		return feePayerColumn;
	}

	@NotNull
	private TableColumn<HistoryData, String> getTypeColumn(final TableView<HistoryData> table) {
		final var typeColumn = new TableColumn<HistoryData, String>("");

		final var typeBox = getTitleBox("Type", typeFilterVBox);
		typeColumn.setGraphic(typeBox);
		typeColumn.setCellValueFactory(new PropertyValueFactory<>("title"));
		typeColumn.prefWidthProperty().bind(table.widthProperty().divide(32).multiply(9));

		return typeColumn;
	}

	private TableRowExpanderColumn<HistoryData> getExpanderColumn() {
		final var expanderColumn = new TableRowExpanderColumn<>(this::buildAccountVBox);
		expanderColumn.setStyle("-fx-alignment: TOP-CENTER; -fx-padding: 10");
		return expanderColumn;

	}

	/**
	 * Setups the column with the re-sign button
	 *
	 * @param table
	 * 		The table where the button will be added
	 * @return a TableColumn
	 */
	private TableColumn<HistoryData, String> getReSignColumn(final TableView<HistoryData> table) {
		final var actionColumn = new TableColumn<HistoryData, String>("");
		actionColumn.setCellValueFactory(new PropertyValueFactory<>(""));
		final var cellFactory =
				(Callback<TableColumn<HistoryData, String>, TableCell<HistoryData, String>>) column -> new TableCell<>() {
					final Button button = formatButton(RESET_ICON, 30);

					@Override
					public void updateItem(final String item, final boolean empty) {
						setText(null);
						if (!empty) {
							if (controller.getSetupPhase().equals(SetupPhase.TEST_PHASE)) {
								final var x = table.getItems().get(getIndex());
								button.setText(x.getFileName());
								button.setStyle("-fx-font-size: 2");
							}
							final var historyData = getTableView().getItems().get(getIndex());
							button.setVisible(!historyData.isExpired());
							button.setDisable(!historyData.isHistory());

							if (historyData.getType().equals(FileType.ACCOUNT_INFO) ||
									historyData.getType().equals(FileType.BUNDLE) ||
									historyData.getType().equals(FileType.PUBLIC_KEY)) {
								button.setVisible(historyData.getActions().equals(Actions.DECLINE));
							}
							button.setOnAction(actionEvent -> setHistoryDataAction(historyData));
							setGraphic(button);
							return;
						}
						setGraphic(null);
					}

					private void setHistoryDataAction(final HistoryData historyData) {
						historyData.setHistory(false);
						noise = false;
						historyMap.put(historyData.getCode(), historyData);
						button.setDisable(true);
						controller.homePaneController.setForceUpdate(true);
						controller.homePaneController.initializeHomePane();
					}
				};
		actionColumn.setCellFactory(cellFactory);
		return actionColumn;
	}

	/**
	 * Sets up the transaction details box
	 *
	 * @param parameter
	 * 		the tablerow
	 * @return a VBox
	 */
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

	// endregion


	/**
	 * Store the map to the System Files folder
	 */
	private void storeMap() {
		// Store map
		if (noise) {
			return;
		}
		if (new File(DEFAULT_SYSTEM_FOLDER).mkdirs()) {
			logger.info("Creating system folder");
		}
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

	/**
	 * Load all the allowed files from the history folder
	 */
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
				logger.info("Loading element: {}", element.getAsJsonObject().get("filename").getAsString());
				final var historyData = new HistoryData(element.getAsJsonObject());
				historyMap.put(historyData.getCode(), historyData);
			}
		} catch (final FileNotFoundException | HederaClientException | JsonProcessingException e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * Add a listener to the map to store and reload the table
	 */
	private void setupMapListener() {
		historyMap.addListener((MapChangeListener<Integer, HistoryData>) change -> {
			if (!noise) {
				storeMap();
				refreshTable();
				//setupTable();
			}
		});
	}

	/**
	 * Parses the files in the history folder
	 *
	 * @throws FileNotFoundException
	 * 		if a file is not found
	 */
	private void parseHistoryFolder() throws FileNotFoundException {

		final var files = getRemoteFiles();
		for (final var file : files) {
			if (METADATA.equals(file.getType()) || COMMENT.equals(file.getType())) {
				continue;
			}
			logger.info("Parsing file {}", file.getName());
			final var data = new HistoryData(file);
			data.setHistory(true);
			noise = true;
			historyMap.put(data.getCode(), data);
			noise = false;
		}
		logger.info("here");
		storeMap();
		noise = false;
	}

	/**
	 * Only allow files that are allowed by the app
	 *
	 * @param name
	 * 		the name of the file
	 * @return true if the file type is allowed
	 */
	private boolean isAllowedFile(final String name) {
		try {
			return !FileType.getType(FilenameUtils.getExtension(name)).equals(FileType.UNKNOWN);
		} catch (final HederaClientException e) {
			return false;
		}
	}


	/**
	 * Adds the fee filter to the list
	 */
	public void feePayerFilterAccept() {
		filters.add(feePayerPredicate);
	}

	/**
	 * Removes the fee filter. Resets the filter box
	 */
	public void resetFeeFilter() {
		feePayerTextField.clear();
		filters.remove(feePayerPredicate);
	}

	/**
	 * Filters the table according to the last action column
	 */
	public void actionFilterAccept() {
		logger.info("Filtering action dates");
		final var startValue = actionsStartDatePicker.getValue();
		final var endValue = actionsEndDatePicker.getValue();

		final var start = startValue != null ? startValue : LocalDate.now();
		final var end = endValue != null ? endValue : LocalDate.now();

		if (start.isAfter(end)) {
			return;
		}

		filters.add(actionDatePredicate);
	}

	/**
	 * Removes the action date filter from the filter list. Resets the filter box
	 */
	public void actionFilterReset() {
		declinedCheckBox.setSelected(true);
		acceptedCheckBox.setSelected(true);
		actionsStartDatePicker.getEditor().clear();
		actionsEndDatePicker.getEditor().clear();
		filters.remove(actionDatePredicate);
	}

	/**
	 * Filters the table according to the expiration date column
	 */
	public void expirationFilterAccept() {
		logger.info("Filtering action dates");
		final var startValue = expirationStartDatePicker.getValue();
		final var endValue = expirationEndDatePicker.getValue();

		start = startValue != null ? startValue : LocalDate.now();
		end = endValue != null ? endValue : LocalDate.now();

		if (start.isAfter(end)) {
			return;
		}

		filters.add(expirationDatePredicate);
	}

	/**
	 * Remove the expiration date filter from the filter list. Resets the filter box
	 */
	public void expirationFilterReset() {
		expirationEndDatePicker.getEditor().clear();
		expirationStartDatePicker.getEditor().clear();
		filters.remove(expirationDatePredicate);
	}

	/**
	 * Setups an HBox with the title and a button to open the filterbox
	 *
	 * @param title
	 * 		The title that will be set in the header.
	 * @param filterBox
	 * 		The filterbox that should be toggled.
	 * @return an HBox with a label and a button.
	 */
	@NotNull
	private HBox getTitleBox(final String title, final VBox filterBox) {
		final var hBox = new HBox();
		final var isTest = SetupPhase.TEST_PHASE.equals(controller.getSetupPhase());

		if (!isTest) {
			hBox.getChildren().add(new Label(title));
			final var region = getRegion();
			hBox.getChildren().add(region);
		}

		final var button = isTest ?
				new Button(title + "@@@") :
				formatButton(FILTER_ICON, 20);
		button.setOnAction(actionEvent -> {
			final var b = !filterBox.isVisible();
			feePayerFilterVBox.setVisible(false);
			typeFilterVBox.setVisible(false);
			actedDateFilterVBox.setVisible(false);
			expirationDateFilterVBox.setVisible(false);
			filterBox.setVisible(b);
		});
		hBox.getChildren().add(button);
		hBox.setAlignment(Pos.CENTER_RIGHT);
		return hBox;
	}

	/**
	 * Setups a button with an icon
	 *
	 * @param imageLocation
	 * 		the relative location of the image of the icon to be used
	 * @return a button with the image provided
	 */
	private Button formatButton(final String imageLocation, final int size) {
		final var button = new Button();
		final var imageView = new ImageView(new Image(imageLocation));
		imageView.setFitHeight(size);
		imageView.setPreserveRatio(true);
		button.setGraphic(imageView);
		button.setStyle("-fx-background-color: transparent; -fx-border-color: transparent");
		return button;
	}

	/**
	 * Setups a list of datepickers to work within the locale constraints.
	 *
	 * @param pickers
	 * 		a list of datepickers
	 */
	private void formatDatePicker(final DatePicker... pickers) {
		if (pickers == null) {
			return;
		}
		for (final var picker : pickers) {
			picker.setPromptText(dateFormat.toLowerCase(Locale.ROOT));
			picker.setConverter(new StringConverter<>() {
				final DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern(dateFormat);

				@Override
				public String toString(final LocalDate localDate) {
					if (localDate != null) {
						return dateFormatter.format(localDate);
					}
					return "";
				}

				@Override
				public LocalDate fromString(final String s) {
					if (s != null && !s.isEmpty()) {
						return LocalDate.parse(s, dateFormatter);
					}
					return null;
				}
			});
		}
	}

	/**
	 * Set up an expanding region
	 *
	 * @return a region that always grows horizontally
	 */
	@NotNull
	private Region getRegion() {
		final var typeRegion = new Region();
		typeRegion.setPrefHeight(Region.USE_COMPUTED_SIZE);
		typeRegion.setPrefWidth(Region.USE_COMPUTED_SIZE);
		HBox.setHgrow(typeRegion, Priority.ALWAYS);
		return typeRegion;
	}

	/**
	 * Allow cells to wrap
	 *
	 * @return a tablecell
	 */
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

	/**
	 * Sets up the format for an account ID cell
	 *
	 * @param cellData
	 * 		a cell that should have an account ID
	 * @return a SimpleStringProperty
	 */
	@NotNull
	private SimpleStringProperty setupFeePayerCell(final TableColumn.CellDataFeatures<HistoryData, String> cellData) {
		final var id = Identifier.parse(cellData.getValue().getFeePayer(), controller.getCurrentNetwork());
		if (id.equals(Identifier.ZERO)) {
			return new SimpleStringProperty("");
		}
		return new SimpleStringProperty(id.toNicknameAndChecksum(controller.getAccountsList()));
	}

	/**
	 * Set up a tooltip button
	 *
	 * @param text
	 * 		the text of the tooltip
	 * @return a formatted button
	 */
	@NotNull
	private Button getToolTipButton(final String text) {
		final var image = new Image("icons" + File.separator + "helpIcon.png");
		final var imageView = new ImageView(image);
		imageView.setPreserveRatio(true);
		imageView.setFitHeight(15);
		final var toolTipButton = new Button();
		toolTipButton.setGraphic(imageView);
		toolTipButton.setStyle("-fx-background-color: transparent; -fx-border-color: transparent");
		toolTipButton.setPadding(new Insets(0, 0, 10, 0));
		toolTipButton.setMinWidth(25);

		toolTipButton.setOnAction(
				actionEvent -> Utilities.showTooltip(controller.homePane, toolTipButton, text));
		return toolTipButton;
	}

	public void rebuildHistory() throws IOException {
		if (controller.getSetupPhase().equals(SetupPhase.TEST_PHASE)) {
			Files.deleteIfExists(Path.of(HISTORY_MAP));
			loadMap();
			initializeHistoryPane();
			controller.homePaneController.setForceUpdate(true);
			controller.homePaneController.initializeHomePane();
		}
	}

	public boolean isHistory(final int code) {
		return historyMap.containsKey(code) && historyMap.get(code).isHistory();
	}
}
