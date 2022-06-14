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
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.ui.utilities.HistoryData;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.DateCell;
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
import javafx.scene.input.KeyEvent;
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
import java.io.IOException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_SYSTEM_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.HISTORY_MAP;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.FILTER_TOOLTIP_TEXT;
import static com.hedera.hashgraph.client.core.enums.FileType.ACCOUNT_INFO;
import static com.hedera.hashgraph.client.core.enums.FileType.BUNDLE;
import static com.hedera.hashgraph.client.core.enums.FileType.COMMENT;
import static com.hedera.hashgraph.client.core.enums.FileType.METADATA;
import static com.hedera.hashgraph.client.core.enums.FileType.PUBLIC_KEY;
import static com.hedera.hashgraph.client.core.enums.FileType.SOFTWARE_UPDATE;
import static com.hedera.hashgraph.client.core.enums.FileType.UNKNOWN;
import static com.hedera.hashgraph.client.core.enums.FileType.getType;
import static com.hedera.hashgraph.client.core.utils.FXUtils.formatButton;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.parseAccountNumbers;
import static java.lang.String.format;
import static java.lang.String.join;
import static java.nio.file.Files.deleteIfExists;
import static javafx.beans.binding.Bindings.createObjectBinding;

public class HistoryPaneController implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(HistoryPaneController.class);
	public static final String RESET_ICON = "icons/sign-back.png";
	public static final String SENT_ICON = "icons/icons8-sent-100.png";
	public static final String FILTER_ICON = "icons/filter.png";

	private final Map<Integer, Boolean> historyMap = new HashMap<>();
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

	private LocalDate start = LocalDate.now();
	private LocalDate end = LocalDate.now();

	private final ObservableList<Predicate<HistoryData>> filters = FXCollections.observableArrayList();
	private Predicate<HistoryData> feePayerPredicate;
	private Predicate<HistoryData> typePredicate;
	private Predicate<HistoryData> actionDatePredicate;
	private Predicate<HistoryData> actionTypePredicate;
	private Predicate<HistoryData> expirationDatePredicate;
	private final TableView<HistoryData> tableView = new TableView<>();

	private final String dateFormat = Locale.getDefault().equals(Locale.US) ? "MM/dd/yyyy" : "dd/MM/yyyy";

	@FXML
	private Controller controller;

	// region INITIALIZATION
	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	void initializeHistoryPane() {
		loadHistory();
		setupTable();
		setupPredicates();
		setupFilterBoxes();
		setupBindings();
	}

	private void setupBindings() {
		rebuild.managedProperty().bind(rebuild.visibleProperty());
		contentScrollPane.setFitToWidth(true);
	}


	/**
	 * Load history from flat file
	 */
	private void loadHistory() {
		cleanHistory();
		try {
			if (!new File(HISTORY_MAP).exists()) {
				logger.info("Map not found. Parsing history folder");
				parseHistoryFolder();
				FXCollections.sort(tableList, Comparator.reverseOrder());
				return;
			}
			final var array = readJsonArray(HISTORY_MAP);
			for (final var element : array) {
				final var jsonObject = element.getAsJsonObject();
				final var message = jsonObject.get("filename").toString();
				logger.info("Loading element {}", message);
				final var historyData = new HistoryData(jsonObject);
				tableList.add(historyData);
				historyMap.put(historyData.getCode(), historyData.isHistory());
			}
			FXCollections.sort(tableList, Comparator.reverseOrder());
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * Stores the map to a file
	 */
	private void storeHistory() {
		if (new File(DEFAULT_SYSTEM_FOLDER).mkdirs()) {
			logger.info("Creating system folder");
		}
		logger.info("Storing map to {}", HISTORY_MAP);
		final var array = new JsonArray();
		for (final var entry : tableList) {
			array.add(entry.asJson());
		}

		try {
			writeJsonObject(HISTORY_MAP, array);
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * For backwards compatibility: parse the history folder and creates the flat history file
	 */
	private void parseHistoryFolder() {
		final var files = getRemoteFiles();
		final var jsonArray = new JsonArray();
		noise = true;
		for (final var file : files) {
			if (METADATA.equals(file.getType()) || COMMENT.equals(file.getType())) {
				continue;
			}
			logger.info("Parsing file {}", file.getName());
			final var data = new HistoryData(file);
			data.setHistory(true);
			jsonArray.add(data.asJson());
			historyMap.put(data.getCode(), data.isHistory());
			tableList.add(data);
		}
		storeHistory();
		noise = false;
	}

	// endregion

	// region GETTER and SETTERS

	/**
	 * Adds a file to the map
	 */
	public void addToHistory(final RemoteFile remoteFile) {
		noise = false;
		final var addition = new HistoryData(remoteFile);
		tableList.remove(addition);
		addition.setHistory(true);
		historyMap.put(addition.getCode(), true);
		tableList.add(0, addition);
		tableView.refresh();
	}

	/**
	 * Check if the file is in history
	 *
	 * @param code
	 * 		the hashcode of the file
	 * @return If the code is in the map, and it is marked as history, return true
	 */
	public boolean isHistory(final int code) {
		return historyMap.containsKey(code) && (Boolean.TRUE.equals(historyMap.get(code)));
	}

	/**
	 * Cleans all maps and tables
	 */
	public void cleanHistory() {
		noise = false;
		historyMap.clear();
		tableList.clear();
		filteredList.clear();
	}

	// endregion

	// region TABLE SETUP

	/**
	 * Set up the main history table
	 */
	private void setupTable() {
		tableList.addListener((ListChangeListener<HistoryData>) change -> {
			if (noise) {
				return;
			}
			while (change.next()) {
				if (change.wasRemoved() || change.wasAdded()) {
					storeHistory();
					filteredList.sorted();
				}
			}
		});
		contentScrollPane.setContent(tableView);
		setupTableBindings();

		tableView.getColumns().clear();
		tableView.getColumns().add(getExpanderColumn());
		tableView.getColumns().add(getTypeColumn());
		tableView.getColumns().add(getExpirationColumn());
		tableView.getColumns().add(getFeePayerColumn());
		tableView.getColumns().add(getLastActionColumn());
		tableView.getColumns().add(getButtonColumn(tableView));

		setDataToTable();
	}

	/**
	 * Sets the history data to the table
	 */
	private void setDataToTable() {
		final var sortedList = new SortedList<>(filteredList);
		sortedList.comparatorProperty().bind(tableView.comparatorProperty());
		tableView.setItems(sortedList);
		tableView.sort();
	}

	/**
	 * Set up the table bindings and listeners
	 */
	private void setupTableBindings() {
		typeFilter.addListener(this::typeFilterOnChanged);

		actionsFilter.addListener(this::actionsFilterOnChanged);

		acceptedCheckBox.selectedProperty().addListener(this::acceptedCheckBoxOnChanged);

		declinedCheckBox.selectedProperty().addListener(this::declinedCheckBoxOnChanged);

		feePayerTextField.setOnKeyReleased(this::feePayerTextFieldHandle);

		filteredList.predicateProperty().bind(
				createObjectBinding(() -> filters.stream().reduce(x -> true, Predicate::and), filters));

	}

	/**
	 * Set up the type column
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<HistoryData, String> getTypeColumn() {
		final var typeColumn = new TableColumn<HistoryData, String>("");
		final var typeBox = getTitleBox("Type", typeFilterVBox);
		typeColumn.setGraphic(typeBox);
		typeColumn.setCellValueFactory(new PropertyValueFactory<>("title"));
		typeColumn.prefWidthProperty().bind(tableView.widthProperty().divide(32).multiply(9));
		return typeColumn;
	}

	/**
	 * Set up the fee payer table
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<HistoryData, String> getFeePayerColumn() {
		final var feePayerColumn = new TableColumn<HistoryData, String>("");
		feePayerColumn.setCellValueFactory(new PropertyValueFactory<>("feePayer"));
		feePayerColumn.prefWidthProperty().bind(tableView.widthProperty().divide(5));
		feePayerColumn.setCellFactory(historyDataStringTableColumn -> setWrapping());
		feePayerColumn.setCellValueFactory(this::setupFeePayerCell);
		final var feePayerHBox = getTitleBox("Fee Payer Account", feePayerFilterVBox);
		feePayerColumn.setGraphic(feePayerHBox);
		return feePayerColumn;
	}

	/**
	 * Set up the expiration date column
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<HistoryData, String> getExpirationColumn() {
		final var expirationDateColumn = new TableColumn<HistoryData, String>("");
		expirationDateColumn.setCellValueFactory(new PropertyValueFactory<>("expirationDate"));
		expirationDateColumn.prefWidthProperty().bind(tableView.widthProperty().divide(5));
		expirationDateColumn.setCellFactory(historyDataStringTableColumn -> setWrapping());
		final var lastActionBox = getTitleBox("Expiration:", expirationDateFilterVBox);
		expirationDateColumn.setGraphic(lastActionBox);
		return expirationDateColumn;
	}

	/**
	 * Set up the last action column
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<HistoryData, String> getLastActionColumn() {
		final var lastAction = new TableColumn<HistoryData, String>("");
		lastAction.setCellValueFactory(new PropertyValueFactory<>("lastAction"));
		lastAction.prefWidthProperty().bind(tableView.widthProperty().divide(5));
		lastAction.setCellFactory(historyDataStringTableColumn -> setWrapping());
		lastAction.setSortType(TableColumn.SortType.DESCENDING);
		final var lastActionBox = getTitleBox("Last acted on:", actedDateFilterVBox);
		lastAction.setGraphic(lastActionBox);
		return lastAction;
	}

	/**
	 * Set up the expander column
	 *
	 * @return a formatted expander column
	 */
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
	private TableColumn<HistoryData, String> getButtonColumn(final TableView<HistoryData> table) {
		final var actionColumn = new TableColumn<HistoryData, String>("");
		actionColumn.setCellValueFactory(new PropertyValueFactory<>(""));
		final var cellFactory =
				(Callback<TableColumn<HistoryData, String>, TableCell<HistoryData, String>>) column -> new TableCell<>() {
					final Button button = formatButton(RESET_ICON, 30);

					@Override
					public void updateItem(final String item, final boolean empty) {
						setText(null);
						if (!empty) {
							final var historyData = table.getItems().get(getIndex());
							final var succeeded = historyData.transactionSucceeded();
							if (succeeded != null) {
								setGraphic(twoImages(Boolean.TRUE.equals(succeeded)));
								return;
							}

							setupButtonBehavior(historyData);
							setGraphic(button);
							return;
						}
						setGraphic(null);
					}

					private void setupButtonBehavior(final HistoryData historyData) {
						if (controller.getSetupPhase().equals(SetupPhase.TEST_PHASE)) {
							button.setText(historyData.getFileName());
							button.setStyle("-fx-font-size: 2");
						}

						button.setVisible(!historyData.isExpired());
						button.setDisable(!historyData.isHistory());

						if (ACCOUNT_INFO.equals(historyData.getType()) ||
								BUNDLE.equals(historyData.getType()) ||
								PUBLIC_KEY.equals(historyData.getType()) ||
								SOFTWARE_UPDATE.equals(historyData.getType())) {
							button.setVisible(historyData.getActions().equals(Actions.DECLINE));
						}
						button.setOnAction(actionEvent -> setHistoryDataAction(historyData));
					}

					private void setHistoryDataAction(final HistoryData historyData) {
						noise = true;
						historyMap.put(historyData.getCode(), false);
						final var row = getRow(historyData);
						if (row < 0) {
							return;
						}
						tableList.remove(historyData);
						historyData.setHistory(false);
						noise = false;
						tableList.add(row, historyData);
						button.setDisable(true);
						controller.homePaneController.setForceUpdate(true);
						controller.homePaneController.initializeHomePane();
					}

					private int getRow(final HistoryData historyData) {
						return IntStream.range(0, tableList.size()).filter(
								i -> tableList.get(i).equals(historyData)).findFirst().orElse(-1);
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
		} catch (final HederaClientException e) {
			logger.error(e);
		}
		final var l = new Label(data.getFileName());
		returnBox.getChildren().add(l);
		return returnBox;
	}

	// endregion

	// region FILTERS

	/**
	 * Set up the predicate filters
	 */
	private void setupPredicates() {
		formatDatePicker(expirationStartDatePicker, expirationEndDatePicker, actionsStartDatePicker,
				actionsEndDatePicker);

		setupStartEndDates(expirationStartDatePicker, expirationEndDatePicker);
		setupStartEndDates(actionsStartDatePicker, actionsEndDatePicker);

		feePayerPredicate = historyData -> {
			final var accounts = parseAccountNumbers(feePayerTextField.getText(), controller.getCurrentNetwork());
			final var text = join(", ", accounts.stream()
					.map(account -> Identifier
							.parse(account.toString(), controller.getCurrentNetwork())
							.toReadableString())
					.collect(Collectors.toCollection(ArrayList::new)));
			feePayerTextField.setText(text);
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

	private void setupStartEndDates(final DatePicker start, final DatePicker end) {
		end.disableProperty().bind(start.valueProperty().isNull());
		end.setDayCellFactory(picker -> new DateCell() {
			@Override
			public void updateItem(final LocalDate date, final boolean empty) {
				super.updateItem(date, empty);
				final var begin = start.getValue();
				final LocalDate today = LocalDate.now();
				setDisable(empty || date.compareTo(begin) < 0 || date.compareTo(today) > 0);
			}
		});
	}

	private void setupFilterBoxes() {
		typeFilter.addAll(EnumSet.allOf(FileType.class));
		actionsFilter.addAll(Actions.ACCEPT, Actions.DECLINE);
		setupTypeFilterBox();
		setupFeePayerFilterBox();
		setupExpirationDateBox();
		setupActionDateBox();
	}

	private void setupTypeFilterBox() {
		typeFilterVBox.managedProperty().bind(typeFilterVBox.visibleProperty());
		typeFilterVBox.setVisible(false);
		final var size = typeFilter.size();
		final var filterTitle = size > 0 ? format("filters (%d)", size) : "filters";
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

		EnumSet.allOf(FileType.class).forEach(type -> {
			final var typeString = type.toKind().toLowerCase();
			if (!"".equals(typeString)) {
				checkBoxes.add(getCheckBox(type, typeString));
			}
		});

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


	// endregion

	// region HELPERS


	/**
	 * Sets up the title box for each column
	 *
	 * @param title
	 * 		the explanatory title of the column
	 * @param filterBox
	 * 		the filter box corresponding to the column
	 * @return an HBox that will be placed as the graphic for the column
	 */
	private HBox getTitleBox(final String title, final VBox filterBox) {
		final var hBox = new HBox();
		final var isTest = SetupPhase.TEST_PHASE.equals(controller.getSetupPhase());

		if (!isTest) {
			hBox.getChildren().add(new Label(title));
			hBox.getChildren().add(getRegion());
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

	private HBox twoImages(final boolean success) {
		final var sent = new Image(SENT_ICON);
		final var check = success ? new Image("icons/greencheck.png") : new Image("icons/icons8-box-important-96.png");
		final var bottom = new ImageView(sent);
		bottom.setFitHeight(30);
		bottom.setPreserveRatio(true);
		final var top = new ImageView(check);
		top.setFitHeight(20);
		top.setPreserveRatio(true);
		final HBox layout = new HBox(10);
		layout.getChildren().addAll(bottom, top);
		layout.setSpacing(-1);
		layout.setPadding(new Insets(10));
		return layout;
	}

	/**
	 * Loads all the remote files from the history folder.
	 *
	 * @return an array of remote files
	 */
	private ArrayList<RemoteFile> getRemoteFiles() {
		final var filenames = new File(DEFAULT_HISTORY).listFiles((dir, name) -> isAllowedFile(name));
		final var remoteFiles = new ArrayList<RemoteFile>();
		for (final var filename : filenames) {
			FileDetails details = null;
			try {
				details = FileDetails.parse(filename);
			} catch (final HederaClientException e) {
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

	/**
	 * Only allow files that are allowed by the app
	 *
	 * @param name
	 * 		the name of the file
	 * @return true if the file type is allowed
	 */
	private boolean isAllowedFile(final String name) {
		try {
			return !getType(FilenameUtils.getExtension(name)).equals(UNKNOWN);
		} catch (final HederaClientException e) {
			return false;
		}
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
				actionEvent -> CommonMethods.showTooltip(controller.homePane, toolTipButton, text));
		return toolTipButton;
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
		cell.setPrefHeight(Region.USE_COMPUTED_SIZE);
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
		final var id = cellData.getValue().getFeePayerId();
		if (id.equals(Identifier.ZERO)) {
			return new SimpleStringProperty("");
		}
		return new SimpleStringProperty(id.toNicknameAndChecksum(controller.getAccountsList()));
	}

	/**
	 * Sets up a checkbox for the type filter
	 *
	 * @param type
	 * 		the type of file
	 * @param typeString
	 * 		the assigned label for the checkbox
	 * @return a formatted checkbox
	 */
	@NotNull
	private CheckBox getCheckBox(final FileType type, final String typeString) {
		final var checkBox = new CheckBox(format("%s (%d)", typeString, countType(type)));
		checkBox.setSelected(typeFilter.contains(type));
		checkBox.selectedProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.FALSE.equals(t1)) {
				typeFilter.remove(type);
			} else {
				typeFilter.add(type);

			}
		});
		return checkBox;
	}

	/**
	 * Counts the number of files with a certain type
	 *
	 * @param type
	 * 		the type needed
	 * @return an integer
	 */
	private int countType(final FileType type) {
		var count = 0;
		for (final var value : tableList) {
			if (type.equals(value.getType())) {
				count++;
			}
		}
		return count;
	}

	// endregion

	// region EVENTS

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
	 * Filters the table according to the last action column
	 */
	public void actionFilterAccept() {
		logger.info("Filtering action dates");
		final var startValue = actionsStartDatePicker.getValue();
		final var endValue = actionsEndDatePicker.getValue();

		final var startLocalDate = startValue != null ? startValue : LocalDate.now();
		final var endLocalDate = endValue != null ? endValue : LocalDate.now();

		if (startLocalDate.isAfter(endLocalDate)) {
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
	 * Configures a region that grows to use all available space
	 *
	 * @return a Region
	 */
	@NotNull
	private Region getRegion() {
		final var region = new Region();
		HBox.setHgrow(region, Priority.ALWAYS);
		VBox.setVgrow(region, Priority.ALWAYS);
		region.setPrefWidth(Region.USE_COMPUTED_SIZE);
		region.setPrefHeight(Region.USE_COMPUTED_SIZE);
		return region;
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
			picker.setDayCellFactory(p -> new DateCell() {
				@Override
				public void updateItem(final LocalDate date, final boolean empty) {
					super.updateItem(date, empty);
					final LocalDate today = LocalDate.now();
					setDisable(empty || date.compareTo(today) > 0);
				}
			});
		}
	}

	/**
	 * TESTING ONLY: rebuilds the history from the folder
	 */
	public void rebuildHistory() throws IOException {
		deleteIfExists(Path.of(Constants.HISTORY_MAP));
		initializeHistoryPane();
		controller.homePaneController.setForceUpdate(true);
		controller.homePaneController.initializeHomePane();
	}

	/**
	 * Action when type filter has changed
	 *
	 * @param change
	 * 		change listener
	 */
	private void typeFilterOnChanged(final ListChangeListener.Change<? extends FileType> change) {
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
	}

	/**
	 * Action when actions filter has changed
	 *
	 * @param change
	 * 		change listener
	 */
	private void actionsFilterOnChanged(final ListChangeListener.Change<? extends Actions> change) {
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
	}

	/**
	 * Action when the accepted checkbox has changed
	 *
	 * @param observableValue
	 * 		observable
	 * @param aBoolean
	 * 		boolean
	 * @param t1
	 * 		new value
	 */
	private void acceptedCheckBoxOnChanged(final ObservableValue<? extends Boolean> observableValue,
			final Boolean aBoolean,
			final Boolean t1) {
		if (Boolean.TRUE.equals(t1)) {
			actionsFilter.add(Actions.ACCEPT);
		} else {
			actionsFilter.remove(Actions.ACCEPT);
		}
	}

	/**
	 * Action when the declined checkbox is selected
	 *
	 * @param observableValue
	 * 		observable
	 * @param aBoolean
	 * 		boolean
	 * @param t1
	 * 		new value
	 */
	private void declinedCheckBoxOnChanged(final ObservableValue<? extends Boolean> observableValue,
			final Boolean aBoolean,
			final Boolean t1) {
		if (Boolean.TRUE.equals(t1)) {
			actionsFilter.add(Actions.DECLINE);
		} else {
			actionsFilter.remove(Actions.DECLINE);
		}
	}

	/**
	 * Action when a key is pressed in the fee payer box
	 *
	 * @param event
	 * 		the key event
	 */
	private void feePayerTextFieldHandle(final KeyEvent event) {
		if (event.getCode() != KeyCode.ENTER || event.getCode() != KeyCode.TAB) {
			return;
		}
		feePayerFilterAccept();
	}
	// endregion
}
