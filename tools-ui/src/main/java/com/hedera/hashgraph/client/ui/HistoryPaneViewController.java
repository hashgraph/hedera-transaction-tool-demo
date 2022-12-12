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
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;

import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.FILTER_TOOLTIP_TEXT;
import static com.hedera.hashgraph.client.core.enums.FileType.ACCOUNT_INFO;
import static com.hedera.hashgraph.client.core.enums.FileType.BUNDLE;
import static com.hedera.hashgraph.client.core.enums.FileType.PUBLIC_KEY;
import static com.hedera.hashgraph.client.core.enums.FileType.SOFTWARE_UPDATE;
import static com.hedera.hashgraph.client.core.utils.FXUtils.formatButton;
import static java.lang.String.format;
import static java.nio.file.Files.deleteIfExists;

public class HistoryPaneViewController implements GenericFileReadWriteAware {
	private static final Logger LOG = LogManager.getLogger(HistoryPaneViewController.class);

	@FXML
	public StackPane historyPane;
	@FXML
	public ScrollPane contentScrollPane;

	@FXML
	public Button feePayerIDTooltip;
	@FXML
	public TextField feePayerTextField;

	@FXML
	public CheckBox acceptedCheckBox;
	@FXML
	public CheckBox declinedCheckBox;

	@FXML
	public VBox typeFilterVBox;

	@FXML
	public VBox feePayerFilterVBox;

	@FXML
	public VBox actedDateFilterVBox;

	@FXML
	public VBox expirationDateFilterVBox;

	@FXML
	public DatePicker expirationStartDatePicker;

	@FXML
	public DatePicker expirationEndDatePicker;

	@FXML
	public DatePicker actionsStartDatePicker;

	@FXML
	public DatePicker actionsEndDatePicker;

	@FXML
	public Button rebuild;

	@FXML
	private MainController controller;


	private final TableView<HistoryData> tableView = new TableView<>();

	private final String dateFormat = Locale.getDefault().equals(Locale.US) ? "MM/dd/yyyy" : "dd/MM/yyyy";

	private final HistoryPaneModel model = new HistoryPaneModel();

	void injectMainController(final MainController controller) {
		this.controller = controller;
	}

	public HistoryPaneModel getModel() {
		return model;
	}

	void updateHistoryPane() {
		getModel().loadHistory();

		contentScrollPane.setContent(tableView);
		getModel().getTypeFilter().addListener(getModel()::typeFilterOnChanged);
		getModel().getActionsFilter().addListener(getModel()::actionsFilterOnChanged);
		acceptedCheckBox.selectedProperty().addListener(getModel()::acceptedCheckBoxOnChanged);
		declinedCheckBox.selectedProperty().addListener(getModel()::declinedCheckBoxOnChanged);
		feePayerTextField.setOnKeyReleased(this::feePayerTextFieldHandle);
		tableView.getColumns().clear();
		tableView.getColumns().add(createExpanderColumn());
		tableView.getColumns().add(createTypeColumn());
		tableView.getColumns().add(createExpirationColumn());
		tableView.getColumns().add(createFeePayerColumn());
		tableView.getColumns().add(createLastActionColumn());
		tableView.getColumns().add(createButtonColumn(tableView));
		final var sortedList = new SortedList<>(getModel().getFilteredList());
		sortedList.comparatorProperty().bind(tableView.comparatorProperty());
		tableView.setItems(sortedList);
		tableView.sort();

		formatDatePicker(expirationStartDatePicker, expirationEndDatePicker, actionsStartDatePicker,
				actionsEndDatePicker);

		setupStartEndDates(expirationStartDatePicker, expirationEndDatePicker);
		setupStartEndDates(actionsStartDatePicker, actionsEndDatePicker);

		getModel().setupFeePayerFilter(feePayerTextField.getText(), controller.getCurrentNetwork(),
				feePayer -> feePayerTextField.setText(feePayer));
		getModel().setupExpirationDateFilter();
		getModel().setupActionDateFilter();

		getModel().getTypeFilter().addAll(EnumSet.allOf(FileType.class));
		getModel().getActionsFilter().addAll(Actions.ACCEPT, Actions.DECLINE);
		setupTypeFilterBox();
		setupFeePayerFilterBox();
		setupExpirationDateBox();
		setupActionDateBox();

		rebuild.managedProperty().bind(rebuild.visibleProperty());
		contentScrollPane.setFitToWidth(true);
	}

	/**
	 * Adds a file to the map
	 */
	public void addToHistory(final RemoteFile remoteFile) {
		getModel().addToHistory(remoteFile);
		tableView.refresh();
	}

	public void removeFromHistory(final RemoteFile remoteFile) {
		getModel().removeFromHistory(remoteFile);
		tableView.refresh();
	}

	/**
	 * Set up the type column
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<HistoryData, String> createTypeColumn() {
		final var typeColumn = new TableColumn<HistoryData, String>("");
		final var typeBox = createTitleBox("Type", typeFilterVBox);
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
	private TableColumn<HistoryData, String> createFeePayerColumn() {
		final var feePayerColumn = new TableColumn<HistoryData, String>("");
		feePayerColumn.setCellValueFactory(new PropertyValueFactory<>("feePayer"));
		feePayerColumn.prefWidthProperty().bind(tableView.widthProperty().divide(5));
		feePayerColumn.setCellFactory(historyDataStringTableColumn -> createWrappingCell());
		feePayerColumn.setCellValueFactory(this::setupFeePayerCell);
		final var feePayerHBox = createTitleBox("Fee Payer Account", feePayerFilterVBox);
		feePayerColumn.setGraphic(feePayerHBox);
		return feePayerColumn;
	}

	/**
	 * Set up the expiration date column
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<HistoryData, String> createExpirationColumn() {
		final var expirationDateColumn = new TableColumn<HistoryData, String>("");
		expirationDateColumn.setCellValueFactory(new PropertyValueFactory<>("expirationDate"));
		expirationDateColumn.prefWidthProperty().bind(tableView.widthProperty().divide(5));
		expirationDateColumn.setCellFactory(historyDataStringTableColumn -> createWrappingCell());
		final var lastActionBox = createTitleBox("Expiration:", expirationDateFilterVBox);
		expirationDateColumn.setGraphic(lastActionBox);
		return expirationDateColumn;
	}

	/**
	 * Set up the last action column
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<HistoryData, String> createLastActionColumn() {
		final var lastAction = new TableColumn<HistoryData, String>("");
		lastAction.setCellValueFactory(new PropertyValueFactory<>("lastAction"));
		lastAction.prefWidthProperty().bind(tableView.widthProperty().divide(5));
		lastAction.setCellFactory(historyDataStringTableColumn -> createWrappingCell());
		lastAction.setSortType(TableColumn.SortType.DESCENDING);
		final var lastActionBox = createTitleBox("Last acted on:", actedDateFilterVBox);
		lastAction.setGraphic(lastActionBox);
		return lastAction;
	}

	/**
	 * Set up the expander column
	 *
	 * @return a formatted expander column
	 */
	private TableRowExpanderColumn<HistoryData> createExpanderColumn() {
		final var expanderColumn = new TableRowExpanderColumn<>(this::createAccountVBox);
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
	private TableColumn<HistoryData, String> createButtonColumn(final TableView<HistoryData> table) {
		final var actionColumn = new TableColumn<HistoryData, String>("");
		actionColumn.setCellValueFactory(new PropertyValueFactory<>(""));
		final var cellFactory =
				(Callback<TableColumn<HistoryData, String>, TableCell<HistoryData, String>>) column -> new TableCell<>() {
					final Button button = formatButton(StyleConstants.RESET_ICON, 30);

					@Override
					public void updateItem(final String item, final boolean empty) {
						setText(null);
						if (!empty) {
							final var historyData = table.getItems().get(getIndex());
							final var succeeded = historyData.transactionSucceeded();
							if (succeeded != null) {
								setGraphic(createTwoImages(Boolean.TRUE.equals(succeeded)));
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
						onHistoryDataAction(historyData, button);
					}

				};
		actionColumn.setCellFactory(cellFactory);
		return actionColumn;
	}

	private void onHistoryDataAction(final HistoryData historyData, final Button button) {
		getModel().setNoise(true);
		int row;
		try {
			getModel().getHistoryMap().put(historyData.getCode(), false);
			row = getModel().getRow(historyData);
			if (row < 0) {
				return;
			}
			getModel().getTableList().remove(historyData);
			historyData.setHistory(false);
		} finally {
			getModel().setNoise(false);
		}
		getModel().getTableList().add(row, historyData);
		button.setDisable(true);
		controller.homePaneController.setForceUpdate(true);
		controller.homePaneController.initializeHomePane();
	}

	/**
	 * Sets up the transaction details box
	 *
	 * @param parameter
	 * 		the tablerow
	 * @return a VBox
	 */
	private VBox createAccountVBox(final TableRowExpanderColumn.TableRowDataFeatures<HistoryData> parameter) {
		final var data = parameter.getValue();
		final var returnBox = new VBox();
		try {
			final var remoteFile =
					new RemoteFile().getSingleRemoteFile(FileDetails.parse(new File(data.getRemoteFilePath())));
			remoteFile.setHistory(true);
			return remoteFile.buildDetailsBox();
		} catch (final HederaClientException e) {
			LOG.error(e);
		}
		final var l = new Label(data.getFileName());
		returnBox.getChildren().add(l);
		return returnBox;
	}

	// endregion

	// region FILTERS

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

	private void setupTypeFilterBox() {
		typeFilterVBox.managedProperty().bind(typeFilterVBox.visibleProperty());
		typeFilterVBox.setVisible(false);
		final var size = getModel().getTypeFilter().size();
		final var filterTitle = size > 0 ? format("filters (%d)", size) : "filters";
		final var title = new Label(filterTitle);
		title.setStyle("-fx-border-color: transparent;-fx-background-color: transparent");
		title.setPadding(new Insets(5));

		final var toolTipButton = createToolTipButton(FILTER_TOOLTIP_TEXT);

		final var titleBox = new HBox();
		titleBox.getChildren().addAll(title, toolTipButton);

		final var vBox = new VBox();
		vBox.setPadding(new Insets(5));
		vBox.setVisible(true);

		vBox.managedProperty().bind(vBox.visibleProperty());
		final var gridPane = createCheckboxesGridPane();
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
	private GridPane createCheckboxesGridPane() {
		final var gridPane = new GridPane();
		gridPane.setHgap(5);
		gridPane.setVgap(5);
		getModel().setNoise(true);
		try {
			final var selectAll = formatButton("icons/icons8-check-all-50.png", 25);
			selectAll.setText("Select all");
			final var selectNone = formatButton("icons/icons8-uncheck-all-50.png", 25);
			selectNone.setText("Select none");

			final List<CheckBox> checkBoxes = new ArrayList<>();

			EnumSet.allOf(FileType.class).forEach(type -> {
				final var typeString = type.toKind().toLowerCase();
				if (!"".equals(typeString)) {
					checkBoxes.add(createCheckBox(type, typeString));
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
		} finally {
			getModel().setNoise(false);
		}
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
	private HBox createTitleBox(final String title, final VBox filterBox) {
		final var hBox = new HBox();
		final var isTest = SetupPhase.TEST_PHASE.equals(controller.getSetupPhase());

		if (!isTest) {
			hBox.getChildren().add(new Label(title));
			hBox.getChildren().add(createRegion());
		}

		final var button = isTest ?
				new Button(title + "@@@") :
				formatButton(StyleConstants.FILTER_ICON, 20);
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

	private HBox createTwoImages(final boolean success) {
		final var sent = new Image(StyleConstants.SENT_ICON);
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
	 * Set up a tooltip button
	 *
	 * @param text
	 * 		the text of the tooltip
	 * @return a formatted button
	 */
	@NotNull
	private Button createToolTipButton(final String text) {
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
	private TableCell<HistoryData, String> createWrappingCell() {
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
	private CheckBox createCheckBox(final FileType type, final String typeString) {
		final var checkBox = new CheckBox(format("%s (%d)", typeString, getModel().countType(type)));
		checkBox.setSelected(getModel().getTypeFilter().contains(type));
		checkBox.selectedProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.FALSE.equals(t1)) {
				getModel().getTypeFilter().remove(type);
			} else {
				getModel().getTypeFilter().add(type);

			}
		});
		return checkBox;
	}


	/**
	 * Removes the fee filter. Resets the filter box
	 */
	public void resetFeeFilter() {
		feePayerTextField.clear();
		getModel().removeFeeFilter();
	}

	/**
	 * Filters the table according to the expiration date column
	 */
	public void expirationFilterAccept() {
		LOG.info("Filtering action dates");
		final var startValue = expirationStartDatePicker.getValue();
		final var endValue = expirationEndDatePicker.getValue();
		getModel().addExpirationDateFilter(startValue, endValue);
	}

	/**
	 * Remove the expiration date filter from the filter list. Resets the filter box
	 */
	public void expirationFilterReset() {
		expirationEndDatePicker.getEditor().clear();
		expirationStartDatePicker.getEditor().clear();
		getModel().removeExpirationDateFilter();
	}

	/**
	 * Filters the table according to the last action column
	 */
	public void actionFilterAccept() {
		LOG.info("Filtering action dates");
		final var startValue = actionsStartDatePicker.getValue();
		final var endValue = actionsEndDatePicker.getValue();

		final var startLocalDate = startValue != null ? startValue : LocalDate.now();
		final var endLocalDate = endValue != null ? endValue : LocalDate.now();

		if (startLocalDate.isAfter(endLocalDate)) {
			return;
		}
		getModel().addActionDateFilter();
	}

	/**
	 * Removes the action date filter from the filter list. Resets the filter box
	 */
	public void actionFilterReset() {
		declinedCheckBox.setSelected(true);
		acceptedCheckBox.setSelected(true);
		actionsStartDatePicker.getEditor().clear();
		actionsEndDatePicker.getEditor().clear();
		getModel().removeActionDateFilter();
	}

	/**
	 * Configures a region that grows to use all available space
	 *
	 * @return a Region
	 */
	@NotNull
	private Region createRegion() {
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
		updateHistoryPane();
		controller.homePaneController.setForceUpdate(true);
		controller.homePaneController.initializeHomePane();
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
		getModel().addFeePayerFilter();
	}

	public boolean isHistory(final int code) {
		return getModel().isHistory(code);
	}

	public void addFeePayerFilter() {
		getModel().addFeePayerFilter();
	}
}
