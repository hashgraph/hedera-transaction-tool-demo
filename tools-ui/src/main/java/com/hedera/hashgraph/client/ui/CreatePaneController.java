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
import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.ToolTipMessages;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.fileservices.FileAdapterFactory;
import com.hedera.hashgraph.client.core.interfaces.FileService;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.UserComments;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolFreezeTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolSystemTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.popups.KeyDesignerPopup;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.utilities.AccountAmountStrings;
import com.hedera.hashgraph.client.ui.utilities.AutoCompleteNickname;
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.FreezeType;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.HbarUnit;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PublicKey;
import javafx.beans.binding.Bindings;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.DateCell;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.util.Pair;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.ToggleSwitch;
import org.zeroturnaround.zip.ZipUtil;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.ZIP_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_TO_UPDATE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AMOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AUTO_RENEW_PERIOD_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DEL_UNDEL_SWITCH;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ENTITY_TO_DEL_UNDEL;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.EXPIRATION_DATE_TIME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FILE_CONTRACT_SWITCH;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_FILE_HASH_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_FILE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_START_TIME_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_TYPE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.INITIAL_BALANCE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NEW_KEY_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSFERS;
import static com.hedera.hashgraph.client.core.constants.Messages.TRANSACTION_CREATED_MESSAGE;
import static com.hedera.hashgraph.client.core.security.AddressChecksums.parseAddress;
import static com.hedera.hashgraph.client.core.security.AddressChecksums.parseStatus;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.RED_BORDER_STYLE;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.isNotLong;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.setCurrencyFormat;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.showTooltip;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.string2Hbar;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.stripHBarFormat;

public class CreatePaneController implements GenericFileReadWriteAware {

	// private fields
	private static final Logger logger = LogManager.getLogger(CreatePaneController.class);
	private static final int LIMIT = 255;
	protected static final double FIXED_CELL_SIZE = 30;
	private static final String SELECT_STRING = "SELECT";
	private static final String REGEX = "[^\\d]";
	private static final String TEMP_DIRECTORY = System.getProperty("java.io.tmpdir");
	private static final String START_STYLE = "-fx-background-radius: 10; -fx-border-radius: 10;";
	private static final String MENU_BUTTON_STYLE =
			"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: 10; " +
					"-fx-background-radius: 10;";
	public static final String FILE_ID_PROPERTIES = "fileID";
	public static final String FILENAME_PROPERTY = "filename";
	public static final String FEE_PAYER_ACCOUNT_ID_PROPERTY = "feePayerAccountId";
	public static final String NODE_ID_PROPERTIES = "nodeID";
	public static final String CHUNK_SIZE_PROPERTIES = "chunkSize";
	public static final String FIRS_TRANSACTION_VALID_START_PROPERTY = "firsTransactionValidStart";
	public static final String VALID_INCREMENT_PROPERTY = "validIncrement";
	public static final String TRANSACTION_VALID_DURATION_PROPERTY = "transactionValidDuration";
	public static final String MEMO_PROPERTY = "memo";
	public static final String TRANSACTION_FEE_PROPERTY = "transactionFee";
	public static final String TEXTFIELD_ERROR = "-fx-text-fill: red; -fx-background-radius: 10;-fx-border-radius: 10";
	public static final String TEXTFIELD_DEFAULT =
			"-fx-text-fill: black; -fx-background-radius: 10;-fx-border-radius: 10";
	public static final String SELECT_FREEZE_TYPE = "SELECT FREEZE TYPE";
	public static final String ACCOUNT_PARSED = "Account {} parsed";
	public static final String REMAINING_TIME_MESSAGE =
			"The transaction will expire in %d seconds. This might not be enough time to sign, " +
					"collate, and submit it";
	public static final String NINE_ZEROS = "000000000";
	public static final String FREEZE_AND_UPGRADE = "Freeze and upgrade";
	private final TimeZone timeZone = TimeZone.getDefault();
	private final TimeZone timeZoneSystem = TimeZone.getDefault();
	private final TimeZone freezeTimeZone = TimeZone.getDefault();

	private CreateTransactionType transactionType;
	private List<FileService> outputDirectories = new ArrayList<>();
	private final Set<String> accountNickNames = new HashSet<>();
	private JsonObject newKeyJSON = null;
	private JsonObject originalKey = new JsonObject();

	File contents = null;

	@FXML
	public Controller controller;

	// region FXML
	public ScrollPane createScrollPane;

	public ChoiceBox<String> selectTransactionType;

	// Buttons
	public Button createKeyButton;
	public Button acceptFromAccountButton;
	public Button acceptToAccountButton;
	public Button updateKeyButton;
	public Button browseContentsButton;
	public Button setNowValidStart;
	public Button browseTransactions;
	public Button resetFormButton;

	// Tooltip buttons
	public Button nowTimeToolTip;

	public AnchorPane createAnchorPane;

	public VBox mainVBox;
	public VBox commentsVBox;
	public VBox commonFieldsVBox;
	public VBox createAccountVBox;
	public VBox updateAccountVBox;
	public VBox transferCurrencyVBox;
	public VBox fromAccountsVBox;
	public VBox toAccountsVBox;
	public VBox accountIDToUpdateVBox;
	public VBox systemExpirationVBox;
	public VBox systemDeleteUndeleteVBox;
	public VBox fileIDToUpdateVBox;
	public VBox fileContentsUpdateVBox;
	public VBox freezeVBox;
	public VBox freezeFileVBox;
	public VBox freezeStartVBox;
	public VBox freezeChoiceVBox;

	public HBox fromHBox;
	public HBox toHBox;
	public HBox createChoiceHBox;
	public HBox systemSlidersHBox;
	public HBox copyFromAccountHBox;
	public HBox updateCopyFromAccountHBox;
	public HBox timeZoneHBox;
	public HBox timeZoneSystemHBox;
	public HBox freezeTimeZoneHBox;

	public TextArea memoField;
	public TextField feePayerAccountField;
	public TextField nodeAccountField;
	public TextField createInitialBalance;
	public TextField entityID;
	public TextField hourField;
	public TextField minuteField;
	public TextField secondsField;
	public TextField hourFieldSystem;
	public TextField minuteFieldSystem;
	public TextField secondsFieldSystem;
	public TextField updateFileID;
	public TextField chunkSizeTextField;
	public TextField keyLoadingField;
	public TextField transferToAccountIDTextField;
	public TextField transferToAmountTextField;
	public TextField transferFromAccountIDTextField;
	public TextField transferFromAmountTextField;
	public TextField updateAccountID;
	public TextField updateAutoRenew;
	public TextField updateARPOriginal;
	public TextField intervalTextField;
	public TextField createAutoRenew;
	public TextField contentsTextField;
	public TextField nanosField;
	public TextField transactionFee;
	public TextField loadTransactionTextField;
	public TextField freezeHourField;
	public TextField freezeMinuteField;
	public TextField freezeSecondsField;
	public TextField freezeNanosField;
	public TextField freezeFileIDTextField;
	public TextField freezeFileHashTextField;

	public TableView<AccountAmountStrings> fromTransferTable;
	public TableView<AccountAmountStrings> toTransferTable;

	public TextArea createCommentsTextArea;

	public DatePicker datePicker;
	public DatePicker datePickerSystem;
	public DatePicker freezeDatePicker;
	// Labels
	public Label totalTransferLabel;
	public Label createCharsLeft;
	public Label updateRSRLabel;
	public Label updateRSROriginal;
	public Label newValueLabel;
	public Label createRSRLabel;
	public Label createUTCTimeLabel;
	public Label entityLabel;
	public Label expirationLabel;
	public Label systemCreateLocalTimeLabel;
	public Label shaLabel;
	public Label invalidTransactionFee;
	public Label freezeUTCTimeLabel;

	// Error messages
	public Label invalidTransferList;
	public Label invalidTransferTotal;
	public Label invalidUpdateAccountToUpdate;
	public Label invalidUpdatedAutoRenew;
	public Label invalidUpdateNewKey;
	public Label invalidDate;
	public Label invalidFeePayer;
	public Label invalidNode;
	public Label invalidCreateInitialBalance;
	public Label invalidCreateAutoRenew;
	public Label invalidCreateNewKey;
	public Label errorInvalidFromAccount;
	public Label errorInvalidToAccount;
	public Label invalidEntity;
	public Label invalidExpirationDate;
	public Label invalidUpdateFileToUpdate;
	public Label invalidChunkSizeLabel;
	public Label invalidIntervalLabel;
	public Label contentsFilePathError;
	public Label freezeTimeErrorLabel;

	// Keys scroll panes
	public ScrollPane updateOriginalKey;
	public ScrollPane updateNewKey;
	public ScrollPane createNewKey;

	// Switches
	public ToggleSwitch updateReceiverSignatureRequired;
	public ToggleSwitch createSignatureRequired;

	public ChoiceBox<String> systemActionChoiceBox;
	public ChoiceBox<String> systemTypeChoiceBox;
	public ChoiceBox<String> freezeTypeChoiceBox;

	public Hyperlink contentsLink;
	protected static final int MEMO_LENGTH = 99;
	private boolean fromFile = false;
	private boolean noise = false;


	// endregion

	void injectMainController(Controller controller) {
		this.controller = controller;
	}

	void initializeCreatePane() {

		setupOutputDirectoriesList();

		loadAccountNicknames();

		// region INITIALIZE FIELDS
		makeBoxesInvisible();

		setupCommonFieldsEvents();

		setupSelectTransaction();

		setupManagedProperty(commentsVBox, commonFieldsVBox, createAccountVBox, updateAccountVBox, transferCurrencyVBox,
				invalidTransferTotal, invalidTransferList, createNewKey, accountIDToUpdateVBox, createChoiceHBox,
				systemDeleteUndeleteVBox, systemSlidersHBox, systemExpirationVBox, freezeVBox, freezeFileVBox,
				freezeChoiceVBox, contentsTextField, contentsLink, fileContentsUpdateVBox, fileIDToUpdateVBox,
				freezeStartVBox, shaLabel, contentsFilePathError, invalidUpdateNewKey, resetFormButton,
				freezeUTCTimeLabel, freezeTimeErrorLabel, invalidDate);

		setupTransferFields();

		setupUpdateFields();

		setupCreateFields();

		setupSystemFields();

		setupFileContentsFields();

		setupFreezeFields();

		setupTooltips();
	}

	private void setupSelectTransaction() {
		selectTransactionType.getItems().clear();
		selectTransactionType.setItems(CreateTransactionType.names());
		selectTransactionType.setValue(SELECT_STRING);
	}

	private void makeBoxesInvisible() {
		commonFieldsVBox.setVisible(false);
		createAccountVBox.setVisible(false);
		updateAccountVBox.setVisible(false);
		transferCurrencyVBox.setVisible(false);
		accountIDToUpdateVBox.setVisible(false);
		createChoiceHBox.setVisible(false);
		systemDeleteUndeleteVBox.setVisible(false);
		fileContentsUpdateVBox.setVisible(false);
		freezeVBox.setVisible(false);
	}

	private void setupTransferFields() {
		transferTableEvents(transferFromAccountIDTextField, transferFromAmountTextField, fromTransferTable,
				acceptFromAccountButton, errorInvalidFromAccount);
		fromTransferTable.prefWidthProperty().bind(fromHBox.widthProperty());
		fromTransferTable.prefHeightProperty().bind(fromTransferTable.heightProperty().multiply(.4));

		transferTableEvents(transferToAccountIDTextField, transferToAmountTextField, toTransferTable,
				acceptToAccountButton, errorInvalidToAccount);
		toTransferTable.prefWidthProperty().bind(toHBox.widthProperty());
		toTransferTable.prefHeightProperty().bind(toTransferTable.heightProperty().multiply(.4));
		transferFromAmountTextField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixTimeTextField(transferFromAmountTextField, newValue, "\\d*",
						"[^\\d.]"));
		transferToAmountTextField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixTimeTextField(transferToAmountTextField, newValue, "\\d*",
						"[^\\d.]"));

		BooleanProperty transferBoolean = new SimpleBooleanProperty();
		transferBoolean.setValue(toTransferTable.getItems().isEmpty() ^ fromTransferTable.getItems().isEmpty());

		invalidTransferList.visibleProperty().bind(transferBoolean);
	}

	private void setupCreateFields() {
		setupNewKeyObject();
		formatHBarTextField(createInitialBalance);
		loadAccountNicknames();
		var autoCompleteNickname = new AutoCompleteNickname(accountNickNames);
		autoCompleteNickname.setVisible(false);
		autoCompleteNickname.managedProperty().bind(autoCompleteNickname.visibleProperty());
		copyFromAccountHBox.getChildren().clear();
		copyFromAccountHBox.getChildren().add(autoCompleteNickname);
		createSignatureRequired.selectedProperty().addListener(
				(observableValue, aBoolean, t1) -> createRSRLabel.setText(Boolean.TRUE.equals(t1) ? "true" : "false"
				));

		createKeyButton.setOnAction(e -> {
			autoCompleteNickname.clear();
			autoCompleteNickname.setVisible(false);

			var key = newKeyJSON;
			var keyDesignerPopup = !newKeyJSON.equals(new JsonObject()) ?
					new KeyDesignerPopup(getStringPublicKeyMap(), EncryptionUtils.jsonToKey(newKeyJSON)) :
					new KeyDesignerPopup(getStringPublicKeyMap());
			key = keyDesignerPopup.display();
			createNewKey.setVisible(true);
			processKey(key, createNewKey);
		});


		autoCompleteNickname.setOnKeyReleased(
				keyEvent -> getKeyFromNickname(autoCompleteNickname, keyEvent.getCode(), createNewKey));
	}

	private Map<String, PublicKey> getStringPublicKeyMap() {
		Map<String, PublicKey> publicKeys = new HashMap<>();
		var keys =
				new File(KEYS_FOLDER).listFiles((dir, name) -> name.endsWith(PUB_EXTENSION));
		assert keys != null;
		Arrays.stream(keys).forEach(keyFile -> publicKeys.put(FilenameUtils.getBaseName(keyFile.getName()),
				EncryptionUtils.publicKeyFromFile(keyFile.getAbsolutePath())));
		return publicKeys;
	}

	private void setupUpdateFields() {
		setupKeyPane(new TreeView<>(), updateNewKey);


		updateAutoRenew.textProperty().addListener(
				(observable, oldValue, newValue) -> fixTimeTextField(updateAutoRenew, newValue, "\\d*", REGEX));

		updateReceiverSignatureRequired.selectedProperty().addListener(
				(observableValue, aBoolean, t1) -> updateRSRLabel.setText(Boolean.TRUE.equals(t1) ? "true" : "false"));
		loadAccountNicknames();
		var updateFromNickName = new AutoCompleteNickname(accountNickNames);
		updateFromNickName.setVisible(false);
		updateFromNickName.managedProperty().bind(updateFromNickName.visibleProperty());
		updateCopyFromAccountHBox.getChildren().clear();
		updateCopyFromAccountHBox.getChildren().add(updateFromNickName);
		formatAccountTextField(updateAccountID, invalidUpdateAccountToUpdate, updateAccountID.getParent());

		updateAccountID.setOnKeyPressed(keyEvent -> {
			final var keyCode = keyEvent.getCode();
			if (keyCode.equals(KeyCode.TAB) || keyCode.equals(KeyCode.ENTER)) {
				findAccountInfoAndPreloadFields();
			}
		});

		updateKeyButton.setOnAction(e -> {
			updateFromNickName.setVisible(false);
			updateFromNickName.clear();

			newValueLabel.setVisible(true);
			updateNewKey.setVisible(true);
			var keyDesignerPopup =
					new KeyDesignerPopup(getStringPublicKeyMap(), EncryptionUtils.jsonToKey(newKeyJSON));
			var key = keyDesignerPopup.display();
			if (key == null || key.equals(new JsonObject())) {
				key = originalKey;
			}
			var keyTree = controller.buildKeyTreeView(key);
			keyTree.setStyle("-fx-border-color: white; -fx-background-color: white");
			keyTree.setMinWidth(800);
			updateNewKey.setContent(keyTree);
			keyTree.prefWidthProperty().bind(updateNewKey.widthProperty());
			newKeyJSON = key;
		});


		updateFromNickName.setOnKeyReleased(
				keyEvent -> getKeyFromNickname(updateFromNickName, keyEvent.getCode(), updateNewKey));
	}

	private void setupFileContentsFields() {
		fileIDToUpdateVBox.visibleProperty().bind(fileContentsUpdateVBox.visibleProperty());

		contentsTextField.setOnKeyReleased(keyEvent -> {
			contentsFilePathError.setVisible(false);
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				if ("".equals(contentsTextField.getText())) {
					return;
				}
				contents = new File(contentsTextField.getText());
				setContentsAction();
			}
		});

		formatAccountTextField(updateFileID, invalidUpdateFileToUpdate, updateFileID.getParent());

		contentsLink.setOnAction(actionEvent -> {
			final var destFile =
					new File(TEMP_DIRECTORY, contents.getName().replace(" ", "_"));
			try {
				org.apache.commons.io.FileUtils.copyFile(contents,
						destFile);
			} catch (IOException e) {
				logger.error(e);
				controller.displaySystemMessage(e);
				return;
			}
			var r = Runtime.getRuntime();
			var command = String.format("open -e %s", destFile.getAbsolutePath());
			try {
				r.exec(command);
			} catch (IOException e) {
				logger.error(e);
				controller.displaySystemMessage(e);
			}
		});

		setupIntNumberField(chunkSizeTextField, 4095);
		setupIntNumberField(intervalTextField, Integer.MAX_VALUE);

	}

	private void setupSystemFields() {
		setupTimeZoneChooser(timeZoneSystem, timeZoneSystemHBox, datePickerSystem, hourFieldSystem,
				minuteFieldSystem, secondsFieldSystem, new TextField(NINE_ZEROS), systemCreateLocalTimeLabel,
				invalidExpirationDate);
		systemSlidersHBox.visibleProperty().bind(systemDeleteUndeleteVBox.visibleProperty());
		systemTypeChoiceBox.getItems().clear();
		systemTypeChoiceBox.getItems().addAll("File", "Smart Contract");
		systemTypeChoiceBox.getSelectionModel().select(0);

		systemTypeChoiceBox.getSelectionModel().selectedIndexProperty().addListener((observableValue, number, t1) -> {
			switch (t1.intValue()) {
				case 0:
					entityLabel.setText("File ID");
					expirationLabel.setText("File will expire on:");
					break;
				case 1:
					entityLabel.setText("Contract ID");
					expirationLabel.setText("Contract will expire on:");
					break;
				default:
					if (!systemTypeChoiceBox.getItems().isEmpty()) {
						logger.error("Unexpected value: {}", t1.intValue());
					} else {
						logger.info("System type choice box cleared");
					}
			}
		});

		datePicker.valueProperty().addListener(
				(observableValue, localDate, t1) -> {
					systemExpirationVBox.setDisable(t1 == null);
					freezeStartVBox.setDisable(t1 == null);
					if (datePicker.getValue() != null) {
						var localTime =
								LocalTime.of(Integer.parseInt(hourField.getText()),
										Integer.parseInt(minuteField.getText()),
										Integer.parseInt(secondsField.getText()));

						var start = LocalDateTime.of(datePicker.getValue(), localTime);
						configureDateTime(datePickerSystem, hourFieldSystem, minuteFieldSystem, secondsFieldSystem,
								new TextField(NINE_ZEROS), systemCreateLocalTimeLabel, start, timeZoneSystem,
								invalidExpirationDate);
						configureDateTime(freezeDatePicker, freezeHourField, freezeMinuteField, freezeSecondsField,
								freezeNanosField, freezeUTCTimeLabel, start, freezeTimeZone, freezeTimeErrorLabel);
					} else {
						resetSystemTime();
						resetFreezeTime();
					}
				});

		systemActionChoiceBox.getItems().clear();
		systemActionChoiceBox.getItems().addAll("Remove Content", "Restore Content");
		systemActionChoiceBox.getSelectionModel().select(0);
		systemActionChoiceBox.getSelectionModel().selectedIndexProperty().addListener(
				(observableValue, number, t1) -> systemExpirationVBox.setVisible(t1.intValue() == 0));

		formatAccountTextField(entityID, invalidEntity, entityID.getParent());
	}

	private void setupFreezeFields() {
		setupTimeZoneChooser(freezeTimeZone, freezeTimeZoneHBox, freezeDatePicker, freezeHourField,
				freezeMinuteField, freezeSecondsField, freezeNanosField, freezeUTCTimeLabel, invalidExpirationDate);
		freezeChoiceVBox.setVisible(false);
		freezeFileVBox.setVisible(false);
		freezeStartVBox.setVisible(false);
		noise = true;
		var freezeValues = Arrays.asList(FreezeType.values());
		List<String> freezeValuesAsStrings =
				freezeValues.stream().map(
						freezeValue -> {
							var name = StringUtils.capitalize(
									freezeValue.toString().toLowerCase(Locale.ROOT).replace("_", " "));
							if (name.equals("Freeze upgrade")) {
								name = FREEZE_AND_UPGRADE;
							}
							return name;
						}).collect(Collectors.toList());
		freezeValuesAsStrings.remove("Unknown freeze type");
		freezeValuesAsStrings.remove("Telemetry upgrade");

		freezeTypeChoiceBox.getItems().clear();
		freezeTypeChoiceBox.getItems().add(SELECT_FREEZE_TYPE);
		freezeTypeChoiceBox.getItems().addAll(freezeValuesAsStrings);
		freezeTypeChoiceBox.getSelectionModel().select(SELECT_FREEZE_TYPE);
		noise = false;

		freezeTypeChoiceBox.getSelectionModel().selectedItemProperty().addListener((observableValue, s, t1) -> {
			if (noise) {
				return;
			}
			if (SELECT_FREEZE_TYPE.equals(t1)) {
				logger.info("Back to select");
				cleanAllFreezeFields();
				freezeFileVBox.setVisible(false);
				freezeStartVBox.setVisible(false);
				return;
			}
			if (FREEZE_AND_UPGRADE.equals(t1)) {
				t1 = "freeze upgrade";
			}
			FreezeType type = FreezeType.valueOf(t1.replace(" ", "_").toUpperCase(Locale.ROOT));
			switch (type) {
				case FREEZE_ONLY:
					// Freezes the network at the specified time. The start_time field must be provided and must
					// reference a future time. Any values specified for the update_file and file_hash fields will
					// be ignored. This transaction does not perform any network changes or upgrades and requires
					// manual intervention to restart the network.
					logger.info("Freeze only selected");
					freezeFileVBox.setVisible(false);
					freezeStartVBox.setVisible(true);
					break;
				case PREPARE_UPGRADE:
					// A non-freezing operation that initiates network wide preparation in advance of a scheduled
					// freeze upgrade. The update_file and file_hash fields must be provided and valid. The
					// start_time field may be omitted and any value present will be ignored.
					logger.info("Prepare upgrade selected");
					freezeFileVBox.setVisible(true);
					freezeStartVBox.setVisible(false);
					break;
				case FREEZE_UPGRADE:
					// Freezes the network at the specified time and performs the previously prepared automatic
					// upgrade across the entire network.
					logger.info("Freeze upgrade selected");
					freezeFileVBox.setVisible(true);
					freezeStartVBox.setVisible(true);
					break;
				case FREEZE_ABORT:
					// Aborts a pending network freeze operation.
					logger.info("Freeze abort selected");
					freezeFileVBox.setVisible(false);
					freezeStartVBox.setVisible(false);
					break;
				case TELEMETRY_UPGRADE:
					// Performs an immediate upgrade on auxilary services and containers providing
					// telemetry/metrics. Does not impact network operations.
					logger.info("Telemetry upgrade selected");
					freezeFileVBox.setVisible(true);
					freezeStartVBox.setVisible(true);
					break;
				default:
					throw new IllegalStateException("Unexpected value: " + type);
			}
		});

	}

	private void setupTooltips() {
		// All tooltips will be added here
		nowTimeToolTip.setOnAction(
				actionEvent -> showTooltip(controller.settingsPane, nowTimeToolTip,
						ToolTipMessages.NOW_TOOLTIP_TEXT));
	}

	private void loadAccountNicknames() {
		try {
			var nicknames =
					new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

			var accountInfos = controller.accountsPaneController.getIdNickNames();
			for (var s : accountInfos.keySet()) {
				if (nicknames.has(s)) {
					accountNickNames.add(nicknames.get(s).getAsString());
				}
			}
		} catch (HederaClientException e) {
			controller.logAndDisplayError(e);
		}
	}

	private void setupTimeZoneChooser(TimeZone zone, HBox hBox, DatePicker date, TextField hour,
			TextField minute, TextField seconds, TextField nanos, Label label, Label errorLabel) {
		var chooser = new AutoCompleteNickname(ZoneId.getAvailableZoneIds());
		chooser.setDefault(zone.getID());
		hBox.getChildren().clear(); // need to clear the hox before entering the new Field.
		hBox.getChildren().add(chooser);
		chooser.textProperty().addListener((observableValue, aBoolean, t1) -> {
			var timeZones = ZoneId.getAvailableZoneIds();
			if (!timeZones.contains(chooser.getText())) {
				return;
			}
			if (date.getValue() != null) {
				var instant = getDate(date, hour, minute, seconds, ZoneId.of(zone.getID())).asInstant();
				var ldt = LocalDateTime.ofInstant(instant, ZoneId.of(chooser.getText()));
				date.setValue(ldt.toLocalDate());
				hour.setText(String.valueOf(ldt.getHour()));
				minute.setText(String.format("%02d", ldt.getMinute()));
				seconds.setText(String.format("%02d", ldt.getSecond()));
				nanos.setText(String.format("%09d", ldt.getNano()));
				label.setText("");
			}
			zone.setID(chooser.getText());
			logger.info("Timezone changed to: {}", zone.getID());
			setLocalDateString(date, hour, minute, seconds, nanos, zone, label, errorLabel);
		});
	}

	private void getKeyFromNickname(AutoCompleteNickname autoCompleteNickname, KeyCode keyCode, ScrollPane scrollPane) {
		if (keyCode.equals(KeyCode.ENTER)) {
			var nick = autoCompleteNickname.getFirstItem();
			autoCompleteNickname.setText(nick);
			var nicknames = controller.accountsPaneController.getIdNickNames();
			var id = "";
			for (var entry : nicknames.entrySet()) {
				if (nick.equals(entry.getValue())) {
					id = entry.getKey();
				}
			}

			if (!controller.accountsPaneController.getAccountInfos().containsKey(id)) {
				return;
			}
			var address = controller.accountsPaneController.getAccountInfos().get(id);
			logger.info("Chosen account: {}", nick);
			try {
				var info =
						AccountInfo.fromBytes(readBytes(address));
				var key = info.key;
				if (key != null) {
					setKeyFromFile(key, scrollPane);
					scrollPane.requestFocus();
				}
			} catch (InvalidProtocolBufferException | HederaClientException e) {
				logger.error(e);
			}
			autoCompleteNickname.clear();
			autoCompleteNickname.setVisible(false);
		}
	}

	private void setKeyTreeInBox(JsonObject key, ScrollPane keyScrollPane) {
		var keyTree = controller.buildKeyTreeView(key);
		keyTree.setStyle("-fx-border-color: white; -fx-background-color: white");
		keyScrollPane.setContent(keyTree);
		keyTree.setMinWidth(800);
		keyTree.prefWidthProperty().bind(createNewKey.widthProperty());
		newKeyJSON = key;
	}

	private void setKeyFromFile(Key newKey, ScrollPane createScrollPane) {
		JsonObject keyJson = EncryptionUtils.keyToJson(newKey);

		setKeyTreeInBox(keyJson, createScrollPane);
		createScrollPane.setVisible(true);
	}

	private void setupOutputDirectoriesList() {
		try {
			if (controller.getOneDriveCredentials() != null) {
				var inputs = controller.getOneDriveCredentials().keySet();
				outputDirectories = new ArrayList<>();
				for (var s :
						inputs) {
					var fs = FileAdapterFactory.getAdapter(s);
					if (fs != null && fs.exists()) {
						outputDirectories.add(fs);
					}
				}
			}
		} catch (HederaClientException e) {
			logger.error(e);
			controller.displaySystemMessage(e);
		}
	}

	// region CREATE ACCOUNT

	private Pair<UserComments, ToolTransaction> createAccountTransactionAction() {
		if (!checkAndFlagCreateFields()) {
			return null;
		}

		var input = buildJsonInput();

		try {
			var tx = new ToolCryptoCreateTransaction(input);
			displayAndLogInformation("Create Account " + TRANSACTION_CREATED_MESSAGE);
			return getUserCommentsTransactionPair(tx);
		} catch (HederaClientException e) {
			logger.error(e);
			return null;
		}

	}

	private void cleanAllCreateFields() {
		cleanFields();
		clearErrorMessages(invalidCreateAutoRenew, invalidDate, invalidFeePayer, invalidCreateNewKey, invalidNode);
	}

	private void clearErrorMessages(Label... errorMessages) {
		for (var errorMessage : errorMessages) {
			errorMessage.setVisible(false);
		}
	}

	private boolean checkAndFlagCreateFields() {
		var flag = checkAndFlagCommonFields();

		if (newKeyJSON == null || new KeyList().equals(EncryptionUtils.jsonToKey(newKeyJSON))) {
			invalidCreateNewKey.setVisible(true);
			flag = false;
		}

		if (flag) {
			clearErrorMessages(invalidCreateAutoRenew, invalidDate, invalidFeePayer, invalidCreateNewKey, invalidNode);

		}
		return flag;
	}

	// endregion

	// region UPDATE ACCOUNT

	public Pair<UserComments, ToolTransaction> updateTransactionAction() {
		if (!checkAndFlagUpdateFields()) {
			return null;
		}
		try {
			var input = buildJsonInput();
			var tx = new ToolCryptoUpdateTransaction(input);
			displayAndLogInformation("Update Account " + TRANSACTION_CREATED_MESSAGE);
			return getUserCommentsTransactionPair(tx);
		} catch (HederaClientException e) {
			logger.error(e);
			return null;
		}
	}


	private boolean checkAndFlagUpdateFields() {
		var flag = checkAndFlagCommonFields();

		try {
			if ("".equals(updateAccountID.getText())) {
				invalidUpdateAccountToUpdate.setVisible(true);
			} else {
				var account = Identifier.parse(updateAccountID.getText());
				updateAccountID.setText(account.toNicknameAndChecksum(controller.getAccountsList()));
			}
		} catch (Exception e) {
			invalidUpdateAccountToUpdate.setVisible(true);
			displayAndLogInformation("Fee payer cannot be parsed");
			flag = false;
		}

		if ("".equals(updateAutoRenew.getText()) || Long.parseLong(updateAutoRenew.getText()) <= 0L || Long.parseLong(
				updateAutoRenew.getText()) > 7776000L) {
			invalidUpdatedAutoRenew.setVisible(true);
			controller.displaySystemMessage("Error: auto renew field not in range");
			flag = false;
		} else {
			invalidUpdatedAutoRenew.setVisible(false);
		}

		return flag;
	}

	private void cleanAllUpdateFields() {
		updateAccountID.clear();
		cleanFields();
		updateARPOriginal.clear();
		updateRSROriginal.setText("???");
		updateOriginalKey.setContent(new HBox());

		clearErrorMessages(invalidUpdatedAutoRenew, invalidDate, invalidFeePayer, invalidUpdateNewKey, invalidNode,
				invalidUpdateAccountToUpdate);

	}

	private void findAccountInfoAndPreloadFields() {
		var accountsInfoMap = controller.getAccountInfoMap();
		try {
			var account = Identifier.parse(updateAccountID.getText());
			if (accountsInfoMap.containsKey(account)) {
				var accountInfo = accountsInfoMap.get(account);
				updateARPOriginal.setText(String.format("%d s", accountInfo.autoRenewPeriod.getSeconds()));
				updateRSROriginal.setText(String.valueOf(accountInfo.isReceiverSignatureRequired));
				controller.loadPubKeys();
				var jsonObjectKey = EncryptionUtils.keyToJson(accountInfo.key);
				originalKey = EncryptionUtils.keyToJson(accountInfo.key);
				var oldKeyTreeView = controller.buildKeyTreeView(jsonObjectKey);
				setupKeyPane(oldKeyTreeView, updateOriginalKey);

				if (!fromFile) {
					updateReceiverSignatureRequired.setSelected(accountInfo.isReceiverSignatureRequired);
					updateAutoRenew.setText(String.format("%d", accountInfo.autoRenewPeriod.getSeconds()));
					newKeyJSON = EncryptionUtils.keyToJson(accountInfo.key);
					final var newKeyTreeView = controller.buildKeyTreeView(jsonObjectKey);
					setupKeyPane(newKeyTreeView, updateNewKey);
				}
				// in case they were visible before
				clearErrorMessages(invalidUpdatedAutoRenew, invalidDate, invalidFeePayer, invalidUpdateNewKey,
						invalidNode, invalidUpdateAccountToUpdate);

			}
		} catch (Exception e) {
			logger.info("Not an account ID");
			invalidUpdateAccountToUpdate.setVisible(true);
		}
	}

	// endregion

	// region TRANSFER

	public Pair<UserComments, ToolTransaction> createTransferTransactionAction() throws HederaClientException {

		if (!checkAndFlagTransferFields()) {
			return null;
		}
		var input = buildJsonInput();
		var tx = new ToolTransferTransaction(input);
		displayAndLogInformation("Transfer " + TRANSACTION_CREATED_MESSAGE);
		return getUserCommentsTransactionPair(tx);
	}

	private void updateTotalAmount() {
		var credits = toTransferTable.getItems();
		var debits = fromTransferTable.getItems();

		long total = 0;
		for (AccountAmountStrings a : credits) {
			total += a.getAmountAsLong();
		}
		for (AccountAmountStrings a : debits) {
			total -= a.getAmountAsLong();
		}

		var sign = total < 0 ? "-" : "";

		final var totalString = Utilities.setHBarFormat(Math.abs(total));
		if (total > 0) {
			transferFromAmountTextField.setText(totalString);
		} else if (total < 0) {
			transferToAmountTextField.setText(totalString);
		} else {
			transferFromAmountTextField.clear();
			transferToAmountTextField.clear();
		}

		totalTransferLabel.setText(sign + totalString);
	}

	private boolean checkAndFlagTransferFields() {

		var flag = checkAndFlagCommonFields();

		if (Long.parseLong(totalTransferLabel.getText().replace("\u0127", "")
				.replace(" ", "")
				.replace(".", "")
				.replace("-", "")) != 0) {
			invalidTransferTotal.setVisible(true);
			displayAndLogInformation("Invalid TransferList");
			flag = false;
		} else {
			invalidTransferTotal.setVisible(false);
		}

		if (toTransferTable.getItems().isEmpty() || fromTransferTable.getItems().isEmpty()) {
			displayAndLogInformation("Missing sender or recipient");
			flag = false;
		}

		return flag;
	}

	private void cleanAllTransferFields() {

		cleanCommonFields();
		createCommentsTextArea.clear();

		toTransferTable.getItems().clear();
		fromTransferTable.getItems().clear();
		transferCurrencyVBox.setVisible(false);

		clearErrorMessages(invalidDate, invalidFeePayer, invalidNode);

		initializeTable(fromTransferTable);
		initializeTable(toTransferTable);
	}

	private void initializeTable(TableView<AccountAmountStrings> table) {
		ObservableList<AccountAmountStrings> data = FXCollections.observableArrayList();
		table.setFixedCellSize(FIXED_CELL_SIZE);
		table.setStyle("-fx-font-size: " + FIXED_CELL_SIZE / 2);

		table.setEditable(true);

		table.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

		var accountColumn = new TableColumn<AccountAmountStrings, String>("Account ID");
		accountColumn.setCellValueFactory(new PropertyValueFactory<>("accountID"));

		var amountColumn = new TableColumn<AccountAmountStrings, String>("Amount");
		amountColumn.setCellValueFactory(new PropertyValueFactory<>("amount"));

		table.setItems(data);
		table.getColumns().clear();
		table.getColumns().addAll(accountColumn, amountColumn);

		accountColumn.prefWidthProperty().bind(table.widthProperty().multiply(0.395));
		amountColumn.prefWidthProperty().bind(table.widthProperty().multiply(0.60));

		accountColumn.setResizable(false);
		amountColumn.setResizable(false);

		var contextMenu = new ContextMenu();
		var menuItem = new MenuItem("delete");

		contextMenu.getItems().addAll(menuItem);

		table.addEventHandler(MouseEvent.MOUSE_CLICKED, mouseEvent -> {
			if (mouseEvent.getButton() == MouseButton.SECONDARY) {
				contextMenu.show(table, mouseEvent.getScreenX(), mouseEvent.getScreenY());
			}
		});

		menuItem.setOnAction(event -> {
			if (!table.getItems().isEmpty()) {
				var selectedItem = table.getSelectionModel().getSelectedItem();
				table.getItems().remove(selectedItem);
				updateTotalAmount();
			}
		});


		table.getItems().addListener(
				(ListChangeListener<AccountAmountStrings>) change -> table.setMinHeight(table.getFixedCellSize() * (
						!change.getList().isEmpty() ? table.getItems().size() + 1.1 : 2.1)));
		table.prefHeightProperty().bind(
				table.fixedCellSizeProperty().multiply(Bindings.size(table.getItems()).add(1.1)));

		table.maxHeightProperty().bind(table.prefHeightProperty());
		table.managedProperty().bind(table.visibleProperty());
	}

	private void addAccountAmountToTable(TextField account, TextField amount,
			TableView<AccountAmountStrings> thisTable) {
		if ("".equals(account.getText())) {
			PopupMessage.display("Empty account", "Missing account ID");
			return;
		}
		if ("".equals(amount.getText())) {
			PopupMessage.display("Empty amount", "Missing amount to be transferred");
			return;
		}

		account.setStyle(null);
		account.setStyle(START_STYLE);
		amount.setStyle(null);
		amount.setStyle(START_STYLE);

		var newTransaction =
				new AccountAmountStrings(account.getText(), stripHBarFormat(amount.getText()));

		final var status = parseAddress(newTransaction.getAccountID()).getStatus();
		if (status.equals(parseStatus.BAD_CHECKSUM) || status.equals(parseStatus.BAD_FORMAT)) {
			account.setStyle(RED_BORDER_STYLE);
			account.selectAll();
			account.requestFocus();
			return;
		}

		if (isNotLong(stripHBarFormat(amount.getText()))) {
			amount.setStyle(RED_BORDER_STYLE);
			amount.selectAll();
			amount.requestFocus();
			return;
		}

		thisTable.getItems().add(newTransaction);

		updateTotalAmount();
		account.clear();
		amount.clear();
		account.requestFocus();
	}

	private void transferTableEvents(TextField accountIDTextField, TextField amountTextField,
			TableView<AccountAmountStrings> table, Button acceptButton, Label errorLabel) {

		formatAccountTextField(accountIDTextField, errorLabel, amountTextField);

		amountTextField.setOnKeyReleased((KeyEvent event) -> {
			if (event.getCode() == KeyCode.TAB) {
				setHBarFormat(amountTextField);
				acceptButton.requestFocus();
			}
			if (event.getCode() == KeyCode.ENTER) {
				setHBarFormat(amountTextField);
				addAccountAmountToTable(accountIDTextField, amountTextField, table);
			}
		});

		amountTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				setHBarFormat(amountTextField);
			}
		});


		acceptButton.setOnKeyPressed((KeyEvent event) -> {
			if (event.getCode() == KeyCode.ENTER || event.getCode() == KeyCode.SPACE) {
				addAccountAmountToTable(accountIDTextField, amountTextField, table);
			}
		});
		acceptButton.setOnMouseClicked(event -> addAccountAmountToTable(accountIDTextField, amountTextField, table));
	}

	// endregion

	// region SYSTEM
	public void cleanAllSystemFields() {
		cleanCommonFields();
		hourFieldSystem.setText("01");
		minuteFieldSystem.setText("00");
		secondsField.setText("00");
		datePickerSystem.setValue(null);
		entityID.clear();
	}

	private void resetSystemTime() {
		datePickerSystem.setValue(null);
		hourFieldSystem.setText("01");
		minuteFieldSystem.setText("00");
		secondsFieldSystem.setText("00");
		logger.info("Expiration time cleared");
	}

	private void resetFreezeTime() {
		freezeDatePicker.setValue(null);
		freezeHourField.setText("01");
		freezeMinuteField.setText("00");
		freezeSecondsField.setText("00");
		logger.info("Freeze time cleared");
	}

	private boolean checkSystemFields() {

		if (!checkAndFlagCommonFields()) {
			return false;
		}

		try {
			var account = Identifier.parse(entityID.getText()).toReadableString();
			logger.info(ACCOUNT_PARSED, account);
		} catch (Exception e) {
			return false;
		}

		if (systemActionChoiceBox.getSelectionModel().getSelectedItem().contains("Remove")) {
			var validExpiration =
					isDateValid(hourFieldSystem, minuteFieldSystem, secondsFieldSystem, new TextField(NINE_ZEROS),
							datePickerSystem, ZoneId.of(timeZoneSystem.getID()), timeZoneSystemHBox);
			invalidExpirationDate.setVisible(!validExpiration);
			return validExpiration;
		}
		return true;
	}

	private boolean checkFreezeFields() {
		if (!checkAndFlagCommonFields()) {
			return false;
		}
		final var choice = freezeTypeChoiceBox.getValue();
		if (SELECT_FREEZE_TYPE.equals(choice)) {
			return false;
		}


		var freezeType = FREEZE_AND_UPGRADE.equals(choice) ?
				FreezeType.FREEZE_UPGRADE :
				FreezeType.valueOf(choice.toUpperCase(Locale.ROOT).replace(" ", "_"));

		var validStart = true;
		var validFile = true;
		var validHash = true;
		switch (freezeType) {
			case FREEZE_ONLY:
				validStart =
						isDateValid(freezeHourField, freezeMinuteField, freezeSecondsField, freezeNanosField,
								datePicker, ZoneId.of(freezeTimeZone.getID()), freezeTimeZoneHBox);
				freezeTimeErrorLabel.setVisible(!validStart);
				break;
			case PREPARE_UPGRADE:
				try {
					var file = Identifier.parse(freezeFileIDTextField.getText()).toReadableString();
					logger.info(ACCOUNT_PARSED, file);
				} catch (Exception e) {
					validFile = false;
				}
				validHash = !"".equals(freezeFileHashTextField.getText());
				break;
			case FREEZE_UPGRADE:
			case TELEMETRY_UPGRADE:
				validStart =
						isDateValid(freezeHourField, freezeMinuteField, freezeSecondsField, freezeNanosField,
								datePicker, ZoneId.of(freezeTimeZone.getID()), freezeTimeZoneHBox);
				freezeTimeErrorLabel.setVisible(!validStart);
				try {
					var file = Identifier.parse(freezeFileIDTextField.getText()).toReadableString();
					logger.info(ACCOUNT_PARSED, file);
				} catch (Exception e) {
					validFile = false;
				}
				validHash = !"".equals(freezeFileHashTextField.getText());
				break;
			case FREEZE_ABORT:
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + freezeType);
		}
		return validStart && validFile && validHash;
	}

	private boolean checkAndFlagSystemFields() {
		var flag = checkAndFlagCommonFields();
		return flag && checkSystemFields();
	}

	private boolean checkAndFlagFreezeFields() {
		var flag = checkAndFlagCommonFields();
		final var freezeFlag = checkFreezeFields();
		return flag && freezeFlag;
	}

	private Pair<UserComments, ToolTransaction> createSystemTransactionAction() {
		if (!checkAndFlagSystemFields()) {
			return null;
		}

		var input = buildJsonInput();

		try {
			var tx = new ToolSystemTransaction(input);
			displayAndLogInformation("System transaction created");
			return getUserCommentsTransactionPair(tx);
		} catch (HederaClientException e) {
			controller.displaySystemMessage(e);
			logger.error(e);
			return null;
		}

	}

	private Pair<UserComments, ToolTransaction> createFreezeTransaction() {
		if (!checkAndFlagFreezeFields()) {
			return null;
		}

		var input = buildJsonInput();

		try {
			var tx = new ToolFreezeTransaction(input);
			displayAndLogInformation("Freeze transaction created");
			return getUserCommentsTransactionPair(tx);
		} catch (HederaClientException e) {
			controller.displaySystemMessage(e);
			logger.error(e);
			return null;
		}
	}

	// endregion

	// region FILES

	/**
	 * Reset the form
	 */
	public void cleanAllFileUpdateContentsFields() {
		cleanCommonFields();
		updateFileID.clear();
		contentsLink.setText("");
		contentsLink.setVisible(false);
		contentsTextField.clear();
		intervalTextField.clear();
		chunkSizeTextField.clear();
		shaLabel.setText("");
		shaLabel.setVisible(false);
		contentsFilePathError.setVisible(false);
		contents = null;
	}


	private void cleanAllFreezeFields() {
		cleanCommonFields();
		freezeTypeChoiceBox.getSelectionModel().select(SELECT_FREEZE_TYPE);
		freezeDatePicker.setValue(null);
		freezeHourField.setText("00");
		freezeMinuteField.setText("00");
		freezeSecondsField.setText("00");
		freezeNanosField.setText(NINE_ZEROS);
		freezeFileIDTextField.clear();
		freezeFileHashTextField.clear();
	}

	/**
	 * Error check the form
	 *
	 * @return true if the form checks out
	 */
	private boolean checkAndFlagFileUpdateContentsFields() {
		// Check common fields
		var flag = checkAndFlagCommonFields();

		// Check file id field
		try {
			var account = Identifier.parse(updateFileID.getText());
			updateFileID.setText(account.toReadableString());
		} catch (Exception e) {
			displayAndLogInformation(e);
			invalidUpdateFileToUpdate.setVisible(true);
			flag = false;
		}

		// check chunk size field
		try {
			var chunk = Integer.parseInt(chunkSizeTextField.getText());
			if (chunk <= 0 || chunk > 4095) {
				invalidChunkSizeLabel.setVisible(true);
				flag = false;
			}
		} catch (NumberFormatException e) {
			displayAndLogInformation(e);
			invalidChunkSizeLabel.setVisible(true);
			flag = false;
		}

		// Check interval field
		try {
			var interval = Integer.parseInt(intervalTextField.getText());
			if (interval <= 0) {
				invalidIntervalLabel.setVisible(true);
				flag = false;
			}
		} catch (NumberFormatException e) {
			displayAndLogInformation(e);
			invalidIntervalLabel.setVisible(true);
			flag = false;
		}

		// check contents
		try {
			if (contents == null || !contents.exists() || contents.isDirectory()) {
				contentsFilePathError.setVisible(true);
				flag = false;
			}
		} catch (Exception e) {
			displayAndLogInformation(e);
			contentsFilePathError.setVisible(true);
			flag = false;
		}

		// making sure all errors are cleared if the transaction is valid
		if (flag) {
			invalidUpdateFileToUpdate.setVisible(false);
			invalidChunkSizeLabel.setVisible(false);
			invalidIntervalLabel.setVisible(false);
			contentsFilePathError.setVisible(false);
		}

		return flag;
	}

	/**
	 * Prepare all files necessary for the App to create the transactions through the Home Pane
	 *
	 * @param remoteLocation
	 * 		the location to store the zip
	 */
	private void prepareZipAndComment(FileService remoteLocation) throws HederaClientException {
		if (!checkForm()) {
			return;
		}

		// setup json file
		var outputObject = new JsonObject();
		outputObject.addProperty(FILENAME_PROPERTY, contents.getName());
		outputObject.add(FILE_ID_PROPERTIES, Identifier.parse(updateFileID.getText()).asJSON());
		outputObject.add(FEE_PAYER_ACCOUNT_ID_PROPERTY, Identifier.parse(feePayerAccountField.getText()).asJSON());
		outputObject.add(NODE_ID_PROPERTIES, Identifier.parse(nodeAccountField.getText()).asJSON());
		outputObject.addProperty(CHUNK_SIZE_PROPERTIES, Integer.parseInt(chunkSizeTextField.getText()));

		final var date = getDate(datePicker, hourField, minuteField, secondsField, ZoneId.of(timeZone.getID()));
		date.plusNanos(Integer.parseInt(nanosField.getText()));
		outputObject.add(FIRS_TRANSACTION_VALID_START_PROPERTY, date.asJSON());
		outputObject.addProperty(VALID_INCREMENT_PROPERTY, Integer.parseInt(intervalTextField.getText()));
		outputObject.addProperty(TRANSACTION_VALID_DURATION_PROPERTY, controller.getTxValidDuration());
		outputObject.addProperty(MEMO_PROPERTY, memoField.getText() == null ? "" : memoField.getText());
		outputObject.addProperty(TRANSACTION_FEE_PROPERTY, Long.parseLong(stripHBarFormat(transactionFee.getText())));

		final var jsonName = String.format("%s/%s", TEMP_DIRECTORY,
				contents.getName().replace(FilenameUtils.getExtension(contents.getName()), "json"));

		try {
			Files.deleteIfExists(Path.of(jsonName));
		} catch (IOException e) {
			throw new HederaClientException(e);
		}

		writeJsonObject(jsonName, outputObject);
		var jsonFile = new File(jsonName);
		assert jsonFile.exists();

		var toPack = new File[] { jsonFile, contents };

		final var destZipFile = new File(jsonName.replace(JSON_EXTENSION, ZIP_EXTENSION));
		final var destTxtFile = new File(jsonName.replace(JSON_EXTENSION, TXT_EXTENSION));

		ZipUtil.packEntries(toPack, destZipFile);
		var userComments = new UserComments.Builder()
				.withAuthor(controller.getUserName())
				.withComment(createCommentsTextArea.getText())
				.build();

		userComments.toFile(destTxtFile.getAbsolutePath());
		displayAndLogInformation("File update contents transaction created");

		List<File> files = new ArrayList<>();
		files.add(destZipFile);
		files.add(destTxtFile);
		moveToOutput(files, remoteLocation);

		for (var file : files) {
			try {
				Files.deleteIfExists(file.toPath());
				logger.info("{} has been deleted", file.getAbsolutePath());
			} catch (IOException e) {
				logger.error("{} cannot be deleted", file.getAbsolutePath());
			}
		}

		try {
			Files.deleteIfExists(Path.of(jsonName));
			logger.info("Json file deleted");
		} catch (IOException e) {
			logger.error("Json file could not be deleted");
		}

		controller.homePaneController.setForceUpdate(true);
		initializeCreatePane();
		selectTransactionType.setValue(SELECT_STRING);
	}

	/**
	 * Browse to the file contents
	 */
	@FXML
	private void browseToContentsFile() {
		contents = BrowserUtilities.browseFiles(controller.getLastTransactionsDirectory(), createAnchorPane);
		if (contents == null) {
			return;
		}
		controller.setLastBrowsedDirectory(contents);
		setContentsAction();
	}

	// endregion

	// region COMMON METHODS

	/**
	 * Action to be taken when the user chooses a transaction to create
	 */
	public void chooseTransactionTypeChangedAction() {
		transactionType = CreateTransactionType.get(selectTransactionType.getValue());

		makeBoxesInvisible();

		cleanAllCreateFields();
		cleanAllUpdateFields();
		cleanAllTransferFields();
		cleanAllSystemFields();
		cleanAllFileUpdateContentsFields();
		cleanAllFreezeFields();

		commentsVBox.setVisible(true);
		commonFieldsVBox.setVisible(true);
		createChoiceHBox.setVisible(true);

		if (transactionType != CreateTransactionType.SELECT) {
			createChoiceHBox.getChildren().clear();
			createChoiceHBox.getChildren().add(createTransactionMenuButton(transactionType));
		}

		switch (transactionType) {
			case SELECT:
				commentsVBox.setVisible(false);
				commonFieldsVBox.setVisible(false);
				createChoiceHBox.setVisible(false);
				break;
			case CREATE:
				createAccountVBox.setVisible(true);
				break;
			case UPDATE:
				accountIDToUpdateVBox.setVisible(true);
				updateAccountVBox.setVisible(true);
				break;
			case TRANSFER:
				transferCurrencyVBox.setVisible(true);
				break;
			case SYSTEM:
				systemDeleteUndeleteVBox.setVisible(true);
				break;
			case FILE_UPDATE:
				fileContentsUpdateVBox.setVisible(true);
				break;
			case FREEZE:
				freezeChoiceVBox.setVisible(true);
				freezeVBox.setVisible(true);
				break;
			case UNKNOWN:
			default:
				logger.info("Not Implemented");
		}
	}


	/**
	 * When the user presses the <b>CREATE</b> button, all data in the form is collected into a json object
	 *
	 * @return a json object with the all the information collected in the form
	 */
	private JsonObject buildJsonInput() {
		var input = new JsonObject();
		var transactionValidStart =
				getDate(datePicker, hourField, minuteField, secondsField, ZoneId.of(timeZone.getID())).plusNanos(
						Integer.parseInt(nanosField.getText()));

		// Common elements
		addCommonElements(input, transactionValidStart);

		// Crypto create account fields
		addCryptoCreateElements(input);

		// Crypto update account fields
		addCryptoUpdateElements(input);

		// Transfer fields
		addCryptoTransferElements(input);

		// System delete/un-delete fields
		addSystemElements(input);

		// Freeze fields
		if (addFreezeNetworkFields(input)) {
			return null;
		}
		return input;

	}

	private boolean addFreezeNetworkFields(JsonObject input) {
		final var freezeChoiceValue = FREEZE_AND_UPGRADE.equals(freezeTypeChoiceBox.getValue()) ?
				"freeze upgrade" :
				freezeTypeChoiceBox.getValue();

		final var freezeType = freezeChoiceValue.toUpperCase(Locale.ROOT).replace(" ", "_");

		if (!"select freeze type".equalsIgnoreCase(freezeChoiceValue)) {
			input.addProperty(FREEZE_TYPE_FIELD_NAME, freezeType);

			switch (FreezeType.valueOf(freezeType)) {
				case UNKNOWN_FREEZE_TYPE:
					logger.error("Unrecognized freeze type");
					return true;
				case FREEZE_ONLY:
					input.addProperty(FREEZE_START_TIME_FIELD_NAME,
							getDate(freezeDatePicker, freezeHourField, freezeMinuteField, freezeSecondsField,
									ZoneId.of(freezeTimeZone.getID())).asRFCString());
					break;
				case PREPARE_UPGRADE:
					input.add(FREEZE_FILE_ID_FIELD_NAME, Identifier.parse(freezeFileIDTextField.getText()).asJSON());
					input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, freezeFileHashTextField.getText());
					break;
				case FREEZE_UPGRADE:
				case TELEMETRY_UPGRADE:
					input.add(FREEZE_FILE_ID_FIELD_NAME, Identifier.parse(freezeFileIDTextField.getText()).asJSON());
					input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, freezeFileHashTextField.getText());
					input.addProperty(FREEZE_START_TIME_FIELD_NAME,
							getDate(freezeDatePicker, freezeHourField, freezeMinuteField, freezeSecondsField,
									ZoneId.of(freezeTimeZone.getID())).asRFCString());
					break;
				case FREEZE_ABORT:
					break;
				default:
					throw new IllegalStateException("Unexpected value: " + FreezeType.valueOf(freezeType));
			}

		}
		return false;
	}

	private void addSystemElements(JsonObject input) {
		// Entity ID
		if (!"".equals(entityID.getText())) {
			input.add(ENTITY_TO_DEL_UNDEL, Identifier.parse(entityID.getText()).asJSON());
		}

		// File/Contract
		input.addProperty(FILE_CONTRACT_SWITCH, systemTypeChoiceBox.getValue());

		// Delete/Un-delete
		input.addProperty(DEL_UNDEL_SWITCH, systemActionChoiceBox.getValue());

		// Expiration time
		if (datePickerSystem != null) {
			input.addProperty(EXPIRATION_DATE_TIME,
					getDate(datePickerSystem, hourFieldSystem, minuteFieldSystem, secondsFieldSystem,
							ZoneId.of(timeZoneSystem.getID())).asRFCString());
		}
	}

	private void addCryptoTransferElements(JsonObject input) {
		//Get transfers from tables
		if (!fromTransferTable.getItems().isEmpty() && !toTransferTable.getItems().isEmpty()) {
			List<AccountAmountStrings> transfers = new ArrayList<>();
			for (var a : fromTransferTable.getItems()) {
				transfers.add(a.negate());
			}
			transfers.addAll(toTransferTable.getItems());
			var jsonArray = new JsonArray();
			for (var a : transfers) {
				var accountAmountPair = new JsonObject();
				accountAmountPair.add(ACCOUNT, a.getAccountAsJSON());
				accountAmountPair.addProperty(AMOUNT, a.getAmountAsLong());
				jsonArray.add(accountAmountPair);
			}
			input.add(TRANSFERS, jsonArray);
		}
	}

	private void addCryptoUpdateElements(JsonObject input) {
		// Account ID
		if (!"".equals(updateAccountID.getText())) {
			input.add(ACCOUNT_TO_UPDATE, Identifier.parse(updateAccountID.getText()).asJSON());
		}

		// Auto renew
		if (!"".equals(updateAutoRenew.getText())) {
			input.addProperty(AUTO_RENEW_PERIOD_FIELD_NAME, Long.parseLong(updateAutoRenew.getText()));
		}

		// Receiver Sig Required
		input.addProperty(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, updateReceiverSignatureRequired.isSelected());
	}

	private void addCryptoCreateElements(JsonObject input) {
		// Balance
		if (!"".equals(createInitialBalance.getText())) {
			var balanceJson = new JsonObject();
			balanceJson.addProperty(H_BARS, 0);
			balanceJson.addProperty(TINY_BARS, string2Hbar(createInitialBalance.getText()).toTinybars());
			input.add(INITIAL_BALANCE_FIELD_NAME, balanceJson);
		}
		// Auto renew
		if (!"".equals(createAutoRenew.getText())) {
			input.addProperty(AUTO_RENEW_PERIOD_FIELD_NAME, Long.parseLong(createAutoRenew.getText()));
		}

		// Key
		if (!newKeyJSON.isJsonNull() && newKeyJSON.size() != 0) {
			input.add(NEW_KEY_FIELD_NAME, newKeyJSON);
		}

		// Receiver Sig Required
		input.addProperty(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, createSignatureRequired.isSelected());
	}

	private void addCommonElements(JsonObject input, Timestamp transactionValidStart) {
		// Transaction valid start
		input.add(TRANSACTION_VALID_START_FIELD_NAME, transactionValidStart.asJSON());

		// memo field
		if (!"".equals(memoField.getText())) {
			input.addProperty(MEMO_FIELD_NAME, memoField.getText());
		}

		// Fee payer
		var feePayerID = Identifier.parse(feePayerAccountField.getText());
		input.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerID.asJSON());

		// Use default fee for transactions (note: Large binary files might override this)
		var feeJson = new JsonObject();
		var fee = Utilities.string2Hbar(transactionFee.getText());
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, fee.to(HbarUnit.TINYBAR));
		input.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		// Use default for transaction valid duration
		input.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, controller.getTxValidDuration());

		// Node ID
		input.add(NODE_ID_FIELD_NAME, Identifier.parse(nodeAccountField.getText()).asJSON());

		// Network
		input.addProperty(NETWORK_FIELD_NAME, controller.getCurrentNetwork());
	}

	/**
	 * Pairs the created transaction with the comments the user might have left
	 *
	 * @param tx
	 * 		the created transaction
	 * @return a pair with user comments and the transaction
	 */
	private Pair<UserComments, ToolTransaction> getUserCommentsTransactionPair(ToolTransaction tx) {

		var creatorComments = new UserComments.Builder().withAuthor(controller.getUserName()).withComment(
				createCommentsTextArea.getText()).build();

		logger.info(creatorComments);

		controller.displaySystemMessage(String.format("With comments: %s", creatorComments));

		switch (transactionType) {
			case CREATE:
				cleanAllCreateFields();
				break;
			case UPDATE:
				cleanAllUpdateFields();
				break;
			case TRANSFER:
				cleanAllTransferFields();
				break;
			case SYSTEM:
				cleanAllSystemFields();
				break;
			case FILE_UPDATE:
				cleanAllFileUpdateContentsFields();
				break;
			default:
				logger.error("Unknown transaction");
		}

		return new Pair<>(creatorComments, tx);
	}

	/**
	 * Checks the form for errors and incomplete fields
	 *
	 * @return true if there is enough data to create a transaction
	 */
	private boolean checkForm() {
		var flag = true;
		switch (transactionType) {
			case CREATE:
				flag = checkAndFlagCreateFields();
				break;
			case UPDATE:
				flag = checkAndFlagUpdateFields();
				break;
			case TRANSFER:
				flag = checkAndFlagTransferFields();
				break;
			case SYSTEM:
				flag = checkAndFlagSystemFields();
				break;
			case FILE_UPDATE:
				flag = checkAndFlagFileUpdateContentsFields();
				break;
			case UNKNOWN:
			case SELECT:
			default:
		}
		if (flag) {
			logger.info("Finish button enabled ({})", transactionType.getTypeString());
		}
		return flag;
	}

	private boolean checkAndFlagCommonFields() {
		var accounts = controller.getAccountsList();
		// check the date first
		var flag =
				isDateValid(hourField, minuteField, secondsField, freezeNanosField, datePicker,
						ZoneId.of(timeZone.getID()),
						timeZoneHBox);
		if (!flag) {
			invalidDate.setVisible(true);
		}

		// Check and flag the fee payer
		try {
			var feePayer = Identifier.parse(feePayerAccountField.getText());
			feePayerAccountField.setText(feePayer.toNicknameAndChecksum(accounts));
			invalidFeePayer.setVisible(false);
		} catch (Exception e) {
			invalidFeePayer.setVisible(true);
			displayAndLogInformation("Fee payer cannot be parsed");
			flag = false;
		}

		// Check and flag the node
		try {
			var node = Identifier.parse(nodeAccountField.getText());
			nodeAccountField.setText(node.toNicknameAndChecksum(accounts));
			invalidNode.setVisible(false);
		} catch (Exception e) {
			invalidNode.setVisible(true);
			displayAndLogInformation("Node ID cannot be parsed");
			flag = false;
		}
		return flag;
	}

	private void displayAndLogInformation(String message) {
		controller.displaySystemMessage("Error: " + message);
		logger.info(message);
	}

	private void displayAndLogInformation(Exception exception) {
		controller.displaySystemMessage("Error: " + exception.toString());
		logger.error(exception);
	}

	private void setupNewKeyObject() {
		var kk = new JsonObject();
		kk.add("keys", new JsonArray());
		newKeyJSON = new JsonObject();
		newKeyJSON.add("keyList", kk);
	}

	private MenuButton createTransactionMenuButton(CreateTransactionType type) {
		var menuButton = new MenuButton();
		setupOutputDirectoriesList();
		menuButton.setStyle(MENU_BUTTON_STYLE);
		menuButton.setText("CREATE AND EXPORT");
		menuButton.setMinWidth(300);


		menuButton.getItems().clear();
		// setup button text
		for (var s : outputDirectories) {
			var menuItem = new MenuItem(s.getPath().replace(System.getProperty("user.home") + File.separator, ""));
			logger.info("Adding menu-item: \"{}\"", menuItem.getText());
			var email = controller.getEmailFromMap(s.getPath());
			controller.setUserName(email);

			menuItem.setOnAction(actionEvent -> {
				if (doNotStoreExpiringTransaction()) {
					return;
				}
				storeToOutput(type, s);
			});
			menuButton.getItems().add(menuItem);
		}

		var menuItem = new MenuItem("browse for directory");
		menuItem.setOnAction(actionEvent -> {
			if (doNotStoreExpiringTransaction()) {
				return;
			}
			storeOutputToBrowsedOutput(type);
		});
		menuButton.getItems().add(menuItem);
		return menuButton;

	}

	/**
	 * Store a transaction and comments to one of the standard outputs
	 *
	 * @param type
	 * 		the transaction type
	 * @param fileService
	 * 		the file service that will be used to store the transaction
	 */
	private void storeToOutput(CreateTransactionType type, FileService fileService) {
		fromFile = false;
		if (fileService == null) {
			return;
		}
		try {
			var pair = getUserCommentsTransactionPair(type);
			if (pair == null) {
				return;
			}
			if (type.equals(CreateTransactionType.FILE_UPDATE)) {
				prepareZipAndComment(fileService);
				return;
			}
			storeTransactionAndComment(pair, fileService);
		} catch (HederaClientException e) {
			controller.displaySystemMessage(e);
		}

	}

	/**
	 * Browse to a folder and store the transaction and comment
	 *
	 * @param type
	 * 		the transaction type
	 */
	private void storeOutputToBrowsedOutput(CreateTransactionType type) {
		var s = BrowserUtilities.browseDirectories(controller.getLastTransactionsDirectory(), createAnchorPane);
		controller.setLastBrowsedDirectory(new File(s));
		FileService fileService = null;
		try {
			fileService = FileAdapterFactory.getAdapter(s);
		} catch (HederaClientException e) {
			controller.displaySystemMessage(e);
		}
		storeToOutput(type, fileService);
	}

	/**
	 * Browse to a file and load the transaction
	 *
	 * @return a Tool transaction
	 */
	private File loadTransaction() {
		logger.info("browsing transactions");
		var file = BrowserUtilities.browseFiles(controller.getLastTransactionsDirectory(), createAnchorPane,
				"Transaction", TRANSACTION_EXTENSION, SIGNED_TRANSACTION_EXTENSION, ZIP_EXTENSION);
		if (file == null) {
			return null;
		}
		controller.setLastBrowsedDirectory(file);
		return file;
	}

	private boolean doNotStoreExpiringTransaction() {
		var answer = true;
		if (datePicker.getValue() == null) {
			return false;
		}
		var date = getDate(datePicker, hourField, minuteField, secondsField, ZoneId.of(timeZone.getID()));
		var now = new Date();
		var secs = date.getSeconds() - now.getTime() / 1000;
		if (secs < 120) {
			answer = PopupMessage.display("Warning", String.format(REMAINING_TIME_MESSAGE,
					secs / 1000), true, "CONTINUE", "CANCEL");

		}
		return !answer;
	}

	private Pair<UserComments, ToolTransaction> getUserCommentsTransactionPair(
			CreateTransactionType type) throws HederaClientException {
		Pair<UserComments, ToolTransaction> pair = null;
		switch (type) {
			case TRANSFER:
				pair = createTransferTransactionAction();
				break;
			case UPDATE:
				pair = updateTransactionAction();
				break;
			case CREATE:
				pair = createAccountTransactionAction();
				break;
			case SYSTEM:
				pair = createSystemTransactionAction();
				break;
			case FREEZE:
				pair = createFreezeTransaction();
				break;
			case FILE_UPDATE:
			default:
				logger.error("Cannot recognize transaction type");
				controller.displaySystemMessage("Cannot recognize transaction type");
		}
		return pair;
	}

	private void setupCommonFieldsEvents() {
		memoField.lengthProperty().addListener((observable, oldValue, newValue) -> setTextSizeLimit(memoField,
				MEMO_LENGTH, oldValue, newValue));

		nodeAccountField.setText(controller.getDefaultNodeID());

		configureDateTime(datePicker, hourField, minuteField, secondsField, nanosField, createUTCTimeLabel,
				LocalDateTime.now(), timeZone, invalidDate);
		setupTimeZoneChooser(timeZone, timeZoneHBox, datePicker, hourField, minuteField, secondsField, nanosField,
				createUTCTimeLabel, invalidDate);
		configureDateTime(freezeDatePicker, freezeHourField, freezeMinuteField, freezeSecondsField, freezeNanosField,
				freezeUTCTimeLabel, LocalDateTime.now(), freezeTimeZone, freezeTimeErrorLabel);

		// endregion
		formatAccountTextField(nodeAccountField, invalidNode, feePayerAccountField);
		formatAccountTextField(feePayerAccountField, invalidFeePayer, feePayerAccountField.getParent());

		createCommentsTextArea.lengthProperty().addListener((observable, oldValue, newValue) -> {
			setTextSizeLimit(createCommentsTextArea, LIMIT, oldValue, newValue);
			if (newValue.intValue() > oldValue.intValue() && createCommentsTextArea.getText().length() >= LIMIT) {
				createCommentsTextArea.setText(createCommentsTextArea.getText().substring(0, LIMIT));
			}

			createCharsLeft.setText(
					String.format("Characters left: %d", LIMIT - createCommentsTextArea.getText().length()));
		});

		setNowValidStart.setOnAction(
				actionEvent -> {
					setNowTime(Instant.now().plusMillis(1));
					// Also set the expiration date for System Modify at the same time

					setTimeInForm(Instant.now().plusMillis(1), timeZoneSystem, hourFieldSystem, minuteFieldSystem,
							secondsFieldSystem,
							datePickerSystem);
				});

		createCharsLeft.setText(String.format("Characters left: %d", LIMIT));

		resetFormButton.visibleProperty().bind(commentsVBox.visibleProperty());
	}

	private void setTextSizeLimit(TextArea field, int endIndex, Number oldValue, Number newValue) {
		if (newValue.intValue() > oldValue.intValue() && field.getText().length() >= endIndex) {
			field.setText(memoField.getText().substring(0, endIndex));
		}
	}

	private void configureDateTime(DatePicker date, TextField hour, TextField minute, TextField seconds,
			TextField nanos, Label localTime, LocalDateTime today, TimeZone zone, Label errorLabel) {
		hour.textProperty().addListener((observable, oldValue, newValue) -> {
			fixTimeTextField(hour, newValue, "\\d*", REGEX);
			refreshLocalTime(date, hour, minute, seconds, nanos, localTime, zone, errorLabel);
		});

		minute.textProperty().addListener((observable, oldValue, newValue) -> {
			fixTimeTextField(minute, newValue, "\\d*", REGEX);
			refreshLocalTime(date, hour, minute, seconds, nanos, localTime, zone, errorLabel);
		});

		seconds.textProperty().addListener((observable, oldValue, newValue) -> {
			fixTimeTextField(seconds, newValue, "\\d*", REGEX);
			refreshLocalTime(date, hour, minute, seconds, nanos, localTime, zone, errorLabel);
		});

		nanos.textProperty().addListener((observable, oldValue, newValue) -> {
			fixTimeTextField(nanos, newValue, "\\d*", REGEX);
			refreshLocalTime(date, hour, minute, seconds, nanos, localTime, zone, errorLabel);
		});

		setupNumberField(hour, 23);
		setupNumberField(minute, 59);
		setupNumberField(seconds, 59);
		setupNumberField(nanos, 999999999);

		date.setDayCellFactory(picker -> new DateCell() {
			@Override
			public void updateItem(LocalDate date, boolean empty) {
				super.updateItem(date, empty);
				var localTime =
						LocalTime.of(Integer.parseInt(hour.getText()), Integer.parseInt(minute.getText()),
								Integer.parseInt(seconds.getText()));
				var dateTime = LocalDateTime.of(date, localTime);

				setDisable(empty || dateTime.compareTo(today) <= 0);
			}
		});

		date.valueProperty().addListener(
				(observable, oldDate, newDate) -> refreshLocalTime(date, hour, minute, seconds, nanos, localTime,
						zone, errorLabel));

		// region FOCUS EVENTS
		hour.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				logger.info("Hours text field changed to: {}", hour.getText());
				setLocalDateString(date, hour, minute, seconds, nanos, zone, localTime, errorLabel);
			}
		});
		minute.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				logger.info("Minute text field changed to: {}", minute.getText());
				setLocalDateString(date, hour, minute, seconds, nanos, zone, localTime, errorLabel);
			}
		});
		seconds.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				logger.info("Second text field changed to: {}", seconds.getText());
				setLocalDateString(date, hour, minute, seconds, nanos, zone, localTime, errorLabel);
			}
		});
		date.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (date.getValue() != null && Boolean.FALSE.equals(newPropertyValue)) {
				logger.info("Date changed to: {}", date.getValue());
				setLocalDateString(date, hour, minute, seconds, nanos, zone, localTime, errorLabel);
			}
		});
	}

	private void fixTimeTextField(TextField hour, String newValue, String s, String regex) {
		if (!newValue.matches(s)) {
			hour.setText(newValue.replaceAll(regex, ""));
		}
	}

	private void refreshLocalTime(DatePicker date, TextField hour, TextField minute, TextField seconds, TextField nanos,
			Label localTime, TimeZone timeZone, Label errorLabel) {
		final var hourText = hour.getText();
		final var minuteText = minute.getText();
		final var secondsText = seconds.getText();
		final var nanosText = nanos.getText();

		if ("".equals(hourText) || "".equals(minuteText) || "".equals(secondsText) || "".equals(
				nanosText) || Integer.parseInt(hourText) > 23 || Integer.parseInt(minuteText) > 59 || Integer.parseInt(
				secondsText) > 59 || Integer.parseInt(nanosText) > 100000000) {
			return;
		}

		final var dateValue = date.getValue();
		if (dateValue != null) {
			logger.info("Date changed to: {}", dateValue);
			setLocalDateString(date, hour, minute, seconds, nanos, timeZone, localTime, errorLabel);
			localTime.setVisible(true);
		} else {
			logger.info("Date cleared");
			localTime.setVisible(false);
			localTime.setText("");
		}
	}

	private boolean isDateValid(TextField hourField, TextField minuteField, TextField secondsField,
			TextField nanosField, DatePicker datePicker, ZoneId zoneId, HBox timeZoneHBox) {
		var flag = true;
		try {
			var zone = timeZoneHBox.getChildren().get(0);
			assert zone instanceof AutoCompleteNickname;
			var zoneString = ((AutoCompleteNickname) zone).getText();
			if (!ZoneId.getAvailableZoneIds().contains(zoneString)) {
				displayAndLogInformation("Invalid time zone");
				flag = false;
			}

			var hour = Integer.parseInt(hourField.getText());
			var minute = Integer.parseInt(minuteField.getText());
			var second = Integer.parseInt(secondsField.getText());
			var nanos = Long.parseLong(nanosField.getText());

			if (hour < 0 || hour > 23) {
				displayAndLogInformation("Invalid hours field");
				flag = false;
			}
			if (minute < 0 || minute > 59) {
				displayAndLogInformation("Invalid minutes field");
				flag = false;
			}
			if (second < 0 || second > 59) {
				displayAndLogInformation("Invalid seconds field");
				flag = false;
			}

			if (nanos < 0 || nanos > 99999999) {
				displayAndLogInformation("Invalid nanos field");
				flag = false;
			}

			var localDateTime =
					LocalDateTime.of(datePicker.getValue() != null ? datePicker.getValue() :
							LocalDate.now(), LocalTime.of(hour, minute, second));

			var transactionValidStart = Date.from(localDateTime.atZone(zoneId).toInstant().plusNanos(nanos));

			if (transactionValidStart.before(new Date())) {
				displayAndLogInformation("Transaction valid start in the past");
				flag = false;
			}

		} catch (Exception e) {
			flag = false;
		}
		return flag;
	}

	private void formatAccountTextField(TextField textField, Label errorLabel, Node nextNode) {
		textField.setOnKeyReleased((KeyEvent event) -> {
			textField.setStyle(TEXTFIELD_DEFAULT);
			errorLabel.setVisible(false);
			if (event.getCode() == KeyCode.ENTER) {
				nextNode.requestFocus();
			}
		});
		textField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				accountTFRemoveFocus(textField, errorLabel);
			}
		});
	}

	private void accountTFRemoveFocus(TextField textField, Label errorLabel) {
		var account = textField.getText();
		if ("".equals(account)) {
			return;
		}
		if (account.contains("(")) {
			account = account.substring(account.lastIndexOf("(") + 1, account.lastIndexOf(")"));
		}
		if (!Utilities.isNotLong(account)) {
			account = "0.0." + account;
		}

		var parsedAddress = parseAddress(account);
		switch (parsedAddress.getStatus()) {
			case BAD_FORMAT:
				textField.setStyle(TEXTFIELD_ERROR);
				errorLabel.setVisible(true);
				PopupMessage.display("Account format error",
						"The account format cannot be parsed. Acceptable formats are:\n" +
								" \u2022 XX (e.g. 12345),\n" +
								" \u2022 XX.XX.XX (e.g. 1.2.345), or\n" +
								" \u2022 XX.XX.XX-CCCCC (e.g. 1.2.345-abcde)");
				errorLabel.requestFocus();
				break;
			case BAD_CHECKSUM:
				textField.setStyle(TEXTFIELD_ERROR);
				errorLabel.setVisible(true);
				PopupMessage.display("Incorrect Checksum",
						String.format(
								"The checksum entered does not correspond to the account. Please check and try again" +
										".\nThe correct checkum is \"%s\".",
								parsedAddress.getCorrectChecksum()));
				errorLabel.requestFocus();
				break;
			case GOOD_NO_CHECKSUM:
			case GOOD_WITH_CHECKSUM:
				textField.setStyle(TEXTFIELD_DEFAULT);
				var id = Identifier.parse(account);
				textField.setText(id.toNicknameAndChecksum(controller.getAccountsList()));
				errorLabel.setVisible(false);
		}


	}

	private void formatHBarTextField(TextField textField) {
		textField.setOnKeyReleased((KeyEvent event) -> {
			if (event.getCode() == KeyCode.ENTER) {
				setHBarFormat(textField);
			}
		});

		textField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				setHBarFormat(textField);
			}
		});
	}

	private void storeTransactionAndComment(Pair<UserComments, ToolTransaction> pair,
			FileService remoteLocation) throws HederaClientException {
		var transaction = pair.getValue();
		var userComments = pair.getKey();

		var accountId = transaction.getFeePayerID();
		final var seconds = transaction.getTransactionValidStart().getEpochSecond();
		var filenames = String.format("%d-%s-%d", seconds, accountId.toReadableString().replace(".", "_"),
				transaction.hashCode());

		var tempStorage = TEMP_DIRECTORY + "tempStorage";
		if (new File(tempStorage).mkdirs()) {
			logger.info("Temporary folder created");
		}

		var i = 0;
		var txFile = new File(tempStorage + File.separator + filenames + ".tx");
		while (txFile.exists()) {
			txFile = new File(tempStorage + File.separator + filenames + i++ + ".tx");
		}

		try {
			transaction.store(txFile.getAbsolutePath());
			userComments.toFile(txFile.getAbsolutePath().replace(".tx", ".txt"));
		} catch (HederaClientException e) {
			logger.error(e);
			controller.displaySystemMessage(e);
		}

		var txtFile = new File(txFile.getAbsolutePath().replace(".tx", ".txt"));

		List<File> files = new ArrayList<>();
		files.add(txFile);
		files.add(txtFile);

		moveToOutput(files, remoteLocation);

		// remove all temporary files from local storage;
		try {
			if (txFile.exists()) {
				Files.delete(txFile.toPath());
				logger.info("File {} deleted", txFile.getName());
			}

			if (txtFile.exists()) {
				Files.delete(txtFile.toPath());
				logger.info("File {} deleted", txtFile.getName());
			}

			FileUtils.deleteDirectory(new File(tempStorage));
			assert !new File(tempStorage).exists();

		} catch (Exception e) {
			throw new HederaClientException("Error while deleting temporary files");
		}

		//reload the home pane, to show the transaction
		controller.homePaneController.initializeHomePane();
		initializeCreatePane();
		selectTransactionType.setValue(SELECT_STRING);
	}

	private void moveToOutput(List<File> files, FileService remoteLocation) {
		var remote = remoteLocation.getPath() + File.separator;
		for (var f : files) {
			try {
				var outputFolder = File.separator;
				if (controller.getOneDriveCredentials().containsKey(remoteLocation.getPath())) {
					var user = controller.getEmailFromMap(remoteLocation.getPath());
					outputFolder = "".equals(user) ? File.separator : "/OutputFiles/" + user + File.separator;
					logger.info("Exporting file: {}", f.getAbsolutePath());
				}
				FileUtils.moveFile(f, new File(remote + outputFolder + f.getName()));
			} catch (IOException e) {
				logger.error(e);
				controller.displaySystemMessage(e.getMessage());
				PopupMessage.display("Unable to upload",
						"Could not upload the file to the specified folder. Please check you have the appropriate " +
								"permissions",
						"OK");
			}
		}
	}

	private void setupKeyPane(TreeView<String> jsonObjectKey, ScrollPane scrollPane) {
		jsonObjectKey.prefHeightProperty().bind(scrollPane.heightProperty());
		jsonObjectKey.prefWidthProperty().bind(scrollPane.widthProperty());
		jsonObjectKey.setStyle("-fx-border-color: white");
		jsonObjectKey.setMinWidth(800);
		scrollPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.NEVER);
		scrollPane.setContent(jsonObjectKey);
		scrollPane.managedProperty().bind(scrollPane.visibleProperty());
	}

	private void cleanFields() {
		cleanCommonFields();
		createAutoRenew.setText(String.valueOf(controller.getAutoRenewPeriod()));
		updateAutoRenew.setText(String.valueOf(controller.getAutoRenewPeriod()));
		createSignatureRequired.setSelected(false);
		updateReceiverSignatureRequired.setSelected(false);
		createCommentsTextArea.clear();
		createInitialBalance.setText("0");
		setupNewKeyObject();

		createNewKey.setContent(new HBox());
		createNewKey.setVisible(false);

		updateNewKey.setContent(new HBox());
		updateNewKey.setVisible(false);
	}

	private void cleanCommonFields() {
		hourField.setText(String.format("%02d", controller.getDefaultHours()));
		minuteField.setText(String.format("%02d", controller.getDefaultMinutes()));
		secondsField.setText(String.format("%02d", controller.getDefaultSeconds()));
		nanosField.setText(NINE_ZEROS);
		datePicker.setValue(null);
		feePayerAccountField.clear();
		final var defaultNodeID =
				Identifier.parse(controller.getDefaultNodeID()).toNicknameAndChecksum(controller.getAccountsList());
		nodeAccountField.setText(defaultNodeID);
		transactionFee.setText(setCurrencyFormat(controller.getDefaultTxFee()));
		setupHbarNumberField(transactionFee);
		memoField.clear();
		createUTCTimeLabel.setText("");
	}

	private void setNowTime(Instant now) {
		setTimeInForm(now, timeZone, hourField, minuteField, secondsField, datePicker);
		this.nanosField.setText(String.format("%09d", now.atZone(ZoneId.of(timeZone.getID())).getNano()));
	}

	private void setTimeInForm(Instant start, TimeZone timeZone, TextField hourField,
			TextField minuteField, TextField secondsField, DatePicker datePicker) {
		final var zonedDateTimeFreezeStart = start.atZone(ZoneId.of(timeZone.getID()));
		hourField.setText(String.format("%02d", zonedDateTimeFreezeStart.getHour()));
		minuteField.setText(String.format("%02d", zonedDateTimeFreezeStart.getMinute()));
		secondsField.setText(String.format("%02d", zonedDateTimeFreezeStart.getSecond()));
		datePicker.setValue(zonedDateTimeFreezeStart.toLocalDate());
	}

	private void setLocalDateString(DatePicker datePicker, TextField hourField, TextField minuteField,
			TextField secondsField, TextField nanos, TimeZone timeZone, Label label, Label invalidDate) {

		if (datePicker.getValue() == null) {
			return;
		}

		var localDateTime = LocalDateTime.of(datePicker.getValue(),
				LocalTime.of(Integer.parseInt(hourField.getText()), Integer.parseInt(minuteField.getText()),
						Integer.parseInt(secondsField.getText())));
		var transactionValidStart = Date.from(localDateTime.atZone(ZoneId.of(timeZone.getID())).toInstant());

		final var beforeNow = transactionValidStart.toInstant().isBefore(Instant.now());
		label.setStyle("-fx-text-fill: " + (beforeNow ? "red" : "black"));
		invalidDate.setVisible(beforeNow);

		var dateTimeFormatter =
				DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.of("UTC"));

		label.setText(dateTimeFormatter.format(transactionValidStart.toInstant().plusNanos(
				Long.parseLong(nanos.getText()))) + " Coordinated Universal Time");
	}

	private Timestamp getDate(DatePicker dates, TextField hours, TextField minutes, TextField seconds, ZoneId zoneId) {
		var hour = Integer.parseInt(hours.getText());
		var minute = Integer.parseInt(minutes.getText());
		var second = Integer.parseInt(seconds.getText());
		var localDateTime = LocalDateTime.of(dates.getValue() != null ? dates.getValue() :
				LocalDate.now(), LocalTime.of(hour, minute, second));

		return new Timestamp(localDateTime.atZone(zoneId).toInstant());
	}

	private void setupNumberField(TextField timeField, int limit) {
		timeField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixTimeTextField(timeField, newValue, "\\d*", REGEX));
		timeField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				checkTimeField(timeField, limit);
			}
		});
		timeField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				logger.info("Time field changed to: {}", timeField.getText());
				checkTimeField(timeField, limit);
			}
		});
	}

	private void setupHbarNumberField(TextField currencyField) {
		currencyField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixTimeTextField(currencyField, newValue, "[^\\d.\\s]",
						"[^\\d.\\s]"));
		currencyField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				setHBarFormat(currencyField);
			}
		});
		currencyField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				setHBarFormat(currencyField);
			}
		});
	}

	private void setupIntNumberField(TextField intField, int limit) {
		intField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixTimeTextField(intField, newValue, "\\d*", REGEX));
		intField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				try {
					intField.setText(String.valueOf(Math.min(Integer.parseInt(intField.getText()), limit)));
				} catch (NumberFormatException e) {
					logger.error("Cannot parse field");
				}
			}
		});
	}

	private void checkTimeField(TextField timeField, int limit) {
		try {
			var k = Math.min(Integer.parseInt(timeField.getText()), limit);
			var format = (limit < 100) ? "%02d" : "%09d";
			timeField.setText(String.format(format, k));
		} catch (NumberFormatException e) {
			logger.error("Cannot parse field");
		}
	}

	private void setupManagedProperty(Node... nodes) {
		for (var n : nodes) {
			n.managedProperty().bind(n.visibleProperty());
		}
	}

	private void processKey(JsonObject key, ScrollPane keyPane) {
		final var emptyKey = new JsonObject();
		if (!key.equals(emptyKey)) {
			newKeyJSON = key;
			invalidCreateNewKey.setVisible(false);
			invalidUpdateNewKey.setVisible(false);
		}

		if (!key.equals(emptyKey) && !key.toString().equals("{\"keyList\":{\"keys\":[]}}")) {
			setKeyTreeInBox(key, keyPane);
		}
	}

	private static void setHBarFormat(TextField currencyTextField) {
		long hBarsLong = Utilities.string2Hbar(currencyTextField.getText()).toTinybars();
		logger.debug("Currency text field changed to: {}", currencyTextField.getText());
		var hBarsString = Utilities.setHBarFormat(hBarsLong);
		currencyTextField.setText(hBarsString.substring(0, hBarsString.length() - 1));
	}

	private void setContentsAction() {
		if (contents.exists() && contents.isFile()) {
			contentsLink.setText(contents.getName());
			shaLabel.setText(
					String.format("SHA-384 Checksum: %s", EncryptionUtils.getChecksum(contents.getAbsolutePath())));
			contentsTextField.setVisible(false);
			contentsLink.setVisible(true);
			shaLabel.setVisible(true);
		} else {
			contentsFilePathError.setVisible(true);
			contents = null;
		}
	}

	public void loadFormFromTransaction() {
		File transactionFile = null;
		cleanFields();
		if (SetupPhase.NORMAL_OPERATION_PHASE.equals(controller.getSetupPhase())) {
			transactionFile = loadTransaction();
			fromFile = true;
		}
		if (SetupPhase.TEST_PHASE.equals(controller.getSetupPhase()) && !"".equals(loadTransactionTextField.getText())) {
			transactionFile = new File(loadTransactionTextField.getText());
		}
		if (transactionFile == null) {
			return;
		}
		if (ZIP_EXTENSION.equals(FilenameUtils.getExtension(transactionFile.getName()))) {
			try {
				loadLargeFileUpdateToForm(transactionFile);
			} catch (HederaClientException e) {
				PopupMessage.display("Error loading file", "Cannot load information from zip file");
			}
			return;
		}
		if (!TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(transactionFile.getName()))) {
			return;
		}
		ToolTransaction transaction;
		try {
			transaction = new ToolTransaction().parseFile(transactionFile);
		} catch (HederaClientException e) {
			logger.error(e.getMessage());
			PopupMessage.display("Invalid transaction", "The file selected cannot be loaded.");
			return;
		}

		switch (transaction.getTransactionType()) {
			case CRYPTO_TRANSFER:
				selectTransactionType.setValue("Transfer");
				loadCryptoTransferToForm((ToolTransferTransaction) transaction);
				break;
			case CRYPTO_CREATE:
				selectTransactionType.setValue("Account Creation");
				loadCryptoCreateToForm((ToolCryptoCreateTransaction) transaction);
				break;
			case CRYPTO_UPDATE:
				selectTransactionType.setValue("Account Update");
				loadCryptoUpdateToForm((ToolCryptoUpdateTransaction) transaction);
				break;
			case SYSTEM_DELETE_UNDELETE:
				selectTransactionType.setValue("Admin Modify Content");
				loadSystemTransactionToForm((ToolSystemTransaction) transaction);
				break;
			case FREEZE:
				selectTransactionType.setValue("Network Freeze and Update");
				loadFreezeTransactionToForm((ToolFreezeTransaction) transaction);
				break;
			default:
				PopupMessage.display("Unsupported transaction", "The transaction is not yet supported by the tool.");
				break;
		}
		loadCommonTransactionFields(transaction);
		checkForm();
	}

	private void loadCommonTransactionFields(ToolTransaction transaction) {
		setNowTime(transaction.getTransactionValidStart());
		transactionFee.setText(Utilities.setCurrencyFormat(transaction.getTransactionFee().toTinybars()));
		nodeAccountField.setText(transaction.getNodeID().toNicknameAndChecksum(controller.getAccountsList()));
		feePayerAccountField.setText(transaction.getFeePayerID().toNicknameAndChecksum(controller.getAccountsList()));
		memoField.setText(transaction.getMemo());
	}

	private void loadLargeFileUpdateToForm(File transactionFile) throws HederaClientException {
		var tempStorage = TEMP_DIRECTORY + "tempStorage";
		selectTransactionType.setValue("File Contents Update");
		unZip(transactionFile.getAbsolutePath(), tempStorage);
		var files = new File(tempStorage).listFiles(
				(dir, name) -> JSON_EXTENSION.equals(FilenameUtils.getExtension(name)));
		if (files == null) {
			throw new HederaClientException("Error reading files");
		}
		if (files.length != 1) {
			throw new HederaClientException("Incorrect number of json files");
		}
		var details = readJsonObject(files[0].getPath());
		try {
			FileUtils.deleteDirectory(new File(tempStorage));
		} catch (IOException e) {
			throw new HederaClientException("Cannot delete directory", e.getCause());
		}

		if (details.has(FILE_ID_PROPERTIES)) {
			updateFileID.setText(
					Identifier.parse(details.get(FILE_ID_PROPERTIES).getAsJsonObject()).toNicknameAndChecksum(
							controller.getAccountsList()));
		}
		if (details.has(FEE_PAYER_ACCOUNT_ID_PROPERTY)) {
			feePayerAccountField.setText(
					Identifier.parse(details.get(FEE_PAYER_ACCOUNT_ID_PROPERTY).getAsJsonObject()).toNicknameAndChecksum(
							controller.getAccountsList()));
		}
		if (details.has(NODE_ID_PROPERTIES)) {
			nodeAccountField.setText(
					Identifier.parse(details.get(NODE_ID_PROPERTIES).getAsJsonObject()).toNicknameAndChecksum(
							controller.getAccountsList()));
		}
		if (details.has(CHUNK_SIZE_PROPERTIES)) {
			chunkSizeTextField.setText(String.valueOf(details.get(CHUNK_SIZE_PROPERTIES).getAsInt()));
		}
		if (details.has(FIRS_TRANSACTION_VALID_START_PROPERTY)) {
			setNowTime(new Timestamp(details.get(FIRS_TRANSACTION_VALID_START_PROPERTY).getAsJsonObject()).asInstant());
		}
		if (details.has(VALID_INCREMENT_PROPERTY)) {
			intervalTextField.setText(String.valueOf(details.get(VALID_INCREMENT_PROPERTY).getAsInt()));
		}
		if (details.has(MEMO_PROPERTY)) {
			memoField.setText(details.get(MEMO_PROPERTY).getAsString());
		}
		if (details.has(TRANSACTION_FEE_PROPERTY)) {
			transactionFee.setText(Utilities.setCurrencyFormat(details.get(TRANSACTION_FEE_PROPERTY).getAsLong()));
		}
	}

	private void loadCryptoTransferToForm(ToolTransferTransaction transaction) {
		cleanAllTransferFields();
		var transfers = transaction.getAccountAmountMap();
		for (Map.Entry<Identifier, Hbar> entry : transfers.entrySet()) {
			var newTransaction =
					new AccountAmountStrings(entry.getKey().toNicknameAndChecksum(controller.getAccountsList()),
							String.valueOf(Math.abs(entry.getValue().toTinybars())));
			var table = entry.getValue().toTinybars() > 0 ? toTransferTable : fromTransferTable;
			table.getItems().add(newTransaction);
		}
		transferCurrencyVBox.setVisible(true);
	}

	private void loadCryptoCreateToForm(ToolCryptoCreateTransaction transaction) {
		cleanAllCreateFields();
		createInitialBalance.setText(Utilities.setCurrencyFormat(transaction.getInitialBalance().toTinybars()));
		createAutoRenew.setText(String.valueOf(transaction.getAutoRenewDuration().getSeconds()));
		updateReceiverSignatureRequired.setSelected(transaction.isReceiverSignatureRequired());
		createNewKey.setVisible(true);
		processKey(EncryptionUtils.keyToJson(transaction.getKey()), createNewKey);
	}

	private void loadCryptoUpdateToForm(ToolCryptoUpdateTransaction transaction) {
		cleanAllUpdateFields();
		updateAccountID.setText(transaction.getAccount().toNicknameAndChecksum(controller.getAccountsList()));
		findAccountInfoAndPreloadFields();
		final var autoRenewDuration = transaction.getAutoRenewDuration();
		if (autoRenewDuration != null) {
			updateAutoRenew.setText(String.valueOf(autoRenewDuration.getSeconds()));
		}
		final var receiverSignatureRequired = transaction.isReceiverSignatureRequired();
		if (receiverSignatureRequired != null) {
			updateReceiverSignatureRequired.setSelected(receiverSignatureRequired);
		}
		final var key = transaction.getKey();
		if (key != null) {
			updateNewKey.setVisible(true);
			processKey(EncryptionUtils.keyToJson(key), updateNewKey);
		}
	}

	private void loadSystemTransactionToForm(ToolSystemTransaction transaction) {
		cleanAllSystemFields();
		if (transaction.isDelete()) {
			setTimeInForm(transaction.getExpiration(), timeZoneSystem, hourFieldSystem, minuteFieldSystem,
					secondsFieldSystem, datePickerSystem);
		}
		entityID.setText(transaction.getEntity().toNicknameAndChecksum(controller.getAccountsList()));
		if (Boolean.TRUE.equals(transaction.isDelete())) {
			systemActionChoiceBox.getSelectionModel().select(0);
		}
		if (Boolean.FALSE.equals(transaction.isDelete())) {
			systemActionChoiceBox.getSelectionModel().select(1);
		}

		if (Boolean.TRUE.equals(transaction.isFile())) {
			systemTypeChoiceBox.getSelectionModel().select(0);
		}
		if (Boolean.FALSE.equals(transaction.isFile())) {
			systemTypeChoiceBox.getSelectionModel().select(1);
		}
	}

	private void loadFreezeTransactionToForm(ToolFreezeTransaction transaction) {
		cleanAllFreezeFields();
		final var freezeType = transaction.getFreezeType();
		if (freezeType.equals(FreezeType.FREEZE_UPGRADE)) {
			freezeTypeChoiceBox.setValue(FREEZE_AND_UPGRADE);
		} else {
			freezeTypeChoiceBox.setValue(freezeType.toString());
		}

		switch (freezeType) {
			case UNKNOWN_FREEZE_TYPE:
				PopupMessage.display("Cannot load transaction",
						"The app could not parse the transaction's freeze type. Please check you are loading the " +
								"correct file");
				return;
			case FREEZE_ONLY:
				// Freezes the network at the specified time. The start_time field must be provided and must
				// reference a future time. Any values specified for the update_file and file_hash fields will
				// be ignored. This transaction does not perform any network changes or upgrades and requires
				// manual intervention to restart the network.
				setTimeInForm(transaction.getStartTime().asInstant(), freezeTimeZone, freezeHourField,
						freezeMinuteField, freezeSecondsField, freezeDatePicker);
				break;
			case PREPARE_UPGRADE:
				// A non-freezing operation that initiates network wide preparation in advance of a scheduled
				// freeze upgrade. The update_file and file_hash fields must be provided and valid. The
				// start_time field may be omitted and any value present will be ignored.
				freezeFileIDTextField.setText(
						transaction.getFileID().toNicknameAndChecksum(controller.getAccountsList()));
				freezeFileHashTextField.setText(transaction.getFileHash());
				break;
			case FREEZE_UPGRADE:
				// Performs an immediate upgrade on auxilary services and containers providing
				// telemetry/metrics. Does not impact network operations.
			case TELEMETRY_UPGRADE:
				// Freezes the network at the specified time and performs the previously prepared automatic
				// upgrade across the entire network.
				setTimeInForm(transaction.getStartTime().asInstant(), freezeTimeZone, freezeHourField,
						freezeMinuteField, freezeSecondsField, freezeDatePicker);
				freezeFileIDTextField.setText(
						transaction.getFileID().toNicknameAndChecksum(controller.getAccountsList()));
				freezeFileHashTextField.setText(transaction.getFileHash());
				break;
			case FREEZE_ABORT:
				// Aborts a pending network freeze operation.
				break;

			default:
				throw new IllegalStateException("Unexpected value: " + freezeType);
		}
	}

	public void cleanForm() {
		fromFile = false;
		var type = selectTransactionType.getValue();
		initializeCreatePane();
		selectTransactionType.setValue(type);
	}

	public void loadFormFromTransactionTest(KeyEvent keyEvent) {
		if (keyEvent.getCode().equals(KeyCode.ENTER)) {
			loadFormFromTransaction();
		} else {
			logger.info("here");
		}
	}


	//endregion
}
