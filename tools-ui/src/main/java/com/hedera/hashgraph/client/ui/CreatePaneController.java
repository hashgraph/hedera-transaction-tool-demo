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
import com.hedera.hashgraph.client.core.constants.JsonConstants;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.fileservices.FileAdapterFactory;
import com.hedera.hashgraph.client.core.interfaces.FileService;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.LargeBinaryFile;
import com.hedera.hashgraph.client.core.remote.TransactionFile;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.UserComments;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolFileAppendTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolFileUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolFreezeTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolSystemTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.popups.ExtraKeysSelectorPopup;
import com.hedera.hashgraph.client.ui.popups.KeyDesignerPopup;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.popups.ProgressPopup;
import com.hedera.hashgraph.client.ui.popups.TransactionPopup;
import com.hedera.hashgraph.client.ui.utilities.AccountAmountStrings;
import com.hedera.hashgraph.client.ui.utilities.AutoCompleteNickname;
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import com.hedera.hashgraph.client.ui.utilities.TimeFieldSet;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.FreezeType;
import com.hedera.hashgraph.sdk.HbarUnit;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.fxml.FXML;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ProgressBar;
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
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.util.Pair;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;
import org.controlsfx.control.ToggleSwitch;
import org.jetbrains.annotations.NotNull;
import org.zeroturnaround.zip.ZipUtil;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNT_PARSED;
import static com.hedera.hashgraph.client.core.constants.Constants.CHUNK_SIZE_PROPERTIES;
import static com.hedera.hashgraph.client.core.constants.Constants.CONTENT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_RECEIPTS;
import static com.hedera.hashgraph.client.core.constants.Constants.FEE_PAYER_ACCOUNT_ID_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.FILENAME_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.FILE_ID_PROPERTIES;
import static com.hedera.hashgraph.client.core.constants.Constants.FIRST_TRANSACTION_VALID_START_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.FIXED_CELL_SIZE;
import static com.hedera.hashgraph.client.core.constants.Constants.FREEZE_AND_UPGRADE;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.LARGE_BINARY_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.LIMIT;
import static com.hedera.hashgraph.client.core.constants.Constants.MAX_MEMO_BYTES;
import static com.hedera.hashgraph.client.core.constants.Constants.MAX_TOKEN_AUTOMATIC_ASSOCIATIONS;
import static com.hedera.hashgraph.client.core.constants.Constants.MEMO_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MEMO_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.MENU_BUTTON_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.NINE_ZEROS;
import static com.hedera.hashgraph.client.core.constants.Constants.NODE_ID_PROPERTIES;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.RECEIPT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.REGEX;
import static com.hedera.hashgraph.client.core.constants.Constants.REMAINING_TIME_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.SELECT_FREEZE_TYPE;
import static com.hedera.hashgraph.client.core.constants.Constants.SELECT_STRING;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.START_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.TEMP_DIRECTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.TEXTFIELD_DEFAULT;
import static com.hedera.hashgraph.client.core.constants.Constants.TEXTFIELD_ERROR;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_FEE_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_VALID_DURATION_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.VALID_INCREMENT_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.ZIP_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_TO_UPDATE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AMOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AUTO_RENEW_PERIOD_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DECLINE_STAKING_REWARDS_FIELD_NAME;
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
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MAX_TOKEN_ASSOCIATIONS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NEW_KEY_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.STAKED_ACCOUNT_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.STAKED_NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSFERS;
import static com.hedera.hashgraph.client.core.constants.Messages.TRANSACTION_CREATED_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.NOW_TOOLTIP_TEXT;
import static com.hedera.hashgraph.client.core.security.AddressChecksums.parseAddress;
import static com.hedera.hashgraph.client.core.security.AddressChecksums.parseStatus;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.showTooltip;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.splitString;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.splitStringDigest;
import static com.hedera.hashgraph.client.ui.AccountsPaneController.CANCEL_LABEL;
import static com.hedera.hashgraph.client.ui.popups.SigningKeysPopup.display;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.RED_BORDER_STYLE;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.isNotLong;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.setCurrencyFormat;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.string2Hbar;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.stripHBarFormat;
import static java.lang.Thread.sleep;

public class CreatePaneController implements GenericFileReadWriteAware {

	// private fields
	private static final Logger logger = LogManager.getLogger(CreatePaneController.class);
	public static final String STATUS = "Status";
	public static final String TRANSACTION_FAILED_ERROR_MESSAGE =
			"Transaction failed with error %s. Please review the transaction and try again.";

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
	public Button signAndSubmitButton;

	public GridPane storeOrSubmitGridPane;

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
	public HBox systemSlidersHBox;
	public HBox copyFromAccountHBox;
	public HBox updateCopyFromAccountHBox;
	public HBox timeZoneHBox;
	public HBox timeZoneSystemHBox;
	public HBox freezeTimeZoneHBox;
	public HBox storeTransactionHBox;

	public TextArea memoField;
	public TextField feePayerAccountField;
	public TextField stakedAccountIdField;
	public TextField stakedNodeIdField;
	public TextField stakedAccountIdOriginal;
	public TextField stakedAccountIdNew;
	public TextField stakedNodeIdOriginal;
	public TextField stakedNodeIdNew;
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
	public TextField createAccountMemo;
	public TextField createMaxTokenAssociations;
	public TextField updateAccountMemoOriginal;
	public TextField updateAccountMemoNew;
	public TextField updateMaxTokensOriginal;
	public TextField updateMaxTokensNew;

	public TableView<AccountAmountStrings> fromTransferTable;
	public TableView<AccountAmountStrings> toTransferTable;

	public TextArea createCommentsTextArea;

	public DatePicker datePicker;
	public DatePicker datePickerSystem;
	public DatePicker freezeDatePicker;

	// Labels
	public Label totalTransferLabel;
	public Label updateBytesRemaining;
	public Label createMemoByteCount;
	public Label createCharsLeft;
	public Label updateRSRLabel;
	public Label updateRSROriginal;
	public Label newValueLabel;
	public Label createRSRLabel;
	public Label declineStakingRewardsLabel;
	public Label declineStakingRewardsOriginal;
	public Label declineStakingRewardsLabelUpdate;
	public Label createUTCTimeLabel;
	public Label entityLabel;
	public Label expirationLabel;
	public Label systemCreateLocalTimeLabel;
	public HBox shaTextFlow;
	public Text fileDigest;
	public Label freezeUTCTimeLabel;

	// Error messages
	public Label invalidTransferList;
	public Label invalidTransferTotal;
	public Label invalidUpdateAccountToUpdate;
	public Label invalidUpdatedAutoRenew;
	public Label invalidUpdateNewKey;
	public Label invalidDate;
	public Label invalidFeePayer;
	public Label invalidStakedAccountId;
	public Label invalidStakedNodeId;
	public Label invalidStakedAccountIdUpdate;
	public Label invalidStakedNodeIdUpdate;
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
	public Label invalidFreezeFile;
	public Label invalidTransactionFee;
	public Label invalidFreezeFileHash;
	public Label createMTAErrorLabel;
	public Label updateMTAerrorLabel;

	// Keys scroll panes
	public ScrollPane updateOriginalKey;
	public ScrollPane updateNewKey;
	public ScrollPane createNewKey;

	// Switches
	public ToggleSwitch updateReceiverSignatureRequired;
	public ToggleSwitch createSignatureRequired;
	public ToggleSwitch declineStakingRewards;
	public ToggleSwitch declineStakingRewardsNew;

	public ChoiceBox<String> systemActionChoiceBox;
	public ChoiceBox<String> systemTypeChoiceBox;
	public ChoiceBox<String> freezeTypeChoiceBox;

	public Hyperlink contentsLink;

	private boolean fromFile = false;
	private boolean noise = false;

	private TimeFieldSet startFieldsSet;
	private TimeFieldSet systemFieldsSet;
	private TimeFieldSet freezeFieldsSet;

	// endregion

	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	void initializeCreatePane() {
		setupOutputDirectoriesList();

		loadAccountNicknames();

		// region INITIALIZE FIELDS
		makeBoxesInvisible();

		setupCommonFieldsEvents();

		setupManagedProperty(commentsVBox, commonFieldsVBox, createAccountVBox, updateAccountVBox, transferCurrencyVBox,
				invalidTransferTotal, invalidTransferList, createNewKey, accountIDToUpdateVBox, storeOrSubmitGridPane,
				systemDeleteUndeleteVBox, systemSlidersHBox, systemExpirationVBox, freezeVBox, freezeFileVBox,
				freezeChoiceVBox, contentsTextField, contentsLink, fileContentsUpdateVBox, fileIDToUpdateVBox,
				freezeStartVBox, shaTextFlow, contentsFilePathError, invalidUpdateNewKey, resetFormButton,
				freezeUTCTimeLabel, freezeTimeErrorLabel, invalidDate, createUTCTimeLabel, systemCreateLocalTimeLabel,
				invalidFreezeFileHash, updateMTAerrorLabel);

		setupTextFieldResizeProperty(feePayerAccountField, nodeAccountField, entityID, updateFileID,
				transferToAccountIDTextField, transferFromAccountIDTextField, updateAccountID, freezeFileIDTextField,
				freezeFileHashTextField, stakedAccountIdField, stakedNodeIdField
		);

		setupTransferFields();

		setupCreateFields();

		setupUpdateFields();

		setupSystemFields();

		setupFileContentsFields();

		setupFreezeFields();

		setupTooltips();

		setupSelectTransaction();

	}

	private void setupSelectTransaction() {
		noise = true;
		selectTransactionType.getItems().clear();
		selectTransactionType.setItems(CreateTransactionType.names());
		selectTransactionType.setValue(SELECT_STRING);
		noise = false;
	}

	private void makeBoxesInvisible() {
		commentsVBox.setVisible(false);
		commonFieldsVBox.setVisible(false);
		createAccountVBox.setVisible(false);
		updateAccountVBox.setVisible(false);
		transferCurrencyVBox.setVisible(false);
		accountIDToUpdateVBox.setVisible(false);
		storeOrSubmitGridPane.setVisible(false);
		systemDeleteUndeleteVBox.setVisible(false);
		fileContentsUpdateVBox.setVisible(false);
		freezeVBox.setVisible(false);
		freezeChoiceVBox.setVisible(false);
	}

	private void setupTransferFields() {
		transferTableEvents(transferFromAccountIDTextField, transferFromAmountTextField, fromTransferTable,
				acceptFromAccountButton, errorInvalidFromAccount);
		transferTableEvents(transferToAccountIDTextField, transferToAmountTextField, toTransferTable,
				acceptToAccountButton, errorInvalidToAccount);

		transferFromAmountTextField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixNumericTextField(transferFromAmountTextField, newValue, "\\d*",
						"[^\\d.]"));
		transferToAmountTextField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixNumericTextField(transferToAmountTextField, newValue, "\\d*",
						"[^\\d.]"));

		final BooleanProperty transferBoolean = new SimpleBooleanProperty();
		transferBoolean.setValue(toTransferTable.getItems().isEmpty() ^ fromTransferTable.getItems().isEmpty());

		invalidTransferList.visibleProperty().bind(transferBoolean);
	}

	private void setupCreateFields() {
		newKeyJSON = emptyKeyObject();
		formatHBarTextField(createInitialBalance);
		loadAccountNicknames();
		final var autoCompleteNickname = new AutoCompleteNickname(accountNickNames);
		autoCompleteNickname.setVisible(false);
		autoCompleteNickname.managedProperty().bind(autoCompleteNickname.visibleProperty());
		copyFromAccountHBox.getChildren().clear();
		copyFromAccountHBox.getChildren().add(autoCompleteNickname);
		createSignatureRequired.selectedProperty().addListener(
				(observableValue, aBoolean, t1) -> createRSRLabel.setText(Boolean.TRUE.equals(t1) ? "true" : "false"
				));
		declineStakingRewards.selectedProperty().addListener(
				(observableValue, aBoolean, t1) -> declineStakingRewardsLabel.setText(Boolean.TRUE.equals(t1) ? "true" : "false"
				));

		createAccountMemo.textProperty().addListener(
				(observableValue, s, t1) -> setMemoByteCounter(createAccountMemo, createMemoByteCount));
		createKeyButton.setOnAction(e -> {
			autoCompleteNickname.clear();
			autoCompleteNickname.setVisible(false);

			var key = newKeyJSON;
			final var keyDesignerPopup = !newKeyJSON.equals(new JsonObject()) ?
					new KeyDesignerPopup(getStringPublicKeyMap(), EncryptionUtils.jsonToKey(newKeyJSON)) :
					new KeyDesignerPopup(getStringPublicKeyMap());
			key = keyDesignerPopup.display();
			createNewKey.setVisible(true);
			processKey(key, createNewKey);
		});

		setupTokenAssociationsFields(createMTAErrorLabel, createMaxTokenAssociations);

		autoCompleteNickname.setOnKeyReleased(
				keyEvent -> getKeyFromNickname(autoCompleteNickname, keyEvent.getCode(), createNewKey));

		formatAccountTextField(stakedAccountIdField, invalidStakedAccountId, stakedAccountIdField.getParent());
		numericFieldListen(stakedNodeIdField);
	}

	private void setupUpdateFields() {
		setupKeyPane(new TreeView<>(), updateNewKey);

		updateAutoRenew.textProperty().addListener(
				(observable, oldValue, newValue) -> fixNumericTextField(updateAutoRenew, newValue, "\\d*",
						REGEX));

		updateReceiverSignatureRequired.selectedProperty().addListener(
				(observableValue, aBoolean, t1) -> updateRSRLabel.setText(Boolean.TRUE.equals(t1) ? "true" : "false"));
		declineStakingRewardsNew.selectedProperty().addListener(
				(observableValue, aBoolean, t1) -> declineStakingRewardsLabelUpdate.setText(Boolean.TRUE.equals(t1) ? "true" : "false"));
		loadAccountNicknames();
		final var updateFromNickName = new AutoCompleteNickname(accountNickNames);
		updateFromNickName.setVisible(false);
		updateFromNickName.managedProperty().bind(updateFromNickName.visibleProperty());
		updateCopyFromAccountHBox.getChildren().clear();
		updateCopyFromAccountHBox.getChildren().add(updateFromNickName);
		formatAccountTextField(updateAccountID, invalidUpdateAccountToUpdate, updateAccountID.getParent());
		formatAccountTextField(stakedAccountIdNew, invalidStakedAccountIdUpdate, stakedAccountIdNew.getParent());
		numericFieldListen(stakedNodeIdNew);

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
			final var keyDesignerPopup =
					new KeyDesignerPopup(getStringPublicKeyMap(), EncryptionUtils.jsonToKey(newKeyJSON));
			var key = keyDesignerPopup.display();
			if (key == null || key.equals(new JsonObject())) {
				key = originalKey;
			}
			final var keyTree = controller.buildKeyTreeView(key);
			keyTree.setStyle("-fx-border-color: white; -fx-background-color: white");
			keyTree.setMinWidth(800);
			updateNewKey.setContent(keyTree);
			keyTree.prefWidthProperty().bind(updateNewKey.widthProperty());
			newKeyJSON = key;
		});

		updateAccountMemoNew.textProperty().addListener(
				(observableValue, s, t1) -> setMemoByteCounter(updateAccountMemoNew, updateBytesRemaining));

		updateFromNickName.setOnKeyReleased(
				keyEvent -> getKeyFromNickname(updateFromNickName, keyEvent.getCode(), updateNewKey));

		setupTokenAssociationsFields(updateMTAerrorLabel, updateMaxTokensNew);
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
				org.apache.commons.io.FileUtils.copyFile(contents, destFile);
			} catch (final IOException e) {
				logger.error(e);
				controller.displaySystemMessage(e);
				return;
			}
			final var r = Runtime.getRuntime();
			final var command = String.format("open -e %s", destFile.getAbsolutePath());
			try {
				r.exec(command);
			} catch (final IOException e) {
				logger.error(e);
				controller.displaySystemMessage(e);
			}
		});

		setupIntNumberField(chunkSizeTextField, 1025);
		setupIntNumberField(intervalTextField, Integer.MAX_VALUE);

	}

	private void setupSystemFields() {
		systemFieldsSet = new TimeFieldSet(datePickerSystem, hourFieldSystem, minuteFieldSystem, secondsFieldSystem,
				new TextField(), timeZoneSystemHBox, systemCreateLocalTimeLabel, invalidExpirationDate);
		systemFieldsSet.configureDateTime(startFieldsSet.getLocalDateTime());

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


		systemActionChoiceBox.getItems().clear();
		systemActionChoiceBox.getItems().addAll("Remove Content", "Restore Content");
		systemActionChoiceBox.getSelectionModel().select(0);
		systemActionChoiceBox.getSelectionModel().selectedIndexProperty().addListener(
				(observableValue, number, t1) -> systemExpirationVBox.setVisible(t1.intValue() == 0));

		formatAccountTextField(entityID, invalidEntity, entityID.getParent());

	}

	private void setupFreezeFields() {
		freezeFieldsSet = new TimeFieldSet(freezeDatePicker, freezeHourField, freezeMinuteField, freezeSecondsField,
				freezeNanosField, freezeTimeZoneHBox, freezeUTCTimeLabel, freezeTimeErrorLabel);
		freezeFieldsSet.configureDateTime(startFieldsSet.getLocalDateTime());

		freezeChoiceVBox.setVisible(false);
		freezeFileVBox.setVisible(false);
		freezeStartVBox.setVisible(false);

		formatAccountTextField(freezeFileIDTextField, invalidFreezeFile, freezeFileHashTextField);

		noise = true;
		final var freezeValues = Arrays.asList(FreezeType.values());
		final var freezeValuesAsStrings =
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
			cleanCommonFields();
			cleanFreezeExclusiveFields();
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
			final var type = FreezeType.valueOf(t1.replace(" ", "_").toUpperCase(Locale.ROOT));
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
		freezeFileHashTextField.focusedProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.FALSE.equals(t1)) {
				isValidHash();
			}
		});
		freezeFileHashTextField.setOnKeyReleased(event -> {
			if (KeyCode.ENTER.equals(event.getCode()) || KeyCode.TAB.equals(event.getCode())) {
				freezeFileHashTextField.getParent().requestFocus();
			}
		});
	}

	private void setupTooltips() {
		// All tooltips will be added here
		nowTimeToolTip.setOnAction(
				actionEvent -> showTooltip(controller.settingsPane, nowTimeToolTip,
						NOW_TOOLTIP_TEXT));

	}

	private void setupOutputDirectoriesList() {
		try {
			if (controller.getOneDriveCredentials() != null) {
				final var inputs = controller.getOneDriveCredentials().keySet();
				outputDirectories = new ArrayList<>();
				for (final var s :
						inputs) {
					final var fs = FileAdapterFactory.getAdapter(s);
					if (fs != null && fs.exists()) {
						outputDirectories.add(fs);
					}
				}
			}
		} catch (final HederaClientException e) {
			logger.error(e);
			controller.displaySystemMessage(e);
		}
	}

	// region CREATE ACCOUNT

	private Pair<UserComments, ToolTransaction> createAccountTransactionAction() {
		if (!checkAndFlagCreateFields()) {
			return null;
		}

		final var input = buildJsonInput();

		try {
			final var tx = new ToolCryptoCreateTransaction(input);
			displayAndLogInformation("Create Account " + TRANSACTION_CREATED_MESSAGE);
			return getUserCommentsTransactionPair(tx);
		} catch (final HederaClientException e) {
			logger.error(e);
			return null;
		}

	}

	private void cleanAllCreateFields() {
		cleanCommonFields();
		createAutoRenew.setText(String.valueOf(controller.getAutoRenewPeriod()));
		createSignatureRequired.setSelected(false);
		declineStakingRewards.setSelected(false);
		createInitialBalance.setText("0");
		stakedAccountIdField.setText("");
		stakedNodeIdField.setText("");
		newKeyJSON = emptyKeyObject();
		createNewKey.setContent(new HBox());
		createNewKey.setVisible(false);
		clearErrorMessages(invalidCreateAutoRenew, invalidDate, invalidFeePayer, invalidCreateNewKey, invalidNode,
				invalidStakedAccountId, invalidStakedNodeId);
	}

	private void clearErrorMessages(final Label... errorMessages) {
		for (final var errorMessage : errorMessages) {
			errorMessage.setVisible(false);
		}
	}

	private boolean checkAndFlagCreateFields() {
		var flag = checkAndFlagCommonFields();

		if (newKeyJSON == null || new KeyList().equals(EncryptionUtils.jsonToKey(newKeyJSON))) {
			invalidCreateNewKey.setVisible(true);
			flag = false;
		}

		if ((stakedAccountIdField.getText() != null) && (!stakedAccountIdField.getText().isEmpty())) {
			try {
				final var accountId = Identifier.parse(stakedAccountIdField.getText(), controller.getCurrentNetwork());
				stakedAccountIdField.setText(accountId.toNicknameAndChecksum(controller.getAccountsList()));
				invalidStakedAccountId.setVisible(false);
			} catch (final Exception e) {
				invalidStakedAccountId.setVisible(true);
				displayAndLogInformation("Staked Account ID cannot be parsed");
				flag = false;
			}
		} else {
			invalidStakedAccountId.setVisible(false);
		}

		if ((stakedNodeIdField.getText() != null) && (!stakedNodeIdField.getText().isEmpty())) {

			if ((stakedAccountIdField.getText() != null) && (!stakedAccountIdField.getText().isEmpty())) {
				invalidStakedNodeId.setVisible(true);
				invalidStakedNodeId.setText("Staked Node ID cannot be set when Staked Account ID is set");
				displayAndLogInformation("Staked Node ID cannot be set when Staked Account ID is set");
				flag = false;
			} else {
				invalidStakedNodeId.setText("Invalid node ID");
				try {
					// validate node ID? I don't see a way to do that with sdk
					invalidStakedNodeId.setVisible(false);
				} catch (final Exception e) {
					invalidStakedNodeId.setVisible(true);
					displayAndLogInformation("Staked Node ID cannot be parsed");
					flag = false;
				}
			}
		} else {
			invalidStakedNodeId.setVisible(false);
		}

		if (flag) {
			clearErrorMessages(invalidCreateAutoRenew, invalidDate, invalidFeePayer, invalidCreateNewKey, invalidNode, invalidStakedAccountId);

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
			final var input = buildJsonInput();
			final var tx = new ToolCryptoUpdateTransaction(input);
			displayAndLogInformation("Update Account " + TRANSACTION_CREATED_MESSAGE);
			return getUserCommentsTransactionPair(tx);
		} catch (final HederaClientException e) {
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
				final var account = Identifier.parse(updateAccountID.getText(), controller.getCurrentNetwork());
				updateAccountID.setText(account.toNicknameAndChecksum(controller.getAccountsList()));
			}
		} catch (final Exception e) {
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


		if ((stakedAccountIdNew.getText() != null) && (!stakedAccountIdNew.getText().isEmpty())) {
			try {
				final var accountId = Identifier.parse(stakedAccountIdNew.getText(), controller.getCurrentNetwork());
				stakedAccountIdNew.setText(accountId.toNicknameAndChecksum(controller.getAccountsList()));
				invalidStakedAccountIdUpdate.setVisible(false);
			} catch (final Exception e) {
				invalidStakedAccountIdUpdate.setVisible(true);
				displayAndLogInformation("Staked Account ID cannot be parsed");
				flag = false;
			}
		} else {
			invalidStakedAccountIdUpdate.setVisible(false);
		}

		if ((stakedNodeIdNew.getText() != null) && (!stakedNodeIdNew.getText().isEmpty())) {

			if ((stakedAccountIdNew.getText() != null) && (!stakedAccountIdNew.getText().isEmpty())) {
				invalidStakedNodeIdUpdate.setVisible(true);
				invalidStakedNodeIdUpdate.setText("Staked Node ID cannot be set when Staked Account ID is set");
				displayAndLogInformation("Staked Node ID cannot be set when Staked Account ID is set");
				flag = false;
			} else {
				invalidStakedNodeIdUpdate.setText("Invalid node ID");
				try {
					// validate node ID? I don't see a way to do that with sdk
					invalidStakedNodeIdUpdate.setVisible(false);
				} catch (final Exception e) {
					invalidStakedNodeIdUpdate.setVisible(true);
					displayAndLogInformation("Staked Node ID cannot be parsed");
					flag = false;
				}
			}
		} else {
			invalidStakedNodeIdUpdate.setVisible(false);
		}

		return flag;
	}

	private void cleanAllUpdateFields() {
		cleanCommonFields();
		updateAccountID.clear();
		updateAutoRenew.setText(String.valueOf(controller.getAutoRenewPeriod()));
		updateReceiverSignatureRequired.setSelected(false);
		updateARPOriginal.clear();
		updateRSROriginal.setText("???");
		declineStakingRewardsOriginal.setText("unknown");
		stakedAccountIdOriginal.setPromptText("unknown");
		stakedNodeIdOriginal.setPromptText("unknown");
		updateOriginalKey.setContent(new HBox());
		updateNewKey.setContent(new HBox());
		updateNewKey.setVisible(false);

		clearErrorMessages(invalidUpdatedAutoRenew, invalidDate, invalidFeePayer, invalidUpdateNewKey, invalidNode,
				invalidUpdateAccountToUpdate, invalidStakedAccountId, invalidStakedNodeId);

	}

	private void findAccountInfoAndPreloadFields() {
		final var accountsInfoMap = controller.getAccountInfoMap();
		try {
			final var account = Identifier.parse(updateAccountID.getText(), controller.getCurrentNetwork());
			if (!accountsInfoMap.containsKey(account)) {
				PopupMessage.display("Missing account information",
						String.format(
								"In order to display data regarding account %s, please download the information from " +
										"the network.",
								account.toReadableStringAndChecksum()));
				return;
			}
			final var accountInfo = accountsInfoMap.get(account);
			updateARPOriginal.setText(String.format("%d s", accountInfo.autoRenewPeriod.getSeconds()));
			updateRSROriginal.setText(String.valueOf(accountInfo.isReceiverSignatureRequired));
			controller.loadPubKeys();
			final var jsonObjectKey = EncryptionUtils.keyToJson(accountInfo.key);
			originalKey = EncryptionUtils.keyToJson(accountInfo.key);
			final var oldKeyTreeView = controller.buildKeyTreeView(jsonObjectKey);
			setupKeyPane(oldKeyTreeView, updateOriginalKey);
			updateAccountMemoOriginal.setText(accountInfo.accountMemo);
			updateMaxTokensOriginal.setText(String.valueOf(accountInfo.maxAutomaticTokenAssociations));
			if (accountInfo.stakingInfo != null) {
				if (accountInfo.stakingInfo.stakedAccountId != null) {
					final var stakedAccountId = new Identifier(accountInfo.stakingInfo.stakedAccountId);
					stakedAccountId.setNetworkName(controller.getCurrentNetwork());
					stakedAccountIdOriginal.setText(stakedAccountId.toNicknameAndChecksum(controller.getAccountsList()));
				} else {
					stakedAccountIdOriginal.setText("");
					stakedAccountIdOriginal.setPromptText("unset");
				}
				if (accountInfo.stakingInfo.stakedNodeId != null) {
					stakedNodeIdOriginal.setText(accountInfo.stakingInfo.stakedNodeId.toString());
				} else {
					stakedNodeIdOriginal.setText("");
					stakedNodeIdOriginal.setPromptText("unset");
				}
				declineStakingRewardsOriginal.setText(String.valueOf(accountInfo.stakingInfo.declineStakingReward));
			} else {
				stakedAccountIdOriginal.setText("");
				stakedNodeIdOriginal.setText("");
				declineStakingRewardsOriginal.setText("unset");
				stakedAccountIdOriginal.setPromptText("unset");
				stakedNodeIdOriginal.setPromptText("unset");
			}
			if (!fromFile) {
				updateReceiverSignatureRequired.setSelected(accountInfo.isReceiverSignatureRequired);
				updateAutoRenew.setText(String.format("%d", accountInfo.autoRenewPeriod.getSeconds()));
				newKeyJSON = EncryptionUtils.keyToJson(accountInfo.key);
				updateAccountMemoOriginal.setText("");
				updateMaxTokensOriginal.setText("0");

				final var newKeyTreeView = controller.buildKeyTreeView(jsonObjectKey);
				setupKeyPane(newKeyTreeView, updateNewKey);
			}
			// in case they were visible before
			clearErrorMessages(invalidUpdatedAutoRenew, invalidDate, invalidFeePayer, invalidUpdateNewKey,
					invalidNode, invalidUpdateAccountToUpdate, invalidStakedAccountIdUpdate, invalidStakedNodeIdUpdate);

		} catch (final Exception e) {
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
		final var input = buildJsonInput();
		final var tx = new ToolTransferTransaction(input);
		displayAndLogInformation("Transfer " + TRANSACTION_CREATED_MESSAGE);
		return getUserCommentsTransactionPair(tx);
	}

	private void updateTotalAmount() {
		final var credits = toTransferTable.getItems();
		final var debits = fromTransferTable.getItems();

		long total = 0;
		for (final var a : credits) {
			total += a.getAmountAsLong();
		}
		for (final var a : debits) {
			total -= a.getAmountAsLong();
		}

		final var sign = total < 0 ? "-" : "";

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
		toTransferTable.getItems().clear();
		fromTransferTable.getItems().clear();
		transferCurrencyVBox.setVisible(false);
		clearErrorMessages(invalidDate, invalidFeePayer, invalidNode);
		initializeTable(fromTransferTable);
		initializeTable(toTransferTable);
	}

	private void initializeTable(final TableView<AccountAmountStrings> table) {
		final ObservableList<AccountAmountStrings> data = FXCollections.observableArrayList();
		table.setFixedCellSize(FIXED_CELL_SIZE);
		table.setStyle("-fx-font-size: " + FIXED_CELL_SIZE / 2);

		table.setEditable(true);

		table.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

		final var accountColumn = new TableColumn<AccountAmountStrings, String>("Account ID");
		accountColumn.setCellValueFactory(new PropertyValueFactory<>("accountID"));

		final var amountColumn = new TableColumn<AccountAmountStrings, String>("Amount");
		amountColumn.setCellValueFactory(new PropertyValueFactory<>("amount"));

		table.setItems(data);
		table.getColumns().clear();
		table.getColumns().addAll(accountColumn, amountColumn);

		accountColumn.prefWidthProperty().bind(table.widthProperty().multiply(0.6));
		amountColumn.prefWidthProperty().bind(table.widthProperty().multiply(0.395));

		accountColumn.setResizable(false);
		amountColumn.setResizable(false);

		final var contextMenu = new ContextMenu();
		final var menuItem = new MenuItem("delete");

		contextMenu.getItems().addAll(menuItem);

		table.addEventHandler(MouseEvent.MOUSE_CLICKED, mouseEvent -> {
			if (mouseEvent.getButton() == MouseButton.SECONDARY) {
				contextMenu.show(table, mouseEvent.getScreenX(), mouseEvent.getScreenY());
			}
		});

		menuItem.setOnAction(event -> {
			if (!table.getItems().isEmpty()) {
				final var selectedItem = table.getSelectionModel().getSelectedItem();
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

	private void addAccountAmountToTable(final TextField account, final TextField amount,
			final TableView<AccountAmountStrings> thisTable) {
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

		final AccountAmountStrings newTransaction;
		newTransaction = new AccountAmountStrings(account.getText(), stripHBarFormat(amount.getText()));

		final var status = parseAddress(NetworkEnum.asLedger(controller.getCurrentNetwork()).toBytes(),
				newTransaction.getStrippedAccountID()).getStatus();
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

		transferCurrencyVBox.setAlignment(Pos.CENTER);
		updateTotalAmount();
		account.clear();
		amount.clear();
		account.requestFocus();
	}

	private void transferTableEvents(final TextField accountIDTextField, final TextField amountTextField,
			final TableView<AccountAmountStrings> table, final Button acceptButton, final Label errorLabel) {

		table.prefWidthProperty().bind(transferCurrencyVBox.widthProperty().multiply(2).divide(3));
		table.prefHeightProperty().bind(fromTransferTable.heightProperty().multiply(.4));


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
		systemFieldsSet.reset(controller.getDefaultHours(), controller.getDefaultMinutes(),
				controller.getDefaultSeconds());
		entityID.clear();
	}

	private boolean checkSystemFields() {

		if (!checkAndFlagCommonFields()) {
			return false;
		}

		try {
			final var account = Identifier.parse(entityID.getText(), controller.getCurrentNetwork()).toReadableString();
			logger.info(ACCOUNT_PARSED, account);
		} catch (final Exception e) {
			return false;
		}

		if (systemActionChoiceBox.getSelectionModel().getSelectedItem().contains(
				"Remove") && freezeDatePicker.getValue() != null) {
			final var validExpiration =
					isDateValid(hourFieldSystem, minuteFieldSystem, secondsFieldSystem, new TextField(
									NINE_ZEROS),
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

		final var freezeType = FREEZE_AND_UPGRADE.equals(choice) ?
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
					final var file =
							Identifier.parse(freezeFileIDTextField.getText(),
									controller.getCurrentNetwork()).toReadableString();
					logger.info(ACCOUNT_PARSED, file);
				} catch (final Exception e) {
					validFile = false;
				}
				validHash = isValidHash();
				break;
			case FREEZE_UPGRADE:
			case TELEMETRY_UPGRADE:
				validStart =
						isDateValid(freezeHourField, freezeMinuteField, freezeSecondsField, freezeNanosField,
								datePicker, ZoneId.of(freezeTimeZone.getID()), freezeTimeZoneHBox);
				freezeTimeErrorLabel.setVisible(!validStart);
				try {
					final var file =
							Identifier.parse(freezeFileIDTextField.getText(),
									controller.getCurrentNetwork()).toReadableString();
					logger.info(ACCOUNT_PARSED, file);
				} catch (final Exception e) {
					validFile = false;
				}
				validHash = isValidHash();
				break;
			case FREEZE_ABORT:
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + freezeType);
		}
		return validStart && validFile && validHash;
	}

	/**
	 * Check if the hash has a valid value
	 *
	 * @return true if the hash is valid
	 */
	private boolean isValidHash() {
		boolean validHash;
		validHash = !"".equals(freezeFileHashTextField.getText());
		try {
			Hex.decode(freezeFileHashTextField.getText());
		} catch (final Exception e) {
			logger.error("Hash is invalid");
			validHash = false;
		}
		invalidFreezeFileHash.setVisible(!validHash);
		return validHash;
	}

	private boolean checkAndFlagSystemFields() {
		final var flag = checkAndFlagCommonFields();
		return flag && checkSystemFields();
	}

	private boolean checkAndFlagFreezeFields() {
		final var flag = checkAndFlagCommonFields();
		final var freezeFlag = checkFreezeFields();
		return flag && freezeFlag;
	}

	private Pair<UserComments, ToolTransaction> createSystemTransactionAction() {
		if (!checkAndFlagSystemFields()) {
			return null;
		}

		final var input = buildJsonInput();

		try {
			final var tx = new ToolSystemTransaction(input);
			displayAndLogInformation("System transaction created");
			return getUserCommentsTransactionPair(tx);
		} catch (final HederaClientException e) {
			controller.displaySystemMessage(e);
			logger.error(e);
			return null;
		}

	}

	private Pair<UserComments, ToolTransaction> createFreezeTransaction() {
		if (!checkAndFlagFreezeFields()) {
			return null;
		}

		final var input = buildJsonInput();

		try {
			final var tx = new ToolFreezeTransaction(input);
			displayAndLogInformation("Freeze transaction created");
			return getUserCommentsTransactionPair(tx);
		} catch (final HederaClientException e) {
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
		intervalTextField.setText("1000000000");
		chunkSizeTextField.setText("1024");
		fileDigest.setText("");
		shaTextFlow.setVisible(false);
		contentsFilePathError.setVisible(false);
		contents = null;
	}


	private void cleanAllFreezeFields() {
		cleanCommonFields();
		freezeTypeChoiceBox.getSelectionModel().select(SELECT_FREEZE_TYPE);
		cleanFreezeExclusiveFields();
	}

	private void cleanFreezeExclusiveFields() {
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
			final var account = Identifier.parse(updateFileID.getText(), controller.getCurrentNetwork());
			updateFileID.setText(account.toReadableString());
		} catch (final Exception e) {
			displayAndLogInformation(e);
			invalidUpdateFileToUpdate.setVisible(true);
			flag = false;
		}

		// check chunk size field
		try {
			final var chunk = Integer.parseInt(chunkSizeTextField.getText());
			if (chunk <= 0 || chunk > 4095) {
				invalidChunkSizeLabel.setVisible(true);
				flag = false;
			}
		} catch (final NumberFormatException e) {
			displayAndLogInformation(e);
			invalidChunkSizeLabel.setVisible(true);
			flag = false;
		}

		// Check interval field
		try {
			final var interval = Integer.parseInt(intervalTextField.getText());
			if (interval <= 0) {
				invalidIntervalLabel.setVisible(true);
				flag = false;
			}
		} catch (final NumberFormatException e) {
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
		} catch (final Exception e) {
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
	private void prepareZipAndComment(final FileService remoteLocation) throws HederaClientException {
		if (!checkForm()) {
			return;
		}
		final var jsonName = String.format("%s/%s", TEMP_DIRECTORY,
				contents.getName().replace(FilenameUtils.getExtension(contents.getName()), "json"));

		final var jsonPath = Path.of(jsonName);
		try {
			Files.deleteIfExists(jsonPath);
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}

		final var lfuFile = createLargeFileUpdateFiles();

		final List<File> files = new ArrayList<>();
		files.add(new File(lfuFile));
		files.add(new File(lfuFile.replace(LARGE_BINARY_EXTENSION, TXT_EXTENSION)));
		moveToOutput(files, remoteLocation);

		for (final var file : files) {
			try {
				Files.deleteIfExists(file.toPath());
				logger.info("{} has been deleted", file.getAbsolutePath());
			} catch (final IOException e) {
				logger.error("{} cannot be deleted", file.getAbsolutePath());
			}
		}

		try {
			Files.deleteIfExists(jsonPath);
			logger.info("Json file deleted");
		} catch (final IOException e) {
			logger.error("Json file could not be deleted");
		}

		controller.homePaneController.setForceUpdate(true);
		initializeCreatePane();
		selectTransactionType.setValue(SELECT_STRING);
	}

	private String createLargeFileUpdateFiles() throws HederaClientException {
		final var outputObject = getFileUpdateJson();
		final var payer = Identifier.parse(outputObject.get("feePayerAccountId").getAsJsonObject()).toReadableString();
		final var time = new Timestamp(outputObject.get("firsTransactionValidStart").getAsJsonObject());

		final var name = contents.getName();
		final var extension = FilenameUtils.getExtension(name);
		final var location = String.format("%s/%s", TEMP_DIRECTORY, extension.equals("") ?
				name + "." + JSON_EXTENSION :
				name.replace(extension, JSON_EXTENSION));

		writeJsonObject(location, outputObject);
		final var jsonFile = new File(location);
		final var toPack = new File[] { jsonFile, contents };

		final var destZipFile = new File(
				String.format("%s/%s_%s_%s.%s", TEMP_DIRECTORY, payer.replace(".", "_"), time.getSeconds(),
						time.getNanos(), LARGE_BINARY_EXTENSION));
		final var destTxtFile = new File(destZipFile.getAbsolutePath().replace(LARGE_BINARY_EXTENSION, TXT_EXTENSION));

		try {
			Files.deleteIfExists(destZipFile.toPath());
			ZipUtil.packEntries(toPack, destZipFile);
		} catch (final Exception e) {
			throw new HederaClientException(e);
		}

		final var userComments = new UserComments.Builder()
				.withAuthor(controller.getUserName())
				.withComment(createCommentsTextArea.getText())
				.build();

		userComments.toFile(destTxtFile.getAbsolutePath());
		displayAndLogInformation("File update contents transaction created");
		return destZipFile.getAbsolutePath();
	}

	@NotNull
	private JsonObject getFileUpdateJson() {
		// setup json file
		final var outputObject = new JsonObject();
		outputObject.addProperty(FILENAME_PROPERTY, contents.getName());
		outputObject.add(FILE_ID_PROPERTIES,
				Identifier.parse(updateFileID.getText(), controller.getCurrentNetwork()).asJSON());
		outputObject.add(FEE_PAYER_ACCOUNT_ID_PROPERTY,
				Identifier.parse(feePayerAccountField.getText(), controller.getCurrentNetwork()).asJSON());
		outputObject.add(NODE_ID_PROPERTIES,
				Identifier.parse(nodeAccountField.getText(), controller.getCurrentNetwork()).asJSON());
		outputObject.addProperty(CHUNK_SIZE_PROPERTIES, Integer.parseInt(chunkSizeTextField.getText()));

		final var date = startFieldsSet.getDate();
		outputObject.add(FIRST_TRANSACTION_VALID_START_PROPERTY, date.asJSON());
		outputObject.addProperty(VALID_INCREMENT_PROPERTY, Integer.parseInt(intervalTextField.getText()));
		outputObject.addProperty(TRANSACTION_VALID_DURATION_PROPERTY, controller.getTxValidDuration());
		outputObject.addProperty(MEMO_PROPERTY, memoField.getText() == null ? "" : memoField.getText());
		outputObject.addProperty(TRANSACTION_FEE_PROPERTY,
				Long.parseLong(stripHBarFormat(transactionFee.getText())));
		return outputObject;
	}


	// endregion

	// region LOAD FROM FILE

	/**
	 * Browse to the file contents
	 */
	@FXML
	private void browseToContentsFile() {
		contents = BrowserUtilities.browseFiles(controller.getLastTransactionsDirectory(), createAnchorPane, "Content",
				CONTENT_EXTENSION);
		if (contents == null) {
			return;
		}
		controller.setLastBrowsedDirectory(contents);
		setContentsAction();
	}

	private void setContentsAction() {
		if (contents.exists() && contents.isFile()) {
			contentsLink.setText(contents.getName());
			final var digest = EncryptionUtils.getChecksum(contents.getAbsolutePath());
			fileDigest.setText(splitStringDigest(splitString(digest), 6));
			fileDigest.setFont(Font.font("Courier New", 18));
			contentsTextField.setVisible(false);
			contentsLink.setVisible(true);
			shaTextFlow.setVisible(true);
		} else {
			contentsFilePathError.setVisible(true);
			contents = null;
		}
	}

	public void loadFormFromTransaction() {
		File transactionFile = null;
		cleanForm();
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
		if (LARGE_BINARY_EXTENSION.equals(FilenameUtils.getExtension(transactionFile.getName()))) {
			try {
				loadLargeFileUpdateToForm(transactionFile);
			} catch (final HederaClientException e) {
				PopupMessage.display("Error loading file", "Cannot load information from zip file");
			}
			return;
		}
		if (!TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(transactionFile.getName()))) {
			return;
		}
		final ToolTransaction transaction;
		try {
			transaction = new ToolTransaction().parseFile(transactionFile);
		} catch (final HederaClientException e) {
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

	private void loadCommonTransactionFields(final ToolTransaction transaction) {
		setNowTime(transaction.getTransactionValidStart());
		transactionFee.setText(Utilities.setCurrencyFormat(transaction.getTransactionFee().toTinybars()));
		final var nodeID = transaction.getNodeID();
		nodeID.setNetworkName(controller.getCurrentNetwork());
		nodeAccountField.setText(nodeID.toNicknameAndChecksum(controller.getAccountsList()));
		final var feePayerID = transaction.getFeePayerID();
		feePayerID.setNetworkName(controller.getCurrentNetwork());
		feePayerAccountField.setText(feePayerID.toNicknameAndChecksum(controller.getAccountsList()));
		memoField.setText(transaction.getMemo());
	}

	private void loadCryptoTransferToForm(final ToolTransferTransaction transaction) {
		cleanAllTransferFields();
		final var transfers = transaction.getAccountAmountMap();
		for (final var entry : transfers.entrySet()) {
			final AccountAmountStrings newTransaction;
			final var key = entry.getKey();
			key.setNetworkName(controller.getCurrentNetwork());
			final var value = entry.getValue();
			newTransaction =
					new AccountAmountStrings(key.toNicknameAndChecksum(controller.getAccountsList()),
							String.valueOf(Math.abs(value.toTinybars())));
			final var table = value.toTinybars() > 0 ? toTransferTable : fromTransferTable;
			table.getItems().add(newTransaction);
		}
		transferCurrencyVBox.setVisible(true);
	}

	private void loadCryptoCreateToForm(final ToolCryptoCreateTransaction transaction) {
		cleanAllCreateFields();
		createInitialBalance.setText(Utilities.setCurrencyFormat(transaction.getInitialBalance().toTinybars()));
		createAutoRenew.setText(String.valueOf(transaction.getAutoRenewDuration().getSeconds()));
		updateReceiverSignatureRequired.setSelected(transaction.isReceiverSignatureRequired());
		createNewKey.setVisible(true);
		processKey(EncryptionUtils.keyToJson(transaction.getKey()), createNewKey);
		createMaxTokenAssociations.setText(String.valueOf(transaction.getMaxTokenAssociations()));
		createAccountMemo.setText(transaction.getAccountMemo());

		final var stakedAccountId = transaction.getStakedAccountId();
		if (stakedAccountId == null) {
			stakedAccountIdField.setText("");
		} else {
			stakedAccountId.setNetworkName(controller.getCurrentNetwork());
			stakedAccountIdField.setText(stakedAccountId.toNicknameAndChecksum(controller.getAccountsList()));
		}

		final var stakedNodeId = transaction.getStakedNodeId();
		if (stakedNodeId == null) {
			stakedNodeIdField.setText("");
		} else {
			stakedNodeIdField.setText(stakedNodeId.toString());
		}

		declineStakingRewards.setSelected(transaction.isDeclineStakingRewards());
		declineStakingRewardsLabel.setText(String.valueOf(transaction.isDeclineStakingRewards()));
	}

	private void loadCryptoUpdateToForm(final ToolCryptoUpdateTransaction transaction) {
		cleanAllUpdateFields();
		final var account = transaction.getAccount();
		account.setNetworkName(controller.getCurrentNetwork());
		updateAccountID.setText(account.toNicknameAndChecksum(controller.getAccountsList()));
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
		final var accountMemo = transaction.getAccountMemo();
		if (accountMemo != null) {
			updateAccountMemoNew.setText(accountMemo);
		}
		final var tokenAssociations = transaction.getMaxTokenAssociations();
		if (tokenAssociations != null) {
			updateMaxTokensNew.setText(String.valueOf(tokenAssociations));
		}

		final var stakedAccountId = transaction.getStakedAccountId();
		if (stakedAccountId == null) {
			stakedAccountIdNew.setText("");
		} else {
			stakedAccountId.setNetworkName(controller.getCurrentNetwork());
			stakedAccountIdNew.setText(stakedAccountId.toNicknameAndChecksum(controller.getAccountsList()));
		}

		final var stakedNodeId = transaction.getStakedNodeId();
		if (stakedNodeId == null) {
			stakedNodeIdNew.setText("");
		} else {
			stakedNodeIdNew.setText(stakedNodeId.toString());
		}

		if (transaction.isDeclineStakingRewards() == null) {
			declineStakingRewardsNew.setSelected(false);
			declineStakingRewardsLabelUpdate.setText(String.valueOf(false));
		} else {
			declineStakingRewardsNew.setSelected(transaction.isDeclineStakingRewards());
			declineStakingRewardsLabelUpdate.setText(String.valueOf(transaction.isDeclineStakingRewards()));
		}
	}

	private void loadLargeFileUpdateToForm(final File transactionFile) throws HederaClientException {
		final var tempStorage = new File(TEMP_DIRECTORY, "tempStorage").getAbsolutePath();
		selectTransactionType.setValue("File Contents Update");
		unZip(transactionFile.getAbsolutePath(), tempStorage);
		final var files = new File(tempStorage).listFiles(
				(dir, name) -> JSON_EXTENSION.equals(FilenameUtils.getExtension(name)));
		if (files == null) {
			throw new HederaClientException("Error reading files");
		}
		if (files.length != 1) {
			throw new HederaClientException("Incorrect number of json files");
		}
		final var details = readJsonObject(files[0].getPath());
		try {
			FileUtils.deleteDirectory(new File(tempStorage));
		} catch (final IOException e) {
			throw new HederaClientException("Cannot delete directory", e.getCause());
		}

		if (details.has(FILE_ID_PROPERTIES)) {
			final var fileIdentifier = Identifier.parse(details.get(FILE_ID_PROPERTIES).getAsJsonObject());
			fileIdentifier.setNetworkName(controller.getCurrentNetwork());
			updateFileID.setText(fileIdentifier.toNicknameAndChecksum(controller.getAccountsList()));
		}
		if (details.has(FEE_PAYER_ACCOUNT_ID_PROPERTY)) {
			feePayerAccountField.setText(
					Identifier.parse(details.get(
							FEE_PAYER_ACCOUNT_ID_PROPERTY).getAsJsonObject()).toNicknameAndChecksum(
							controller.getAccountsList()));
		}
		if (details.has(NODE_ID_PROPERTIES)) {
			final var nodeIdentifier = Identifier.parse(details.get(NODE_ID_PROPERTIES).getAsJsonObject());
			nodeIdentifier.setNetworkName(controller.getCurrentNetwork());
			nodeAccountField.setText(nodeIdentifier.toNicknameAndChecksum(controller.getAccountsList()));
		}
		if (details.has(CHUNK_SIZE_PROPERTIES)) {
			chunkSizeTextField.setText(String.valueOf(details.get(CHUNK_SIZE_PROPERTIES).getAsInt()));
		}
		if (details.has(FIRST_TRANSACTION_VALID_START_PROPERTY)) {
			setNowTime(new Timestamp(
					details.get(FIRST_TRANSACTION_VALID_START_PROPERTY).getAsJsonObject()).asInstant());
		}
		if (details.has(VALID_INCREMENT_PROPERTY)) {
			intervalTextField.setText(String.valueOf(details.get(VALID_INCREMENT_PROPERTY).getAsInt()));
		}
		if (details.has(MEMO_PROPERTY)) {
			memoField.setText(details.get(MEMO_PROPERTY).getAsString());
		}
		if (details.has(TRANSACTION_FEE_PROPERTY)) {
			transactionFee.setText(
					Utilities.setCurrencyFormat(details.get(TRANSACTION_FEE_PROPERTY).getAsLong()));
		}
	}

	private void loadSystemTransactionToForm(final ToolSystemTransaction transaction) {
		cleanAllSystemFields();
		if (transaction.isDelete()) {
			systemFieldsSet.setDate(transaction.getExpiration());
		}
		final var entity = transaction.getEntity();
		entity.setNetworkName(controller.getCurrentNetwork());
		entityID.setText(entity.toNicknameAndChecksum(controller.getAccountsList()));
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

	private void loadFreezeTransactionToForm(final ToolFreezeTransaction transaction) {
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
				freezeFieldsSet.setDate(transaction.getStartTime().asInstant());
				break;
			case PREPARE_UPGRADE:
				// A non-freezing operation that initiates network wide preparation in advance of a scheduled
				// freeze upgrade. The update_file and file_hash fields must be provided and valid. The
				// start_time field may be omitted and any value present will be ignored.
				final var fileID = transaction.getFileID();
				fileID.setNetworkName(controller.getCurrentNetwork());
				freezeFileIDTextField.setText(fileID.toNicknameAndChecksum(controller.getAccountsList()));
				freezeFileHashTextField.setText(transaction.getFileHash());
				break;
			case FREEZE_UPGRADE:
				// Performs an immediate upgrade on auxilary services and containers providing
				// telemetry/metrics. Does not impact network operations.
			case TELEMETRY_UPGRADE:
				// Freezes the network at the specified time and performs the previously prepared automatic
				// upgrade across the entire network.
				freezeFieldsSet.setDate(transaction.getStartTime().asInstant());
				final var fileID1 = transaction.getFileID();
				fileID1.setNetworkName(controller.getCurrentNetwork());
				freezeFileIDTextField.setText(fileID1.toNicknameAndChecksum(controller.getAccountsList()));
				freezeFileHashTextField.setText(transaction.getFileHash());
				break;
			case FREEZE_ABORT:
				// Aborts a pending network freeze operation.
				break;

			default:
				throw new IllegalStateException("Unexpected value: " + freezeType);
		}
	}

	public void loadFormFromTransactionTest(final KeyEvent keyEvent) {
		if (keyEvent.getCode().equals(KeyCode.ENTER)) {
			loadFormFromTransaction();
		} else {
			logger.info("here");
		}
	}

	//endregion

	// region COMMON METHODS

	private void loadAccountNicknames() {
		try {
			final var nicknames =
					new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

			final var accountInfos = controller.accountsPaneController.getIdNickNames();
			for (final var s : accountInfos.keySet()) {
				if (nicknames.has(s)) {
					accountNickNames.add(nicknames.get(s).getAsString());
				}
			}
		} catch (final HederaClientException e) {
			controller.logAndDisplayError(e);
		}
	}

	private void getKeyFromNickname(
			final AutoCompleteNickname autoCompleteNickname, final KeyCode keyCode, final ScrollPane scrollPane) {
		if (keyCode.equals(KeyCode.ENTER)) {
			final var nick = autoCompleteNickname.getFirstItem();
			autoCompleteNickname.setText(nick);
			final var nicknames = controller.accountsPaneController.getIdNickNames();
			var id = "";
			for (final var entry : nicknames.entrySet()) {
				if (nick.equals(entry.getValue())) {
					id = entry.getKey();
				}
			}

			if (!controller.accountsPaneController.getAccountInfos().containsKey(id)) {
				return;
			}
			final var address = controller.accountsPaneController.getAccountInfos().get(id);
			logger.info("Chosen account: {}", nick);
			try {
				final var info =
						AccountInfo.fromBytes(readBytes(address));
				final var key = info.key;
				if (key != null) {
					setKeyFromFile(key, scrollPane);
					scrollPane.requestFocus();
				}
			} catch (final InvalidProtocolBufferException | HederaClientException e) {
				logger.error(e);
			}
			autoCompleteNickname.clear();
			autoCompleteNickname.setVisible(false);
		}
	}

	private void setKeyTreeInBox(final JsonObject key, final ScrollPane keyScrollPane) {
		final var keyTree = controller.buildKeyTreeView(key);
		keyTree.setStyle("-fx-border-color: white; -fx-background-color: white");
		keyScrollPane.setContent(keyTree);
		keyTree.setMinWidth(800);
		keyTree.prefWidthProperty().bind(createNewKey.widthProperty());
		newKeyJSON = key;
	}

	private void setKeyFromFile(final Key newKey, final ScrollPane createScrollPane) {
		final var keyJson = EncryptionUtils.keyToJson(newKey);

		setKeyTreeInBox(keyJson, createScrollPane);
		createScrollPane.setVisible(true);
	}


	private void setupTokenAssociationsFields(final Label errorLabel, final TextField textField) {
		errorLabel.setText(
				String.format("Maximum number of associations cannot exceed %d", MAX_TOKEN_AUTOMATIC_ASSOCIATIONS));
		textField.textProperty().addListener(
				(observable, oldValue, newValue) -> mtaChangeListener(oldValue, newValue, textField,
						errorLabel));
		textField.focusedProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.TRUE.equals(t1)) {
				textField.selectAll();
			}
			errorLabel.setVisible(false);
		});
	}

	private void mtaChangeListener(final String oldValue, final String newValue, final TextField textField,
			final Label error) {
		if (Objects.equals(newValue, "")) {
			return;
		}
		if (!newValue.matches("\\d*")) {
			textField.setText(newValue.replaceAll("\\D", ""));
		}

		error.setVisible(false);
		if (Integer.parseInt(textField.getText()) > MAX_TOKEN_AUTOMATIC_ASSOCIATIONS) {
			textField.setText(oldValue);
			error.setVisible(true);
		}
	}

	private void numericFieldListen(final TextField textField) {
		textField.textProperty().addListener(
				(observable, oldValue, newValue) -> {
					if (!newValue.matches("\\d*")) {
						textField.setText(newValue.replaceAll("\\D", ""));
					}
				});
	}

	private void setMemoByteCounter(final TextField textField, final Label label) {
		final var text = textField.getText();
		final var byteSize = text.getBytes(StandardCharsets.UTF_8).length;
		if (byteSize > MAX_MEMO_BYTES) {
			textField.setText(CommonMethods.trimString(text, MAX_MEMO_BYTES));
		}
		label.setText(
				String.format("Bytes remaining:\t%d", MAX_MEMO_BYTES - textField.getText().getBytes(
						StandardCharsets.UTF_8).length));
	}

	private Map<String, PublicKey> getStringPublicKeyMap() {
		final Map<String, PublicKey> publicKeys = new HashMap<>();
		final var keys =
				new File(KEYS_FOLDER).listFiles((dir, name) -> name.endsWith(PUB_EXTENSION));
		if (keys == null) {
			throw new HederaClientRuntimeException("Error reading public keys list");
		}
		Arrays.stream(keys).forEach(keyFile -> publicKeys.put(FilenameUtils.getBaseName(keyFile.getName()),
				EncryptionUtils.publicKeyFromFile(keyFile.getAbsolutePath())));
		return publicKeys;
	}


	/**
	 * Action to be taken when the user chooses a transaction to create
	 */
	public void chooseTransactionTypeChangedAction() {
		if (noise) {
			return;
		}
		transactionType = CreateTransactionType.get(selectTransactionType.getValue());
		signAndSubmitButton.setDisable(true);

		makeBoxesInvisible();

		cleanAllCreateFields();
		cleanAllUpdateFields();
		cleanAllTransferFields();
		cleanAllSystemFields();
		cleanAllFileUpdateContentsFields();
		cleanAllFreezeFields();

		commentsVBox.setVisible(true);
		commonFieldsVBox.setVisible(true);
		storeOrSubmitGridPane.setVisible(true);

		if (transactionType != CreateTransactionType.SELECT) {
			storeTransactionHBox.getChildren().clear();
			storeTransactionHBox.getChildren().add(createTransactionMenuButton(transactionType));
		}

		switch (transactionType) {
			case SELECT:
				commentsVBox.setVisible(false);
				commonFieldsVBox.setVisible(false);
				storeOrSubmitGridPane.setVisible(false);
				break;
			case CREATE:
				createAccountVBox.setVisible(true);
				signAndSubmitButton.setDisable(false);
				break;
			case UPDATE:
				accountIDToUpdateVBox.setVisible(true);
				updateAccountVBox.setVisible(true);
				signAndSubmitButton.setDisable(false);
				break;
			case TRANSFER:
				transferCurrencyVBox.setVisible(true);
				signAndSubmitButton.setDisable(false);
				break;
			case SYSTEM:
				systemDeleteUndeleteVBox.setVisible(true);
				signAndSubmitButton.setDisable(false);
				break;
			case FILE_UPDATE:
				fileContentsUpdateVBox.setVisible(true);
				signAndSubmitButton.setDisable(false);
				break;
			case FREEZE:
				freezeChoiceVBox.setVisible(true);
				freezeVBox.setVisible(true);
				signAndSubmitButton.setDisable(true);
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
		final var input = new JsonObject();
		final var transactionValidStart = startFieldsSet.getDate();

		// Common elements
		addCommonElements(input, transactionValidStart);
		switch (transactionType) {
			case CREATE:
				// Crypto create account fields
				addCryptoCreateElements(input);
				break;
			case UPDATE:
				// Crypto update account fields
				addCryptoUpdateElements(input);
				break;
			case TRANSFER:
				// Transfer fields
				addCryptoTransferElements(input);
				break;
			case SYSTEM:
				// System delete/un-delete fields
				addSystemElements(input);
				break;
			case FREEZE:
				// Freeze fields
				if (addFreezeNetworkFields(input)) {
					return null;
				}
				break;
			default:
				break;
		}
		return input;
	}

	private void addCommonElements(final JsonObject input, final Timestamp transactionValidStart) {
		// Transaction valid start
		input.add(TRANSACTION_VALID_START_FIELD_NAME, transactionValidStart.asJSON());

		// memo field
		if (!"".equals(memoField.getText())) {
			input.addProperty(MEMO_FIELD_NAME, memoField.getText());
		}

		// Fee payer
		final var feePayerID = Identifier.parse(feePayerAccountField.getText(), controller.getCurrentNetwork());
		input.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerID.asJSON());

		// Use default fee for transactions (note: Large binary files might override this)
		final var feeJson = new JsonObject();
		final var fee = Utilities.string2Hbar(transactionFee.getText());
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, fee.to(HbarUnit.TINYBAR));
		input.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		// Use default for transaction valid duration
		input.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, controller.getTxValidDuration());

		// Node ID
		input.add(NODE_ID_FIELD_NAME,
				Identifier.parse(nodeAccountField.getText(), controller.getCurrentNetwork()).asJSON());

		// Network
		input.addProperty(NETWORK_FIELD_NAME, controller.getCurrentNetwork());
	}

	private void addCryptoTransferElements(final JsonObject input) {
		//Get transfers from tables
		if (!fromTransferTable.getItems().isEmpty() && !toTransferTable.getItems().isEmpty()) {
			final List<AccountAmountStrings> transfers = new ArrayList<>();
			for (final var a : fromTransferTable.getItems()) {
				transfers.add(a.negate());
			}
			transfers.addAll(toTransferTable.getItems());
			final var jsonArray = new JsonArray();
			for (final var a : transfers) {
				final var accountAmountPair = new JsonObject();
				final var accountAsJSON =
						Identifier.parse(a.getStrippedAccountID(), controller.getCurrentNetwork()).asJSON();
				accountAmountPair.add(ACCOUNT, accountAsJSON);
				accountAmountPair.addProperty(AMOUNT, a.getAmountAsLong());
				jsonArray.add(accountAmountPair);
			}
			input.add(TRANSFERS, jsonArray);
		}
	}

	private void addCryptoCreateElements(final JsonObject input) {
		// Balance
		if (!"".equals(createInitialBalance.getText())) {
			final var balanceJson = new JsonObject();
			balanceJson.addProperty(H_BARS, 0);
			balanceJson.addProperty(TINY_BARS, string2Hbar(createInitialBalance.getText()).toTinybars());
			input.add(INITIAL_BALANCE_FIELD_NAME, balanceJson);
		}
		// Auto renew
		if (!"".equals(createAutoRenew.getText())) {
			input.addProperty(AUTO_RENEW_PERIOD_FIELD_NAME, Long.parseLong(createAutoRenew.getText()));
		}

		// Key
		if (!newKeyJSON.isJsonNull() && newKeyJSON.size() != 0 && !newKeyJSON.equals(emptyKeyObject())) {
			input.add(NEW_KEY_FIELD_NAME, newKeyJSON);
		}

		// Account Memo
		input.addProperty(ACCOUNT_MEMO_FIELD_NAME, createAccountMemo.getText());

		// Max Auto Token Associations
		final var text = createMaxTokenAssociations.getText();
		input.addProperty(MAX_TOKEN_ASSOCIATIONS_FIELD_NAME, text.isEmpty() ? 0 : Integer.parseInt(text));

		// Receiver Sig Required
		input.addProperty(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, createSignatureRequired.isSelected());

		// Account Memo
		input.addProperty(ACCOUNT_MEMO_FIELD_NAME, createAccountMemo.getText());

		// Max Auto Token Associations
		final var newTokens = createMaxTokenAssociations.getText();
		input.addProperty(MAX_TOKEN_ASSOCIATIONS_FIELD_NAME, newTokens.isEmpty() ? 0 : Integer.parseInt(newTokens));

		if ((stakedAccountIdField.getText() != null) && (!stakedAccountIdField.getText().isEmpty())) {
			input.add(STAKED_ACCOUNT_ID_FIELD_NAME,
					Identifier.parse(stakedAccountIdField.getText(), controller.getCurrentNetwork()).asJSON());
		}
		if ((stakedNodeIdField.getText() != null) && (!stakedNodeIdField.getText().isEmpty())) {
			input.addProperty(STAKED_NODE_ID_FIELD_NAME, Long.parseLong(stakedNodeIdField.getText()));
		}

		input.addProperty(DECLINE_STAKING_REWARDS_FIELD_NAME, declineStakingRewards.isSelected());

	}

	private void addCryptoUpdateElements(final JsonObject input) {
		// Account ID
		if (!"".equals(updateAccountID.getText())) {
			input.add(ACCOUNT_TO_UPDATE,
					Identifier.parse(updateAccountID.getText(), controller.getCurrentNetwork()).asJSON());
		}
		final var account = Identifier.parse(updateAccountID.getText(), controller.getCurrentNetwork());
		final var info = controller.getAccountInfoMap().getOrDefault(account, null);

		// Key
		if (!newKeyJSON.isJsonNull() && newKeyJSON.size() != 0 && !newKeyJSON.equals(originalKey) && !newKeyJSON.equals(
				emptyKeyObject())) {
			input.add(NEW_KEY_FIELD_NAME, newKeyJSON);
		}

		// Auto renew
		final var originalARP = info != null ? info.autoRenewPeriod.getSeconds() : 0;
		final var newARP = Long.parseLong(updateAutoRenew.getText());
		if (originalARP != newARP) {
			input.addProperty(AUTO_RENEW_PERIOD_FIELD_NAME, newARP);
		}

		// Receiver Sig Required
		final var originalSigRequired = info != null && info.isReceiverSignatureRequired;
		final var newSigRequired = updateReceiverSignatureRequired.isSelected();
		if (originalSigRequired != newSigRequired) {
			input.addProperty(RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME, newSigRequired);
		}

		// Account Memo
		if (!updateAccountMemoNew.getText().equals(updateAccountMemoOriginal.getText())) {
			input.addProperty(ACCOUNT_MEMO_FIELD_NAME, updateAccountMemoNew.getText());
		}

		// Max Auto Token Associations
		final var oldTokens = updateMaxTokensOriginal.getText();
		final var newTokens = updateMaxTokensNew.getText();
		try {
			if (Integer.parseInt(oldTokens) != Integer.parseInt(newTokens)) {
				input.addProperty(MAX_TOKEN_ASSOCIATIONS_FIELD_NAME,
						newTokens.isEmpty() ? 0 : Integer.parseInt(newTokens));
			}
		} catch (final NumberFormatException e) {
			logger.error("Cannot parse string: {}", e.getMessage());
		}

		if ((stakedAccountIdNew.getText() != null) && (!stakedAccountIdNew.getText().isEmpty())) {
			input.add(STAKED_ACCOUNT_ID_FIELD_NAME,
					Identifier.parse(stakedAccountIdNew.getText(), controller.getCurrentNetwork()).asJSON());
		}
		if ((stakedNodeIdNew.getText() != null) && (!stakedNodeIdNew.getText().isEmpty())) {
			input.addProperty(STAKED_NODE_ID_FIELD_NAME, Long.parseLong(stakedNodeIdNew.getText()));
		}

		final var originalDeclineStakingRewardsNew = info != null && info.stakingInfo != null && info.stakingInfo.declineStakingReward;
		if (originalDeclineStakingRewardsNew != declineStakingRewardsNew.isSelected()) {
			input.addProperty(DECLINE_STAKING_REWARDS_FIELD_NAME, declineStakingRewardsNew.isSelected());
		}
	}

	private void addSystemElements(final JsonObject input) {
		// Entity ID
		if (!"".equals(entityID.getText())) {
			input.add(ENTITY_TO_DEL_UNDEL,
					Identifier.parse(entityID.getText(), controller.getCurrentNetwork()).asJSON());
		}

		// File/Contract
		input.addProperty(FILE_CONTRACT_SWITCH, systemTypeChoiceBox.getValue());

		// Delete/Un-delete
		input.addProperty(DEL_UNDEL_SWITCH, systemActionChoiceBox.getValue());

		// Expiration time
		if (datePickerSystem.getValue() != null) {
			input.addProperty(EXPIRATION_DATE_TIME, systemFieldsSet.getDate().asRFCString());
		}
	}

	private boolean addFreezeNetworkFields(final JsonObject input) {
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
					input.addProperty(FREEZE_START_TIME_FIELD_NAME, freezeFieldsSet.getDate().asRFCString());
					break;
				case PREPARE_UPGRADE:
					input.add(FREEZE_FILE_ID_FIELD_NAME,
							Identifier.parse(freezeFileIDTextField.getText(), controller.getCurrentNetwork()).asJSON());
					input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, freezeFileHashTextField.getText());
					break;
				case FREEZE_UPGRADE:
				case TELEMETRY_UPGRADE:
					input.add(FREEZE_FILE_ID_FIELD_NAME,
							Identifier.parse(freezeFileIDTextField.getText(), controller.getCurrentNetwork()).asJSON());
					input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, freezeFileHashTextField.getText());
					input.addProperty(FREEZE_START_TIME_FIELD_NAME, freezeFieldsSet.getDate().asRFCString());
					break;
				case FREEZE_ABORT:
					break;
				default:
					throw new IllegalStateException("Unexpected value: " + FreezeType.valueOf(freezeType));
			}

		}
		return false;
	}

	/**
	 * Pairs the created transaction with the comments the user might have left
	 *
	 * @param tx
	 * 		the created transaction
	 * @return a pair with user comments and the transaction
	 */
	private Pair<UserComments, ToolTransaction> getUserCommentsTransactionPair(final ToolTransaction tx) {

		final var creatorComments = new UserComments.Builder().withAuthor(controller.getUserName()).withComment(
				createCommentsTextArea.getText()).build();

		logger.info(creatorComments);

		controller.displaySystemMessage(String.format("With comments: %s", creatorComments));


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
		final var accounts = controller.getAccountsList();
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
			final var feePayer = Identifier.parse(feePayerAccountField.getText(), controller.getCurrentNetwork());
			feePayerAccountField.setText(feePayer.toNicknameAndChecksum(accounts));
			invalidFeePayer.setVisible(false);
		} catch (final Exception e) {
			invalidFeePayer.setVisible(true);
			displayAndLogInformation("Fee payer cannot be parsed");
			flag = false;
		}

		// Check and flag the node
		try {
			final var node = Identifier.parse(nodeAccountField.getText(), controller.getCurrentNetwork());
			final var client = CommonMethods.getClient(controller.getCurrentNetwork());
			if (client.getNetwork().containsValue(node.asAccount())) {
				nodeAccountField.setText(node.toNicknameAndChecksum(accounts));
				invalidNode.setVisible(false);
			} else {
				invalidNode.setVisible(true);
				displayAndLogInformation("Node ID out of range");
				flag = false;
			}
		} catch (final Exception e) {
			invalidNode.setVisible(true);
			displayAndLogInformation("Node ID cannot be parsed");
			flag = false;
		}
		return flag;
	}

	private void displayAndLogInformation(final String message) {
		controller.displaySystemMessage("Error: " + message);
		logger.info(message);
	}

	private void displayAndLogInformation(final Exception exception) {
		controller.displaySystemMessage("Error: " + exception.toString());
		logger.error(exception);
	}

	private JsonObject emptyKeyObject() {
		final var kk = new JsonObject();
		kk.add("keys", new JsonArray());
		final var object = new JsonObject();
		object.add("keyList", kk);
		return object;
	}

	private MenuButton createTransactionMenuButton(final CreateTransactionType type) {
		final var menuButton = new MenuButton();
		setupOutputDirectoriesList();
		menuButton.setStyle(MENU_BUTTON_STYLE);
		menuButton.setText("CREATE AND EXPORT");
		menuButton.setMinWidth(300);


		menuButton.getItems().clear();
		// setup button text
		for (final var s : outputDirectories) {
			final var menuItem = new MenuItem(s.getPath().replace(System.getProperty("user.home") + File.separator,
					""));
			logger.info("Adding menu-item: \"{}\"", menuItem.getText());
			final var email = controller.getEmailFromMap(s.getPath());
			controller.setUserName(email);

			menuItem.setOnAction(actionEvent -> {
				if (doNotStoreExpiringTransaction()) {
					return;
				}
				storeToOutput(type, s);
			});
			menuButton.getItems().add(menuItem);
		}

		final var menuItem = new MenuItem("browse for directory");
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
	private void storeToOutput(final CreateTransactionType type, final FileService fileService) {
		fromFile = false;
		if (fileService == null) {
			return;
		}
		try {
			// File update is treated differently because it does not conform to the usual creation paradigm
			if (type.equals(CreateTransactionType.FILE_UPDATE)) {
				prepareZipAndComment(fileService);
				return;
			}

			final var pair = getUserCommentsTransactionPair(type);
			if (pair == null) {
				return;
			}
			storeTransactionAndComment(pair, fileService);
			cleanForm();
		} catch (final HederaClientException e) {
			controller.displaySystemMessage(e);
		}

	}

	/**
	 * Browse to a folder and store the transaction and comment
	 *
	 * @param type
	 * 		the transaction type
	 */
	private void storeOutputToBrowsedOutput(final CreateTransactionType type) {
		final var s = BrowserUtilities.browseDirectories(controller.getLastTransactionsDirectory(), createAnchorPane);
		controller.setLastBrowsedDirectory(new File(s));
		FileService fileService = null;
		try {
			fileService = FileAdapterFactory.getAdapter(s);
		} catch (final HederaClientException e) {
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
		final var file = BrowserUtilities.browseFiles(controller.getLastTransactionsDirectory(), createAnchorPane,
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
		final var date = startFieldsSet.getDate();
		final var now = new Date();
		final var secs = date.getSeconds() - now.getTime() / 1000;
		if (secs < 120) {
			answer = PopupMessage.display("Warning", String.format(REMAINING_TIME_MESSAGE, secs), true,
					"CONTINUE",
					"CANCEL");
		}
		return !answer;
	}

	private Pair<UserComments, ToolTransaction> getUserCommentsTransactionPair(
			final CreateTransactionType type) throws HederaClientException {
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

		startFieldsSet = new TimeFieldSet(datePicker, hourField, minuteField, secondsField, nanosField, timeZoneHBox,
				createUTCTimeLabel, invalidDate);
		startFieldsSet.configureDateTime(LocalDateTime.now());

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
					startFieldsSet.setDate(Instant.now());
					systemFieldsSet.setDate(Instant.now());
				});

		createCharsLeft.setText(String.format("Characters left: %d", LIMIT));

		resetFormButton.visibleProperty().bind(commentsVBox.visibleProperty());
	}

	private void setTextSizeLimit(final TextArea field, final int endIndex, final Number oldValue,
			final Number newValue) {
		if (newValue.intValue() > oldValue.intValue() && field.getText().length() >= endIndex) {
			field.setText(memoField.getText().substring(0, endIndex));
		}
	}

	private void fixNumericTextField(final TextField hour, final String newValue, final String s, final String regex) {
		if (!newValue.matches(s)) {
			hour.setText(newValue.replaceAll(regex, ""));
		}
	}

	private boolean isDateValid(final TextField hourField, final TextField minuteField, final TextField secondsField,
			final TextField nanosField, final DatePicker datePicker, final ZoneId zoneId, final HBox timeZoneHBox) {
		var flag = true;
		try {
			final var zone = timeZoneHBox.getChildren().get(0);
			if (!(zone instanceof AutoCompleteNickname)) {
				throw new HederaClientRuntimeException("Unrecognized node");
			}
			final var zoneString = ((AutoCompleteNickname) zone).getText();
			if (!ZoneId.getAvailableZoneIds().contains(zoneString)) {
				displayAndLogInformation("Invalid time zone");
				flag = false;
			}

			final var hour = Integer.parseInt(hourField.getText());
			final var minute = Integer.parseInt(minuteField.getText());
			final var second = Integer.parseInt(secondsField.getText());
			final var nanos = Long.parseLong(nanosField.getText());

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

			final var localDateTime =
					LocalDateTime.of(datePicker.getValue() != null ? datePicker.getValue() :
							LocalDate.now(), LocalTime.of(hour, minute, second));

			final var transactionValidStart = Date.from(localDateTime.atZone(zoneId).toInstant().plusNanos(nanos));

			if (transactionValidStart.before(new Date())) {
				displayAndLogInformation("Transaction valid start in the past");
				flag = false;
			}

		} catch (final Exception e) {
			flag = false;
		}
		return flag;
	}

	private void formatAccountTextField(final TextField textField, final Label errorLabel, final Node nextNode) {
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

	private void accountTFRemoveFocus(final TextField textField, final Label errorLabel) {
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

		final var parsedAddress = parseAddress(NetworkEnum.asLedger(controller.getCurrentNetwork()).toBytes(), account);
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
				final var id = Identifier.parse(account, controller.getCurrentNetwork());
				textField.setText(id.toNicknameAndChecksum(controller.getAccountsList()));
				errorLabel.setVisible(false);
		}


	}

	private void formatHBarTextField(final TextField textField) {
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

	private void storeTransactionAndComment(final Pair<UserComments, ToolTransaction> pair,
			final FileService remoteLocation) throws HederaClientException {
		final var transaction = pair.getValue();
		final var userComments = pair.getKey();

		final var accountId = transaction.getFeePayerID();
		final var seconds = transaction.getTransactionValidStart().getEpochSecond();
		final var filenames = String.format("%d-%s-%d", seconds, accountId.toReadableString().replace(".", "_"),
				transaction.hashCode());

		final var tempStorage = new File(TEMP_DIRECTORY, "tempStorage").getAbsolutePath();
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
		} catch (final HederaClientException e) {
			logger.error(e);
			controller.displaySystemMessage(e);
		}

		final var txtFile = new File(txFile.getAbsolutePath().replace(".tx", ".txt"));

		final List<File> files = new ArrayList<>();
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
			if (new File(tempStorage).exists()) {
				throw new HederaClientRuntimeException("Unable to delete temporary storage folder");
			}

		} catch (final Exception e) {
			throw new HederaClientException("Error while deleting temporary files");
		}

		//reload the home pane, to show the transaction
		controller.homePaneController.initializeHomePane();
		initializeCreatePane();
		selectTransactionType.setValue(SELECT_STRING);
	}

	private void moveToOutput(final List<File> files, final FileService remoteLocation) {
		final var remote = remoteLocation.getPath() + File.separator;
		for (final var f : files) {
			try {
				var outputFolder = File.separator;
				if (controller.getOneDriveCredentials().containsKey(remoteLocation.getPath())) {
					final var user = controller.getEmailFromMap(remoteLocation.getPath());
					outputFolder = "".equals(user) ? File.separator : "/OutputFiles/" + user + File.separator;
					logger.info("Exporting file: {}", f.getAbsolutePath());
				}
				FileUtils.moveFile(f, new File(remote + outputFolder + f.getName()));
			} catch (final IOException e) {
				logger.error(e);
				controller.displaySystemMessage(e.getMessage());
				PopupMessage.display("Unable to upload",
						"Could not upload the file to the specified folder. Please check you have the appropriate " +
								"permissions",
						"OK");
			}
		}
	}

	private void setupKeyPane(final TreeView<String> jsonObjectKey, final ScrollPane scrollPane) {
		jsonObjectKey.prefHeightProperty().bind(scrollPane.heightProperty());
		jsonObjectKey.prefWidthProperty().bind(scrollPane.widthProperty());
		jsonObjectKey.setStyle("-fx-border-color: white");
		jsonObjectKey.setMinWidth(800);
		scrollPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.NEVER);
		scrollPane.setContent(jsonObjectKey);
		scrollPane.managedProperty().bind(scrollPane.visibleProperty());
	}

	private void cleanCommonFields() {
		startFieldsSet.reset(controller.getDefaultHours(), controller.getDefaultMinutes(),
				controller.getDefaultSeconds());
		feePayerAccountField.clear();
		createCommentsTextArea.clear();
		final var defaultNodeID =
				Identifier.parse(controller.getDefaultNodeID(), controller.getCurrentNetwork()).toNicknameAndChecksum(
						controller.getAccountsList());
		nodeAccountField.setText(defaultNodeID);
		transactionFee.setText(setCurrencyFormat(controller.getDefaultTxFee()));
		setupHbarNumberField(transactionFee);
		memoField.clear();
		createUTCTimeLabel.setText("");
	}

	private void setNowTime(final Instant now) {
		startFieldsSet.setDate(now);
	}

	private void setupHbarNumberField(final TextField currencyField) {
		currencyField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixNumericTextField(currencyField, newValue, "[^\\d.\\s]",
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

	private void setupIntNumberField(final TextField intField, final int limit) {
		intField.textProperty().addListener(
				(observable, oldValue, newValue) -> fixNumericTextField(intField, newValue, "\\d*", REGEX));
		intField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				try {
					intField.setText(String.valueOf(Math.min(Integer.parseInt(intField.getText()), limit)));
				} catch (final NumberFormatException e) {
					logger.error("Cannot parse field");
				}
			}
		});
	}

	private void setupManagedProperty(final Node... nodes) {
		for (final var n : nodes) {
			n.managedProperty().bind(n.visibleProperty());
		}
	}

	private void setupTextFieldResizeProperty(final TextField... textFields) {
		for (final var tf : textFields) {
			tf.setMinWidth(300);
			tf.textProperty().addListener((ov, prevText, currText) -> resizeTextField(tf, currText));
		}

	}

	private void resizeTextField(final TextField tf, final String currText) {
		// Do this in a Platform.runLater because of Textfield has no padding at first time and so on
		Platform.runLater(() -> {
			final var text = new Text(currText);
			text.setFont(tf.getFont()); // Set the same font, so the size is the same
			final var width = text.getLayoutBounds().getWidth() // This big is the Text in the TextField
					+ tf.getPadding().getLeft() + tf.getPadding().getRight() // Add the padding of the TextField
					+ 4d; // Add some spacing
			tf.setPrefWidth(width); // Set the width
			tf.positionCaret(tf.getCaretPosition());
		});
	}

	private void processKey(final JsonObject key, final ScrollPane keyPane) {
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

	private static void setHBarFormat(final TextField currencyTextField) {
		final var hBarsLong = Utilities.string2Hbar(currencyTextField.getText()).toTinybars();
		logger.debug("Currency text field changed to: {}", currencyTextField.getText());
		final var hBarsString = Utilities.setHBarFormat(hBarsLong);
		currencyTextField.setText(hBarsString.substring(0, hBarsString.length() - 1));
	}

	public void cleanForm() {
		fromFile = false;
		final var type = selectTransactionType.getValue();
		switch (CreateTransactionType.get(type)) {
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
			case FREEZE:
				cleanAllFreezeFields();
				break;
			default:
				logger.error("Unknown transaction");
		}

		initializeCreatePane();
		selectTransactionType.setValue(type);
	}

	public void signAndSubmitAction() throws HederaClientException {

		switch (transactionType) {
			case FILE_UPDATE:
				signAndSubmitMultipleTransactions();
				break;
			case CREATE:
			case UPDATE:
			case TRANSFER:
			case SYSTEM:
				signAndSubmitSingleTransaction();
				break;
			default:
				throw new HederaClientException("Cannot process transaction");
		}
	}

	private void signAndSubmitMultipleTransactions() throws HederaClientException {
		startFieldsSet.setDate(Instant.now());
		if (!checkNode()) {
			invalidNode.setVisible(true);
			startFieldsSet.reset(1,0,0);
			return;
		}
		final var largeUpdateFile = new File(createLargeFileUpdateFiles());
		final var largeBinaryFile = new LargeBinaryFile(FileDetails.parse(largeUpdateFile));
		final var transactions = largeBinaryFile.createTransactionList();
		logger.info("Transactions created");

		final var privateKeyFiles = getPrivateKeys(transactions.get(0));
		if (privateKeyFiles.isEmpty()) {
			return;
		}
		final List<PrivateKey> privateKeys = new ArrayList<>();
		for (final var privateKeyFile : privateKeyFiles) {
			final var nameKeyPair = controller.getAccountKeyPair(privateKeyFile);
			logger.info("Signing transaction with key: {}", nameKeyPair.getKey());
			privateKeys.add(PrivateKey.fromBytes(nameKeyPair.getValue().getPrivate().getEncoded()));
		}

		final var submit = TransactionPopup.display(largeBinaryFile);
		if (!submit) {
			return;
		}

		final var progressBar = new ProgressBar();
		final var cancelButton = new Button(CANCEL_LABEL);
		final var size = transactions.size();
		final var window = ProgressPopup.setupProgressPopup(progressBar, cancelButton, "Updating File Contents",
				"Please wait while the file update transactions are being submitted.", size);
		final Status[] error = new Status[1];

		final Task<Void> task = new Task<>() {
			@Override
			protected Void call() throws InterruptedException, HederaClientException {
				long counter = 0;
				for (final var transaction : transactions) {
					final var toolTransaction = transaction.atNow();
					toolTransaction.setNetwork(controller.getCurrentNetwork());
					for (final var privateKey : privateKeys) {
						toolTransaction.sign(privateKey);
						updateProgress(counter, size);
					}
					try {
						final var receipt = toolTransaction.submit();
						storeReceipt(receipt, FilenameUtils.getBaseName(largeBinaryFile.getName()),
								TransactionType.FILE_UPDATE.toString());
						if (receipt.status != Status.OK && receipt.status != Status.SUCCESS) {
							error[0] = receipt.status;
							cancel();
						}
						logger.info(receipt);
					} catch (final PrecheckStatusException e) {
						logger.error(e.getMessage());
						storeReceipt(e.status, FilenameUtils.getBaseName(largeBinaryFile.getName()));
						error[0] = e.status;
						cancel();
					} catch (final ReceiptStatusException e) {
						logger.error(e.getMessage());
						storeReceipt(e.receipt, FilenameUtils.getBaseName(largeBinaryFile.getName()),
								TransactionType.FILE_UPDATE.toString());
						error[0] = e.receipt.status;
						cancel();
					}
					counter++;
				}
				sleep(1000);
				updateProgress(size, size);
				return null;
			}
		};

		cancelButton.setOnAction(actionEvent -> {
			logger.info("Task cancelled by user");
			task.cancel();
		});

		progressBar.progressProperty().bind(task.progressProperty());
		new Thread(task).start();
		task.setOnSucceeded(workerStateEvent -> {
			logger.info("Transactions signed");
			largeBinaryFile.setHistory(true);
			moveToHistory(largeBinaryFile, privateKeyFiles);
			controller.historyPaneController.addToHistory(largeBinaryFile);

			if (window != null) {
				window.close();
			}
			PopupMessage.display("Update succeeded",
					String.format("Contents update for file %s succeeded", updateFileID.getText()));
			initializeCreatePane();
		});

		task.setOnCancelled(workerStateEvent -> {
			logger.info("Update balances cancelled");
			if (window != null) {
				window.close();
			}
			moveToHistory(largeBinaryFile, privateKeyFiles);
			largeBinaryFile.setHistory(true);
			controller.historyPaneController.addToHistory(largeBinaryFile);

			final var errorMessage = error[0] != null ? getErrorMessage(error[0]) : "CANCELLED";
			PopupMessage.display("Update failed",
					String.format("File update failed with error %s. Please review the transaction and try again.",
							errorMessage));
			initializeCreatePane();
		});

		task.setOnFailed(workerStateEvent -> {
			logger.info("Update balances failed");
			moveToHistory(largeBinaryFile, privateKeyFiles);
			if (window != null) {
				window.close();
			}
		});
	}

	private boolean checkNode() {
		final Identifier node;
		try {
			node = Identifier.parse(nodeAccountField.getText(), controller.getCurrentNetwork());
		} catch (final Exception e) {
			return false;
		}
		final var client = CommonMethods.getClient(controller.getCurrentNetwork());
		return client.getNetwork().containsValue(node.asAccount());
	}

	private void moveToHistory(final LargeBinaryFile largeBinaryFile, final List<File> privateKeyFiles) {
		try {
			for (final var privateKeyFile : privateKeyFiles) {
				largeBinaryFile.moveToHistory(Actions.ACCEPT, "",
						FilenameUtils.getBaseName(privateKeyFile.getName()));
			}

		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	private void signAndSubmitSingleTransaction() throws HederaClientException {
		startFieldsSet.setDate(Instant.now().plusSeconds(1));
		final var pair = getUserCommentsTransactionPair(transactionType);
		if (pair == null) {
			return;
		}

		final var transaction = pair.getValue();
		final var privateKeys = getPrivateKeys(transaction);
		if (privateKeys.isEmpty()) {
			return;
		}

		Collections.sort(privateKeys);

		final var transactionName = transaction.getTransaction().getTransactionId().toString().replace(
				"@", "_").replace(".", "_");
		final var location = DEFAULT_HISTORY + File.separator + transactionName + "." + TRANSACTION_EXTENSION;
		transaction.store(location);
		final var rf = new TransactionFile(location);
		final var comments = createCommentsTextArea.getText();

		for (final var privateKeyFile : privateKeys) {
			final var nameKeyPair = controller.getAccountKeyPair(privateKeyFile);
			logger.info("Signing transaction with key: {}", nameKeyPair.getKey());
			transaction.sign(PrivateKey.fromBytes(nameKeyPair.getValue().getPrivate().getEncoded()));
			rf.moveToHistory(Actions.ACCEPT, comments, FilenameUtils.getBaseName(privateKeyFile.getName()));
		}

		final var keyTree = getKeyTree(rf);
		if (keyTree.getRoot() != null) {
			rf.setTreeView(keyTree);
		}
		final var oldKeyTree = getOldKey(rf);
		if (oldKeyTree.getRoot() != null) {
			rf.setOldKey(oldKeyTree);
		}
		rf.setHistory(false);

		final var submit = TransactionPopup.display(rf);
		if (!submit) {
			return;
		}

		rf.setHistory(true);

		try {
			final var receipt = transaction.submit();
			logger.info(receipt);
			showReceiptOnPopup(transaction, receipt);
			storeReceipt(receipt, transactionName, rf.getTransactionType().toString());
			controller.homePaneController.initializeHomePane();
			initializeCreatePane();
			selectTransactionType.setValue(SELECT_STRING);
		} catch (final InterruptedException e) {
			logger.error(e.getMessage());
			logger.error("Thread interrupted");
			Thread.currentThread().interrupt();
		} catch (final ReceiptStatusException e) {
			logger.error(e.getMessage());
			storeReceipt(e.receipt, transactionName, rf.getTransactionType().toString());
			PopupMessage.display(STATUS,
					String.format(TRANSACTION_FAILED_ERROR_MESSAGE, getErrorMessage(e.receipt.status)));
		} catch (final PrecheckStatusException e) {
			logger.error(e.getMessage());
			storeReceipt(e.status, transactionName);
			PopupMessage.display(STATUS, String.format(TRANSACTION_FAILED_ERROR_MESSAGE, getErrorMessage(e.status)));
		}
		controller.historyPaneController.addToHistory(rf);
	}

	private TreeView<String> getOldKey(final TransactionFile transactionFile) {
		return TransactionType.CRYPTO_UPDATE.equals(transactionFile.getTransaction().getTransactionType()) ?
				controller.buildKeyTreeView(originalKey) :
				new TreeView<>();
	}


	private TreeView<String> getKeyTree(final TransactionFile transactionFile) {
		final KeyList key;
		switch (transactionFile.getTransaction().getTransactionType()) {
			case CRYPTO_CREATE:
				key = ((ToolCryptoCreateTransaction) transactionFile.getTransaction()).getKey();
				break;
			case CRYPTO_UPDATE:
				key = ((ToolCryptoUpdateTransaction) transactionFile.getTransaction()).getKey();
				break;
			default:
				return new TreeView<>();
		}
		return controller.buildKeyTreeView(key);
	}

	private void showReceiptOnPopup(final ToolTransaction transaction, final TransactionReceipt receipt) {
		switch (receipt.status) {
			case OK:
			case SUCCESS:
				final var message = getPopupMessage(transaction, receipt);
				PopupMessage.display("Final status", message);
				break;
			default:
				PopupMessage.display("Final status",
						String.format("The transaction failed with status %s. Please review and try again.",
								getErrorMessage(receipt.status)));
		}
	}

	@NotNull
	private List<File> getPrivateKeys(final ToolTransaction transaction) {
		final var knownSigners = controller.accountsPaneController.getFeePayers();
		final var signers = transaction.getSigningAccounts();
		final var privateKeys = new HashSet<>(controller.extractRequiredKeys(transaction.getSigningKeys()));

		final Set<Identifier> unknownSigners = new HashSet<>();
		for (final var signer : signers) {
			final var identifier = new Identifier(signer, controller.getCurrentNetwork());
			if (!knownSigners.contains(identifier)) {
				unknownSigners.add(identifier);
			}
		}

		if (!unknownSigners.isEmpty()) {
			final var ids = unknownSigners.stream().sorted().map(Identifier::toReadableAccountAndNetwork).collect(
					Collectors.joining(", "));
			final var message = unknownSigners.size() > 1 ?
					String.format("accounts: %n%s%nare", ids) :
					String.format("account %s is", ids);
			PopupMessage.display("Unknown keys", String.format(
					"The keys for %s unknown. You will be required to select signing keys as an extra step.", message));
			privateKeys.addAll(ExtraKeysSelectorPopup.display(new HashSet<>(privateKeys)));
		}
		var response = display(privateKeys);
		while (!Boolean.TRUE.equals(response)) {
			if (response == null) {
				return new ArrayList<>();
			}
			final var list = ExtraKeysSelectorPopup.display(new HashSet<>(privateKeys));
			privateKeys.clear();
			privateKeys.addAll(list);
			response = display(privateKeys);
		}
		return new ArrayList<>(privateKeys);
	}

	private String getPopupMessage(final ToolTransaction transaction, final TransactionReceipt receipt) {
		var message = "";
		switch (transaction.getTransactionType()) {
			case CRYPTO_TRANSFER:
				final var amount = ((ToolTransferTransaction) transaction).getHbarsTransferred();
				message = String.format("The transfer transaction suceeded. %s were transferred.", amount.toString());
				break;
			case CRYPTO_CREATE:
				message = String.format("The crypto create transaction suceeded. Account created with account id %s",
						receipt.accountId.toString());
				break;
			case CRYPTO_UPDATE:
				final var account =
						((ToolCryptoUpdateTransaction) transaction).getAccount().toReadableAccountAndNetwork();
				final var fields = String.join("\n\t\u2022 ", ((ToolCryptoUpdateTransaction) transaction).getUpdateList());
				message = String.format("The account update transaction succeeded. The following account properties were updated for %s:\n\t\u2022 %s", account,
						fields);
				break;
			case SYSTEM_DELETE_UNDELETE:
				final var systemTransaction = (ToolSystemTransaction) transaction;
				final var entity = String.format(systemTransaction.isFile() ? "File %s" : "Contract %s",
						systemTransaction.getEntity().toReadableString());
				final var outcome = systemTransaction.isDelete() ? "has been deleted" : "has been restored";
				message = String.format("The system transaction succeeded. %s %s", entity, outcome);
				break;
			case FILE_UPDATE:
				final var file = ((ToolFileUpdateTransaction) transaction).getFile().toReadableString();
				message = String.format("The file update transaction suceeded. File %s was updated", file);
				break;
			case FILE_APPEND:
				final var append = ((ToolFileAppendTransaction) transaction).getFile().toReadableString();
				message = String.format("The file append transaction suceeded. File %s was updated", append);
				break;
			case FREEZE:
				message = "The freeze transaction suceeded";
				break;
			default:
				message = "Unknown transaction type";
		}
		return message;
	}


	//endregion


	private String getErrorMessage(final Status status) {
		return status.toString();
	}

	private void storeReceipt(final TransactionReceipt receipt, final String name, final String type) {
		try {
			if (new File(DEFAULT_RECEIPTS).mkdirs()) {
				logger.info("Receipts folder created");
			}
			final var pathname = DEFAULT_RECEIPTS + File.separator + name + "." + RECEIPT_EXTENSION;
			if (new File(pathname).exists()) {
				Files.deleteIfExists(Path.of(pathname));
			}
			final var jsonObject = new JsonObject();
			jsonObject.addProperty(JsonConstants.STATUS_PROPERTY, receipt.status.toString());
			jsonObject.addProperty(JsonConstants.RECEIPT_PROPERTY, receipt.toString());
			jsonObject.add(JsonConstants.TIMESTAMP_PROPERTY, new Timestamp().asJSON());
			jsonObject.addProperty(JsonConstants.TYPE_PROPERTY, type);
			if ("Create New Account Transaction".equals(type)) {
				jsonObject.addProperty(JsonConstants.ENTITY_PROPERTY, receipt.accountId.toString());
			}

			writeJsonObject(pathname, jsonObject);
		} catch (final IOException | HederaClientException e) {
			logger.error(e.getMessage());
		}

	}

	private void storeReceipt(final Status status, final String name) {
		try {
			if (new File(DEFAULT_RECEIPTS).mkdirs()) {
				logger.info("Receipts folder created");
			}
			final var pathname = DEFAULT_RECEIPTS + File.separator + name + "." + RECEIPT_EXTENSION;
			if (new File(pathname).exists()) {
				Files.deleteIfExists(Path.of(pathname));
			}
			final var jsonObject = new JsonObject();
			jsonObject.addProperty(JsonConstants.STATUS_PROPERTY, status.toString());
			jsonObject.addProperty(JsonConstants.RECEIPT_PROPERTY, "No receipt available");
			jsonObject.add(JsonConstants.TIMESTAMP_PROPERTY, new Timestamp().asJSON());
			writeJsonObject(pathname, jsonObject);
		} catch (final IOException | HederaClientException e) {
			logger.error(e.getMessage());
		}
	}
}
