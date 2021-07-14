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

import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.security.AddressChecksums;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.popups.CompleteKeysPopup;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.popups.ThreeButtonPopup;
import com.hedera.hashgraph.client.ui.popups.TwoButtonPopup;
import com.hedera.hashgraph.client.ui.utilities.AccountLineInformation;
import com.hedera.hashgraph.client.ui.utilities.ResponseEnum;
import com.hedera.hashgraph.client.ui.utilities.ResponseTuple;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.util.Callback;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.table.TableRowExpanderColumn;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TEXT_BOX_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.ACCOUNTS_FOLDER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.UNKNOWN_KEY_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.Messages.NICKNAME_IN_USE_MESSAGE;
import static com.hedera.hashgraph.client.core.utils.EncryptionUtils.info2Json;


public class AccountsPaneController implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(AccountsPaneController.class);
	public static final String PATH_NAME_EXTENSION = "%s%s.%s";
	public static final String ACCOUNTS = "Accounts";
	public static final String FORMAT_NAME_EXTENSION = "%s.%s";
	public static final String ARCHIVE = "Archive";
	public static final String DELETE_ACCOUNT_WARNING_MESSAGE =
			"This operation will delete the account from your application. Are you sure you would like delete this " +
					"account?";
	public static final String BOX_STYLE =
			"-fx-border-radius: 10; -fx-background-radius: 10; -fx-background-color: white; -fx-border-color: " +
					"lightgray";
	public static final String CONTINUE_LABEL = "CONTINUE";
	public static final String CANCEL_LABEL = "CANCEL";

	public StackPane accountsPane;
	public ScrollPane accountsScrollPane;
	public TextField hiddenPathAccount;
	public Button importAccountButton;
	public Button importFolderButton;


	@FXML
	private Controller controller;
	private final Map<String, String> accountInfos;    // is loaded from accountInfo.info
	private final Map<String, String> idNickNames;     // key: accountID string, value: nickName
	private final List<AccountLineInformation> accountLineInformation = new ArrayList<>();

	public AccountsPaneController() {
		accountInfos = new HashMap<>();
		idNickNames = new HashMap<>();
	}

	/***
	 *
	 */
	private void getAccountsFromFileSystem(Map<String, String> accountInfos,
			Map<String, String> idNickNames) throws HederaClientException {
		accountInfos.clear();

		idNickNames.clear();

		var nicknames =
				(new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		var accountFiles = new File(ACCOUNTS_INFO_FOLDER).listFiles((dir, name) -> name.endsWith(INFO_EXTENSION));

		if (accountFiles == null) {
			throw new HederaClientException(ACCOUNTS_FOLDER_ERROR_MESSAGE);
		}

		for (var accountFile : accountFiles) {
			var accountId = FilenameUtils.getBaseName(accountFile.getName());
			if (!nicknames.has(accountId)) {
				nicknames.addProperty(accountId, accountId);
			}
			idNickNames.put(accountId, nicknames.get(accountId).getAsString());
			accountInfos.put(accountId, accountFile.getAbsolutePath());
		}
		writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);
	}


	//region INITIALIZATION
	void injectMainController(Controller controller) {
		this.controller = controller;
	}

	void initializeAccountPane() {
		hiddenPathAccount.clear();
		try {
			getAccountsFromFileSystem(accountInfos, idNickNames);
			controller.setAccountInfoMap(accountInfos);
			updateAccountLineInformation();
		} catch (Exception exception) {
			logger.error(exception);
		}
		accountsScrollPane.setContent(setupAccountTable());
	}

	/**
	 * Builds the list of accounts that will be used to populate the table
	 */
	private void updateAccountLineInformation() throws InvalidProtocolBufferException, HederaClientException {
		var nicknames =
				(new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

		accountLineInformation.clear();
		for (Map.Entry<String, String> entry : accountInfos.entrySet()) {
			var info = AccountInfo.fromBytes(readBytes(entry.getValue()));
			var balance = info.balance;
			var line =
					new AccountLineInformation(nicknames.get(entry.getKey()).getAsString(),
							Identifier.parse(entry.getKey()),
							balance, isSigner(info));
			accountLineInformation.add(line);
		}
	}

	// endregion

	// region GETTERS
	public Map<String, String> getAccountInfos() {
		return accountInfos;
	}

	public Map<String, String> getIdNickNames() {
		return idNickNames;
	}
	// endregion

	// region TABLE SETUP

	/**
	 * Setups the table of accounts
	 *
	 * @return a table
	 */
	private TableView<AccountLineInformation> setupAccountTable() {
		Collections.sort(accountLineInformation);
		var table = new TableView<AccountLineInformation>();
		table.getItems().clear();

		TableColumn<AccountLineInformation, String> nicknameColumn = getNicknameColumn(table);

		TableColumn<AccountLineInformation, Identifier> accountIDColumn = getAccountIDColumn(table);

		TableColumn<AccountLineInformation, Hbar> balanceColumn = getBalanceColumn(table);

		TableColumn<AccountLineInformation, String> canSignColumn = getCanSignColumn(table);

		TableRowExpanderColumn<AccountLineInformation> expanderColumn = getExpanderColumn();

		TableColumn<AccountLineInformation, String> actionColumn = getActionColumn(table);

		table.getColumns().addAll(expanderColumn, nicknameColumn, accountIDColumn, balanceColumn, canSignColumn,
				actionColumn);
		table.getItems().addAll(accountLineInformation);

		if (accountLineInformation.size() == 1) {
			expanderColumn.toggleExpanded(0);
		}
		return table;
	}

	/**
	 * Setup the nickname column in the table
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, String> getNicknameColumn(TableView<AccountLineInformation> table) {
		var nicknameColumn = new TableColumn<AccountLineInformation, String>("Nickname");
		nicknameColumn.setCellValueFactory(new PropertyValueFactory<>("nickname"));
		nicknameColumn.prefWidthProperty().bind(table.widthProperty().divide(10).multiply(2));
		nicknameColumn.setStyle("-fx-alignment: TOP-LEFT; -fx-padding: 10");
		return nicknameColumn;
	}

	/**
	 * Setup the Account ID column
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, Identifier> getAccountIDColumn(TableView<AccountLineInformation> table) {
		var accountIDColumn = new TableColumn<AccountLineInformation, Identifier>("Account ID");
		accountIDColumn.setCellValueFactory(new PropertyValueFactory<>("account"));
		accountIDColumn.setCellFactory(tc -> new TableCell<>() {
			@Override
			protected void updateItem(Identifier accountID, boolean empty) {
				if (empty) {
					setText("");
				} else {
					var checksum = AddressChecksums.checksum(accountID.toReadableString());
					setText(String.format("%s-%s", accountID.toReadableString(), checksum));
				}
			}
		});
		accountIDColumn.prefWidthProperty().bind(table.widthProperty().divide(10).multiply(2));
		accountIDColumn.setStyle("-fx-alignment: TOP-LEFT; -fx-padding: 10");
		return accountIDColumn;
	}

	/**
	 * Setup the action column
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, String> getActionColumn(TableView<AccountLineInformation> table) {
		var actionColumn = new TableColumn<AccountLineInformation, String>("");
		actionColumn.setCellValueFactory(new PropertyValueFactory<>(""));
		Callback<TableColumn<AccountLineInformation, String>, TableCell<AccountLineInformation, String>> cellFactory =
				accountLineInformationStringTableColumn -> new TableCell<>() {
					final Button button = deleteButton();

					@Override
					public void updateItem(String item, boolean empty) {
						setText(null);
						if (!empty) {
							if (controller.getSetupPhase().equals(SetupPhase.TEST_PHASE)) {
								var x = table.getItems().get(getIndex());
								button.setText(x.getNickname() + "T");
								button.setStyle("-fx-font-size: 2");
							}
							button.setOnAction(actionEvent -> setAccountTableButtonAction());
							setGraphic(button);
							return;
						}
						setGraphic(null);
					}

					private void setAccountTableButtonAction() {
						var answer = PopupMessage.display("Warning", DELETE_ACCOUNT_WARNING_MESSAGE, true,
								CONTINUE_LABEL, CANCEL_LABEL);
						if (Boolean.TRUE.equals(answer)) {
							var accountLineInformation1 = getTableView().getItems().get(getIndex());
							logger.info("Deleting {}", accountLineInformation1.getNickname());
							deleteAccount(accountLineInformation1);
							table.getItems().remove(getIndex());
						}
					}
				};
		actionColumn.setCellFactory(cellFactory);
		return actionColumn;
	}

	/**
	 * Setup the expander column
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableRowExpanderColumn<AccountLineInformation> getExpanderColumn() {
		var expanderColumn = new TableRowExpanderColumn<>(this::buildAccountVBox);
		expanderColumn.setStyle("-fx-alignment: TOP-CENTER; -fx-padding: 10");
		return expanderColumn;
	}

	/**
	 * Setup the "Can Sign?" column
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, String> getCanSignColumn(TableView<AccountLineInformation> table) {
		var canSignColumn = new TableColumn<AccountLineInformation, String>("Can Sign?");
		canSignColumn.setCellValueFactory(new PropertyValueFactory<>("signer"));
		canSignColumn.prefWidthProperty().bind(table.widthProperty().divide(10));
		canSignColumn.setStyle("-fx-alignment: TOP-CENTER; -fx-padding: 10");
		return canSignColumn;
	}

	/**
	 * Setup the Balance column
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, Hbar> getBalanceColumn(TableView<AccountLineInformation> table) {
		var balanceColumn = new TableColumn<AccountLineInformation, Hbar>("Balance (At file creation)");
		balanceColumn.setCellValueFactory(new PropertyValueFactory<>("balance"));
		balanceColumn.setCellFactory(tc -> new TableCell<>() {
			@Override
			protected void updateItem(Hbar hBars, boolean empty) {
				if (empty) {
					setText("");
				} else {
					setFont(Font.font("Courier", 17));
					setText(hBars.toString());
				}
			}
		});

		balanceColumn.prefWidthProperty().bind(table.widthProperty().divide(10).multiply(4).subtract(24));
		balanceColumn.setStyle("-fx-alignment: TOP-RIGHT; -fx-padding: 10");
		return balanceColumn;
	}

	/**
	 * Setups a delete button
	 *
	 * @return a button with the delete.png icon
	 */
	private Button deleteButton() {
		var button = new Button();
		final var imageView = new ImageView(new Image("icons/delete.png"));
		imageView.setFitHeight(20);
		imageView.setPreserveRatio(true);
		button.setGraphic(imageView);
		button.setStyle("-fx-background-color: transparent; -fx-border-color: transparent");
		return button;
	}

	/**
	 * action to delete an account
	 *
	 * @param accountLineInformation
	 * 		the line from the table that must be deleted
	 */
	public void deleteAccount(AccountLineInformation accountLineInformation) {
		var accountID = accountLineInformation.getAccount().toReadableString();
		var directory = controller.getPreferredStorageDirectory();
		if (Paths.get(directory, ACCOUNTS, ARCHIVE).toFile().mkdirs()) {
			logger.info("Archive directory created");
		}

		var oldInfoPath =
				Paths.get(directory, ACCOUNTS, String.format(FORMAT_NAME_EXTENSION, accountID, INFO_EXTENSION));
		var oldJsonPath =
				Paths.get(directory, ACCOUNTS, String.format(FORMAT_NAME_EXTENSION, accountID, JSON_EXTENSION));

		var newInfoPath = findFileName(Paths.get(directory, ACCOUNTS, ARCHIVE), accountID, INFO_EXTENSION);
		var newJsonPath = findFileName(Paths.get(directory, ACCOUNTS, ARCHIVE), accountID, JSON_EXTENSION);

		// move the folder to the deleted accounts folder
		try {
			Files.move(oldInfoPath, newInfoPath, StandardCopyOption.REPLACE_EXISTING);
			Files.move(oldJsonPath, newJsonPath, StandardCopyOption.REPLACE_EXISTING);
		} catch (IOException e) {
			controller.logAndDisplayError(e);
		}

		// update nickname store
		JsonObject nicknames;
		try {
			nicknames = (new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			if (nicknames.has(accountID)) {
				nicknames.remove(accountID);
				writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);
			}
		} catch (HederaClientException e) {
			controller.logAndDisplayError(e);
		}
		// Delete the account info from the History of accepted files
		var historyInfo = new File(controller.getPreferredStorageDirectory() + "/History").listFiles(
				(dir, name) -> name.contains(accountID));

		assert historyInfo != null;
		if (historyInfo.length > 0) {
			for (var file : historyInfo) {
				try {
					Files.deleteIfExists(file.toPath());
					logger.info("File {} deleted", file.getAbsolutePath());
				} catch (IOException e) {
					logger.error("File {} cannot be deleted", file.getAbsolutePath());
				}
			}
		}
		logger.info("Account {} has been deleted", accountID);
		controller.createPaneController.initializeCreatePane();
	}

	/**
	 * Finds out if any of the pem files in the keys folder can be used to sign for this account
	 *
	 * @param accountInfo
	 * 		the proto that contains the account information
	 * @return true if any of the private keys in the app corresponds to one of the public keys in the account
	 */
	private boolean isSigner(AccountInfo accountInfo) {
		var flatKey =
				EncryptionUtils.flatPubKeys(Collections.singletonList(accountInfo.key));
		List<String> knownKeys = new ArrayList<>();
		for (var key : flatKey) {
			var keyName = controller.showKeyString(key);

			if (keyName.endsWith(PUB_EXTENSION)) {
				knownKeys.add(keyName);
			}
		}
		if (knownKeys.isEmpty()) {
			return false;
		}
		for (var knownKey : knownKeys) {
			if (new File(controller.getPreferredStorageDirectory() + "/Keys/" + knownKey.replace(PUB_EXTENSION,
					PK_EXTENSION)).exists()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * builds an HBox with details about the account. It also provides an action to change the nickname of the account
	 *
	 * @return an HBox node
	 */
	private VBox buildAccountVBox(TableRowExpanderColumn.TableRowDataFeatures<AccountLineInformation> parameter) {
		var lineInformation = parameter.getValue();
		var allBoxes = new VBox();

		allBoxes.setStyle(BOX_STYLE);
		allBoxes.setPadding(new Insets(10));
		allBoxes.setSpacing(10);
		try {
			var returnBox = new HBox();
			var changeNicknameButton = setupWhiteButton("CHANGE");
			var acceptNicknameButton = setupWhiteButton("ACCEPT");
			acceptNicknameButton.setVisible(false);

			var region = setupFillerRegion();

			var nickname = new TextField(lineInformation.getNickname());
			nickname.setStyle(TEXT_BOX_STYLE);
			nickname.setPrefWidth(300);
			nickname.editableProperty().bind(acceptNicknameButton.visibleProperty());
			nickname.setFocusTraversable(false);

			changeNicknameButton.setOnAction(actionEvent -> {
				changeNicknameButton.setVisible(false);
				acceptNicknameButton.setVisible(true);
				nickname.requestFocus();
			});

			acceptNicknameButton.setOnAction(
					actionEvent -> updateNickname(parameter, lineInformation, changeNicknameButton,
							acceptNicknameButton,
							nickname));
			nickname.setOnKeyReleased(
					keyEvent -> {
						if (KeyCode.ENTER.equals(keyEvent.getCode()) && nickname.isEditable()) {
							updateNickname(parameter, lineInformation, changeNicknameButton, acceptNicknameButton,
									nickname);
						}
					});

			var nickNameBox = new HBox();
			nickNameBox.getChildren().addAll(new Label("Nickname"), region, nickname);
			nickNameBox.setSpacing(10);
			nickNameBox.setAlignment(Pos.CENTER_LEFT);

			final var filePath = accountInfos.get(lineInformation.getAccount().toReadableString());
			var info = AccountInfo.fromBytes(readBytes(filePath));

			var creationTime = getFileDateString(filePath);

			var keyTreeView = controller.buildKeyTreeView(info.key);
			double height = 28;
			keyTreeView.setPrefHeight((keyTreeView.expandedItemCountProperty().get() + 1) * height);

			keyTreeView.setStyle("-fx-border-color: white");

			keyTreeView.setOnMouseClicked(
					mouseEvent -> displayCompleteKeysPopup(keyTreeView.getSelectionModel().getSelectedItem(),
							mouseEvent));

			var keysVBox = new VBox();

			keysVBox.getChildren().add(keyTreeView);

			var leftVBox = new VBox();
			leftVBox.setSpacing(10);
			final var account = new Identifier(info.accountId).toReadableString();

			leftVBox.getChildren().addAll(nickNameBox,
					setupTextField("Account ID", String.format("%s (%s)", account, AddressChecksums.checksum(account))),
					setupTextField(String.format("Balance (as of %s)", creationTime), info.balance.toString()),
					setupTextField("Auto Renew Period", info.autoRenewPeriod.getSeconds() + " s"),
					setupTextField("Expiration Date", getExpirationTimeString(info)),
					setupTextField("Receiver Signature Required", String.valueOf(info.isReceiverSignatureRequired)
					));

			HBox.setHgrow(leftVBox, Priority.ALWAYS);
			HBox.setHgrow(keysVBox, Priority.ALWAYS);
			VBox.setVgrow(keysVBox, Priority.ALWAYS);
			leftVBox.setMaxWidth(700);

			var rightVBox = new VBox();
			var buttons = new HBox();
			buttons.getChildren().addAll(changeNicknameButton, acceptNicknameButton);
			rightVBox.getChildren().addAll(buttons);
			rightVBox.setSpacing(10);
			rightVBox.setAlignment(Pos.TOP_LEFT);
			keysVBox.prefWidthProperty().bind(returnBox.widthProperty().divide(2).subtract(50));

			returnBox.getChildren().addAll(leftVBox, rightVBox);
			returnBox.setSpacing(20);

			allBoxes.setPrefHeight(Region.USE_COMPUTED_SIZE);
			allBoxes.setPrefWidth(Region.USE_COMPUTED_SIZE);
			allBoxes.getChildren().addAll(returnBox, new Label("Key"), keyTreeView);

		} catch (Exception e) {
			logger.error(e);
		}
		return allBoxes;
	}

	/**
	 * Updates the nickname of an account
	 *
	 * @param parameter
	 * 		the column parameter
	 * @param lineInformation
	 * 		the data for the account
	 * @param changeNicknameButton
	 * 		the button to change the nickname
	 * @param acceptNicknameButton
	 * 		the button to accept the nickname
	 * @param nickname
	 * 		the nickname text field
	 */
	private void updateNickname(TableRowExpanderColumn.TableRowDataFeatures<AccountLineInformation> parameter,
			AccountLineInformation lineInformation, Button changeNicknameButton, Button acceptNicknameButton,
			TextField nickname) {
		changeNicknameButton.setVisible(true);
		acceptNicknameButton.setVisible(false);
		final var newNickname = nickname.getText();

		if (lineInformation.getNickname().equals(newNickname)) {
			return;
		}
		if (idNickNames.containsValue(newNickname)) {
			PopupMessage.display("Duplicate nickname", String.format(NICKNAME_IN_USE_MESSAGE, newNickname),
					CONTINUE_LABEL);
			nickname.setText(lineInformation.getNickname());
			return;
		}
		parameter.toggleExpanded();
		lineInformation.setNickname(newNickname);
		changeNickname(newNickname, lineInformation.getAccount().toReadableString());
	}

	/**
	 * if the user double clicks on a tree item display the keys popup
	 *
	 * @param item
	 * 		the selected item in the tree
	 * @param mouseEvent
	 * 		the event that triggered the method
	 */
	private void displayCompleteKeysPopup(TreeItem<String> item, MouseEvent mouseEvent) {
		if (mouseEvent.getClickCount() != 2) {
			return;
		}
		if (item == null) {
			return;
		}
		if (!item.getValue().endsWith(PUB_EXTENSION)) {
			PopupMessage.display("Unknown key", UNKNOWN_KEY_ERROR_MESSAGE);
			return;
		}
		final Boolean displayPopup =
				CompleteKeysPopup.display(controller.keysPaneController.getPublicKeysMap().get(item.getValue()),
						true);
		if (displayPopup.equals(true)) {
			controller.keysPaneController.initializeKeysPane();
		}
	}

	private String getFileDateString(String filePath) throws IOException {
		var attr = Files.readAttributes(new File(filePath).toPath(), BasicFileAttributes.class);
		var ldt = LocalDateTime.ofInstant(attr.creationTime().toInstant(), ZoneOffset.UTC);
		var transactionValidStart = Date.from(ldt.atZone(ZoneId.of("UTC")).toInstant());
		var localDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		var tz = TimeZone.getDefault();
		return localDateFormat.format(transactionValidStart) + " " + tz.getDisplayName(true, TimeZone.SHORT);
	}

	private Region setupFillerRegion() {
		var region = new Region();
		region.setPrefWidth(Region.USE_COMPUTED_SIZE);
		region.setPrefHeight(Region.USE_COMPUTED_SIZE);
		HBox.setHgrow(region, Priority.ALWAYS);
		return region;
	}

	private Button setupWhiteButton(String change) {
		var changeNicknameButton = new Button(change);
		changeNicknameButton.setStyle(WHITE_BUTTON_STYLE);
		changeNicknameButton.setMinWidth(150);
		changeNicknameButton.managedProperty().bind(changeNicknameButton.visibleProperty());
		return changeNicknameButton;
	}

	/**
	 * Change the nickname assigned to an account
	 *
	 * @param newNickname
	 * 		new nickname
	 * @param account
	 * 		account id string
	 */
	private void changeNickname(String newNickname, String account) {
		try {
			var nicknames =
					(new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			if (nicknames.has(account)) {
				nicknames.addProperty(account, newNickname);
			}
			writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);
			getAccountsFromFileSystem(accountInfos, idNickNames);
			controller.setAccountInfoMap(accountInfos);
		} catch (HederaClientException exception) {
			logger.error(exception);
		}

	}

	/**
	 * Builds an HBox with a label and a formatted text field
	 *
	 * @param field
	 * 		explanatory text in the label
	 * @param text
	 * 		text field text
	 * @return an HBox
	 */
	private HBox setupTextField(String field, String text) {
		var hBox = new HBox();
		var fieldLabel = new Label(field);
		fieldLabel.setWrapText(true);
		var textField = new TextField(text);
		textField.setPrefWidth(300);
		textField.setMinWidth(300);
		textField.setMaxWidth(300);
		textField.setEditable(false);
		textField.setFocusTraversable(false);
		textField.setStyle(
				"-fx-background-color: white; -fx-border-color: #c2c2c2; -fx-background-radius: 10; " +
						"-fx-border-radius: 10");


		var region = setupFillerRegion();
		hBox.getChildren().addAll(fieldLabel, region, textField);
		hBox.setAlignment(Pos.CENTER_LEFT);
		return hBox;
	}

	/**
	 * Returns a human-readable string that represents the expiration date of the account
	 *
	 * @param accountInfo
	 * 		the proto that contains the account information
	 * @return A human readable date, or "never" if the account doesn't expire (system accounts)
	 */
	private String getExpirationTimeString(AccountInfo accountInfo) {
		var d = new Date(accountInfo.expirationTime.getEpochSecond() * 1000);
		var cal = Calendar.getInstance();
		cal.add(Calendar.YEAR, 100);
		var future = cal.getTime();
		cal.add(Calendar.YEAR, -102);
		var past = cal.getTime();

		return (d.after(future) || d.before(past)) ? "Never" : Utilities.timestampToString(
				new Timestamp(accountInfo.expirationTime));
	}

	// endregion

	// region IMPORT ACCOUNT


	/**
	 * Import accounts from a folder/folders
	 */
	public void importAccountFromFolder() throws HederaClientException {
		controller.setThisPane(accountsPane);
		File folder;
		if (!hiddenPathAccount.getText().isEmpty()) {
			folder = new File(hiddenPathAccount.getText());
			hiddenPathAccount.clear();
		} else {
			var directory = controller.getLastTransactionsDirectory();
			if (directory == null) {
				return;
			}
			folder = new File(BrowserUtilities.browseDirectories(directory, controller.getThisPane()));
			controller.setLastTransactionsDirectory(folder);
		}

		var files = folder.listFiles((dir, name) -> name.endsWith(INFO_EXTENSION));
		if (files == null) {
			return;
		}

		if (files.length == 0) {
			PopupMessage.display("No info files",
					"The application could not detect any account files. Please select another directory.");
			return;
		}

		importInfoFiles(Arrays.asList(files));

	}

	/**
	 * Import account info from either a .info file
	 * 1. for .info file, the user should set this account's nick name; corresponding file directory would be created;
	 * account.json and accountInfo.info would be saved in corresponding directory;
	 *
	 * @throws HederaClientException
	 * 		if importAccountFromFile throws an exception
	 */
	public void importAccountFromFile() throws HederaClientException {

		// browse to file
		controller.setThisPane(accountsPane);
		List<File> files = new ArrayList<>();
		if (!hiddenPathAccount.getText().isEmpty()) {
			// for testing purposes only
			files.add(new File(hiddenPathAccount.getText()));
			hiddenPathAccount.clear();
			importInfoFiles(files);
			return;
		}

		var directory = controller.getLastTransactionsDirectory();
		if (directory == null) {
			return;
		}
		files = BrowserUtilities.browseMultiFiles(directory, controller.getThisPane(), "Info", "info");
		if (files == null) {
			return;
		}
		controller.setLastTransactionsDirectory(files.get(0));
		importInfoFiles(files);


	}

	/**
	 * Imports one or more account info files to the app
	 *
	 * @param files
	 * 		list of files
	 */
	public void importInfoFiles(List<File> files) throws HederaClientException {
		List<File> duplicates = new ArrayList<>();
		List<File> newFiles = new ArrayList<>();
		Set<String> nicknames = new HashSet<>(idNickNames.values());

		// separate list into new files and previously imported accounts
		for (var file : files) {
			if (inAccountMaps(getAccountIDFromInfoFile(file))) {
				duplicates.add(file);
			} else {
				newFiles.add(file);
			}
		}

		// First deal with the duplicates
		var counter = handleDuplicateFiles(duplicates);

		// Now we must deal with the new files
		counter += handleNewFiles(newFiles, nicknames);

		// Only refresh the panes if the accounts have changed
		if (counter > 0) {
			refreshPanes();
		}
	}

	/**
	 * Handle duplicate files
	 *
	 * @param duplicates
	 * 		a list of duplicates
	 * @return the number of accounts changes
	 */
	private int handleDuplicateFiles(List<File> duplicates) throws HederaClientException {
		int counter = 0;
		if (!duplicates.isEmpty()) {
			var keepAsking = true;
			var responseEnum = ResponseEnum.UNKNOWN;
			for (var file : duplicates) {
				if (keepAsking) {
					responseEnum = ThreeButtonPopup.display(file,
							"Account %s has already been imported.\nDo you want to replace the existing account " +
									"information?",
							false, duplicates.size() > 1);
				}
				switch (Objects.requireNonNull(responseEnum)) {
					case IGNORE_ONCE:
						keepAsking = true;
						break;
					case IGNORE_ALWAYS:
						keepAsking = false;
						break;
					case REPLACE_ONCE:
						replaceInfo(file);
						counter++;
						keepAsking = true;
						break;
					case REPLACE_ALWAYS:
						replaceInfo(file);
						counter++;
						keepAsking = false;
						break;
					case UNKNOWN:
						break;
					default:
						throw new IllegalStateException("Unexpected value: " + Objects.requireNonNull(responseEnum));
				}
			}
		}
		return counter;
	}

	/**
	 * Handle the new Accounts
	 *
	 * @param newFiles
	 * 		list of new files
	 * @param nicknames
	 * 		list of nicknames
	 * @return the number of accounts accepted
	 */
	private int handleNewFiles(List<File> newFiles, Set<String> nicknames) throws HederaClientException {
		int counter = 0;
		if (!newFiles.isEmpty()) {
			var responseEnum = ResponseEnum.UNKNOWN;
			var keepAsking = true;
			for (var file : newFiles) {
				var responseTuple = new ResponseTuple();
				if (keepAsking) {
					while (responseTuple.getNickname().equals("")) {
						responseTuple =
								TwoButtonPopup.display(FilenameUtils.getBaseName(file.getName()), newFiles.size() > 1);
						if (nicknames.contains(responseTuple.getNickname())) {
							PopupMessage.display("Duplicate nickname",
									String.format(NICKNAME_IN_USE_MESSAGE, responseTuple.getNickname()),
									CONTINUE_LABEL);
							responseTuple.setNickname("");
						}
					}
					responseEnum = responseTuple.getResponseEnum();
					nicknames.add(responseTuple.getNickname());
				}
				switch (responseEnum) {
					case IGNORE_ONCE:
						keepAsking = true;
						break;
					case IGNORE_ALWAYS:
						keepAsking = false;
						break;
					case REPLACE_ONCE:
						importFromFile(file, responseTuple.getNickname());
						counter++;
						keepAsking = true;
						break;
					case REPLACE_ALWAYS:
						importFromFile(file, FilenameUtils.getBaseName(file.getName()));
						counter++;
						keepAsking = false;
						break;
					case KEEP_BOTH_ONCE:
					case KEEP_BOTH_ALWAYS:
					case UNKNOWN:
				}
			}
		}
		return counter;
	}

	/**
	 * If accounts have been imported or changed, refresh panes
	 *
	 * @throws HederaClientException
	 * 		if there is an InvalidProtocolException thrown
	 */
	private void refreshPanes() throws HederaClientException {
		initializeAccountPane();
		controller.keysPaneController.initializeKeysPane();
		controller.homePaneController.setForceUpdate(true);
		controller.homePaneController.initializeHomePane();
		getAccountsFromFileSystem(accountInfos, idNickNames);
		controller.setAccountInfoMap(accountInfos);
		try {
			updateAccountLineInformation();
		} catch (InvalidProtocolBufferException e) {
			controller.logAndDisplayError(e);
			throw new HederaClientException(e);
		}
	}

	/**
	 * Replaces an existing account info with the one contained in the file
	 *
	 * @param file
	 * 		a file containing an account info
	 */
	private void replaceInfo(File file) throws HederaClientException {
		var identifier = getAccountIDFromInfoFile(file).toReadableString();
		var newPath =
				new File(String.format(PATH_NAME_EXTENSION, ACCOUNTS_INFO_FOLDER, identifier, INFO_EXTENSION)).toPath();
		try {
			Files.deleteIfExists(newPath);
			Files.copy(file.toPath(), newPath);
		} catch (IOException e) {
			throw new HederaClientException(e);
		}
		controller.createPaneController.initializeCreatePane();
	}

	/**
	 * Given a file containing an account info, returns the account id of the file
	 *
	 * @param file
	 * 		where information is stored
	 * @return the account id
	 */
	private Identifier getAccountIDFromInfoFile(File file) throws HederaClientException {
		if (file == null) {
			throw new HederaClientRuntimeException("Null file");
		}
		if (!file.exists()) {
			throw new HederaClientRuntimeException(String.format("File %s cannot be found", file.getName()));
		}
		if (!file.isFile()) {
			throw new HederaClientRuntimeException("Must be a file, not a directory");
		}
		if (!file.getName().endsWith(INFO_EXTENSION)) {
			throw new HederaClientException(String.format("%s is not an info file", file.getAbsolutePath()));
		}
		try {
			var info = AccountInfo.fromBytes(readBytes(file.getAbsolutePath()));
			return new Identifier(info.accountId);
		} catch (InvalidProtocolBufferException e) {
			controller.logAndDisplayError(e);
			throw new HederaClientException(e);
		}

	}

	/**
	 * If the account has been previously imported, the folder should exist.
	 *
	 * @param account
	 * 		an account id
	 * @return true if the account has been previously imported.
	 */
	private boolean inAccountMaps(Identifier account) {
		final var file = new File(
				String.format("%s/Accounts/%s.%s", controller.getPreferredStorageDirectory(),
						account.toReadableString(), INFO_EXTENSION));
		return file.exists() && !file.isDirectory();
	}

	/**
	 * Imports an account from a file, creates directory structure and the required files
	 *
	 * @param file
	 * 		the info file to import
	 * @param nickname
	 * 		the nickname assigned to the file
	 */
	private void importFromFile(File file, String nickname) throws HederaClientException {
		var newAccountID = getAccountIDFromInfoFile(file).toReadableString();
		var account = new JsonObject();
		account.addProperty("accountID", newAccountID);
		account.addProperty("nickname", nickname);

		idNickNames.put(newAccountID, nickname);
		accountInfos.put(nickname,
				String.format(PATH_NAME_EXTENSION, ACCOUNTS_INFO_FOLDER, newAccountID, INFO_EXTENSION));
		storeAccount(nickname, file.getAbsolutePath());
		controller.createPaneController.initializeCreatePane();
	}


	/**
	 *
	 */
	private void storeAccount(String nickname, String infoFile) {
		try {
			logger.info("Importing account");

			var info = AccountInfo.fromBytes(readBytes(infoFile));
			var accountId = new Identifier(info.accountId).toReadableString();

			// Update the list of nicknames.
			var nicknames =
					(new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

			nicknames.addProperty(accountId, nickname);
			writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);

			// Store the info file in its new location
			var newInfoName = String.format(PATH_NAME_EXTENSION, ACCOUNTS_INFO_FOLDER, accountId, INFO_EXTENSION);
			logger.info("Storing account details to: {}", newInfoName);
			Files.deleteIfExists(new File(newInfoName).toPath());
			writeBytes(newInfoName, info.toBytes());

			var infoJson = info2Json(info);
			writeJsonObject(newInfoName.replace(INFO_EXTENSION, JSON_EXTENSION), infoJson);

		} catch (IOException | HederaClientException ex) {
			logger.error("Unable to store AccountInfo.", ex);
		}
	}

	// endregion

	/**
	 * Testing only: if a the enter key is pressed in the hidden field, the address is accepted as the new account info
	 * file
	 *
	 * @param keyEvent
	 * 		the triggering key event
	 */
	public void choosePath(KeyEvent keyEvent) throws HederaClientException {
		if ((KeyCode.ENTER).equals(keyEvent.getCode())) {
			var infoPath = (hiddenPathAccount.getText()).replace(" ", "");
			if (infoPath.endsWith(".info") && (new File(infoPath)).exists()) {
				importAccountFromFile();
			}
		}
	}

}
