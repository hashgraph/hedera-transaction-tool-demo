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
import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.queries.AccountInfoQuery;
import com.hedera.hashgraph.client.core.queries.BalanceQuery;
import com.hedera.hashgraph.client.core.remote.InfoFile;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.security.AddressChecksums;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.popups.AccountHistoryPopup;
import com.hedera.hashgraph.client.ui.popups.CompleteKeysPopup;
import com.hedera.hashgraph.client.ui.popups.ExtraKeysSelectorPopup;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.popups.ProgressPopup;
import com.hedera.hashgraph.client.ui.popups.TwoButtonPopup;
import com.hedera.hashgraph.client.ui.utilities.AccountLineInformation;
import com.hedera.hashgraph.client.ui.utilities.KeyPairUtility;
import com.hedera.hashgraph.client.ui.utilities.ResponseEnum;
import com.hedera.hashgraph.client.ui.utilities.ResponseTuple;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.LedgerId;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.proto.AccountID;
import com.hedera.hashgraph.sdk.proto.CryptoGetInfoResponse;

import javafx.beans.Observable;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.ChoiceDialog;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Separator;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeItem;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.Stage;
import javafx.util.Callback;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.appender.rolling.FileExtension;
import org.controlsfx.control.table.TableRowExpanderColumn;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.BALANCES_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.ZIP_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.ACCOUNTS_FOLDER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.FEE_PAYER_NOT_SET_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.UNKNOWN_KEY_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.Messages.NICKNAME_IN_USE_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.ACCOUNTS_TO_QUERY_TOOLTIP_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.FEE_PAYER_TOOLTIP_MESSAGES;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.NETWORKS_TOOLTIP_MESSAGES;
import static com.hedera.hashgraph.client.core.utils.EncryptionUtils.info2Json;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.getKeysFromInfo;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.instantToLocalTimeDate;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.parseAccountNumbers;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.timestampToString;
import static java.lang.String.format;
import static java.lang.String.valueOf;
import static java.lang.Thread.sleep;


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
	public static final String FX_TEXT_FILL_BLACK = "-fx-text-fill: black; -fx-background-color: white";
	public static final String ACCOUNT_PROPERTY = "account";
	public static final String BALANCE_PROPERTY = "balance";
	public static final String DATE_PROPERTY = "date";
	public static final String TIMEOUR_ERROR_MESSAGE =
			"The operation failed due to timeout. Please try again later or contact the network " +
					"administrator.";
	public static final String NO_ACCOUNTS_SELECTED_TITLE = "No accounts selected";
	public static final String ERROR_TITLE = "Error";
	public static final String UNKNOWN_NETWORK_STRING = "UNKNOWN";
	public static final String COLUMN_STYLE_STRING = "-fx-alignment: TOP-CENTER; -fx-padding: 10";


	@FXML
	private Controller controller;

	public StackPane accountsPane;
	public ScrollPane accountsScrollPane;
	public TextField hiddenPathAccount;
	public Button importAccountButton;
	public Button importFolderButton;
	public Button setUnknownAccountNetworksButton;
	public TextField accountsToUpdateTextField;
	public Button selectAccountsButton;
	public ChoiceBox<Object> feePayerChoiceBoxA;
	public ChoiceBox<Object> networkChoiceBoxA;
	public Button accountsTooltip;
	public Button networkTooltipA;
	public Button feePayerTooltipA;

	public TitledPane addAccountsTitledPane;
	public Button addCustomPayerButton;
	public Button addCustomPayerButton1;
	public TextField feePayerTextFieldA;

	private final Map<String, String> accountInfos;    // is loaded from accountInfo.info
	private final Map<String, String> idNickNames;     // key: accountID string, value: nickName
	private final ObservableList<AccountLineInformation> accountLineInformation = FXCollections.observableArrayList(
			information -> new Observable[] { information.selectedProperty() });
	private final JsonObject balances = new JsonObject();
	private boolean noise = false;
	private final CheckBox selectAll = new CheckBox();
	private final Set<PublicKey> keys = new HashSet<>();
	private final Set<Identifier> feePayers = new HashSet<>();

	public AccountsPaneController() {
		accountInfos = new HashMap<>();
		idNickNames = new HashMap<>();
	}

	public Set<Identifier> getFeePayers() {
		return feePayers;
	}

	//region INITIALIZATION
	void injectMainController(final Controller controller) {
		this.controller = controller;
	}

	void initializeAccountPane() {
		hiddenPathAccount.clear();

		fixAccountFiles();
		setupAccountMaps();

		accountsScrollPane.setContent(setupAccountTable());
		accountLineInformation.addListener((ListChangeListener<AccountLineInformation>) change -> {
			if (!noise) {
				selectAll.setSelected(false);
			}

			setUnknownAccountNetworksButton.setVisible(accountLineInformation.stream()
					.filter(i -> i.isSelected() && UNKNOWN_NETWORK_STRING.equals(i.getLedgerId())).anyMatch(i -> true));
			setUnknownAccountNetworksButton.setManaged(setUnknownAccountNetworksButton.isVisible());
		});
		selectAccountsButton.setOnAction(event -> selectAccountsButtonAction());

		setupTooltips();
		setupFeePayers();
		setupNetworkBox(networkChoiceBoxA);
		setupFeePayerChoiceBox();
		setupInfoRequestFields();
	}

	public void setupAccountMaps() {
		try {
			getAccountsFromFileSystem(accountInfos, idNickNames);
			getBalancesFromFileSystem();
			controller.setAccountInfoMap(accountInfos);
			updateAccountLineInformation();
		} catch (final Exception exception) {
			logger.error(exception);
		}
	}

	private void fixAccountFiles() {

		try {
			JsonObject nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

			final var noNetworkFiles = new File(ACCOUNTS_INFO_FOLDER).listFiles(
					(dir, name) -> !name.contains("-") &&
							(INFO_EXTENSION.equals(FilenameUtils.getExtension(name))));

			if (noNetworkFiles != null) {
				for (final var file : noNetworkFiles) {
					try {
						if (!nicknames.has(FilenameUtils.getBaseName(file.getName()))) {
							continue;
						}
						final var name = FilenameUtils.getBaseName(file.getName());
						final var id = Identifier.parse(name, UNKNOWN_NETWORK_STRING);
						final var oldPath = file.getAbsolutePath();
						final var newPath = file.getAbsolutePath().replace(name, id.toReadableAccountAndNetwork());
						Files.move(Path.of(oldPath), Path.of(newPath));
						Files.move(Path.of(oldPath.replace(INFO_EXTENSION, JSON_EXTENSION)),
								Path.of(newPath.replace(INFO_EXTENSION, JSON_EXTENSION)));
						if (nicknames.has(name)) {
							final var n = nicknames.get(name);
							nicknames.remove(name);
							nicknames.add(id.toReadableAccountAndNetwork(), n);
						}
					} catch (Exception e) {
						logger.error("failed to set network on " + file.getAbsolutePath(), e);
					}
				}
				writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);
			}

			final var otherInfoFiles = new File(ACCOUNTS_INFO_FOLDER).listFiles(
					(dir, name) -> name.contains("-") &&
							(INFO_EXTENSION.equals(FilenameUtils.getExtension(name))));

			Set<AccountID> accountsWithLedgers = new HashSet<>();
			Map<File, AccountID> unknownFiles = new HashMap<>();

			if (otherInfoFiles != null) {
				for (final var file : otherInfoFiles) {
					try {
						String baseName = FilenameUtils.getBaseName(file.getName());
						if (!nicknames.has(baseName)) {
							continue;
						}
						String network = baseName.substring(baseName.lastIndexOf("-") + 1);

						var ledger = NetworkEnum.asLedger(network);
						var curInfo =
								CryptoGetInfoResponse.AccountInfo.parseFrom(readBytes(file.getAbsolutePath()));

						if (UNKNOWN_NETWORK_STRING.equalsIgnoreCase(network)) {
							unknownFiles.put(file, curInfo.getAccountID());
							continue;
						}

						var ledgerStr = ByteString.copyFrom(ledger.toBytes());

						if (!ledgerStr.equals(curInfo.getLedgerId())) {

							logger.info("fixing ledger id in {}, it was {}, it is now {}", file.getAbsolutePath(),
									Arrays.toString(curInfo.getLedgerId().toByteArray()), Arrays.toString(ledgerStr.toByteArray()));

							var newInfo = CryptoGetInfoResponse.AccountInfo
									.newBuilder(curInfo)
									.setLedgerId(ledgerStr)
									.build();
							FileUtils.writeByteArrayToFile(file, newInfo.toByteArray());
						}

						accountsWithLedgers.add(curInfo.getAccountID());

					} catch (Exception e) {
						logger.error("failed to fix ledgerId on " + file.getAbsolutePath(), e);
					}
				}
			}

			for (final var file : unknownFiles.keySet()) {
				if (accountsWithLedgers.contains(unknownFiles.get(file))) {
					var json = new File(file.getParentFile(),
							FilenameUtils.removeExtension(file.getName()) + "." + JSON_EXTENSION);

					logger.info("deleting files {} and {} because an account with the same ID has a network associated with it.",
							file.getAbsolutePath(), json.getAbsolutePath());

					if (!file.delete()) {
						logger.error("failed to delete {}", file.getAbsolutePath());
					}

					if (!json.delete()) {
						logger.error("failed to delete {}", json.getAbsolutePath());
					}
				}
			}
		} catch (final Exception e) {
			logger.error("fixAccountFiles failed", e);
		}
	}

	private void setupInfoRequestFields() {
		feePayerTextFieldA.managedProperty().bind(feePayerTextFieldA.visibleProperty());
		addCustomPayerButton.managedProperty().bind(addCustomPayerButton.visibleProperty());
		addCustomPayerButton1.managedProperty().bind(addCustomPayerButton1.visibleProperty());
		feePayerChoiceBoxA.managedProperty().bind(feePayerChoiceBoxA.visibleProperty());
		addCustomPayerButton1.visibleProperty().bind(addCustomPayerButton.visibleProperty().not());
		feePayerChoiceBoxA.visibleProperty().bind(feePayerTextFieldA.visibleProperty().not());
		addCustomPayerButton.visibleProperty().bind(feePayerTextFieldA.visibleProperty().not());
		feePayerTextFieldA.focusedProperty().addListener((observableValue, aBoolean, t1) -> addCustomFeePayer(t1));
		feePayerTextFieldA.setOnKeyPressed(event -> {
			final var code = event.getCode();
			if (code.equals(KeyCode.ENTER) || code.equals(KeyCode.TAB)) {
				feePayerChoiceBoxA.getParent().requestFocus();
			}
		});
	}

	private void selectAccountsButtonAction() {
		try {
			final var feePayerSelection = feePayerChoiceBoxA.getSelectionModel().getSelectedItem();
			if (!(feePayerSelection instanceof String)) {
				return;
			}
			final var feePayerString = (String) feePayerSelection;
			final var feePayer = "".equals(feePayerString) ?
					getFeePayer() :
					Identifier.parse(feePayerString, controller.getCurrentNetwork().toUpperCase(Locale.ROOT));

			if (feePayer == null) {
				return;
			}
			feePayer.setNetworkName(networkChoiceBoxA.getValue().toString());

			final var network = networkChoiceBoxA.getValue();
			if (!(network instanceof String)) {
				return;
			}
			final var accounts = parseAccountNumbers(accountsToUpdateTextField.getText(), (String) network);
			if (accounts.isEmpty()) {
				PopupMessage.display(NO_ACCOUNTS_SELECTED_TITLE,
						"The \"Accounts\" field is either empty or no valid accounts could be parsed.");
				return;
			}

			final var keyFiles = getKeyFiles(feePayer);
			if (keyFiles.isEmpty()) {
				PopupMessage.display(ERROR_TITLE,
						"At least one key must be selected in order to sign the transaction");
				return;
			}

			if (!(networkChoiceBoxA.getValue() instanceof String)) {
				return;
			}
			getInfosFromNetwork(accounts, feePayer, keyFiles);
		} catch (final HederaClientException | InvalidProtocolBufferException e) {
			logger.error(e.getMessage());
		}
	}

	private void setupTooltips() {
		accountsTooltip.setOnAction(actionEvent -> CommonMethods.showTooltip(controller.accountsPane, accountsTooltip,
				ACCOUNTS_TO_QUERY_TOOLTIP_MESSAGE));
		feePayerTooltipA.setOnAction(actionEvent -> CommonMethods.showTooltip(controller.accountsPane, feePayerTooltipA,
				FEE_PAYER_TOOLTIP_MESSAGES));
		networkTooltipA.setOnAction(actionEvent -> CommonMethods.showTooltip(controller.accountsPane, networkTooltipA,
				NETWORKS_TOOLTIP_MESSAGES));
	}

	private Set<File> getKeyFiles(
			final Identifier feePayer) throws HederaClientException, InvalidProtocolBufferException {
		final var filename = feePayer.toReadableString() + "-" + feePayer.getNetworkName() + "." + INFO_EXTENSION;
		final var accountInfo = new File(ACCOUNTS_INFO_FOLDER, filename);
		final Set<File> returnSet = new HashSet<>();
		if (accountInfo.exists()) {
			final var fullKey = AccountInfo.fromBytes(readBytes(accountInfo.getAbsolutePath())).key;
			returnSet.addAll(getPrivateKeysFrom(fullKey));
		}
		if (returnSet.isEmpty()) {
			return new HashSet<>(ExtraKeysSelectorPopup.display(new HashSet<>()));
		}
		return returnSet;
	}

	private Identifier getFeePayer() {
		final var defaultFeePayer = controller.getDefaultFeePayer();
		if (Identifier.ZERO.equals(defaultFeePayer)) {
			PopupMessage.display("Fee payer not set", FEE_PAYER_NOT_SET_ERROR_MESSAGE);
			return null;
		}
		return defaultFeePayer;
	}

	private void getInfosFromNetwork(final List<AccountId> accounts, final Identifier feePayer,
			final Set<File> privateKeysFiles) {

		final var query = getAccountInfoQuery(feePayer, privateKeysFiles);

		try {
			final var tmpdir = Files.createTempDirectory("tmpDirPrefix").toFile();
			FileUtils.cleanDirectory(tmpdir);

			final List<File> newFiles = new ArrayList<>();
			for (final var account : accounts) {
				logger.info("Requesting information for account {}", account);
				final var identifier = new Identifier(account, feePayer.getNetworkName());
				query.setNetwork(getAccountNetwork(feePayer));
				final var info = query.getInfo(identifier);
				final var filePath =
						tmpdir.getAbsolutePath() + File.separator + identifier.toReadableAccountAndNetwork() + "." + INFO_EXTENSION;
				writeBytes(filePath, info.toBytes());
				logger.info("Account info for {} stored to {}", account, filePath);
				newFiles.add(new File(filePath));
			}

			if (!newFiles.isEmpty()) {
				importInfoFiles(newFiles);
			}
			accountsToUpdateTextField.clear();
			if (addAccountsTitledPane.isExpanded()) {
				addAccountsTitledPane.setExpanded(false);
			}

		} catch (final TimeoutException e) {
			PopupMessage.display("Timeout error", TIMEOUR_ERROR_MESSAGE);
			logger.error(e.getMessage());
		} catch (final PrecheckStatusException e) {
			PopupMessage.display("Precheck Error", precheckErrorString(e));
			logger.error(e.getMessage());
		} catch (final IOException | HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	private String getAccountNetwork(final Identifier account) {
		final var network = accountLineInformation.stream()
				.filter(lineInformation -> account.equals(lineInformation.getAccount()))
				.findFirst()
				.map(AccountLineInformation::getLedgerId)
				.orElse(UNKNOWN_NETWORK_STRING);
		return network.equals(UNKNOWN_NETWORK_STRING) ? (String) networkChoiceBoxA.getValue() : network;
	}

	@NotNull
	private String precheckErrorString(final PrecheckStatusException e) {
		return "Request " + e.getMessage();
	}


	@NotNull
	private AccountInfoQuery getAccountInfoQuery(final Identifier feePayer, final Set<File> privateKeysFiles) {

		final var utility = new KeyPairUtility();
		final var privateKeys =
				privateKeysFiles.stream().map(privateKeyFile -> utility.getKeyPairFromPEM(privateKeyFile,
						format("Please enter the password for key %s",
								FilenameUtils.getBaseName(privateKeyFile.getName())))).map(
						keyPair -> PrivateKey.fromBytes(keyPair.getPrivate().getEncoded())).collect(Collectors.toList());

		return AccountInfoQuery.Builder
				.anAccountInfoQuery()
				.withSigningKeys(privateKeys)
				.withFeePayer(feePayer.asAccount())
				.withFee(Hbar.fromTinybars(controller.getDefaultTxFee()))
				.build();
	}

	private Set<File> getPrivateKeysFrom(final Key fullKey) {
		final var flatKeys = EncryptionUtils.flatPubKeysString(Collections.singletonList(fullKey));
		final var pubFiles = controller.getPubFiles();
		return flatKeys.stream().filter(pubFiles::containsKey).map(s -> new File(KEYS_FOLDER,
				FilenameUtils.getBaseName(pubFiles.get(s).getFileName().toString()) + "." + PK_EXTENSION)).collect(
				Collectors.toSet());
	}

	/**
	 * Select the accounts that can be fee payers
	 */
	private void setupFeePayers() {
		// Get public keys
		final var privateKeysFiles = new File(KEYS_FOLDER).listFiles((dir, name) -> name.endsWith(PK_EXTENSION));
		if (privateKeysFiles == null) {
			throw new HederaClientRuntimeException("Error reading private keys");
		}

		for (final var privateKeysFile : privateKeysFiles) {
			final var publicKeyFile =
					new File(KEYS_FOLDER, FilenameUtils.getBaseName(privateKeysFile.getName()) + "." + PUB_EXTENSION);
			if (publicKeyFile.exists()) {
				keys.add(EncryptionUtils.publicKeyFromFile(publicKeyFile.getAbsolutePath()));
			}
		}


		final var accountFiles = new File(ACCOUNTS_INFO_FOLDER).listFiles((dir, name) -> name.endsWith(INFO_EXTENSION));
		if (accountFiles == null) {
			throw new HederaClientRuntimeException("Error reading account files");
		}

		feePayers.clear();
		for (final var accountFile : accountFiles) {
			final InfoFile infoFile;
			try {
				infoFile = new InfoFile(FileDetails.parse(accountFile));
			} catch (final HederaClientException e) {
				logger.error(e.getMessage());
				return;
			}
			if (infoFile.canSign(keys)) {
				feePayers.add(infoFile.getAccountID());
			}
		}
	}

	private void getBalancesFromFileSystem() throws HederaClientException {
		if (!new File(BALANCES_FILE).exists()) {
			return;
		}
		final var balancesArray = readJsonArray(BALANCES_FILE);
		for (final var jsonElement : balancesArray) {
			final var element = jsonElement.getAsJsonObject();
			if (!element.has(ACCOUNT_PROPERTY) || !element.has(DATE_PROPERTY) || !element.has(BALANCE_PROPERTY)) {
				throw new HederaClientException("Invalid element in balances array.");
			}
			final var date = element.get(DATE_PROPERTY).getAsLong();
			final var balance = element.get(BALANCE_PROPERTY).getAsLong();
			final var account = element.get(ACCOUNT_PROPERTY).getAsString();


			final var jsonObject = new JsonObject();
			jsonObject.addProperty(DATE_PROPERTY, date);
			jsonObject.addProperty(BALANCE_PROPERTY, balance);
			if (balances.has(account) &&
					balances.get(account).getAsJsonObject().get(DATE_PROPERTY).getAsLong() > date) {
				continue;
			}
			balances.add(account, jsonObject);
		}
	}

	/***
	 *
	 */
	private void getAccountsFromFileSystem(final Map<String, String> accountInfos,
			final Map<String, String> idNickNames) throws HederaClientException {
		accountInfos.clear();

		idNickNames.clear();

		final var nicknames =
				new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		final var accountFiles = new File(ACCOUNTS_INFO_FOLDER).listFiles((dir, name) -> name.endsWith(INFO_EXTENSION));

		if (accountFiles == null) {
			throw new HederaClientException(ACCOUNTS_FOLDER_ERROR_MESSAGE);
		}

		for (final var accountFile : accountFiles) {
			final var accountId = FilenameUtils.getBaseName(accountFile.getName());
			if (!nicknames.has(accountId)) {
				nicknames.addProperty(accountId, accountId);
			}
			idNickNames.put(accountId, nicknames.get(accountId).getAsString());
			accountInfos.put(accountId, accountFile.getAbsolutePath());
		}
		writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);
	}


	/**
	 * Builds the list of accounts that will be used to populate the table
	 */
	private void updateAccountLineInformation() throws IOException, HederaClientException {
		final var nicknames =
				new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

		accountLineInformation.clear();
		for (final var entry : accountInfos.entrySet()) {

			// For legacy accounts
			if (!balances.has(entry.getKey())) {
				final var location = entry.getValue();
				updateBalanceFromInfo(location);
			}

			final var balance =
					Hbar.fromTinybars(balances.get(entry.getKey()).getAsJsonObject().get(BALANCE_PROPERTY).getAsLong());
			final var date = balances.get(entry.getKey()).getAsJsonObject().get(DATE_PROPERTY).getAsLong();
			final var info = AccountInfo.fromBytes(readBytes(entry.getValue()));
			final var ledgerString = getLedgerString(FilenameUtils.getBaseName(entry.getValue()), info.ledgerId);
			final var account = new Identifier(info.accountId, ledgerString);
			final var line =
					new AccountLineInformation(info, nicknames.get(entry.getKey()).getAsString(),
							account, balance, date, isSigner(info), ledgerString);
			accountLineInformation.add(line);
		}
		Collections.sort(accountLineInformation);
	}

	/**
	 * If the balances json does not have information about an account in the table, update it from the info file
	 *
	 * @param location
	 * 		the file location of the account info
	 */
	private void updateBalanceFromInfo(final String location) throws HederaClientException {
		final AccountInfo info;
		final BasicFileAttributes attributes;
		if (!new File(location).exists()) {
			logger.info("File does not exist");
			return;
		}

		try {
			info = AccountInfo.fromBytes(readBytes(location));
			attributes = Files.readAttributes(new File(location).toPath(), BasicFileAttributes.class);
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}


		final var ledgerString =
				getLedgerString(FilenameUtils.getBaseName(location), info.ledgerId).toUpperCase(Locale.ROOT);

		final var accountName = new Identifier(info.accountId, ledgerString).toReadableAccountAndNetwork();
		updateBalance(accountName, info.balance, attributes.creationTime().toMillis());
	}

	private String getLedgerString(final String baseName, final LedgerId ledger) {
		if (baseName.contains("-")) {
			return baseName.substring(baseName.lastIndexOf("-") + 1);
		}
		return NetworkEnum.from(ledger).toString();
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
		final var table = new TableView<AccountLineInformation>();
		table.getItems().clear();

		final var nicknameColumn = getNicknameColumn(table);

		final var accountIDColumn = getAccountIDColumn(table);

		final var networkColumn = getNetworkColumn(table);

		final var dateColumn = getLastRefreshDateColumn(table);

		final var balanceColumn = getBalanceColumn(table);

		final var expanderColumn = getExpanderColumn();

		final var deleteColumn = getDeleteColumn(table);

		final var checkBoxColumn = getCheckBoxColumn(table);

		table.getColumns().addAll(expanderColumn, checkBoxColumn, nicknameColumn, accountIDColumn, networkColumn,
				dateColumn, balanceColumn, deleteColumn);
		table.setItems(accountLineInformation);

		if (accountLineInformation.size() == 1) {
			expanderColumn.toggleExpanded(0);
		}
		return table;
	}


	/**
	 * Set up a checkbox column in the table
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	private TableColumn<AccountLineInformation, Boolean> getCheckBoxColumn(
			final TableView<AccountLineInformation> table) {
		final var checkBoxColumn = new TableColumn<AccountLineInformation, Boolean>("");
		checkBoxColumn.setGraphic(selectAll);
		checkBoxColumn.setCellValueFactory(f -> f.getValue().selectedProperty());
		checkBoxColumn.setCellFactory(CheckBoxTableCell.forTableColumn(checkBoxColumn));

		selectAll.setOnAction(actionEvent -> {
			actionEvent.consume();
			try {
				noise = true;
				for (final var item : table.getItems()) {
					item.setSelected(selectAll.isSelected());
				}
			} finally {
				noise = false;
			}
		});

		checkBoxColumn.prefWidthProperty().bind(table.widthProperty().divide(20));
		checkBoxColumn.setEditable(true);
		table.setEditable(true);
		checkBoxColumn.setStyle("-fx-alignment: TOP-CENTER; -fx-padding: 11 0 0 0");
		return checkBoxColumn;
	}

	/**
	 * Set up the nickname column in the table
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, String> getNicknameColumn(
			final TableView<AccountLineInformation> table) {
		final var nicknameColumn = new TableColumn<AccountLineInformation, String>("Account");
		nicknameColumn.setCellValueFactory(new PropertyValueFactory<>("nickname"));
		nicknameColumn.prefWidthProperty().bind(table.widthProperty().divide(40).multiply(7));
		nicknameColumn.setStyle("-fx-alignment: TOP-LEFT; -fx-padding: 10");
		nicknameColumn.setCellFactory(p -> new TableCell<>() {
			@Override
			protected void updateItem(final String s, final boolean b) {
				super.updateItem(s, b);
				if (isEmpty()) {
					setText("");
				} else {
					setWrapText(true);
					setText(s);
				}
			}
		});


		return nicknameColumn;
	}

	private TableColumn<AccountLineInformation, String> getLastRefreshDateColumn(
			final TableView<AccountLineInformation> table) {
		final var dateColumn = new TableColumn<AccountLineInformation, String>("Last updated");
		dateColumn.setCellValueFactory(
				f -> {
					final var date =
							Instant.ofEpochMilli(f.getValue().getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
					return new SimpleStringProperty(date.format(DateTimeFormatter.ofPattern("MM/dd/yyyy")));
				});
		dateColumn.prefWidthProperty().bind(table.widthProperty().divide(20).multiply(3));
		dateColumn.setStyle(COLUMN_STYLE_STRING);
		return dateColumn;
	}


	/**
	 * Set up the Account ID column
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, Identifier> getAccountIDColumn(
			final TableView<AccountLineInformation> table) {
		final var accountIDColumn = new TableColumn<AccountLineInformation, Identifier>("");
		final var title = new HBox();
		final var refreshSelectedButton = refreshButton();
		refreshSelectedButton.setOnAction(actionEvent -> updateSelectedInfos());
		title.getChildren().add(new Label("Account ID"));
		title.getChildren().add(refreshSelectedButton);
		title.setSpacing(5);
		title.setAlignment(Pos.CENTER_RIGHT);
		accountIDColumn.setGraphic(title);

		accountIDColumn.setCellValueFactory(new PropertyValueFactory<>(ACCOUNT_PROPERTY));
		accountIDColumn.setCellFactory(tc -> new TableCell<>() {
			@Override
			protected void updateItem(final Identifier accountID, final boolean empty) {
				if (empty) {
					setText("");
				} else {

					setText(accountID.toReadableStringAndChecksum());
				}
			}
		});
		accountIDColumn.prefWidthProperty().bind(table.widthProperty().divide(40).multiply(7));
		accountIDColumn.setStyle("-fx-alignment: TOP-LEFT; -fx-padding: 10");
		return accountIDColumn;
	}


	/**
	 * Set up the Network column
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, String> getNetworkColumn(final TableView<AccountLineInformation> table) {
		final var networkColumn = new TableColumn<AccountLineInformation, String>("Network");
		networkColumn.setCellValueFactory(new PropertyValueFactory<>("ledgerId"));
		networkColumn.prefWidthProperty().bind(table.widthProperty().divide(20).multiply(2));
		networkColumn.setStyle(COLUMN_STYLE_STRING);
		networkColumn.setCellFactory(p -> new TableCell<>() {
			@Override
			protected void updateItem(final String s, final boolean b) {
				super.updateItem(s, b);
				if (isEmpty()) {
					setText("");
				} else {
					setFont(Font.font("Consolas", 15));
					setText(s);
				}
			}
		});


		return networkColumn;
	}

	/**
	 * Set up the action column
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, String> getDeleteColumn(final TableView<AccountLineInformation> table) {
		final var actionColumn = new TableColumn<AccountLineInformation, String>("");
		actionColumn.setCellValueFactory(new PropertyValueFactory<>(""));
		final var cellFactory =
				(Callback<TableColumn<AccountLineInformation, String>, TableCell<AccountLineInformation, String>>) accountLineInformationStringTableColumn -> new TableCell<>() {
					final Button button = deleteButton();

					@Override
					public void updateItem(final String item, final boolean empty) {
						setText(null);
						if (!empty) {
							if (controller.getSetupPhase().equals(SetupPhase.TEST_PHASE)) {
								final var x = table.getItems().get(getIndex());
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
						final var answer = PopupMessage.display("Warning", DELETE_ACCOUNT_WARNING_MESSAGE, true,
								CONTINUE_LABEL, CANCEL_LABEL);
						if (Boolean.TRUE.equals(answer)) {
							final var accountLineInformation1 = getTableView().getItems().get(getIndex());
							logger.info("Deleting {}", accountLineInformation1.getNickname());
							refresh(accountLineInformation1);
							table.getItems().remove(getIndex());
							setupFeePayers();
							final var key = accountLineInformation1.getAccount().toReadableAccountAndNetwork();
							idNickNames.remove(key);
							controller.removeAccount(key);
							feePayers.remove(accountLineInformation1.getAccount());
							setupFeePayerChoiceBox();
							controller.homePaneController.setForceUpdate(true);
							controller.settingsPaneController.setupFeePayerChoicebox();
						}
					}
				};
		actionColumn.setCellFactory(cellFactory);
		return actionColumn;
	}

	/**
	 * Set up the expander column
	 *
	 * @return a formatted column
	 */
	@NotNull
	private TableRowExpanderColumn<AccountLineInformation> getExpanderColumn() {
		final var expanderColumn = new TableRowExpanderColumn<>(this::buildAccountVBox);
		expanderColumn.setStyle(COLUMN_STYLE_STRING);
		return expanderColumn;
	}

	/**
	 * Set up the Balance column
	 *
	 * @param table
	 * 		a table containing account information
	 * @return a formatted column
	 */
	@NotNull
	private TableColumn<AccountLineInformation, Hbar> getBalanceColumn(final TableView<AccountLineInformation> table) {
		final var refresh = refreshButton();
		refresh.setOnAction(actionEvent -> updateSelectedBalances());
		final var title = new HBox();
		title.getChildren().add(new Label("Balance"));
		title.getChildren().add(refresh);
		title.setSpacing(5);
		title.setAlignment(Pos.CENTER_RIGHT);
		final var balanceColumn = new TableColumn<AccountLineInformation, Hbar>("");
		balanceColumn.setGraphic(title);
		balanceColumn.setCellValueFactory(new PropertyValueFactory<>(BALANCE_PROPERTY));
		balanceColumn.setCellFactory(tc -> new TableCell<>() {
			@Override
			protected void updateItem(final Hbar hBars, final boolean empty) {
				if (empty) {
					setText("");
				} else {
					setFont(Font.font("Courier New", 17));
					setText(hBars.toString());
				}
			}
		});

		balanceColumn.prefWidthProperty().bind(table.widthProperty().divide(20).multiply(5));
		balanceColumn.setStyle("-fx-alignment: TOP-RIGHT; -fx-padding: 10");
		return balanceColumn;
	}

	/**
	 * Setups a delete button
	 *
	 * @return a button with the delete.png icon
	 */
	private Button deleteButton() {
		final var button = new Button();
		final var imageView = new ImageView(new Image("icons/delete.png"));
		imageView.setFitHeight(20);
		imageView.setPreserveRatio(true);
		button.setGraphic(imageView);
		button.setStyle("-fx-background-color: transparent; -fx-border-color: transparent");
		return button;
	}

	/**
	 * Setups a refresh button
	 *
	 * @return a button with the refresh.png icon
	 */
	private Button refreshButton() {
		final var button = new Button();
		final var imageView = new ImageView(new Image("icons/refresh.png"));
		imageView.setFitHeight(30);
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
	public void refresh(final AccountLineInformation accountLineInformation) {
		final var accountID = accountLineInformation.getAccount();
		final var accountIDString = accountID.toReadableAccountAndNetwork();
		if (controller.getDefaultFeePayer().equals(accountID)) {
			removeDefaultFeePayer();
		}
		final var directory = controller.getPreferredStorageDirectory();
		final var oldInfoPath =
				Paths.get(directory, ACCOUNTS, format(FORMAT_NAME_EXTENSION, accountIDString, INFO_EXTENSION));
		final var oldJsonPath =
				Paths.get(directory, ACCOUNTS, format(FORMAT_NAME_EXTENSION, accountIDString, JSON_EXTENSION));

		// move the folder to the deleted accounts folder
		try {
			archiveOldInfoFile(oldInfoPath);
			archiveOldInfoFile(oldJsonPath);
		} catch (final IOException e) {
			controller.logAndDisplayError(e);
		}

		// update nickname store
		final JsonObject nicknames;
		try {
			nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			if (nicknames.has(accountIDString)) {
				nicknames.remove(accountIDString);
				writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);
			}
		} catch (final HederaClientException e) {
			controller.logAndDisplayError(e);
		}
		// Delete the account info from the History of accepted files
		final var historyInfo = new File(controller.getPreferredStorageDirectory() + "/History").listFiles(
				(dir, name) -> name.contains(accountIDString));
		if (historyInfo == null) {
			throw new HederaClientRuntimeException("Error reading history");
		}
		if (historyInfo.length > 0) {
			for (final var file : historyInfo) {
				try {
					Files.deleteIfExists(file.toPath());
					logger.info("File {} deleted", file.getAbsolutePath());
				} catch (final IOException e) {
					logger.error("File {} cannot be deleted", file.getAbsolutePath());
				}
			}
		}
		logger.info("Account {} has been deleted", accountIDString);

		//Archiving history
		final var historyList =
				new File(ACCOUNTS_INFO_FOLDER, ARCHIVE).listFiles((dir, name) -> name.startsWith(accountIDString + "_"
				));

		final var deletedFolder = new File(ACCOUNTS_INFO_FOLDER, ARCHIVE + File.separator + "DELETED");
		if (deletedFolder.mkdirs()) {
			logger.info("Deleted folder created");
		}
		try {
			final var zip = zipFiles(historyList,
					deletedFolder.getAbsolutePath() + File.separator + accountIDString + "." + ZIP_EXTENSION);
			if (zip != null) {
				logger.info("Archive for account {} stored to {}", accountIDString, zip.getAbsolutePath());
			}
		} catch (final HederaClientException e) {
			logger.error("IO Error during zip process");
		}
		controller.createPaneController.initializeCreatePane();
	}

	private void removeDefaultFeePayer() {
		final var network = networkChoiceBoxA.getValue();
		if (!(network instanceof String)) {
			return;
		}
		controller.removeDefaultFeePayer((String) network);
		controller.settingsPaneController.initializeSettingsPane();
		setupFeePayerChoiceBox();
		controller.settingsPaneController.setupFeePayerChoicebox();
	}

	/**
	 * Finds out if any of the pem files in the "Keys" folder can be used to sign for this account
	 *
	 * @param accountInfo
	 * 		the proto that contains the account information
	 * @return true if any of the private keys in the app corresponds to one of the public keys in the account
	 */
	private boolean isSigner(final AccountInfo accountInfo) {
		final var knownKeys = getKeysFromInfo(accountInfo, controller);

		if (knownKeys.isEmpty()) {
			return false;
		}
		for (final var knownKey : knownKeys) {
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
	private VBox buildAccountVBox(final TableRowExpanderColumn.TableRowDataFeatures<AccountLineInformation> parameter) {
		final var lineInformation = parameter.getValue();
		final var allBoxes = new VBox();

		allBoxes.setStyle(BOX_STYLE);
		allBoxes.setPadding(new Insets(10));
		allBoxes.setSpacing(10);
		try {
			final var returnBox = new HBox();
			returnBox.setSpacing(20);
			final var changeNicknameButton = setupWhiteButton("CHANGE");
			final var acceptNicknameButton = setupWhiteButton("ACCEPT");
			acceptNicknameButton.setVisible(false);

			final var nickname = setupBoxTextField(lineInformation.getNickname());
			nickname.editableProperty().bind(acceptNicknameButton.visibleProperty());
			nickname.setFocusTraversable(false);

			changeNicknameButton.setOnAction(actionEvent -> {
				changeNicknameButton.setVisible(false);
				acceptNicknameButton.setVisible(true);
				nickname.requestFocus();
			});

			acceptNicknameButton.setOnAction(
					actionEvent -> updateNickname(parameter, lineInformation, changeNicknameButton,
							acceptNicknameButton, nickname));
			nickname.setOnKeyReleased(
					keyEvent -> {
						if (KeyCode.ENTER.equals(keyEvent.getCode()) && nickname.isEditable()) {
							updateNickname(parameter, lineInformation, changeNicknameButton, acceptNicknameButton,
									nickname);
						}
					});


			final var key = lineInformation.getAccount().toReadableAccountAndNetwork();
			final var filePath = accountInfos.get(key);
			final var info = AccountInfo.fromBytes(readBytes(filePath));

			final var keyTreeView = controller.buildKeyTreeView(info.key);
			final double height = 28;
			keyTreeView.setPrefHeight((keyTreeView.expandedItemCountProperty().get() + 1) * height);

			keyTreeView.setStyle("-fx-border-color: white");

			keyTreeView.setOnMouseClicked(
					mouseEvent -> displayCompleteKeysPopup(keyTreeView.getSelectionModel().getSelectedItem(),
							mouseEvent));

			final var keysVBox = new VBox();

			keysVBox.getChildren().add(keyTreeView);
			final var refreshButton = refreshButton();

			final var asJsonObject = balances.get(key).getAsJsonObject();
			final var date = asJsonObject.get(DATE_PROPERTY).getAsLong();
			final var hbars = Hbar.fromTinybars(asJsonObject.get(BALANCE_PROPERTY).getAsLong());
			final var dateLabel =
					setupBoxLabel(
							format("Balance (as of %s)", instantToLocalTimeDate(new Date(date).toInstant())));
			final var networkBox = getNetworkBox(info, lineInformation);

			final var balanceTextField = setupBoxTextField(hbars.toString());

			final var memoTextField = setupBoxTextField(info.accountMemo);

			final var gridPane =
					refreshGridPane(nickname, info, networkBox, refreshButton, dateLabel, balanceTextField,
							memoTextField);
			HBox.setHgrow(keysVBox, Priority.ALWAYS);
			VBox.setVgrow(keysVBox, Priority.ALWAYS);

			refreshButton.setDisable(UNKNOWN_NETWORK_STRING.equals(lineInformation.getLedgerId()));
			refreshButton.setOnAction(actionEvent -> {
				parameter.toggleExpanded();
				updateBalances(Collections.singletonList(lineInformation));
			});


			final var buttons = new HBox();
			buttons.getChildren().addAll(changeNicknameButton, acceptNicknameButton);
			keysVBox.prefWidthProperty().bind(returnBox.widthProperty().divide(2).subtract(50));

			gridPane.add(buttons, 2, 0);


			final var link = new Hyperlink("History");

			link.setOnAction(actionEvent -> AccountHistoryPopup.display(
					new Identifier(info.accountId, parameter.getValue().getLedgerId()), controller));

			gridPane.add(link, 3, 0);

			returnBox.getChildren().add(gridPane);

			allBoxes.setPrefHeight(Region.USE_COMPUTED_SIZE);
			allBoxes.setPrefWidth(Region.USE_COMPUTED_SIZE);
			final var keyLabel = new Label("Key");
			keyLabel.setStyle(FX_TEXT_FILL_BLACK);
			allBoxes.getChildren().addAll(returnBox, keyLabel, keyTreeView);


		} catch (final Exception e) {
			logger.error(e);
		}
		return allBoxes;
	}

	@NotNull
	private HBox getNetworkBox(final AccountInfo info, final AccountLineInformation accountLineInformation) {
		final var networkBox = new HBox();
		final var doesNotHaveLedger = UNKNOWN_NETWORK_STRING.equals(accountLineInformation.getLedgerId());
		final var networkChoiceBox = new ChoiceBox<>();

		networkChoiceBox.setPrefWidth(300);
		networkChoiceBox.setMinWidth(300);
		networkChoiceBox.setMaxWidth(300);
		networkChoiceBox.setStyle(
				"-fx-background-color: white; -fx-border-color: #c2c2c2; -fx-background-radius: 10; " +
						"-fx-border-radius: 10");
		controller.networkBoxSetup(networkChoiceBox);
		networkChoiceBox.getSelectionModel().selectedItemProperty().addListener((observableValue, o, t1) -> {
			if (!noise) {
				if (t1 instanceof String) {
					handleNetworkString(info, accountLineInformation, (String) t1);
					handleNetworkStringFinish();
				}
				if (t1 instanceof Separator) {
					networkChoiceBox.getSelectionModel().select(o);
				}
			}
		});

		final var networkTextField = setupBoxTextField(accountLineInformation.getLedgerId());
		networkBox.getChildren().add(doesNotHaveLedger ? networkChoiceBox : networkTextField);
		return networkBox;
	}

	private void handleNetworkString(
			final AccountInfo info, final AccountLineInformation accountLineInformation, final String t1) {
		final var oldName = new Identifier(info.accountId,
				accountLineInformation.getLedgerId()).toReadableAccountAndNetwork();
		final var newName = new Identifier(info.accountId, t1).toReadableAccountAndNetwork();
		final var oldFiles =
				new File(ACCOUNTS_INFO_FOLDER).listFiles((dir, name) -> name.contains(oldName));
		for (final var oldFile : oldFiles) {
			try {
				if (INFO_EXTENSION.equalsIgnoreCase(FilenameUtils.getExtension(oldFile.getName()))) {
					var newInfo = CryptoGetInfoResponse.AccountInfo
							.newBuilder(CryptoGetInfoResponse.AccountInfo.parseFrom(readBytes(oldFile.getAbsolutePath())))
							.setLedgerId(ByteString.copyFrom(NetworkEnum.asLedger(t1).toBytes()))
							.build();
					FileUtils.writeByteArrayToFile(Path.of(oldFile.getAbsolutePath().replace(oldName, newName)).toFile(),
							newInfo.toByteArray());
					if (!oldFile.delete()) {
						logger.error("failed to delete {}", oldFile.getAbsolutePath());
					}
				} else {
					Files.move(oldFile.toPath(), Path.of(oldFile.getAbsolutePath().replace(oldName, newName)));
				}
			} catch (final Exception e) {
				logger.error("Cannot move file", e);
			}
		}

		updateNicknamesFile(accountLineInformation, oldName, newName);

		final var oldBalance = balances.get(oldName);
		balances.remove(oldName);
		balances.add(newName, oldBalance);

		accountLineInformation.setLedgerID(t1);
	}

	private void handleNetworkStringFinish() {
		setupFeePayers();
		setupFeePayerChoiceBox();
		initializeAccountPane();
	}

	private void updateNicknamesFile(
			final AccountLineInformation accountLineInformation, final String oldName, final String newName) {
		try {
			final var nicknames =
					new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) :
							new JsonObject();
			if (nicknames.has(oldName)) {
				nicknames.remove(oldName);
				nicknames.addProperty(newName, accountLineInformation.getNickname());
				writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);
			}
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	@NotNull
	private GridPane refreshGridPane(
			final TextField nickname, final AccountInfo info, final Node network, final Button refreshButton,
			final Label date, final TextField balance, final TextField memo) {
		final var gridPane = new GridPane();
		gridPane.setVgap(10);
		gridPane.setHgap(10);

		gridPane.add(setupBoxLabel("Nickname"), 0, 0);
		gridPane.add(nickname, 1, 0);
		gridPane.add(setupBoxLabel("Account ID"), 0, 1);
		final var account = new Identifier(info.accountId).toReadableString();
		gridPane.add(setupBoxTextField(format("%s (%s)", account, AddressChecksums.checksum(account))), 1, 1);
		gridPane.add(setupBoxLabel("Network"), 0, 2);
		gridPane.add(network, 1, 2);
		gridPane.add(date, 0, 3);
		gridPane.add(balance, 1, 3);
		gridPane.add(refreshButton, 2, 3);
		gridPane.add(setupBoxLabel("Auto Renew Period"), 0, 4);
		gridPane.add(setupBoxTextField(info.autoRenewPeriod.getSeconds() + " s"), 1, 4);
		gridPane.add(setupBoxLabel("Expiration Date"), 0, 5);
		gridPane.add(setupBoxTextField(getExpirationTimeString(info)), 1, 5);
		gridPane.add(setupBoxLabel("Receiver Signature Required"), 0, 6);
		gridPane.add(setupBoxTextField(valueOf(info.isReceiverSignatureRequired)), 1, 6);

		var cnt = 6;

		var stakingInfo = info.stakingInfo;
		if (stakingInfo != null && stakingInfo.stakedAccountId != null) {
			gridPane.add(setupBoxLabel("Staked Account ID"), 0, ++cnt);
			final var stakingAccountStr = new Identifier(info.stakingInfo.stakedAccountId).toReadableString();
			gridPane.add(setupBoxTextField(format("%s (%s)", stakingAccountStr, AddressChecksums.checksum(stakingAccountStr))), 1, cnt);
		}

		if (stakingInfo != null && stakingInfo.stakedNodeId != null) {
			gridPane.add(setupBoxLabel("Staked Node ID"), 0, ++cnt);
			gridPane.add(setupBoxTextField(info.stakingInfo.stakedNodeId.toString()), 1, cnt);
		}

		if (stakingInfo != null) {
			gridPane.add(setupBoxLabel("Decline Staking Rewards"), 0, ++cnt);
			gridPane.add(setupBoxTextField(valueOf(info.stakingInfo.declineStakingReward)), 1, cnt);
		}

		if (!"".equals(memo.getText())) {
			gridPane.add(setupBoxLabel("Account memo"), 0, ++cnt);
			gridPane.add(memo, 1, cnt);
		}

		final var col1 = new ColumnConstraints();
		col1.setPercentWidth(50);

		final var col2 = new ColumnConstraints();
		col2.setPercentWidth(30);

		final var col3 = new ColumnConstraints();
		col3.setPercentWidth(20);

		gridPane.getColumnConstraints().addAll(col1, col2, col3);

		return gridPane;
	}

	private Hbar refreshBalance(final Identifier identifier, final String network) {
		final var query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(identifier.asAccount())
				.withNetwork(network)
				.build();
		Hbar balance;
		try {
			balance = query.getBalance();
			final var now = new Date();
			final var accountName = identifier.toReadableString() + "-" + network;
			updateBalance(accountName, balance, now.getTime());
		} catch (final PrecheckStatusException e) {
			PopupMessage.display("Precheck Error", precheckErrorString(e));
			logger.error(e.getMessage());
			balance = new Hbar(-1);
		} catch (final TimeoutException e) {
			PopupMessage.display("Timeout error", TIMEOUR_ERROR_MESSAGE);
			logger.error(e.getMessage());
			balance = new Hbar(-2);
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
			balance = new Hbar(-3);
		}
		final var finalBalance = balance;
		accountLineInformation.stream().filter(
				lineInformation -> lineInformation.getAccount().equals(identifier)).forEach(
				lineInformation -> lineInformation.setBalance(finalBalance));
		return balance;
	}

	private void updateBalance(final String identifier, final Hbar balance,
			final long date) throws HederaClientException {
		final var object = new JsonObject();
		object.addProperty(DATE_PROPERTY, date);
		object.addProperty(BALANCE_PROPERTY, balance.toTinybars());
		balances.add(identifier, object);
		final var array = new File(BALANCES_FILE).exists() ? readJsonArray(BALANCES_FILE) : new JsonArray();
		object.addProperty(ACCOUNT_PROPERTY, identifier);
		array.add(object);
		writeJsonObject(BALANCES_FILE, array);
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
	private void updateNickname(final TableRowExpanderColumn.TableRowDataFeatures<AccountLineInformation> parameter,
			final AccountLineInformation lineInformation, final Button changeNicknameButton,
			final Button acceptNicknameButton,
			final TextField nickname) {
		changeNicknameButton.setVisible(true);
		acceptNicknameButton.setVisible(false);
		final var newNickname = nickname.getText();

		if (lineInformation.getNickname().equals(newNickname)) {
			return;
		}
		if (idNickNames.containsValue(newNickname)) {
			PopupMessage.display("Duplicate nickname", format(NICKNAME_IN_USE_MESSAGE, newNickname),
					CONTINUE_LABEL);
			nickname.setText(lineInformation.getNickname());
			return;
		}
		parameter.toggleExpanded();
		lineInformation.setNickname(newNickname);
		changeNickname(newNickname, lineInformation.getAccount().toReadableAccountAndNetwork());
		setupFeePayers();
		try {
			noise = true;
			setupFeePayerChoiceBox();
		} finally {
			noise = false;
		}
	}

	/**
	 * if the user double-clicks on a tree item display the keys popup
	 *
	 * @param item
	 * 		the selected item in the tree
	 * @param mouseEvent
	 * 		the event that triggered the method
	 */
	private void displayCompleteKeysPopup(final TreeItem<String> item, final MouseEvent mouseEvent) {
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
		final var displayPopup =
				CompleteKeysPopup.display(controller.keysPaneController.getPublicKeysMap().get(item.getValue()),
						true);
		if (displayPopup.equals(true)) {
			controller.keysPaneController.initializeKeysPane();
		}
	}

	private Button setupWhiteButton(final String change) {
		final var changeNicknameButton = new Button(change);
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
	private void changeNickname(final String newNickname, final String account) {
		try {
			final var nicknames =
					new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			if (nicknames.has(account)) {
				nicknames.addProperty(account, newNickname);
			}
			writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);
			getAccountsFromFileSystem(accountInfos, idNickNames);
			controller.setAccountInfoMap(accountInfos);
		} catch (final HederaClientException exception) {
			logger.error(exception);
		}

	}

	private Label setupBoxLabel(final String field) {
		final var fieldLabel = new Label(field);
		fieldLabel.setStyle(FX_TEXT_FILL_BLACK);
		fieldLabel.setVisible(true);
		fieldLabel.setWrapText(true);
		return fieldLabel;
	}

	private TextField setupBoxTextField(final String text) {
		final var textField = new TextField(text);
		textField.setPrefWidth(300);
		textField.setMinWidth(300);
		textField.setMaxWidth(300);
		textField.setEditable(false);
		textField.setFocusTraversable(false);
		textField.setStyle(
				"-fx-background-color: white; -fx-border-color: #c2c2c2; -fx-background-radius: 10; " +
						"-fx-border-radius: 10");
		return textField;
	}

	/**
	 * Returns a human-readable string that represents the expiration date of the account
	 *
	 * @param accountInfo
	 * 		the proto that contains the account information
	 * @return A human-readable date, or "never" if the account doesn't expire (system accounts)
	 */
	private String getExpirationTimeString(final AccountInfo accountInfo) {
		final var d = new Date(accountInfo.expirationTime.getEpochSecond() * 1000);
		final var cal = Calendar.getInstance();
		cal.add(Calendar.YEAR, 100);
		final var future = cal.getTime();
		cal.add(Calendar.YEAR, -102);
		final var past = cal.getTime();

		return d.after(future) || d.before(past) ? "Never" : timestampToString(
				new Timestamp(accountInfo.expirationTime));
	}

	// endregion

	// region IMPORT ACCOUNT


	/**
	 * Import accounts from a folder/folders
	 */
	public void importAccountFromFolder() throws HederaClientException, InvalidProtocolBufferException {
		controller.setThisPane(accountsPane);
		final File folder;
		if (!hiddenPathAccount.getText().isEmpty()) {
			folder = new File(hiddenPathAccount.getText());
			hiddenPathAccount.clear();
		} else {
			final var directory = controller.getLastTransactionsDirectory();
			if (directory == null) {
				return;
			}
			folder = new File(BrowserUtilities.browseDirectories(directory, controller.getThisPane()));
			controller.setLastBrowsedDirectory(folder);
		}

		final var files = folder.listFiles((dir, name) -> name.endsWith(INFO_EXTENSION));
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

	public void setUnknownAccountNetworks() {

		long count = accountLineInformation.stream()
				.filter(i -> i.isSelected() && UNKNOWN_NETWORK_STRING.equals(i.getLedgerId())).count();

		ChoiceDialog<Object> choiceDialog = new ChoiceDialog<>();
		controller.networkBoxSetup(choiceDialog.getItems());
		choiceDialog.setContentText("Network");
		choiceDialog.setHeaderText("Set the network on " + count + " account(s) associated with an UNKNOWN network.");
		choiceDialog.setGraphic(null);
		choiceDialog.setTitle("Set UNKNOWN Network Accounts");

		var result = count > 0 ? choiceDialog.showAndWait() : Optional.empty();

		boolean changed = false;
		for (final var lineInformation : accountLineInformation) {
			if (lineInformation.isSelected()) {
				if (result.isPresent() && (result.get() instanceof String)
						&& UNKNOWN_NETWORK_STRING.equals(lineInformation.getLedgerId())) {
					handleNetworkString(lineInformation.getInfo(), lineInformation, (String) result.get());
					changed = true;
				}
				lineInformation.setSelected(false);
			}
		}
		if (changed) {
			handleNetworkStringFinish();
		}

	}

	/**
	 * Import account info from either a .info file
	 * 1. for .info file, the user should set this account's nickname; corresponding file directory would be created;
	 * account.json and accountInfo.info would be saved in corresponding directory;
	 *
	 * @throws HederaClientException
	 * 		if importAccountFromFile throws an exception
	 */
	public void importAccountFromFile() throws HederaClientException, InvalidProtocolBufferException {

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

		final var directory = controller.getLastTransactionsDirectory();
		if (directory == null) {
			return;
		}
		files = BrowserUtilities.browseMultiFiles(directory, controller.getThisPane(), "Info", "info");
		if (files == null) {
			return;
		}
		controller.setLastBrowsedDirectory(files.get(0));
		importInfoFiles(files);


	}

	/**
	 * Imports one or more account info files to the app
	 *
	 * @param files
	 * 		list of files
	 */
	public int importInfoFiles(final List<File> files) throws HederaClientException, InvalidProtocolBufferException {
		final List<File> duplicates = new ArrayList<>();
		final List<File> newFiles = new ArrayList<>();
		final Set<String> nicknames = new HashSet<>(idNickNames.values());

		// separate list into new files and previously imported accounts
		for (final var file : files) {
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

		return counter;
	}

	/**
	 * Handle duplicate files
	 *
	 * @param duplicates
	 * 		a list of duplicates
	 * @return the number of accounts changes
	 */
	private int handleDuplicateFiles(final List<File> duplicates) throws HederaClientException {
		var counter = 0;
		if (!duplicates.isEmpty()) {
			for (final var file : duplicates) {
				replaceInfo(file);
				counter++;
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
	private int handleNewFiles(final List<File> newFiles, final Set<String> nicknames) throws HederaClientException,
			InvalidProtocolBufferException {
		var counter = 0;
		var responseEnum = ResponseEnum.UNKNOWN;
		var keepAsking = true;

		if (newFiles.isEmpty()) {
			return counter;
		}
		for (final var file : newFiles) {
			final var memo = AccountInfo.fromBytes(readBytes(file)).accountMemo;
			var newNickname = "".equals(memo) ? FilenameUtils.getBaseName(file.getName()) : memo;
			if (keepAsking) {
				final var responseTuple =
						getNicknameTuple(newFiles.size(), nicknames, newNickname);
				responseEnum = responseTuple.getResponseEnum();
				if (ResponseEnum.UNKNOWN.equals(responseEnum)) {
					return 0;
				}
				nicknames.add(responseTuple.getNickname());
				newNickname = responseTuple.getNickname();
			}
			switch (responseEnum) {
				case IGNORE_ONCE:
					break;
				case IGNORE_ALWAYS:
					keepAsking = false;
					break;
				case REPLACE_ONCE:
					importFromFile(file, newNickname);
					counter++;
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
		return counter;
	}

	/**
	 * Gets the nickname tuple for new files
	 *
	 * @param newFiles
	 * 		the number of new files (to decide how many buttons to show in the popup)
	 * @param nicknames
	 * 		the nicknames set
	 * @param file
	 * 		the file that needs a new nickname
	 * @return the tuple
	 */
	@NotNull
	private ResponseTuple getNicknameTuple(final int newFiles, final Set<String> nicknames, final String file) {
		var responseTuple = new ResponseTuple();
		while (responseTuple.getNickname().equals("")) {
			responseTuple = TwoButtonPopup.display(file, newFiles > 1);
			if (responseTuple.getResponseEnum().equals(ResponseEnum.UNKNOWN)) {
				responseTuple.setNickname("");
				PopupMessage.display("Info declined", "The action has been cancelled: The account has not been " +
						"imported", CONTINUE_LABEL);
				return responseTuple;
			}
			if (nicknames.contains(responseTuple.getNickname())) {
				PopupMessage.display("Duplicate nickname",
						format(NICKNAME_IN_USE_MESSAGE, responseTuple.getNickname()),
						CONTINUE_LABEL);
				responseTuple.setNickname("");
			}
		}
		return responseTuple;
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
		} catch (final IOException e) {
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
	private void replaceInfo(final File file) throws HederaClientException {
		final var identifier = getAccountIDFromInfoFile(file);
		final var infoPath =
				new File(format(PATH_NAME_EXTENSION, ACCOUNTS_INFO_FOLDER, identifier.toReadableAccountAndNetwork(),
						INFO_EXTENSION)).toPath();
		final var jsonPath =
				new File(format(PATH_NAME_EXTENSION, ACCOUNTS_INFO_FOLDER, identifier.toReadableAccountAndNetwork(),
						JSON_EXTENSION)).toPath();
		try {
			archiveOldInfoFile(infoPath);
			archiveOldInfoFile(jsonPath);
			storeAccount(idNickNames.get(identifier.toReadableAccountAndNetwork()), file.getAbsolutePath());
			updateBalanceFromInfo(infoPath.toString());
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}
		controller.createPaneController.initializeCreatePane();
	}

	private void archiveOldInfoFile(final Path newPath) throws IOException {
		final var archive = new File(ACCOUNTS_INFO_FOLDER, ARCHIVE);
		if (archive.mkdirs()) {
			logger.info("Archive directory created");
		}
		final var attributes = Files.readAttributes(newPath, BasicFileAttributes.class);
		final var fileName = newPath.toFile().getName();
		final var name = FilenameUtils.getBaseName(
				fileName) + "_" + attributes.creationTime().toMillis() + "." + FilenameUtils.getExtension(fileName);
		Files.move(newPath, Path.of(archive.getAbsolutePath(), name));
		Files.deleteIfExists(newPath);
	}

	/**
	 * Given a file containing an account info, returns the account id of the file
	 *
	 * @param file
	 * 		where information is stored
	 * @return the account id
	 */
	private Identifier getAccountIDFromInfoFile(final File file) throws HederaClientException {
		if (file == null) {
			throw new HederaClientRuntimeException("Null file");
		}
		if (!file.exists()) {
			throw new HederaClientRuntimeException(format("File %s cannot be found", file.getName()));
		}
		if (!file.isFile()) {
			throw new HederaClientRuntimeException("Must be a file, not a directory");
		}
		if (!file.getName().endsWith(INFO_EXTENSION)) {
			throw new HederaClientException(format("%s is not an info file", file.getAbsolutePath()));
		}

		try {
			final var info = AccountInfo.fromBytes(readBytes(file.getAbsolutePath()));
			return new Identifier(info.accountId, getNetworkFromInfo(info));
		} catch (final InvalidProtocolBufferException e) {
			controller.logAndDisplayError(e);
			throw new HederaClientException(e);
		}

	}

	private String getNetworkFromInfo(final AccountInfo info) {
		if (info.ledgerId.toString().equals("03")) {
			return "INTEGRATION";
		}
		if ("".equals(info.ledgerId.toString())) {
			return UNKNOWN_NETWORK_STRING;
		}
		return NetworkEnum.asLedger(info.ledgerId.toString()).toString();
	}

	/**
	 * If the account has been previously imported, the folder should exist.
	 *
	 * @param account
	 * 		an account id
	 * @return true if the account has been previously imported.
	 */
	private boolean inAccountMaps(final Identifier account) {
		final var file = new File(
				format("%s/Accounts/%s.%s", controller.getPreferredStorageDirectory(),
						account.toReadableAccountAndNetwork(), INFO_EXTENSION));
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
	void importFromFile(final File file, final String nickname) throws HederaClientException {
		final var newAccountID = getAccountIDFromInfoFile(file).toReadableAccountAndNetwork();
		final var account = new JsonObject();
		account.addProperty("accountID", newAccountID);
		account.addProperty("nickname", nickname);
		updateBalanceFromInfo(file.toString());
		idNickNames.put(newAccountID, nickname);
		accountInfos.put(nickname,
				format(PATH_NAME_EXTENSION, ACCOUNTS_INFO_FOLDER, newAccountID, INFO_EXTENSION));
		storeAccount(nickname, file.getAbsolutePath());
		controller.createPaneController.initializeCreatePane();
	}

	/**
	 * Stores an info in the Accounts folder
	 *
	 * @param nickname
	 * 		the nickname of the new account
	 * @param infoFile
	 * 		the location of the file
	 */
	private void storeAccount(final String nickname, final String infoFile) {
		try {
			logger.info("Importing account");

			final var info = AccountInfo.fromBytes(readBytes(infoFile));
			final var accountId = getAccountIDFromInfoFile(new File(infoFile)).toReadableAccountAndNetwork();

			// Update the list of nicknames.
			final var nicknames =
					new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

			nicknames.addProperty(accountId, nickname);
			writeJsonObject(ACCOUNTS_MAP_FILE, nicknames);

			// Store the info file in its new location
			final var newInfoName = format(PATH_NAME_EXTENSION, ACCOUNTS_INFO_FOLDER, accountId, INFO_EXTENSION);
			logger.info("Storing account details to: {}", newInfoName);
			Files.deleteIfExists(new File(newInfoName).toPath());
			writeBytes(newInfoName, info.toBytes());

			final var newJsonName = newInfoName.replace(INFO_EXTENSION, JSON_EXTENSION);
			Files.deleteIfExists(new File(newJsonName).toPath());
			writeJsonObject(newJsonName, info2Json(info));

		} catch (final IOException | HederaClientException ex) {
			logger.error("Unable to store AccountInfo.", ex);
		}
	}

	public void updateSelectedBalances() {
		final List<AccountLineInformation> list =
				accountLineInformation.stream().filter(AccountLineInformation::isSelected).collect(Collectors.toList());

		for (final var lineInformation : list) {
			if (UNKNOWN_NETWORK_STRING.equals(lineInformation.getLedgerId())) {
				PopupMessage.display("Unknown network",
						"One or more accounts don't have a known network. Please select the network in the account " +
								"card and try again");
				return;
			}
		}

		updateBalances(list);

		accountLineInformation.forEach(lineInformation -> lineInformation.setSelected(false));
	}

	private void updateSelectedInfos() {
		final List<AccountId> list = new ArrayList<>();
		for (final var lineInformation : accountLineInformation) {
			if (lineInformation.isSelected()) {
				list.add(lineInformation.getAccount().asAccount());
				lineInformation.setSelected(false);
			}
		}
		if (list.isEmpty()) {
			PopupMessage.display(NO_ACCOUNTS_SELECTED_TITLE, "At least one account must be selected");
			return;
		}
		try {
			final var feePayer = getFeePayer();
			if (feePayer == null) {
				return;
			}
			final var keyFiles = getKeyFiles(feePayer);
			if (keyFiles.isEmpty()) {
				PopupMessage.display(ERROR_TITLE,
						"At least one key must be selected in order to sign the transaction");
				return;
			}
			getInfosFromNetwork(list, feePayer, keyFiles);
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		} catch (final InvalidProtocolBufferException e) {
			PopupMessage.display("Invalid Information", "Cannot parse the response from the network");
			logger.error(e.getMessage());
		}
	}

	private void updateBalances(final List<AccountLineInformation> list) {
		final long size = list.size();
		final var progressBar = new ProgressBar();
		final var cancelButton = new Button(CANCEL_LABEL);
		Stage window = null;
		if (size == 0) {
			PopupMessage.display(NO_ACCOUNTS_SELECTED_TITLE, "At least one account must be selected");
			return;
		}

		if (size > 4 || SetupPhase.TEST_PHASE.equals(controller.getSetupPhase())) {
			window = ProgressPopup.setupProgressPopup(progressBar, cancelButton, "Updating Balances",
					"Please wait while the account balances are being updated.", size);
		}
		final Task<Void> task = new Task<>() {
			@Override
			protected Void call() throws InterruptedException {
				long counter = 0;
				for (final var lineInformation : list) {
					final var identifier = lineInformation.getAccount();
					final var network = lineInformation.getLedgerId();
					if (UNKNOWN_NETWORK_STRING.equals(network)) {
						continue;
					}
					final var balance = refreshBalance(identifier, network);
					updateProgress(counter, size);
					final var accountIdString = identifier.toReadableString();
					logger.info("Account {} new balance {}", accountIdString, balance);
					// Add a delay to prevent timeouts
					sleep(500);
					counter++;
				}
				updateProgress(size, size);
				return null;
			}
		};
		progressBar.progressProperty().bind(task.progressProperty());
		new Thread(task).start();

		final var finalWindow = window;
		task.setOnSucceeded(workerStateEvent -> {
			try {
				updateAccountLineInformation();
			} catch (final IOException | HederaClientException e) {
				logger.error(e.getMessage());
			}
			if (finalWindow != null) {
				finalWindow.close();
			}
		});

		task.setOnCancelled(workerStateEvent -> {
			logger.info("Update balances cancelled");
			if (finalWindow != null) {
				finalWindow.close();
			}
		});

		task.setOnFailed(workerStateEvent -> {
			logger.info("Update balances failed");
			if (finalWindow != null) {
				finalWindow.close();
			}
		});
	}


	// endregion

	/**
	 * Testing only: if the enter key is pressed in the hidden field, the address is accepted as the new account info
	 * file
	 *
	 * @param keyEvent
	 * 		the triggering key event
	 */
	public void choosePath(final KeyEvent keyEvent) throws HederaClientException, InvalidProtocolBufferException {
		if (KeyCode.ENTER.equals(keyEvent.getCode())) {
			final var infoPath = hiddenPathAccount.getText().replace(" ", "");
			if (infoPath.endsWith(".info") && new File(infoPath).exists()) {
				importAccountFromFile();
			}
		}
	}

	/**
	 * Set up for fee payer choice box
	 */
	public void setupFeePayerChoiceBox() {
		String feePayer = "";
		try {
			noise = true;
			feePayer = controller.setupChoiceBoxFeePayer(feePayerChoiceBoxA, feePayerTextFieldA);
		} finally {
			noise = false;
		}

		if ("".equals(feePayer)) {
			return;
		}
		feePayerChoiceBoxA.getSelectionModel().select(feePayer);
		feePayerChoiceBoxA.getSelectionModel().selectedItemProperty().addListener((observableValue, o, t1) -> {
			if (noise) {
				return;
			}
			final var net = networkChoiceBoxA.getValue();
			if (t1 instanceof String && net instanceof String && !"".equals(t1) && !"".equals(net)) {
				final var text = (String) t1;
				controller.setDefaultFeePayer(Identifier.parse(text, (String) net));
			}
		});
	}

	private void setupNetworkBox(final ChoiceBox<Object> choiceBox) {
		try {
			noise = true;
			controller.networkBoxSetup(choiceBox);

			choiceBox.getSelectionModel().select(controller.getCurrentNetwork());
			choiceBox.getSelectionModel().selectedItemProperty().addListener((observableValue, o, t1) -> {
				if (t1 instanceof String) {
					final var payers = controller.getDefaultFeePayers();
					final var s = ((String) t1).toUpperCase(Locale.ROOT);
					feePayerTextFieldA.setVisible(true);
					if (payers.containsKey(s)) {
						controller.setupChoiceBoxFeePayer(feePayerChoiceBoxA, feePayerTextFieldA, s);
						feePayerChoiceBoxA.getSelectionModel().select(payers.get(s));
					}
				}
			});
		} finally {
			noise = false;
		}
	}

	public void addFeePayerAction() {
		feePayerTextFieldA.setVisible(true);
		feePayerTextFieldA.requestFocus();
	}

	private void addCustomFeePayer(final Boolean t1) {
		if (Boolean.FALSE.equals(t1)) {
			final var tempSet = new HashSet<>(controller.getFeePayers());
			tempSet.addAll(controller.getCustomFeePayers());

			if ("".equals(feePayerTextFieldA.getText())) {
				return;
			}

			try {
				final var id = Identifier.parse(feePayerTextFieldA.getText());
				controller.setDefaultFeePayer(id);
				if (!tempSet.contains(id)) {
					controller.addCustomFeePayer(id);
				}
				setupFeePayerChoiceBox();
				feePayerTextFieldA.setVisible(false);
				feePayerTextFieldA.clear();
				controller.settingsPaneController.setupFeePayerChoicebox();
			} catch (final Exception e) {
				logger.error("Cannot parse identifier {}", e.getMessage());
				PopupMessage.display(ERROR_TITLE, "Cannot parse your input to an account. Please try again.");
				feePayerTextFieldA.requestFocus();
				feePayerTextFieldA.setVisible(true);
			}
		}

	}


	// endregion

}
