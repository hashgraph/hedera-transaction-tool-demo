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

import com.google.common.net.InetAddresses;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.Messages;
import com.hedera.hashgraph.client.core.constants.ToolTipMessages;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.ui.components.HbarCurrencyFormat;
import com.hedera.hashgraph.client.ui.popups.NewNetworkPopup;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.utilities.DriveSetupHelper;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.HbarUnit;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.util.StringConverter;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.ToggleSwitch;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.prefs.BackingStoreException;

import static com.hedera.hashgraph.client.core.constants.Constants.CUSTOM_NETWORK_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.DRIVE_LIMIT;
import static com.hedera.hashgraph.client.core.constants.Constants.HBAR_POSITIVE_FORMAT_STRING;
import static com.hedera.hashgraph.client.core.constants.Constants.MAXIMUM_AUTO_RENEW_PERIOD;
import static com.hedera.hashgraph.client.core.constants.Constants.MINIMUM_AUTO_RENEW_PERIOD;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.FEE_PAYER_TOOLTIP_MESSAGES;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.FOLDER_TOOLTIP_MESSAGES;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.GENERATE_RECORD_TOOLTIP_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.NETWORKS_TOOLTIP_MESSAGES;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.NODE_ID_TOOLTIP_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.START_TIME_TOOLTIP_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.TRANSACTION_FEE_TOOLTIP_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ToolTipMessages.VALID_DURATION_TOOLTIP_MESSAGE;
import static javafx.scene.control.Alert.AlertType;
import static javafx.scene.control.Control.USE_COMPUTED_SIZE;

public class SettingsPaneController implements GenericFileReadWriteAware {

	private static final Logger LOG = LogManager.getLogger(SettingsPaneController.class);
	private static final String REGEX = "\\D";
	private static final String REGEX1 = "\\d*";


	private boolean noise = false;

	public TextField loadStorageTextField;
	public TextField pathTextFieldSP;
	public TextField emailTextFieldSP;
	public TextField nodeIDTextField;
	public TextField txValidDurationTextField;
	public TextField autoRenewPeriodTextField;
	public TextField hoursTextField;
	public TextField minutesTextField;
	public TextField secondsTextField;
	public TextField defaultTransactionFee;
	public TextField customFeePayerTextField;

	public Label loadStorageLabel;
	public Label nodeIDLabel;
	public Label txValidDurationLabel;
	public Label generateRecordLabel;
	public Label localTimeLabel;
	public Label tvsErrorLabel;
	public Label arpErrorLabel;
	public Label accountIDErrorLabel;

	public Button addFolderButton;
	public Button confirmAddFolderButtonSP;
	public Button browseNewFolderButton;
	public Button cancelAddToEmailMapButton;
	public Button addCustomNetworkButton;
	public Button deleteCustomNetworkButton;
	public Button deleteCustomPayerButton;
	public Button addCustomPayerButton;
	public Button addCustomPayerButton1;

	public ImageView pathGreenCheck;
	public ImageView emailGreenCheck;

	public ToggleSwitch generateRecordSlider;

	public VBox storageBox;
	public VBox transactionBox;
	public VBox aboutBox;
	public VBox transactionFoldersVBoxSP;
	public GridPane addPathGridPane;
	public ScrollPane settingScrollPane;
	public Label drivesErrorLabelSP;
	public HBox addFolderPathHBoxSP;
	public ImageView deleteImage;
	public ImageView editImage;
	public Button nodeIDTooltip;
	public Button validDurationTooltip;
	public Button generateRecordTooltip;
	public Button startTimeTooltip;
	public Button maxFeeTooltip;
	public Button autoRenewTooltip;
	public Button folderTooltip;
	public Button feePayerTooltip;
	public TextField versionLabel;
	public Button networkTooltip;
	public ChoiceBox<Object> networkChoicebox;
	public ChoiceBox<Object> feePayerChoicebox;

	@FXML
	private MainController controller;

	DriveSetupHelper driveSetupHelper;

	private final HbarCurrencyFormat hbarCurrencyFormat = new HbarCurrencyFormat();
	private final HbarCurrencyFormat hbarCurrencyFormatNoSymbol = new HbarCurrencyFormat(false);

	void injectMainController(final MainController controller) {
		this.controller = controller;
	}

	void initializeSettingsPane() {
		try {
			settingScrollPane.setFitToWidth(true);
			// bindings
			managedPropertyBinding(addFolderButton, addPathGridPane, pathGreenCheck, drivesErrorLabelSP,
					addFolderPathHBoxSP, tvsErrorLabel, confirmAddFolderButtonSP, cancelAddToEmailMapButton,
					browseNewFolderButton, deleteImage, editImage, customFeePayerTextField, feePayerChoicebox,
					addCustomPayerButton1, addCustomPayerButton);

			feePayerChoicebox.visibleProperty().bind(customFeePayerTextField.visibleProperty().not());
			customFeePayerTextField.setOnKeyReleased(event -> {
				final var code = event.getCode();
				if (code.equals(KeyCode.ENTER) || code.equals(KeyCode.TAB)) {
					feePayerChoicebox.getParent().requestFocus();
				}
			});

			addCustomPayerButton1.visibleProperty().bind(customFeePayerTextField.visibleProperty());
			addCustomPayerButton.visibleProperty().bind(customFeePayerTextField.visibleProperty().not());

			addCustomPayerButton1.setOnAction(actionEvent -> customFeePayerTextField.getParent().requestFocus());

			customFeePayerTextField.focusedProperty().addListener(
					(observableValue, aBoolean, t1) -> addCustomFeePayer(t1));

			//Initialize drive builder
			driveSetupHelper = DriveSetupHelper.Builder.aDriveSetupHelper()
					.withController(controller)
					.withDriveLimit(DRIVE_LIMIT)
					.withPathGreenCheck(pathGreenCheck)
					.withEmailGreenCheck(emailGreenCheck)
					.withPathTextField(pathTextFieldSP)
					.withEmailTextField(emailTextFieldSP)
					.withDrivesErrorLabel(drivesErrorLabelSP)
					.withAddToEmailMapButton(addFolderButton)
					.withConfirmAddFolderButton(confirmAddFolderButtonSP)
					.withBrowseNewFolderButton(browseNewFolderButton)
					.withAddFolderPathHBox(addFolderPathHBoxSP)
					.withCancelAddToEmailMapButton(cancelAddToEmailMapButton)
					.withTransactionFoldersVBox(transactionFoldersVBoxSP)
					.withAddPathGridPane(addPathGridPane)
					.withTempProperties(controller.getProperties())
					.withDeleteImage(deleteImage.getImage())
					.withEditImage(editImage.getImage())
					.build();

			loadStorageTextField.setText(controller.getPreferredStorageDirectory());
			defaultTransactionFee.setText(hbarCurrencyFormatNoSymbol.format(controller.getDefaultTxFee(), HbarUnit.TINYBAR).replace(" ", ""));
			hoursTextField.setText(String.valueOf(controller.getDefaultHours()));
			minutesTextField.setText(String.format("%02d", controller.getDefaultMinutes()));
			secondsTextField.setText(String.format("%02d", controller.getDefaultSeconds()));

			final Identifier defaultNodeID = Identifier.parse(controller.getDefaultNodeID());
			nodeIDTextField.setText(defaultNodeID.toNicknameAndChecksum(controller.getAccountsList()));
			nodeIDTextField.setEditable(true);

			txValidDurationTextField.setText(String.valueOf(controller.getTxValidDuration()));
			autoRenewPeriodTextField.setText(String.valueOf(controller.getAutoRenewPeriod()));

			localTimeLabel.setText(getLocalTime());

			addPathGridPane.setVisible(false);

			driveSetupHelper.refreshTransactionsFolderVBox();

			readVersion();

			// region Events
			setupNodeIDTextField();

			setupNetworkBox();

			setupTxValidDurationTextField();

			setupAutoRenewTextField();

			setupHoursField();

			setupMinutesField();

			setupSecondsField();

			setupDefaultTransactionFeeTextField();

			setupFeePayerChoicebox();

			generateRecordSlider.selectedProperty().addListener(
					(observableValue, aBoolean, t1) -> {
						generateRecordLabel.setText(Boolean.TRUE.equals(t1) ? "yes" : "no");
						controller.setGenerateRecord(t1);
					});

			// endregion

			// region Tooltips
			validDurationTooltip.setOnAction(
					actionEvent -> CommonMethods.showTooltip(controller.settingsPane, validDurationTooltip,
							VALID_DURATION_TOOLTIP_MESSAGE));

			generateRecordTooltip.setOnAction(
					actionEvent -> CommonMethods.showTooltip(controller.settingsPane, generateRecordTooltip,
							GENERATE_RECORD_TOOLTIP_MESSAGE));

			startTimeTooltip.setOnAction(
					actionEvent -> CommonMethods.showTooltip(controller.settingsPane, startTimeTooltip,
							START_TIME_TOOLTIP_MESSAGE));

			folderTooltip.setOnAction(actionEvent -> CommonMethods.showTooltip(controller.settingsPane, folderTooltip,
					FOLDER_TOOLTIP_MESSAGES));

			networkTooltip.setOnAction(actionEvent -> CommonMethods.showTooltip(controller.settingsPane, networkTooltip,
					NETWORKS_TOOLTIP_MESSAGES));

			feePayerTooltip.setOnAction(
					actionEvent -> CommonMethods.showTooltip(controller.settingsPane, feePayerTooltip,
							FEE_PAYER_TOOLTIP_MESSAGES));
			// endregion

		} catch (final Exception e) {
			LOG.error(e.getStackTrace());
			controller.displaySystemMessage(e);
		}
	}

	private void addCustomFeePayer(final Boolean t1) {
		if (Boolean.FALSE.equals(t1)) {
			final var tempSet = new HashSet<>(controller.getFeePayers());
			tempSet.addAll(controller.getCustomFeePayers());

			if ("".equals(customFeePayerTextField.getText())) {
				return;
			}

			try {
				final var network =
						networkChoicebox.getValue() instanceof String ? (String) networkChoicebox.getValue() : "";
				final var id =
						Identifier.parse(customFeePayerTextField.getText(), network);
				controller.setDefaultFeePayer(id);
				customFeePayerTextField.setVisible(false);
				customFeePayerTextField.clear();
				if (!tempSet.contains(id)) {
					controller.addCustomFeePayer(id);
				}
				setupFeePayerChoicebox();
				controller.accountsPaneController.setupFeePayerChoiceBox();
			} catch (final Exception e) {
				LOG.error("Cannot parse identifier {}", e.getMessage());
				PopupMessage.display("Error", "Cannot parse your input to an account. Please try again.");
				customFeePayerTextField.requestFocus();
				customFeePayerTextField.setVisible(true);
			}
		}
	}

	/**
	 * Setup for fee payer choicebox
	 */
	public void setupFeePayerChoicebox() {
		noise = true;
		final var feePayer = controller.setupChoiceBoxFeePayer(feePayerChoicebox, customFeePayerTextField);
		noise = false;

		if ("".equals(feePayer)) {
			return;
		}
		feePayerChoicebox.getSelectionModel().select(feePayer);
		feePayerChoicebox.getSelectionModel().selectedItemProperty().addListener((observableValue, o, t1) -> {
			if (t1 instanceof String && !"".equals(t1)) {
				final var text = (String) t1;
				final var idString =
						(text.contains("(")) ? text.substring(text.indexOf("(") + 1, text.indexOf(")")) : text;
				deleteCustomPayerButton.setDisable(
						controller.getFeePayers().contains(Identifier.parse(idString, controller.getCurrentNetwork())));
				controller.setDefaultFeePayer(Identifier.parse(text, controller.getCurrentNetwork()));
			}
		});
	}

	private void updateNetworkBox() {
		noise = true;
		controller.networkBoxSetup(networkChoicebox);
		noise = false;
		networkChoicebox.getSelectionModel().select(controller.getCurrentNetwork());
	}

	private void setupNetworkBox() {
		networkChoicebox.getSelectionModel().selectedItemProperty().addListener(
				(observableValue, oldValue, newValue) -> {
					if (!noise) {
						try {
							if (newValue instanceof String) {
								controller.setCurrentNetwork((String) newValue);
								deleteCustomNetworkButton.setDisable(
										!controller.getCustomNetworks().contains(controller.getCurrentNetwork()));
								controller.setupChoiceBoxFeePayer(feePayerChoicebox, customFeePayerTextField);
							} else {
								throw new IllegalStateException(
										"Only a String should be selected by the network Combobox!");
							}
							final String account = calcNodeId(controller.getDefaultNodeID()).toNicknameAndChecksum(
									controller.getAccountsList());
							nodeIDTextField.setText(account);
							checkNodeID();
						} catch (final Exception e) {
							LOG.error("Error in network selection", e);
							networkChoicebox.getSelectionModel().select(oldValue);
						}
					}
				});
		updateNetworkBox();
	}


	private void setupDefaultTransactionFeeTextField() {
//		this looks to be an attempt to limit input
//		defaultTransactionFee.textProperty().addListener((observable, oldValue, newValue) -> {
//			if (!newValue.matches(REGEX1)) {
//				defaultTransactionFee.setText(newValue.replaceAll("[^\\d. ]", ""));
//			}
//		});

		defaultTransactionFee.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				checkFee();
			}
		});

		defaultTransactionFee.setOnKeyPressed(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.TAB)) {
				checkFee();
			}
		});

		//This only does the logging, the validation needs to happen in the textFormatter
		defaultTransactionFee.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				LOG.info("Transaction fee text field changed to: {}", defaultTransactionFee.getText());
				checkFee();
			}
		});

		maxFeeTooltip.setOnAction(actionEvent -> CommonMethods.showTooltip(controller.settingsPane, maxFeeTooltip,
				TRANSACTION_FEE_TOOLTIP_MESSAGE));

//		this is preventing input, highlight, etc.
		//the issue is that if the value present doesn't match, it is unchagneable.
		//the value should never not match, but if there is a bug that saves the wrong value (allows it to be saved)
		//then everything is broken.
		defaultTransactionFee.setTextFormatter(new TextFormatter<>(change ->
				change.getControlNewText().matches(HBAR_POSITIVE_FORMAT_STRING) ? change : null));
//		the cursor in the skin is wrong, and as I was typing (426525555555) at some point the value deisplayued was
//		way off, then it turned into 50 000 000 000 hsdfasdfasdf
//			h needs a 'show symbol' type of method, as this deosn't want the currentrecny, but other places do'
//		defaultTransactionFee.setSkin(new HbarFieldSkin(defaultTransactionFee));
//		defaultTransactionFee.setTextFormatter(new TextFormatter<String>(new StringConverter<String>() {
//			final HbarCurrencyFormat format = new HbarCurrencyFormat(false);
//			@Override
//			public String toString(String object) {
//				return format.format(object);
//			}
//
//			@Override
//			public String fromString(String string) {
//				return format.parse(string).getValue().toPlainString();
//			}
//		}, "0", change ->
//				change.getControlNewText().matches(HBAR_POSITIVE_FORMAT_STRING) ? change : null));
	}

	private void setupHoursField() {
		hoursTextField.textProperty().addListener((observable, oldValue, newValue) -> {
			if (!newValue.matches(REGEX1)) {
				hoursTextField.setText(newValue.replaceAll(REGEX, ""));
			}
		});

		hoursTextField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				checkHours();
			}
		});

		hoursTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				LOG.info("Hours text field changed to: {}", hoursTextField.getText());
				checkHours();
			}
		});
	}

	private void setupMinutesField() {
		minutesTextField.textProperty().addListener((observable, oldValue, newValue) -> {
			if (!newValue.matches(REGEX1)) {
				minutesTextField.setText(newValue.replaceAll(REGEX, ""));
			}
		});

		minutesTextField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				checkMinutes();
			}
		});

		minutesTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				LOG.info("Minute text field changed to: {}", minutesTextField.getText());
				checkMinutes();
			}
		});
	}

	private void setupSecondsField() {
		secondsTextField.textProperty().addListener((observable, oldValue, newValue) -> {
			if (!newValue.matches(REGEX1)) {
				secondsTextField.setText(newValue.replaceAll(REGEX, ""));
			}
		});

		secondsTextField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				checkSeconds();
			}
		});

		secondsTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				LOG.info("Second text field changed to: {}", secondsTextField.getText());
				checkSeconds();
			}
		});
	}

	private void setupAutoRenewTextField() {
		autoRenewPeriodTextField.textProperty().addListener((observable, oldValue, newValue) -> {
			if (!newValue.matches(REGEX1)) {
				autoRenewPeriodTextField.setText(newValue.replaceAll(REGEX, ""));
			}
		});

		autoRenewPeriodTextField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				checkAutoRenewPeriod();
			}
		});

		autoRenewPeriodTextField.setOnKeyPressed(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.TAB)) {
				checkAutoRenewPeriod();
			}
		});

		autoRenewPeriodTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				LOG.info("Auto renew period text field changed to: {}", autoRenewPeriodTextField.getText());
				checkAutoRenewPeriod();
			}
		});

		autoRenewTooltip.setOnAction(
				actionEvent -> CommonMethods.showTooltip(controller.settingsPane, autoRenewTooltip,
						ToolTipMessages.AUTO_RENEW_PERIOD_TOOLTIP_MESSAGE));


	}

	private void setupTxValidDurationTextField() {
		txValidDurationTextField.textProperty().addListener((observable, oldValue, newValue) -> {
			if (!newValue.matches(REGEX1)) {
				txValidDurationTextField.setText(newValue.replaceAll(REGEX, ""));
			}
		});

		txValidDurationTextField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				checkTransactionValidDuration();
			}
		});

		txValidDurationTextField.setOnKeyPressed(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.TAB)) {
				checkTransactionValidDuration();
			}
		});

		txValidDurationTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				LOG.info("Transaction valid duration text field changed to: {}",
						txValidDurationTextField.getText());
				checkTransactionValidDuration();
			}
		});
	}

	private void setupNodeIDTextField() {
		nodeIDTextField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER)) {
				checkNodeID();
			}
		});

		nodeIDTextField.setOnKeyPressed(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.TAB)) {
				checkNodeID();
			}
		});

		nodeIDTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (Boolean.FALSE.equals(newPropertyValue)) {
				checkNodeID();
			}
		});

		nodeIDTooltip.setOnAction(actionEvent -> CommonMethods.showTooltip(controller.settingsPane, nodeIDTooltip,
				NODE_ID_TOOLTIP_MESSAGE));

	}

	private void managedPropertyBinding(final Node... nodes) {
		for (final var n : nodes) {
			n.managedProperty().bind(n.visibleProperty());
		}
	}

	private void checkFee() {
		final var fee = hbarCurrencyFormatNoSymbol.parse(defaultTransactionFee.getText()).toTinybars();
		controller.setDefaultTxFee(fee);
//		defaultTransactionFee.setText(hbarCurrencyFormatNoSymbol.format(controller.getDefaultTxFee()).replace(" ", ""));
	}

	private void checkSeconds() {
		var s = Integer.parseInt(secondsTextField.getText());
		if (s > 59) {
			secondsTextField.setText(Integer.toString(59));
			s = 59;
		}
		controller.setDefaultSeconds(s);
		secondsTextField.setText(String.format("%02d", controller.getDefaultSeconds()));
		localTimeLabel.setText(getLocalTime());
	}

	private void checkMinutes() {
		var m = Integer.parseInt(minutesTextField.getText());
		if (m > 59) {
			minutesTextField.setText(Integer.toString(59));
			m = 59;
		}
		controller.setDefaultMinutes(m);
		minutesTextField.setText(String.format("%02d", controller.getDefaultMinutes()));
		localTimeLabel.setText(getLocalTime());
	}

	private void checkHours() {
		var h = Integer.parseInt(hoursTextField.getText());
		if (h > 23) {
			hoursTextField.setText(Integer.toString(23));
			h = 23;
		}
		controller.setDefaultHours(h);
		localTimeLabel.setText(getLocalTime());
	}

	private void checkTransactionValidDuration() {
		final var duration = Integer.parseInt(txValidDurationTextField.getText());
		if (duration < 1 || duration > 180) {
			tvsErrorLabel.setVisible(true);
		} else {
			controller.setTxValidDuration(duration);
			txValidDurationTextField.setText(String.valueOf(controller.getTxValidDuration()));
			tvsErrorLabel.setVisible(false);
			settingScrollPane.requestFocus();
		}
	}

	private void checkAutoRenewPeriod() {
		final var duration = Integer.parseInt(autoRenewPeriodTextField.getText());
		if (duration < MINIMUM_AUTO_RENEW_PERIOD || duration > MAXIMUM_AUTO_RENEW_PERIOD) {
			arpErrorLabel.setVisible(true);
		} else {
			controller.setAutoRenewPeriod(duration);
			autoRenewPeriodTextField.setText(String.valueOf(controller.getAutoRenewPeriod()));
			arpErrorLabel.setVisible(false);
			settingScrollPane.requestFocus();
		}
	}

	private Identifier calcNodeId(final String account) {
		return Optional.ofNullable(networkChoicebox.getValue())
				.filter(value -> value instanceof String)
				.map(value -> (String) value)
				.map(network -> Identifier.parse(account, network))
				.orElseGet(() -> Identifier.parse(account));
	}

	private void checkNodeID() {
		final String account = nodeIDTextField.getText();
		try {
			LOG.info("Node ID text field changed to: {}", account);
			final Identifier calculatedIdentifier = calcNodeId(controller.getDefaultNodeID());
			final String calculatedAccount = calculatedIdentifier.toNicknameAndChecksum(controller.getAccountsList());
			if (Objects.equals(calculatedAccount, account)) {
				controller.setDefaultNodeID(calculatedIdentifier.toReadableString());
				accountIDErrorLabel.setVisible(false);
			} else {
				final Identifier accountID = calcNodeId(account);
				if (accountID.isValid()) {
					controller.setDefaultNodeID(accountID.toReadableString());
					nodeIDTextField.setText(accountID.toNicknameAndChecksum(controller.getAccountsList()));
					settingScrollPane.requestFocus();
					accountIDErrorLabel.setVisible(false);
				} else {
					accountIDErrorLabel.setVisible(true);
					LOG.error("Invalid NodeID: '{}'", account);
				}
			}
		} catch (final Exception e) {
			accountIDErrorLabel.setVisible(true);
			LOG.error("Invalid NodeID: '" + account + "'", e);
		}
	}

	private String getLocalTime() {
		final var localDateTime = LocalDateTime.of(LocalDate.now(),
				LocalTime.of(controller.getDefaultHours(), controller.getDefaultMinutes(),
						controller.getDefaultSeconds()));
		final var transactionValidStart = Date.from(localDateTime.atZone(ZoneId.of("UTC")).toInstant());
		final var localDateFormat = new SimpleDateFormat("HH:mm:ss");

		final var tz = TimeZone.getDefault();

		return localDateFormat.format(transactionValidStart) + " " + tz.getDisplayName();
	}

	//region SETTINGS
	public void browseStorageIconPressed() throws IOException {
		final var directory =
				BrowserUtilities.browseDirectories(controller.getPreferredStorageDirectory(), controller.getThisPane());
		//If the user didn't choose a directory (by clicking 'Cancel')
		if (directory.isEmpty()) {
			return;
		}
		final var previous = new File(controller.getPreferredStorageDirectory());
		final var newDir = new File(directory + "/TransactionTools");
		controller.setLastBrowsedDirectory(newDir);

		FileUtils.moveDirectory(previous, newDir);
		loadStorageTextField.setText(directory + "/TransactionTools");
		controller.setPreferredStorageDirectory(loadStorageTextField.getText());
		LOG.info("Storage directory set to: {}", controller.getPreferredStorageDirectory());
	}


	//endregion

	// region Validation, Save, Cancel

	public void resetApplication() throws BackingStoreException, IOException {

		final var continueType = new ButtonType("CONTINUE", ButtonBar.ButtonData.OK_DONE);
		final var cancelType = new ButtonType("CANCEL", ButtonBar.ButtonData.CANCEL_CLOSE);
		final var a = new Alert(AlertType.WARNING, Messages.RESET_ALERT_MESSAGE, cancelType, continueType
		);
		final var result = a.showAndWait();
		if (result.isPresent() && result.get() == continueType) {
			controller.resetApp();
		}
	}


	/**
	 * Read the version from build.properties file. Version is displayed as
	 * Version, Time the last build is done in UTC, Last commit ID in the build.
	 */
	public void readVersion() {
		final var version = controller.getVersion();
		versionLabel.setText(version.replace("Version: ", "v"));
		versionLabel.setStyle("-fx-background-color: white; -fx-border-color: white");
		versionLabel.setPrefWidth(USE_COMPUTED_SIZE);
		versionLabel.setPrefHeight(USE_COMPUTED_SIZE);
	}

	// endregion Save or Cancel

	// region ADD FOLDER
	public void addNewFolderAction() {
		driveSetupHelper.addNewFolderAction();
	}

	public void browseNewFolderAction() {
		final var directory = BrowserUtilities.browseDirectories("", controller.getThisPane());
		//If the user didn't choose a directory (by clicking 'Cancel')
		if (directory.isEmpty()) {
			return;
		}

		pathTextFieldSP.setText(directory);
		driveSetupHelper.validateOneDrivePathAction();
	}

	public void addFolderEmailToMapAction() {
		driveSetupHelper.validateEmailAction();
	}

	public void cancelAddToEmailMap() {
		driveSetupHelper.cancelAddToEmailMap();
	}

	public void addCustomNetworkAction() throws IOException {
		if (new File(CUSTOM_NETWORK_FOLDER).mkdirs()) {
			LOG.info("Folder {} created", CUSTOM_NETWORK_FOLDER);
		}
		final JsonObject customNetwork = NewNetworkPopup.display();
		if (!customNetwork.has("nickname") || !customNetwork.has("file")) {
			LOG.info("Invalid custom network");
			return;
		}
		final var nickname = customNetwork.get("nickname").getAsString();
		final var filename = nickname + "." + Constants.JSON_EXTENSION;
		final var location = customNetwork.get("file").getAsString();
		if (!verifyJsonNetwork(location)) {
			PopupMessage.display("Error", "The json file does not contain a valid network");
			return;
		}
		FileUtils.copyFile(new File(location), new File(CUSTOM_NETWORK_FOLDER, filename));
		final var customNetworks = controller.getCustomNetworks();
		if (!customNetworks.contains(nickname)) {
			throw new HederaClientRuntimeException("Unrecognized custom network");
		}
		controller.setCurrentNetwork(nickname);
		updateNetworkBox();
	}

	private boolean verifyJsonNetwork(final String location) {
		try {
			final var array = readJsonArray(location);
			for (final JsonElement jsonElement : array) {
				final var node = jsonElement.getAsJsonObject();
				if (!node.has("accountID")) {
					return false;
				}
				if (!node.has("ipAddress")) {
					return false;
				}
				if (!node.has("port")) {
					return false;
				}
				Identifier.parse(node.get("accountID").getAsString());
				InetAddresses.forString(node.get("ipAddress").getAsString());
				final var port = node.get("port").getAsInt();
				if (port < 49152 || port > 65535) {
					return false;
				}
			}
		} catch (final Exception e) {
			LOG.error(e.getMessage());
			return false;
		}
		return true;
	}

	public void deleteCustomNetworkAction() throws IOException {
		final var answer = PopupMessage.display("Delete Network",
				"This will remove the selected network from your app. Are you sure?", true, "CONTINUE", "CANCEL");
		if (Boolean.TRUE.equals(answer)) {
			Files.deleteIfExists(
					Path.of(CUSTOM_NETWORK_FOLDER, controller.getCurrentNetwork() + "." + Constants.JSON_EXTENSION));
		}
		controller.setCurrentNetwork("MAINNET");
		updateNetworkBox();
	}

	public void addFeePayerAction() {
		customFeePayerTextField.setVisible(true);
	}

	public void deleteFeePayerAction() {
		final var selectedItem = feePayerChoicebox.getSelectionModel().getSelectedItem();
		if (!(selectedItem instanceof String)) {
			return;
		}
		controller.removeCustomFeePayer(Identifier.parse((String) selectedItem, controller.getCurrentNetwork()));

		final var allPayers = new TreeSet<>(controller.getFeePayers());
		allPayers.addAll(controller.getCustomFeePayers());

		if (!allPayers.isEmpty()) {
			final var choice = allPayers.first();
			feePayerChoicebox.setValue(choice.toNicknameAndChecksum(controller.getAccountsList()));
			controller.setDefaultFeePayer(choice);
			setupFeePayerChoicebox();
			controller.accountsPaneController.setupFeePayerChoiceBox();
			return;
		}
		final var zero = Identifier.ZERO;
		zero.setNetworkName(controller.getCurrentNetwork());
		controller.setDefaultFeePayer(zero);
		controller.accountsPaneController.initializeAccountPane();
		addFeePayerAction();
	}


	// endregion

}
