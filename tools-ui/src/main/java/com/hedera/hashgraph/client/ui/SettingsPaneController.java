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

import com.hedera.hashgraph.client.core.constants.Messages;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.ui.utilities.DriveSetupHelper;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.ToggleSwitch;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.TimeZone;
import java.util.prefs.BackingStoreException;

import static com.hedera.hashgraph.client.core.constants.Constants.DRIVE_LIMIT;
import static com.hedera.hashgraph.client.core.constants.Constants.MAXIMUM_AUTO_RENEW_PERIOD;
import static com.hedera.hashgraph.client.core.constants.Constants.MINIMUM_AUTO_RENEW_PERIOD;
import static javafx.scene.control.Alert.AlertType;
import static javafx.scene.control.Control.USE_COMPUTED_SIZE;

public class SettingsPaneController {

	private static final Logger logger = LogManager.getLogger(SettingsPaneController.class);
	private static final String REGEX = "[^\\d]";
	private static final String REGEX1 = "\\d*";

	// todo move to config file

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

	public Label loadStorageLabel;
	public Label nodeIDLabel;
	public Label txValidDurationLabel;
	public Label generateRecordLabel;
	public Label localTimeLabel;
	public Label tvsErrorLabel;
	public Label arpErrorLabel;
	public Label accountIDErrorLabel;

	public Button addFolderButton;
	public Button confirmAddFolderButton;
	public Button browseNewFolderButton;
	public Button cancelAddToEmailMapButton;


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
	public Label versionLabel;
	public ImageView deleteImage;
	public ImageView editImage;
	public Button nodeIDTooltip;
	public Button validDurationTooltip;
	public Button generateRecordTooltip;
	public Button startTimeTooltip;
	public Button maxFeeTooltip;
	public Button autoRenewTooltip;
	public Button folderTooltip;


	DriveSetupHelper driveSetupHelper;

	@FXML
	private Controller controller;

	void injectMainController(Controller controller) {
		this.controller = controller;
	}

	void initializeSettingsPane() {
		try {
			settingScrollPane.setFitToWidth(true);
			// bindings
			managedPropertyBinding(addFolderButton, addPathGridPane, pathGreenCheck, drivesErrorLabelSP,
					addFolderPathHBoxSP, tvsErrorLabel, confirmAddFolderButton, cancelAddToEmailMapButton,
					browseNewFolderButton, deleteImage, editImage);

			versionLabel.setPrefWidth(USE_COMPUTED_SIZE);

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
					.withConfirmAddFolderButton(confirmAddFolderButton)
					.withBrowseNewFolderButton(browseNewFolderButton)
					.withAddFolderPathHBox(addFolderPathHBoxSP)
					.withCancelAddToEmailMapButton(cancelAddToEmailMapButton)
					.withTransactionFoldersVBox(transactionFoldersVBoxSP)
					.withAddPathGridPane(addPathGridPane)
					.withTempProperties(controller.properties)
					.withDeleteImage(deleteImage.getImage())
					.withEditImage(editImage.getImage())
					.build();

			loadStorageTextField.setText(controller.getPreferredStorageDirectory());
			defaultTransactionFee.setText(
					Utilities.setHBarFormat(controller.properties.getDefaultTxFee()).replace("\u0127", ""));
			hoursTextField.setText(String.valueOf(controller.properties.getDefaultHours()));
			minutesTextField.setText(String.format("%02d", controller.properties.getDefaultMinutes()));
			secondsTextField.setText(String.format("%02d", controller.properties.getDefaultSeconds()));
			nodeIDTextField.setText(controller.properties.getDefaultNodeID());
			nodeIDTextField.setEditable(true);
			txValidDurationTextField.setText(String.valueOf(controller.properties.getTxValidDuration()));
			autoRenewPeriodTextField.setText(String.valueOf(controller.properties.getAutoRenewPeriod()));

			localTimeLabel.setText(getLocalTime());

			addPathGridPane.setVisible(false);

			driveSetupHelper.refreshTransactionsFolderVBox();

			readVersion();

			// region Events
			nodeIDTextField.textProperty().addListener((observable, oldValue, newValue) -> {
				if (!newValue.matches(REGEX1)) {
					nodeIDTextField.setText(newValue.replaceAll("[^\\d.]", ""));
				}
			});

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

			defaultTransactionFee.textProperty().addListener((observable, oldValue, newValue) -> {
				if (!newValue.matches(REGEX1)) {
					defaultTransactionFee.setText(newValue.replaceAll("[^\\d. ]", ""));
				}
			});

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

			generateRecordSlider.selectedProperty().addListener(
					(observableValue, aBoolean, t1) -> {
						generateRecordLabel.setText((t1) ? "yes" : "no");
						controller.properties.setGenerateRecord(t1);
					});

			// endregion
			// region FOCUS EVENTS
			hoursTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (!newPropertyValue) {
					logger.info(String.format("Hours text field changed to: %s", hoursTextField.getText()));
					checkHours();
				}
			});
			minutesTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (!newPropertyValue) {
					logger.info(String.format("Minute text field changed to: %s", minutesTextField.getText()));
					checkMinutes();
				}
			});
			secondsTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (!newPropertyValue) {
					logger.info(String.format("Second text field changed to: %s", secondsTextField.getText()));
					checkSeconds();
				}
			});
			nodeIDTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (!newPropertyValue) {
					logger.info(String.format("Node ID text field changed to: %s", nodeIDTextField.getText()));
					checkNodeID();
				}
			});
			txValidDurationTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (!newPropertyValue) {
					logger.info(String.format("Transaction valid duration text field changed to: %s",
							txValidDurationTextField.getText()));
					checkTransactionValidDuration();
				}
			});
			autoRenewPeriodTextField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (!newPropertyValue) {
					logger.info(String.format("Auto renew period text field changed to: %s",
							autoRenewPeriodTextField.getText()));
					checkAutoRenewPeriod();
				}
			});
			defaultTransactionFee.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (!newPropertyValue) {
					logger.info(
							String.format("Transaction fee text field changed to: %s",
									defaultTransactionFee.getText()));
					checkTransactionFee();
				}
			});
			// endregion

			// region Tooltips
			nodeIDTooltip.setOnAction(actionEvent -> Utilities.showTooltip(controller.settingsPane, nodeIDTooltip,
					"The account ID of the node that will submit the transaction to the Hedera network"));

			validDurationTooltip.setOnAction(
					actionEvent -> Utilities.showTooltip(controller.settingsPane, validDurationTooltip,
							"The period of time in " +
									"seconds for when the transaction is valid on the Hedera network.\n" +
									"Min: 30 seconds Max: 180 seconds"));

			generateRecordTooltip.setOnAction(
					actionEvent -> Utilities.showTooltip(controller.settingsPane, generateRecordTooltip,
							"Whether the transaction should generate a record or not"));

			startTimeTooltip.setOnAction(
					actionEvent -> Utilities.showTooltip(controller.settingsPane, startTimeTooltip,
							"The start time of the transaction from which the transaction valid duration begins in " +
									"UTC" +
									" " +
									"format."));

			maxFeeTooltip.setOnAction(actionEvent -> Utilities.showTooltip(controller.settingsPane, maxFeeTooltip,
					"The max transaction fee that will be offered"));

			autoRenewTooltip.setOnAction(
					actionEvent -> Utilities.showTooltip(controller.settingsPane, autoRenewTooltip,
							"The period of time in which the account will renew in seconds.\n" +
									"Min:7000000 seconds \n" +
									"Max: 8000000 seconds"));

			folderTooltip.setOnAction(actionEvent -> Utilities.showTooltip(controller.settingsPane, folderTooltip,
					"The shared folder must contain the InputFiles and OutputFiles directories."));


			// endregion

		} catch (Exception e) {
			logger.error(e.getStackTrace());
			controller.displaySystemMessage(e);
		}
	}

	private void managedPropertyBinding(Node... nodes) {
		for (var n : nodes) {
			n.managedProperty().bind(n.visibleProperty());
		}
	}

	private void checkFee() {

		var txFee = defaultTransactionFee.getText().replace(" ", "") + "00000000";

		if (txFee.contains(".")) {
			txFee = txFee.substring(0, txFee.lastIndexOf(".") + 9).replace(".", "");
		}

		var fee = Long.parseLong(txFee);

		controller.properties.setDefaultTxFee(fee);
		defaultTransactionFee.setText(Utilities.setHBarFormat(controller.properties.getDefaultTxFee()));

	}

	private void checkSeconds() {
		var s = Integer.parseInt(secondsTextField.getText());
		if (s > 59) {
			secondsTextField.setText(Integer.toString(59));
			s = 59;
		}
		controller.properties.setDefaultSeconds(s);
		secondsTextField.setText(String.format("%02d", controller.properties.getDefaultSeconds()));
		localTimeLabel.setText(getLocalTime());
	}

	private void checkMinutes() {
		var m = Integer.parseInt(minutesTextField.getText());
		if (m > 59) {
			minutesTextField.setText(Integer.toString(59));
			m = 59;
		}
		controller.properties.setDefaultMinutes(m);
		minutesTextField.setText(String.format("%02d", controller.properties.getDefaultMinutes()));
		localTimeLabel.setText(getLocalTime());
	}

	private void checkHours() {
		var h = Integer.parseInt(hoursTextField.getText());
		if (h > 23) {
			hoursTextField.setText(Integer.toString(23));
			h = 23;
		}
		controller.properties.setDefaultHours(h);
		localTimeLabel.setText(getLocalTime());
	}

	private void checkTransactionValidDuration() {
		var duration = Integer.parseInt(txValidDurationTextField.getText());
		if (duration < 1 || duration > 180) {
			tvsErrorLabel.setVisible(true);
		} else {
			controller.properties.setTxValidDuration(duration);
			txValidDurationTextField.setText(String.valueOf(controller.properties.getTxValidDuration()));
			tvsErrorLabel.setVisible(false);
			settingScrollPane.requestFocus();
		}
	}

	private void checkAutoRenewPeriod() {
		var duration = Integer.parseInt(autoRenewPeriodTextField.getText());
		if (duration < MINIMUM_AUTO_RENEW_PERIOD || duration > MAXIMUM_AUTO_RENEW_PERIOD) {
			arpErrorLabel.setVisible(true);
		} else {
			controller.properties.setAutoRenewPeriod(duration);
			autoRenewPeriodTextField.setText(String.valueOf(controller.properties.getAutoRenewPeriod()));
			arpErrorLabel.setVisible(false);
			settingScrollPane.requestFocus();
		}
	}

	private void checkTransactionFee() {
		var transactionFee = Long.parseLong(Utilities.stripHBarFormat(defaultTransactionFee.getText()));
		controller.properties.setDefaultTxFee(transactionFee);
		defaultTransactionFee.setText(Utilities.setHBarFormat(transactionFee));
	}

	private void checkNodeID() {
		var account = nodeIDTextField.getText();
		Identifier accountID;
		try {
			accountID = Identifier.parse(account);
			if (accountID.isValid()) {
				controller.properties.setDefaultNodeID(accountID.toReadableString());
				nodeIDTextField.clear();
				nodeIDTextField.setText(controller.properties.getDefaultNodeID());
				settingScrollPane.requestFocus();
				accountIDErrorLabel.setVisible(false);
			} else {
				accountIDErrorLabel.setVisible(true);
			}
		} catch (Exception e) {
			// empty catch
		}
	}

	private String getLocalTime() {
		var localDateTime = LocalDateTime.of(LocalDate.now(),
				LocalTime.of(controller.properties.getDefaultHours(), controller.properties.getDefaultMinutes(),
						controller.properties.getDefaultSeconds()));
		var transactionValidStart = Date.from(localDateTime.atZone(ZoneId.of("UTC")).toInstant());
		var localDateFormat = new SimpleDateFormat("HH:mm:ss");

		var tz = TimeZone.getDefault();

		return localDateFormat.format(transactionValidStart) + " " + tz.getDisplayName();
	}

	//region SETTINGS
	public void browseStorageIconPressed() throws IOException {
		var directory =
				BrowserUtilities.browseDirectories(controller.getPreferredStorageDirectory(), controller.thisPane);
		//If the user didn't choose a directory (by clicking 'Cancel')
		if (directory.isEmpty()) {
			return;
		}
		var previous = new File(controller.getPreferredStorageDirectory());
		var newDir = new File(directory + "/TransactionTools");
		controller.setLastTransactionsDirectory(newDir);

		FileUtils.moveDirectory(previous, newDir);
		loadStorageTextField.setText(directory + "/TransactionTools");
		controller.setPreferredStorageDirectory(loadStorageTextField.getText());
		logger.info(String.format("Storage directory set to: %s", controller.getPreferredStorageDirectory()));
	}


	//endregion

	// region Validation, Save, Cancel

	public void resetApplication() throws BackingStoreException, IOException {

		var continueType = new ButtonType("CONTINUE", ButtonBar.ButtonData.OK_DONE);
		var cancelType = new ButtonType("CANCEL", ButtonBar.ButtonData.CANCEL_CLOSE);
		var a = new Alert(AlertType.WARNING, Messages.RESET_ALERT_MESSAGE, cancelType, continueType
		);
		var result = a.showAndWait();
		if (result.isPresent()) {
			if (result.get() == continueType) {
				controller.resetApp();
			}
		}
	}


	/**
	 * Read the version from build.properties file. Version is displayed as
	 * Version, Time the last build is done in UTC, Last commit ID in the build.
	 */
	public void readVersion() {
		versionLabel.setText(controller.getVersion());
	}

	// endregion Save or Cancel

	// region ADD FOLDER
	public void addNewFolderAction() {
		driveSetupHelper.addNewFolderAction();
	}

	public void browseNewFolderAction() {
		var directory = BrowserUtilities.browseDirectories("", controller.thisPane);
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


	// endregion

}
