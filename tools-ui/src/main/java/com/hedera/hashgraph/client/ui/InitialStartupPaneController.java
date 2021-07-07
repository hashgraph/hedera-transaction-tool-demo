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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.ui;


import com.codahale.passpol.BreachDatabase;
import com.codahale.passpol.PasswordPolicy;
import com.codahale.passpol.Status;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.Messages;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.utilities.DriveSetupHelper;
import com.hedera.hashgraph.client.ui.utilities.MnemonicPhraseHelper;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.DRIVE_LIMIT;
import static com.hedera.hashgraph.client.core.constants.Constants.INITIAL_MAP_LOCATION;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.checkPasswordPolicy;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.deleteDirectory;

public class InitialStartupPaneController implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(InitialStartupPaneController.class);

	UserAccessibleProperties properties;
	private char[] password;
	private DriveSetupHelper driveSetupHelper;
	private MnemonicPhraseHelper mnemonicPhraseHelper;

	//	region FXML
	public VBox passphraseBox;
	public VBox drivesBox;
	public VBox mainVBox;
	public VBox scrollBoxParent;
	public VBox finishBox;
	public VBox transactionFoldersVBoxIS;
	public VBox phraseBox;
	public VBox passwordBox;

	public HBox addFolderPathHBox;
	public HBox showEmailMapHBox;

	public ScrollPane scrollPane;
	public GridPane keysGridPane;
	public GridPane passwordGridPane;
	public GridPane addPathGridPaneIS;

	public ButtonBar generateKeyPairButtonBar;
	public ButtonBar addToEmailMapButtonBar;
	public ButtonBar passwordButtonBar;

	public Button generateKeys;
	public Button resetButton;
	public Button addToEmailMapButton;
	public Button cancelAddToEmailMapButtonIS;
	public Button copyToClipBoardButton;
	public Button pasteFromClipBoardButton;
	public Button confirmAddFolderButton;
	public Button acceptPasswordButton;
	public Button browseNewFolderButton;

	public Label characterCount;
	public Label passwordErrorLabel;
	public Label drivesErrorLabel;
	public Label mnemonicErrorMessage;
	public Label copyToClipboardLabel;

	public TextField hiddenPathInitial;
	public TextField pathTextField;
	public TextField emailTextField;

	public PasswordField appPasswordField;
	public PasswordField reEnterPasswordField;

	public ImageView checkPassword;
	public ImageView reCheckPassword;
	public ImageView emailGreenCheck;
	public ImageView pathGreenCheck;
	public ImageView deleteImageIS;
	public ImageView editImageIS;

	@FXML
	private Controller controller;
	//	endregion


	/**
	 * Constructor
	 */
	public InitialStartupPaneController() {
		logger.info(String.format("Default storage directory to: %s", DEFAULT_STORAGE));
	}

	/**
	 * Setups the transaction tool directory
	 *
	 * @param location
	 * 		location where the transaction tool directory will be built
	 */
	private void setupTransactionDirectory(String location) {
		var directory = new File(location);
		if (!directory.exists()) {
			if (!directory.mkdirs()) {
				logger.info("Directory already exists");
			}
		}

		if (new File(String.format("%s/Accounts/", location)).mkdirs()) {
			logger.info("Accounts info folder has been created");
		}
		if (new File(String.format("%s/Files/UserFiles", location)).mkdirs()) {
			logger.info("User files folder has been created");
		}
		if (new File(String.format("%s/Files/.System", location)).mkdirs()) {
			logger.info("System files folder has been created");
		}
		if (new File(String.format("%s/Keys/", location)).mkdirs()) {
			logger.info("Keys folder has been created");
		}
		if (new File(String.format("%s/History/", location)).mkdirs()) {
			logger.info("History folder has been created");
		}
		if (new File(String.format("%s/logs/", location)).mkdirs()) {
			logger.info("Log folder has been created");
		}
	}

	/**
	 * Inject the main controller
	 *
	 * @param controller
	 * 		main controller
	 */
	void injectMainController(Controller controller) {
		this.controller = controller;
	}

	/**
	 * Pane initialization
	 */
	void initializeStartupPane() {
		properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");

		controller.setDisableButtons(true);
		controller.menuButtonBar.setVisible(false);
		controller.setPreferredStorageDirectory(DEFAULT_STORAGE);
		setupTransactionDirectory(DEFAULT_STORAGE);

		keysGridPane.setVisible(true);
		passphraseBox.setVisible(true);
		generateKeys.setVisible(true);
		generateKeys.setText("GENERATE");
		copyToClipBoardButton.setVisible(false);

		resetDrivesBox();

		// Initialize the Mnemonic builder
		mnemonicPhraseHelper = MnemonicPhraseHelper.Builder.aMnemonicPhraseHelper()
				.withMnemonicErrorMessage(mnemonicErrorMessage)
				.withPhraseBox(phraseBox)
				.withStorageDirectory(controller.getPreferredStorageDirectory())
				.withCopyToClipBoardButton(copyToClipBoardButton)
				.withPasteFromClipBoardButton(pasteFromClipBoardButton)
				.withGenerateKeys(generateKeys)
				.withFinishBox(finishBox)
				.build();

		//Initialize drive builder
		driveSetupHelper = DriveSetupHelper.Builder.aDriveSetupHelper()
				.withController(controller)
				.withDriveLimit(DRIVE_LIMIT)
				.withPathGreenCheck(pathGreenCheck)
				.withEmailGreenCheck(emailGreenCheck)
				.withPathTextField(pathTextField)
				.withEmailTextField(emailTextField)
				.withDrivesErrorLabel(drivesErrorLabel)
				.withAddToEmailMapButton(addToEmailMapButton)
				.withConfirmAddFolderButton(confirmAddFolderButton)
				.withBrowseNewFolderButton(browseNewFolderButton)
				.withAddFolderPathHBox(addFolderPathHBox)
				.withCancelAddToEmailMapButton(cancelAddToEmailMapButtonIS)
				.withTransactionFoldersVBox(transactionFoldersVBoxIS)
				.withAddPathGridPane(addPathGridPaneIS)
				.withStorageBox(passphraseBox)
				.withTempProperties(properties)
				.withEditImage(editImageIS.getImage())
				.withDeleteImage(deleteImageIS.getImage())
				.build();

		hiddenPathInitial.clear();
		resetPasswordBox();

		resetPassphraseBox();
		finishBox.setVisible(false);

		// wait until the app is running to give focus to the password field
		Platform.runLater(() -> appPasswordField.requestFocus());

		setupPasswordEvents();

		// If a box is not visible, set it to un-managed
		setManagedProperties(drivesErrorLabel, addFolderPathHBox, addToEmailMapButton, drivesBox, passphraseBox,
				finishBox, generateKeyPairButtonBar, generateKeyPairButtonBar, mnemonicErrorMessage,
				copyToClipboardLabel, copyToClipBoardButton, pasteFromClipBoardButton);

		pasteFromClipBoardButton.visibleProperty().bind(copyToClipBoardButton.visibleProperty().not());

		// Auto scroll always to the bottom
		scrollPane.vvalueProperty().bind(mainVBox.heightProperty());
	}

	/**
	 * Finish the setup and initialize controllers
	 */
	public void finishSetup() {
		try {
			writeJsonObject(ACCOUNTS_MAP_FILE, new JsonObject());
		} catch (HederaClientException e) {
			logger.error(e);
		}

		controller.setSetupPhase(SetupPhase.NORMAL_OPERATION_PHASE);
		controller.properties.setLegacy(false);
		controller.homePane.setVisible(true);
		controller.setDisableButtons(false);

		controller.homePaneController.initializeHomePane();
		controller.accountsPaneController.initializeAccountPane();
		controller.keysPaneController.initializeKeysPane();
		controller.createPaneController.initializeCreatePane();
		controller.settingsPaneController.initializeSettingsPane();
		if (new File(INITIAL_MAP_LOCATION).delete()) {
			logger.info("Initial map file deleted");
		}
		controller.changeTab(controller.homePane);
		controller.menuButtonBar.setVisible(true);
	}

	/**
	 * If the user presses the reset button at any time, the process should restart from a clean state
	 */
	public void resetSetup() {
		if (PopupMessage.display("Confirm", Messages.INITIAL_SETUP_RESET_MESSAGE, true, "Yes",
				"No")) {
			deleteDirectory(new File(controller.getPreferredStorageDirectory()));
			controller.properties.resetProperties();
			initializeStartupPane();
		}
	}

	/**
	 * Choose a directory (input folder)
	 */
	public void browseNewFolderAction() {
		var directory = BrowserUtilities.browseDirectories("", controller.thisPane);
		if (directory.isEmpty()) {
			return;
		}
		pathTextField.setText(directory);
		driveSetupHelper.validateOneDrivePathAction();
	}

	/**
	 * Accept email
	 */
	public void addToEmailMap() {
		driveSetupHelper.addNewFolderAction();
	}

	/**
	 * Add a folder/email pair to the map
	 */
	public void addFolderEmailToMapAction() {
		driveSetupHelper.validateEmailAction();
	}

	/**
	 * Cancel adding the current folder/email pair to the map
	 */
	public void cancelAddToEmailMap() {
		driveSetupHelper.cancelAddToEmailMap();
	}

	/**
	 * Copy the mnemonic phrase to the clipboard
	 */
	public void copyPhraseToClipBoard() {
		mnemonicPhraseHelper.copyPhraseToClipBoard();
	}

	/**
	 * Paste a set of words from the clipboard to the mnemonic box
	 */
	public void pastePhraseFromClipBoard() {
		mnemonicPhraseHelper.pastePhraseFromClipBoard();
	}

	/**
	 * Store the hash of the password
	 *
	 * @throws HederaClientException
	 * 		if an InvalidKeySpecException or NoSuchAlgorithmException is thrown by the Hash calculation
	 */
	public void acceptPassword() throws HederaClientException {
		password = appPasswordField.getText().toCharArray();
		acceptPasswordButton.setVisible(false);
		acceptPasswordButton.setDisable(true);
		var filler = new char[password.length];
		Arrays.fill(filler, 'x');
		appPasswordField.clear();
		reEnterPasswordField.clear();
		appPasswordField.setText(String.valueOf(filler));
		reEnterPasswordField.setText(String.valueOf(filler));
		appPasswordField.setDisable(true);
		reEnterPasswordField.setDisable(true);
		properties.setHash(password);

		// Show the next box
		drivesBox.setVisible(true);
		if (properties.getOneDriveCredentials().size() > 0) {
			passphraseBox.setVisible(true);
		}
	}

	/**
	 * Generate the mnemonic passphrase and display it for the user in the designated area
	 */
	public void generatePassphraseEvent() {
		mnemonicPhraseHelper.generatePassphraseEvent(password, controller.getSalt(), true);
		controller.properties.setLegacy(false);
	}

	/**
	 * Clean up the password box after a reset
	 */
	private void resetPasswordBox() {
		passphraseBox.setVisible(true);
		appPasswordField.setDisable(false);
		acceptPasswordButton.setVisible(false);
		appPasswordField.setText("");
		checkPassword.setVisible(false);
		reEnterPasswordField.setText("");
		reEnterPasswordField.setDisable(true);
		reCheckPassword.setVisible(false);
		appPasswordField.requestFocus();
	}

	/**
	 * Clean up the mnemonic passphrase box after a reset
	 */
	private void resetPassphraseBox() {
		passphraseBox.setVisible(false);
		phraseBox.getChildren().clear();
		var mnemonicGridPane = new GridPane();
		mnemonicPhraseHelper.setupEmptyMnemonicBox(mnemonicGridPane);
		phraseBox.getChildren().add(mnemonicGridPane);
	}

	/**
	 * Clean up the drives box after a reset
	 */
	private void resetDrivesBox() {
		if (new File(controller.getPreferredStorageDirectory() + "/Files/user.properties").exists()) {
			controller.properties.setOneDriveCredentials(new HashMap<>());
		}
		drivesBox.setVisible(false);
		transactionFoldersVBoxIS.getChildren().clear();
		addToEmailMapButton.setVisible(false);
		pathTextField.clear();
		pathGreenCheck.setVisible(false);

		emailTextField.clear();
		emailGreenCheck.setVisible(false);
		drivesErrorLabel.setText("");
		drivesErrorLabel.setVisible(false);

		addFolderPathHBox.setVisible(true);
		addPathGridPaneIS.setVisible(true);
	}

	/**
	 * Event setup for the password fields and the accept password button
	 */
	private void setupPasswordEvents() {
		var policy = new PasswordPolicy(BreachDatabase.top100K(), 10, 1024);

		appPasswordField.setOnKeyReleased(keyEvent -> {
			final var length = appPasswordField.getText().length();
			characterCount.setText(String.valueOf(length));
			characterCount.setStyle(Constants.RED_STYLE);
			checkPassword.setVisible(false);
			if (Status.OK.equals(policy.check(appPasswordField.getText()))) {
				characterCount.setStyle(Constants.GREEN_STYLE);
				checkPassword.setVisible(true);
				passwordErrorLabel.setVisible(false);
				reEnterPasswordField.setDisable(false);
			}
			if (isTabOrEnter(keyEvent)) {
				passwordErrorLabel.setVisible(false);
				return;
			}

			checkPasswordPolicy(policy, appPasswordField, checkPassword, passwordErrorLabel, reEnterPasswordField);
		});

		reEnterPasswordField.setOnKeyReleased(keyEvent -> {
			if (appPasswordField.getText().equals(reEnterPasswordField.getText())) {
				reCheckPassword.setVisible(true);
				acceptPasswordButton.setVisible(true);
				acceptPasswordButton.setDisable(false);
			} else {
				reCheckPassword.setVisible(false);
				acceptPasswordButton.setVisible(false);
				acceptPasswordButton.setDisable(true);
			}

			if (keyEvent.getCode().equals(KeyCode.ENTER) && reCheckPassword.isVisible()) {
				try {
					acceptPassword();
				} catch (HederaClientException e) {
					controller.logAndDisplayError(e);
				}
			}
		});
	}

	private boolean isTabOrEnter(KeyEvent keyEvent) {
		return !(keyEvent.getCode().equals(KeyCode.ENTER) || keyEvent.getCode().equals(KeyCode.TAB));
	}

	/**
	 * For each node in the list, bind the managed property to their visibility
	 *
	 * @param nodes
	 * 		list of nodes that can be visible or invisible
	 */
	private void setManagedProperties(Node... nodes) {
		for (var n : nodes) {
			n.managedProperty().bind(n.visibleProperty());
		}
	}


	/**
	 * TESTING ONLY: Setting up a different directory from default
	 *
	 * @param keyEvent
	 * 		key released event
	 */
	public void choosePath(KeyEvent keyEvent) {
		if ((KeyCode.ENTER).equals(keyEvent.getCode())) {
			var infoPath = (hiddenPathInitial.getText()).replace(" ", "");
			var location = new File(infoPath);
			if (location.exists()) {
				if (location.isDirectory()) {
					String directory;
					if (hiddenPathInitial.getText().isEmpty()) {
						directory = BrowserUtilities.browseDirectories("", controller.thisPane);
					} else {
						directory = hiddenPathInitial.getText();
						hiddenPathInitial.clear();
					}
					controller.setPreferredStorageDirectory(directory);
					keysGridPane.setVisible(true);
					passphraseBox.setVisible(true);
					setupTransactionDirectory(directory);
				}
			}
		}
	}

}
