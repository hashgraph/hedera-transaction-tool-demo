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

package com.hedera.hashgraph.client.ui.utilities;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.Controller;
import javafx.beans.binding.Bindings;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;
import javafx.scene.layout.VBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BooleanSupplier;
import java.util.regex.Pattern;

import static com.hedera.hashgraph.client.core.constants.Constants.INITIAL_MAP_LOCATION;
import static com.hedera.hashgraph.client.core.constants.Messages.REMOVE_DRIVE_MESSAGE;
import static com.hedera.hashgraph.client.ui.popups.PopupMessage.display;
import static java.lang.Boolean.parseBoolean;
import static javafx.scene.control.PopupControl.USE_COMPUTED_SIZE;

public class DriveSetupHelper implements GenericFileReadWriteAware {
	public static final String CANCEL_STRING = "CANCEL";
	public static final String CREATE_STRING = "CREATE";
	@FXML
	private Controller controller = null;

	private static final Logger logger = LogManager.getLogger(DriveSetupHelper.class);
	private static final Pattern VALID_EMAIL_ADDRESS_REGEX =
			Pattern.compile("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$", Pattern.CASE_INSENSITIVE);
	private static final BooleanSupplier isInCircleCi = () ->
			parseBoolean(Optional.ofNullable(System.getenv("IN_CIRCLE_CI")).orElse("false"));
	private static final BooleanSupplier isInGithubActions = () ->
			Optional.ofNullable(System.getenv("GITHUB_ACTION")).isPresent();
	private static final String USER_HOME = System.getProperty("user.home") + File.separator;
	private static final String CURRENT_RELATIVE_PATH = Paths.get("").toAbsolutePath() + File.separator;
	public static final String INPUT_FILES = "InputFiles";
	public static final String OUTPUT_FILES = "OutputFiles";
	public static final String EMAIL_STRING = "email";
	public static final String DRIVE_STRING = "drive";

	private ImageView pathGreenCheck = null;
	private ImageView emailGreenCheck = null;

	private TextField pathTextField = null;
	private TextField emailTextField = null;

	private Label drivesErrorLabel = null;

	private Button addToEmailMapButton = null;
	private Button cancelAddToEmailMapButton = null;
	private Button confirmAddFolderButton = null;
	private Button browseNewFolderButton = null;

	private HBox addFolderPathHBox = null;
	private VBox storageBox = null;
	private VBox transactionFoldersVBox = null;

	private GridPane addPathGridPane = null;

	private UserAccessibleProperties tempProperties = null;
	private Map<String, String> credentials = new HashMap<>();

	private Image editImage = null;
	private Image deleteImage = null;

	private int driveLimit = 5;

	private DriveSetupHelper() {
	}

	/**
	 * Check that the input drive is valid (exists and has the appropriate folder structure)
	 */
	public void validateOneDrivePathAction() {
		final var text = pathTextField.getText();
		final var oneDrive = new File(text);
		if (!oneDrive.exists()) {
			setDrivesErrorLabel("The chosen drive does not exist.");
			pathGreenCheck.setVisible(false);
			return;
		}

		if (tempProperties.isKey(pathTextField.getText())) {
			setDrivesErrorLabel(
					"The chosen drive has already been linked to the app and is associated with an email " +
							"account.");
			pathGreenCheck.setVisible(false);
			return;
		}

		if (missingInnerDrives(text)) {
			pathGreenCheck.setVisible(false);
			drivesErrorLabel.setVisible(true);
			return;
		}

		pathGreenCheck.setVisible(true);
		drivesErrorLabel.setVisible(false);
		drivesErrorLabel.setText("");
		emailTextField.requestFocus();
	}

	/**
	 * Check the output drive is valid (exists and the user has "write-permission")
	 */
	public void validateEmailAction() {
		final var drivePath = pathTextField.getText();
		final var email = emailTextField.getText();
		final var outDrive = new File(drivePath, OUTPUT_FILES);

		final var out = new File(outDrive, email);

		if ((!out.exists() || !out.isDirectory()) && cannotAdd(outDrive, out)) {
			return;
		}
		credentials.put(drivePath, email);
		tempProperties.setOneDriveCredentials(credentials);
		controller.setDrivesChanged(true);
		clearGridPane();
		try {
			refreshTransactionsFolderVBox();
			addFolderPathHBox.setVisible(false);
			if (storageBox != null) {
				storageBox.setVisible(true);
			}
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	private boolean cannotAdd(final File outDrive, final File out) {
		final var display = display("Output drive",
				"The output folder does not exist. Do you want to create it?", true, CREATE_STRING, CANCEL_STRING);
		if (outDrive.canWrite() && Boolean.TRUE.equals(display)) {
			return !out.mkdirs();
		}
		display("Folder not found",
				"The email address entered is not valid. You will not be able to submit signed transactions. " +
						"Please enter a valid email address.",
				"OK");

		return true;
	}

	/**
	 * Refresh the folders vBox.
	 */
	public void refreshTransactionsFolderVBox() {
		final var emailMap = tempProperties.getOneDriveCredentials();
		if (emailMap.isEmpty()) {
			addToEmailMapButton.setVisible(true);
			return;
		}
		transactionFoldersVBox.getChildren().clear();
		for (final Map.Entry<String, String> entry : emailMap.entrySet()) {
			final var hBox = buildFolderEmailBox(entry.getKey(), entry.getValue());
			transactionFoldersVBox.getChildren().add(hBox);
		}
	}

	/**
	 * Start the "add drive" process
	 */
	public void addNewFolderAction() {
		addFolderPathHBox.setVisible(true);
		addPathGridPane.setVisible(true);
		emailGreenCheck.setVisible(false);
		pathGreenCheck.setVisible(false);
		drivesErrorLabel.setVisible(false);
		drivesErrorLabel.setText("");
	}

	/**
	 * Resets the "add drive" process.
	 */
	public void cancelAddToEmailMap() {
		addFolderPathHBox.setVisible(false);
		pathTextField.clear();
		emailTextField.clear();
		addPathGridPane.setVisible(false);
		emailGreenCheck.setVisible(false);
		pathGreenCheck.setVisible(false);
		drivesErrorLabel.setVisible(false);
		drivesErrorLabel.setText("");
		credentials = tempProperties.getOneDriveCredentials();
		if (storageBox != null) {
			storageBox.setVisible(true);
		}
	}

	/**
	 * Set up the "add folder" grid pane
	 *
	 * @param inputPath
	 * 		The path to the input folder
	 * @param outputFolder
	 * 		The name of the output folder (email address)
	 * @return a formatted grid pane
	 */
	private HBox buildFolderEmailBox(final String inputPath, final String outputFolder) {
		final var hBox = new HBox();
		hBox.setSpacing(20);
		hBox.setAlignment(Pos.BOTTOM_LEFT);

		final var gridPane = buildGridPane(inputPath, outputFolder);
		final var delete = setupButton(deleteImage);
		final var edit = setupButton(editImage);

		// Events
		delete.setOnAction(actionEvent -> deleteDriveAction(inputPath));

		edit.setOnAction(actionEvent -> editDriveAction(inputPath, outputFolder));

		delete.setDisable(tempProperties.getOneDriveCredentials().size() == 1);

		addToEmailMapButton.setVisible(tempProperties.getOneDriveCredentials().size() < driveLimit);

		hBox.getChildren().addAll(gridPane, edit, delete);

		return hBox;
	}

	/**
	 * Builds a grid pane with a drive's data
	 *
	 * @param inputPath
	 * 		The path to the input folder
	 * @param outputFolder
	 * 		The name of the output folder (email address)
	 * @return a formatted grid pane
	 */
	private GridPane buildGridPane(final String inputPath, final String outputFolder) {
		final var gridPane = new GridPane();

		gridPane.add(new Label("Input Paths: "), 0, 0);
		gridPane.add(new Label("Output Folder: "), 0, 1);

		final var keyLabel = new Label(inputPath);
		keyLabel.setWrapText(true);
		gridPane.add(keyLabel, 1, 0);
		final var valueLabel = new Label(outputFolder);
		valueLabel.setWrapText(true);
		gridPane.add(valueLabel, 1, 1);

		final var rowConstraints = new RowConstraints();
		rowConstraints.setPrefHeight(USE_COMPUTED_SIZE);


		gridPane.setStyle("-fx-border-color: darkgrey; -fx-background-radius: 10; -fx-border-radius: 10");
		gridPane.setPadding(new Insets(10));
		if (storageBox == null) {
			final var columnConstraints1 = new ColumnConstraints();
			columnConstraints1.prefWidthProperty().bind(gridPane.widthProperty().divide(4));
			final var columnConstraints = new ColumnConstraints();
			columnConstraints.prefWidthProperty().bind(gridPane.widthProperty().multiply(3).divide(4));
			gridPane.getRowConstraints().addAll(rowConstraints, rowConstraints);
			gridPane.getColumnConstraints().addAll(columnConstraints1, columnConstraints);
		}
		gridPane.setHgap(10);
		gridPane.setVgap(5);

		HBox.setHgrow(gridPane, Priority.ALWAYS);
		return gridPane;
	}

	/**
	 * Edit a drive/email pair
	 *
	 * @param inputPath
	 * 		The path to the input folder
	 * @param outputFolder
	 * 		The name of the output folder (email address)
	 */
	private void editDriveAction(final String inputPath, final String outputFolder) {
		drivesErrorLabel.setVisible(false);
		credentials.remove(inputPath);
		refreshTransactionsFolderVBox();
		if (tempProperties.getOneDriveCredentials().size() == 0) {
			addToEmailMapButton.setVisible(false);
		}
		pathTextField.setText(inputPath);
		pathGreenCheck.setVisible(validatePath(pathTextField.getText()));
		emailTextField.setText(outputFolder);
		addFolderPathHBox.setVisible(true);
		addPathGridPane.setVisible(true);
		pathTextField.requestFocus();
		controller.setDrivesChanged(true);
	}

	/**
	 * Delete a drive/emil pair from the map
	 *
	 * @param inputPath
	 * 		The path to the input folder
	 */
	private void deleteDriveAction(final String inputPath) {
		final boolean deleteDrive =
				display("Warning", REMOVE_DRIVE_MESSAGE, true, "CONTINUE", CANCEL_STRING);
		if (deleteDrive) {
			tempProperties.removeOneDriveCredential(inputPath);
			credentials.remove(inputPath);
			refreshTransactionsFolderVBox();
			controller.setDrivesChanged(true);
		}
	}

	/**
	 * Sets the format for the buttons in the grid pane
	 *
	 * @param icon
	 * 		the icon that will be displayed in the button
	 * @return a formatted button
	 */
	private Button setupButton(final Image icon) {
		final var imageView = new ImageView(icon);
		imageView.setFitHeight(20);
		imageView.setFitWidth(20);

		final var button = new Button();
		button.setGraphic(imageView);
		button.setStyle(
				"-fx-border-radius: 5; -fx-background-radius: 5; -fx-background-color: white; -fx-border-color: " +
						"darkgrey");
		return button;
	}

	/**
	 * Clears the input drive/email grid pane
	 */
	private void clearGridPane() {
		drivesErrorLabel.setVisible(false);
		pathGreenCheck.setVisible(false);
		browseNewFolderButton.setVisible(true);
		pathTextField.clear();
		emailTextField.clear();
	}

	/**
	 * Shows the error message to the user
	 *
	 * @param errorMessage
	 * 		the message
	 */
	private void setDrivesErrorLabel(final String errorMessage) {
		drivesErrorLabel.setVisible(true);
		drivesErrorLabel.setText(errorMessage);
	}

	/**
	 * Verifies if the inner drives are missing. If they are, and the user has write-permissions to the folder, it asks
	 * the user for permission to create them.
	 *
	 * @param text
	 * 		the selected location
	 * @return true if the inner structure is missing
	 */
	private boolean missingInnerDrives(final String text) {
		if (new File(text, INPUT_FILES).exists() && new File(text, OUTPUT_FILES).exists()) {
			return false;
		}
		final var invalidFolder = display("Invalid folder",
				"The chosen drive is missing a critical subdirectory. Would you like to create it?", true,
				CREATE_STRING, CANCEL_STRING);
		if (new File(text).canWrite() && Boolean.TRUE.equals(invalidFolder)) {
			if (!new File(text, INPUT_FILES).mkdirs() || !new File(text, OUTPUT_FILES).mkdirs()) {
				return true;
			}
			logger.info("Local files folder created");
			return false;
		}

		setDrivesErrorLabel(
				"The chosen drive is missing a critical subdirectory. Please check the path or contact the " +
						"administrator");
		return true;
	}


	/**
	 * Check a string is a valid email
	 *
	 * @param emailStr
	 * 		the string
	 * @return true if the string represents a valid email
	 */
	private boolean isEmail(final String emailStr) {
		final var matcher = VALID_EMAIL_ADDRESS_REGEX.matcher(emailStr);
		return matcher.find();
	}

	/**
	 * Validates the path has the correct folder structure
	 *
	 * @param path
	 * 		the path to the drive
	 * @return true if the folder structure is complete
	 */
	private boolean validatePath(final String path) {
		if (new File(path).exists() && new File(path).isDirectory()) {
			final var input = new File(path, INPUT_FILES);
			final var output = new File(path, OUTPUT_FILES);
			return input.exists() && input.isDirectory() && output.exists() && output.isDirectory();
		}
		return false;
	}

	/**
	 * Validate the path/email pair
	 *
	 * @param path
	 * 		the path to the drive
	 * @param email
	 * 		user's email
	 * @return true if the pair is valid
	 */
	private boolean validatePathEmailPair(final String path, final String email) {

		logger.info("Validating path {}, email {}", path, email);
		final var matcher = VALID_EMAIL_ADDRESS_REGEX.matcher(email);
		final var drive = new File(path);
		if (!matcher.find()) {
			logger.info("{} is not a valid email address", email);
			return false;
		}


		final var input = new File(String.format("%s/%s/", path, INPUT_FILES));
		final var output = new File(String.format("%s/%s/%s/", path, OUTPUT_FILES, email));

		if (input.exists() && output.exists()) {
			logger.info("Input {} and output {} exist", input.getAbsolutePath(), output.getAbsolutePath());
			return true;
		}

		if (!drive.canWrite()) {
			logger.info("Cannot create directories at: {}", USER_HOME);
			return false;
		}

		if (!input.exists()) {
			if (input.mkdirs()) {
				logger.info("Input folder created: {}", input.getAbsolutePath());
			} else {
				logger.info("Cannot create input folder: {}", input.getAbsolutePath());
				return false;
			}
		}

		if (!output.exists()) {
			if (output.mkdirs()) {
				logger.info("Output directory created: {}", output.getAbsolutePath());
			} else {
				logger.info("Cannot create output folder {}: ", output.getAbsolutePath());
				return false;
			}
		}

		return output.exists() && output.isDirectory();

	}

	/**
	 * Checks that all drives present in the properties are valid
	 *
	 * @param userProperties
	 * 		the properties
	 */
	private void checkDefaultDrives(final UserAccessibleProperties userProperties) {
		final var initialMapFile = new File(INITIAL_MAP_LOCATION);
		logger.info(initialMapFile.getAbsolutePath());

		if (!initialMapFile.exists()) {
			return;
		}

		final JsonArray map;
		try {
			final var initialMap = readJsonObject(initialMapFile);
			map = initialMap.has("map") ? initialMap.getAsJsonArray("map") : new JsonArray();
		} catch (final HederaClientException exception) {
			logger.error(exception.getMessage());
			return;
		}

		if (map.size() == 0) {
			return;
		}

		while (map.size() > driveLimit) {
			map.remove(map.size() - 1);
		}

		// check the initial map is correct
		for (final var jsonElement : map) {
			final var j = jsonElement.getAsJsonObject();
			if (j.has(DRIVE_STRING) && j.has(EMAIL_STRING)) {
				validateAndStoreJsonElement(userProperties, j);
			}

		}

		if (userProperties.getOneDriveCredentials().size() > 0) {
			addFolderPathHBox.setVisible(false);
			refreshTransactionsFolderVBox();
			credentials = userProperties.getOneDriveCredentials();
		}
	}

	/**
	 * Given a pair {drive/email} check for correctness and add to the internal map
	 *
	 * @param userProperties
	 * 		the properties
	 * @param jsonObject
	 * 		the pair
	 */
	private void validateAndStoreJsonElement(final UserAccessibleProperties userProperties,
			final JsonObject jsonObject) {
		final var driveString = jsonObject.get(DRIVE_STRING).getAsString();
		final var email = jsonObject.get(EMAIL_STRING).getAsString();
		if ("".equals(driveString) || "".equals(email)) {
			logger.error("Incomplete pair. Skipping {}", jsonObject);
			return;
		}

		var home = USER_HOME;

		if (isInCircleCi.getAsBoolean()) {
			home = "/repo/";
		} else if (isInGithubActions.getAsBoolean()) {
			home = CURRENT_RELATIVE_PATH;
		}

		final var drive = home + driveString;

		if (validatePathEmailPair(drive, email)) {
			logger.info("Adding drive/email pair ({}, {})", drive, email);
			userProperties.addOneDriveCredential(drive, email);
		}
	}

	/**
	 * Set up the bindings for multiple ui elements
	 */
	private void setupBindings() {
		emailTextField.disableProperty().bind(pathGreenCheck.visibleProperty().not());

		pathGreenCheck.visibleProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.TRUE.equals(t1) && emailGreenCheck.isVisible()) {
				drivesErrorLabel.setVisible(false);
			}
		});

		pathTextField.setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.ENTER) || keyEvent.getCode().equals(KeyCode.TAB)) {
				validateOneDrivePathAction();
			}
		});

		cancelAddToEmailMapButton.visibleProperty().bind(
				Bindings.size(transactionFoldersVBox.getChildren()).greaterThan(0));

		// If both green checks are on, there should be no error messages showing.
		emailGreenCheck.visibleProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.TRUE.equals(t1) && pathGreenCheck.isVisible()) {
				drivesErrorLabel.setVisible(false);
			}
		});

		confirmAddFolderButton.disableProperty().bind(
				emailGreenCheck.visibleProperty().not().or(pathGreenCheck.visibleProperty().not()));

		confirmAddFolderButton.visibleProperty().bind(
				emailGreenCheck.visibleProperty().and(pathGreenCheck.visibleProperty()));

		emailTextField.textProperty().addListener(
				(observableValue, aBoolean, t1) -> emailGreenCheck.setVisible(isEmail(emailTextField.getText())));

		emailTextField.setOnKeyReleased(this::keyReleasedEvent);

	}

	/**
	 * Behavior when a key is released in the email field
	 *
	 * @param keyEvent
	 * 		the event
	 */
	private void keyReleasedEvent(final KeyEvent keyEvent) {
		emailGreenCheck.setVisible(isEmail(emailTextField.getText()));
		if (keyEvent.getCode().equals(KeyCode.ENTER) || keyEvent.getCode().equals(KeyCode.TAB)) {
			if (validatePath(pathTextField.getText()) && isEmail(emailTextField.getText())) {
				drivesErrorLabel.setVisible(false);
				drivesErrorLabel.setText("");
				validateEmailAction();
				return;
			}
			setDrivesErrorLabel("The email address entered is not valid");
		}
	}

	public static final class Builder {
		private Controller controller;
		private ImageView pathGreenCheck;
		private ImageView emailGreenCheck;
		private TextField pathTextField;
		private TextField emailTextField;
		private Label drivesErrorLabel;
		private Button addToEmailMapButton;
		private Button cancelAddToEmailMapButton;
		private Button confirmAddFolderButton;
		private Button browseNewFolderButton;
		private HBox addFolderPathHBox;
		private VBox storageBox;
		private VBox transactionFoldersVBox;
		private GridPane addPathGridPane;
		private UserAccessibleProperties tempProperties;
		private Image editImage;
		private Image deleteImage;
		private int driveLimit;

		private Builder() {
		}

		public static Builder aDriveSetupHelper() {
			return new Builder();
		}

		public Builder withController(final Controller controller) {
			this.controller = controller;
			return this;
		}


		public Builder withPathGreenCheck(final ImageView pathGreenCheck) {
			this.pathGreenCheck = pathGreenCheck;
			return this;
		}

		public Builder withEmailGreenCheck(final ImageView emailGreenCheck) {
			this.emailGreenCheck = emailGreenCheck;
			return this;
		}

		public Builder withPathTextField(final TextField pathTextField) {
			this.pathTextField = pathTextField;
			return this;
		}

		public Builder withEmailTextField(final TextField emailTextField) {
			this.emailTextField = emailTextField;
			return this;
		}

		public Builder withDrivesErrorLabel(final Label drivesErrorLabel) {
			this.drivesErrorLabel = drivesErrorLabel;
			return this;
		}

		public Builder withAddToEmailMapButton(final Button addToEmailMapButton) {
			this.addToEmailMapButton = addToEmailMapButton;
			return this;
		}

		public Builder withCancelAddToEmailMapButton(final Button cancelAddToEmailMapButton) {
			this.cancelAddToEmailMapButton = cancelAddToEmailMapButton;
			return this;
		}

		public Builder withConfirmAddFolderButton(final Button confirmAddFolderButton) {
			this.confirmAddFolderButton = confirmAddFolderButton;
			return this;
		}

		public Builder withBrowseNewFolderButton(final Button browseNewFolderButton) {
			this.browseNewFolderButton = browseNewFolderButton;
			return this;
		}

		public Builder withAddFolderPathHBox(final HBox addFolderPathHBox) {
			this.addFolderPathHBox = addFolderPathHBox;
			return this;
		}

		public Builder withStorageBox(final VBox storageBox) {
			this.storageBox = storageBox;
			return this;
		}

		public Builder withTransactionFoldersVBox(final VBox transactionFoldersVBox) {
			this.transactionFoldersVBox = transactionFoldersVBox;
			return this;
		}

		public Builder withAddPathGridPane(final GridPane addPathGridPane) {
			this.addPathGridPane = addPathGridPane;
			return this;
		}

		public Builder withTempProperties(final UserAccessibleProperties tempProperties) {
			this.tempProperties = tempProperties;
			return this;
		}

		public Builder withDriveLimit(final int driveLimit) {
			this.driveLimit = driveLimit;
			return this;
		}

		public Builder withEditImage(final Image editImage) {
			this.editImage = editImage;
			return this;
		}

		public Builder withDeleteImage(final Image deleteImage) {
			this.deleteImage = deleteImage;
			return this;
		}

		public DriveSetupHelper build() {
			final DriveSetupHelper helper = new DriveSetupHelper();
			helper.pathGreenCheck = pathGreenCheck;
			helper.emailGreenCheck = emailGreenCheck;
			helper.pathTextField = pathTextField;
			helper.emailTextField = emailTextField;
			helper.drivesErrorLabel = drivesErrorLabel;
			helper.addToEmailMapButton = addToEmailMapButton;
			helper.cancelAddToEmailMapButton = cancelAddToEmailMapButton;
			helper.confirmAddFolderButton = confirmAddFolderButton;
			helper.browseNewFolderButton = browseNewFolderButton;
			helper.addFolderPathHBox = addFolderPathHBox;
			helper.storageBox = storageBox;
			helper.transactionFoldersVBox = transactionFoldersVBox;
			helper.addPathGridPane = addPathGridPane;
			helper.tempProperties = tempProperties;
			helper.driveLimit = driveLimit;
			helper.editImage = editImage;
			helper.deleteImage = deleteImage;
			helper.controller = controller;
			helper.credentials = tempProperties.getOneDriveCredentials();
			helper.setupBindings();
			helper.checkDefaultDrives(tempProperties);
			return helper;
		}

	}
}
