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
import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.Messages;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.utilities.KeyPairUtility;
import com.hedera.hashgraph.client.ui.utilities.KeyStructureUtility;
import com.hedera.hashgraph.client.ui.utilities.ReloadFilesService;
import com.hedera.hashgraph.client.ui.utilities.UpdateHelper;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Key;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.TextArea;
import javafx.scene.control.TreeView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.zeroturnaround.zip.commons.IOUtils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.TimeZone;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEVELOPMENT;
import static com.hedera.hashgraph.client.core.constants.Constants.KEY_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.LAST_INDEX;
import static com.hedera.hashgraph.client.core.constants.Constants.LAST_TRANSACTIONS_DIRECTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.MENU_BUTTON_HIGHLIGHT_COLOR;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.RELOAD_PERIOD;
import static com.hedera.hashgraph.client.core.constants.Constants.SALT_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.SETUP_PHASE;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_NAME;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PREFERENCE_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.INITIAL_SETUP_PHASE;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.NORMAL_OPERATION_PHASE;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.PASSWORD_RECOVERY_PHASE;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.TEST_PHASE;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.fromInt;
import static org.zeroturnaround.zip.commons.FileUtils.copyDirectory;
import static org.zeroturnaround.zip.commons.FileUtils.deleteDirectory;

public class Controller implements Initializable, GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(Controller.class);
	public static final String DISCLAIMER =
			"This software is designed for use solely by the Hedera Council and staff. The software is being " +
					"released as open source as example code only, and is not intended or suitable for use in " +
					"its current form by anyone other than members of the Hedera Council and Hedera personnel. " +
					"If you are not a Hedera Council member or staff member, use of this application or of the " +
					"code in its current form is not recommended and is at your own risk.";

	private final DoubleProperty fontSize = new SimpleDoubleProperty(10);
	private boolean disableButtons = false;
	private boolean drivesChanged = true;

	public AnchorPane centerPane;
	public BorderPane borderPane;
	public Pane homePane;
	public StackPane accountsPane;
	public Pane createPane;
	public Pane settingsPane;
	public StackPane initialStartupPane;
	public StackPane recoverPasswordPane;
	public StackPane keysPane;

	public Button homeButton;
	public Button createButton;
	public Button accountsButton;
	public Button keysButton;
	public Button settingsButton;
	public Button hederaButton;
	public TextArea systemMessagesTextField;
	public ButtonBar menuButtonBar;

	private final Preferences preferences = Preferences.userNodeForPackage(Controller.class);

	private UserAccessibleProperties properties;

	public void setProperties(UserAccessibleProperties properties) {
		this.properties = properties;
	}

	private Pane thisPane = new Pane();

	public Pane getThisPane() {
		return thisPane;
	}

	public void setThisPane(Pane thisPane) {
		this.thisPane = thisPane;
	}

	// sub controllers
	@FXML
	public AccountsPaneController accountsPaneController;
	@FXML
	public KeysPaneController keysPaneController;
	@FXML
	public HomePaneController homePaneController;
	@FXML
	public SettingsPaneController settingsPaneController;
	@FXML
	public CreatePaneController createPaneController;
	@FXML
	public InitialStartupPaneController initialStartupPaneController;
	@FXML
	public RecoverPasswordPaneController recoverPasswordPaneController;


	// Utility
	private KeyStructureUtility keyStructureUtility;
	public final KeyPairUtility keyPairUtility = new KeyPairUtility();

	void setDisableButtons(boolean disableButtons) {
		this.disableButtons = disableButtons;
	}

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		resetButtonBackgrounds();
		properties = new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");
		keyStructureUtility = new KeyStructureUtility(this);

		var lastVersion = properties.getVersionString();
		var thisVersion = getVersion();


		if (!thisVersion.equalsIgnoreCase(lastVersion)) {
			newVersionAction(lastVersion, thisVersion);
			replacePublicKey();
		}

		if (homePane.isVisible()) {
			homeButton.setStyle(MENU_BUTTON_HIGHLIGHT_COLOR);
		}

		systemMessagesTextField.setVisible(DEVELOPMENT);
		systemMessagesTextField.managedProperty().bind(systemMessagesTextField.visibleProperty());


		if (!new File(getPreferredStorageDirectory()).exists()) {
			try {
				resetPreferences();
			} catch (BackingStoreException | IOException e) {
				logAndDisplayError(e);
			}
			setSetupPhase(INITIAL_SETUP_PHASE);
		} else {
			logger.info("The current storage directory is: {}", getPreferredStorageDirectory());
		}


		// Check if update from previous version is needed
		var updateHelper = new UpdateHelper(properties.getPreferredStorageDirectory());

		if ((getSetupPhase() == TEST_PHASE || getSetupPhase() == NORMAL_OPERATION_PHASE) && !updateHelper.isUpdated()) {
			logger.info("Application directory needs to be updated");
			try {
				updateHelper.handleMigration();
			} catch (HederaClientException | IOException e) {
				logger.error("Cannot complete migration {}", e.toString());
			}
		}

		// Home pane should load files periodically
		if (getSetupPhase() == NORMAL_OPERATION_PHASE) {
			var service = new ReloadFilesService(homePaneController);
			service.setPeriod(Duration.seconds(RELOAD_PERIOD));
			service.start();
		}
		initialStartupPaneController.injectMainController(this);
		recoverPasswordPaneController.injectMainController(this);
		homePaneController.injectMainController(this);
		settingsPaneController.injectMainController(this);
		createPaneController.injectMainController(this);
		keysPaneController.injectMainController(this);
		accountsPaneController.injectMainController(this);

		startupPhaseInitialization();

		fontSize.bind(homePane.heightProperty().add(homePane.widthProperty()).divide(125));
		centerPane.styleProperty().bind(Bindings.concat("-fx-font-size: ", fontSize.asString(), ";"));

		createPane.setVisible(false);
		accountsPane.setVisible(false);
		settingsPane.setVisible(false);
		hederaButton.setOnMouseClicked(mouseEvent -> {
			if (mouseEvent.getClickCount() == 2) {
				var version = getVersion().replace(", ", "\n").replace(" ", "\u00A0");
				PopupMessage.display("Hedera Transaction Tool", version, "CONTINUE");
			}
		});


	}

	public void setDrivesChanged(boolean drivesChanged) {
		this.drivesChanged = drivesChanged;
	}

	private void replacePublicKey() {
		InputStream readStream = this.getClass().getClassLoader().getResourceAsStream("gpgPublicKey.asc");
		final var key = new File(DEFAULT_STORAGE, Constants.PUBLIC_KEY_LOCATION);
		try (OutputStream outputStream = new FileOutputStream(key)) {
			Files.deleteIfExists(key.toPath());
			assert readStream != null;
			IOUtils.copy(readStream, outputStream);
		} catch (IOException exception) {
			logger.error(exception);
		}
	}

	private void newVersionAction(String lastVersion, String thisVersion) {
		// export version to server
		setVersionString(thisVersion);
		exportVersionToServers(lastVersion, thisVersion);
		if (getSetupPhase().equals(NORMAL_OPERATION_PHASE) || getSetupPhase().equals(INITIAL_SETUP_PHASE)) {
			// This popup should only appear during initial startup and normal operation (disabled for password
			// recovery)
			PopupMessage.display("Important note", DISCLAIMER, "I UNDERSTAND AND AGREE");
		}
		properties.setVersionString(thisVersion);
	}


	private void startupPhaseInitialization() {
		switch (getSetupPhase()) {
			case PASSWORD_RECOVERY_PHASE:
				if (!checkRecoveryPhraseExists()) {
					setSetupPhase(INITIAL_SETUP_PHASE);
					try {
						resetPreferences();
					} catch (BackingStoreException | IOException e) {
						logAndDisplayError(e);
					}
				}
				properties =
						new UserAccessibleProperties(getPreferredStorageDirectory() + File.separator + USER_PROPERTIES,
								"");

				setDisableButtons(true);
				homePane.setVisible(false);
				thisPane = recoverPasswordPane;
				recoverPasswordPane.setVisible(true);

				try {
					recoverPasswordPaneController.initializeRecoveryPane();
				} catch (Exception e) {
					logger.error("Unable to initialize recovery pane controller.", e);
					displaySystemMessage("Unable to initialize recovery pane controller.");
				}
				break;
			case INITIAL_SETUP_PHASE:
				setDisableButtons(true);
				recoverPasswordPane.setVisible(false);
				homePane.setVisible(false);
				thisPane = initialStartupPane;
				initialStartupPane.setVisible(true);
				initialStartupPaneController.initializeStartupPane();
				break;
			case NORMAL_OPERATION_PHASE:
				if (new File(getPreferredStorageDirectory(), MNEMONIC_PATH).exists() && "".equals(
						properties.getHash())) {
					logger.info("Missing hash in config file: Putting the application in password recovery mode");
					PopupMessage.display("Missing Password", Messages.PASSWORD_NOT_FOUND_MESSAGE, "CONTINUE");
					setSetupPhase(PASSWORD_RECOVERY_PHASE);
					break;

				}
				properties =
						new UserAccessibleProperties(getPreferredStorageDirectory() + File.separator + USER_PROPERTIES,
								"");
				recoverPasswordPane.setVisible(false);
				setDisableButtons(false);
				thisPane = homePane;
				homePane.setVisible(true);
				keysPaneController.initializeKeysPane();
				accountsPaneController.initializeAccountPane();
				homePaneController.initializeHomePane();
				settingsPaneController.initializeSettingsPane();
				createPaneController.initializeCreatePane();
				break;
			case TEST_PHASE:
				properties =
						new UserAccessibleProperties(getPreferredStorageDirectory() + File.separator + USER_PROPERTIES,
								"");
				recoverPasswordPane.setVisible(false);
				setDisableButtons(false);
				thisPane = homePane;
				homePane.setVisible(true);
				keysPaneController.initializeKeysPane();
				accountsPaneController.initializeAccountPane();
				homePaneController.initializeHomePane();
				settingsPaneController.initializeSettingsPane();
				createPaneController.initializeCreatePane();
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + getSetupPhase());
		}
	}


	private boolean checkRecoveryPhraseExists() {
		if (new File(getPreferredStorageDirectory(), MNEMONIC_PATH).exists()) {
			return true;
		}

		// For backwards compatibility with V2.0: search hardcoded paths to old mnemonic.txt file
		if (!new File(System.getProperty("user.home") + "/Hedera Applications/").exists()) {
			return false;
		}

		var ms = new File(Constants.KEYS_FOLDER).listFiles((dir, name) -> name.contains("mnemonic"));
		assert ms != null;
		List<File> mnemonics = new ArrayList<>(Arrays.asList(ms));

		// If there is more than one mnemonic file, we cannot say which one is the correct one
		if (mnemonics.size() != 1) {
			return false;
		}

		for (var mnemonicFile : mnemonics) {
			var hardCodedMnemonic = mnemonicFile.getAbsolutePath();
			if (new File(hardCodedMnemonic).exists()) {
				try {
					org.apache.commons.io.FileUtils.moveFile(new File(hardCodedMnemonic),
							new File(getPreferredStorageDirectory(), MNEMONIC_PATH));
				} catch (IOException e) {
					logger.error(e);
					displaySystemMessage(Arrays.toString(e.getStackTrace()));
				}
				return true;
			}
		}
		return false;
	}

	public void logAndDisplayError(Exception e) {
		logger.error(e);
		displaySystemMessage(e.toString());
	}
	//region NAVIGATION

	public void changePaneImageClicked(javafx.event.ActionEvent event) {
		if (disableButtons) {
			return;
		}
		resetButtonBackgrounds();

		var button = (Button) event.getSource();
		button.setStyle(MENU_BUTTON_HIGHLIGHT_COLOR);
		homePaneController.setForceUpdate(false);
		switch (button.getId()) {
			case "homeButton":
				if (drivesChanged) {
					homePaneController.setForceUpdate(true);
				}
				homePaneController.initializeHomePane();
				changeTab(homePane);
				break;
			case "createButton":
				accountsPaneController.initializeAccountPane();
				changeTab(createPane);
				break;
			case "accountsButton":
				accountsPaneController.initializeAccountPane();
				changeTab(accountsPane);
				break;
			case "settingsButton":
				settingsPaneController.initializeSettingsPane();
				changeTab(settingsPane);
				break;
			case "keysButton":
				keysPaneController.initializeKeysPane();
				changeTab(keysPane);
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + button.getId());
		}


	}

	public Map<Identifier, AccountInfo> getAccountInfoMap() {
		Map<Identifier, AccountInfo> map = null;
		try {
			map = new HashMap<>();
			var accountInfoMap = properties.getAccountInfoMap();
			for (var entry : accountInfoMap.entrySet()) {
				var infoFile = entry.getValue();
				if (new File(infoFile).exists()) {
					var info = AccountInfo.fromBytes(readBytes(infoFile));
					map.put(new Identifier(info.accountId), info);
				}
			}
		} catch (InvalidProtocolBufferException | HederaClientException e) {
			logAndDisplayError(e);
		}

		return map;
	}

	private void resetButtonBackgrounds() {
		var buttons = menuButtonBar.getButtons();
		for (var button : buttons) {
			button.setStyle("-fx-background-color: white;");
		}
	}

	void changeTab(Pane next) {
		homePane.setVisible(false);
		accountsPane.setVisible(false);
		createPane.setVisible(false);
		settingsPane.setVisible(false);
		initialStartupPane.setVisible(false);
		recoverPasswordPane.setVisible(false);

		Pane lastPane = thisPane;
		lastPane.setVisible(false);
		thisPane = next;
		thisPane.setVisible(true);
		thisPane.toFront();
	}

	//	endregion

	// region PREFERENCES
	public String getLastTransactionsDirectory() {
		return properties.getLastBrowsedDirectory();
	}

	public void setLastTransactionsDirectory(File directory) {
		properties.setLastBrowsedDirectory(directory);
	}

	public String getPreferredStorageDirectory() {
		return properties.getPreferredStorageDirectory();
	}

	public void setPreferredStorageDirectory(String directory) {
		properties.setPreferredStorageDirectory(directory);
	}

	void setSetupPhase(SetupPhase phase) {
		properties.setSetupPhase(phase);
	}

	SetupPhase getSetupPhase() {

		// For backwards compatibility with previous versions
		final var phase = preferences.getInt(SETUP_PHASE, 0);
		if (phase != 0 && INITIAL_SETUP_PHASE.equals(properties.getSetupPhase())) {
			preferences.putInt(SETUP_PHASE, 0);
			properties.setSetupPhase(fromInt(phase));
		}
		return properties.getSetupPhase();
	}

	void setVersionString(String versionString) {
		properties.setVersionString(versionString);
	}

	public void setUserName(String userName) {
		preferences.put(USER_NAME, userName);
	}

	String getUserName() {
		return preferences.get(USER_NAME, "default");
	}

	int getLastIndex() {
		return preferences.getInt(LAST_INDEX, 0);
	}

	void setLastIndex(int index) {
		preferences.putInt(LAST_INDEX, index);
	}

	void incrementIndex() {
		var index = getLastIndex();
		preferences.putInt(LAST_INDEX, index + 1);
	}

	void resetPreferences() throws BackingStoreException, IOException {
		final var userProperties = new File(getPreferredStorageDirectory(), USER_PROPERTIES);
		Files.deleteIfExists(userProperties.toPath());
		logger.info("User preferences deleted");
		preferences.clear();
		logger.info("Preferences Reset");
	}

	void resetApp() throws BackingStoreException, IOException {
		var defaultStorageDirectory = DEFAULT_STORAGE;
		var userDefStorageDirectory = settingsPaneController.loadStorageTextField.getText();
		var sourceDir = userDefStorageDirectory.equals("") ? new File(
				defaultStorageDirectory) : new File(userDefStorageDirectory);

		logger.info("Default storage {}", defaultStorageDirectory);
		logger.info("Before reset storage {}", sourceDir.getPath());
		var secondsToArchive = Instant.now().getEpochSecond();
		var resetStorageDirectory =
				String.format("%s.%s", sourceDir.getPath(), secondsToArchive);
		preferences.put(LAST_TRANSACTIONS_DIRECTORY, resetStorageDirectory);
		resetPreferences();
		var userPreferenceFile = new File(sourceDir.getPath(), USER_PREFERENCE_FILE);
		if (userPreferenceFile.createNewFile()) {
			logger.info("New user preference file created");
		} else {
			logger.error("Could not create user preferences file");
		}
		preferences.exportNode(new FileOutputStream(userPreferenceFile, false));

		var destination = new File(resetStorageDirectory);
		copyDirectory(sourceDir, destination);
		logger.info("Transactions Tool storage archived to {}", destination.getPath());

		preferences.clear();
		deleteDirectory(sourceDir);
		Platform.exit();
	}


	// endregion

	/**
	 * Display an error message on the hidden pane on home page
	 *
	 * @param error
	 * 		the error to be displayed
	 */
	public void displaySystemMessage(String error) {
		var d = new Date();
		systemMessagesTextField.appendText(d + ": " + error + System.getProperty("line.separator"));
	}

	/**
	 * Display an error message on the hidden pane on home page
	 *
	 * @param exception
	 * 		an exception
	 */
	public void displaySystemMessage(Exception exception) {
		var d = new Date();
		systemMessagesTextField.appendText(
				d + ": " + exception.toString() + System.getProperty("line.separator"));
		logger.error(exception);
	}

	/**
	 * Read the version from build.properties file. Version is displayed as
	 * Version, Time the last build is done in UTC, Last commit ID in the build.
	 */
	public String getVersion() {
		var buildProps = new Properties();
		var version = "";
		try {
			buildProperties(buildProps);
			var formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
			var dateTime = formatter.parse(buildProps.get("git.build.time").toString());
			formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
			var dateTimeUTC = formatter.format(dateTime);
			version = String.format("Version: %s, UTC-BUILD-TIME: %s, COMMIT-ID: %s",
					buildProps.get("git.build.version"),
					dateTimeUTC, buildProps.get("git.commit.id.abbrev"));
		} catch (Exception ex) {
			logger.error("Error Printing Version {}", ex.getMessage());
			displaySystemMessage(ex);
		}
		return version;
	}


	/**
	 * If a new version has been detected, the app assumes it has been recently updated, and writes a text file in the
	 * outputs
	 *
	 * @param oldVersion
	 * 		previous version of the app
	 * @param newVersion
	 * 		version the app has been updated to
	 */
	private void exportVersionToServers(String oldVersion, String newVersion) {
		var drives = properties.getOneDriveCredentials();
		for (var entry : drives.entrySet()) {
			var key = entry.getKey();
			var value = entry.getValue();
			final var cleanName = newVersion.replace(":", "-").replace(".", "-").replace(",", "").replace(" ", "");
			final var fileName = String.format("%s/OutputFiles/%s/SoftwareUpdated-%s.txt", key, value, cleanName);
			try (var writer = new BufferedWriter(new FileWriter(fileName))) {
				writer.write(
						String.format("Software updated from version %s to version %s on %s", oldVersion, newVersion,
								new Date()));
			} catch (IOException e) {
				logger.error(e);
			}
		}
	}

	private void buildProperties(Properties buildProps) throws URISyntaxException {
		var localPath = new File(
				StartUI.class.getProtectionDomain().getCodeSource().getLocation().toURI()).getParent() +
				"/build.properties";
		try (InputStream inputStream = new FileInputStream(localPath)) {
			buildProps.load(inputStream);
		} catch (IOException e) {
			logAndDisplayError(e);
		}
	}


	public byte[] getSalt() {
		if (properties.hasSalt()) {
			var token = properties.getHash();
			var decoder = Base64.getDecoder();

			var tokenBytes = decoder.decode(token);
			if (tokenBytes.length < Constants.SALT_LENGTH + KEY_LENGTH / 8) {
				logger.error("Token size check failed");
			}
			return Arrays.copyOfRange(tokenBytes, 0, Constants.SALT_LENGTH);
		}
		return new byte[SALT_LENGTH];
	}

	//region PROPERTIES

	/**
	 * Determines which algorithm was used to encrypt the mnemonic
	 *
	 * @return true if the stored mnemonic was encrypted using the PBKDF2 algorithm (Version 2.X of the app)
	 */
	public boolean isLegacyMnemonic() {
		return properties.isLegacy();
	}

	public void setAccountInfoMap(Map<String, String> accountInfos) {
		properties.setAccountInfoMap(accountInfos);
	}

	public void setLegacy(boolean b) {
		properties.setLegacy(b);
	}

	public void resetProperties() {
		properties.resetProperties();
	}

	public void setOneDriveCredentials(Map<String, String> objectObjectHashMap) {
		properties.setOneDriveCredentials(objectObjectHashMap);
	}

	public UserAccessibleProperties getProperties() {
		return this.properties;
	}

	public long getDefaultTxFee() {
		return properties.getDefaultTxFee();
	}

	public int getDefaultHours() {
		return properties.getDefaultHours();
	}

	public int getDefaultMinutes() {
		return properties.getDefaultMinutes();
	}

	public int getDefaultSeconds() {
		return properties.getDefaultSeconds();
	}

	public String getDefaultNodeID() {
		return properties.getDefaultNodeID();
	}

	public long getTxValidDuration() {
		return properties.getTxValidDuration();
	}

	public long getAutoRenewPeriod() {
		return properties.getAutoRenewPeriod();
	}

	public void setGenerateRecord(boolean b) {
		properties.setGenerateRecord(b);
	}

	public void setDefaultTxFee(long fee) {
		properties.setDefaultTxFee(fee);
	}

	public void setDefaultSeconds(int s) {
		properties.setDefaultSeconds(s);
	}

	public void setDefaultMinutes(int m) {
		properties.setDefaultMinutes(m);
	}

	public void setDefaultHours(int h) {
		properties.setDefaultHours(h);
	}

	public void setTxValidDuration(int duration) {
		properties.setTxValidDuration(duration);
	}

	public void setAutoRenewPeriod(int duration) {
		properties.setAutoRenewPeriod(duration);
	}

	public void setDefaultNodeID(String node) {
		properties.setDefaultNodeID(node);
	}

	public int getMnemonicHashCode() {
		return properties.getMnemonicHashCode();
	}

	public Map<String, String> getOneDriveCredentials() {
		return properties.getOneDriveCredentials();
	}

	public String getHash() {
		return properties.getHash();
	}

	public boolean hasSalt() {
		return properties.hasSalt();
	}

	public void setHash(char[] password) throws HederaClientException {
		properties.setHash(password);
	}

	public void setSalt(boolean b) {
		properties.setSalt(b);
	}

	public void setMnemonicHashCode(int hashCode) {
		properties.setMnemonicHashCode(hashCode);
	}

	public String getEmailFromMap(String path) {
		return properties.getEmailFromMap(path);
	}

	public String getNetworkProperty() {
		return properties.getNetworkProperty();
	}

	public boolean getGenerateRecord() {
		return properties.getGenerateRecord();
	}
	//endregion


	public String showKeyString(ByteString key) {
		return keyStructureUtility.showKeyString(key);
	}

	public TreeView<String> buildKeyTreeView(Key key) throws IOException {
		return keyStructureUtility.buildKeyTreeView(key);
	}

	public TreeView<String> buildKeyTreeView(JsonObject key) {
		return keyStructureUtility.buildKeyTreeView(key);
	}

	public void loadPubKeys() {
		keyStructureUtility.loadPubKeys();
	}

	public Map<String, Path> getPubFiles() {
		return keyStructureUtility.getPubFiles();
	}

	public JsonObject getAccountsList() {
		JsonObject object = new JsonObject();
		try {
			object = (new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (HederaClientException e) {
			logger.error(e.getMessage());
		}
		return object;
	}
}
