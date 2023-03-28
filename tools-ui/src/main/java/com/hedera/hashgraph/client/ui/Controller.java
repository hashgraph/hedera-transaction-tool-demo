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
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.updater.GithubUpdater;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.client.ui.utilities.KeyPairUtility;
import com.hedera.hashgraph.client.ui.utilities.KeyStructureUtility;
import com.hedera.hashgraph.client.ui.utilities.ReloadFilesService;
import com.hedera.hashgraph.client.ui.utilities.UpdateHelper;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Key;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Separator;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;
import org.jetbrains.annotations.NotNull;
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
import java.security.KeyPair;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.TimeZone;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_INTERNAL_FILES;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_SYSTEM_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.DEVELOPMENT;
import static com.hedera.hashgraph.client.core.constants.Constants.INPUT_FILES;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.LAST_INDEX;
import static com.hedera.hashgraph.client.core.constants.Constants.LAST_TRANSACTIONS_DIRECTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.MENU_BUTTON_HIGHLIGHT_COLOR;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.RELOAD_PERIOD;
import static com.hedera.hashgraph.client.core.constants.Constants.SETUP_PHASE;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_NAME;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PREFERENCE_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.INITIAL_SETUP_PHASE;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.NORMAL_OPERATION_PHASE;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.PASSWORD_RECOVERY_PHASE;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.TEST_PHASE;
import static com.hedera.hashgraph.client.core.enums.SetupPhase.fromInt;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.getSaltBytes;
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

	private boolean disableButtons = false;
	private boolean drivesChanged = false;

	public AnchorPane centerPane;
	public BorderPane borderPane;
	public Pane homePane;
	public StackPane accountsPane;
	public Pane createPane;
	public Pane settingsPane;
	public StackPane initialStartupPane;
	public StackPane keysPane;
	public StackPane historyPane;

	public Button homeButton;
	public Button historyButton;
	public Button createButton;
	public Button submitButton;
	public Button accountsButton;
	public Button keysButton;
	public Button settingsButton;
	public Button hederaButton;
	public TextArea systemMessagesTextField;
	public ButtonBar menuButtonBar;

	private GithubUpdater updater;

	private final Preferences preferences = Preferences.userNodeForPackage(Controller.class);

	private UserAccessibleProperties properties;

	public void setProperties(final UserAccessibleProperties properties) {
		this.properties = properties;
	}

	private Pane thisPane = new Pane();

	public Pane getThisPane() {
		return thisPane;
	}

	public void setThisPane(final Pane thisPane) {
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
	public HistoryPaneController historyPaneController;


	// Utility
	private KeyStructureUtility keyStructureUtility;
	private KeyPairUtility keyPairUtility;

	public KeyPairUtility getKeyPairUtility() {
		return keyPairUtility;
	}

	void setDisableButtons(final boolean disableButtons) {
		this.disableButtons = disableButtons;
	}

	@Override
	public void initialize(final URL location, final ResourceBundle resources) {

		setManagedProperty(homeButton, historyButton, createButton, accountsButton, keysButton, settingsButton,
				submitButton);

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");
		keyPairUtility = properties.getSetupPhase().equals(TEST_PHASE) ?
				new KeyPairUtility(Constants.TEST_EXPIRATION_TIME) :
				new KeyPairUtility();
		keyStructureUtility = new KeyStructureUtility(this);

		resetButtonBackgrounds();

		final var lastVersion = properties.getVersionString();
		final var thisVersion = getVersion();


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
			} catch (final BackingStoreException | IOException e) {
				logAndDisplayError(e);
			}
			setSetupPhase(INITIAL_SETUP_PHASE);
		} else {
			logger.info("The current storage directory is: {}", getPreferredStorageDirectory());
		}


		// Check if update from previous version is needed
		final var updateHelper = new UpdateHelper(properties.getPreferredStorageDirectory());

		if ((getSetupPhase() == TEST_PHASE || getSetupPhase() == NORMAL_OPERATION_PHASE) && !updateHelper.isUpdated()) {
			logger.info("Application directory needs to be updated");
			try {
				updateHelper.handleMigration();
				replacePublicKey();
			} catch (final HederaClientException | IOException e) {
				logger.error("Cannot complete migration {}", e.getMessage());
			}
		}

		// Home pane should load files periodically
		if (getSetupPhase() == NORMAL_OPERATION_PHASE) {
			final var service = new ReloadFilesService(homePaneController);
			service.setPeriod(Duration.seconds(RELOAD_PERIOD));
			service.start();
		}
		initialStartupPaneController.injectMainController(this);
		historyPaneController.injectMainController(this);
		homePaneController.injectMainController(this);
		settingsPaneController.injectMainController(this);
		createPaneController.injectMainController(this);
		keysPaneController.injectMainController(this);
		accountsPaneController.injectMainController(this);


		startupPhaseInitialization();

		createPane.setVisible(false);
		accountsPane.setVisible(false);
		settingsPane.setVisible(false);
		hederaButton.setOnMouseClicked(mouseEvent -> {
			if (mouseEvent.getClickCount() == 2) {
				final var version = getVersion().replace(", ", "\n").replace(" ", "\u00A0");
				PopupMessage.display("Hedera Transaction Tool", version, "CONTINUE");
			}
		});
	}

	/**
	 * Bind the managed property of a node to its visible property
	 *
	 * @param nodes
	 * 		a set of nodes
	 */
	private void setManagedProperty(final Node... nodes) {
		for (final var node : nodes) {
			node.managedProperty().bind(node.visibleProperty());
		}
	}

	public void setDrivesChanged(final boolean drivesChanged) {
		this.drivesChanged = drivesChanged;
	}

	/**
	 * This method will run only when the application first starts after update. It will replace the existing gpg key
	 * with the one provided by the new application. We are always assuming that the key could have changed.
	 */
	private void replacePublicKey() {
		// First delete the old key if it exists.
		try {
			Files.deleteIfExists(Path.of(DEFAULT_STORAGE, Constants.PUBLIC_KEY_LOCATION));
		} catch (final IOException e) {
			logger.error(e.getMessage());
		}
		// Then replace it with the key provided in the app resources.
		final var readStream = this.getClass().getClassLoader().getResourceAsStream("gpgPublicKey.asc");

		if (new File(DEFAULT_SYSTEM_FOLDER).mkdirs()) {
			logger.info("System folder created");
		}

		final var key = new File(DEFAULT_STORAGE, Constants.PUBLIC_KEY_LOCATION);

		try (final OutputStream outputStream = new FileOutputStream(key)) {
			if (readStream == null) {
				throw new HederaClientRuntimeException("Read stream is null");
			}
			IOUtils.copy(readStream, outputStream);
		} catch (final IOException exception) {
			logger.error(exception.getMessage());
		}
	}

	private void newVersionAction(final String lastVersion, final String thisVersion) {
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
			case INITIAL_SETUP_PHASE:
				setDisableButtons(true);
				homePane.setVisible(false);
				thisPane = initialStartupPane;
				initialStartupPane.setVisible(true);
				initialStartupPaneController.initializeStartupPane();
				initUpdater();
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
				initialStartupPaneController.setupTransactionDirectory(properties.getPreferredStorageDirectory());
				setDisableButtons(false);
				thisPane = homePane;
				homePane.setVisible(true);
				keysPaneController.initializeKeysPane();
				accountsPaneController.initializeAccountPane();
				historyPaneController.initializeHistoryPane();
				homePaneController.initializeHomePane();
				settingsPaneController.initializeSettingsPane();
				createPaneController.initializeCreatePane();
				initUpdater();
				break;
			case TEST_PHASE:
				properties =
						new UserAccessibleProperties(getPreferredStorageDirectory() + File.separator + USER_PROPERTIES,
								"");
				setDisableButtons(false);
				thisPane = homePane;
				homePane.setVisible(true);
				historyPaneController.initializeHistoryPane();
				historyPaneController.cleanHistory();
				historyPaneController.rebuild.setVisible(true);
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


	public void logAndDisplayError(final Exception e) {
		logger.error(e.getMessage());
		displaySystemMessage(e.toString());
	}
	//region NAVIGATION

	public void changePaneImageClicked(final javafx.event.ActionEvent event) {
		if (disableButtons) {
			return;
		}
		resetButtonBackgrounds();

		final var button = (Button) event.getSource();
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
			case "historyButton":
				changeTab(historyPane);
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + button.getId());
		}


	}

	public Map<Identifier, AccountInfo> getAccountInfoMap() {
		Map<Identifier, AccountInfo> map = null;
		try {
			map = new HashMap<>();
			final var accountInfoMap = properties.getAccountInfoMap();
			for (final var entry : accountInfoMap.entrySet()) {
				final var infoFile = entry.getValue();
				if (new File(infoFile).exists()) {
					final var info = AccountInfo.fromBytes(readBytes(infoFile));
					final var baseName = FilenameUtils.getBaseName(infoFile);
					final String ledger = getLedger(info, baseName);
					map.put(new Identifier(info.accountId, ledger), info);
				}
			}
		} catch (final InvalidProtocolBufferException | HederaClientException e) {
			logAndDisplayError(e);
		}

		return map;
	}

	private String getLedger(final AccountInfo info, final String baseName) {
		final var aString = info.ledgerId.toString();

		var ledgerString = baseName.contains("-") ? baseName.substring(baseName.indexOf("-") + 1) : "";

		return NetworkEnum.isNetwork(ledgerString) ?
				ledgerString :
				aString;
	}

	private void resetButtonBackgrounds() {
		final var buttons = menuButtonBar.getButtons();
		for (final var button : buttons) {
			button.setStyle("-fx-background-color: white;");
		}
	}

	void changeTab(final Pane next) {
		homePane.setVisible(false);
		accountsPane.setVisible(false);
		createPane.setVisible(false);
		settingsPane.setVisible(false);
		initialStartupPane.setVisible(false);
		final var lastPane = thisPane;
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

	public String getPreferredStorageDirectory() {
		return properties.getPreferredStorageDirectory();
	}

	public void setPreferredStorageDirectory(final String directory) {
		properties.setPreferredStorageDirectory(directory);
	}

	void setSetupPhase(final SetupPhase phase) {
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

	void setVersionString(final String versionString) {
		properties.setVersionString(versionString);
	}

	public void setUserName(final String userName) {
		preferences.put(USER_NAME, userName);
	}

	String getUserName() {
		return preferences.get(USER_NAME, "default");
	}

	int getLastIndex() {
		return preferences.getInt(LAST_INDEX, 0);
	}

	void setLastIndex(final int index) {
		preferences.putInt(LAST_INDEX, index);
	}

	void incrementIndex() {
		final var index = getLastIndex();
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
		final var defaultStorageDirectory = DEFAULT_STORAGE;
		final var userDefStorageDirectory = settingsPaneController.loadStorageTextField.getText();
		final var sourceDir = userDefStorageDirectory.equals("") ? new File(
				defaultStorageDirectory) : new File(userDefStorageDirectory);

		logger.info("Default storage {}", defaultStorageDirectory);
		logger.info("Before reset storage {}", sourceDir.getPath());
		final var secondsToArchive = Instant.now().getEpochSecond();
		final var resetStorageDirectory =
				String.format("%s.%s", sourceDir.getPath(), secondsToArchive);
		preferences.put(LAST_TRANSACTIONS_DIRECTORY, resetStorageDirectory);
		resetPreferences();
		final var userPreferenceFile = new File(sourceDir.getPath(), USER_PREFERENCE_FILE);
		if (userPreferenceFile.createNewFile()) {
			logger.info("New user preference file created");
		} else {
			logger.error("Could not create user preferences file");
		}
		preferences.exportNode(new FileOutputStream(userPreferenceFile, false));

		final var destination = new File(resetStorageDirectory);
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
	public void displaySystemMessage(final String error) {
		final var d = new Date();
		systemMessagesTextField.appendText(d + ": " + error + System.getProperty("line.separator"));
	}

	/**
	 * Display an error message on the hidden pane on home page
	 *
	 * @param exception
	 * 		an exception
	 */
	public void displaySystemMessage(final Exception exception) {
		final var d = new Date();
		systemMessagesTextField.appendText(
				d + ": " + exception.toString() + System.getProperty("line.separator"));
		logger.error(exception.getMessage());
	}

	/**
	 * Read the version from build.properties file. Version is displayed as
	 * Version, Time the last build is done in UTC, Last commit ID in the build.
	 */
	public String getVersion() {
		final var buildProps = new Properties();
		var version = "";
		try {
			buildProperties(buildProps);
			final var formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
			final var dateTime = formatter.parse(buildProps.get("git.build.time").toString());
			formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
			final var dateTimeUTC = formatter.format(dateTime);
			version = String.format("Version: %s, UTC-BUILD-TIME: %s, COMMIT-ID: %s",
					buildProps.get("git.build.version"),
					dateTimeUTC, buildProps.get("git.commit.id.abbrev"));
		} catch (final Exception ex) {
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
	private void exportVersionToServers(final String oldVersion, final String newVersion) {
		final var drives = properties.getOneDriveCredentials();
		for (final var entry : drives.entrySet()) {
			final var key = entry.getKey();
			final var value = entry.getValue();
			final var cleanName = newVersion.replace(":", "-").replace(".", "-").replace(",", "").replace(" ", "");
			final var fileName = String.format("%s/OutputFiles/%s/SoftwareUpdated-%s.txt", key, value, cleanName);
			if (!new File(fileName).getParentFile().exists()) {
				logger.error("Cannot export version: path {} does not exist", fileName);
				return;
			}
			try (final var writer = new BufferedWriter(new FileWriter(fileName))) {
				writer.write(
						String.format("Software updated from version %s to version %s on %s", oldVersion, newVersion,
								new Date()));
			} catch (final IOException e) {
				logger.error(e.getMessage());
			}
		}
	}

	private void buildProperties(final Properties buildProps) throws URISyntaxException {
		final var localPath = new File(
				StartUI.class.getProtectionDomain().getCodeSource().getLocation().toURI()).getParent() +
				"/build.properties";
		try (final InputStream inputStream = new FileInputStream(localPath)) {
			buildProps.load(inputStream);
		} catch (final IOException e) {
			logAndDisplayError(e);
		}
	}


	public byte[] getSalt() {
		return getSaltBytes(properties);
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

	public void setAccountInfoMap(final Map<String, String> accountInfos) {
		properties.setAccountInfoMap(accountInfos);
	}

	public void setLegacy(final boolean b) {
		properties.setLegacy(b);
	}

	public void resetProperties() {
		properties.resetProperties();
	}

	public void setOneDriveCredentials(final Map<String, String> objectObjectHashMap) {
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

	public void setGenerateRecord(final boolean b) {
		properties.setGenerateRecord(b);
	}

	public void setDefaultTxFee(final long fee) {
		properties.setDefaultTxFee(fee);
	}

	public void setDefaultSeconds(final int s) {
		properties.setDefaultSeconds(s);
	}

	public void setDefaultMinutes(final int m) {
		properties.setDefaultMinutes(m);
	}

	public void setDefaultHours(final int h) {
		properties.setDefaultHours(h);
	}

	public void setTxValidDuration(final int duration) {
		properties.setTxValidDuration(duration);
	}

	public void setAutoRenewPeriod(final int duration) {
		properties.setAutoRenewPeriod(duration);
	}

	public void setDefaultNodeID(final String node) {
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

	public void setHash(final char[] password) throws HederaClientException {
		properties.setHash(password);
	}

	public void setSalt(final boolean b) {
		properties.setSalt(b);
	}

	public String getEmailFromMap(final String path) {
		return properties.getEmailFromMap(path);
	}

	public boolean getGenerateRecord() {
		return properties.getGenerateRecord();
	}

	public Set<Identifier> getCustomFeePayers(final String network) {
		return properties.getCustomFeePayers(network);
	}

	public Set<Identifier> getCustomFeePayers() {
		return properties.getCustomFeePayers(getCurrentNetwork());
	}

	public void addCustomFeePayer(final Identifier identifier) {
		properties.addCustomFeePayer(identifier);
	}

	public void removeCustomFeePayer(final Identifier identifier) {
		properties.removeCustomFeePayer(identifier);
	}

	//endregion

	public String jsonKeyToPrettyString(final JsonObject key) {
		return keyStructureUtility.jsonKeyToPrettyString(key);
	}

	public String showKeyString(final ByteString key) {
		return keyStructureUtility.showKeyString(key);
	}

	public TreeView<String> buildKeyTreeView(final Key key) {
		return keyStructureUtility.buildKeyTreeView(key);
	}

	public TreeView<String> buildKeyTreeView(final JsonObject key) {
		return keyStructureUtility.buildKeyTreeView(key);
	}

	public void loadPubKeys() {
		keyStructureUtility.loadPubKeys();
	}

	public Map<String, Path> getPubFiles() {
		return keyStructureUtility.getPubFiles();
	}

	public JsonObject getAccountsList() {
		var object = new JsonObject();
		try {
			object = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
		return object;
	}

	public void removeAccount(final String account) {
		final var jsonObject = getAccountsList();
		if (jsonObject.has(account)) {
			jsonObject.remove(account);
		}
		try {
			writeJsonObject(ACCOUNTS_MAP_FILE, jsonObject);
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	public void setLastBrowsedDirectory(final File file) {
		if (file.isDirectory()) {
			properties.setLastBrowsedDirectory(file);
		} else {
			properties.setLastBrowsedDirectory(file.getParentFile());
		}
	}

	public Set<String> getCustomNetworks() {

		final var customNetworksFolder = new File(Constants.CUSTOM_NETWORK_FOLDER);
		if (customNetworksFolder.mkdirs()) {
			logger.info("Custom networks folder storage created");
		}
		final var networks = customNetworksFolder.listFiles(
				(dir, name) -> Constants.JSON_EXTENSION.equals(FilenameUtils.getExtension(name)));
		if (networks == null) {
			throw new HederaClientRuntimeException("Error reading custom networks");
		}
		final var networkSet =
				Arrays.stream(networks).map(network -> FilenameUtils.getBaseName(network.getName())).collect(
						Collectors.toSet());
		if (!networkSet.isEmpty()) {
			properties.setCustomNetworks(networkSet);
		}
		return networkSet;
	}

	public Set<String> getDefaultNetworks() {
		final Set<String> defaultNetworks = new HashSet<>();
		defaultNetworks.add("MAINNET");
		defaultNetworks.add("TESTNET");
		defaultNetworks.add("PREVIEWNET");
		return defaultNetworks;
	}

	public String getCurrentNetwork() {
		return properties.getCurrentNetwork();
	}

	public void setCurrentNetwork(final String network) {
		properties.setCurrentNetwork(network, getDefaultNetworks());
	}

	public Set<Identifier> getFeePayers() {
		return accountsPaneController.getFeePayers();
	}

	public Identifier getDefaultFeePayer() {
		return properties.getDefaultFeePayer();
	}

	public Identifier getDefaultFeePayer(final String network) {
		return properties.getDefaultFeePayer(network);
	}

	/**
	 * Returns the stored fee payers per known network
	 *
	 * @return a map where the keys are networks and the values are account nicknames.
	 */
	public Map<String, String> getDefaultFeePayers() {
		return properties.getDefaultFeePayers();
	}

	public void setDefaultFeePayer(final Identifier feePayer) {
		properties.setDefaultFeePayer(feePayer);
	}

	public void removeDefaultFeePayer(final String network) {
		properties.removeDefaultFeePayer(network);
	}

	void networkBoxSetup(final ChoiceBox<Object> comboBox) {
		networkBoxSetup(comboBox.getItems());
	}
	void networkBoxSetup(final ObservableList<Object> items) {
		items.clear();
		items.addAll(getDefaultNetworks());
		final var customNetworks = getCustomNetworks();
		if (!customNetworks.isEmpty()) {
			items.addAll(customNetworks);
		}
	}

	String setupChoiceBoxFeePayer(final ChoiceBox<Object> choiceBox, final TextField textfield) {
		return setupChoiceBoxFeePayer(choiceBox, textfield, getCurrentNetwork());
	}

	String setupChoiceBoxFeePayer(final ChoiceBox<Object> choiceBox, final TextField textfield,
			final String currentNetwork) {
		final var defaultFeePayer = getDefaultFeePayer(currentNetwork);
		choiceBox.getItems().clear();

		// In case the default was deleted
		if ((defaultFeePayer.getAccountNum() != 0) && !getCustomFeePayers().contains(defaultFeePayer)) {
			addCustomFeePayer(defaultFeePayer);
		}

		var feePayer = defaultFeePayer.getAccountNum() == 0 ? "" :
				defaultFeePayer.toNicknameAndChecksum(getAccountsList());

		final List<String> accounts = new ArrayList<>();

		for (final var payer : getFeePayers()) {
			if (payer.getNetworkName().equals(currentNetwork.toUpperCase(Locale.ROOT))) {
				final var toNicknameAndChecksum = payer.toNicknameAndChecksum(getAccountsList());
				accounts.add(toNicknameAndChecksum);
			}
		}
		accounts.sort(null);

		final List<String> customFeePayers = new ArrayList<>();
		getCustomFeePayers(currentNetwork).forEach(customFeePayer -> {
			final var s = customFeePayer.toNicknameAndChecksum(getAccountsList());
			if (accounts.contains(s)) {
				removeCustomFeePayer(customFeePayer);
			} else {
				customFeePayers.add(s);
			}
		});
		customFeePayers.sort(null);


		final Set<String> allPayers = new HashSet<>(accounts);
		allPayers.addAll(customFeePayers);
		final List<String> sortedAllPayers = new ArrayList<>(allPayers);
		Collections.sort(sortedAllPayers);

		final var custom = !customFeePayers.isEmpty();


		choiceBox.getItems().clear();
		choiceBox.getItems().addAll(accounts);
		if (custom) {
			choiceBox.getItems().add(new Separator());
			choiceBox.getItems().addAll(customFeePayers);
		}

		if (Identifier.ZERO.equals(defaultFeePayer)) {
			if (allPayers.isEmpty()) {
				textfield.setVisible(true);
				textfield.requestFocus();
			} else {
				feePayer = sortedAllPayers.get(0);
				setDefaultFeePayer(Identifier.parse(feePayer, currentNetwork));
			}
		}

		choiceBox.getSelectionModel().select(feePayer);

		textfield.setVisible(Identifier.ZERO.equals(defaultFeePayer));

		return feePayer;
	}

	/**
	 * Loads a map of public keys from the keys folder
	 *
	 * @return a map (HEX KEY, PATH)
	 */
	public Map<String, String> getPublicKeys() {
		final var map = new HashMap<String, String>();
		try {
			final var pubFiles = new File(KEYS_FOLDER).listFiles((dir, name) -> name.endsWith(PUB_EXTENSION));
			if (pubFiles == null) {
				return new HashMap<>();
			}
			for (final var pubFile : pubFiles) {
				final var absolutePath = pubFile.getAbsolutePath();
				map.put(Hex.toHexString(readBytes(absolutePath)), absolutePath);
			}
			logger.debug("pubFiles loaded");
		} catch (final Exception ex) {
			logger.error(ex.getMessage());
		}
		return map;
	}

	/**
	 * loads a map of private keys from the keys folder
	 *
	 * @return a map (KEY-NAME, FILE)
	 */
	@NotNull
	public Map<String, File> getPrivateKeys() {
		final var privateKeys = new HashMap<String, File>();

		final var files = new File(getPreferredStorageDirectory() + File.separator + "Keys").listFiles(
				(dir, name) -> name.endsWith(PK_EXTENSION));
		if (files != null) {
			for (final var file : files) {
				privateKeys.put(FilenameUtils.getBaseName(file.getName()), file);
			}
		}
		return privateKeys;
	}

	/**
	 * Given a list of signers, extract the list of private key files
	 *
	 * @param signers
	 * 		a set of public keys
	 * @return a list of the available private key files
	 */
	@NotNull
	public List<File> extractRequiredKeys(final Set<ByteString> signers) {
		final Map<String, File> privateKeys = getPrivateKeys();
		final Map<String, String> publicKeys = getPublicKeys();
		final List<File> requiredKeys = new ArrayList<>();
		for (final var signer : signers) {
			final var key = Hex.toHexString(Hex.encode(signer.toByteArray()));
			if (publicKeys.containsKey(key)) {
				final var filename = FilenameUtils.getBaseName(publicKeys.get(key));
				if (privateKeys.containsKey(filename)) {
					requiredKeys.add(privateKeys.get(filename));
				}
			}
		}
		requiredKeys.sort(homePaneController.new SortByFileBaseName());
		return requiredKeys;
	}

	/**
	 * Load a key pair from a file
	 *
	 * @param pemFile
	 * 		the file containing the private key
	 * @return a tuple with the name and the key pair
	 * @throws HederaClientException
	 * 		if the key cannot be decrypted
	 */
	public Pair<String, KeyPair> getAccountKeyPair(final File pemFile) throws HederaClientException {
		final var pair = getKeyPairUtility().getAccountKeyPair(pemFile);
		if (pair == null) {
			displaySystemMessage(String.format("File %s not decrypted", pemFile.getName()));
			throw new HederaClientException(String.format("File %s not decrypted", pemFile.getName()));
		}
		if (pair.getValue() == null) {
			displaySystemMessage(String.format("Invalid keypair in file %s", pemFile.getName()));
			throw new HederaClientException(String.format("Invalid keypair in file %s", pemFile.getName()));
		}
		return pair;
	}

	private void initUpdater() {
		if (updater != null) {
			updater.shutdown();
		}

		updater = new GithubUpdater(getVersion(), new File(DEFAULT_INTERNAL_FILES, INPUT_FILES),
				() -> homePaneController.setForceUpdate(true));

		final ChangeListener<Boolean> listener = new ChangeListener<>() {
			private boolean curValue = false;
			@Override
			public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
				if (curValue != newValue) {
					curValue = newValue;
					if (newValue) {
						updater.start(false);
					} else {
						updater.stop();
					}
				}
			}
		};

		homePane.sceneProperty().addListener((observableScene, oldScene, newScene) -> {
			if (newScene != null) {
				newScene.windowProperty().addListener((observableWindow, oldWindow, newWindow) -> {
					if (newWindow != null) {
						listener.changed(null, false, newWindow.isFocused());
						newWindow.focusedProperty().addListener(listener);
					}
				});
			}
		});
	}
}
