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

import com.codahale.passpol.BreachDatabase;
import com.codahale.passpol.PasswordPolicy;
import com.codahale.passpol.Status;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.ui.pages.InitialStartupPage;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.scene.Node;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.assertj.core.util.Files;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.AfterEach;
import org.testfx.api.FxRobotException;
import org.testfx.api.FxToolkit;

import javax.swing.JFileChooser;
import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeoutException;
import java.util.function.BooleanSupplier;

import static com.hedera.hashgraph.client.core.constants.Constants.MAX_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MIN_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.APP_PASSWORD_FIELD_1;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.COPY_TO_CLIPBOARD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FINISH_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.GENERATE_KEYS_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.MNEMONIC_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASTE_FROM_CLIPBOARD;
import static java.lang.Boolean.parseBoolean;
import static org.apache.commons.io.FileUtils.deleteDirectory;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class InitialStartupPaneControllerTest extends TestBase implements GenericFileReadWriteAware {

	public static BooleanSupplier isInCircleCi = () ->
			parseBoolean(Optional.ofNullable(System.getenv("IN_CIRCLE_CI")).orElse("false"));

	public static final String PASSWORD = "tempura hopscotch";
	public static final String USER_HOME = System.getProperty("user.home") + File.separator;
	private final Logger logger = LogManager.getLogger(InitialStartupPage.class);
	private static final String DIR_TEST_ONE_DRIVE = "src/test/resources/OneDrive";
	private static final String CURRENT_RELATIVE_PATH = Paths.get("").toAbsolutePath() + File.separator;
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

	private InitialStartupPage initialStartupPage;
	private UserAccessibleProperties properties;

	private final String storedMnemonic =
			"DIGNITY DOMAIN INVOLVE REPORT SAIL MIDDLE RHYTHM HUSBAND USAGE PRETTY RATE TOWN " +
					"ACCOUNT SIDE EXTRA OUTER EAGLE EIGHT DESIGN PAGE REGULAR BIRD RACE ANSWER";

	//@BeforeEach
	@Before
	public void setUp() throws Exception {
		if (new File(USER_HOME + "Documents" + File.separator + "initialMap.json").delete()) {
			logger.info("Default drives file deleted");
		}
		if (new File(DEFAULT_STORAGE).exists()) {
			deleteDirectory(new File(DEFAULT_STORAGE));
		}
		if (new File(DEFAULT_STORAGE + "/Files/").mkdirs()) {
			logger.info("Files directory created");
		}

		logger.info("Resetting preferences");
		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");
		properties.resetProperties();
		properties.setSetupPhase(SetupPhase.INITIAL_SETUP_PHASE);

		logger.info("Setting up folder structure");

		if (new File(
				CURRENT_RELATIVE_PATH + "/src/test/resources/Transactions - Documents/OutputFiles/test1" +
						".council2@hederacouncil.org/").mkdirs()) {
			logger.info("Output path created");
		}

		if (new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "/InputFiles/").mkdirs()) {
			logger.info("Output path created");
		}

		if (new File(
				CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "/OutputFiles/test1.council2@hederacouncil.org/").mkdirs()) {
			logger.info("Output path created");
		}

		if ((new File(DEFAULT_STORAGE)).exists()) {
			deleteDirectory(new File(DEFAULT_STORAGE));
		}

		var controller = new Controller();
		var version = controller.getVersion();
		properties.setVersionString(version);

		logger.info("Setting up stage");
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);
		initialStartupPage = new InitialStartupPage(this);
		logger.info("Setup done");
	}

	@Test(expected = FxRobotException.class)
	public void clickOnBogusItem_Test() {
		logger.info("clickOnBogusItem_Test");
		clickOn("#exterminate");
	}

	@Test
	public void clickOnRealItem_Test() {
		try {
			logger.info("clickOnRealItem_Test");
			assertTrue(find(APP_PASSWORD_FIELD_1).isVisible());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	public void setOneDrive_Test() {

		try {
			logger.info("Setup password, then enter path and email");
			var pane = initialStartupPage
					.enterPassword(PASSWORD)
					.reEnterPassword(PASSWORD)
					.acceptPassword()
					.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
					.enterStringUsername("test1.council2@hederacouncil.org");

			assertTrue(find("#confirmAddFolderButton").isVisible());
			pane.acceptOneDrive();
			logger.info("Path and email are verified: Add OneDrive folder button is visible");

			sleep(500);
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	public void generatePassphrase_Test() {
		initialStartupPage.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
				.enterStringUsername("test1.council2@hederacouncil.org")
				.acceptOneDrive();
		logger.info("Password setup done; OneDrive setup done; Default storage accepted");

		var emptyGrid = (GridPane) ((VBox) find("#phraseBox")).getChildren().get(0);
		var empty = emptyGrid.getChildren();
		assertEquals(24, empty.size());
		// Check all fields are empty
		for (var n :
				empty) {
			assertTrue(n instanceof TextField);
			assertEquals("", ((TextField) n).getText());
		}
		logger.info("The grid is empty");

		initialStartupPage.clickOnGenerateMnemonic();
		var fullGrid = (GridPane) ((VBox) find("#phraseBox")).getChildren().get(0);
		var full = fullGrid.getChildren();
		assertEquals(24, full.size());
		logger.info("Mnemonic is generated: the grid has been populated");
		// Check all fields are full and collect them in one string

		var words = "";
		for (var n : full) {
			assertTrue(n instanceof TextField);
			assertNotEquals("", ((TextField) n).getText());
			words = words.concat(((TextField) n).getText()).concat(" ");
		}
		logger.info("The words in the grid correspond to the words generated");

		initialStartupPage.clickMnemonicPopupButton("OK")
				.enterNewPasswordInPopup(PASSWORD);


		assertFalse(find(GENERATE_KEYS_BUTTON).isVisible());
		logger.info("Generate keys button is no longer visible");

		assertTrue(find(COPY_TO_CLIPBOARD).isVisible());
		logger.info("Copy to clipboard button is visible");

		assertTrue(find(FINISH_BOX).isVisible());
		assertTrue(find("FINISH").isVisible());
		logger.info("Next step is visible");

		var salt = getSalt(properties.getHash(), properties.isLegacy());
		var mnemonic = getMnemonicFromFile(PASSWORD.toCharArray(), salt,
				properties.getPreferredStorageDirectory() + File.separator + MNEMONIC_PATH);

		var storedWords = mnemonic.toString();

		assertEquals(storedWords.concat(" ").toLowerCase(), words.toLowerCase());
		logger.info("The stored mnemonic is the same as the words displayed: Mnemonic can be decrypted using password");
	}


	@Test
	public void generatePassphraseFromProvidedWords_Text() {

		// Test will not work in headless mode
		if (parseBoolean(System.getProperty("headless"))) {
			logger.info("Cannot run headless test");
			return;
		}

		try {
			logger.info("generatePassphraseFromProvidedWords_Text");
			logger.info("Load stored mnemonic for testing");

			var storedWords = storedMnemonic.toLowerCase().split(" ");

			initialStartupPage.enterPassword(PASSWORD)
					.reEnterPassword(PASSWORD)
					.acceptPassword()
					.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
					.enterStringUsername("test1.council2@hederacouncil.org")
					.acceptOneDrive();
			logger.info("Password setup done; OneDrive setup done; Default storage accepted");


			var emptyGrid = (GridPane) ((VBox) find("#phraseBox")).getChildren().get(0);
			var empty = emptyGrid.getChildren();
			assertEquals(24, empty.size());
			// Check all fields are empty
			var count = 0;
			for (var n :
					empty) {
				assertTrue(n instanceof TextField);
				assertEquals("", ((TextField) n).getText());
				clickOn(n);
				write(storedWords[count++]);
				type(KeyCode.ENTER);
			}

			logger.info("Words have been entered in the empty grid: Ready to generate mnemonic");

			initialStartupPage.clickOnGenerateMnemonic();
			initialStartupPage.clickMnemonicPopupButton("OK");

			assertFalse(find(GENERATE_KEYS_BUTTON).isVisible());
			logger.info("Generate keys button is no longer visible");

			assertTrue(find(COPY_TO_CLIPBOARD).isVisible());
			logger.info("Copy to clipboard button is visible");

			assertTrue(find(FINISH_BOX).isVisible());
			assertTrue(find("FINISH").isVisible());
			logger.info("Next step is visible");

			var mnemonic =
					getMnemonicFromFile(PASSWORD.toCharArray(), getSalt(properties.getHash(), properties.isLegacy()),
							properties.getPreferredStorageDirectory() + File.separator + MNEMONIC_PATH);

			var recoveredWords = mnemonic.toString().split(" ");

			assertArrayEquals(storedWords, recoveredWords);
			logger.info("The stored mnemonic is the same as the words displayed: Mnemonic can be decrypted using " +
					"password");
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	public void generatePassphraseFromPaste_Text() {
		// Test will not work in headless mode
		if (parseBoolean(System.getProperty("headless"))) {
			logger.info("Cannot run headless test");
			return;
		}

		initialStartupPage.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
				.enterStringUsername("test1.council2@hederacouncil.org")
				.acceptOneDrive();
		logger.info("Password setup done; OneDrive setup done; Default storage accepted");
		assertTrue(find("PASTE").isVisible());

		var clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

		var longStringSelection = new StringSelection("test " + storedMnemonic);
		var shortStringSelection =
				new StringSelection("Dignity domain involve report sail middle rhythm husband usage pretty");
		var latinStringSelection = new StringSelection(
				"magna di curant, parva neglegunt. magna di curant, parva neglegunt. magna di curant, parva neglegunt" +
						". magna di curant, parva neglegunt. magna di curant, parva");
		var goodStringSelection = new StringSelection(storedMnemonic);

		clipboard.setContents(longStringSelection, longStringSelection);
		initialStartupPage.pressPaste();
		assertFalse(find(COPY_TO_CLIPBOARD).isVisible());
		assertTrue(find(PASTE_FROM_CLIPBOARD).isVisible());
		assertTrue(find(MNEMONIC_ERROR_MESSAGE).isVisible());

		clipboard.setContents(shortStringSelection, shortStringSelection);
		initialStartupPage.pressPaste();
		assertFalse(find(COPY_TO_CLIPBOARD).isVisible());
		assertTrue(find(PASTE_FROM_CLIPBOARD).isVisible());
		assertTrue(find(MNEMONIC_ERROR_MESSAGE).isVisible());

		clipboard.setContents(latinStringSelection, latinStringSelection);
		initialStartupPage.pressPaste();
		assertFalse(find(GENERATE_KEYS_BUTTON).isVisible());
		assertTrue(find(PASTE_FROM_CLIPBOARD).isVisible());
		assertFalse(find(MNEMONIC_ERROR_MESSAGE).isVisible());


		clipboard.setContents(goodStringSelection, goodStringSelection);
		initialStartupPage.pressPaste();

		assertTrue(find(GENERATE_KEYS_BUTTON).isVisible());

	}

	@Test
	public void checkPredictiveTextInPassphrase_text() {
		logger.info("Load stored mnemonic for testing");

		initialStartupPage.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
				.enterStringUsername("test1.council2@hederacouncil.org")
				.acceptOneDrive();
		logger.info("Password setup done; OneDrive setup done; Default storage accepted");

		var emptyGrid = (GridPane) ((VBox) find("#phraseBox")).getChildren().get(0);
		var empty = emptyGrid.getChildren();
		assertEquals(24, empty.size());


		assertTrue(empty.get(0) instanceof TextField);
		var textField = (TextField) empty.get(0);
		assertEquals("", textField.getText());
		clickOn(textField);

		write("p");

		assertTrue(find("pact").isVisible());
		assertTrue(find("paddle").isVisible());
		assertTrue(find("page").isVisible());
		assertTrue(find("pair").isVisible());
		assertTrue(find("palace").isVisible());

		write("o");
		assertNull(find("pact"));
		assertNull(find("paddle"));
		assertNull(find("page"));
		assertNull(find("pair"));
		assertNull(find("palace"));

		assertTrue(find("poem").isVisible());
		assertTrue(find("poet").isVisible());
		assertTrue(find("point").isVisible());
		assertTrue(find("polar").isVisible());
		assertTrue(find("pole").isVisible());

		clickOn((Node) find("point"));
		write("\n");

		assertTrue(empty.get(1).isFocused());

		write("poet\n");
		assertTrue(empty.get(2).isFocused());

	}

	@Test
	public void testCreateLocalOneDriveFolders_Test() throws IOException {
		logger.info("Setup password, then enter path and email");
		var pane = initialStartupPage
				.enterOneDriveFolder(DIR_TEST_ONE_DRIVE);

		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE, "InputFiles").exists());
		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE, "OutputFiles").exists());

		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE, "InputFiles").isDirectory());
		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE, "OutputFiles").isDirectory());

		initialStartupPage.enterStringUsername("test1.council2@hederacouncil.org");

		assertTrue(find("#confirmAddFolderButton").isVisible());
		pane.acceptOneDrive();

		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE,
				"OutputFiles/test1.council2@hederacouncil.org").exists());
		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE,
				"OutputFiles/test1.council2@hederacouncil.org").isDirectory());

		logger.info("Path and email are verified: Add OneDrive folder button is visible");

		var in = new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE, "InputFiles");
		var ou = new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE, "OutputFiles");

		if (in.exists()) {
			deleteDirectory(in);
		}
		if (ou.exists()) {
			deleteDirectory(ou);
		}
	}

	@Test
	public void testDefaultDrivesSetup_test() throws TimeoutException, HederaClientException, IOException {

		final var initialMap = buildDrivesJson(1);

		logger.info("Restarting stage");
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		logger.info("Setup password, then enter verify path and email exist");

		var oneDrive = new File(DIR_TEST_ONE_DRIVE).getAbsolutePath();
		var in = new File(oneDrive + "/InputFiles");
		var out = new File(oneDrive + "/OutputFiles");

		logger.info(in.getAbsolutePath());

		assertTrue(in.exists());
		assertTrue(out.exists());
		assertTrue(in.isDirectory());
		assertTrue(out.isDirectory());

		assertTrue(new File(out.getAbsolutePath() + "/test1.council2@hederacouncil.org").isDirectory());
		assertTrue(new File(out.getAbsolutePath() + "/test1.council2@hederacouncil.org").exists());
		logger.info("Path and email are verified");

		var box = find("#passphraseBox");
		assertTrue(box.isVisible());

		if (in.exists()) {
			deleteDirectory(in);
		}

		if (out.exists()) {
			deleteDirectory(out);
		}

		if (initialMap.exists()) {
			initialMap.deleteOnExit();
		}
		deleteDummyDrive(0);
	}

	@Test
	public void testDefaultDrivesSetupOverLimit_test() throws TimeoutException, HederaClientException, IOException {

		final var testSize = 50;
		final var initialMap = buildDrivesJson(testSize);
		logger.info("Restarting stage");
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		logger.info("Setup password, then enter verify path and email exist");

		if (initialMap.exists()) {
			initialMap.deleteOnExit();
		}

		var map = properties.getOneDriveCredentials();
		assertEquals(Constants.DRIVE_LIMIT, map.size());
		for (Map.Entry<String, String> entry : map.entrySet()) {
			String drive = entry.getKey();
			String email = entry.getValue();
			int index = Integer.parseInt(drive.substring(drive.length() - 2));
			assertEquals(String.format("test1.council%02d@hederacouncil.org", index), email);
		}

		var box = find("#passphraseBox");

		assertTrue(box.isVisible());

		for (int i = 0; i < testSize; i++) {
			deleteDummyDrive(i);
		}
	}

	@Test
	public void testDefaultDrivesSetupUnderLimit_test() throws TimeoutException, HederaClientException, IOException {

		final var initialMap = buildDrivesJson(0);
		logger.info("Restarting stage");
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		logger.info("Setup password, then enter verify path and email exist");

		var box = find("#passphraseBox");
		assertFalse(box.isVisible());

		if (initialMap.exists()) {
			initialMap.deleteOnExit();
		}
		for (int i = 0; i < 10; i++) {
			deleteDummyDrive(i);
		}
	}


	@Test
	public void passwordPolicy_test() {
		final var passwordPolicy =
				new PasswordPolicy(BreachDatabase.anyOf(BreachDatabase.top100K(), BreachDatabase.haveIBeenPwned()),
						MIN_PASSWORD_LENGTH, MAX_PASSWORD_LENGTH);
		assertEquals(Status.TOO_SHORT, passwordPolicy.check("password"));
		assertEquals(Status.BREACHED, passwordPolicy.check("1234567890"));
	}

	@AfterEach
	public void tearDown() throws IOException {
		var currentRelativePath = Paths.get("");
		var s = currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory";
		if ((new File(s)).exists()) {
			deleteDirectory(new File(s));
		}

		var toolsFolder =
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents/TransactionTools";
		if (new File(toolsFolder).exists()) {
			deleteDirectory(new File(toolsFolder));
		}

		if (new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE).exists()) {
			Files.delete(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE));
		}

		properties.resetProperties();
		logger.info("Preferences cleared");
		if (new File(DEFAULT_STORAGE).exists()) {
			deleteDirectory(new File(DEFAULT_STORAGE));
		}
		release(new KeyCode[] { });
		release(new MouseButton[] { });
	}

	private Mnemonic getMnemonicFromFile(final char[] password, byte[] salt, String path) {
		var mnemonicFile = new File(path);
		Mnemonic mnemonic = null;
		try {
			if (mnemonicFile.exists()) {
				mnemonic = SecurityUtilities.fromEncryptedFile(password, salt, path);
			}
		} catch (HederaClientException e) {
			logger.error(e);
		}
		return mnemonic;

	}

	/**
	 * Builds a json file containing drive/email pairs for testing
	 *
	 * @param k
	 * 		the number of drives
	 * @return the json file
	 */
	private File buildDrivesJson(int k) throws HederaClientException {
		final var initialMap = new File(USER_HOME + "Documents", "initialMap.json");
		if (k == 0) {
			writeJsonObject(initialMap.getAbsolutePath(), new JsonObject());
			return initialMap;
		}

		var one = new File(DIR_TEST_ONE_DRIVE);
		logger.info(one.getAbsolutePath());
		var home = (isInCircleCi.getAsBoolean()) ? "/repo" : USER_HOME;
		var user = new File(home);
		logger.info(user.getAbsolutePath());
		assertTrue(one.getAbsolutePath().contains(user.getAbsolutePath()));

		var map = new JsonObject();
		var array = new JsonArray();
		final var drive = one.getAbsolutePath().replace(user.getAbsolutePath(), "");

		for (var i = 0; i < k; i++) {
			var element = new JsonObject();
			element.addProperty("drive", String.format("%s%02d", drive, i));
			element.addProperty("email", String.format("test1.council%02d@hederacouncil.org", i));
			setupDummyOneDrive(i);
			array.add(element);
		}
		map.add("map", array);

		writeJsonObject(initialMap.getAbsolutePath(), map);
		return initialMap;
	}

	/**
	 * Creates a dummy drive in the test resources directory
	 *
	 * @param i
	 * 		the ordinal number of the drive
	 */
	private void setupDummyOneDrive(int i) {
		logger.info("Setting up dummy one drive");

		final var dirName = String.format("%s%02d", DIR_TEST_ONE_DRIVE, i);
		if (!new File(dirName).exists()) {
			if (new File(dirName, "InputFiles").mkdirs() && new File(dirName,
					String.format("OutputFiles/test1.council%02d@hederacouncil.org", i)).mkdirs()) {
				logger.info("Drive created");
			}
		}
	}

	/**
	 * Deletes a dummy drive
	 *
	 * @param i
	 * 		the ordinal number of the drive
	 */
	private void deleteDummyDrive(int i) throws IOException {
		logger.info("Deleting up dummy one drive");
		final var directory = new File(String.format("%s%02d", DIR_TEST_ONE_DRIVE, i));
		if (directory.exists()) {
			deleteDirectory(directory);
		}
	}
}
