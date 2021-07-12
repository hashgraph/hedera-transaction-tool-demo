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
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.ui.pages.InitialStartupPage;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
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
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.concurrent.TimeoutException;
import java.util.function.BooleanSupplier;

import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCEPT_APP_PASSWORD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.APP_PASSWORD_FIELD_1;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.APP_PASSWORD_FIELD_2;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.COPY_TO_CLIPBOARD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FINISH_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.GENERATE_KEYS_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.LINK_FOLDERS_VBOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.MNEMONIC_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASSWORD_CHECK_IMAGE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASTE_FROM_CLIPBOARD;
import static java.lang.Boolean.parseBoolean;
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
	private static final String DIR_TEST_ONE_DRIVE = "src/test/resources/OneDrive/";
	private static final String CURRENT_RELATIVE_PATH = Paths.get("").toAbsolutePath().toString() + File.separator;
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

	private InitialStartupPage initialStartupPage;
	private UserAccessibleProperties properties;

	private String storedMnemonic =
			"DIGNITY DOMAIN INVOLVE REPORT SAIL MIDDLE RHYTHM HUSBAND USAGE PRETTY RATE TOWN " +
					"ACCOUNT SIDE EXTRA OUTER EAGLE EIGHT DESIGN PAGE REGULAR BIRD RACE ANSWER";

	//@BeforeEach
	@Before
	public void setUp() throws Exception {
		if (new File(USER_HOME + "Documents" + File.separator + "initialMap.json").delete()) {
			logger.info("Default drives file deleted");
		}
		if (new File(DEFAULT_STORAGE).exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
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
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		Controller controller = new Controller();
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
	public void enterAppPassword_Test() {

		try {
			initialStartupPage.enterPassword("123");
			assertTrue(find(APP_PASSWORD_FIELD_2).isDisabled());
			assertEquals("Passwords should be at least 10 characters long",
					((Label) find("#passwordErrorLabel")).getText());

			initialStartupPage.enterPassword("1234567890");
			assertTrue(find(APP_PASSWORD_FIELD_2).isDisabled());
			assertEquals("The password selected has been breached. Please select a more unique password",
					((Label) find("#passwordErrorLabel")).getText());

			initialStartupPage.enterPassword(
					"ukU1r5VQoYL7WB00JG6pM2k4NAZsjNdwvfdcpWkaClSGr9k9hAufjHDyLNF6wxfDnBlvW6OzPcLNVuo32wAo7ZP7hlghtFPBkQaKgeYY1PzIiiQagWU41HP4cOSauasophHgwgFabwC83Ufm0HXm1oQ6k65RvZGREh2LoSOlLd27H08crj5nMTikQjEvRMOdUcNCkQnNGEpsCaU53SpChyGii51BLsRIAuKoHtspugMGx5XjFopNsVO1Z10nTq6nlsy5hm4o9yPojxrP4B3agphSo9A3A8K8L0KkPronZZMZPWxxxbMUIqvcOaFiesyaj1BvTSTME2TU1zuq6nsAtW4rcfOQnt9FmLaGGxkyOiVej7XnCWeWjU403hvjiiWiVB7UWU9z9F7HjeMQpSGoesd5XiGEMH5DHP36cllXyzBrsd0IQSuPO6cGcYjiG8cCZwdATd5vJLrdLvuabkahw6QFw8CPosy81brplArtbxN6HGgFLQvxH3GQZ9ceCHglhjGMsabn3YV8Rni9mYEV8L2TAqTSGdchCQkdE6ba1DFHzHHp5fHSgqBxKNNsR2eawpVq3xJauTRhv3O7g8zeaSgQf67PCV7MW2aHvfVezuv2Ve8P9jWn1mx7gXfA5o8XKMpn2HuCbeAMl5FM2OnaLn9MKZMgCvEBR7uzfhVWPiTNdajQ511sYMKuNdtHD4xlAlFa0cIJ4XQb5oE86LlUuoqHNNRvKGGZ4xnoeFASv5ttvtknBPiCvV5hcfwPCHZJq0j0ZdBxCyb1wqzCwjIJ8layZfoStkqw7TIsHK0rXMbkTunhmOy4N7BtogymjMikRvxhN1IcMfn0FqWjgADl9UtYceMH6YUYuqUZtbdfD7GgtRqk1WVoAr7wDUG4CTF8QfoangAvsCWgN3goyNf12xz79bC7uFB7qYerQAfhdWFHa6NdQ0Kevhs1QPPD37FPVl9paM71xsGsp2SzgA9O74il90Ng6yJYMhF9IjCxglVwMnlkajaUwkr1iplStHCsK");
			assertTrue(find(APP_PASSWORD_FIELD_2).isDisabled());
			assertEquals("Passwords should be at most 1024 characters long",
					((Label) find("#passwordErrorLabel")).getText());

			initialStartupPage.enterPassword(PASSWORD);
			assertTrue(find("#checkPassword").isVisible());
			logger.info("Check mark is visible");

			assertFalse(find(APP_PASSWORD_FIELD_2).isDisabled());
			assertTrue(find(PASSWORD_CHECK_IMAGE).isVisible());
			logger.info("Second password field is visible and enabled");

			initialStartupPage.reEnterPassword("987654321");
			assertFalse(find(ACCEPT_APP_PASSWORD).isVisible());
			logger.info("Passwords don't match: Button is not visible");

			initialStartupPage.reEnterPassword(PASSWORD);
			assertTrue(find(ACCEPT_APP_PASSWORD).isVisible());
			logger.info("Passwords match: Button is visible");

			initialStartupPage.acceptPassword();
			assertTrue(find(LINK_FOLDERS_VBOX).isVisible());
			logger.info("Password accepted: Next step is visible");
		} catch (Exception e) {
			logger.error(e);
		}

	}

	@Test
	public void setOneDrive_Test() {

		try {
			logger.info("Setup password, then enter path and email");
			InitialStartupPage pane = initialStartupPage
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
		initialStartupPage.enterPassword(PASSWORD)
				.reEnterPassword(PASSWORD)
				.acceptPassword()
				.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
				.enterStringUsername("test1.council2@hederacouncil.org")
				.acceptOneDrive();
		logger.info("Password setup done; OneDrive setup done; Default storage accepted");

		GridPane emptyGrid = (GridPane) ((VBox) find("#phraseBox")).getChildren().get(0);
		ObservableList<Node> empty = emptyGrid.getChildren();
		assertEquals(24, empty.size());
		// Check all fields are empty
		for (Node n :
				empty) {
			assertTrue(n instanceof TextField);
			assertEquals("", ((TextField) n).getText());
		}
		logger.info("The grid is empty");

		initialStartupPage.clickOnGenerateMnemonic();
		GridPane fullGrid = (GridPane) ((VBox) find("#phraseBox")).getChildren().get(0);
		ObservableList<Node> full = fullGrid.getChildren();
		assertEquals(24, full.size());
		logger.info("Mnemonic is generated: the grid has been populated");
		// Check all fields are full and collect them in one string

		String words = "";
		for (Node n : full) {
			assertTrue(n instanceof TextField);
			assertNotEquals("", ((TextField) n).getText());
			words = words.concat(((TextField) n).getText()).concat(" ");
		}
		logger.info("The words in the grid correspond to the words generated");

		initialStartupPage.clickMnemonicPopupButton("OK");
		assertFalse(find(GENERATE_KEYS_BUTTON).isVisible());
		logger.info("Generate keys button is no longer visible");

		assertTrue(find(COPY_TO_CLIPBOARD).isVisible());
		logger.info("Copy to clipboard button is visible");

		assertTrue(find(FINISH_BOX).isVisible());
		assertTrue(find("FINISH").isVisible());
		logger.info("Next step is visible");

		byte[] salt = getSalt(properties.getHash(), properties.isLegacy());
		Mnemonic mnemonic = getMnemonicFromFile(PASSWORD.toCharArray(), salt,
				properties.getPreferredStorageDirectory() + File.separator + MNEMONIC_PATH);

		String storedWords = mnemonic.toString();

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

			String[] storedWords = storedMnemonic.toLowerCase().split(" ");

			initialStartupPage.enterPassword(PASSWORD)
					.reEnterPassword(PASSWORD)
					.acceptPassword()
					.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
					.enterStringUsername("test1.council2@hederacouncil.org")
					.acceptOneDrive();
			logger.info("Password setup done; OneDrive setup done; Default storage accepted");


			GridPane emptyGrid = (GridPane) ((VBox) find("#phraseBox")).getChildren().get(0);
			ObservableList<Node> empty = emptyGrid.getChildren();
			assertEquals(24, empty.size());
			// Check all fields are empty
			int count = 0;
			for (Node n :
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

			Mnemonic mnemonic =
					getMnemonicFromFile(PASSWORD.toCharArray(), getSalt(properties.getHash(), properties.isLegacy()),
							properties.getPreferredStorageDirectory() + File.separator + MNEMONIC_PATH);

			String[] recoveredWords = mnemonic.toString().split(" ");

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

		initialStartupPage.enterPassword(PASSWORD)
				.reEnterPassword(PASSWORD)
				.acceptPassword()
				.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
				.enterStringUsername("test1.council2@hederacouncil.org")
				.acceptOneDrive();
		logger.info("Password setup done; OneDrive setup done; Default storage accepted");
		assertTrue(find("PASTE").isVisible());

		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

		StringSelection longStringSelection = new StringSelection("test " + storedMnemonic);
		StringSelection shortStringSelection =
				new StringSelection("Dignity domain involve report sail middle rhythm husband usage pretty");
		StringSelection latinStringSelection = new StringSelection(
				"magna di curant, parva neglegunt. magna di curant, parva neglegunt. magna di curant, parva neglegunt" +
						". magna di curant, parva neglegunt. magna di curant, parva");
		StringSelection goodStringSelection = new StringSelection(storedMnemonic);

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

		initialStartupPage.enterPassword(PASSWORD)
				.reEnterPassword(PASSWORD)
				.acceptPassword()
				.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
				.enterStringUsername("test1.council2@hederacouncil.org")
				.acceptOneDrive();
		logger.info("Password setup done; OneDrive setup done; Default storage accepted");

		GridPane emptyGrid = (GridPane) ((VBox) find("#phraseBox")).getChildren().get(0);
		ObservableList<Node> empty = emptyGrid.getChildren();
		assertEquals(24, empty.size());


		assertTrue(empty.get(0) instanceof TextField);
		TextField textField = (TextField) empty.get(0);
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

	//@Test
	public void finishSetup_test() {
		try {
			initialStartupPage.enterPassword(PASSWORD)
					.reEnterPassword(PASSWORD)
					.acceptPassword()
					.enterOneDriveFolder("/src/test/resources/Transactions - Documents")
					.enterStringUsername("test1.council2@hederacouncil.org")
					.acceptOneDrive()
					.clickOnGenerateMnemonic()
					.clickMnemonicPopupButton("OK")
					.clickOnFinishSetup();

			logger.info("App setup complete");

			assertEquals(SetupPhase.NORMAL_OPERATION_PHASE, properties.getSetupPhase());

			properties.setSetupPhase(SetupPhase.TEST_PHASE);

			logger.info("Restarting stage");
			FxToolkit.registerPrimaryStage();
			FxToolkit.setupApplication(StartUI.class);

			Node home = find("#homePane");
			assertTrue(home.isVisible());
			logger.info("After initial setup is done, home pane is visible");
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	public void testCreateLocalOneDriveFolders_Test() throws IOException {
		logger.info("Setup password, then enter path and email");
		InitialStartupPage pane = initialStartupPage
				.enterPassword(PASSWORD)
				.reEnterPassword(PASSWORD)
				.acceptPassword()
				.enterOneDriveFolder(DIR_TEST_ONE_DRIVE);

		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "InputFiles").exists());
		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "OutputFiles").exists());

		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "InputFiles").isDirectory());
		assertTrue(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "OutputFiles").isDirectory());

		initialStartupPage.enterStringUsername("test1.council2@hederacouncil.org");

		assertTrue(find("#confirmAddFolderButton").isVisible());
		pane.acceptOneDrive();

		assertTrue(new File(
				CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "OutputFiles/test1.council2@hederacouncil.org").isDirectory());
		assertTrue(new File(
				CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "OutputFiles/test1.council2@hederacouncil.org").exists());


		logger.info("Path and email are verified: Add OneDrive folder button is visible");

		File in = new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "InputFiles");
		File ou = new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE + "OutputFiles");

		if (in.exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(in);
		}
		if (ou.exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(ou);
		}
	}

	@Test
	public void testDefaultDrivesSetup_test() throws TimeoutException, HederaClientException, IOException {

		final File initialMap = buildDrivesJson();

		logger.info("Restarting stage");
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		logger.info("Setup password, then enter verify path and email exist");
		initialStartupPage.enterPassword(PASSWORD)
				.reEnterPassword(PASSWORD)
				.acceptPassword();

		String oneDrive = new File(DIR_TEST_ONE_DRIVE).getAbsolutePath();
		File in = new File(oneDrive + "/InputFiles");
		File out = new File(oneDrive + "/OutputFiles");

		logger.info(in.getAbsolutePath());

		assertTrue(in.exists());
		assertTrue(out.exists());
		assertTrue(in.isDirectory());
		assertTrue(out.isDirectory());

		assertTrue(new File(out.getAbsolutePath() + "/test1.council2@hederacouncil.org").isDirectory());
		assertTrue(new File(out.getAbsolutePath() + "/test1.council2@hederacouncil.org").exists());

		logger.info("Path and email are verified");

		Node box = find("#passphraseBox");
		assertTrue(box.isVisible());

		if (in.exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(in);
		}

		if (out.exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(out);
		}

		if (initialMap.exists()) {
			initialMap.deleteOnExit();
		}
	}

	private File buildDrivesJson() throws HederaClientException {
		File one = new File(DIR_TEST_ONE_DRIVE);
		logger.info(one.getAbsolutePath());
		String home = (isInCircleCi.getAsBoolean()) ? "/repo" : USER_HOME;
		File user = new File(home);
		logger.info(user.getAbsolutePath());
		assertTrue(one.getAbsolutePath().contains(user.getAbsolutePath()));

		JsonObject map = new JsonObject();
		JsonArray array = new JsonArray();
		JsonObject element = new JsonObject();
		element.addProperty("drive", one.getAbsolutePath().replace(user.getAbsolutePath(), ""));
		element.addProperty("email", "test1.council2@hederacouncil.org");
		array.add(element);
		map.add("map", array);
		final File initialMap = new File(USER_HOME + "Documents" + File.separator + "initialMap.json");
		writeJsonObject(initialMap.getAbsolutePath(), map);
		return initialMap;
	}

	@Test
	public void passwordPolicy_test() {
		final PasswordPolicy passwordPolicy = new PasswordPolicy(BreachDatabase.top100K(), 10, 256);
		assertEquals(Status.TOO_SHORT, passwordPolicy.check("password"));
		assertEquals(Status.BREACHED, passwordPolicy.check("1234567890"));
	}

	@AfterEach
	public void tearDown() throws IOException {
		Path currentRelativePath = Paths.get("");
		String s = currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/testDirectory";
		if ((new File(s)).exists()) {
			FileUtils.deleteDirectory(new File(s));
		}

		String toolsFolder =
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents/TransactionTools";
		if (new File(toolsFolder).exists()) {
			FileUtils.deleteDirectory(new File(toolsFolder));
		}

		if (new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE).exists()) {
			Files.delete(new File(CURRENT_RELATIVE_PATH + DIR_TEST_ONE_DRIVE));
		}

		properties.resetProperties();
		logger.info("Preferences cleared");
		if (new File(DEFAULT_STORAGE).exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
		release(new KeyCode[] { });
		release(new MouseButton[] { });
	}

	private Mnemonic getMnemonicFromFile(final char[] password, byte[] salt, String path) {
		File mnemonicFile = new File(path);
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
}
