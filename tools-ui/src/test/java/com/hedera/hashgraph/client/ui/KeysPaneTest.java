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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.KeysPanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.utilities.AutoCompleteTextField;
import com.hedera.hashgraph.client.ui.utilities.KeysTableRow;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.testfx.api.FxRobotException;
import org.testfx.api.FxToolkit;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;

import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_NICKNAME_ERROR_LABEL;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_RECOVER_KEY_INDEX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_RECOVER_KEY_NICKNAME;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_KEYS_VBOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_CREATE_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_GENERATE_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_RECOVERY_PHRASE_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_RECOVER_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NICKNAME;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PUBLIC_KEYS_VBOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SIGNING_KEYS_VBOX;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findButtonInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static junit.framework.TestCase.assertNull;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class KeysPaneTest extends TestBase {
	private static final String OUTPUT_PATH =
			"/src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org/";
	private KeysPanePage keysPanePage;
	private MainWindowPage mainWindowPage;

	private final Path currentRelativePath = Paths.get("");
	public UserAccessibleProperties properties;
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private static final String PASSWORD = "123456789";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	private final String storedMnemonic =
			"DIGNITY DOMAIN INVOLVE REPORT SAIL MIDDLE RHYTHM HUSBAND USAGE PRETTY RATE TOWN " +
					"ACCOUNT SIDE EXTRA OUTER EAGLE EIGHT DESIGN PAGE REGULAR BIRD RACE ANSWER";

	@Before
	public void setUp() throws Exception {

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		if (new File(DEFAULT_STORAGE).mkdirs()) {
			logger.info("TransactionTools folder created");
		}

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");

		if (new File(currentRelativePath.toAbsolutePath() + OUTPUT_PATH).mkdirs()) {
			logger.info("Output path created");
		}

		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		properties.setMnemonicHashCode(-915976044);

		Map<String, String> emailMap = new HashMap<>();

		emailMap.put(
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");


		var objectMapper = new ObjectMapper();
		var mapAsString = objectMapper.writeValueAsString(emailMap);

		properties.setOneDriveCredentials(emailMap);
		properties.setHash(PASSWORD.toCharArray());
		properties.setSalt(true);
		properties.setLegacy(true);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);

		var controller = new Controller();
		var version = controller.getVersion();
		properties.setVersionString(version);

		setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE, MNEMONIC_PATH));

		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);
		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
		}

		keysPanePage = new KeysPanePage(this);
		mainWindowPage = new MainWindowPage(this);
		mainWindowPage.clickOnKeysButton();
	}

	@Test(expected = FxRobotException.class)
	public void clickOnBogusItem_Test() {
		logger.info("clickOnBogusItem_Test");
		clickOn("#exterminate");
	}

	@Test
	public void changeMnemonicPassword_test() {
		logger.info("changeMnemonicPassword_test");
		keysPanePage.pressRecoveryPhrase()
				.enterPopupPassword(PASSWORD);
		VBox gridPaneVBox = find("#recoveryVBox");
		assertNotNull(gridPaneVBox);
		assertTrue(gridPaneVBox.isVisible());

		var mnemonic = ((Label) (((HBox) gridPaneVBox.getChildren().get(1)).getChildren()).get(0)).getText();

		var change = find("CHANGE PASSWORD");
		assertTrue(change.isVisible());
		clickOn(change);
		var passwordFields = keysPanePage.getPopupPasswordFields();
		keysPanePage.typePassword("tempura sushi", passwordFields.get(0));
		Assertions.assertFalse(passwordFields.get(1).isDisable());
		keysPanePage.typePassword("tempura sushi", passwordFields.get(1));
		var newPopupButtons = keysPanePage.getPopupButtons();
		clickOn(newPopupButtons.get(0));
		clickOn("CLOSE");
		keysPanePage.pressRecoveryPhrase()
				.enterPopupPassword(PASSWORD);
		gridPaneVBox = find("#recoveryVBox");
		assertNotNull(gridPaneVBox);
		assertTrue(gridPaneVBox.isVisible());

		var mnemonicAfterCancel = ((Label) (((HBox) gridPaneVBox.getChildren().get(1)).getChildren()).get(0)).getText();
		assertEquals(mnemonic, mnemonicAfterCancel);
		assertTrue(change.isVisible());
		clickOn(change);
		passwordFields = keysPanePage.getPopupPasswordFields();
		keysPanePage.typePassword("tempura sushi", passwordFields.get(0));
		Assertions.assertFalse(passwordFields.get(1).isDisable());
		keysPanePage.typePassword("tempura sushi", passwordFields.get(1));
		var newPopupButtons1 = keysPanePage.getPopupButtons();
		clickOn(newPopupButtons1.get(1));
		var mnemonicBox = find("#mnemonicWordsVBox");
		assertFalse(mnemonicBox.isVisible());

		keysPanePage.pressRecoveryPhrase()
				.enterPopupPassword("tempura sushi");
		var mnemonicAfterChange = ((Label) (((HBox) gridPaneVBox.getChildren().get(1)).getChildren()).get(0)).getText();

		assertEquals(mnemonic, mnemonicAfterChange);

	}

	@Test
	public void generateKeyWorkflow_Test() throws KeyStoreException {
		logger.info("generateKeyWorkflow_Test");
		assertTrue(find(KEYS_GENERATE_KEYS).isVisible());

		assertTrue(find(KEYS_RECOVER_KEYS).isVisible());
		assertTrue(find(KEYS_RECOVERY_PHRASE_BUTTON).isVisible());
		logger.info("All buttons are visible");

		keysPanePage.pressGenerateKeyButton();
		assertFalse(find(KEYS_GENERATE_KEYS).isVisible());
		assertTrue(find(NICKNAME).isVisible());
		assertTrue(find(KEYS_CREATE_KEYS).isVisible());
		assertTrue(find(KEYS_CREATE_KEYS).isDisabled());
		logger.info("Nickname textfield is visible");

		assertFalse(find(KEYS_GENERATE_KEYS).isVisible());

		assertFalse(find(KEYS_RECOVER_KEYS).isVisible());
		assertFalse(find(KEYS_RECOVERY_PHRASE_BUTTON).isVisible());
		logger.info("All buttons are invisible");

		keysPanePage.enterNickName("test-key-0");
		assertTrue(find(KEYS_CREATE_KEYS).isVisible());
		assertFalse(find(KEYS_CREATE_KEYS).isDisabled());
		logger.info("Generate keys button is enabled");

		keysPanePage.cancelCreateKeys();
		assertTrue(find(KEYS_GENERATE_KEYS).isVisible());
		assertTrue(find(KEYS_GENERATE_KEYS).isVisible());

		assertTrue(find(KEYS_RECOVER_KEYS).isVisible());
		assertTrue(find(KEYS_RECOVERY_PHRASE_BUTTON).isVisible());
		logger.info("All buttons are visible after cancel");

		keysPanePage.pressGenerateKeyButton()
				.enterNickName("test-key-1")
				.pressCreateKeysButton()
				.enterPopupPassword("123456");

		var wrongPasswordNodes =
				((VBox) Objects.requireNonNull(getPopupNodes()).get(0)).getChildren();
		assertEquals(2, Objects.requireNonNull(wrongPasswordNodes).size());
		assertTrue(
				((Label) wrongPasswordNodes.get(0)).getText().contains(
						"The password you entered does not match our records"));

		logger.info("Wrong password is rejected");

		var ok = (Button) ((HBox) wrongPasswordNodes.get(1)).getChildren().get(1);

		keysPanePage.clickOn(ok)
				.enterPopupPassword(PASSWORD);

		var rightPasswordNodes = getPopupNodes();
		assertEquals(4, Objects.requireNonNull(rightPasswordNodes).size());
		assertTrue(((Label) rightPasswordNodes.get(0)).getText().contains("Keys Generated"));
		assertTrue(((Label) rightPasswordNodes.get(1)).getText().contains(
				"A private and public key pair has been created, with index"));
		assertTrue(((Label) rightPasswordNodes.get(1)).getText().contains("2")); //HERE BAD
		logger.info("Popup indicates key 0 has been generated");

		keysPanePage.closePopup("CONTINUE");

		assertFalse(new File(DEFAULT_STORAGE + "/Keys/test-key-0.pem").exists());
		assertTrue(new File(DEFAULT_STORAGE + "/Keys/test-key-1.pem").exists());
		assertTrue(new File(DEFAULT_STORAGE + "/Keys/test-key-1.pub").exists());
		logger.info("Keys files have been created");

		assertEquals(2, Ed25519KeyStore.getIndex(DEFAULT_STORAGE + "/Keys/test-key-1.pem"));
		logger.info("Index of the created key is 2");

		keysPanePage.pressGenerateKeyButton()
				.enterNickName("test-key-1");

		assertTrue(find(ACCOUNTS_NICKNAME_ERROR_LABEL).isVisible());
		logger.info("Cannot enter the same nickname twice");

		keysPanePage.enterNickName("test-key-2")
				.pressCreateKeysButton()
				.enterPopupPassword(PASSWORD)
				.closePopup("CONTINUE");

		assertTrue(new File(DEFAULT_STORAGE + "/Keys/test-key-2.pem").exists());
		assertTrue(new File(DEFAULT_STORAGE + "/Keys/test-key-2.pub").exists());
		logger.info("Second set of Keys files have been created");

		assertEquals(3, Ed25519KeyStore.getIndex(DEFAULT_STORAGE + "/Keys/test-key-2.pem"));
		logger.info("Index of the created key is 3");

		assertTrue(find(KEYS_GENERATE_KEYS).isVisible());
		assertTrue(find(KEYS_GENERATE_KEYS).isVisible());

		assertTrue(find(KEYS_RECOVER_KEYS).isVisible());
		assertTrue(find(KEYS_RECOVERY_PHRASE_BUTTON).isVisible());
		assertFalse(find(CREATE_KEYS_VBOX).isVisible());
		logger.info("All buttons are visible after process");


	}

	@Test
	public void recoverKey_Test() throws KeyStoreException {
		logger.info("recoverKey_Test");
		var oldKeyStore = Ed25519KeyStore.read(PASSWORD.toCharArray(),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));

		var oldKey = oldKeyStore.get(0);

		keysPanePage.pressRecoverKeysButton();

		assertFalse(find(KEYS_GENERATE_KEYS).isVisible());
		assertFalse(find(KEYS_RECOVER_KEYS).isVisible());
		assertFalse(find(KEYS_RECOVERY_PHRASE_BUTTON).isVisible());
		logger.info("No buttons are visible");

		keysPanePage.enterIndex(1);
		assertEquals("principalTestingKey", ((TextField) find(ACCOUNTS_RECOVER_KEY_NICKNAME)).getText());

		keysPanePage.pressCancelRecoverKeysButton();
		assertTrue(find(KEYS_GENERATE_KEYS).isVisible());
		assertTrue(find(KEYS_RECOVER_KEYS).isVisible());
		assertTrue(find(KEYS_RECOVERY_PHRASE_BUTTON).isVisible());
		logger.info("Buttons are visible");

		keysPanePage.pressRecoverKeysButton();
		assertTrue(((TextField) find(ACCOUNTS_RECOVER_KEY_NICKNAME)).getText().isEmpty());
		assertTrue(((TextField) find(ACCOUNTS_RECOVER_KEY_INDEX)).getText().isEmpty());

		keysPanePage.enterRecoverNickname("principalTesting");
		assertTrue(((TextField) find(ACCOUNTS_RECOVER_KEY_INDEX)).getText().isEmpty());

		keysPanePage.pressRecoverKeys();
		var missingIndex = ((VBox) Objects.requireNonNull(getPopupNodes()).get(0)).getChildren();
		assertEquals(2, Objects.requireNonNull(missingIndex).size());
		assertTrue(
				((Label) missingIndex.get(0)).getText().contains("Cannot recover a key without an index."));

		keysPanePage.closeOKPopup();

		keysPanePage.enterRecoverNickname("principalTestingKey");

		keysPanePage.pressRecoverKeys();
		var existingKeyPair = ((VBox) getPopupNodes().get(0)).getChildren();

		keysPanePage.closePopup("CONTINUE");
		assertTrue(((Label) existingKeyPair.get(0)).getText().contains("This operation is irreversible"));

		keysPanePage.enterPopupPassword(PASSWORD);

		var keysRecovered = getPopupNodes();
		assertEquals(4, Objects.requireNonNull(keysRecovered).size());
		assertTrue(
				((Label) keysRecovered.get(1)).getText().contains(
						"The private and public key pair has been recovered. It can be found at..."));

		keysPanePage.closePopup("CONTINUE");

		var newKeyStore = Ed25519KeyStore.read(PASSWORD.toCharArray(),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));

		var newKey = newKeyStore.get(0);

		assertArrayEquals(oldKey.getPublic().getEncoded(), newKey.getPublic().getEncoded());


	}

	@Test
	public void showRecoveryPhrase_Test() {
		logger.info("showRecoveryPhrase_Test");
		keysPanePage.pressRecoveryPhrase().enterPopupPassword(PASSWORD);
		assertTrue(find("#recoveryVBox").isVisible());
		VBox gridPaneVBox1 = find("#recoveryVBox");
		assertNotNull(gridPaneVBox1);
		var text = (Label) ((HBox) gridPaneVBox1.getChildren().get(1)).getChildren().get(0);
		var mnemonicWords = text.getText().replace("\n", " ").split("\\ +");

		assertEquals(24, mnemonicWords.length);

		logger.info("Test view is reset after closing it");
		keysPanePage.pressCloseViewMnemonic()
				.pressRecoveryPhrase();
		assertFalse(find("#recoveryVBox").isVisible());

		keysPanePage.pressCancelPassword();
		assertFalse(find("#recoveryVBox").isVisible());
	}

	@Test
	public void generateMissingPublicKey_Test() throws IOException {
		logger.info("generateMissingPublicKey_Test");
		keysPanePage.createKey("test", PASSWORD);
		mainWindowPage.clickOnHomeButton();
		FileUtils.moveFile(new File(DEFAULT_STORAGE + "/Keys/test.pub"),
				new File(DEFAULT_STORAGE + "/Keys/test_copy.pub"));
		mainWindowPage.clickOnKeysButton();

		keysPanePage.closePopup("CONTINUE");
		keysPanePage.enterPopupPassword(PASSWORD);
		assertTrue(
				FileUtils.contentEquals(new File(DEFAULT_STORAGE + "/Keys/test.pub"),
						new File(DEFAULT_STORAGE + "/Keys/test_copy.pub")));
	}

	@Test
	public void generateMultipleMissingPublicKeys_Test() throws IOException {
		logger.info("generateMultipleMissingPublicKeys_Test");
		keysPanePage.createKey("test1", PASSWORD);
		keysPanePage.createKey("test2", PASSWORD);
		keysPanePage.createKey("test3", PASSWORD);
		keysPanePage.createKey("test4", PASSWORD);
		keysPanePage.createKey("test5", PASSWORD);

		mainWindowPage.clickOnHomeButton();

		FileUtils.moveFile(new File(DEFAULT_STORAGE + "/Keys/test1.pub"),
				new File(DEFAULT_STORAGE + "/Keys/test1_copy.pub"));
		FileUtils.moveFile(new File(DEFAULT_STORAGE + "/Keys/test3.pub"),
				new File(DEFAULT_STORAGE + "/Keys/test3_copy.pub"));
		FileUtils.moveFile(new File(DEFAULT_STORAGE + "/Keys/test5.pub"),
				new File(DEFAULT_STORAGE + "/Keys/test5_copy.pub"));

		mainWindowPage.clickOnKeysButton();
		keysPanePage.closePopup("CONTINUE");

		keysPanePage.enterPopupPassword(PASSWORD);
		keysPanePage.enterPopupPassword(PASSWORD);
		keysPanePage.enterPopupPassword(PASSWORD);

		assertTrue(
				FileUtils.contentEquals(new File(DEFAULT_STORAGE + "/Keys/test1.pub"),
						new File(DEFAULT_STORAGE + "/Keys/test1_copy.pub")));
		assertTrue(
				FileUtils.contentEquals(new File(DEFAULT_STORAGE + "/Keys/test3.pub"),
						new File(DEFAULT_STORAGE + "/Keys/test3_copy.pub")));
		assertTrue(
				FileUtils.contentEquals(new File(DEFAULT_STORAGE + "/Keys/test5.pub"),
						new File(DEFAULT_STORAGE + "/Keys/test5_copy.pub")));

	}

	@Test
	public void showKeyDetails_test() throws IOException {
		logger.info("showKeyDetails_test");
		keysPanePage.createKey("test1", PASSWORD);
		keysPanePage.createKey("test2", PASSWORD);
		keysPanePage.createKey("test3", PASSWORD);
		keysPanePage.createKey("test4", PASSWORD);
		keysPanePage.createKey("test5", PASSWORD);

		mainWindowPage.clickOnHomeButton();

		FileUtils.moveFile(new File(DEFAULT_STORAGE + "/Keys/test1.pem"),
				new File(DEFAULT_STORAGE + "/test1_copy.pem"));
		FileUtils.moveFile(new File(DEFAULT_STORAGE + "/Keys/test3.pem"),
				new File(DEFAULT_STORAGE + "/test3_copy.pem"));
		FileUtils.moveFile(new File(DEFAULT_STORAGE + "/Keys/test5.pem"),
				new File(DEFAULT_STORAGE + "/test5_copy.pem"));

		mainWindowPage.clickOnKeysButton();
		VBox tableBox = find(SIGNING_KEYS_VBOX);
		assertNotNull(tableBox);

		var nodes = tableBox.getChildren();
		assertEquals(2, nodes.size());


		assertTrue(nodes.get(1) instanceof TableView);
		var keysTable = (TableView) nodes.get(1);

		var tableRows = getPublicKeysTableRows(keysTable);


		for (var tableRow : tableRows) {
			if (tableRow.isSigner()) {
				if ("\u2713".equals(tableRow.getMnemonic())) { // Checkmark
					assertTrue(Integer.parseInt(tableRow.getIndex()) >= 0);
				} else {
					assertEquals("none", tableRow.getIndex());
				}
			} else {
				assertEquals("public key", tableRow.getIndex());
				assertEquals("", tableRow.getMnemonic());
			}
		}

		doubleClickOn("test1");

		changeNickname("otherName");

		assertFalse(doesNameExist("test1"));
		assertTrue(doesNameExist("otherName"));

		var keysAfter = new File(DEFAULT_STORAGE + KEYS_STRING).listFiles((dir, name) -> name.contains("test1"));
		assert keysAfter != null;
		assertEquals(0, keysAfter.length);
		keysAfter = new File(DEFAULT_STORAGE + KEYS_STRING).listFiles((dir, name) -> name.contains("otherName"));
		assert keysAfter != null;
		assertEquals(1, keysAfter.length);

		doubleClickOn("test2");
		changeNickname("otherPemName");

		assertFalse(doesNameExist("test2"));
		assertTrue(doesNameExist("otherPemName"));

		keysAfter = new File(DEFAULT_STORAGE + KEYS_STRING).listFiles((dir, name) -> name.contains("test2"));
		assert keysAfter != null;
		assertEquals(0, keysAfter.length);
		keysAfter = new File(DEFAULT_STORAGE + KEYS_STRING).listFiles((dir, name) -> name.contains("otherPemName"));
		assert keysAfter != null;
		assertEquals(2, keysAfter.length);

		doubleClickOn("otherPemName");

		var popupNodes = getPopupNodes();
		assert popupNodes != null;
		assertTrue(popupNodes.get(popupNodes.size() - 1) instanceof Button);
		var continueButton = (Button) popupNodes.get(popupNodes.size() - 1);

		// Show private key
		var privateKeyVBoxNodes = ((VBox) popupNodes.get(3)).getChildren();
		var privateKeyNodes = ((HBox) privateKeyVBoxNodes.get(2)).getChildren();
		assertTrue(privateKeyNodes.get(0) instanceof TextArea);
		assertTrue(privateKeyNodes.get(1) instanceof VBox);


		var pattern = Pattern.compile("[^a-z0-9 ]", Pattern.CASE_INSENSITIVE);

		var vBox = (VBox) privateKeyNodes.get(1);
		assertEquals(3, vBox.getChildren().size());
		var show = vBox.getChildren().get(0);
		var hide = vBox.getChildren().get(1);
		assertTrue(show instanceof Button);
		assertEquals("SHOW", ((Button) show).getText());
		assertTrue(show.isVisible());

		assertTrue(hide instanceof Button);
		assertEquals("HIDE", ((Button) hide).getText());
		assertFalse(hide.isVisible());

		var hiddenArea = (TextArea) privateKeyNodes.get(0);
		assertTrue(hiddenArea.isDisabled());
		assertFalse(hiddenArea.isEditable());

		assertTrue(pattern.matcher(hiddenArea.getText()).find());

		clickOn(show);
		keysPanePage.enterPopupPassword(PASSWORD);

		assertTrue(hide.isVisible());
		assertFalse(show.isVisible());

		var visibleArea = (TextArea) privateKeyNodes.get(0);
		assertFalse(visibleArea.isDisabled());
		assertFalse(visibleArea.isEditable());

		assertFalse(pattern.matcher(visibleArea.getText()).find());

		clickOn(hide);
		var reHiddenArea = (TextArea) privateKeyNodes.get(0);
		assertTrue(reHiddenArea.isDisabled());
		assertFalse(reHiddenArea.isEditable());

		assertTrue(pattern.matcher(reHiddenArea.getText()).find());

		clickOn(continueButton);

		logger.info("Test done");

	}

	@Test
	public void changeKeyPassword_test() {
		logger.info("changeKeyPassword_test");
		keysPanePage.createKey("test1", PASSWORD);
		VBox tableBox = find(SIGNING_KEYS_VBOX);
		assertNotNull(tableBox);
		doubleClickOn("test1");

		var popupNodes = getPopupNodes();

		// Show private key
		var privateKeyVBoxNodes = ((VBox) popupNodes.get(3)).getChildren();
		var privateKeyNodes = ((HBox) privateKeyVBoxNodes.get(2)).getChildren();
		assertTrue(privateKeyNodes.get(0) instanceof TextArea);
		assertTrue(privateKeyNodes.get(1) instanceof VBox);

		var buttons = ((VBox) privateKeyNodes.get(1)).getChildren();
		assertTrue(buttons.get(2) instanceof Button);
		clickOn(buttons.get(2));

		clickOn(findButtonInPopup(getPopupNodes(), "CONTINUE"));

		keysPanePage.enterPopupPassword(PASSWORD);

		var passwordFields = keysPanePage.getPopupPasswordFields();
		var errorFields = keysPanePage.getPopupErrorFields();
		keysPanePage.typePassword("tempura", passwordFields.get(0));
		assertTrue(errorFields.get(0).isVisible());
		keysPanePage.typePassword("tempura sushi", passwordFields.get(0));
		assertFalse(errorFields.get(0).isVisible());
		assertFalse(passwordFields.get(1).isDisable());

		Assertions.assertFalse(passwordFields.get(1).isDisable());
		keysPanePage.typePassword("tempura", passwordFields.get(1));
		assertTrue(errorFields.get(1).isVisible());
		keysPanePage.typePassword("tempura sushi", passwordFields.get(1));
		assertFalse(errorFields.get(1).isVisible());

		keysPanePage.typePassword("tempura sashimi", passwordFields.get(1));
		assertTrue(errorFields.get(1).isVisible());

		keysPanePage.typePassword("tempura sashimi", passwordFields.get(0));
		assertFalse(errorFields.get(1).isVisible());

		var newPopupButtons1 = keysPanePage.getPopupButtons();
		clickOn(newPopupButtons1.get(1));

		var newPopupButtons = keysPanePage.getPopupButtons();
		clickOn(newPopupButtons.get(0));

		clickOn(findButtonInPopup(popupNodes, "CLOSE"));

		sleep(500);
		doubleClickOn("test1");
		var popupNodes1 = getPopupNodes();

		// Show private key
		var privateKeyVBoxNodes1 = ((VBox) popupNodes1.get(3)).getChildren();
		var privateKeyNodes1 = ((HBox) privateKeyVBoxNodes1.get(2)).getChildren();
		assertTrue(privateKeyNodes1.get(0) instanceof TextArea);
		assertTrue(privateKeyNodes1.get(1) instanceof VBox);
		var hiddenArea = (TextArea) privateKeyNodes1.get(0);

		var buttons1 = ((VBox) privateKeyNodes1.get(1)).getChildren();
		assertTrue(buttons1.get(0) instanceof Button);
		clickOn(buttons1.get(0));
		keysPanePage.enterPopupPassword("tempura sashimi");

		var privateKeyVBoxNodes2 = ((VBox) popupNodes1.get(3)).getChildren();
		var privateKeyNodes2 = ((HBox) privateKeyVBoxNodes2.get(2)).getChildren();
		assertTrue(privateKeyNodes2.get(0) instanceof TextArea);
		assertTrue(privateKeyNodes2.get(1) instanceof VBox);
		var hiddenArea2 = (TextArea) privateKeyNodes2.get(0);


		var pattern = Pattern.compile("^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d).+$", Pattern.CASE_INSENSITIVE);
		assertFalse(hiddenArea2.isEditable());
		assertTrue(pattern.matcher(hiddenArea2.getText()).matches());
		clickOn(findButtonInPopup(popupNodes1, "CLOSE"));
	}

	@Test
	public void linkPhraseToKey_Test() throws IOException, KeyStoreException {
		logger.info("linkPhraseToKey_Test");
		keysPanePage.createKey("test1", PASSWORD);
		removeLineFromPem(DEFAULT_STORAGE + "Keys/test1.pem");

		mainWindowPage.clickOnHomeButton().clickOnKeysButton();
		keysPanePage.pressContinue();
		keysPanePage.enterPopupPassword(PASSWORD);

		var signingKeysBox = find(PUBLIC_KEYS_VBOX);
		assertTrue(signingKeysBox instanceof VBox);
		var nodes = ((VBox) signingKeysBox).getChildren();
		assertEquals(2, nodes.size());
		assertTrue(nodes.get(1) instanceof TableView);

		TableView<KeysTableRow> table = (TableView) nodes.get(1);
		var tableRows = table.getItems();
		assertEquals(2, tableRows.size());

		assertEquals(Ed25519KeyStore.getMnemonicHashCode(DEFAULT_STORAGE + "Keys/principalTestingKey.pem"),
				Ed25519KeyStore.getMnemonicHashCode(DEFAULT_STORAGE + "Keys/test1.pem"));

		FileUtils.copyFile(
				new File("src/test/resources/Keys/signPaneKey.pem"),
				new File(DEFAULT_STORAGE + "Keys/signPaneKey.pem"));


		mainWindowPage.clickOnHomeButton().clickOnKeysButton();

		keysPanePage.pressContinue()
				.pressCancelPassword();

		keysPanePage.pressContinue();
		keysPanePage.enterPopupPassword(PASSWORD);

		mainWindowPage.clickOnHomeButton().clickOnKeysButton();

		logger.info("Done testing hash link to keys");
	}

	@After
	public void tearDown() {
		try {
			var currentRelativePath = Paths.get("");
			var s = currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/testDirectory";
			if ((new File(s)).exists()) {
				FileUtils.deleteDirectory(new File(s));
			}

			var out =
					currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/Transactions - " +
							"Documents/OutputFiles/test1.council2@hederacouncil.org";
			FileUtils.cleanDirectory(new File(out));

			properties.resetProperties();

			if (new File(DEFAULT_STORAGE).exists()) {
				FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
			}

		} catch (Exception e) {
			logger.error(e);
			assertNull(e);
		}
	}

	// region AUXILIARY METHODS

	private void removeLineFromPem(String pemFile) throws FileNotFoundException {
		List<String> lines = new ArrayList<>();
		BufferedReader reader;
		try {
			reader = new BufferedReader(new FileReader(pemFile));
			var line = reader.readLine();
			while (line != null) {
				if (!line.contains("Recovery Phrase Hash:")) {
					lines.add(line);
				}
				line = reader.readLine();
			}
			reader.close();
		} catch (IOException e) {
			logger.error(e);
			logger.error("Cannot read pemfile" + pemFile);
		}

		try (var printWriter = new PrintWriter(pemFile)) {
			for (var line : lines) {
				printWriter.println(line);
			}
		}

	}

	private void changeNickname(String newName) {
		var popupNodes = getPopupNodes();
		assert popupNodes != null;
		assertTrue(popupNodes.get(popupNodes.size() - 1) instanceof Button);
		var continueButton = (Button) popupNodes.get(popupNodes.size() - 1);

		// Change Nickname
		var nicknameNodes = ((VBox) popupNodes.get(0)).getChildren();
		assertTrue(nicknameNodes.get(0) instanceof Label);
		assertTrue(nicknameNodes.get(1) instanceof HBox);

		var nodes = ((HBox) nicknameNodes.get(1)).getChildren();

		assertTrue(nodes.get(0) instanceof TextField);
		assertTrue(nodes.get(1) instanceof Button);

		((TextField) nodes.get(0)).setText(newName);
		clickOn(nodes.get(1));
		continuePopup();
		clickOn(continueButton);
	}

	private void continuePopup() {
		var popupContinueNodes = getPopupNodes();
		assert popupContinueNodes != null;
		var vBox = (VBox) popupContinueNodes.get(0);
		for (var popupContinueNode : vBox.getChildren()) {
			if (popupContinueNode instanceof HBox) {
				clickOn(popupContinueNode);
			}
		}
	}

	private boolean doesNameExist(String testString) {
		VBox tableBox = find(SIGNING_KEYS_VBOX);
		assertNotNull(tableBox);
		var nodes = tableBox.getChildren();
		assertEquals(2, nodes.size());
		assertTrue(nodes.get(1) instanceof TableView);
		var keysTable = (TableView) nodes.get(1);
		var tableRows = getPublicKeysTableRows(keysTable);
		var stringExists = false;
		for (var tableRow : tableRows) {
			if (testString.equals(tableRow.getKeyName())) {
				stringExists = true;
			}
		}
		return stringExists;
	}

	private List<KeysTableRow> getPublicKeysTableRows(TableView keysTable) {
		List<KeysTableRow> tableRows = new ArrayList<>();
		var rowData = keysTable.getItems();
		for (var rowDatum : rowData) {
			assertTrue(rowDatum instanceof KeysTableRow);
			tableRows.add((KeysTableRow) rowDatum);
		}
		return tableRows;
	}

	// endregion

}
