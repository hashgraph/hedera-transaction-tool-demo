/*
 * Hedera Transaction Tool
 *
 * Copyright (C) 2018 - 2021 Hedera Hashgraph, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hedera.hashgraph.client.ui.popups;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.client.ui.StartUI;
import com.hedera.hashgraph.client.ui.TestBase;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.KeysPanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.utilities.AutoCompleteTextField;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import junit.framework.TestCase;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class NewPasswordPopupTest extends TestBase {
	private static final String OUTPUT_PATH =
			"/src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org/";
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private static final String PASSWORD = "123456789";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	private final Path currentRelativePath = Paths.get("");
	private final String storedMnemonic =
			"DIGNITY DOMAIN INVOLVE REPORT SAIL MIDDLE RHYTHM HUSBAND USAGE PRETTY RATE TOWN " +
					"ACCOUNT SIDE EXTRA OUTER EAGLE EIGHT DESIGN PAGE REGULAR BIRD RACE ANSWER";
	private final String otherMnemonic =
			"LUXURY PENALTY STAGE CANCEL ASK TOPIC OPEN SIEGE EMERGE FEED SUDDEN DISCOVER HELMET JEALOUS CULTURE EXIT " +
					"ROSE DIAGRAM TURKEY TRIAL DISTANCE DYNAMIC FINAL SHIVER";
	public UserAccessibleProperties properties;
	private KeysPanePage keysPanePage;

	@Before
	public void setUp() throws Exception {

		TestUtil.buildFolders();

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");

		if (new File(currentRelativePath.toAbsolutePath() + OUTPUT_PATH).mkdirs()) {
			logger.info("Output path created");
		}

		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		properties.setMnemonicHashCode(-915976044);

		final Map<String, String> emailMap = new HashMap<>();

		emailMap.put(
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");


		final ObjectMapper objectMapper = new ObjectMapper();
		final String mapAsString = objectMapper.writeValueAsString(emailMap);
		logger.info(mapAsString);

		properties.setOneDriveCredentials(emailMap);
		properties.setHash(PASSWORD.toCharArray());
		properties.setSalt(true);
		properties.setLegacy(true);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);

		final Controller controller = new Controller();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		//setupTransactionDirectory(DEFAULT_STORAGE);

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
		final MainWindowPage mainWindowPage = new MainWindowPage(this);
		mainWindowPage.clickOnKeysButton();
	}

	@After
	public void tearDown() throws Exception {
		try {
			final Path currentRelativePath = Paths.get("");
			final String s = currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory";
			if ((new File(s)).exists()) {
				FileUtils.deleteDirectory(new File(s));
			}

			final String out =
					currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
							"Documents/OutputFiles/test1.council2@hederacouncil.org";
			FileUtils.cleanDirectory(new File(out));

			properties.resetProperties();

			if (new File(DEFAULT_STORAGE).exists()) {
				FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
			}
			FxToolkit.hideStage();
			FxToolkit.cleanupStages();
		} catch (final Exception e) {
			logger.error(e);
			assertNull(e);
		}
	}

	@Test
	public void checkPopupNodes_test() {
		doubleClickOn("principalTestingKey");
		final var labels = keysPanePage.getPopupLabels();
		assertEquals(6, labels.size());
		assertEquals("Key Name", labels.get(0).getText());
		assertEquals("Public Key", labels.get(2).getText());
		assertEquals("Private Key", labels.get(3).getText());
		assertEquals("Index: 1", labels.get(4).getText());
		assertEquals("Associated accounts", labels.get(5).getText());
		assertTrue(labels.get(1).getText().contains("principalTestingKey.pub"));

		final var buttons = keysPanePage.getPopupButtons();
		assertEquals(5, buttons.size());
		assertEquals("CHANGE", buttons.get(0).getText());
		assertEquals("SHOW", buttons.get(1).getText());
		assertEquals("HIDE", buttons.get(2).getText());
		assertFalse(buttons.get(2).isVisible());
		assertEquals("CHANGE PASSWORD", buttons.get(3).getText());
		assertEquals("CLOSE", buttons.get(4).getText());
		clickOn(buttons.get(4));
	}

	@Test
	public void changePasswordCancel_test() {
		doubleClickOn("principalTestingKey");
		final var buttons = keysPanePage.getPopupButtons();
		clickOn(buttons.get(3));

		final var buttons0 = keysPanePage.getPopupButtons();
		assertEquals(2, buttons0.size());

		clickOn(buttons0.get(0));
		final var enterPassword = keysPanePage.getPopupLabels();
		assertEquals("Please enter the password for key principalTestingKey", enterPassword.get(0).getText());

		keysPanePage.enterPopupPassword(PASSWORD);

		final var passwordFields = keysPanePage.getPopupPasswordFields();
		assertEquals(2, passwordFields.size());
		keysPanePage.typePassword("tempura", passwordFields.get(0));
		assertTrue(passwordFields.get(1).isDisable());

		passwordFields.get(0).clear();
		keysPanePage.typePassword("tempura sushi", passwordFields.get(0));
		assertFalse(passwordFields.get(1).isDisable());

		keysPanePage.typePassword("sushi tempura", passwordFields.get(1));

		final var newPopupButtons = keysPanePage.getPopupButtons();
		assertFalse(newPopupButtons.get(1).isVisible());

		keysPanePage.typePassword("tempura sushi", passwordFields.get(1));
		assertTrue(newPopupButtons.get(1).isVisible());

		clickOn(newPopupButtons.get(0));

		final var labels = keysPanePage.getPopupLabels();
		assertEquals("The password for principalTestingKey has not been changed", labels.get(0).getText());
		keysPanePage.closePopup("CONTINUE");
		clickOn(buttons.get(1));
		keysPanePage.enterPopupPassword(PASSWORD);
		assertTrue(buttons.get(2).isVisible());
		clickOn(buttons.get(4));

	}

	@Test
	public void changePasswordAccept_test() {
		doubleClickOn("principalTestingKey");
		final var buttons = keysPanePage.getPopupButtons();
		clickOn(buttons.get(3));
		final var buttons0 = keysPanePage.getPopupButtons();
		clickOn(buttons0.get(0));
		keysPanePage.enterPopupPassword(PASSWORD);
		final var passwordFields = keysPanePage.getPopupPasswordFields();
		keysPanePage.typePassword("tempura sushi", passwordFields.get(0));
		assertFalse(passwordFields.get(1).isDisable());
		final var newPopupButtons = keysPanePage.getPopupButtons();
		keysPanePage.typePassword("tempura sushi", passwordFields.get(1));
		clickOn(newPopupButtons.get(1));
		final var labels = keysPanePage.getPopupLabels();
		assertEquals("The password for principalTestingKey has been changed", labels.get(0).getText());
		keysPanePage.closePopup("CONTINUE");
		clickOn(buttons.get(1));
		keysPanePage.enterPopupPassword("tempura sushi");
		assertTrue(buttons.get(2).isVisible());

		final File pemFile = new File(KEYS_FOLDER, "principalTestingKey.pem");
		assertTrue(pemFile.exists());
		Ed25519KeyStore keyStore = null;
		final Exception e = assertThrows(KeyStoreException.class, () -> Ed25519KeyStore.read(PASSWORD.toCharArray(),
				pemFile));
		assertTrue(e.getMessage().contains("PKCSException"));
		try {
			keyStore = Ed25519KeyStore.read("tempura sushi".toCharArray(), pemFile);
		} catch (final KeyStoreException exception) {
			exception.printStackTrace();
		}
		assertNotNull(keyStore);
		clickOn(buttons.get(4));
	}

	@Test
	public void forgotMnemonicPasswordCancel_test() {
		logger.info("forgotMnemonicPasswordCancel_test");
		keysPanePage.pressRecoveryPhrase()
				.enterPopupPassword(PASSWORD);
		final VBox gridPaneVBox = find("#recoveryVBox");
		Assert.assertNotNull(gridPaneVBox);
		Assert.assertTrue(gridPaneVBox.isVisible());
		final var boxChildren = ((HBox) gridPaneVBox.getChildren().get(1)).getChildren();
		Assert.assertTrue(boxChildren.get(0) instanceof Label);
		final var oldMnemonic = ((Label) boxChildren.get(0)).getText().toUpperCase(Locale.ROOT);

		keysPanePage.pressCloseViewMnemonic()
				.pressRecoveryPhrase()
				.pressHyperlinkPassword("Forgot your password?")
				.pressPopupButton("RESET")
				.pressPopupButton("RESET")
				.enteMnemonicInPopup(storedMnemonic)
				.pressPopupButton("RECOVER")
				.pressPopupButton("CANCEL");

		final var wordsPane = TestUtil.findGridpanesInPopup();
		Assert.assertEquals(1, wordsPane.size());

		final var children = wordsPane.get(0).getChildren();
		for (final Node child : children) {
			Assert.assertTrue(child instanceof AutoCompleteTextField);
			Assert.assertTrue(storedMnemonic.toLowerCase(Locale.ROOT).contains(
					((AutoCompleteTextField) child).getText().toLowerCase(Locale.ROOT)));
		}

		keysPanePage.pressPopupButton("CANCEL");
		keysPanePage.pressRecoveryPhrase()
				.enterPopupPassword(PASSWORD);

		final var other = otherMnemonic.split(" ");
		Arrays.stream(other).map(oldMnemonic::contains).forEach(TestCase::assertTrue);
	}

}