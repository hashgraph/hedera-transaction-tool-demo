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
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.util.HashMap;
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
	private KeysPanePage keysPanePage;

	private final Path currentRelativePath = Paths.get("");
	public UserAccessibleProperties properties;
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private static final String PASSWORD = "123456789";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

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


		ObjectMapper objectMapper = new ObjectMapper();
		String mapAsString = objectMapper.writeValueAsString(emailMap);
		logger.info(mapAsString);

		properties.setOneDriveCredentials(emailMap);
		properties.setHash(PASSWORD.toCharArray());
		properties.setSalt(true);
		properties.setLegacy(true);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);

		Controller controller = new Controller();
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
		MainWindowPage mainWindowPage = new MainWindowPage(this);
		mainWindowPage.clickOnKeysButton();
	}

	@After
	public void tearDown() throws Exception {
		try {
			Path currentRelativePath = Paths.get("");
			String s = currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory";
			if ((new File(s)).exists()) {
				FileUtils.deleteDirectory(new File(s));
			}

			String out =
					currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
							"Documents/OutputFiles/test1.council2@hederacouncil.org";
			FileUtils.cleanDirectory(new File(out));

			properties.resetProperties();

			if (new File(DEFAULT_STORAGE).exists()) {
				FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
			}
			FxToolkit.hideStage();
			FxToolkit.cleanupStages();
		} catch (Exception e) {
			logger.error(e);
			assertNull(e);
		}
	}

	@Test
	public void checkPopupNodes_test() {
		doubleClickOn("principalTestingKey");
		var labels = keysPanePage.getPopupLabels();
		assertEquals(5, labels.size());
		assertEquals("Key Name", labels.get(0).getText());
		assertEquals("Public Key", labels.get(2).getText());
		assertEquals("Private Key", labels.get(3).getText());
		assertEquals("Index: 1", labels.get(4).getText());
		assertTrue(labels.get(1).getText().contains("principalTestingKey.pub"));

		var buttons = keysPanePage.getPopupButtons();
		assertEquals(5, buttons.size());
		assertEquals("CHANGE", buttons.get(0).getText());
		assertEquals("SHOW", buttons.get(1).getText());
		assertEquals("HIDE", buttons.get(2).getText());
		assertFalse(buttons.get(2).isVisible());
		assertEquals("CHANGE PASSWORD", buttons.get(3).getText());
		assertEquals("CLOSE", buttons.get(4).getText());
		clickOn(buttons.get(4).getText());
	}

	@Test
	public void changePasswordCancel_test() {
		doubleClickOn("principalTestingKey");
		var buttons = keysPanePage.getPopupButtons();
		clickOn(buttons.get(3).getText());

		var buttons0 = keysPanePage.getPopupButtons();
		assertEquals(2, buttons0.size());

		clickOn(buttons0.get(0).getText());
		var enterPassword = keysPanePage.getPopupLabels();
		assertEquals("Please enter the password for key principalTestingKey", enterPassword.get(0).getText());

		keysPanePage.enterPopupPassword(PASSWORD);

		var passwordFields = keysPanePage.getPopupPasswordFields();
		assertEquals(2, passwordFields.size());
		keysPanePage.typePassword("tempura", passwordFields.get(0));
		assertTrue(passwordFields.get(1).isDisable());

		passwordFields.get(0).clear();
		keysPanePage.typePassword("tempura sushi", passwordFields.get(0));
		assertFalse(passwordFields.get(1).isDisable());

		keysPanePage.typePassword("sushi tempura", passwordFields.get(1));

		var newPopupButtons = keysPanePage.getPopupButtons();
		assertFalse(newPopupButtons.get(1).isVisible());

		keysPanePage.typePassword("tempura sushi", passwordFields.get(1));
		assertTrue(newPopupButtons.get(1).isVisible());

		clickOn(newPopupButtons.get(0));

		var labels = keysPanePage.getPopupLabels();
		assertEquals("The password for principalTestingKey has not been changed", labels.get(0).getText());
		keysPanePage.closePopup("CONTINUE");
		clickOn(buttons.get(1));
		keysPanePage.enterPopupPassword(PASSWORD);
		assertTrue(buttons.get(2).isVisible());
		clickOn(buttons.get(4).getText());
	}

	@Test
	public void changePasswordAccept_test() {
		doubleClickOn("principalTestingKey");
		var buttons = keysPanePage.getPopupButtons();
		clickOn(buttons.get(3));
		var buttons0 = keysPanePage.getPopupButtons();
		clickOn(buttons0.get(0));
		keysPanePage.enterPopupPassword(PASSWORD);
		var passwordFields = keysPanePage.getPopupPasswordFields();
		keysPanePage.typePassword("tempura sushi", passwordFields.get(0));
		assertFalse(passwordFields.get(1).isDisable());
		var newPopupButtons = keysPanePage.getPopupButtons();
		keysPanePage.typePassword("tempura sushi", passwordFields.get(1));
		clickOn(newPopupButtons.get(1));
		var labels = keysPanePage.getPopupLabels();
		assertEquals("The password for principalTestingKey has been changed", labels.get(0).getText());
		keysPanePage.closePopup("CONTINUE");
		clickOn(buttons.get(1));
		keysPanePage.enterPopupPassword("tempura sushi");
		assertTrue(buttons.get(2).isVisible());

		File pemFile = new File(KEYS_FOLDER, "principalTestingKey.pem");
		assertTrue(pemFile.exists());
		Ed25519KeyStore keyStore = null;
		Exception e = assertThrows(KeyStoreException.class, () -> Ed25519KeyStore.read(PASSWORD.toCharArray(),
				pemFile));
		assertEquals("org.bouncycastle.pkcs.PKCSException: unable to read encrypted data: Error finalising cipher",
				e.getMessage());
		try {
			keyStore = Ed25519KeyStore.read("tempura sushi".toCharArray(), pemFile);
		} catch (KeyStoreException exception) {
			exception.printStackTrace();
		}
		assertNotNull(keyStore);
		clickOn(buttons.get(4).getText());
	}

}