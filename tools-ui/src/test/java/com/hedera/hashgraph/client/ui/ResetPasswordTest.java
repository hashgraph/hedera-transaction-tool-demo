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

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.ui.pages.KeysPanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_KEYS;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.keyFromPassword;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.toEncryptedFile;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ResetPasswordTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(ResetPasswordTest.class);

	private static final String OUTPUT_PATH =
			"/src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org/";
	private static final Path CURRENT_RELATIVE_PATH = Paths.get("");
	private static final String PASSWORD = "tempurasushi";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	private static final List<String> TEST_WORDS =
			Arrays.asList("dignity", "domain", "involve", "report",
					"sail", "middle", "rhythm", "husband",
					"usage", "pretty", "rate", "town",
					"account", "side", "extra", "outer",
					"eagle", "eight", "design", "page",
					"regular", "bird", "race", "answer");
	private static final List<String> TEST_WORDS_2 =
			Arrays.asList("hurry", "rib", "magnet", "advance",
					"mirror", "gift", "token", "border",
					"slogan", "universe", "local", "appear",
					"add", "art", "away", "flush",
					"myth", "normal", "profit", "trim",
					"fault", "decide", "kiss", "afford");

	private KeysPanePage keysPanePage;


	@BeforeEach
	public void setUp() throws Exception {
		System.gc();
		logger.info("Starting test class: {}", getClass().getSimpleName());
		TestUtil.buildFolders();

		final var properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");

		if (new File(CURRENT_RELATIVE_PATH.toAbsolutePath() + OUTPUT_PATH).mkdirs()) {
			logger.info("Output path created");
		}

		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		properties.setMnemonicHashCode(-915976044);

		final Map<String, String> emailMap = new HashMap<>();

		emailMap.put(
				CURRENT_RELATIVE_PATH.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties.setOneDriveCredentials(emailMap);
		properties.setHash(PASSWORD.toCharArray());
		properties.setSalt(true);
		properties.setLegacy(true);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);

		final var controller = new Controller();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		//setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE, MNEMONIC_PATH));

		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		final var mnemonic = Mnemonic.fromWords(TEST_WORDS);
		properties.setMnemonicHashCode(mnemonic.words.hashCode());
		properties.setHash(TEST_PASSWORD.toCharArray());
		properties.setLegacy(false);
		final var salt = Utilities.getSaltBytes(properties);
		final var passwordBytes = keyFromPassword(TEST_PASSWORD.toCharArray(), salt);
		toEncryptedFile(passwordBytes, Constants.DEFAULT_STORAGE + File.separator + Constants.MNEMONIC_PATH,
				mnemonic.toString());

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);
		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
		}

		keysPanePage = new KeysPanePage(this);
		final var mainWindowPage = new MainWindowPage(this);
		mainWindowPage.clickOnKeysButton();
	}

	@AfterEach
	public void tearDown() throws Exception {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

		final var storage = new File(Constants.DEFAULT_STORAGE);
		if (storage.exists()) {
			FileUtils.deleteDirectory(storage);
		}
	}

	@Test
	@Disabled("Popup isn't being setup properly for this test.")
	public void forgottenMnemonicPassword_test() {
		keysPanePage.pressRecoveryPhrase()
				.pressPopupHyperlink()
				.clickOnPopupButton("RESET")
				.clickOnPopupButton("RESET")
				.setWords(TEST_WORDS)
				.clickOnPopupButton("RECOVER")
				.clickOnPopupButton("CONTINUE")
				.enterPasswordAndConfirm(PASSWORD)
				.clickOnPopupButton("CONTINUE");
		assertTrue(find("#recoveryVBox").isVisible());
		keysPanePage.pressCloseViewMnemonic();

		keysPanePage.pressRecoveryPhrase().enterPopupPassword(PASSWORD);
		final var recoveryBox = find("#recoveryVBox");
		assertTrue(recoveryBox instanceof VBox);
		assertTrue(recoveryBox.isVisible());

		final var children = ((VBox) recoveryBox).getChildren();
		assertEquals(2, children.size());
		assertTrue(children.get(1) instanceof HBox);
		final var labels = ((HBox) children.get(1)).getChildren();
		assertEquals(1, labels.size());
		assertTrue(labels.get(0) instanceof Label);

		final var words = ((Label) labels.get(0)).getText().toLowerCase(Locale.ROOT);
		for (final var testWord : TEST_WORDS) {
			assertTrue(words.contains(testWord));
		}
	}

	@Test
	@Disabled("Nickname field isn't being setup properly for this test.")
	public void forgottenPasswordKey_test() throws KeyStoreException {
		keysPanePage.createKey("testKey", TEST_PASSWORD);
		doubleClickOn("testKey");

		final var pem = new File(DEFAULT_KEYS, "testKey.pem");
		final var keyStoreBefore = Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), pem.getAbsolutePath());

		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var privateKeyVBoxNodes = ((VBox) popupNodes.get(3)).getChildren();
		final var privateKeyNodes = ((HBox) privateKeyVBoxNodes.get(2)).getChildren();
		assertTrue(privateKeyNodes.get(0) instanceof TextArea);
		assertTrue(privateKeyNodes.get(1) instanceof VBox);

		final var vBox = (VBox) privateKeyNodes.get(1);
		assertEquals(3, vBox.getChildren().size());
		final var show = vBox.getChildren().get(0);
		assertTrue(show instanceof Button);
		assertEquals("SHOW", ((Button) show).getText());
		clickOn(show);

		keysPanePage.pressPopupHyperlink()
				.clickOnPopupButton("RESET")
				.clickOnPopupButton("RESET")
				.enterPopupPassword(TEST_PASSWORD)
				.enterPasswordAndConfirm("penthouseart")
				.clickOnPopupButton("CONTINUE");

		final var keyStoreAfter = Ed25519KeyStore.read("penthouseart".toCharArray(), pem.getAbsolutePath());
		assertArrayEquals(keyStoreBefore.get(0).getPrivate().getEncoded(),
				keyStoreAfter.get(0).getPrivate().getEncoded());

	}

	@Test
	@Disabled("Popup isn't being setup properly for this test.")
	public void forgottenPasswordKeyDifferentMnemonic_test() throws KeyStoreException {
		doubleClickOn("principalTestingKey");

		final var pem = new File(DEFAULT_KEYS, "principalTestingKey.pem");
		final var keyStoreBefore = Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), pem.getAbsolutePath());

		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var privateKeyVBoxNodes = ((VBox) popupNodes.get(3)).getChildren();
		final var privateKeyNodes = ((HBox) privateKeyVBoxNodes.get(2)).getChildren();
		assertTrue(privateKeyNodes.get(0) instanceof TextArea);
		assertTrue(privateKeyNodes.get(1) instanceof VBox);

		final var vBox = (VBox) privateKeyNodes.get(1);
		assertEquals(3, vBox.getChildren().size());
		final var show = vBox.getChildren().get(0);
		assertTrue(show instanceof Button);
		assertEquals("SHOW", ((Button) show).getText());
		clickOn(show);

		keysPanePage.pressPopupHyperlink()
				.clickOnPopupButton("RESET")
				.clickOnPopupButton("RESET");
		keysPanePage.clickOnPopupButton("CONTINUE");

		final var keyStoreAfter = Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), pem.getAbsolutePath());
		assertArrayEquals(keyStoreBefore.get(0).getPrivate().getEncoded(),
				keyStoreAfter.get(0).getPrivate().getEncoded());
		keysPanePage.clickOnPopupButton("CLOSE");
	}

	@Test
	@Disabled("Popup isn't being setup properly for this test.")
	public void mnemonicsDontMatchReplace_test() {
		keysPanePage.pressRecoveryPhrase().enterPopupPassword(TEST_PASSWORD);
		final var recoveryBox = find("#recoveryVBox");
		assertTrue(recoveryBox instanceof VBox);
		assertTrue(recoveryBox.isVisible());
		var children = ((VBox) recoveryBox).getChildren();
		var labels = ((HBox) children.get(1)).getChildren();
		assertEquals(1, labels.size());
		assertTrue(labels.get(0) instanceof Label);


		var words = ((Label) labels.get(0)).getText().toLowerCase(Locale.ROOT);
		for (final var testWord : TEST_WORDS) {
			assertTrue(words.contains(testWord));
		}
		keysPanePage.pressCloseViewMnemonic();
		keysPanePage.pressRecoveryPhrase()
				.pressPopupHyperlink()
				.clickOnPopupButton("RESET")
				.clickOnPopupButton("RESET")
				.setWords(TEST_WORDS_2)
				.clickOnPopupButton("RECOVER")
				.clickOnPopupButton("CONTINUE")
				.clickOnPopupButton("CANCEL")
				.clickOnPopupButton("CANCEL");

		final var recoveryBox2 = find("#recoveryVBox");
		assertTrue(recoveryBox2 instanceof VBox);
		assertFalse(recoveryBox2.isVisible());


		keysPanePage.pressRecoveryPhrase()
				.pressPopupHyperlink()
				.clickOnPopupButton("RESET")
				.clickOnPopupButton("RESET")
				.setWords(TEST_WORDS_2)
				.clickOnPopupButton("RECOVER")
				.clickOnPopupButton("CONTINUE")
				.clickOnPopupButton("CONTINUE")
				.enterPasswordAndConfirm(PASSWORD)
				.clickOnPopupButton("CONTINUE");
		assertTrue(find("#recoveryVBox").isVisible());
		keysPanePage.pressCloseViewMnemonic();

		keysPanePage.pressRecoveryPhrase().enterPopupPassword(PASSWORD);
		final var recoveryBox3 = find("#recoveryVBox");
		assertTrue(recoveryBox3 instanceof VBox);
		assertTrue(recoveryBox3.isVisible());


		children = ((VBox) recoveryBox3).getChildren();
		labels = ((HBox) children.get(1)).getChildren();
		assertEquals(1, labels.size());
		assertTrue(labels.get(0) instanceof Label);

		words = ((Label) labels.get(0)).getText().toLowerCase(Locale.ROOT);
		for (final var testWord : TEST_WORDS_2) {
			assertTrue(words.contains(testWord));
		}

	}
}
