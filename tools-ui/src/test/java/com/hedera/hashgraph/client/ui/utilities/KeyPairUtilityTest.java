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

import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.client.ui.StartUI;
import com.hedera.hashgraph.client.ui.TestBase;
import com.hedera.hashgraph.client.ui.pages.HistoryWindowPage;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_EXPIRATION_TIME;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

@SuppressWarnings("StatementWithEmptyBody")

public class KeyPairUtilityTest extends TestBase {

	private static final Logger logger = LogManager.getLogger(KeyPairUtility.class);

	private final Controller controller = new Controller();
	MainWindowPage mainWindowPage;
	HomePanePage homePanePage;
	HistoryWindowPage historyWindowPage;

	@Before
	public void setUp() throws Exception {
		final var currentRelativePath = Paths.get("");

		TestUtil.buildFolders();

		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original"), new File(DEFAULT_STORAGE));
		FileUtils.cleanDirectory(new File(DEFAULT_STORAGE, KEYS_STRING));
		FileUtils.deleteDirectory(new File(ACCOUNTS_INFO_FOLDER, "0.0.56"));

		final var properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");

		if (new File(currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory" +
				"/TransactionTools/Keys/").mkdirs()) {
			logger.info("Keys path created");
		}

		final var empty = currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents - " +
				"empty/";

		if (new File(empty, "InputFiles").mkdirs()) {
			logger.info("Empty input files folder created");
		}

		if (new File(empty, "OutputFiles/test1.council2@hederacouncil.org").mkdirs()) {
			logger.info("Empty output files folder created");
		}

		final var transactions = new File("src/test/resources/TestTransactions").listFiles();
		for (final File transaction : transactions) {
			FileUtils.copyFile(transaction, new File(empty, "InputFiles" + File.separator + transaction.getName()));
		}

		final Map<String, String> emailMap = new HashMap<>();
		emailMap.put(empty, "test1.council2@hederacouncil.org");

		properties.setOneDriveCredentials(emailMap);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		//setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE, MNEMONIC_PATH));

		final var keys = new File("src/test/resources/Keys_Large").listFiles();
		for (final File key : keys) {
			FileUtils.copyFile(key, new File(KEYS_FOLDER, key.getName()));
		}

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);


		final var version = controller.getVersion();
		properties.setVersionString(version);
		properties.setSetupPhase(SetupPhase.TEST_PHASE);

		mainWindowPage = new MainWindowPage(this);
		homePanePage = new HomePanePage(this);
		historyWindowPage = new HistoryWindowPage(this);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);
	}

	@After
	public void tearDown() throws Exception {
		FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		FileUtils.deleteDirectory(
				new File(Paths.get("").toAbsolutePath() + "/src/test/resources/Transactions - Documents - " +
						"empty/"));
	}

	@Test
	public void mapExpiration_test() {
		clickOn("apopowycz-tx");
		clickOn("lbaird-tx");
		clickOn("shunjan-tx");
		clickOn("SIGN\u2026");

		homePanePage.enterPasswordInPopup("123654789");
		mainWindowPage.clickOnHistoryButton();
		historyWindowPage.clickOnResign("1743832800-0_0_94-58824159.tx");
		mainWindowPage.clickOnHomeButton();

		clickOn("lbaird-tx");
		clickOn("shunjan-tx");
		clickOn("SIGN\u2026");
		final var startTimer = Instant.now();
		var popup = getPopupNodes();
		assertNull(popup);

		while (Instant.now().isBefore(startTimer.plusSeconds(TEST_EXPIRATION_TIME))) {
			// wait for timer to expire
		}

		mainWindowPage.clickOnHistoryButton();
		historyWindowPage.clickOnResign("1743832800-0_0_94-58824159.tx");
		mainWindowPage.clickOnHomeButton();

		clickOn("lbaird-tx");
		clickOn("SIGN\u2026");
		popup = getPopupNodes();
		assertNotNull(popup);
		homePanePage.enterPasswordInPopup("123654789");
	}

	@Test
	public void differentPasswords_test() throws HederaClientException {
		clickOn("apopowycz-tx");
		clickOn("lbaird-tx");
		clickOn("shunjan-tx");
		clickOn("SIGN\u2026");

		homePanePage.enterPasswordInPopup("123654789");

		mainWindowPage.clickOnHistoryButton();
		historyWindowPage.clickOnResign("1743832800-0_0_94-58824159.tx");
		mainWindowPage.clickOnHomeButton();

		clickOn("apopowycz-tx");
		clickOn("lbaird-tx");
		clickOn("shunjan-tx");
		clickOn("ADD MORE");

		homePanePage.clickOnKeyCheckBox("KeyStore-0")
				.clickOnPopupButton("ACCEPT");


		clickOn("SIGN\u2026");

		assertNotNull(getPopupNodes());

		homePanePage.enterPasswordInPopup(TEST_PASSWORD);


		mainWindowPage.clickOnHistoryButton();
		historyWindowPage.clickOnResign("1743832800-0_0_94-58824159.tx");
		mainWindowPage.clickOnHomeButton();

		clickOn("lbaird-tx");
		clickOn("shunjan-tx");
		clickOn("ADD MORE");

		homePanePage.clickOnKeyCheckBox("KeyStore-1")
				.clickOnPopupButton("ACCEPT");

		clickOn("SIGN\u2026");
		assertNull(getPopupNodes());

		final var startTimer = Instant.now();
		while (Instant.now().isBefore(startTimer.plusSeconds((long) (TEST_EXPIRATION_TIME * 1.5)))) {
			// wait for timer to expire
		}

		mainWindowPage.clickOnHistoryButton();
		historyWindowPage.clickOnResign("1743832800-0_0_94-58824159.tx");
		mainWindowPage.clickOnHomeButton();

		clickOn("lbaird-tx");
		clickOn("SIGN\u2026");

		assertNotNull(getPopupNodes());
		homePanePage.enterPasswordInPopup("123654789");

		mainWindowPage.clickOnHistoryButton();
		historyWindowPage.clickOnResign("1743832800-0_0_94-58824159.tx");
		mainWindowPage.clickOnHomeButton();

		clickOn("lbaird-tx");
		clickOn("ADD MORE");

		homePanePage.clickOnKeyCheckBox("KeyStore-2")
				.clickOnPopupButton("ACCEPT");
		clickOn("SIGN\u2026");


		assertNotNull(getPopupNodes());
		homePanePage.enterPasswordInPopup(TEST_PASSWORD);

	}
}
