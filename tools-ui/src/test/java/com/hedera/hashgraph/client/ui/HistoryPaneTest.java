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
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.pages.HistoryWindowPage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.utilities.HistoryData;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableView;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.testfx.api.FxRobotException;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NEW_FILES_VBOX;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SuppressWarnings("MismatchedQueryAndUpdateOfCollection")

public class HistoryPaneTest extends TestBase implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(HistoryPaneTest.class);
	private static final Path currentRelativePath = Paths.get("");

	private HistoryWindowPage historyWindowPage;
	private MainWindowPage mainWindowPage;

	private final List<HistoryData> transactions = new ArrayList<>();
	private final List<HistoryData> batches = new ArrayList<>();
	private final List<HistoryData> largeBinaries = new ArrayList<>();
	private final List<HistoryData> accountInfos = new ArrayList<>();
	private final List<HistoryData> publicKeys = new ArrayList<>();
	private final List<HistoryData> updates = new ArrayList<>();
	private final List<HistoryData> bundles = new ArrayList<>();

	@Before
	public void setUp() throws Exception {
		System.gc();
		logger.info("Starting test class: {}", getClass().getSimpleName());
		TestUtil.buildFolders();

		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original"), new File(DEFAULT_STORAGE));
		FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + KEYS_STRING));
		FileUtils.deleteDirectory(new File(DEFAULT_STORAGE + "/Accounts/0.0.56"));

		final var properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");

		if (new File(currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory" +
				"/TransactionTools/Keys/").mkdirs()) {
			logger.info("Keys path created");
		}

		// Special case for test
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		final var pathname =
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org/";

		if (new File(pathname).exists()) {
			FileUtils.deleteDirectory(new File(pathname));
		}

		if (new File(pathname).mkdirs()) {
			logger.info("Output directory created");
		}

		final Map<String, String> emailMap = new HashMap<>();
		emailMap.put(
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties.setOneDriveCredentials(emailMap);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		//setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));

		final var controller = new Controller();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		if (new File(DEFAULT_HISTORY).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_HISTORY));
		}
		FileUtils.copyDirectory(new File("src/test/resources/HistoryTest"), new File(DEFAULT_HISTORY));

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		historyWindowPage = new HistoryWindowPage(this);
		mainWindowPage = new MainWindowPage(this);
		mainWindowPage.clickOnHistoryButton();
		historyWindowPage.rebuildHistory();
		populateHistory();
	}

	@After
	public void tearDown() throws Exception {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}

	@Test
	public void clickOnBogusItem_test() {
		assertThrows(FxRobotException.class, () -> clickOn("#exterminate"));
	}

	@Test
	public void reSign_test() {
		mainWindowPage.clickOnHomeButton();
		final var initialFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren().size();
		mainWindowPage.clickOnHistoryButton();

		historyWindowPage.clickOnResign(transactions.get(0).getFileName());
		final var button = find(transactions.get(0).getFileName());
		assertNotNull(button);
		assertTrue(button.isDisable());
		mainWindowPage.clickOnHomeButton();
		final var finalFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren().size();
		assertEquals(initialFiles, finalFiles);
	}

	private void populateHistory() {
		final var content = ((ScrollPane) find("#contentScrollPane")).getContent();
		assertTrue(content instanceof TableView);
		final var tableItems = ((TableView<HistoryData>) content).getItems();
		for (final var tableItem : tableItems) {
			switch (tableItem.getType()) {
				case TRANSACTION:
					transactions.add(tableItem);
					break;
				case BATCH:
					batches.add(tableItem);
					break;
				case LARGE_BINARY:
					largeBinaries.add(tableItem);
					break;
				case SOFTWARE_UPDATE:
					updates.add(tableItem);
					break;
				case ACCOUNT_INFO:
					accountInfos.add(tableItem);
					break;
				case PUBLIC_KEY:
					publicKeys.add(tableItem);
					break;
				case BUNDLE:
					bundles.add(tableItem);
					break;
				case COMMENT:
				case CONFIG:
				case METADATA:
				case UNKNOWN:
					logger.info("Unknown");
			}

		}
	}
}
