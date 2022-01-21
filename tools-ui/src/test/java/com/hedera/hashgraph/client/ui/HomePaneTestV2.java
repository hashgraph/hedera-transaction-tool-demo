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
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.HISTORY_FILES_VBOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NEW_FILES_VBOX;
import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertTrue;

public class HomePaneTestV2 extends TestBase implements GenericFileReadWriteAware {
	protected static final String PRINCIPAL_TESTING_KEY = "principalTestingKey";
	protected static final String PASSWORD = "123456789";
	public static final int ONE_SECOND = 1000;
	private HomePanePage homePanePage;
	private MainWindowPage mainWindowPage;

	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	public UserAccessibleProperties properties;
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private final List<VBox> publicKeyBoxes = new ArrayList<>();
	private final List<VBox> accountInfoBoxes = new ArrayList<>();
	private final List<VBox> batchBoxes = new ArrayList<>();
	private final List<VBox> transactionBoxes = new ArrayList<>();
	private final List<VBox> softwareBoxes = new ArrayList<>();
	private final List<VBox> systemBoxes = new ArrayList<>();
	private final List<VBox> freezeBoxes = new ArrayList<>();


	@Before
	public void setUp() throws Exception {

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		if (new File(DEFAULT_STORAGE).mkdirs()) {
			logger.info("Transaction tools directory created");
		}

		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original"), new File(DEFAULT_STORAGE));
		FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + KEYS_STRING));
		FileUtils.deleteDirectory(new File(DEFAULT_STORAGE + "/Accounts/0.0.56"));

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");

		if (new File(currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory" +
				"/TransactionTools/Keys/").mkdirs()) {
			logger.info("Keys path created");
		}

		// Special case for test: Does not ask for password during setup
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
		setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));

		final Controller controller = new Controller();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
		}
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		homePanePage = new HomePanePage(this);
		mainWindowPage = new MainWindowPage(this);

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		separateBoxes(newFiles, publicKeyBoxes, accountInfoBoxes, batchBoxes, transactionBoxes, softwareBoxes,
				systemBoxes, freezeBoxes);

		assertEquals(newFiles.size(),
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size() + softwareBoxes.size() + systemBoxes.size() + freezeBoxes.size());
	}

	@After
	public void tearDown() throws IOException {
		publicKeyBoxes.clear();
		accountInfoBoxes.clear();
		batchBoxes.clear();
		transactionBoxes.clear();
		softwareBoxes.clear();
		systemBoxes.clear();
		freezeBoxes.clear();

		final var currentRelativePath = Paths.get("");
		final var s = currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory";
		if ((new File(s)).exists()) {
			FileUtils.deleteDirectory(new File(s));
		}

		final var out =
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org";
		if (new File(out).exists()) {
			FileUtils.cleanDirectory(new File(out));
		}

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}

	@Test
	public void findAccountInfosAndAcceptOne_Test() {
		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();

		final var storage = DEFAULT_STORAGE + File.separator + "Accounts";
		assertEquals(10,
				Objects.requireNonNull(new File(storage).listFiles(File::isFile)).length);

		sleep(ONE_SECOND);
		homePanePage.clickOn2ButtonBar(0, accountInfoBoxes.get(1));

		sleep(ONE_SECOND);

		homePanePage.enterStringInPopup("testAccount");
		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var historyFiles = ((VBox) find(HISTORY_FILES_VBOX)).getChildren();

		assertEquals(totalBoxes - 1, refreshFiles.size());
		assertEquals(1, historyFiles.size()); // see other note


		final var acceptedKey = (VBox) historyFiles.get(0);
		final var legend =
				((GridPane) ((HBox) acceptedKey.getChildren().get(1)).getChildren().get(0)).getChildren().get(0);


		assertTrue(legend instanceof Label);
		assertTrue(((Label) legend).getText().contains("accepted on"));

		assertEquals(12,
				Objects.requireNonNull(new File(storage).listFiles(File::isFile)).length);
	}


	private void separateBoxes(final ObservableList<Node> newFiles, final List<VBox> publicKeyBoxes,
			final List<VBox> accountInfoBoxes,
			final List<VBox> batchBoxes, final List<VBox> transactionBoxes, final List<VBox> softwareBoxes,
			final List<VBox> systemBoxes,
			final List<VBox> freezeBoxes) {
		for (final var box : newFiles) {
			assertTrue(box instanceof VBox);

			final var lines = ((VBox) box).getChildren();
			if (lines.size() >= 3) {
				assertTrue(lines.get(0) instanceof Label);
				final var l = ((Label) lines.get(0)).getText();
				if (l.contains("Batch")) {
					batchBoxes.add((VBox) box);
				} else if (l.contains("Transaction") && !(l.contains("ZippedTransactions") || l.contains(
						"Freeze") || l.contains("Upgrade"))) {
					transactionBoxes.add((VBox) box);
				} else if (l.contains("Account Information")) {
					accountInfoBoxes.add((VBox) box);
				} else if (l.contains("Public Key")) {
					publicKeyBoxes.add((VBox) box);
				} else if (l.contains("Software")) {
					softwareBoxes.add((VBox) box);
				} else if (l.contains("Restore") || l.contains("Remove")) {
					systemBoxes.add((VBox) box);
				} else if (l.contains("Freeze") || l.contains("Upgrade")) {
					freezeBoxes.add((VBox) box);
				} else {
					logger.info("here");
				}
			}
		}
	}


}
