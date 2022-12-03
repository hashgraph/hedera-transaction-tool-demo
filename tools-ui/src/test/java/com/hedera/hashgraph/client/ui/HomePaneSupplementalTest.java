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
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.SettingsPanePage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Messages.BUNDLE_TITLE_MESSAGE_FORMAT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NEW_FILES_VBOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_EMAIL_TF;
import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class HomePaneSupplementalTest extends TestBase implements GenericFileReadWriteAware {

	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);

	private final List<VBox> publicKeyBoxes = new ArrayList<>();
	private final List<VBox> accountInfoBoxes = new ArrayList<>();
	private final List<VBox> batchBoxes = new ArrayList<>();
	private final List<VBox> transactionBoxes = new ArrayList<>();
	private final List<VBox> softwareBoxes = new ArrayList<>();
	private final List<VBox> systemBoxes = new ArrayList<>();
	private final List<VBox> freezeBoxes = new ArrayList<>();
	private final List<VBox> bundleBoxes = new ArrayList<>();

	@Before
	public void setUp() throws Exception {
		System.gc();
		logger.info("Starting test class: {}", getClass().getSimpleName());
		buildFolders();

		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original"), new File(DEFAULT_STORAGE));
		FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + KEYS_STRING));
		FileUtils.deleteDirectory(new File(DEFAULT_STORAGE + "/Accounts/0.0.56"));

		final var properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");

		if (new File(currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory" +
				"/TransactionTools/Keys/").mkdirs()) {
			logger.info("Keys path created");
		}

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

		final var controller = new MainController();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
		}
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		FileUtils.copyFile(new File("src/test/resources/Bundles/bundle.zip"),
				new File("src/test/resources/Transactions - Documents/InputFiles/bundle.zip"));
		FileUtils.copyFile(new File("src/test/resources/Bundles/infoBundle.zip"),
				new File("src/test/resources/Transactions - Documents/InputFiles/infoBundle.zip"));
		FileUtils.copyFile(new File("src/test/resources/Bundles/keyBundle.zip"),
				new File("src/test/resources/Transactions - Documents/InputFiles/keyBundle.zip"));

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		separateBoxes(newFiles);

		assertEquals(newFiles.size(),
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size() + softwareBoxes.size() + systemBoxes.size() + freezeBoxes.size() + bundleBoxes.size());
	}

	@After
	public void tearDown() throws IOException, TimeoutException {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

		publicKeyBoxes.clear();
		accountInfoBoxes.clear();
		batchBoxes.clear();
		transactionBoxes.clear();
		softwareBoxes.clear();
		systemBoxes.clear();
		freezeBoxes.clear();

		final var currentRelativePath = Paths.get("");
		final var s = currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory";
		if (new File(s).exists()) {
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

		Files.deleteIfExists(Path.of("src/test/resources/Transactions - Documents/InputFiles/bundle.zip"));
		Files.deleteIfExists(Path.of("src/test/resources/Transactions - Documents/InputFiles/infoBundle.zip"));
		Files.deleteIfExists(Path.of("src/test/resources/Transactions - Documents/InputFiles/keyBundle.zip"));
	}

	@Test
	public void bundleBoxesExist_test() {
		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();
		assertEquals(20, totalBoxes);

		final var storage = DEFAULT_STORAGE + File.separator + "Accounts";
		assertEquals(10,
				Objects.requireNonNull(new File(storage).listFiles(File::isFile)).length);

		final var publicKeysBundle = findInBoxes("Public key", "Account information");
		assertTrue(publicKeysBundle.getChildren().size() > 0);
		final var publicKeys = getLabelsFromGrid(publicKeysBundle).get(1).getText().split("\n");
		assertEquals(6, publicKeys.length);

		final var accountsBundle = findInBoxes("Account information", "Public key");
		assertTrue(accountsBundle.getChildren().size() > 0);
		final var accountKeys = getLabelsFromGrid(accountsBundle).get(1).getText().split("\n");
		assertEquals(5, accountKeys.length);
		assertTrue(getLabelsFromGrid(accountsBundle).get(1).getText().contains("Treasury test"));

		final var mixedBundle = findInBoxes("Public key,Account information", "");
		assertTrue(mixedBundle.getChildren().size() > 0);
		final var publicKeys2 = getLabelsFromGrid(mixedBundle).get(1).getText().split("\n");
		assertEquals(6, publicKeys2.length);
		final var accountKeys2 = getLabelsFromGrid(mixedBundle).get(3).getText().split("\n");
		assertEquals(5, accountKeys2.length);
		assertTrue(getLabelsFromGrid(mixedBundle).get(3).getText().contains("Treasury test"));
	}

	@Test
	public void acceptAccounts_test() throws HederaClientException {
		final var accountsBundle = findInBoxes("Account information", "Public key");
		assertTrue(accountsBundle.getChildren().size() > 0);
		final var initialAcountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(5, initialAcountObject.keySet().size());
		assertTrue(initialAcountObject.has("0.0.2-UNKNOWN"));
		assertEquals("treasury", initialAcountObject.get("0.0.2-UNKNOWN").getAsString());

		final var button = TestUtil.findButtonInPopup(accountsBundle.getChildren(), "ACCEPT");
		ensureVisible(button);
		clickOn(button);
		final var finalAccountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(9, finalAccountObject.keySet().size());
		assertTrue(finalAccountObject.has("0.0.1-UNKNOWN"));
		assertEquals("AccountOne", finalAccountObject.get("0.0.1-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.2-UNKNOWN"));
		assertEquals("Treasury test", finalAccountObject.get("0.0.2-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.3-UNKNOWN"));
		assertEquals("node1", finalAccountObject.get("0.0.3-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.4-UNKNOWN"));
		assertEquals("0.0.4", finalAccountObject.get("0.0.4-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.6-UNKNOWN"));
		assertEquals("AnotherNode", finalAccountObject.get("0.0.6-UNKNOWN").getAsString());
	}

	@Test
	public void acceptAccountsKeepNicknames_test() throws HederaClientException {
		final var accountsBundle = findInBoxes("Account information", "Public key");
		assertTrue(accountsBundle.getChildren().size() > 0);
		final var initialAcountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(5, initialAcountObject.keySet().size());
		assertTrue(initialAcountObject.has("0.0.2-UNKNOWN"));
		assertEquals("treasury", initialAcountObject.get("0.0.2-UNKNOWN").getAsString());

		final var checkBox = findCheckBoxInGridpane(accountsBundle);
		assertNotNull(checkBox);
		assertTrue(checkBox.isSelected());
		ensureVisible(checkBox);
		clickOn(checkBox);
		assertFalse(checkBox.isSelected());

		final var button = TestUtil.findButtonInPopup(accountsBundle.getChildren(), "ACCEPT");
		ensureVisible(button);
		clickOn(button);
		final var finalAccountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(9, finalAccountObject.keySet().size());
		assertTrue(finalAccountObject.has("0.0.1-UNKNOWN"));
		assertEquals("AccountOne", finalAccountObject.get("0.0.1-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.2-UNKNOWN"));
		assertEquals("treasury", finalAccountObject.get("0.0.2-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.3-UNKNOWN"));
		assertEquals("node1", finalAccountObject.get("0.0.3-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.4-UNKNOWN"));
		assertEquals("0.0.4", finalAccountObject.get("0.0.4-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.6-UNKNOWN"));
		assertEquals("AnotherNode", finalAccountObject.get("0.0.6-UNKNOWN").getAsString());
	}

	@Test
	public void declineAccounts_test() throws HederaClientException {
		final var accountsBundle = findInBoxes("Account information", "Public key");
		assertTrue(accountsBundle.getChildren().size() > 0);
		final var initialAcountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(5, initialAcountObject.keySet().size());
		assertTrue(initialAcountObject.has("0.0.2-UNKNOWN"));
		assertEquals("treasury", initialAcountObject.get("0.0.2-UNKNOWN").getAsString());

		final var button = TestUtil.findButtonInPopup(accountsBundle.getChildren(), "DECLINE");
		ensureVisible(button);
		clickOn(button);
		final var finalAccountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(initialAcountObject, finalAccountObject);
	}

	@Test
	public void acceptKeys_test() {
		final var keysBundle = findInBoxes("Public key", "Account information");
		assertTrue(keysBundle.getChildren().size() > 0);

		final var initialPublicKeys = new File(Constants.KEYS_FOLDER).listFiles(
				(dir, name) -> Constants.PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		final var initialKeys = Arrays.stream(initialPublicKeys).map(File::getName).collect(Collectors.toSet());
		assertEquals(1, initialKeys.size());
		assertTrue(initialKeys.contains("principalTestingKey.pub"));

		final var button = TestUtil.findButtonInPopup(keysBundle.getChildren(), "ACCEPT");
		ensureVisible(button);
		clickOn(button);

		final var finalPublicKeys = new File(Constants.KEYS_FOLDER).listFiles(
				(dir, name) -> Constants.PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		final var finalKeys = Arrays.stream(finalPublicKeys).map(File::getName).collect(Collectors.toSet());

		assertEquals(7, finalKeys.size());
		assertTrue(finalKeys.contains("principalTestingKey.pub"));
		assertTrue(finalKeys.contains("genesis.pub"));
		assertTrue(finalKeys.contains("testPubKey.pub"));
		assertTrue(finalKeys.contains("KeyStore-0.pub"));
		assertTrue(finalKeys.contains("KeyStore-1.pub"));
		assertTrue(finalKeys.contains("KeyStore-2.pub"));
		assertTrue(finalKeys.contains("KeyStore-3.pub"));
	}

	@Test
	public void declineKeys_test() {
		final var keysBundle = findInBoxes("Public key", "Account information");
		assertTrue(keysBundle.getChildren().size() > 0);

		final var initialPublicKeys = new File(Constants.KEYS_FOLDER).listFiles(
				(dir, name) -> Constants.PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		final var initialKeys = Arrays.stream(initialPublicKeys).map(File::getName).collect(Collectors.toSet());
		assertEquals(1, initialKeys.size());
		assertTrue(initialKeys.contains("principalTestingKey.pub"));

		final var button = TestUtil.findButtonInPopup(keysBundle.getChildren(), "DECLINE");
		ensureVisible(button);
		clickOn(button);

		final var finalPublicKeys = new File(Constants.KEYS_FOLDER).listFiles(
				(dir, name) -> Constants.PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		final var finalKeys = Arrays.stream(finalPublicKeys).map(File::getName).collect(Collectors.toSet());

		assertEquals(1, finalKeys.size());
		assertTrue(finalKeys.contains("principalTestingKey.pub"));
	}

	@Test
	public void acceptMixed_test() throws HederaClientException {
		final var bundle = findInBoxes("Account information,Public key", "");
		assertTrue(bundle.getChildren().size() > 0);
		final var initialAcountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(5, initialAcountObject.keySet().size());
		assertTrue(initialAcountObject.has("0.0.2-UNKNOWN"));
		assertEquals("treasury", initialAcountObject.get("0.0.2-UNKNOWN").getAsString());

		final var initialPublicKeys = new File(Constants.KEYS_FOLDER).listFiles(
				(dir, name) -> Constants.PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		final var initialKeys = Arrays.stream(initialPublicKeys).map(File::getName).collect(Collectors.toSet());
		assertEquals(1, initialKeys.size());
		assertTrue(initialKeys.contains("principalTestingKey.pub"));

		final var button = TestUtil.findButtonInPopup(bundle.getChildren(), "ACCEPT");
		ensureVisible(button);
		clickOn(button);
		final var finalAccountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(9, finalAccountObject.keySet().size());
		assertTrue(finalAccountObject.has("0.0.1-UNKNOWN"));
		assertEquals("AccountOne", finalAccountObject.get("0.0.1-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.2-UNKNOWN"));
		assertEquals("Treasury test", finalAccountObject.get("0.0.2-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.3-UNKNOWN"));
		assertEquals("node1", finalAccountObject.get("0.0.3-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.4-UNKNOWN"));
		assertEquals("0.0.4", finalAccountObject.get("0.0.4-UNKNOWN").getAsString());
		assertTrue(finalAccountObject.has("0.0.6-UNKNOWN"));
		assertEquals("AnotherNode", finalAccountObject.get("0.0.6-UNKNOWN").getAsString());

		final var finalPublicKeys = new File(Constants.KEYS_FOLDER).listFiles(
				(dir, name) -> Constants.PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		final var finalKeys = Arrays.stream(finalPublicKeys).map(File::getName).collect(Collectors.toSet());

		assertEquals(7, finalKeys.size());
		assertTrue(finalKeys.contains("principalTestingKey.pub"));
		assertTrue(finalKeys.contains("genesis.pub"));
		assertTrue(finalKeys.contains("testPubKey.pub"));
		assertTrue(finalKeys.contains("KeyStore-0.pub"));
		assertTrue(finalKeys.contains("KeyStore-1.pub"));
		assertTrue(finalKeys.contains("KeyStore-2.pub"));
		assertTrue(finalKeys.contains("KeyStore-3.pub"));

	}

	@Test
	public void declineMixed_test() throws HederaClientException {
		final var bundle = findInBoxes("Account information,Public key", "");
		assertTrue(bundle.getChildren().size() > 0);
		final var initialAcountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(5, initialAcountObject.keySet().size());
		assertTrue(initialAcountObject.has("0.0.2-UNKNOWN"));
		assertEquals("treasury", initialAcountObject.get("0.0.2-UNKNOWN").getAsString());

		final var initialPublicKeys = new File(Constants.KEYS_FOLDER).listFiles(
				(dir, name) -> Constants.PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		final var initialKeys = Arrays.stream(initialPublicKeys).map(File::getName).collect(Collectors.toSet());
		assertEquals(1, initialKeys.size());
		assertTrue(initialKeys.contains("principalTestingKey.pub"));


		final var button = TestUtil.findButtonInPopup(bundle.getChildren(), "DECLINE");
		ensureVisible(button);
		clickOn(button);


		final var finalAccountObject = readJsonObject(Constants.ACCOUNTS_MAP_FILE);
		assertEquals(initialAcountObject, finalAccountObject);

		final var finalPublicKeys = new File(Constants.KEYS_FOLDER).listFiles(
				(dir, name) -> Constants.PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		final var finalKeys = Arrays.stream(finalPublicKeys).map(File::getName).collect(Collectors.toSet());

		assertEquals(1, finalKeys.size());
		assertTrue(finalKeys.contains("principalTestingKey.pub"));
	}


	@Test
	public void missingRemote_test() throws IOException, TimeoutException {
		FileUtils.deleteDirectory(new File("src/test/resources/missing"));

		final var settingsPanePage = new SettingsPanePage(this);
		final var mainPage = new MainWindowPage(this);
		mainPage.clickOnSettingsButton();
		new File("src/test/resources/missing").mkdirs();

		final Node node = find("#transactionFoldersVBoxSP");
		assertTrue(node instanceof VBox);
		settingsPanePage.pressAddFolder()
				.setPath("src/test/resources/missing")
				.createPopup();

		assertFalse(find(ONEDRIVE_EMAIL_TF).isDisabled());

		settingsPanePage.setEmail("test@testemail.net").pressConfirmAddFolder().createPopup();

		mainPage.clickOnHomeButton();


		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();
		FileUtils.deleteDirectory(new File("src/test/resources/missing"));
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		ObservableList<Node> popupNodes = null;
		while (popupNodes == null) {
			popupNodes = getPopupNodes();
		}

		final var labels = TestUtil.getLabels(popupNodes);

		assertEquals(1, labels.size());
		assertEquals(
				"the application was unable to read files from the remote location: src/test/resources/missing. please" +
						" " +
						"make sure that the application is able to read the drive.",
				labels.get(0));

		final var button = TestUtil.findButtonInPopup(popupNodes, "CONTINUE");
		clickOn(button);

	}

	private VBox findInBoxes(final String inclusions, final String exclusions) {
		final var include = inclusions.split(",");
		final var exclude = exclusions.split(",");
		for (final var bundleBox : bundleBoxes) {
			var countInclude = 0;
			var countExclude = 0;
			final var labels = getLabelsFromGrid(bundleBox);
			for (final var label : labels) {
				for (final var s : include) {
					if (label.getText().equals(String.format(BUNDLE_TITLE_MESSAGE_FORMAT, s))) {
						countInclude++;
					}
				}
				for (final var s : exclude) {
					if (label.getText().equals(String.format(BUNDLE_TITLE_MESSAGE_FORMAT, s))) {
						countExclude++;
					}
				}
			}
			if (countExclude == 0 && countInclude == include.length) {
				return bundleBox;
			}
		}
		return new VBox();
	}

	private void separateBoxes(final ObservableList<Node> newFiles) {
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
				} else if (l.contains("Bundle")) {
					bundleBoxes.add((VBox) box);
				} else {
					logger.info("here");
				}
			}
		}
	}

	private List<Label> getLabelsFromGrid(final VBox box) {
		assertEquals(3, box.getChildren().size());
		assertTrue(box.getChildren().get(1) instanceof HBox);
		assertEquals(1, ((HBox) box.getChildren().get(1)).getChildren().size());
		assertTrue(((HBox) box.getChildren().get(1)).getChildren().get(0) instanceof GridPane);
		final var labels = ((GridPane) ((HBox) box.getChildren().get(1)).getChildren().get(0)).getChildren();
		final List<Label> returnLabels = new ArrayList<>();
		for (final var label : labels) {
			if (label instanceof Label) {
				returnLabels.add((Label) label);
			}
		}
		return returnLabels;
	}

	private CheckBox findCheckBoxInGridpane(final VBox box) {
		assertEquals(3, box.getChildren().size());
		assertTrue(box.getChildren().get(1) instanceof HBox);
		assertEquals(1, ((HBox) box.getChildren().get(1)).getChildren().size());
		assertTrue(((HBox) box.getChildren().get(1)).getChildren().get(0) instanceof GridPane);
		final var nodes = ((GridPane) ((HBox) box.getChildren().get(1)).getChildren().get(0)).getChildren();
		for (final var node : nodes) {
			if (node instanceof CheckBox) {
				return (CheckBox) node;
			}
		}
		return null;
	}


}
