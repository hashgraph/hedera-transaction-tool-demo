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
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.pages.AccountsPanePage;
import com.hedera.hashgraph.client.ui.pages.CreatePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.testfx.api.FxToolkit;

import javax.swing.JFileChooser;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.TitledPaneEnum.ACCOUNTS;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.TitledPaneEnum.PUBLIC_KEYS;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.copyCreatePaneKeys;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.countTreeNodes;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findLabelsInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;

public class KeyDesignerTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(KeyDesignerTest.class);

	private CreatePanePage createPanePage;
	private AccountsPanePage accountsPanePage;
	private MainWindowPage mainWindowPage;

	private final Path currentRelativePath = Paths.get("");

	public UserAccessibleProperties properties;


	@BeforeEach
	public void setUp() {
		try {
			System.gc();
			logger.info("Starting test class: {}", getClass().getSimpleName());
			TestUtil.buildFolders();

			properties = new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");

			if (new File(currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
					"Documents/OutputFiles/test1.council2@hederacouncil.org/").mkdirs()) {
				logger.info("Output path created");
			}

			remakeTransactionTools();

			//Copy Accounts folders
			FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original/Accounts"),
					new File(DEFAULT_STORAGE + "/Accounts"));
			FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original/Files"),
					new File(DEFAULT_STORAGE + "/Files"));

			properties.setSetupPhase(SetupPhase.TEST_PHASE);

			FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
					new File(DEFAULT_STORAGE, MNEMONIC_PATH));

			final Map<String, String> emailMap = new HashMap<>();

			emailMap.put(
					currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
					"test1.council2@hederacouncil.org");


			final var objectMapper = new ObjectMapper();
			objectMapper.writeValueAsString(emailMap);

			properties.setOneDriveCredentials(emailMap);
			properties.setHash("123456789".toCharArray());

			properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
			//setupTransactionDirectory(DEFAULT_STORAGE);

			final Controller controller = new Controller();
			final var version = controller.getVersion();
			properties.setVersionString(version);

			FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
					new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
			FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
					new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

			final var testPublicKeys = new File("src/test/resources/Keys").listFiles(
					(dir, name) -> PUB_EXTENSION.equals(FilenameUtils.getExtension(name)) && name.startsWith(
							"KeyStore"));
			for (final File testPublicKey : testPublicKeys) {
				if (!new File(KEYS_FOLDER, testPublicKey.getName()).exists()) {
					FileUtils.copyFile(testPublicKey, new File(KEYS_FOLDER, testPublicKey.getName()));
				}
			}

			TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);
			FxToolkit.registerPrimaryStage();
			FxToolkit.setupApplication(StartUI.class);
			if (createPanePage == null) {
				createPanePage = new CreatePanePage(this);
			}
			if (mainWindowPage == null) {
				mainWindowPage = new MainWindowPage(this);
			}
			if (accountsPanePage == null) {
				accountsPanePage = new AccountsPanePage(this);
			}
			final var rootFolder = new JFileChooser().getFileSystemView().getDefaultDirectory().toString();
			if (!(new File(rootFolder)).exists() && new File(rootFolder).mkdirs()) {
				logger.info("Tools root folder created");
			} else {
				logger.info("Tools root directory exists.");
			}
			final var toolsFolder =
					new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents";
			if (!(new File(toolsFolder)).exists() && new File(toolsFolder).mkdirs()) {
				logger.info("Tools document directory created");
			} else {
				logger.info("Tools document directory exists.");
			}

			copyCreatePaneKeys();

			mainWindowPage.clickOnCreateButton();
		} catch (final Exception e) {
			logger.error(e);
			assertNotNull(e);
		}


	}

	@AfterEach
	public void tearDown() throws IOException, TimeoutException {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

		properties.resetProperties();
		properties.setSetupPhase(SetupPhase.INITIAL_SETUP_PHASE);
		final var transactions = (new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org")).listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(".tx") || name.endsWith(".txt") || name.endsWith("txsig");
				});

		assert transactions != null;
		for (final var f :
				transactions) {
			if (f.delete()) {
				logger.debug(String.format("%s has been deleted", f.getName()));
			}
		}
		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}

	@Test
	public void initialState_Test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey();

		final var accountTitledPane =
				createPanePage.getTitledPane(ACCOUNTS);


		final var accountTitledPaneContent = accountTitledPane.getContent();
		assertTrue(accountTitledPaneContent instanceof VBox);
		final var accountsList = ((VBox) accountTitledPaneContent).getChildren();
		assertEquals(1, accountsList.size());
		assertTrue(accountsList.get(0) instanceof ListView);

		final var accounts = ((ListView<?>) accountsList.get(0)).getItems();

		assertEquals(5, accounts.size());

		final var publicKeyTitledPane = createPanePage.getTitledPane(PUBLIC_KEYS);

		final var publicKeyTitledPaneContent = publicKeyTitledPane.getContent();
		assertTrue(publicKeyTitledPaneContent instanceof VBox);
		final var innerVBox = ((VBox) publicKeyTitledPaneContent).getChildren();
		assertEquals(1, innerVBox.size());
		assertTrue(innerVBox.get(0) instanceof VBox);

		final var lists = ((VBox) innerVBox.get(0)).getChildren();
		assertEquals(1, lists.size());

		final var publicKeys = ((ListView<?>) lists.get(0)).getItems();
		assertEquals(11, publicKeys.size());

		createPanePage.clickOnKeyDesignerCancel();

	}

	@Test
	public void addPublicKey_Test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey();

		final var publicKeyTitledPane = createPanePage.getTitledPane(PUBLIC_KEYS);
		final var publicKeyTitledPaneContent = publicKeyTitledPane.getContent();
		assertTrue(publicKeyTitledPaneContent instanceof VBox);
		final var innerVBox = ((VBox) publicKeyTitledPaneContent).getChildren();
		assertEquals(1, innerVBox.size());
		assertTrue(innerVBox.get(0) instanceof VBox);

		final var lists = ((VBox) innerVBox.get(0)).getChildren();
		assertEquals(1, lists.size());

		final var publicKeys = ((ListView<?>) lists.get(0)).getItems();
		assertEquals(11, publicKeys.size());

		createPanePage.clickOnKeyDesignerCancel();
	}

	@Test
	@Disabled("Account 'ninetyFour' does not exist in headless mode.")
	public void setAccountKey_Test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey();

		final var accountsTitledPane = createPanePage.getTitledPane(ACCOUNTS);
		final var accountsTitledPaneContent = accountsTitledPane.getContent();
		assertTrue(accountsTitledPaneContent instanceof VBox);
		final var innerVBox = ((VBox) accountsTitledPaneContent).getChildren();
		assertEquals(1, innerVBox.size());
		assertTrue(innerVBox.get(0) instanceof ListView);
		createPanePage.doubleClickOnAccountKey("ninetyFour");
		final TreeView<?> treeView = getDesignTreeView();
		assertEquals(9, countTreeNodes(treeView.getRoot()));

		createPanePage.doubleClickOnAccountKey("treasury");

		assertEquals(26, countTreeNodes(getDesignTreeView().getRoot()));

		createPanePage.clickOnKeyDesignerCancel();

	}

	@Test
	@Disabled("Choice box isn't being setup properly for this test.")
	public void testPlanItem1_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey();

		final var initialSize = createPanePage.getKeyTreeSize();

		createPanePage.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton();

		assertEquals(initialSize, createPanePage.getKeyTreeSize() - 1);
		createPanePage.closePopup("CANCEL");
	}

	@Test
	@Disabled("Account 'ninetyFour' does not exist in headless mode.")
	public void testPlanItem2_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnAccountKey("ninetyFour")
				.clickOnAddAccountButton();

		assertEquals(9, createPanePage.getKeyTreeSize());
		createPanePage.closePopup("CANCEL");
	}

	@Test
	public void testPlanItem3_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnPublicKey("KeyStore-0")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-1")
				.clickOnAddPublicKeyButton()
				.selectKeyInTree("KeyStore-1")
				.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton();

		assertEquals(5, createPanePage.getKeyTreeSize());
		createPanePage.closePopup("CANCEL");
	}

	@Test
	public void testPlanItem4_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnPublicKey("KeyStore-0")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-1")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton();

		var node = find("Threshold key (x of 3)");
		doubleClickOn(node);

		createPanePage.setThreshold(2);

		node = find("Threshold key (x of 3)");
		assertNull(node);

		node = find("Threshold key (2 of 3)");
		assertNotNull(node);
		createPanePage.closePopup("CANCEL");
	}

	@Test
	public void testPlanItem5_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-0")
				.clickOnAddPublicKeyButton()
				.selectKeyInTree("KeyStore-0")
				.clickOnPublicKey("KeyStore-1")
				.clickOnAddPublicKeyButton();

		var node = find("KeyStore key (x of 2)");
		doubleClickOn(node);

		createPanePage.setThreshold(2);

		node = find("KeyStore key (x of 2)");
		assertNull(node);

		node = find("KeyStore key (2 of 2)");
		assertNotNull(node);
		createPanePage.closePopup("CANCEL");
	}

	@Test
	public void testPlanItem6_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton()
				.deleteKeyFromTree("principalTestingKey");

		final var node = find("Threshold key (x of 0)");
		assertNotNull(node);
		doubleClickOn(node);

		final var labels = findLabelsInPopup(getPopupNodes());
		assertEquals(1, labels.size());
		assertTrue(labels.get(0) instanceof Label);
		assertTrue((labels.get(0)).getText().toLowerCase(Locale.ROOT).contains(
				"selected threshold does not have a list of keys associated with it."));

		clickOn(TestUtil.findButtonInPopup(TestUtil.getPopupNodes(), "CONTINUE"));
		createPanePage.closePopup("CANCEL");
	}

	@Test
	public void testPlanItem7_8_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnPublicKey("KeyStore-0")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-1")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton();

		var node = find("Threshold key (x of 3)");
		doubleClickOn(node);

		createPanePage.setThreshold(2);
		node = find("Threshold key (2 of 3)");
		assertNotNull(node);

		createPanePage.clickOnAccountKey("ninetyFour")
				.clickOnAddAccountButton();

		node = find("Threshold key (2 of 3)");
		assertNull(node);
		node = find("Threshold key (x of 4)");
		assertNotNull(node);
		node = find("Threshold key (2 of 2)");
		assertNotNull(node);
		int nodeCount = createPanePage.getKeyTreeSize();
		assertEquals(13, nodeCount);

		createPanePage.clickOnPublicKey("KeyStore-5")
				.clickOnAddPublicKeyButton();

		node = find("Threshold key (x of 5)");
		assertNotNull(node);

		nodeCount = createPanePage.getKeyTreeSize();
		assertEquals(14, nodeCount);
		createPanePage.closePopup("CANCEL");

	}

	@Test
	public void testPlanItem9_10_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-0")
				.clickOnAddPublicKeyButton()
				.selectKeyInTree("KeyStore-0")
				.clickOnPublicKey("KeyStore-1")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-2")
				.clickOnAddPublicKeyButton();

		var node = find("KeyStore key (x of 3)");
		assertNotNull(node);
		doubleClickOn(node);

		createPanePage.setThreshold(2);
		node = find("KeyStore key (2 of 3)");
		assertNotNull(node);

		node = find("Threshold key (x of 2)");
		assertNotNull(node);
		doubleClickOn(node);

		createPanePage.setThreshold(1);
		node = find("Threshold key (1 of 2)");
		assertNotNull(node);
		createPanePage.expandTree();

		createPanePage.selectKeyInTree("KeyStore key (2 of 3)")
				.clickOnPublicKey("KeyStore-3")
				.clickOnAddPublicKeyButton();

		node = find("Threshold key (1 of 2)");
		assertNotNull(node);

		node = find("KeyStore key (x of 4)");
		assertNotNull(node);
		createPanePage.closePopup("CANCEL");
	}

	@Test
	public void testPlanItem11a_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-0")
				.clickOnAddPublicKeyButton()
				.selectKeyInTree("KeyStore-0")
				.clickOnPublicKey("KeyStore-1")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-2")
				.clickOnAddPublicKeyButton();
		final int initialCount = createPanePage.getKeyTreeSize();
		createPanePage.deleteKeyFromTree("KeyStore-1");
		assertEquals(initialCount, createPanePage.getKeyTreeSize() + 1);

		createPanePage.deleteKeyFromTree("KeyStore key (x of 2)");
		assertEquals(2, createPanePage.getKeyTreeSize());
		createPanePage.closePopup("CANCEL");
	}

	@Test
	@Disabled("Account 'seventySis' does not exist in headless mode.")
	public void testPlanItem11b_test() throws IOException {
		FileUtils.deleteDirectory(new File(KEYS_FOLDER));
		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original/Keys"), new File(KEYS_FOLDER));

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnAccountKey("seventySix")
				.clickOnAddAccountButton()
				.clickOnPublicKey("nmody-tx");

		final int initialCount = createPanePage.getKeyTreeSize();
		createPanePage.deleteKeyFromTree("aharris-tx");
		assertEquals(initialCount, createPanePage.getKeyTreeSize() + 1);
		final var node = find("Threshold key (x of 3)");
		assertNotNull(node);
		createPanePage.closePopup("CANCEL");
	}

	@Test
	public void thresholdPopup_test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey()
				.clickOnPublicKey("principalTestingKey")
				.clickOnAddPublicKeyButton()
				.clickOnPublicKey("KeyStore-0")
				.clickOnAddPublicKeyButton()
				.selectKeyInTree("KeyStore-0")
				.clickOnPublicKey("KeyStore-1")
				.clickOnAddPublicKeyButton();

		final var node = find("KeyStore key (x of 2)");
		doubleClickOn(node);

		createPanePage.setThreshold(5);

		final var popupNodes = TestUtil.getPopupNodes();
		final var labels = TestUtil.findLabelsInPopup(popupNodes);

		assertFalse(labels.isEmpty());
		assertTrue(labels.get(1).getText().contains("try again"));
		createPanePage.setThreshold(1);

		final var node1 = find("KeyStore key (1 of 2)");
		assertNotNull(node1);
		createPanePage.closePopup("CANCEL");
	}

	private TreeView<?> getDesignTreeView() {
		final var popupNodes = getPopupNodes();
		final var node = ((HBox) ((VBox) popupNodes.get(1)).getChildren().get(0)).getChildren().get(2);
		assert node instanceof TitledPane;
		final var content = ((TitledPane) node).getContent();
		assert content instanceof VBox;
		final var nodes = ((VBox) content).getChildren();
		assert nodes.get(0) instanceof TreeView;
		return (TreeView<?>) nodes.get(0);
	}
}
