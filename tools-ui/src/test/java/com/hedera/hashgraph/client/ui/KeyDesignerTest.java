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
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import javafx.scene.control.ListView;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import javax.swing.JFileChooser;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.CenterButtons;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.TitledPaneEnum.ACCOUNTS;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.TitledPaneEnum.PUBLIC_KEYS;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.copyCreatePaneKeys;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.countTreeNodes;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;

public class KeyDesignerTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(KeyDesignerTest.class);

	private CreatePanePage createPanePage;
	private AccountsPanePage accountsPanePage;
	private MainWindowPage mainWindowPage;

	private final Path currentRelativePath = Paths.get("");

	public UserAccessibleProperties properties;


	@Before
	public void setUp() {
		try {
			if (new File(DEFAULT_STORAGE).exists()) {
				FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
			}

			if (new File(DEFAULT_STORAGE).mkdirs()) {
				logger.info("TransactionTools folder created");
			}
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
			final var mapAsString = objectMapper.writeValueAsString(emailMap);

			properties.setOneDriveCredentials(emailMap);
			properties.setHash("123456789".toCharArray());

			properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
			setupTransactionDirectory(DEFAULT_STORAGE);

			final Controller controller = new Controller();
			final var version = controller.getVersion();
			properties.setVersionString(version);

			FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
					new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
			FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
					new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

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

	@After
	public void tearDown() {
		try {
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
		} catch (final Exception e) {
			logger.error(e);
			assertNotNull(e);
		}
	}

	@Test
	public void initialState_Test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey();

		final var accountTitledPane =
				createPanePage.getTitledPane(ACCOUNTS);

		final var arrowButton = createPanePage.getCenterButton(CenterButtons.ARROW_BUTTON);
		assertNotNull(arrowButton);
		assertFalse(arrowButton.getParent().isVisible());
		final var crossButton = createPanePage.getCenterButton(CenterButtons.CROSS_BUTTON);
		assertNotNull(crossButton);
		assertFalse(crossButton.getParent().isVisible());


		final var accountTitledPaneContent = accountTitledPane.getContent();
		assertTrue(accountTitledPaneContent instanceof VBox);
		final var accountsList = ((VBox) accountTitledPaneContent).getChildren();
		assertEquals(1, accountsList.size());
		assertTrue(accountsList.get(0) instanceof ListView);

		final var accounts = ((ListView<?>) accountsList.get(0)).getItems();

		assertEquals(5, accounts.size());

		final var publicKeyTitledPane = createPanePage.getTitledPane(PUBLIC_KEYS);

		assertTrue(arrowButton.getParent().isVisible());
		assertTrue(crossButton.getParent().isVisible());

		final var publicKeyTitledPaneContent = publicKeyTitledPane.getContent();
		assertTrue(publicKeyTitledPaneContent instanceof VBox);
		final var innerVBox = ((VBox) publicKeyTitledPaneContent).getChildren();
		assertEquals(1, innerVBox.size());
		assertTrue(innerVBox.get(0) instanceof VBox);

		final var lists = ((VBox) innerVBox.get(0)).getChildren();
		assertEquals(1, lists.size());

		final var publicKeys = ((ListView<?>) lists.get(0)).getItems();
		assertEquals(1, publicKeys.size());

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
		assertEquals(1, publicKeys.size());

		createPanePage.clickOnKeyDesignerCancel();
	}

	@Test
	public void setAccountKey_Test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setCreateKey();

		final var accountsTitledPane = createPanePage.getTitledPane(ACCOUNTS);
		final var accountsTitledPaneContent = accountsTitledPane.getContent();
		assertTrue(accountsTitledPaneContent instanceof VBox);
		final var innerVBox = ((VBox) accountsTitledPaneContent).getChildren();
		assertEquals(1, innerVBox.size());
		assertTrue(innerVBox.get(0) instanceof ListView);
		createPanePage.clickOnAccountKey("ninetyFour");
		final TreeView<?> treeView = getDesignTreeView();
		assertEquals(9, countTreeNodes(treeView.getRoot()));

		createPanePage.clickOnAccountKey("treasury");
		assertEquals(17, countTreeNodes(getDesignTreeView().getRoot()));

		createPanePage.clickOnKeyDesignerCancel();

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
