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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.ui.pages.AccountsPanePage;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.utilities.AccountLineInformation;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import junitparams.JUnitParamsRunner;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.table.TableRowExpanderColumn;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.testfx.api.FxRobotException;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import java.util.prefs.BackingStoreException;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_SCROLL_PANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.IMPORT_ACCOUNT_BUTTON;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getChildren;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static junit.framework.TestCase.assertNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("ALL")
@RunWith(JUnitParamsRunner.class)
public class AccountsPaneTest extends TestBase implements GenericFileReadWriteAware {

	public static final String USER_PROPERTIES = "/Files/user.properties";
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	protected static final String ZERO_TWO = "zero-two";

	private UserAccessibleProperties properties;
	private AccountsPanePage accountsPanePage;
	private MainWindowPage mainWindowPage;
	private String currentRelativePath = Paths.get("").toAbsolutePath().toString();


	@Before
	public void setUp() throws Exception {


		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		if (new File(DEFAULT_STORAGE).mkdirs()) {
			logger.info("TransactionTools folder created");
		}
		setupTransactionDirectory(DEFAULT_STORAGE);
		//FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original"), new File(DEFAULT_STORAGE));

		if (new File(
				currentRelativePath + "/src/test/resources/Transactions - Documents/OutputFiles/test1" +
						".council2@hederacouncil.org/").mkdirs()) {
			logger.info("Output path created");
		}

		Map<String, String> emailMap = new HashMap<>();
		emailMap.put(currentRelativePath + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + USER_PROPERTIES, "");
		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		properties.setHash("123456789".toCharArray());
		properties.setOneDriveCredentials(emailMap);

		Controller controller = new Controller();
		var version = controller.getVersion();
		properties.setVersionString(version);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));


		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		accountsPanePage = new AccountsPanePage(this);
		mainWindowPage = new MainWindowPage(this);

		mainWindowPage.clickOnAccountsButton();

	}

	@Test(expected = FxRobotException.class)
	public void clickOnBogusItem_Test() {
		clickOn("#exterminate");
	}

	@Test
	public void importOneAccount_Test() throws IOException, HederaClientException {
		String accountsInfoLocation = (Paths.get("")).toAbsolutePath().toString() +
				"/src/test/resources/AccountsInfo";

		Node x = find(IMPORT_ACCOUNT_BUTTON);
		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2.info");

		ObservableList<Node> popupNodes = getPopupNodes();

		assertEquals("Please enter a nickname for account 0.0.2",
				((Label) Objects.requireNonNull(popupNodes).get(0)).getText());//HERE BAD

		accountsPanePage.enterAccountNickName(ZERO_TWO)
				.closeNicknamePopup();


		JsonObject accountJson = readJsonObject(DEFAULT_STORAGE + "/Accounts/0.0.2.json");
		assertNotNull(accountJson);

		final File accountMapFile = new File(DEFAULT_STORAGE + "Files/.System/accountMapFile.json");
		assertTrue(accountMapFile.exists());
		JsonObject accountMap = readJsonObject(accountMapFile);

		assertTrue(accountMap.has("0.0.2"));
		assertEquals(ZERO_TWO, accountMap.get("0.0.2").getAsString());

		AccountInfo accountInfo =
				AccountInfo.fromBytes(readBytes(new File(accountsInfoLocation + "/0.0.2.info").getAbsolutePath()));

		accountsPanePage.expandRow(ZERO_TWO);

		ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		TableView tableView = (TableView) scrollPane.getContent();
		TableRowExpanderColumn tableRowExpanderColumn = (TableRowExpanderColumn) tableView.getColumns().get(0);
		sleep(100);
		VBox vBox = (VBox) tableRowExpanderColumn.getExpandedNode(tableView.getItems().get(0));

		assertTrue(vBox.isVisible());

		assertTrue(findTextInBox("Never", vBox));
		assertTrue(findTextInBox(accountInfo.autoRenewPeriod.getSeconds() + " s", vBox));

		assertTrue(findTextInBox(accountInfo.balance.toString(), vBox));
		assertTrue(findTextInBox(String.valueOf(accountInfo.isReceiverSignatureRequired), vBox));

		TreeView keysTree = findTreeInBox(vBox);

		assertEquals(14, getChildren(keysTree.getRoot()).size());

		accountsPanePage.deleteAccount(ZERO_TWO);
	}

	@Test
	public void deleteAccountAcceptDecline() throws Exception {
		String accountsInfoLocation = (Paths.get("")).toAbsolutePath().toString() +
				"/src/test/resources/AccountsInfo";

		Node x = find(IMPORT_ACCOUNT_BUTTON);
		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2.info");

		ObservableList<Node> popupNodes = getPopupNodes();

		assertEquals("Please enter a nickname for account 0.0.2",
				((Label) Objects.requireNonNull(popupNodes).get(0)).getText());//HERE BAD

		accountsPanePage.enterAccountNickName(ZERO_TWO)
				.closeNicknamePopup();

		clickOn(ZERO_TWO + "T");

		// do not delete
		popupNodes = getPopupNodes();
		Button cancelButton = TestUtil.findButtonInPopup(popupNodes, "CANCEL");

		assertNotNull(cancelButton);

		clickOn(cancelButton);

		assertTrue((new File(DEFAULT_STORAGE + "/Accounts/0.0.2.info")).exists());
		assertTrue((new File(DEFAULT_STORAGE + "/Accounts/0.0.2.json")).exists());

		// delete
		clickOn(ZERO_TWO + "T");

		popupNodes = getPopupNodes();
		Button continueButton = TestUtil.findButtonInPopup(popupNodes, "CONTINUE");

		assertNotNull(continueButton);

		clickOn(continueButton);
		assertFalse((new File(DEFAULT_STORAGE + "/Accounts/0.0.2.info")).exists());
		assertFalse((new File(DEFAULT_STORAGE + "/Accounts/0.0.2.json")).exists());

	}

	@Test
	public void importOneAccountWithExpiration_Test() throws IOException, HederaClientException {
		String accountsInfoLocation = (Paths.get("")).toAbsolutePath().toString() +
				"/src/test/resources/AccountsInfo";

		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.1005.info")
				.enterAccountNickName("thousand-five") //HERE BAD
				.closeNicknamePopup();

		AccountInfo accountInfo =
				AccountInfo.fromBytes(readBytes(new File(accountsInfoLocation + "/0.0.1005.info").getAbsolutePath()));

		final Timestamp expirationTime = new Timestamp(accountInfo.expirationTime);
		long millis = expirationTime.getSeconds() * 1000 + expirationTime.getNanos() / 1000000;
		Date date = new Date(millis);
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

		accountsPanePage.expandRow("thousand-five");

		ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		TableView tableView = (TableView) scrollPane.getContent();
		TableRowExpanderColumn tableRowExpanderColumn = (TableRowExpanderColumn) tableView.getColumns().get(0);
		sleep(100);
		VBox vBox = (VBox) tableRowExpanderColumn.getExpandedNode(tableView.getItems().get(0));

		assertTrue(vBox.isVisible());


		assertTrue(findTextInBox(sdf.format(date), vBox));


		accountsPanePage.deleteAccount("thousand-five");

	}

	@Test
	public void loadTwoAccountsDeleteOne_Test() {
		String accountsInfoLocation = (Paths.get("")).toAbsolutePath().toString() +
				"/src/test/resources/AccountsInfo";

		loadAccountInfo(ZERO_TWO, accountsInfoLocation + "/0.0.2.info");
		loadAccountInfo("seventy", accountsInfoLocation + "/0.0.70.info");

		List<String> items = getNicknames();

		assertEquals(2, items.size());
		assertTrue(items.contains(ZERO_TWO));
		assertTrue(items.contains("seventy"));

		accountsPanePage.deleteAccount("seventy");
		items = getNicknames();

		assertEquals(1, items.size());
		assertTrue(items.contains(ZERO_TWO));
		assertFalse(items.contains("seventy"));
	}

	private List<String> getNicknames() {
		ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		final TableView accountTable = (TableView) table;

		List<String> items = new ArrayList<>();
		for (Object item : accountTable.getItems()) {
			assertTrue(item instanceof AccountLineInformation);
			items.add(((AccountLineInformation) item).getNickname());

		}
		return items;
	}

	private boolean checkBalance(String nickname, String balance) throws HederaClientException {
		ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		final TableView accountTable = (TableView) table;

		List<String> items = new ArrayList<>();
		for (Object item : accountTable.getItems()) {
			assertTrue(item instanceof AccountLineInformation);
			if (((AccountLineInformation) item).getBalance().equals(Hbar.fromString(balance.replace(" ", "")))) {
				return true;
			}
		}
		return false;
	}


	private String accountID(String s) {
		ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		final TableView accountTable = (TableView) table;

		List<String> items = new ArrayList<>();
		for (Object item : accountTable.getItems()) {
			assertTrue(item instanceof AccountLineInformation);
			if (((AccountLineInformation) item).getNickname().contains(s)) {
				return ((AccountLineInformation) item).getAccount().toReadableString();
			}
		}
		return "";
	}

	@Test
	public void loadSameAccount_Test() throws HederaClientException {

		String accountsInfoLocation = (Paths.get("")).toAbsolutePath().toString() +
				"/src/test/resources/AccountsInfo";

		loadAccountInfo(ZERO_TWO, accountsInfoLocation + "/old/0.0.2.info");
		assertTrue(checkBalance(ZERO_TWO, "48 875 333 086.87 385 755"));

		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2.info");
		assertTrue(checkBalance(ZERO_TWO, "48 875 333 086.87 385 755"));

		accountsPanePage.dontReplaceAccount();
		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2.info");
		accountsPanePage.replaceAccount();
		assertTrue(checkBalance(ZERO_TWO, "46 479 878 904.04 547 520"));
	}


	@Test
	public void loadTwoAccountsSameNickname_Test() {
		String accountsInfoLocation = (Paths.get("")).toAbsolutePath().toString() +
				"/src/test/resources/AccountsInfo";

		loadAccountInfo(ZERO_TWO, accountsInfoLocation + "/0.0.2.info");

		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.70.info")
				.enterAccountNickName(ZERO_TWO)
				.enterAccountNickName("seventy")
				.closeNicknamePopup();

		List<String> items = getNicknames();
		assertEquals(2, items.size());
		assertTrue(items.contains(ZERO_TWO));
		assertTrue(items.contains("seventy"));

		assertTrue(accountID(ZERO_TWO).contains("0.0.2"));
		assertTrue(accountID("seventy").contains("0.0.70"));

		accountsPanePage.deleteAccount(ZERO_TWO);
		List<String> items2 = getNicknames();
		assertEquals(1, items2.size());
		accountsPanePage.deleteAccount("seventy");

	}

	@After
	public void tearDown() throws IOException, BackingStoreException {
		try {
			Path currentRelativePath = Paths.get("");
			String s = currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/testDirectory";
			if ((new File(s)).exists()) {
				FileUtils.deleteDirectory(new File(s));
			}

			String out =
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
	private AccountsPanePage loadAccountInfo(String name, String location) {
		logger.info("Loading account {} from {}", name, location);
		return accountsPanePage.loadInfoFromHiddenTextField(location)
				.enterAccountNickName(name)
				.closeNicknamePopup();
	}

	private Mnemonic getMnemonicFromFile(final char[] password, String path) {
		File mnemonicFile = new File(path);
		Mnemonic mnemonic = null;
		try {
			if (mnemonicFile.exists()) {
				mnemonic = SecurityUtilities.fromEncryptedFile(password,
						getSalt(properties.getHash(), properties.isLegacy()), path);
			}
		} catch (HederaClientException e) {
			logger.error(e);
			assertNull(e);
		}
		return mnemonic;

	}

	private AccountsPanePage createKey(String name, String password) {
		return accountsPanePage.scrollPane(1)
				.pressGenerateKeyButton()
				.enterNickName(name)
				.enterPassword(password)
				.enterRepeatPassword(password)
				.pressCreateKeysButton()
				.closePopup("CONTINUE");

	}

	private ScrollPane getScrollPane(String nodeName, int i) {
		Node node = find(nodeName);
		VBox vBox = (VBox) node.getParent();

		return (ScrollPane) vBox.getChildren().get(i);
	}


	private boolean findTextInBox(String text, Node box) {
		ObservableList<Node> nodes = null;
		if (box instanceof VBox) {
			nodes = ((VBox) box).getChildren();
		} else if (box instanceof HBox) {
			nodes = ((HBox) box).getChildren();
		} else if (box instanceof GridPane) {
			nodes = ((GridPane) box).getChildren();
		} else {
			return false;
		}
		for (Node node : nodes) {
			if ((node instanceof HBox || node instanceof VBox || node instanceof GridPane) && findTextInBox(text,
					node)) {
				return true;
			}
			if (node instanceof TextField && ((TextField) node).getText().contains(text)) {
				return true;
			}
			if (node instanceof Label && ((Label) node).getText().contains(text)) {
				return true;
			}
			if (node instanceof ScrollPane) {
				ScrollPane scrollPane = (ScrollPane) node;
				logger.info("here");
			}
		}
		return false;
	}

	private TreeView findTreeInBox(Node box) {
		ObservableList<Node> nodes = null;
		if (box instanceof VBox) {
			nodes = ((VBox) box).getChildren();
		} else if (box instanceof HBox) {
			nodes = ((HBox) box).getChildren();
		} else {
			return null;
		}
		for (Node node : nodes) {
			if (node instanceof HBox || node instanceof VBox) {
				final TreeView treeInHBox = findTreeInBox(node);
				if (treeInHBox != null) {
					return treeInHBox;
				}
			}
			if (node instanceof TreeView) {
				return (TreeView) node;
			}
		}
		return null;
	}


	// endregion

}
