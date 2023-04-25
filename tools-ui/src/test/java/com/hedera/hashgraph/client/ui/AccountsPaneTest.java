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
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.pages.AccountsPanePage;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.SettingsPanePage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.popups.AccountHistoryPopup;
import com.hedera.hashgraph.client.ui.utilities.AccountLineInformation;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.testfx.api.FxRobotException;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;
import java.util.prefs.BackingStoreException;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_SCROLL_PANE;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getChildren;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("rawtypes")
@Disabled("Issues with the FXRobot need to be resolved, first test fails, then the rest will pass")
public class AccountsPaneTest extends TestBase implements GenericFileReadWriteAware {

	public static final String USER_PROPERTIES = "/Files/user.properties";
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";

	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	protected static final String ZERO_TWO = "zero-two";

	private UserAccessibleProperties properties;
	private AccountsPanePage accountsPanePage;
	private final String currentRelativePath = Paths.get("").toAbsolutePath().toString();


	@BeforeEach
	public void setUp() throws Exception {
		logger.info("Starting test class: {}", getClass().getSimpleName());
		TestUtil.buildFolders();

		if (new File(
				currentRelativePath + "/src/test/resources/Transactions - Documents/OutputFiles/test1" +
						".council2@hederacouncil.org/").mkdirs()) {
			logger.info("Output path created");
		}

		final Map<String, String> emailMap = new HashMap<>();
		emailMap.put(currentRelativePath + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + USER_PROPERTIES, "");
		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		properties.setHash("123456789".toCharArray());
		properties.setOneDriveCredentials(emailMap);

		final Controller controller = new Controller();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pem"),
				new File(DEFAULT_STORAGE + "/Keys/genesis.pem"));
		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pub"),
				new File(DEFAULT_STORAGE + "/Keys/genesis.pub"));

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		final var customNetworksFolder = new File(DEFAULT_STORAGE, "Files/.System/CustomNetworks/");
		if (customNetworksFolder.mkdirs()) {
			logger.info("Custom networks folder created: {}", customNetworksFolder.getAbsolutePath());
		}

		Files.copy(Path.of("src/test/resources/customNetwork.json"),
				Path.of(DEFAULT_STORAGE, "Files/.System/CustomNetworks/INTEGRATION.json"));
		properties.setCustomNetworks(Collections.singleton("INTEGRATION"));

		final Set<String> defaultNetworks = new HashSet<>();
		defaultNetworks.add("MAINNET");
		defaultNetworks.add("TESTNET");
		defaultNetworks.add("PREVIEWNET");

		properties.setCurrentNetwork("INTEGRATION", defaultNetworks);

		final Map<String, String> payers = new HashMap<>();
		payers.put("INTEGRATION", "0.0.2-xrbis");
		properties.setDefaultFeePayers(payers);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		accountsPanePage = new AccountsPanePage(this);
		final MainWindowPage mainWindowPage = new MainWindowPage(this);

		mainWindowPage.clickOnAccountsButton();

	}

	@Test
	public void clickOnBogusItem_Test() {
		assertThrows(FxRobotException.class, () -> clickOn("#exterminate"));
	}

	@Test
	public void importOneAccount_Test() throws IOException, HederaClientException {
		final String accountsInfoLocation = (Paths.get("")).toAbsolutePath() +
				"/src/test/resources/AccountsInfo";

//		Node x = find(IMPORT_ACCOUNT_BUTTON);
		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2.info");

		final ObservableList<Node> popupNodes = getPopupNodes();

		assertEquals("Please enter a nickname for account 0.0.2",
				((Label) Objects.requireNonNull(popupNodes).get(0)).getText());//HERE BAD

		accountsPanePage.enterAccountNickName(ZERO_TWO)
				.closeNicknamePopup();


		final JsonObject accountJson = readJsonObject(DEFAULT_STORAGE + "/Accounts/0.0.2-UNKNOWN.json");
		assertNotNull(accountJson);

		final File accountMapFile = new File(DEFAULT_STORAGE + "Files/.System/accountMapFile.json");
		assertTrue(accountMapFile.exists());
		final JsonObject accountMap = readJsonObject(accountMapFile);

		assertTrue(accountMap.has("0.0.2-UNKNOWN"));
		assertEquals(ZERO_TWO, accountMap.get("0.0.2-UNKNOWN").getAsString());

		final AccountInfo accountInfo =
				AccountInfo.fromBytes(readBytes(new File(accountsInfoLocation + "/0.0.2.info").getAbsolutePath()));

		accountsPanePage.expandRow(ZERO_TWO);

		final ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		final TableView tableView = (TableView) scrollPane.getContent();
		final TableRowExpanderColumn tableRowExpanderColumn = (TableRowExpanderColumn) tableView.getColumns().get(0);
		sleep(100);
		final VBox vBox = (VBox) tableRowExpanderColumn.getExpandedNode(tableView.getItems().get(0));

		assertTrue(vBox.isVisible());

		assertTrue(findTextInBox("Never", vBox));
		assertTrue(findTextInBox(accountInfo.autoRenewPeriod.getSeconds() + " s", vBox));

		assertTrue(findTextInBox(accountInfo.balance.toString(), vBox));
		assertTrue(findTextInBox(String.valueOf(accountInfo.isReceiverSignatureRequired), vBox));

		final TreeView keysTree = findTreeInBox(vBox);

		assertEquals(14, getChildren(keysTree.getRoot()).size());

		accountsPanePage.deleteAccount(ZERO_TWO);
	}

	@Test
	public void setNetwork_test() {
		final String accountsInfoLocation = (Paths.get("")).toAbsolutePath() +
				"/src/test/resources/AccountsInfo";

		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2.info")
				.enterAccountNickName(ZERO_TWO)
				.closeNicknamePopup();
		assertTrue(new File(ACCOUNTS_INFO_FOLDER, "0.0.2-UNKNOWN.info").exists());
		assertTrue(new File(ACCOUNTS_INFO_FOLDER, "0.0.2-UNKNOWN.json").exists());
		accountsPanePage.setNetwork("integration");

		// Once the network is set, it should remove the choice box, but what it currently does is minimize the popup info
		// and then doesn't remove the choice box until the popup is reopened (and therefore recreated)
		// So I'm going to remove this one for now
		assertEquals(0, accountsPanePage.findChoiceBoxes().size());
		// And fixing the file names happens after the pane gets initialized, again, which isn't happening from just
		// the network (though it should, and does when using the app normally).
		// The whole interaction here is odd, but works normally, so I'll just leave this out and leave it alone for now
		assertTrue(new File(ACCOUNTS_INFO_FOLDER, "0.0.2-INTEGRATION.info").exists());
		assertTrue(new File(ACCOUNTS_INFO_FOLDER, "0.0.2-INTEGRATION.json").exists());

	}

	@Test
	public void deleteAccountAcceptDecline() {
		final String accountsInfoLocation = (Paths.get("")).toAbsolutePath() +
				"/src/test/resources/AccountsInfo";

		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2.info");

		ObservableList<Node> popupNodes = getPopupNodes();

		assertEquals("Please enter a nickname for account 0.0.2",
				((Label) Objects.requireNonNull(popupNodes).get(0)).getText());//HERE BAD

		accountsPanePage.enterAccountNickName(ZERO_TWO)
				.closeNicknamePopup();

		clickOn(ZERO_TWO + "T");

		// do not delete
		popupNodes = getPopupNodes();
		final Button cancelButton = TestUtil.findButtonInPopup(popupNodes, "CANCEL");

		assertNotNull(cancelButton);

		clickOn(cancelButton);

		assertTrue(new File(DEFAULT_STORAGE + "/Accounts/0.0.2-UNKNOWN.info").exists());
		assertTrue(new File(DEFAULT_STORAGE + "/Accounts/0.0.2-UNKNOWN.json").exists());

		// delete
		clickOn(ZERO_TWO + "T");

		popupNodes = getPopupNodes();
		final Button continueButton = TestUtil.findButtonInPopup(popupNodes, "CONTINUE");

		assertNotNull(continueButton);

		clickOn(continueButton);
		assertFalse(new File(DEFAULT_STORAGE + "/Accounts/0.0.2-UNKNOWN.info").exists());
		assertFalse(new File(DEFAULT_STORAGE + "/Accounts/0.0.2-UNKNOWN.json").exists());
		assertTrue(new File(DEFAULT_STORAGE + "/Accounts/Archive/DELETED/0.0.2-UNKNOWN.zip").exists());
	}

	@Test
	public void importOneAccountWithExpiration_Test() {
		final String accountsInfoLocation = (Paths.get("")).toAbsolutePath() +
				"/src/test/resources/AccountsInfo";

		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.1005.info")
				.enterAccountNickName("thousand-five") //HERE BAD
				.closeNicknamePopup();

		final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

		accountsPanePage.expandRow("thousand-five");

		final ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		final TableView tableView = (TableView) scrollPane.getContent();
		final TableRowExpanderColumn tableRowExpanderColumn = (TableRowExpanderColumn) tableView.getColumns().get(0);
		sleep(100);
		final VBox vBox = (VBox) tableRowExpanderColumn.getExpandedNode(tableView.getItems().get(0));

		assertTrue(vBox.isVisible());
		accountsPanePage.deleteAccount("thousand-five");

	}

	@Test
	@Disabled("Popup nodes not showing up in headless mode")
	public void loadTwoAccountsDeleteOne_Test() {
		final String accountsInfoLocation = (Paths.get("")).toAbsolutePath() +
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

	@Test
	@Disabled("")
	public void loadSameAccount_Test() throws HederaClientException {

		final String accountsInfoLocation = (Paths.get("")).toAbsolutePath() +
				"/src/test/resources/AccountsInfo";

		loadAccountInfo(ZERO_TWO, accountsInfoLocation + "/old/0.0.2.info");
		assertTrue(checkBalance("48 875 333 086.87 385 755"));

		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2.info");
		assertTrue(checkBalance("46 479 878 904.04 547 520"));

	}

	@Test
	public void loadTwoAccountsSameNickname_Test() {
		final String accountsInfoLocation = (Paths.get("")).toAbsolutePath() +
				"/src/test/resources/AccountsInfo";

		loadAccountInfo(ZERO_TWO, accountsInfoLocation + "/0.0.2.info");

		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.70.info")
				.enterAccountNickName(ZERO_TWO)
				.enterAccountNickName("seventy")
				.closeNicknamePopup();

		final List<String> items = getNicknames();
		assertEquals(2, items.size());
		assertTrue(items.contains(ZERO_TWO));
		assertTrue(items.contains("seventy"));

		assertTrue(accountID(ZERO_TWO).contains("0.0.2"));
		assertTrue(accountID("seventy").contains("0.0.70"));

		accountsPanePage.deleteAccount(ZERO_TWO);
		final List<String> items2 = getNicknames();
		assertEquals(1, items2.size());
		accountsPanePage.deleteAccount("seventy");

	}

	@Test
	@Disabled("Uses Integration network, needs to be updated to use Testnet")
	public void checkAccountHistoryPopup_test() throws InterruptedException, HederaClientException {
		final String accountsInfoLocation = (Paths.get("")).toAbsolutePath() +
				"/src/test/resources/AccountsInfo";
		accountsPanePage.loadInfoFromHiddenTextField(accountsInfoLocation + "/0.0.2_integration.info")
				.enterAccountNickName(ZERO_TWO)
				.closeNicknamePopup()
				.setNetwork("TESTNET")
				.selectRow(ZERO_TWO)
				.requestSelectedInfo()
				.enterPasswordInPopup(TEST_PASSWORD);


		sleep(500);
		final File[] archive = new File(DEFAULT_STORAGE, "Accounts/Archive").listFiles(
				(dir, name) -> name.startsWith("0.0.2"));

		assertNotNull(archive);
		assertEquals(2, archive.length);

		final var table = accountsPanePage
				.clickOnSeeHistory()
				.getTableFromPopup(getPopupNodes());

		assertEquals(1, table.getItems().size());

		final var line = (AccountHistoryPopup.TableLine) table.getItems().get(0);
		doubleClickOn(line.getDate());
		final var tableFromPopup = accountsPanePage.clickOnSeeHistory().getTableFromPopup(getPopupNodes());
		assertEquals(9, tableFromPopup.getItems().size());

		accountsPanePage.pressPopupButton("CLOSE")
				.pressPopupButton("CLOSE");
	}

	@Test
	@Disabled("#accoutnsToUpdateTextField is not present in headless mode.")
	public void noFeePayerSelected_test() {
		final MainWindowPage mainWindowPage = new MainWindowPage(this);
		mainWindowPage.clickOnSettingsButton();
		final var settingsPanePage = new SettingsPanePage(this);
		settingsPanePage.openNetworksCombobox("TESTNET");
		mainWindowPage.clickOnAccountsButton();

		clickOn("Add accounts");
		clickOn("#accountsToUpdateTextField");
		write("12345");
		clickOn("#selectAccountsButton");

		final var nodes1 = getPopupNodes();
		assertNotNull(nodes1);
		assertEquals(1, nodes1.size());
		assertTrue(nodes1.get(0) instanceof VBox);
		final var children = ((VBox) nodes1.get(0)).getChildren();
		assertTrue(children.get(0) instanceof Label);
		assertEquals(ErrorMessages.FEE_PAYER_NOT_SET_ERROR_MESSAGE, ((Label) children.get(0)).getText());
		clickOn("CONTINUE");
	}

	@AfterEach
	public void tearDown() throws IOException, BackingStoreException, TimeoutException {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

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

	}

	// region AUXILIARY METHODS
	@SuppressWarnings("UnusedReturnValue")
	private AccountsPanePage loadAccountInfo(final String name, final String location) {
		logger.info("Loading account {} from {}", name, location);
		return accountsPanePage.loadInfoFromHiddenTextField(location)
				.enterAccountNickName(name)
				.closeNicknamePopup();
	}

	private boolean findTextInBox(final String text, final Node box) {
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
		for (final Node node : nodes) {
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
				logger.info("here");
			}
		}
		return false;
	}

	private TreeView findTreeInBox(final Node box) {
		ObservableList<Node> nodes = null;
		if (box instanceof VBox) {
			nodes = ((VBox) box).getChildren();
		} else if (box instanceof HBox) {
			nodes = ((HBox) box).getChildren();
		} else {
			return null;
		}
		for (final Node node : nodes) {
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

	private boolean checkBalance(final String balance) throws HederaClientException {
		final ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		final Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		final TableView accountTable = (TableView) table;

		for (final Object item : accountTable.getItems()) {
			assertTrue(item instanceof AccountLineInformation);
			if (((AccountLineInformation) item).getBalance().equals(Hbar.fromString(balance.replace(" ", "")))) {
				return true;
			}
		}
		return false;
	}

	private String accountID(final String s) {
		final ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		final Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		final TableView accountTable = (TableView) table;

		for (final Object item : accountTable.getItems()) {
			assertTrue(item instanceof AccountLineInformation);
			if (((AccountLineInformation) item).getNickname().contains(s)) {
				return ((AccountLineInformation) item).getAccount().toReadableString();
			}
		}
		return "";
	}

	private List<String> getNicknames() {
		final ScrollPane scrollPane = find(ACCOUNTS_SCROLL_PANE);
		final Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		final TableView accountTable = (TableView) table;

		final List<String> items = new ArrayList<>();
		for (final Object item : accountTable.getItems()) {
			assertTrue(item instanceof AccountLineInformation);
			items.add(((AccountLineInformation) item).getNickname());

		}
		return items;
	}


	// endregion

}
