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

package com.hedera.hashgraph.client.integration;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.integration.pages.AccountsPanePage;
import com.hedera.hashgraph.client.integration.pages.MainWindowPage;
import com.hedera.hashgraph.client.integration.pages.SettingsPanePage;
import com.hedera.hashgraph.client.ui.StartUI;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import javafx.scene.input.KeyCode;
import org.apache.commons.io.FileUtils;
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
import java.security.KeyStoreException;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;

public class QueryNetworkTest extends TestBase implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(BatchTransactionEndToEndTest.class);
	public static final String TRANSACTIONS = "src/test/resources/TempTransactions";
	public static final String RECEIPTS = "src/test/resources/TempReceipts";
	private static AccountId testAccountId;
	private Client client;
	private MainWindowPage mainWindowPage;
	private AccountsPanePage accountsPanePage;
	private SettingsPanePage settingsPanePage;

	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	public UserAccessibleProperties properties;

	@Before
	public void setUp() throws Exception {

		final var output =
				new File("src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org");
		if (output.exists()) {
			FileUtils.cleanDirectory(output);
		}
		if (output.mkdirs()) {
			logger.info("Output directory created");
		}

		final var transactionFolder = new File(TRANSACTIONS);
		if (transactionFolder.exists()) {
			FileUtils.cleanDirectory(transactionFolder);
		}
		if (transactionFolder.mkdirs()) {
			logger.info("Transaction folder created");
		}

		final var receiptFolder = new File(RECEIPTS);
		if (receiptFolder.exists()) {
			FileUtils.cleanDirectory(receiptFolder);
		}
		if (receiptFolder.mkdirs()) {
			logger.info("Transaction folder created");
		}

		final var input = new File("src/test/resources/Transactions - Documents/InputFiles/");
		if (input.exists()) {
			FileUtils.cleanDirectory(input);
		}
		if (input.mkdirs()) {
			logger.info("Input directory created");
		}
		createAccounts();
		TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
		setupUI();
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		mainWindowPage.clickOnAccountsButton();
	}

	@After
	public void tearDown() throws Exception {
		if (new File(DEFAULT_STORAGE).exists()) {
			try {
				FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
			} catch (IOException e) {
				logger.error("Unable to delete {}: {}", DEFAULT_STORAGE, e.getMessage());
			}
		}

		final var output =
				new File("src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org");
		if (output.exists()) {
			FileUtils.cleanDirectory(output);
		}

		final var input = new File("src/test/resources/Transactions - Documents/InputFiles/");
		if (input.exists()) {
			FileUtils.cleanDirectory(input);
		}
	}

	@Test
	public void requestOneBalance_test() throws InterruptedException, HederaClientException, KeyStoreException {
		final var nickname = testAccountId.toString();
		var oldBalance = accountsPanePage.getBalance(nickname);

		accountsPanePage.expandRow(nickname)
				.requestNewBalance(nickname);

		var newBalance = accountsPanePage.getBalance(nickname);
		assertEquals(oldBalance, newBalance);

		TestUtil.transfer(new AccountId(2L), testAccountId, Hbar.fromTinybars(123));
		accountsPanePage.expandRow(nickname)
				.requestNewBalance(nickname);

		mainWindowPage.clickOnSettingsButton().clickOnAccountsButton();

		var finalBalance = accountsPanePage.getBalance(nickname);
		assertEquals(oldBalance.toTinybars() + 123L, finalBalance.toTinybars());
	}

	@Test
	public void requestCheckedBalances_test() throws InterruptedException, HederaClientException {
		final var balancesFiles = new File(Constants.BALANCES_FILE);
		assertTrue(balancesFiles.exists());
		JsonArray initialBalances = readJsonArray(balancesFiles.getAbsolutePath());

		// Request all balances
		accountsPanePage.selectRow("treasury")
				.selectRow(testAccountId.toString())
				.requestSelectedBalances();

		while (true) {
			if (TestUtil.getPopupNodes() == null) {
				break;
			}
		}

		JsonArray balances = readJsonArray(balancesFiles.getAbsolutePath());

		assertEquals(initialBalances.size() + 2, balances.size());
	}

	@Test
	public void requestAllBalances_test() throws InterruptedException, HederaClientException {

		// Request all balances twice
		accountsPanePage.selectAllCheckBoxes()
				.requestSelectedBalances();

		while (true) {
			if (TestUtil.getPopupNodes() == null) {
				break;
			}
		}

		accountsPanePage.selectAllCheckBoxes()
				.requestSelectedBalances();

		while (true) {
			if (TestUtil.getPopupNodes() == null) {
				break;
			}
		}

		final var balancesFiles = new File(Constants.BALANCES_FILE);
		assertTrue(balancesFiles.exists());
		JsonArray balances = readJsonArray(balancesFiles.getAbsolutePath());
		int counter = 0;
		for (JsonElement balance : balances) {
			String account = balance.getAsJsonObject().get("account").getAsString();
			if (new Identifier(testAccountId).toReadableString().equals(account)) {
				counter++;
			}
		}

		assertEquals(3, counter);
	}

	@Test
	public void requestOneInfo_test() throws InterruptedException, HederaClientException {
		// Request all balances
		accountsPanePage.selectRow("treasury")
				.selectRow(testAccountId.toString())
				.requestSelectedBalances();

		var oldBalance = accountsPanePage.getBalance("treasury");

		accountsPanePage.selectRow("treasury")
				.requestSelectedInfo()
				.enterPasswordInPopup(TEST_PASSWORD);

		var newBalance = accountsPanePage.getBalance("treasury");
		assertTrue(newBalance.toTinybars() <= oldBalance.toTinybars());

	}

	@Test
	public void requestUnknownAccountsInfo_test() throws HederaClientException {

		var accounts = accountsPanePage.getAccounts().size();
		accountsPanePage.openAccordion()
				.enterAccounts("30-40")
				.clickRequestAccountsButton()
				.enterPasswordInPopup(TEST_PASSWORD)
				.acceptAllNickNames();

		var newAccounts = accountsPanePage.getAccounts().size();
		assertEquals(accounts + 11, newAccounts);

		accountsPanePage.openAccordion()
				.enterAccounts("45-50, 61, 67")
				.clickRequestAccountsButton()
				.enterPasswordInPopup(TEST_PASSWORD)
				.acceptAllNickNames();

		var newAccounts2 = accountsPanePage.getAccounts().size();

		assertEquals(accounts + 19, newAccounts2);

	}

	private void createAccounts() throws KeyStoreException, HederaClientException, TimeoutException,
			PrecheckStatusException, ReceiptStatusException {
		// create payer account
		var keyStore =
				Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), "src/test/resources/KeyFiles/genesis.pem");
		var genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());


		client = CommonMethods.getClient(NetworkEnum.INTEGRATION);
		client.setOperator(new AccountId(0, 0, 2), genesisKey);
		var key = EncryptionUtils.jsonToKey(readJsonObject("src/test/resources/KeyFiles/jsonKey.json"));
		var transactionResponse = new AccountCreateTransaction()
				.setKey(key)
				.setInitialBalance(new Hbar(1))
				.setAccountMemo("Test payer account")
				.execute(client);

		var receipt = transactionResponse.getReceipt(client);
		testAccountId = Objects.requireNonNull(receipt.accountId);
		logger.info("Payer Id: {}", testAccountId.toString());

		var accountInfo = new AccountInfoQuery()
				.setAccountId(testAccountId)
				.execute(client);

		logger.info("Account Balance = {}", accountInfo.balance.toString());
		writeBytes(String.format("%s/%s.info", "src/test/resources/Transactions - Documents/InputFiles", testAccountId),
				accountInfo.toBytes());
	}

	private void setupUI() throws IOException, KeyStoreException, TimeoutException, PrecheckStatusException,
			HederaClientException {
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

		Map<String, String> emailMap = new HashMap<>();
		emailMap.put(
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties.setOneDriveCredentials(emailMap);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.aes"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));

		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
		}

		var keys = new File("src/test/resources/KeyFiles").listFiles();
		assert keys != null;
		for (File key : keys) {
			FileUtils.copyFile(key, new File(KEYS_FOLDER, key.getName()));
		}

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		final var customNetworksFolder = new File(DEFAULT_STORAGE, "Files/.System/CustomNetworks/");
		if (customNetworksFolder.mkdirs()) {
			logger.info("Custom networks folder created: {}", customNetworksFolder.getAbsolutePath());
		}

		Files.copy(Path.of("src/test/resources/customNetwork.json"),
				Path.of(DEFAULT_STORAGE, "Files/.System/CustomNetworks/integration.json"));
		properties.setCustomNetworks(Collections.singleton("integration"));

		Set<String> defaultNetworks = new HashSet<>();
		defaultNetworks.add("MAINNET");
		defaultNetworks.add("TESTNET");
		defaultNetworks.add("PREVIEWNET");

		properties.setCurrentNetwork("integration", defaultNetworks);

		var treasuryInfo = new AccountInfoQuery()
				.setAccountId(new AccountId(2))
				.execute(client);
		writeBytes(String.format("%s/0.0.2.info", ACCOUNTS_INFO_FOLDER), treasuryInfo.toBytes());

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		clickOn("ACCEPT");

		var nodes = TestUtil.getPopupNodes();

		assert nodes != null;
		var button = TestUtil.findButtonInPopup(nodes, "ACCEPT");
		clickOn(button);

		mainWindowPage = new MainWindowPage(this);
		accountsPanePage = new AccountsPanePage(this);
		settingsPanePage = new SettingsPanePage(this);
	}

	@Test
	public void feePayerChange_test() throws HederaClientException, TimeoutException, IOException {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

		FileUtils.cleanDirectory(new File(ACCOUNTS_INFO_FOLDER));
		properties.setDefaultFeePayer(Identifier.ZERO);
		Files.deleteIfExists(Path.of(Constants.DEFAULT_STORAGE, "Files/.System/accountMapFile.json"));
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		mainWindowPage.clickOnAccountsButton();
		accountsPanePage.openAccordion();

		assertTrue(find("#feePayerTextFieldA").isVisible());
		assertFalse(find("#feePayerChoiceBoxA").isVisible());

		mainWindowPage.clickOnSettingsButton();

		assertTrue(find("#customFeePayerTextField").isVisible());
		assertFalse(find("#feePayerChoicebox").isVisible());

		settingsPanePage.enterFeePayer("2");
		assertFalse(find("#deleteCustomPayerButton").isDisabled());
		assertTrue(find("#deleteCustomPayerButton").isVisible());

		mainWindowPage.clickOnAccountsButton();

		assertFalse(find("#feePayerTextFieldA").isVisible());
		assertTrue(find("#feePayerChoiceBoxA").isVisible());

		accountsPanePage.enterAccounts("2")
				.clickRequestAccountsButton()
				.selectCheckBoxInPopup("genesis")
				.clickOnPopupButton("ACCEPT")
				.enterPasswordInPopup(TEST_PASSWORD);

		write("treasury");
		type(KeyCode.ENTER);

		mainWindowPage.clickOnSettingsButton();
		assertTrue(find("#deleteCustomPayerButton").isDisabled());
		assertTrue(find("#deleteCustomPayerButton").isVisible());

		mainWindowPage.clickOnAccountsButton();

		accountsPanePage.deleteAccount("treasury");
		assertFalse(find("#feePayerTextFieldA").isVisible());
		assertTrue(find("#feePayerChoiceBoxA").isVisible());

		mainWindowPage.clickOnSettingsButton();
		assertFalse(find("#customFeePayerTextField").isVisible());
		assertTrue(find("#feePayerChoicebox").isVisible());
		assertFalse(find("#deleteCustomPayerButton").isDisabled());
		assertTrue(find("#deleteCustomPayerButton").isVisible());

		clickOn("#deleteCustomPayerButton");

		mainWindowPage.clickOnAccountsButton();
		accountsPanePage.openAccordion();
		assertTrue(find("#feePayerTextFieldA").isVisible());
		assertFalse(find("#feePayerChoiceBoxA").isVisible());
	}
}
