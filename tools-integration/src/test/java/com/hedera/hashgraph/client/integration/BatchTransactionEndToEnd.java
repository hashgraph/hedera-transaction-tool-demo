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

import com.hedera.hashgraph.client.cli.ToolsMain;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.client.ui.StartUI;
import com.hedera.hashgraph.sdk.AccountBalanceQuery;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.opencsv.CSVWriter;
import javafx.scene.input.KeyCode;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static junit.framework.TestCase.assertEquals;

public class BatchTransactionEndToEnd extends TestBase implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(BatchTransactionEndToEnd.class);
	public static final String TRANSACTIONS = "src/test/resources/TempTransactions";
	public static final String RECEIPTS = "src/test/resources/TempReceipts";
	private static final int TEST_SIZE = 5;
	private static AccountId payerId;
	private final Set<AccountId> receivers = new HashSet<>();
	private Client client;

	protected static final String PASSWORD = "123456789";
	public static final int ONE_SECOND = 1000;


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
		Calendar cal = new Timestamp().plusSeconds(60).asCalendar();
		var sdf1 = new SimpleDateFormat("MM/dd/yy");
		var sdf2 = new SimpleDateFormat("HH:mm");
		Date date = cal.getTime();

		// Create csv
		List<String[]> distro = new ArrayList<>();
		for (AccountId receiver : receivers) {
			for (int i = 0; i < 100; i++) {
				var line = new String[] { String.valueOf(receiver), String.valueOf((i + 1)), sdf1.format(
						date), "memo line " + i };
				distro.add(line);
			}
		}
		writeCSVWithDistribution(distro, sdf2.format(date));

		setupUI();
	}

	@After
	public void tearDown() throws Exception {
		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
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
	public void signBatchTransaction_test() throws Exception {
		List<String> keys = new ArrayList<>();
		keys.add("KeyStore-0");
		keys.add("KeyStore-1");
		keys.add("KeyStore-2");
		keys.add("KeyStore-3");
		keys.add("KeyStore-4");
		keys.add("KeyStore-5");
		keys.add("KeyStore-6");
		keys.add("KeyStore-7");
		keys.add("KeyStore-8");
		keys.add("KeyStore-9");
		logger.info("Select 3 random keys");
		Collections.shuffle(keys);

		// Sign 3 times
		for (int i = 0; i < 9; i++) {
			clickOn(keys.get(i));
		}
		clickOn("SIGN\u2026");
		enterPasswordInPopup();

		while (true) {
			if (TestUtil.getPopupNodes() == null) {
				break;
			}
		}

		clickOn("ADD SIGNATURE");
		for (int i = 0; i < 8; i++) {
			clickOn(keys.get(i));
		}
		clickOn("SIGN\u2026");
		enterPasswordInPopup();

		while (true) {
			if (TestUtil.getPopupNodes() == null) {
				break;
			}
		}

		clickOn("ADD SIGNATURE");
		for (int i = 0; i < 7; i++) {
			clickOn(keys.get(i));
		}
		clickOn("SIGN\u2026");
		enterPasswordInPopup();

		while (true) {
			if (TestUtil.getPopupNodes() == null) {
				break;
			}
		}

		clickOn("ADD SIGNATURE");

		File[] summaryFiles = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				(dir, name) -> name.contains("summary"));

		assert summaryFiles != null;
		assertEquals(6, summaryFiles.length);

		File[] signatureFiles = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				(dir, name) -> name.contains("signatures"));
		assert signatureFiles != null;
		assertEquals(6, signatureFiles.length);


		File[] transactionFiles = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				(dir, name) -> name.contains("transactions"));
		assert transactionFiles != null;
		assertEquals(6, transactionFiles.length);

		Map<String, List<String>> map = new HashMap<>();
		for (File summaryFile : summaryFiles) {
			var summary = readCSV(summaryFile.getAbsolutePath());
			assertEquals(TEST_SIZE * 100 + 1, summary.size());
			for (List<String> strings : summary) {
				final var key = strings.get(0);
				final var value = strings.get(1);

				if (key.equals("\"Filename\"")) {
					continue;
				}
				List<String> tx = (map.containsKey(key)) ? map.get(key) : new ArrayList<>();
				tx.add(value);
				map.put(key, tx);
			}
		}

		for (Map.Entry<String, List<String>> entry : map.entrySet()) {
			var value = entry.getValue();
			Set<String> set = new HashSet<>(value);
			assertEquals(2, set.size());
		}

		for (File transactionFile : transactionFiles) {
			Files.copy(transactionFile.toPath(), Path.of(TRANSACTIONS, transactionFile.getName()));
		}

		for (File signatureFile : signatureFiles) {
			Files.copy(signatureFile.toPath(), Path.of(TRANSACTIONS, signatureFile.getName()));
		}


		File[] files = new File(TRANSACTIONS).listFiles();
		assert files != null;
		assertEquals(12, files.length);


		final String[] args = { "collate", "-f", TRANSACTIONS, "-a", Constants.ACCOUNTS_INFO_FOLDER };
		ToolsMain.main(args);

		for (File file : files) {
			Files.deleteIfExists(file.toPath());
		}

		files = new File(TRANSACTIONS).listFiles((dir, name) -> name.contains("testCSV"));
		assert files != null;
		assertEquals(2, files.length);

		final var file0 = files[0].getAbsolutePath();
		final var unzipped = TRANSACTIONS + File.separator + FilenameUtils.getBaseName(file0);
		unZip(file0, unzipped);

		logger.info("Submitting {}", TRANSACTIONS);
		final String[] argsSubmit = { "submit", "-t", unzipped, "-n", "INTEGRATION", "-o", RECEIPTS };
		ToolsMain.main(argsSubmit);

		for (AccountId receiver : receivers) {
			var balance = new AccountBalanceQuery()
					.setAccountId(receiver)
					.execute(client);

			logger.info("Final balance account {}: {}", receiver.toString(), balance.hbars.toString());
			assertEquals(5050, balance.hbars.toTinybars());
		}

	}

	private void enterPasswordInPopup() throws HederaClientException {
		var passwords = TestUtil.findPasswordInPopup(Objects.requireNonNull(TestUtil.getPopupNodes()));
		if (passwords == null) {
			throw new HederaClientException("Unexpected popup");
		}
		clickOn(passwords);
		write(PASSWORD);
		type(KeyCode.ENTER);
	}

	private void createAccounts() throws KeyStoreException, HederaClientException, TimeoutException,
			PrecheckStatusException, ReceiptStatusException {
		// create payer account
		var keyStore =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/KeyFiles/genesis.pem");
		var genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());


		client = CommonMethods.getClient(NetworkEnum.INTEGRATION);
		client.setOperator(new AccountId(0, 0, 2), genesisKey);
		var key = EncryptionUtils.jsonToKey(readJsonObject("src/test/resources/KeyFiles/jsonKey.json"));
		var transactionResponse = new AccountCreateTransaction()
				.setKey(key)
				.setInitialBalance(new Hbar(1000))
				.setAccountMemo("Test payer account")
				.execute(client);

		var receipt = transactionResponse.getReceipt(client);
		payerId = Objects.requireNonNull(receipt.accountId);
		logger.info("Payer Id: {}", payerId.toString());

		var accountInfo = new AccountInfoQuery()
				.setAccountId(payerId)
				.execute(client);

		logger.info("Account Balance = {}", accountInfo.balance.toString());
		writeBytes(String.format("%s/%s.info", "src/test/resources/Transactions - Documents/InputFiles", payerId),
				accountInfo.toBytes());

		// Create receiver accounts
		for (var i = 0; i < TEST_SIZE; i++) {
			transactionResponse = new AccountCreateTransaction()
					.setKey(key)
					.setInitialBalance(new Hbar(0))
					.setAccountMemo("Test receiver account")
					.execute(client);
			final var accountId = transactionResponse.getReceipt(client).accountId;
			assert accountId != null;
			logger.info("Receiver account created: {}", accountId.toString());
			receivers.add(accountId);
		}
	}

	private void writeCSVWithDistribution(List<String[]> distro, String time) {
		if (new File("src/test/resources/Transactions - Documents/InputFiles").mkdirs()) {
			logger.info("InputFiles folder created");
		}
		File file = new File("src/test/resources/Transactions - Documents/InputFiles/testCSV.csv");
		try (FileWriter fileWriter = new FileWriter(file)) {
			CSVWriter writer = new CSVWriter(fileWriter);

			List<String[]> data = new ArrayList<>();
			data.add(new String[] { "Sender Account", payerId.toString() });
			data.add(new String[] { "Sending Time", time });
			data.add(new String[] { "Node IDs", "0.0.3", "0.0.4" });
			data.add(new String[] { "AccountID", "Amount", "Start Date", "memo" });
			data.addAll(distro);
			writer.writeAll(data);
			writer.close();
		} catch (IOException e) {
			logger.error(e.getMessage());
		}
	}

	private void setupUI() throws IOException, KeyStoreException, TimeoutException {
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

		Controller controller = new Controller();
		var version = controller.getVersion();
		properties.setVersionString(version);

		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
		}

		var keys = new File("src/test/resources/KeyFiles").listFiles();
		assert keys != null;
		for (File key : keys) {
			FileUtils.copyFile(key, new File(KEYS_FOLDER, key.getName()));
		}

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		clickOn("ACCEPT");

		var nodes = TestUtil.getPopupNodes();

		assert nodes != null;
		var button = TestUtil.findButtonInPopup(nodes, "ACCEPT");
		clickOn(button);

	}

}
