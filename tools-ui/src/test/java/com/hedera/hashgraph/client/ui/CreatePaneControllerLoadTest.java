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
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolSystemTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.client.ui.pages.AccountsPanePage;
import com.hedera.hashgraph.client.ui.pages.CreatePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import javafx.scene.control.TextField;
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
import java.util.Objects;

import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;


public class CreatePaneControllerLoadTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(CreatePaneControllerTest.class);

	private final String resources = new File("src/test/resources/Transactions - Documents/").getAbsolutePath().replace(
			System.getProperty("user.home") + "/", "") + "/";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

	private CreatePanePage createPanePage;
	private AccountsPanePage accountsPanePage;
	private MainWindowPage mainWindowPage;

	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	public UserAccessibleProperties properties;

	@Before
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
					new File(DEFAULT_STORAGE + MNEMONIC_PATH));

			final Map<String, String> emailMap = new HashMap<>();

			emailMap.put(
					currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
					"test1.council2@hederacouncil.org");


			final var objectMapper = new ObjectMapper();
			final var mapAsString = objectMapper.writeValueAsString(emailMap);
			assertNotNull(mapAsString);

			properties.setOneDriveCredentials(emailMap);
			properties.setHash("123456789".toCharArray());

			properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
			//setupTransactionDirectory(DEFAULT_STORAGE);

			final var controller = new Controller();
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
			if (!new File(rootFolder).exists() && new File(rootFolder).mkdirs()) {
				logger.info("Tools root folder created");
			} else {
				logger.info("Tools root directory exists.");
			}
			final var toolsFolder =
					new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents";
			if (!new File(toolsFolder).exists() && new File(toolsFolder).mkdirs()) {
				logger.info("Tools document directory created");
			} else {
				logger.info("Tools document directory exists.");
			}

			final var outputDirectory = new File(
					"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org");
			FileUtils.cleanDirectory(outputDirectory);

			TestUtil.copyCreatePaneKeys();

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
			final var transactions = new File(
					"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
					pathname -> {
						final var name = pathname.getName();
						return name.endsWith(Constants.TXT_EXTENSION) ||
								name.endsWith(Constants.TRANSACTION_EXTENSION) ||
								name.endsWith(Constants.SIGNED_TRANSACTION_EXTENSION);
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
	public void loadTransferAccount_test() throws HederaClientException {
		createPanePage.loadTransaction("src/test/resources/createTransactions/transfer.tx");
		createPanePage.createAndExport(resources);


		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		final var original =
				new ToolTransferTransaction(new File("src/test/resources/createTransactions/transfer.tx"));
		ToolTransferTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("0_0_99")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolTransferTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}

		logger.info(comment);

		assertNotNull(toolTransaction);
		assertEquals(original.getTransactionId().accountId, toolTransaction.getTransactionId().accountId);

		assertEquals(Objects.requireNonNull(original.getTransactionId().validStart).getEpochSecond(),
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals(original.getMemo(), toolTransaction.getMemo());

		final var originalTransferMap = original.getAccountAmountMap();
		final var transferMap = toolTransaction.getAccountAmountMap();

		assert original.getTransaction().getMaxTransactionFee() != null;
		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(original.getTransaction().getMaxTransactionFee().toTinybars(),
				toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());
		assertEquals(originalTransferMap.size(), transferMap.size());
	}

	@Test
	public void loadTransferAccountMakeChanges_test() throws HederaClientException {
		createPanePage.loadTransaction("src/test/resources/createTransactions/transfer.tx")
				.setFeePayerAccount(10006);
		createPanePage.createAndExport(resources);


		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		final var original =
				new ToolTransferTransaction(new File("src/test/resources/createTransactions/transfer.tx"));
		ToolTransferTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("0_0_10006")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolTransferTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}

		logger.info(comment);

		assertNotNull(toolTransaction);
		assertEquals(Identifier.parse("10006").asAccount(), toolTransaction.getTransactionId().accountId);

		assertEquals(Objects.requireNonNull(original.getTransactionId().validStart).getEpochSecond(),
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals(original.getMemo(), toolTransaction.getMemo());

		final var originalTransferMap = original.getAccountAmountMap();
		final var transferMap = toolTransaction.getAccountAmountMap();

		assert original.getTransaction().getMaxTransactionFee() != null;
		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(original.getTransaction().getMaxTransactionFee().toTinybars(),
				toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());
		assertEquals(originalTransferMap.size(), transferMap.size());
	}


	@Test
	public void loadCreateAccount_test() throws HederaClientException {
		createPanePage.loadTransaction("src/test/resources/createTransactions/createAccount.tx");
		createPanePage.createAndExport(resources);

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		final var original =
				new ToolCryptoCreateTransaction(new File("src/test/resources/createTransactions/createAccount.tx"));
		ToolCryptoCreateTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("0_0_8988")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolCryptoCreateTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}
		logger.info(comment);

		assertNotNull(toolTransaction);
		assertEquals(original.getTransactionId().accountId, toolTransaction.getTransactionId().accountId);

		assertEquals(Objects.requireNonNull(original.getTransactionId().validStart).getEpochSecond(),
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals(original.getMemo(), toolTransaction.getMemo());

		final var originalKey = original.getKey();
		final var transferKey = toolTransaction.getKey();

		assert original.getTransaction().getMaxTransactionFee() != null;
		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(original.getTransaction().getMaxTransactionFee().toTinybars(),
				toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());
		assertEquals(originalKey.size(), transferKey.size());
	}

	@Test
	public void loadUpdateAccount_test() throws HederaClientException {
		createPanePage.loadTransaction("src/test/resources/createTransactions/accountUpdate.tx");
		createPanePage.createAndExport(resources);

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		final var original =
				new ToolCryptoUpdateTransaction(new File("src/test/resources/createTransactions/accountUpdate.tx"));
		ToolCryptoUpdateTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("0_0_89")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolCryptoUpdateTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}
		logger.info(comment);

		assertNotNull(toolTransaction);

		assertEquals(original.getAccount().asAccount(), toolTransaction.getAccount().asAccount());

		assertEquals(original.getTransactionId().accountId, toolTransaction.getTransactionId().accountId);

		assertEquals(Objects.requireNonNull(original.getTransactionId().validStart).getEpochSecond(),
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals(original.getMemo(), toolTransaction.getMemo());

		final var originalKey = original.getKey();
		final var transferKey = toolTransaction.getKey();

		assert original.getTransaction().getMaxTransactionFee() != null;
		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(original.getTransaction().getMaxTransactionFee().toTinybars(),
				toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());
		assertEquals(originalKey.size(), transferKey.size());
	}

	@Test
	public void loadUpdateAccountMakeChanges_test() throws HederaClientException {
		createPanePage.loadTransaction("src/test/resources/createTransactions/accountUpdate.tx")
				.setMemo("A memo")
				.setFeePayerAccount(1009);

		createPanePage.createAndExport(resources);

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		final var original =
				new ToolCryptoUpdateTransaction(new File("src/test/resources/createTransactions/accountUpdate.tx"));
		ToolCryptoUpdateTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("0_0_1009")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolCryptoUpdateTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}
		logger.info(comment);

		assertNotNull(toolTransaction);

		assertEquals(original.getAccount().asAccount(), toolTransaction.getAccount().asAccount());

		assertEquals(Identifier.parse("1009").asAccount(), toolTransaction.getTransactionId().accountId);

		assertEquals(Objects.requireNonNull(original.getTransactionId().validStart).getEpochSecond(),
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A memo", toolTransaction.getMemo());

		final var originalKey = original.getKey();
		final var transferKey = toolTransaction.getKey();

		assert original.getTransaction().getMaxTransactionFee() != null;
		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(original.getTransaction().getMaxTransactionFee().toTinybars(),
				toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());
		assertEquals(originalKey.size(), transferKey.size());

	}

	@Test
	public void loadUpdateAccountChangeAccount_test() throws HederaClientException {
		createPanePage.loadTransaction("src/test/resources/createTransactions/accountUpdate.tx")
				.setUpdateAccount(10006);

		createPanePage.createAndExport(resources);

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		final var original =
				new ToolCryptoUpdateTransaction(new File("src/test/resources/createTransactions/accountUpdate.tx"));
		ToolCryptoUpdateTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("0_0_89")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolCryptoUpdateTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}
		logger.info(comment);

		assertNotNull(toolTransaction);

		assertEquals(Identifier.parse("10006").asAccount(), toolTransaction.getAccount().asAccount());

		assertEquals(original.getTransactionId().accountId, toolTransaction.getTransactionId().accountId);

		assertEquals(Objects.requireNonNull(original.getTransactionId().validStart).getEpochSecond(),
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals(original.getMemo(), toolTransaction.getMemo());

		final var originalKey = original.getKey();
		final var transferKey = toolTransaction.getKey();

		assert original.getTransaction().getMaxTransactionFee() != null;
		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(original.getTransaction().getMaxTransactionFee().toTinybars(),
				toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());
		assertEquals(originalKey.size(), transferKey.size());
	}

	@Test
	public void loadSystemTransaction_test() throws HederaClientException {
		createPanePage.loadTransaction("src/test/resources/createTransactions/modifyContent.tx");
		createPanePage.createAndExport(resources);
		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		final var original =
				new ToolSystemTransaction(new File("src/test/resources/createTransactions/modifyContent.tx"));
		ToolSystemTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("0_0_878")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolSystemTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}
		logger.info(comment);

		assertNotNull(toolTransaction);

		assertEquals(original.getEntity().asAccount(), toolTransaction.getEntity().asAccount());

		assertEquals(original.getTransactionId().accountId, toolTransaction.getTransactionId().accountId);

		assertEquals(Objects.requireNonNull(original.getTransactionId().validStart).getEpochSecond(),
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals(original.getMemo(), toolTransaction.getMemo());


		assert original.getTransaction().getMaxTransactionFee() != null;
		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(original.getTransaction().getMaxTransactionFee().toTinybars(),
				toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());
		assertTrue(original.isDelete() && toolTransaction.isDelete());
		assertTrue(original.isFile() && toolTransaction.isFile());
	}

	@Test
	public void largeFileUpdate_test() {
		createPanePage.loadTransaction("src/test/resources/createTransactions/largeFileUpdate.lfu");
		createPanePage.createAndExport(resources);
		var n = find("#updateFileID");
		assertTrue(n instanceof TextField);
		assertTrue(((TextField) n).getText().contains("0.0.15225"));
		n = find("#feePayerAccountField");
		assertTrue(n instanceof TextField);
		assertTrue(((TextField) n).getText().contains("0.0.56"));
		n = find("#nodeAccountField");
		assertTrue(n instanceof TextField);
		assertTrue(((TextField) n).getText().contains("0.0.3"));
		n = find("#chunkSizeTextField");
		assertTrue(n instanceof TextField);
		assertEquals("1024", ((TextField) n).getText());
		n = find("#intervalTextField");
		assertTrue(n instanceof TextField);
		assertEquals("100", ((TextField) n).getText());
		n = find("#transactionFee");
		assertTrue(n instanceof TextField);
		assertEquals("1.90 000 000", ((TextField) n).getText());
	}

}

