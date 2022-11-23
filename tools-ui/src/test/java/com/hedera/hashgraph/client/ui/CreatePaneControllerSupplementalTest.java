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
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.ui.pages.AccountsPanePage;
import com.hedera.hashgraph.client.ui.pages.CreatePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountUpdateTransaction;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeView;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import javax.swing.JFileChooser;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.toEncryptedFile;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_LOCAL_TIME_LABEL;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_NODE_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_UPDATE_ORIGINAL_KEY;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class CreatePaneControllerSupplementalTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(CreatePaneControllerTest.class);

	public static final String CLOUD_OUTPUT_DIRECTORY =
			"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org";
	public static final String LOREM_IPSUM =
			"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et" +
					" dolore magna aliqua.";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

	private static final List<String> TEST_WORDS =
			Arrays.asList("dignity", "domain", "involve", "report",
					"sail", "middle", "rhythm", "husband",
					"usage", "pretty", "rate", "town",
					"account", "side", "extra", "outer",
					"eagle", "eight", "design", "page",
					"regular", "bird", "race", "answer");

	private CreatePanePage createPanePage;
	private AccountsPanePage accountsPanePage;
	private MainWindowPage mainWindowPage;

	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	public UserAccessibleProperties properties;

	private final String resources = new File("src/test/resources/Transactions - Documents/").getAbsolutePath().replace(
			System.getProperty("user.home") + "/", "") + "/";


	@Before
	public void setUp() {
		try {
			System.gc();
			logger.info("Starting test class: {}", getClass().getSimpleName());
			TestUtil.buildFolders();

			properties = new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");

			if (new File(currentRelativePath.toAbsolutePath() + File.separator + CLOUD_OUTPUT_DIRECTORY).mkdirs()) {
				logger.info("Output path created");
			}

			FileUtils.cleanDirectory(new File(CLOUD_OUTPUT_DIRECTORY));

			remakeTransactionTools();

			//Copy Accounts folders
			FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original_II/Accounts"),
					new File(DEFAULT_STORAGE + "/Accounts"));
			FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original_II/Files"),
					new File(DEFAULT_STORAGE + "/Files"));

			properties.setSetupPhase(SetupPhase.TEST_PHASE);

			FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
					new File(DEFAULT_STORAGE + MNEMONIC_PATH));

			final Map<String, String> emailMap = new HashMap<>();

			emailMap.put(
					currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
					"test1.council2@hederacouncil.org");

			final var mnemonic = Mnemonic.fromWords(TEST_WORDS);
			properties.setMnemonicHashCode(mnemonic.words.hashCode());
			properties.setHash(TEST_PASSWORD.toCharArray());
			properties.setLegacy(false);
			final var salt = Utilities.getSaltBytes(properties);
			final var passwordBytes = SecurityUtilities.keyFromPassword(TEST_PASSWORD.toCharArray(), salt);
			toEncryptedFile(passwordBytes, Constants.DEFAULT_STORAGE + File.separator + Constants.MNEMONIC_PATH,
					mnemonic.toString());

			TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

			final var objectMapper = new ObjectMapper();
			final var mapAsString = objectMapper.writeValueAsString(emailMap);
			assertNotNull(mapAsString);

			properties.setOneDriveCredentials(emailMap);
			properties.setHash("123456789".toCharArray());

			properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
			//setupTransactionDirectory(DEFAULT_STORAGE);

			final var controller = new MainController();
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
					CLOUD_OUTPUT_DIRECTORY);
			FileUtils.cleanDirectory(outputDirectory);

			TestUtil.copyCreatePaneKeys();

			mainWindowPage.clickOnCreateButton();
		} catch (final Exception e) {
			logger.error(e);
			assertNotNull(e);
		}

	}

	@After
	public void tearDown() throws IOException, TimeoutException {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

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
		for (final var f : transactions) {
			if (f.delete()) {
				logger.debug(String.format("%s has been deleted", f.getName()));
			}
		}
		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}

	@Test
	public void updateAccountMemoTokens_test() throws HederaClientException {
		final var date = DateUtils.addDays(new Date(), 2);
		final var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		final var sdf = new SimpleDateFormat("yyyy-MM-dd");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		find(CREATE_LOCAL_TIME_LABEL);


		final var dtf = DateTimeFormatter.ofPattern("MM/dd/yyyy");
		final var localDateTime = LocalDateTime.of(LocalDate.parse(datePickerFormat.format(date), dtf),
				LocalTime.of(2, 30, 45));

		final var transactionValidStart = Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString())
				.setComment("this is a comment that will go with the update transaction")
				.setUpdateAccount(73);

		final var oldKey = find(CREATE_UPDATE_ORIGINAL_KEY);
		assertTrue(oldKey.isVisible());
		assertTrue(oldKey instanceof ScrollPane);
		final var tree = ((ScrollPane) oldKey).getContent();

		assertTrue(tree instanceof TreeView);
		assertEquals(6, TestUtil.countTreeNodes(((TreeView<String>) tree).getRoot()));

		createPanePage.setDate(datePickerFormat.format(date))
				.setHours(2)
				.setMinutes(30)
				.setSeconds(45)
				.setMemo("A memo")
				.setFeePayerAccount(1019)
				.setNodeAccount(4)
				.setNewAccountMemo("Account memo test")
				.setNewMaxTokenAss(250);

		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources);

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		ToolCryptoUpdateTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("1019")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolCryptoUpdateTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}

		assertNotNull(toolTransaction);

		assertEquals(new Identifier(0, 0, 4), toolTransaction.getNodeID());
		assertEquals("A memo", toolTransaction.getMemo());

		assertEquals(new Identifier(0, 0, 1019).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A memo", toolTransaction.getMemo());


		assertTrue(toolTransaction.getTransaction() instanceof AccountUpdateTransaction);
		final var accountUpdateTransaction = (AccountUpdateTransaction) toolTransaction.getTransaction();
		assertNotNull(accountUpdateTransaction.getAccountMemo());
		assertEquals("Account memo test", accountUpdateTransaction.getAccountMemo());
		assertNotNull(accountUpdateTransaction.getMaxAutomaticTokenAssociations());
		assertEquals(250, (int) accountUpdateTransaction.getMaxAutomaticTokenAssociations());


		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the update transaction",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));

	}

	@Test
	public void charLimits_test() {
		final var date = DateUtils.addDays(new Date(), 2);
		final var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		final var sdf = new SimpleDateFormat("yyyy-MM-dd");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		find(CREATE_LOCAL_TIME_LABEL);

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString())
				.setComment("this is a comment that will go with the update transaction")
				.setUpdateAccount(73);

		final var oldKey = find(CREATE_UPDATE_ORIGINAL_KEY);
		assertTrue(oldKey.isVisible());
		assertTrue(oldKey instanceof ScrollPane);
		final var tree = ((ScrollPane) oldKey).getContent();

		assertTrue(tree instanceof TreeView);
		assertEquals(6, TestUtil.countTreeNodes(((TreeView<String>) tree).getRoot()));

		createPanePage.setDate(datePickerFormat.format(date))
				.setHours(2)
				.setMinutes(30)
				.setSeconds(45)
				.setMemo("A memo")
				.setFeePayerAccount(1019)
				.setNodeAccount(42)
				.setNewAccountMemo("Account memo test");

		var bytesLabel = find("#updateBytesRemaining");
		assertTrue(bytesLabel instanceof Label);

		assertEquals("Bytes remaining:\t83", ((Label) bytesLabel).getText());

		createPanePage.setNewAccountMemo(LOREM_IPSUM);
		bytesLabel = find("#updateBytesRemaining");
		assertEquals("Bytes remaining:\t0", ((Label) bytesLabel).getText());

		final var text = find("#updateAccountMemoNew");
		assertTrue(text instanceof TextField);
		assertEquals(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore ",
				((TextField) text).getText());

		final var tokensText = find("#updateMaxTokensNew");
		assertTrue(tokensText instanceof TextField);
		createPanePage.setNewMaxTokenAss(1000);
		assertEquals("1000", ((TextField) tokensText).getText());

		final var errorLabel = find("#updateMTAerrorLabel");
		assertTrue(errorLabel instanceof Label);
		assertFalse(errorLabel.isVisible());

		createPanePage.setNewMaxTokenAss(1001);
		assertEquals("100", ((TextField) tokensText).getText());
		assertTrue(errorLabel.isVisible());

		assertEquals("Maximum number of associations cannot exceed 1000", ((Label) errorLabel).getText());

	}

	@Test
	public void createAccount_Test() throws HederaClientException {
		final var date = DateUtils.addDays(new Date(), 2);
		final var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		final var sdf = new SimpleDateFormat("yyyy-MM-dd");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		final Label dateLabel = find(CREATE_LOCAL_TIME_LABEL);

		final var dtf = DateTimeFormatter.ofPattern("MM/dd/yyyy");
		final var localDateTime = LocalDateTime.of(LocalDate.parse(datePickerFormat.format(date), dtf),
				LocalTime.of(2, 30, 45));

		final var transactionValidStart = Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setComment("this is a comment that will go with the transaction")
				.setDate(datePickerFormat.format(date))
				.setNanos("123456789");

		assertTrue(find(CREATE_LOCAL_TIME_LABEL).isVisible());
		logger.info("CREATE: Local date label =>>> {}", dateLabel.getText());
		assertTrue(dateLabel.getText().contains(":00:00"));

		createPanePage.setHours(2)
				.setMinutes(30)
				.setSeconds(45);

		assertTrue(find(CREATE_LOCAL_TIME_LABEL).isVisible());
		assertTrue(dateLabel.getText().contains(":30:45"));

		createPanePage.setMemo("A memo")
				.setFeePayerAccount(1009)
				.setNodeAccount(42)
				.setTransactionFee(1.2345678911)
				.setInitialBalance(1000)
				.setCreateKey()
				.doubleClickOnAccountKey("treasury")
				.saveKey()
				.setAccountMemo(LOREM_IPSUM)
				.setTokenAssociations(666);


		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources);

		assertTrue(find("#invalidNode").isVisible());
		ensureVisible(find(CREATE_NODE_FIELD));
		sleep(3000);
		ensureVisible(find(CREATE_NODE_FIELD));
		createPanePage.setNodeAccount(4)
				.createAndExport(resources);

		assertFalse(find("#invalidNode").isVisible());


		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		ToolCryptoCreateTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("1009")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolCryptoCreateTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}

		assertNotNull(toolTransaction);

		assertEquals(new Identifier(0, 0, 4), toolTransaction.getNodeID());
		assertEquals("A memo", toolTransaction.getMemo());

		assertTrue(toolTransaction.getTransaction() instanceof AccountCreateTransaction);


		assertEquals(100000000000L, toolTransaction.getInitialBalance().toTinybars());

		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(123456789, toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());

		assertEquals(new Identifier(0, 0, 1009).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());

		assertEquals(123456789, toolTransaction.getTransactionId().validStart.getNano());
		assertEquals("A memo", toolTransaction.getMemo());

		assertTrue(toolTransaction.getTransaction() instanceof AccountCreateTransaction);
		final var create = (AccountCreateTransaction) toolTransaction.getTransaction();

		assertEquals(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore ",
				create.getAccountMemo());
		assertEquals(666, create.getMaxAutomaticTokenAssociations());


		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the transaction",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));
	}

	@Test
	public void createAccountCounters_Test() {
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setAccountMemo("Account memo test");

		var bytesLabel = find("#createMemoByteCount");
		assertTrue(bytesLabel instanceof Label);

		assertEquals("Bytes remaining:\t83", ((Label) bytesLabel).getText());

		createPanePage.setAccountMemo(LOREM_IPSUM);
		bytesLabel = find("#createMemoByteCount");
		assertEquals("Bytes remaining:\t0", ((Label) bytesLabel).getText());

		final var text = find("#createAccountMemo");
		assertTrue(text instanceof TextField);
		assertEquals(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore ",
				((TextField) text).getText());

		final var tokensText = find("#createMaxTokenAssociations");
		assertTrue(tokensText instanceof TextField);
		createPanePage.setTokenAssociations(1000);
		assertEquals("1000", ((TextField) tokensText).getText());

		final var errorLabel = find("#createMTAErrorLabel");
		assertTrue(errorLabel instanceof Label);
		assertFalse(errorLabel.isVisible());

		createPanePage.setTokenAssociations(1001);
		assertEquals("100", ((TextField) tokensText).getText());
		assertTrue(errorLabel.isVisible());

		assertEquals("Maximum number of associations cannot exceed 1000", ((Label) errorLabel).getText());
	}


}
