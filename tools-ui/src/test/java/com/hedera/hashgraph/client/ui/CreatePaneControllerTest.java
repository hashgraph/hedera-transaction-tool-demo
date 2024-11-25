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
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolSystemTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.client.ui.pages.AccountsPanePage;
import com.hedera.hashgraph.client.ui.pages.CreatePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountUpdateTransaction;
import com.hedera.hashgraph.sdk.FreezeTransaction;
import com.hedera.hashgraph.sdk.FreezeType;
import com.hedera.hashgraph.sdk.Mnemonic;
import com.hedera.hashgraph.sdk.SystemDeleteTransaction;
import com.hedera.hashgraph.sdk.SystemUndeleteTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeView;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;
import org.jetbrains.annotations.NotNull;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.testfx.api.FxToolkit;

import javax.swing.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;

import static com.hedera.hashgraph.client.core.constants.Constants.CONTENT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.toEncryptedFile;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_ANCHOR_PANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_AUTO_RENEW;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_COMMENTS_AREA;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_COMMENTS_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_COMMON_FIELDS_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_CREATE_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_CREATE_KEY;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_DATE_PICKER;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_FEE_PAYER_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_FROM_TABLE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_HOURS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INITIAL_BALANCE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_CREATE_AUTO_RENEW;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_CREATE_INITIAL_BALANCE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_CREATE_NEW_KEY;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_DATE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_FEE_PAYER;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_NODE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_TRANSFER_FROM_ACCOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_TRANSFER_LIST;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_TRANSFER_TOTAL;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_TRANSFER_TO_ACCOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_UPDATE_ACCOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_UPDATE_AUTO_RENEW;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INVALID_UPDATE_NEW_KEY;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_LOCAL_TIME_LABEL;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_MAIN_CHOICE_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_MEMO_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_MINUTES;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_NANOS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_NODE_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_SECONDS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_SYSTEM_EXPIRATION_VBOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TO_TABLE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSACTION_FEE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_FROM_ACCOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_FROM_AMOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_TOTAL_LABEL;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_TO_ACCOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_TO_AMOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_UPDATE_ACCOUNT_ID;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_UPDATE_ARPO_O;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_UPDATE_AUTO_RENEW;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_UPDATE_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_UPDATE_NEW_KEY;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_UPDATE_ORIGINAL_KEY;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SET_NOW_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SYSTEM_ENTITY_ID_LABEL;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.OperationType.delete;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.OperationType.undelete;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.SystemEntity.contract;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.SystemEntity.file;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

@Disabled("FxRobot issues in headless mode using JUnit 5")
public class CreatePaneControllerTest extends TestBase implements Supplier<TestBase>, GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(CreatePaneControllerTest.class);

	private static final long THREAD_PAUSE_TIME = 1000;
	private static final int TENTH_OF_A_SECOND = 100;
	private static final int AUTO_RENEW_DEFAULT = 7000013;
	private static final String CLOUD_OUTPUT_DIRECTORY =
			"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org";
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final List<String> TEST_WORDS =
			Arrays.asList("dignity", "domain", "involve", "report",
					"sail", "middle", "rhythm", "husband",
					"usage", "pretty", "rate", "town",
					"account", "side", "extra", "outer",
					"eagle", "eight", "design", "page",
					"regular", "bird", "race", "answer");

	private final String resources = new File("src/test/resources/Transactions - Documents/").getAbsolutePath().replace(
			System.getProperty("user.home") + "/", "") + "/";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

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
				mainWindowPage = new MainWindowPage(get());
			}
			if (accountsPanePage == null) {
				accountsPanePage = new AccountsPanePage(get());
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

	@Test
	public void openCreatePageTab_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		assertNotNull(find(CREATE_ANCHOR_PANE));
		assertNotNull(find(CREATE_MAIN_CHOICE_BOX));
		sleep(THREAD_PAUSE_TIME);
	}

	@Test
	public void transactionHeader1_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString());
		assertNotNull(find(CREATE_MEMO_FIELD));
		assertNotNull(find(CREATE_NODE_FIELD));
		assertNotNull(find(CREATE_TRANSACTION_FEE));
		assertNotNull(find(CREATE_FEE_PAYER_FIELD));
		assertNotNull(find(CREATE_DATE_PICKER));
		assertNotNull(find(CREATE_HOURS));
		assertNotNull(find(CREATE_MINUTES));
		assertNotNull(find(CREATE_SECONDS));
		assertNotNull(find(CREATE_COMMENTS_AREA));
		assertTrue(find(CREATE_MEMO_FIELD).isVisible());
		assertTrue(find(CREATE_NODE_FIELD).isVisible());
		assertTrue(find(CREATE_TRANSACTION_FEE).isVisible());
		assertTrue(find(CREATE_FEE_PAYER_FIELD).isVisible());
		assertTrue(find(CREATE_DATE_PICKER).isVisible());
		assertTrue(find(CREATE_HOURS).isVisible());
		assertTrue(find(CREATE_MINUTES).isVisible());
		assertTrue(find(CREATE_SECONDS).isVisible());
		assertTrue(find(CREATE_COMMENTS_AREA).isVisible());
		sleep(THREAD_PAUSE_TIME);
	}

	@Test
	public void transactionHeader2_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString());
		assertNotNull(find(CREATE_MEMO_FIELD));
		assertNotNull(find(CREATE_NODE_FIELD));
		assertNotNull(find(CREATE_TRANSACTION_FEE));
		assertNotNull(find(CREATE_FEE_PAYER_FIELD));
		assertNotNull(find(CREATE_DATE_PICKER));
		assertNotNull(find(CREATE_HOURS));
		assertNotNull(find(CREATE_MINUTES));
		assertNotNull(find(CREATE_SECONDS));
		assertNotNull(find(CREATE_COMMENTS_AREA));
		assertTrue(find(CREATE_MEMO_FIELD).isVisible());
		assertTrue(find(CREATE_NODE_FIELD).isVisible());
		assertTrue(find(CREATE_TRANSACTION_FEE).isVisible());
		assertTrue(find(CREATE_FEE_PAYER_FIELD).isVisible());
		assertTrue(find(CREATE_DATE_PICKER).isVisible());
		assertTrue(find(CREATE_HOURS).isVisible());
		assertTrue(find(CREATE_MINUTES).isVisible());
		assertTrue(find(CREATE_SECONDS).isVisible());
		assertTrue(find(CREATE_COMMENTS_AREA).isVisible());
		sleep(THREAD_PAUSE_TIME);
	}

	@Test
	public void transactionHeader3_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		try {
			createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString());
			assertNotNull(find(CREATE_MEMO_FIELD));
			assertNotNull(find(CREATE_NODE_FIELD));
			assertNotNull(find(CREATE_TRANSACTION_FEE));
			assertNotNull(find(CREATE_FEE_PAYER_FIELD));
			assertNotNull(find(CREATE_DATE_PICKER));
			assertNotNull(find(CREATE_HOURS));
			assertNotNull(find(CREATE_MINUTES));
			assertNotNull(find(CREATE_SECONDS));
			assertNotNull(find(CREATE_COMMENTS_AREA));
			assertTrue(find(CREATE_MEMO_FIELD).isVisible());
			assertTrue(find(CREATE_NODE_FIELD).isVisible());
			assertTrue(find(CREATE_TRANSACTION_FEE).isVisible());
			assertTrue(find(CREATE_FEE_PAYER_FIELD).isVisible());
			assertTrue(find(CREATE_DATE_PICKER).isVisible());
			assertTrue(find(CREATE_HOURS).isVisible());
			assertTrue(find(CREATE_MINUTES).isVisible());
			assertTrue(find(CREATE_SECONDS).isVisible());
			assertTrue(find(CREATE_COMMENTS_AREA).isVisible());
			sleep(THREAD_PAUSE_TIME);
		} catch (final Exception e) {
			logger.error(e);
			assertNotNull(e);
		}
	}

	@Test
	public void exclusiveFields1_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString());
		assertTrue(find(CREATE_COMMENTS_BOX).isVisible());

		assertTrue(find(CREATE_INITIAL_BALANCE).isVisible());
		assertTrue(find(CREATE_COMMON_FIELDS_BOX).isVisible());
		assertTrue(find(CREATE_CREATE_BOX).isVisible());
		assertFalse(find(CREATE_UPDATE_BOX).isVisible());
		assertFalse(find(CREATE_TRANSFER_BOX).isVisible());

		assertTrue(find(CREATE_AUTO_RENEW).isVisible());
		assertFalse(find(CREATE_CREATE_KEY).isVisible());

		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_FEE_PAYER).isVisible());
		assertFalse(find(CREATE_INVALID_NODE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_INITIAL_BALANCE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_AUTO_RENEW).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_AUTO_RENEW).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_FROM_ACCOUNT).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_TO_ACCOUNT).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_LIST).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_TOTAL).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_ACCOUNT).isVisible());
		sleep(THREAD_PAUSE_TIME);
	}

	@Test
	public void exclusiveFields2_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString());
		assertTrue(find(CREATE_COMMENTS_BOX).isVisible());

		assertTrue(find(CREATE_INITIAL_BALANCE).isVisible());
		assertTrue(find(CREATE_COMMON_FIELDS_BOX).isVisible());
		assertFalse(find(CREATE_CREATE_BOX).isVisible());
		assertTrue(find(CREATE_UPDATE_BOX).isVisible());
		assertFalse(find(CREATE_TRANSFER_BOX).isVisible());

		assertTrue(find(CREATE_UPDATE_ACCOUNT_ID).isVisible());
		assertTrue(find(CREATE_UPDATE_AUTO_RENEW).isVisible());
		assertTrue(find(CREATE_UPDATE_ARPO_O).isVisible());
		assertTrue(find(CREATE_UPDATE_ORIGINAL_KEY).isVisible());
		assertFalse(find(CREATE_UPDATE_NEW_KEY).isVisible());

		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_FEE_PAYER).isVisible());
		assertFalse(find(CREATE_INVALID_NODE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_INITIAL_BALANCE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_AUTO_RENEW).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_AUTO_RENEW).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_FROM_ACCOUNT).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_TO_ACCOUNT).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_LIST).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_TOTAL).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_ACCOUNT).isVisible());
		sleep(THREAD_PAUSE_TIME);
	}

	@Test
	public void exclusiveFields3_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString());
		assertTrue(find(CREATE_COMMENTS_BOX).isVisible());

		assertTrue(find(CREATE_INITIAL_BALANCE).isVisible());
		assertTrue(find(CREATE_COMMON_FIELDS_BOX).isVisible());
		assertFalse(find(CREATE_CREATE_BOX).isVisible());
		assertFalse(find(CREATE_UPDATE_BOX).isVisible());
		assertTrue(find(CREATE_TRANSFER_BOX).isVisible());

		assertTrue(find(CREATE_TRANSFER_TO_ACCOUNT).isVisible());
		assertTrue(find(CREATE_TRANSFER_TO_AMOUNT).isVisible());
		assertTrue(find(CREATE_TRANSFER_FROM_ACCOUNT).isVisible());
		assertTrue(find(CREATE_TRANSFER_FROM_AMOUNT).isVisible());
		assertTrue(find(CREATE_FROM_TABLE).isVisible());
		assertTrue(find(CREATE_TO_TABLE).isVisible());

		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_FEE_PAYER).isVisible());
		assertFalse(find(CREATE_INVALID_NODE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_INITIAL_BALANCE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_AUTO_RENEW).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_AUTO_RENEW).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_FROM_ACCOUNT).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_TO_ACCOUNT).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_LIST).isVisible());
		assertFalse(find(CREATE_INVALID_TRANSFER_TOTAL).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_ACCOUNT).isVisible());

		sleep(THREAD_PAUSE_TIME);
	}

	@Test
	public void checkTimeDate_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var date = DateUtils.addDays(new Date(), 2);
		final var sdf = new SimpleDateFormat("MM/dd/yyyy");


		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setDate(sdf.format(date))
				.setHours(31);
		sleep(TENTH_OF_A_SECOND);
		assertEquals(23, Integer.parseInt(((TextField) find(CREATE_HOURS)).getText()));

		createPanePage.setHours(-12);
		sleep(TENTH_OF_A_SECOND);
		assertEquals(12, Integer.parseInt(((TextField) find(CREATE_HOURS)).getText()));

		createPanePage.setHours(99);
		sleep(TENTH_OF_A_SECOND);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(23, Integer.parseInt(((TextField) find(CREATE_HOURS)).getText()));

		createPanePage.setHours(-12);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(12, Integer.parseInt(((TextField) find(CREATE_HOURS)).getText()));

		createPanePage.setHours(9);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(9, Integer.parseInt(((TextField) find(CREATE_HOURS)).getText()));

		createPanePage.setMinutes(99);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(59, Integer.parseInt(((TextField) find(CREATE_MINUTES)).getText()));

		createPanePage.setMinutes(-12);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(12, Integer.parseInt(((TextField) find(CREATE_MINUTES)).getText()));

		createPanePage.setMinutes(9);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(9, Integer.parseInt(((TextField) find(CREATE_MINUTES)).getText()));

		createPanePage.setSeconds(99);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(59, Integer.parseInt(((TextField) find(CREATE_SECONDS)).getText()));

		createPanePage.setSeconds(-12);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(12, Integer.parseInt(((TextField) find(CREATE_SECONDS)).getText()));

		createPanePage.setSeconds(9);
		sleep(THREAD_PAUSE_TIME);
		assertEquals(9, Integer.parseInt(((TextField) find(CREATE_SECONDS)).getText()));
	}

	@Test
	public void useTheNowButton_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString());

		var localDateTime = LocalDateTime.now();
		while (localDateTime.getSecond() < 5 || localDateTime.getSecond() > 50) {
			localDateTime = LocalDateTime.now();
		}

		createPanePage.clickOnNowButton();

		assertEquals(localDateTime.getDayOfMonth(),
				((DatePicker) find(CREATE_DATE_PICKER)).getValue().getDayOfMonth());
		assertEquals(localDateTime.getMonth(), ((DatePicker) find(CREATE_DATE_PICKER)).getValue().getMonth());
		assertEquals(localDateTime.getYear(), ((DatePicker) find(CREATE_DATE_PICKER)).getValue().getYear());

		final var textFieldHours = (TextField) find(CREATE_HOURS);
		final var textFieldMinutes = (TextField) find(CREATE_MINUTES);
		final var textFieldSeconds = (TextField) find(CREATE_SECONDS);
		final var textFieldNanos = (TextField) find(CREATE_NANOS);

		assertEquals(localDateTime.getHour(), Long.parseLong(textFieldHours.getText()));
		assertEquals(localDateTime.getMinute(), Long.parseLong(textFieldMinutes.getText()));
		assertTrue(Math.abs(localDateTime.getSecond() - Long.parseLong(textFieldSeconds.getText())) <= 2);
		assertTrue(Long.parseLong(textFieldNanos.getText()) > 0);
	}

	@Test
	public void useTheNowButtonErrorMessages_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString());

		var localDateTime = LocalDateTime.now();
		while (localDateTime.getSecond() < 5 || localDateTime.getSecond() > 50) {
			localDateTime = LocalDateTime.now();
		}

		createPanePage.clickOnNowButton();

		final var dateUTC = (Label) find("#createUTCTimeLabel");
		final var errorMsg = (Label) find("#invalidDate");

		assertTrue(dateUTC.isVisible());
		assertTrue(dateUTC.getStyle().toLowerCase(Locale.ROOT).contains("red"));
		assertTrue(errorMsg.isVisible());

		final var futureTime = localDateTime.plusYears(1);
		final var formattedDate = getFormattedDate(futureTime);
		createPanePage.setDate(formattedDate);
		assertTrue(dateUTC.isVisible());
		assertFalse(dateUTC.getStyle().toLowerCase(Locale.ROOT).contains("red"));
		assertFalse(errorMsg.isVisible());

		clickOn(SET_NOW_BUTTON);
		assertTrue(dateUTC.isVisible());
		assertTrue(dateUTC.getStyle().toLowerCase(Locale.ROOT).contains("red"));
		assertTrue(errorMsg.isVisible());

	}

	@NotNull
	private String getFormattedDate(final LocalDateTime localDateTime) {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		return datePickerFormat.format(Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant()));
	}

	@Test
	public void changeTimeZone() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.clickOnNowButton();
		final var localDateTime = LocalDateTime.now();
		final var localZoneID = ZoneId.of(TimeZone.getDefault().getID());

		assertEquals(localDateTime.getDayOfMonth(),
				((DatePicker) find(CREATE_DATE_PICKER)).getValue().getDayOfMonth());
		assertEquals(localDateTime.getMonth(), ((DatePicker) find(CREATE_DATE_PICKER)).getValue().getMonth());
		assertEquals(localDateTime.getYear(), ((DatePicker) find(CREATE_DATE_PICKER)).getValue().getYear());

		assertEquals(localDateTime.getHour(), Long.parseLong(((TextField) find(CREATE_HOURS)).getText()));
		assertEquals(localDateTime.getMinute(), Long.parseLong(((TextField) find(CREATE_MINUTES)).getText()));
		assertTrue(
				Math.abs(localDateTime.getSecond() - Long.parseLong(
						((TextField) find(CREATE_SECONDS)).getText())) <= 2);
		final var utcString = getUTCString(localDateTime, localZoneID);


		final var splitUTC = utcString.split(" ");
		final var splitTime = splitUTC[1].split(":");

		assertTrue(((Label) find(CREATE_LOCAL_TIME_LABEL)).getText().contains(splitUTC[0]));
		assertTrue(((Label) find(CREATE_LOCAL_TIME_LABEL)).getText().contains(splitUTC[2]));
		assertTrue(((Label) find(CREATE_LOCAL_TIME_LABEL)).getText().contains(
				MessageFormat.format("{0}:{1}", splitTime[0], splitTime[1])));

		assertEquals(TimeZone.getDefault().getID(), createPanePage.getStartTimezone());
		final var randomTimezone = getRandomTimezone();
		createPanePage.setStartTimezone(randomTimezone);
		checkDateString(utcString);

		assertEquals(TimeZone.getDefault().getID(), createPanePage.getSystemTimezone());
		final var randomTimezoneSystem = getRandomTimezone();
		createPanePage.setSystemTimezone(randomTimezoneSystem);
		// 		Commented because of the likelihood of random differences in the seconds par of the label
		//		assertEquals(utcString, ((Label) find(SYSTEM_LOCAL_TIME_LABEL)).getText());


		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.clickOnNowButton().setStartTimezone("America/New_York").setSystemTimezone("America/Los_Angeles");

	}

	@Test
	public void createAccount_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

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
				.setNodeAccount(4)
				.setTransactionFee(1.2345678911)
				.setInitialBalance(1000)
				.setCreateKey()
				.doubleClickOnAccountKey("treasury")
				.saveKey();

		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources);


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


		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the transaction",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));


	}

	@Test
	public void createTransfer_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var date = DateUtils.addDays(new Date(), 2);
		final var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		final var sdf = new SimpleDateFormat("yyyy-MM-dd");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		final Label dateLabel = find(CREATE_LOCAL_TIME_LABEL);

		final var dtf = DateTimeFormatter.ofPattern("MM/dd/yyyy");
		final var localDateTime = LocalDateTime.of(LocalDate.parse(datePickerFormat.format(date), dtf),
				LocalTime.of(3, 45, 15));

		final var transactionValidStart = Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setComment("this is a comment that will go with the transfer")
				.setDate(datePickerFormat.format(date));

		final var utcDate = find(CREATE_LOCAL_TIME_LABEL);
		assertTrue(utcDate.isVisible());
		//	assertTrue(dateLabel.getText().contains(sdf.format(date)));
		logger.info("TRANSFER: Local date label =>>> {}", dateLabel.getText());
		assertTrue(dateLabel.getText().contains(":00:00"));

		createPanePage.setHours(3)
				.setMinutes(45)
				.setSeconds(15);

		assertTrue(find(CREATE_LOCAL_TIME_LABEL).isVisible());
		assertTrue(dateLabel.getText().contains(":45:15"));

		createPanePage.setMemo("A transfer")
				.setFeePayerAccount(10019)
				.setNodeAccount(4);

		createPanePage.addDebit(1001, 100000000)
				.addDebit(1002, 666666666);

		assertEquals("-766 666 666 ħ", ((Label) find(CREATE_TRANSFER_TOTAL_LABEL)).getText());

		createPanePage.addCredit(1003, 766666666.1);

		assertEquals("0.10 000 000 ħ", ((Label) find(CREATE_TRANSFER_TOTAL_LABEL)).getText());

		createPanePage.addDebit(1004, 0.1);

		assertEquals("0 ħ", ((Label) find(CREATE_TRANSFER_TOTAL_LABEL)).getText());

		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources);

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		ToolTransferTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("10019")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolTransferTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}


		assertNotNull(toolTransaction);
		assertEquals(new Identifier(0, 0, 10019).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A transfer", toolTransaction.getMemo());

		final var transferMap = toolTransaction.getAccountAmountMap();

		assert toolTransaction.getTransaction().getMaxTransactionFee() != null;
		assertEquals(100000000, toolTransaction.getTransaction().getMaxTransactionFee().toTinybars());

		assertEquals(4, transferMap.size());

		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the transfer", comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));
	}

	@Test
	public void setTotalsToTransfer_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.addDebit(1001, 100000000)
				.addDebit(1002, 666666666);

		assertEquals("766666666", ((TextField) find(CREATE_TRANSFER_TO_AMOUNT)).getText());

		createPanePage.addCredit(1003, 866666666);

		assertEquals("100000000", ((TextField) find(CREATE_TRANSFER_FROM_AMOUNT)).getText());
		createPanePage.addDebit(1004, 100000000);

		assertEquals("", ((TextField) find(CREATE_TRANSFER_FROM_AMOUNT)).getText());
		assertEquals("", ((TextField) find(CREATE_TRANSFER_FROM_AMOUNT)).getText());

	}

	@Test
	public void createUpdateAccount_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

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
				.setUpdateKey("treasury");

		createPanePage.saveKey();


		final var newKey = find(CREATE_UPDATE_NEW_KEY);
		assertTrue(newKey.isVisible());
		assertTrue(newKey instanceof ScrollPane);
		final var newTree = ((ScrollPane) newKey).getContent();
		assertTrue(newTree instanceof TreeView);


		assertEquals(18, TestUtil.countTreeNodes(((TreeView<String>) newTree).getRoot()));

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

		assertTrue(toolTransaction.getTransaction() instanceof AccountUpdateTransaction);

		assertEquals(new Identifier(0, 0, 1019).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A memo", toolTransaction.getMemo());

		assertTrue(toolTransaction.getTransaction() instanceof AccountUpdateTransaction);
		final var accountUpdateTransaction = (AccountUpdateTransaction) toolTransaction.getTransaction();
		assertNotNull(accountUpdateTransaction.getKey());
		assertNull(accountUpdateTransaction.getAutoRenewPeriod());

		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the update transaction",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));
	}

	@Test
	public void createAccountUpdateAutoRenew_test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

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

		createPanePage.setDate(datePickerFormat.format(date))
				.setHours(2)
				.setMinutes(30)
				.setSeconds(45)
				.setMemo("A memo")
				.setFeePayerAccount(1059)
				.setNodeAccount(4)
				.setAutoRenew(AUTO_RENEW_DEFAULT);

		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources);


		final var transactions = new File(
				CLOUD_OUTPUT_DIRECTORY).listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		ToolCryptoUpdateTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f : transactions) {
			if (f.getName().contains("1059")) {
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

		assertTrue(toolTransaction.getTransaction() instanceof AccountUpdateTransaction);

		assertEquals(new Identifier(0, 0, 1059).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A memo", toolTransaction.getMemo());

		assertTrue(toolTransaction.getTransaction() instanceof AccountUpdateTransaction);
		final var accountUpdateTransaction = (AccountUpdateTransaction) toolTransaction.getTransaction();
		assertNull(accountUpdateTransaction.getKey());
		assertNotNull(accountUpdateTransaction.getAutoRenewPeriod());
		assertEquals(AUTO_RENEW_DEFAULT, accountUpdateTransaction.getAutoRenewPeriod().getSeconds());

		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the update transaction",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));
	}

	@Test
	public void createLargeBinaryUpdate_test() throws HederaClientException, IOException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var date = DateUtils.addDays(new Date(), 2);
		final var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		final var sdf = new SimpleDateFormat("yyyy-MM-dd");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

		final var dtf = DateTimeFormatter.ofPattern("MM/dd/yyyy");
		final var localDateTime = LocalDateTime.of(LocalDate.parse(datePickerFormat.format(date), dtf),
				LocalTime.of(3, 45, 15));

		final var transactionValidStart = Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString())
				.setComment("this is a comment that will go with the file update")
				.setUpdateFileID(155)
				.setDate(datePickerFormat.format(date))
				.setHours(3)
				.setMinutes(45)
				.setSeconds(15)
				.setMemo("A file update")
				.setFeePayerAccount(10019)
				.setNodeAccount(4);

		createPanePage.setContents("src/test/resources/createTransactions/largeFileUpdate.zip")
				.setChunkSize(1000)
				.setInterval(1000000000);

		logger.info("Exporting to \"{}\"", resources);

		createPanePage.createAndExport(resources);

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.LARGE_BINARY_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});

		assert transactions != null;

		var comment = new JsonObject();

		File zipFile = null;

		for (final var f : transactions) {
			if (f.getName().contains("10019")) {
				if (f.getName().endsWith(Constants.LARGE_BINARY_EXTENSION)) {
					zipFile = f;
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}

		assert zipFile != null;
		unZip(zipFile.getAbsolutePath(), "src/test/resources/unzipped");

		final var unzippedFiles = new File("src/test/resources/unzipped").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.CONTENT_EXTENSION) || name.endsWith(Constants.JSON_EXTENSION);
				});

		assert unzippedFiles != null;
		assertEquals(2, unzippedFiles.length);
		File jsonFile = null;
		File contentFile = null;
		for (final var unzippedFile : unzippedFiles) {
			if (JSON_EXTENSION.equals(FilenameUtils.getExtension(unzippedFile.getName()))) {
				jsonFile = unzippedFile;
			}
			if (CONTENT_EXTENSION.equals(FilenameUtils.getExtension(unzippedFile.getName()))) {
				contentFile = unzippedFile;
			}
		}
		assertNotNull(jsonFile);
		assertNotNull(contentFile);

		final InputStream inputStream1 = new FileInputStream(contentFile);
		final InputStream inputStream2 =
				new FileInputStream("src/test/resources/createTransactions/largeFileUpdate.zip");

		assertTrue(IOUtils.contentEquals(inputStream1, inputStream2));

		final var jsonObject = readJsonObject(jsonFile);

		assertTrue(jsonObject.has("filename"));
		assertEquals("largeFileUpdate.zip", jsonObject.get("filename").getAsString());

		assertTrue(jsonObject.has("fileID"));
		assertEquals(new Identifier(0, 0, 155, "MAINNET").asJSON(), jsonObject.getAsJsonObject("fileID"));

		assertTrue(jsonObject.has("chunkSize"));
		assertEquals(1000, jsonObject.get("chunkSize").getAsInt());

		assertTrue(jsonObject.has("validIncrement"));
		assertEquals(1000000000L, jsonObject.get("validIncrement").getAsLong());

		assertTrue(jsonObject.has("firstTransactionValidStart"));
		final var first = jsonObject.get("firstTransactionValidStart").getAsJsonObject();
		assertEquals(transactionValidStart.getTime() / 1000, first.get("seconds").getAsLong());

		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the file update", comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));

		FileUtils.deleteDirectory(new File("src/test/resources/unzipped"));
		FileUtils.cleanDirectory(
				new File(CLOUD_OUTPUT_DIRECTORY));
	}

	@Test
	public void createSystemDeleteFile_Test() throws HederaClientException, InterruptedException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());


		final var localDateTime = LocalDateTime.now().plusMinutes(1);
		final var transactionValidStart = Date.from(localDateTime.toInstant(OffsetDateTime.now().getOffset()));
		final var expiration = LocalDateTime.now().plusMinutes(5);
		final var expirationDate = Date.from(expiration.toInstant(OffsetDateTime.now().getOffset()));

		final Label entityID = find(SYSTEM_ENTITY_ID_LABEL);

		final VBox expirationBox = find(CREATE_SYSTEM_EXPIRATION_VBOX);

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString());

		assertTrue(expirationBox.isVisible());

		createPanePage.setOperation(undelete);


		assertEquals("File ID", entityID.getText());
		assertFalse(expirationBox.isVisible());

		createPanePage.setOperation(delete);

		assertTrue(expirationBox.isVisible());

		createPanePage.setEntity(file);
		assertEquals("File ID", entityID.getText());

		createPanePage.setEntity(contract);
		assertEquals("Contract ID", entityID.getText());


		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.setComment("this is a comment that will go with the system transaction - File Delete")
				.setOperation(delete)
				.setEntity(file)
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232)
				.setEntityID(2323)
				.setExpirationDate(expiration);

		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources)
				.clickOnPopupButton("CONTINUE");


		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});
		assert transactions != null;

		ToolSystemTransaction toolTransaction = null;
		var comment = new JsonObject();


		for (final var f : transactions) {
			if (f.getName().contains("3232")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolSystemTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}

		assertNotNull(toolTransaction);
		assertEquals(new Identifier(0, 0, 3232).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A comment", toolTransaction.getMemo());

		assertTrue(toolTransaction.getTransaction() instanceof SystemDeleteTransaction);
		assertEquals(2323, toolTransaction.getEntity().getAccountNum());
		assertTrue(toolTransaction.isFile());
		assertEquals(expirationDate.getTime() / 1000, toolTransaction.getExpiration().getEpochSecond());

		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the system transaction - File Delete",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));


	}

	@Test
	public void createSystemDeleteContract_Test() throws HederaClientException, InterruptedException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var localDateTime = LocalDateTime.now().plusMinutes(1);
		final var transactionValidStart = Date.from(localDateTime.toInstant(OffsetDateTime.now().getOffset()));
		final var expiration = LocalDateTime.now().plusMinutes(5);
		final var expirationDate = Date.from(expiration.toInstant(OffsetDateTime.now().getOffset()));

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.setComment("this is a comment that will go with the system transaction - Contract Delete")
				.setOperation(delete)
				.setEntity(contract)
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232)
				.setEntityID(23232)
				.setExpirationDate(expiration);

		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources)
				.clickOnPopupButton("CONTINUE");


		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});
		assert transactions != null;

		ToolSystemTransaction toolTransaction = null;
		var comment = new JsonObject();

		for (final var f :
				transactions) {
			if (f.getName().contains("3232")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolSystemTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}

		assertNotNull(toolTransaction);


		assertEquals(new Identifier(0, 0, 3232).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A comment", toolTransaction.getMemo());

		assertTrue(toolTransaction.getTransaction() instanceof SystemDeleteTransaction);
		assertEquals(23232, toolTransaction.getEntity().getAccountNum());
		assertFalse(toolTransaction.isFile());
		assertEquals(expirationDate.getTime() / 1000, toolTransaction.getExpiration().getEpochSecond());

		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the system transaction - Contract Delete",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));

	}

	@Test
	public void checkRemainingTimePopup_Test() throws Exception {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		var localDateTime = LocalDateTime.now().plusMinutes(1);
		var expiration = LocalDateTime.now().plusMinutes(5);

		final var outputDirectory = new File(
				CLOUD_OUTPUT_DIRECTORY);

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.setComment("this is a comment that will go with the system transaction - Contract Delete")
				.setOperation(delete)
				.setEntity(contract)
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232)
				.setEntityID(23232)
				.setExpirationDate(expiration);

		logger.info("Exporting to \"{}\"", resources);

		createPanePage.createAndExport(resources)
				.clickOnPopupButton("CANCEL");


		var transactions = outputDirectory.listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});
		assert transactions != null;
		assertEquals(0, transactions.length);

		localDateTime = LocalDateTime.now().plusMinutes(30);
		expiration = LocalDateTime.now().plusMinutes(35);
		logger.info("Exporting to \"{}\"", resources);
		createPanePage.setStartDate(localDateTime)
				.setExpirationDate(expiration)
				.createAndExport(resources);

		transactions = outputDirectory.listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});
		assert transactions != null;


		assertEquals(2, transactions.length);
	}

	@Test
	public void createSystemUnDeleteFile_Test() throws HederaClientException, InterruptedException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var localDateTime = LocalDateTime.now().plusMinutes(1);
		final var transactionValidStart = Date.from(localDateTime.toInstant(OffsetDateTime.now().getOffset()));

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.setComment("this is a comment that will go with the system transaction - File UnDelete")
				.setOperation(undelete)
				.setEntity(file)
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232)
				.setEntityID(2323);

		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources)
				.clickOnPopupButton("CONTINUE");


		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});
		assert transactions != null;

		ToolSystemTransaction toolTransaction = null;
		var comment = new JsonObject();


		for (final var f :
				transactions) {
			if (f.getName().contains("3232")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolSystemTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}


		assert toolTransaction != null;
		assertEquals(new Identifier(0, 0, 3232).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A comment", toolTransaction.getMemo());

		assertTrue(toolTransaction.getTransaction() instanceof SystemUndeleteTransaction);
		assertEquals(2323, toolTransaction.getEntity().getAccountNum());
		assertTrue(toolTransaction.isFile());


		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the system transaction - File UnDelete",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));

	}

	@Test
	public void createSystemUnDeleteContract_Test() throws HederaClientException, InterruptedException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var localDateTime = LocalDateTime.now().plusMinutes(1);
		final var transactionValidStart = Date.from(localDateTime.toInstant(OffsetDateTime.now().getOffset()));

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.setComment("this is a comment that will go with the system transaction - Contract UnDelete")
				.setOperation(undelete)
				.setEntity(contract)
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232)
				.setEntityID(23232);

		logger.info("Exporting to \"{}\"", resources);
		createPanePage.createAndExport(resources)
				.clickOnPopupButton("CONTINUE");


		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				pathname -> {
					final var name = pathname.getName();
					return name.endsWith(Constants.TRANSACTION_EXTENSION) || name.endsWith(Constants.TXT_EXTENSION);
				});
		assert transactions != null;

		ToolSystemTransaction toolTransaction = null;
		var comment = new JsonObject();


		for (final var f :
				transactions) {
			if (f.getName().contains("3232")) {
				if (f.getName().endsWith(Constants.TRANSACTION_EXTENSION)) {
					toolTransaction = new ToolSystemTransaction(f);
				}
				if (f.getName().endsWith(Constants.TXT_EXTENSION)) {
					comment = readJsonObject(f.getAbsolutePath());
				}
			}
		}

		assert toolTransaction != null;
		assertEquals(new Identifier(0, 0, 3232).asAccount(),
				toolTransaction.getTransactionId().accountId);

		assertEquals(transactionValidStart.getTime() / 1000,
				Objects.requireNonNull(toolTransaction.getTransactionId().validStart).getEpochSecond());
		assertEquals("A comment", toolTransaction.getMemo());

		assertTrue(toolTransaction.getTransaction() instanceof SystemUndeleteTransaction);
		assertEquals(23232, toolTransaction.getEntity().getAccountNum());
		assertFalse(toolTransaction.isFile());


		assertTrue(comment.has("Author"));
		assertEquals("test1.council2@hederacouncil.org", comment.get("Author").getAsString());
		assertTrue(comment.has("Contents"));
		assertEquals("this is a comment that will go with the system transaction - Contract UnDelete",
				comment.get("Contents").getAsString());
		assertTrue(comment.has("Timestamp"));

	}

	@Test
	public void errorMessagesCreate_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var headless = System.getProperty("headless");
		if (headless != null && headless.equals("true")) {
			// Test will not work on headless mode
			return;
		}

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.createAndExport(resources);

		assertTrue(find(CREATE_INVALID_DATE).isVisible());
		assertTrue(find(CREATE_INVALID_CREATE_NEW_KEY).isVisible());
		assertTrue(find(CREATE_INVALID_FEE_PAYER).isVisible());

		createPanePage.setStartDate(LocalDateTime.now().plusMinutes(2));
		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertTrue(find(CREATE_INVALID_CREATE_NEW_KEY).isVisible());
		assertTrue(find(CREATE_INVALID_FEE_PAYER).isVisible());

		createPanePage.setCreateKey()
				.doubleClickOnAccountKey("treasury")
				.saveKey();

		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_NEW_KEY).isVisible());
		assertTrue(find(CREATE_INVALID_FEE_PAYER).isVisible());

		createPanePage.setFeePayerAccount(22);
		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_FEE_PAYER).isVisible());
	}

	@Test
	public void errorMessagesUpdate_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var headless = System.getProperty("headless");
		if (headless != null && headless.equals("true")) {
			// Test will not work on headless mode
			return;
		}

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString())
				.createAndExport(resources);

		assertTrue(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_NEW_KEY).isVisible());
		assertTrue(find(CREATE_INVALID_FEE_PAYER).isVisible());
		assertTrue(find(CREATE_INVALID_UPDATE_ACCOUNT).isVisible());

		createPanePage.setStartDate(LocalDateTime.now().plusMinutes(2));
		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_CREATE_NEW_KEY).isVisible());
		assertTrue(find(CREATE_INVALID_FEE_PAYER).isVisible());
		assertTrue(find(CREATE_INVALID_UPDATE_ACCOUNT).isVisible());

		createPanePage.setFeePayerAccount(22);
		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_FEE_PAYER).isVisible());
		assertTrue(find(CREATE_INVALID_UPDATE_ACCOUNT).isVisible());

		createPanePage.setUpdateAccount(50);
		assertFalse(find(CREATE_INVALID_DATE).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_NEW_KEY).isVisible());
		assertFalse(find(CREATE_INVALID_FEE_PAYER).isVisible());
		assertFalse(find(CREATE_INVALID_UPDATE_ACCOUNT).isVisible());
	}

	@Test
	public void createAccountFieldsChecksums0_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setFeePayerAccount("0.1");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void createAccountFieldsChecksums1_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString()).setFeePayerAccount("0.0" +
				".1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void createAccountFieldsChecksums2_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString()).setFeePayerAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void createAccountFieldsChecksums3_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString()).setNodeAccount("0.3");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidNode").isVisible());
	}

	@Test
	public void createAccountFieldsChecksums4_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString()).setNodeAccount("0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidNode").isVisible());
	}

	@Test
	public void createAccountFieldsChecksums5_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString()).setNodeAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidNode").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums0_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString())
				.setFeePayerAccount("0.1");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums1_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString()).setFeePayerAccount("0.0" +
				".1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums2_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString()).setFeePayerAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums3_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString()).setNodeAccount("0.3");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidNode").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums4_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString()).setNodeAccount("0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidNode").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums5_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString()).setNodeAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidNode").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums6_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString()).setUpdateAccount("0.66");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidUpdateAccountToUpdate").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums7_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString()).setUpdateAccount(
				"0.0.888888-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidUpdateAccountToUpdate").isVisible());
	}

	@Test
	public void updateAccountFieldsChecksums8_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString()).setUpdateAccount(123);
		assertNotNull(TestUtil.getPopupNodes());
		createPanePage.closePopup("CONTINUE");
		assertFalse(find("#invalidUpdateAccountToUpdate").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums0_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.setFeePayerAccount("0.1");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums1_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString()).setFeePayerAccount("0.0" +
				".1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums2_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString()).setFeePayerAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidFeePayer").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums3_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString()).setNodeAccount("0.3");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidNode").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums4_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString()).setNodeAccount("0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidNode").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums5_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString()).setNodeAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidNode").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums6_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString()).setEntityID("0.66");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidEntity").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums7_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString()).setEntityID("0.0.888888-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidEntity").isVisible());
	}

	@Test
	public void systemAccountFieldsChecksums8_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString()).setEntityID(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidEntity").isVisible());

	}

	@Test
	public void fileAccountFieldsChecksums0_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString())
				.setUpdateFileID("0.3");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidUpdateFileToUpdate").isVisible());

	}

	@Test
	public void fileAccountFieldsChecksums1_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString()).setUpdateFileID(
				"0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidUpdateFileToUpdate").isVisible());

	}

	@Test
	public void fileAccountFieldsChecksums2_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString()).setUpdateFileID(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidUpdateFileToUpdate").isVisible());
	}

	@Test
	public void fileAccountFieldsChecksums3_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString()).setFeePayerAccount("0.1");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidFeePayer").isVisible());

	}

	@Test
	public void fileAccountFieldsChecksums4_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString()).setFeePayerAccount(
				"0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidFeePayer").isVisible());

	}

	@Test
	public void fileAccountFieldsChecksums5_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString()).setFeePayerAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidFeePayer").isVisible());

	}

	@Test
	public void fileAccountFieldsChecksums6_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString()).setNodeAccount("0.3");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidNode").isVisible());

	}

	@Test
	public void fileAccountFieldsChecksums7_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString()).setNodeAccount(
				"0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidNode").isVisible());

	}

	@Test
	public void fileAccountFieldsChecksums8_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString()).setNodeAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidNode").isVisible());
	}

	@Test
	public void transferAccountFieldsChecksums0_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFeePayerAccount("0.1");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidFeePayer").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums1_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFeePayerAccount("0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidFeePayer").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums2_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFeePayerAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidFeePayer").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums3_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setNodeAccount("0.3");
		checkBadChecksum("e.g.");
		assertTrue(find("#invalidNode").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums4_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setNodeAccount("0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#invalidNode").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums5_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setNodeAccount(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#invalidNode").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums6_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFromAccountTransfer("0.33");
		checkBadChecksum("e.g.");
		assertTrue(find("#errorInvalidFromAccount").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums7_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFromAccountTransfer("0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#errorInvalidFromAccount").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums8_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFromAccountTransfer(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#errorInvalidFromAccount").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums9_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setToAccountTransfer("0.33");
		checkBadChecksum("e.g.");
		assertTrue(find("#errorInvalidToAccount").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums10_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setToAccountTransfer("0.0.1-aaaaa");
		checkBadChecksum("The checksum entered does not correspond to the account.");
		assertTrue(find("#errorInvalidToAccount").isVisible());

	}

	@Test
	public void transferAccountFieldsChecksums11_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setToAccountTransfer(123);
		assertNull(TestUtil.getPopupNodes());
		assertFalse(find("#errorInvalidToAccount").isVisible());
	}

	@Test
	public void freezeOnly_test() throws InterruptedException, HederaClientException, InvalidProtocolBufferException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var localDateTime = LocalDateTime.now().plusMinutes(1);
		final var startTime = LocalDateTime.now().plusMinutes(5);

		createPanePage.selectTransaction(CreateTransactionType.FREEZE.getTypeString())
				.setFreezeType(FreezeType.FREEZE_ONLY)
				.setComment("this is a comment that will go with the system transaction - Freeze only")
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232);

		assertTrue(find("#freezeStartVBox").isVisible());
		assertFalse(find("#freezeFileVBox").isVisible());

		createPanePage.setFreezeStartDate(startTime)
				.createAndExport(resources)
				.clickOnPopupButton("CONTINUE");

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(TRANSACTION_EXTENSION));

		assert transactions != null;
		assertEquals(1, transactions.length);
		final var transaction = Transaction.fromBytes(readBytes(transactions[0]));
		assertTrue(transaction instanceof FreezeTransaction);
		final var freeezeTransaction = (FreezeTransaction) transaction;
		assertEquals(FreezeType.FREEZE_ONLY, freeezeTransaction.getFreezeType());

		assertEquals(new Date(startTime.atZone(ZoneId.systemDefault()).toInstant().getEpochSecond() * 1000),
				Date.from(freeezeTransaction.getStartTime()));

		assertNotNull(freeezeTransaction.getTransactionId());
		assertNotNull(freeezeTransaction.getTransactionId().validStart);

		assertEquals(new Date(localDateTime.atZone(ZoneId.systemDefault()).toInstant().getEpochSecond() * 1000),
				Date.from(
						freeezeTransaction.getTransactionId().validStart));
	}

	@Test
	public void freezeAbort_test() throws InterruptedException, HederaClientException, InvalidProtocolBufferException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var localDateTime = LocalDateTime.now().plusMinutes(1);

		createPanePage.selectTransaction(CreateTransactionType.FREEZE.getTypeString())
				.setFreezeType(FreezeType.FREEZE_ABORT)
				.setComment("this is a comment that will go with the system transaction - Freeze abort")
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232);

		assertFalse(find("#freezeStartVBox").isVisible());
		assertFalse(find("#freezeFileVBox").isVisible());

		createPanePage.createAndExport(resources)
				.clickOnPopupButton("CONTINUE");

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(TRANSACTION_EXTENSION));

		assert transactions != null;
		assertEquals(1, transactions.length);
		final var transaction = Transaction.fromBytes(readBytes(transactions[0]));
		assertTrue(transaction instanceof FreezeTransaction);
		final var freeezeTransaction = (FreezeTransaction) transaction;
		assertEquals(FreezeType.FREEZE_ABORT, freeezeTransaction.getFreezeType());

		assertNotNull(freeezeTransaction.getTransactionId());
		assertNotNull(freeezeTransaction.getTransactionId().validStart);

		assertEquals(new Date(localDateTime.atZone(ZoneId.systemDefault()).toInstant().getEpochSecond() * 1000),
				Date.from(
						freeezeTransaction.getTransactionId().validStart));
	}

	@Test
	public void freezeUpgrade_test() throws InterruptedException, HederaClientException,
			InvalidProtocolBufferException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var localDateTime = LocalDateTime.now().plusMinutes(1);
		final var startTime = LocalDateTime.now().plusMinutes(5);

		createPanePage.selectTransaction(CreateTransactionType.FREEZE.getTypeString())
				.setFreezeType(FreezeType.FREEZE_UPGRADE)
				.setComment("this is a comment that will go with the system transaction - Freeze and upgrade")
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232);

		assertTrue(find("#freezeStartVBox").isVisible());
		assertTrue(find("#freezeFileVBox").isVisible());

		createPanePage.setFreezeStartDate(startTime)
				.setFreezeFileId(123);

		createPanePage.setFreezeHash("xxxyyyzzz");
		assertTrue(find("#invalidFreezeFileHash").isVisible());
		createPanePage
				.setFreezeHash("abc123def456");
		assertFalse(find("#invalidFreezeFileHash").isVisible());
		createPanePage
				.createAndExport(resources)
				.clickOnPopupButton("CONTINUE");

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(TRANSACTION_EXTENSION));

		assert transactions != null;
		assertEquals(1, transactions.length);
		final var transaction = Transaction.fromBytes(readBytes(transactions[0]));
		assertTrue(transaction instanceof FreezeTransaction);
		final var freeezeTransaction = (FreezeTransaction) transaction;
		assertEquals(FreezeType.FREEZE_UPGRADE, freeezeTransaction.getFreezeType());

		assertEquals(new Date(startTime.atZone(ZoneId.systemDefault()).toInstant().getEpochSecond() * 1000),
				Date.from(freeezeTransaction.getStartTime()));

		assertNotNull(freeezeTransaction.getTransactionId());
		assertNotNull(freeezeTransaction.getTransactionId().validStart);

		assertEquals(new Date(localDateTime.atZone(ZoneId.systemDefault()).toInstant().getEpochSecond() * 1000),
				Date.from(
						freeezeTransaction.getTransactionId().validStart));

		assertEquals(new Identifier(0, 0, 123).asFile(), freeezeTransaction.getFileId());
		assertArrayEquals(Hex.decode("abc123def456"), freeezeTransaction.getFileHash());
	}

	@Test
	public void prepareUpgrade_test() throws HederaClientException, InvalidProtocolBufferException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var localDateTime = LocalDateTime.now().plusHours(1);

		createPanePage.selectTransaction(CreateTransactionType.FREEZE.getTypeString())
				.setFreezeType(FreezeType.PREPARE_UPGRADE)
				.setComment("this is a comment that will go with the system transaction - Prepare upgrade")
				.setStartDate(localDateTime)
				.setMemo("A comment")
				.setFeePayerAccount(3232);

		createPanePage
				.setFreezeFileId(111)
				.setFreezeHash("xxxyyyzzz");
		assertTrue(find("#invalidFreezeFileHash").isVisible());
		createPanePage
				.setFreezeHash("abc123def456");
		assertFalse(find("#invalidFreezeFileHash").isVisible());
		createPanePage
				.createAndExport(resources);

		final var transactions = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(TRANSACTION_EXTENSION));

		assert transactions != null;
		assertEquals(1, transactions.length);
		final var transaction = Transaction.fromBytes(readBytes(transactions[0]));
		assertTrue(transaction instanceof FreezeTransaction);
		final var freeezeTransaction = (FreezeTransaction) transaction;
		assertEquals(FreezeType.PREPARE_UPGRADE, freeezeTransaction.getFreezeType());

		assertNotNull(freeezeTransaction.getTransactionId());
		assertNotNull(freeezeTransaction.getTransactionId().validStart);

		assertEquals(new Date(localDateTime.atZone(ZoneId.systemDefault()).toInstant().getEpochSecond() * 1000),
				Date.from(
						freeezeTransaction.getTransactionId().validStart));

		assertEquals(new Identifier(0, 0, 111).asFile(), freeezeTransaction.getFileId());
		assertArrayEquals(Hex.decode("abc123def456"), freeezeTransaction.getFileHash());
	}

	@AfterEach
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

	private String getUTCString(final LocalDateTime localDateTime, final ZoneId zoneID) {
		final var date = Date.from(localDateTime.atZone(zoneID).toInstant());
		localDateTime.atZone(zoneID);
		final var dateTimeFormatter =
				DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.of("UTC"));
		return dateTimeFormatter.format(date.toInstant().plusSeconds(0)) +
				" Coordinated Universal Time";
	}

	private String getRandomTimezone() {
		final var zones = TimeZone.getAvailableIDs();
		final var count = (int) (Math.random() * zones.length);
		return zones[count];
	}

	/**
	 * Compare the date, hours and minutes on a time label with a string. Skipping seconds because of corner cases
	 *
	 * @param utcString
	 * 		string that contains the string that holds "truth"
	 */
	private void checkDateString(final String utcString) {
		final var split1 = utcString.split("[ :]");
		final var split2 = ((Label) find(JavaFXIDs.CREATE_LOCAL_TIME_LABEL)).getText().split("[ :]");
		assertEquals(split1[0], split2[0]);
		assertEquals(split1[1], split2[1]);
		assertEquals(split1[2], split2[2]);

		for (var i = 4; i < split1.length; i++)
			assertEquals(split1[i], split2[i]);
	}

	@Override
	public TestBase get() {
		return this;
	}

	private void checkBadChecksum(final String s) {
		final var popupNodes = TestUtil.getPopupNodes();
		Assert.assertNotNull(popupNodes);
		assertEquals(1, popupNodes.size());

		final var children = ((VBox) popupNodes.get(0)).getChildren();
		assertTrue(children.get(0) instanceof Label);
		final var label = (Label) children.get(0);
		assertTrue(label.getText().contains(s));
		clickOn(children.get(1));
	}

}
