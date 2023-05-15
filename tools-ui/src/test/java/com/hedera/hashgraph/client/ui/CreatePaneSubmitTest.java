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

import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.JsonConstants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.pages.CreatePanePage;
import com.hedera.hashgraph.client.ui.pages.HistoryPanePage;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.FileContentsQuery;
import com.hedera.hashgraph.sdk.FileCreateTransaction;
import com.hedera.hashgraph.sdk.FileInfoQuery;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_RECEIPTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.METADATA_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.OperationType.undelete;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findButtonInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findCheckBoxesInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getLabels;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

@Disabled("Headless mode is throwing errors as it can't find popup. Issues still occur in headed mode due thread issues and more.")
public class CreatePaneSubmitTest extends TestBase implements GenericFileReadWriteAware {
	private final static Logger logger = LogManager.getLogger(CreatePaneSubmitTest.class);
	public static final String ONE_DRIVE = "/src/test/resources/Transactions/";
	public static final String ONE_DRIVE_INPUT = "src/test/resources/Transactions/InputFiles/";
	public static final String ONE_DRIVE_OUTPUT =
			"src/test/resources/Transactions/OutputFiles/test1.council2@hederacouncil.org";

	private MainWindowPage mainWindowPage;
	private HomePanePage homePanePage;
	private CreatePanePage createPanePage;
	private HistoryPanePage historyPanePage;

	private Client client;
	private UserAccessibleProperties properties;
	private final List<AccountId> accounts = new ArrayList<>();

	@BeforeEach
	public void setUp() throws Exception {
		final var output = new File(ONE_DRIVE_OUTPUT);
		if (output.exists()) {
			FileUtils.cleanDirectory(output);
		}
		if (output.mkdirs()) {
			logger.info("Output directory created");
		}

		final var input = new File(ONE_DRIVE_INPUT);
		if (input.exists()) {
			FileUtils.cleanDirectory(input);
		}
		if (input.mkdirs()) {
			logger.info("Input directory created");
		}
		setupClient();
		createAccounts(2);
		TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
		setupUI();
		mainWindowPage.clickOnCreateButton();
	}

	@AfterEach
	public void tearDown() throws Exception {
		if (new File(DEFAULT_STORAGE).exists()) {
			try {
				FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
			} catch (final IOException e) {
				logger.error("Unable to delete {}: {}", DEFAULT_STORAGE, e.getMessage());
			}
		}

		final var output = new File(ONE_DRIVE_OUTPUT);
		if (output.exists()) {
			FileUtils.cleanDirectory(output);
		}

		final var input = new File(ONE_DRIVE_INPUT);
		if (input.exists()) {
			FileUtils.cleanDirectory(input);
		}
	}

	@Test
	public void submitOneTransfer() throws HederaClientException, PrecheckStatusException, TimeoutException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		assertFalse(accounts.isEmpty());
		final var oldHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));


		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFeePayerAccount(accounts.get(0).toString())
				.addDebit(accounts.get(0).num, 1)
				.addCredit(accounts.get(1).num, 1)
				.submit()
				.closePopup("CONTINUE")
				.signWithPassword(TEST_PASSWORD)
				.closePopup("SUBMIT")
				.closePopup("CONTINUE");

		final var newHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		assertEquals(1, newHistory.size() - oldHistory.size());
		final var receipts = new File(DEFAULT_RECEIPTS).listFiles();
		assertEquals(1, receipts.length);
		final var receipt = readJsonObject(receipts[0]);
		assertEquals("SUCCESS", receipt.get(JsonConstants.STATUS_PROPERTY).getAsString());

		final var accountInfo = new AccountInfoQuery()
				.setAccountId(accounts.get(1))
				.execute(client);
		assertEquals(100100000000L, accountInfo.balance.toTinybars());

		final var fileName = newHistory.stream().filter(file -> !oldHistory.contains(file)).findFirst().orElse("");

		mainWindowPage.clickOnHistoryButton();
		historyPanePage.expandRow(fileName);
		sleep(100);
		final var expandedBox = historyPanePage.getExpandedBox();
		final var labels = getLabels(expandedBox.getChildren());
		assertTrue(labels.contains("signed by"));
		assertTrue(labels.contains("network response"));
		assertTrue(labels.contains("success"));

	}

	@Test
	public void popups_test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFeePayerAccount(accounts.get(0).toString())
				.addDebit(accounts.get(0).num, 1)
				.addCredit(accounts.get(1).num, 1)
				.submit();

		createPanePage.closePopup("CANCEL");

		final TextField payer = find("#feePayerAccountField");
		assertTrue(payer.getText().contains(new Identifier(0, 0, (int) accounts.get(0).num).toReadableString()));
		final TextField node = find("#nodeAccountField");
		assertTrue(node.getText().contains("0.0.3"));

		createPanePage.submit().closePopup("ADD MORE");
		final var checkboxes = findCheckBoxesInPopup(getPopupNodes());
		for (final var checkbox : checkboxes) {
			if (checkbox.getText().contains("KeyStore-0")) {
				clickOn(checkbox);
			}
			if (checkbox.getText().contains("genesis")) {
				clickOn(checkbox);
			}
		}
		createPanePage.closePopup("ACCEPT");
		final var labels = getLabels(getPopupNodes());

		assertFalse(labels.contains("\t• keystore-0"));
		assertTrue(labels.contains("\t• genesis"));

		createPanePage.closePopup("CONTINUE")
				.signWithPassword(TEST_PASSWORD);

		final var labels2 = getLabels(getPopupNodes());
		assertTrue(labels2.contains("-1 ℏ"));
		assertTrue(labels2.contains("1 ℏ"));

		assertTrue(labels2.contains("0.0.3-tzfmz"));

		createPanePage.closePopup("SUBMIT");
		final var labels3 = getLabels(getPopupNodes());
		assertTrue(labels3.get(0).contains("transferred"));

		createPanePage.closePopup("CONTINUE");

		assertFalse(find("#commentsVBox").isVisible());
		assertFalse(find("#freezeChoiceVBox").isVisible());
		assertFalse(find("#accountIDToUpdateVBox").isVisible());
		assertFalse(find("#fileIDToUpdateVBox").isVisible());
		assertFalse(find("#systemSlidersHBox").isVisible());
		assertFalse(find("#commonFieldsVBox").isVisible());
		assertFalse(find("#createAccountVBox").isVisible());
		assertFalse(find("#updateAccountVBox").isVisible());
		assertFalse(find("#transferCurrencyVBox").isVisible());
		assertFalse(find("#systemDeleteUndeleteVBox").isVisible());
		assertFalse(find("#fileContentsUpdateVBox").isVisible());
		assertFalse(find("#freezeVBox").isVisible());
		assertTrue(find("#createScrollPane").isVisible());
	}

	@Test
	@Disabled("Flaky test")
	public void submitOneCreate() throws HederaClientException, PrecheckStatusException, TimeoutException {
		assertFalse(accounts.isEmpty());
		final var oldHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));


		final var nickname = String.format("account_0 (%s)",
				new Identifier(accounts.get(0), "INTEGRATION").toReadableStringAndChecksum());
		createPanePage.selectTransaction(CreateTransactionType.CREATE.getTypeString())
				.setFeePayerAccount(accounts.get(0).toString())
				.setCreateKey()
				.doubleClickOnAccountKey(nickname)
				.saveKey()
				.setAccountMemo("account_2")
				.submit()
				.closePopup("CONTINUE")
				.signWithPassword(TEST_PASSWORD)
				.closePopup("SUBMIT");

		final var labels = getLabels(getPopupNodes());
		assertEquals(1, labels.size());

		createPanePage.closePopup("CONTINUE");

		final var newHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		assertEquals(1, newHistory.size() - oldHistory.size());


		final var receipts = new File(DEFAULT_RECEIPTS).listFiles();
		assertEquals(1, receipts.length);
		final var receipt = readJsonObject(receipts[0]);
		assertEquals("SUCCESS", receipt.get(JsonConstants.STATUS_PROPERTY).getAsString());
		final var id = receipt.get(JsonConstants.ENTITY_PROPERTY).getAsString();

		assertTrue(labels.get(0).contains(id));

		final var accountInfo = new AccountInfoQuery()
				.setAccountId(Identifier.parse(id, "INTEGRATION").asAccount())
				.execute(client);

		assertEquals("account_2", accountInfo.accountMemo);
		assertEquals(0, accountInfo.balance.toTinybars());
		assertEquals(10, ((KeyList) accountInfo.key).size());
	}

	@Test
	public void submitOneUpdateKey() throws HederaClientException, PrecheckStatusException, TimeoutException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		assertFalse(accounts.isEmpty());
		final var oldHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString())
				.setUpdateAccount(accounts.get(1).num)
				.setFeePayerAccount(accounts.get(0).toString())
				.setUpdateKey("treasury");
		final var node = find("Threshold key (x of 2)");
		doubleClickOn(node);
		createPanePage
				.setThreshold(1)
				.saveKey()
				.submit()
				.closePopup("CONTINUE")
				.signWithPassword(TEST_PASSWORD)
				.closePopup("SUBMIT");

		final var popup = getLabels(getPopupNodes());
		assertTrue(popup.get(0).contains(accounts.get(1).toString()));
		assertTrue(popup.get(0).contains("key"));

		createPanePage.closePopup("CONTINUE");

		final var newHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		assertEquals(1, newHistory.size() - oldHistory.size());
		final var receipts = new File(DEFAULT_RECEIPTS).listFiles();
		assertEquals(1, receipts.length);
		final var receipt = readJsonObject(receipts[0]);
		assertEquals("SUCCESS", receipt.get(JsonConstants.STATUS_PROPERTY).getAsString());

		final var accountInfo = new AccountInfoQuery()
				.setAccountId(accounts.get(1))
				.execute(client);
		final var oldkey = EncryptionUtils.jsonToKey(readJsonObject("src/test/resources/Keys/jsonKey.json"));
		final var key = (KeyList) accountInfo.key;
		assertTrue(key.contains(oldkey));
		assertEquals(2, key.size());
		assertEquals(1, (int) key.getThreshold());

		final var fileName = newHistory.stream().filter(file -> !oldHistory.contains(file)).findFirst().orElse("");
		mainWindowPage.clickOnHistoryButton();
		historyPanePage.expandRow(fileName);
		sleep(100);
		final var expandedBox = historyPanePage.getExpandedBox();
		final var labels = getLabels(expandedBox.getChildren());
		assertTrue(labels.contains("signed by"));
		assertTrue(labels.contains("network response"));
		assertTrue(labels.contains("success"));
	}

	@Test
	public void submitOneUpdateMemo() throws HederaClientException, PrecheckStatusException, TimeoutException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		assertFalse(accounts.isEmpty());
		final var oldHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString())
				.setUpdateAccount(accounts.get(1).num)
				.setFeePayerAccount(accounts.get(0).toString())
				.setNewAccountMemo("account_2")
				.submit()
				.closePopup("CONTINUE")
				.signWithPassword(TEST_PASSWORD)
				.closePopup("SUBMIT");

		final var popup = getLabels(getPopupNodes());
		assertTrue(popup.get(0).contains(accounts.get(1).toString()));
		assertTrue(popup.get(0).contains("account memo"));

		createPanePage.closePopup("CONTINUE");


		final var newHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		assertEquals(1, newHistory.size() - oldHistory.size());
		final var receipts = new File(DEFAULT_RECEIPTS).listFiles();
		assertEquals(1, receipts.length);
		final var receipt = readJsonObject(receipts[0]);
		assertEquals("SUCCESS", receipt.get(JsonConstants.STATUS_PROPERTY).getAsString());

		final var accountInfo = new AccountInfoQuery()
				.setAccountId(accounts.get(1))
				.execute(client);
		assertEquals("account_2", accountInfo.accountMemo);

		final var fileName = newHistory.stream().filter(file -> !oldHistory.contains(file)).findFirst().orElse("");
		mainWindowPage.clickOnHistoryButton();
		historyPanePage.expandRow(fileName);
		sleep(100);
		final var expandedBox = historyPanePage.getExpandedBox();
		final var labels = getLabels(expandedBox.getChildren());
		assertTrue(labels.contains("signed by"));
		assertTrue(labels.contains("network response"));
		assertTrue(labels.contains("success"));


	}

	@Test
	public void submitOneUpdateAssociationsAndMemo() throws HederaClientException, PrecheckStatusException,
			TimeoutException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		assertFalse(accounts.isEmpty());
		final var oldHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		createPanePage.selectTransaction(CreateTransactionType.UPDATE.getTypeString())
				.setUpdateAccount(accounts.get(1).num)
				.setFeePayerAccount(accounts.get(0).toString())
				.setNewAccountMemo("account_2")
				.setNewMaxTokenAss(5)
				.submit()
				.closePopup("CONTINUE")
				.signWithPassword(TEST_PASSWORD)
				.closePopup("SUBMIT");

		final var popup = getLabels(getPopupNodes());
		assertTrue(popup.get(0).contains(accounts.get(1).toString()));
		assertTrue(popup.get(0).contains("account memo"));
		assertTrue(popup.get(0).contains("maximum number of token associations"));

		createPanePage.closePopup("CONTINUE");


		final var newHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		assertEquals(1, newHistory.size() - oldHistory.size());
		final var receipts = new File(DEFAULT_RECEIPTS).listFiles();
		assertEquals(1, receipts.length);
		final var receipt = readJsonObject(receipts[0]);
		assertEquals("SUCCESS", receipt.get(JsonConstants.STATUS_PROPERTY).getAsString());

		final var accountInfo = new AccountInfoQuery()
				.setAccountId(accounts.get(1))
				.execute(client);
		assertEquals("account_2", accountInfo.accountMemo);
		assertEquals(5, accountInfo.maxAutomaticTokenAssociations);

		final var fileName = newHistory.stream().filter(file -> !oldHistory.contains(file)).findFirst().orElse("");
		mainWindowPage.clickOnHistoryButton();
		historyPanePage.expandRow(fileName);
		sleep(100);
		final var expandedBox = historyPanePage.getExpandedBox();
		final var labels = getLabels(expandedBox.getChildren());
		assertTrue(labels.contains("signed by"));
		assertTrue(labels.contains("network response"));
		assertTrue(labels.contains("success"));
	}

	@Test
	@Disabled("Postponed until decision made regarding need")
	public void submitFileDeleteUndelete() throws PrecheckStatusException, TimeoutException, HederaClientException,
			ReceiptStatusException, KeyStoreException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var oldHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		final var keyStore =
				Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");
		final var genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());
		final var key = EncryptionUtils.jsonToKey(readJsonObject("src/test/resources/Keys/jsonKey.json"));
//		key.add(genesisKey);
//		key.setThreshold(1);

		final var originalContents = "text in the file";
		final var contentsByteString = ByteString.copyFromUtf8(originalContents);

		final var transactionResponse = new FileCreateTransaction()
				.setKeys(key)
				.setContents(originalContents)
				.execute(client);

		final var fileID = transactionResponse.getReceipt(client).fileId;

		final var fileInfoOriginal = new FileInfoQuery()
				.setFileId(fileID)
				.execute(client);
		assertFalse(fileInfoOriginal.isDeleted);

		final var contents = new FileContentsQuery()
				.setFileId(fileID)
				.execute(client);

		assertEquals(contentsByteString, contents);

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.setFeePayerAccount(50)
				.setEntityID(fileID.num)
				.submit()
				.closePopup("CONTINUE")
				.signWithPassword(TEST_PASSWORD)
				.closePopup("SUBMIT")
				.closePopup("CONTINUE");

		final var newHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		assertEquals(1, newHistory.size() - oldHistory.size());

		final var fileName = newHistory.stream().filter(file -> !oldHistory.contains(file)).findFirst().orElse("");
		mainWindowPage.clickOnHistoryButton();
		historyPanePage.expandRow(fileName);
		sleep(100);

		final var expandedBox = historyPanePage.getExpandedBox();
		final var labels = getLabels(expandedBox.getChildren());
		assertTrue(labels.contains("signed by"));
		assertTrue(labels.contains("network response"));
		assertTrue(labels.contains("success"));

		final var fileInfoDeleted = new FileInfoQuery()
				.setFileId(fileID)
				.execute(client);
		assertTrue(fileInfoDeleted.isDeleted);

		final var contentsDeleted = new FileContentsQuery()
				.setFileId(fileID)
				.execute(client);

		assertEquals(contentsByteString, contentsDeleted);

		mainWindowPage.clickOnCreateButton();

		createPanePage.selectTransaction(CreateTransactionType.SYSTEM.getTypeString())
				.setFeePayerAccount(50)
				.setOperation(undelete)
				.setEntityID(fileID.num)
				.submit()
				.closePopup("CONTINUE")
				.closePopup("SUBMIT")
				.closePopup("CONTINUE");

		final var fileInfoRestored = new FileInfoQuery()
				.setFileId(fileID)
				.execute(client);
		assertFalse(fileInfoRestored.isDeleted);

		final var contentsUnDeleted = new FileContentsQuery()
				.setFileId(fileID)
				.execute(client);

		assertEquals(contentsByteString, contentsUnDeleted);

		final var finalHistory = Arrays.asList(new File(DEFAULT_HISTORY).list(
				(dir, name) -> !FilenameUtils.getExtension(name).equals(METADATA_EXTENSION)));

		assertEquals(2, finalHistory.size() - oldHistory.size());

		final var lastFile = finalHistory.stream().filter(file -> !newHistory.contains(file)).findFirst().orElse("");
		mainWindowPage.clickOnHistoryButton();
		historyPanePage.expandRow(lastFile);
		sleep(100);

		final var expandedBox2 = historyPanePage.getExpandedBox();
		final var labels2 = getLabels(expandedBox2.getChildren());
		assertTrue(labels2.contains("signed by"));
		assertTrue(labels2.contains("network response"));
		assertTrue(labels2.contains("success"));
	}

	@Test
	public void submitLargeFileUpdate() throws KeyStoreException, HederaClientException, PrecheckStatusException,
			TimeoutException, ReceiptStatusException, IOException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());


		final var keyStore =
				Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");
		final var genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());
		final var key = EncryptionUtils.jsonToKey(readJsonObject("src/test/resources/Keys/jsonKey.json"));
//		key.add(genesisKey);
//		key.setThreshold(1);

		final var originalContents = "text in the file";
		final var contentsByteString = ByteString.copyFromUtf8(originalContents);

		final var transactionResponse = new FileCreateTransaction()
				.setKeys(key)
				.setContents(originalContents)
				.execute(client);

		final var fileID = transactionResponse.getReceipt(client).fileId;

		final var contentsQueried = new FileContentsQuery()
				.setFileId(fileID)
				.execute(client);
		final var initialState = contentsQueried.toByteArray();

		assertArrayEquals(contentsByteString.toByteArray(), initialState);

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString())
				.setFeePayerAccount(accounts.get(0).num)
				.setTransactionFee(10)
				.setUpdateFileID(fileID.num)
				.setContents("src/test/resources/createTransactions/largeFileUpdate.zip")
				.setChunkSize(1000)
				.setInterval(1000000000)
				.submit()
				.closePopup("CONTINUE")
				.signWithPassword(TEST_PASSWORD)
				.closePopup("SUBMIT");

		Button button = null;
		while (button == null) {
			button = findButtonInPopup(getPopupNodes(), "CONTINUE");
		}
		clickOn(button);

		final var contentsUpdated = new FileContentsQuery()
				.setFileId(fileID)
				.execute(client);
		final var byteContents = contentsUpdated.toByteArray();

		final var file = new File("src/test/resources/createTransactions/largeFileUpdate.zip");
		try (final var fl = new FileInputStream(file)) {
			final var arr = new byte[(int) file.length()];
			fl.read(arr);
			assertArrayEquals(arr, byteContents);
		}
	}


	@Test
	public void nodeOutOfRangeFileUpdate_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString())
				.setFeePayerAccount(accounts.get(0).num)
				.setTransactionFee(10)
				.setUpdateFileID(123)
				.setNodeAccount(123)
				.setContents("src/test/resources/createTransactions/largeFileUpdate.zip")
				.setChunkSize(1000)
				.setInterval(1000000000);
		assertFalse(find("#invalidNode").isVisible());

		createPanePage.submit();
		assertTrue(find("#invalidNode").isVisible());

		createPanePage.setNodeAccount(3)
				.submit();
		assertFalse(find("#invalidNode").isVisible());

		createPanePage.closePopup("CANCEL");

	}


	@Test
	public void nodeOutOfRangeTransfer_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());


		createPanePage.selectTransaction(CreateTransactionType.TRANSFER.getTypeString())
				.setFeePayerAccount(accounts.get(0).toString())
				.addDebit(accounts.get(0).num, 1)
				.addCredit(accounts.get(1).num, 1)
				.setNodeAccount(16);
		assertFalse(find("#invalidNode").isVisible());

		createPanePage.submit();
		assertTrue(find("#invalidNode").isVisible());

		createPanePage.setNodeAccount(3)
				.submit();
		assertFalse(find("#invalidNode").isVisible());

		createPanePage.closePopup("CANCEL");

	}

	private void setupClient() throws KeyStoreException {
		// create payer account
		final var keyStore =
				Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");
		final var genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());

		client = CommonMethods.getClient(NetworkEnum.INTEGRATION);
		client.setOperator(new AccountId(0, 0, 2), genesisKey);
	}

	private void createAccounts(
			final int count) throws PrecheckStatusException, TimeoutException, ReceiptStatusException,
			HederaClientException {
		final var key = EncryptionUtils.jsonToKey(readJsonObject("src/test/resources/Keys/jsonKey.json"));
		for (var i = 0; i < count; i++) {
			final var transactionResponse = new AccountCreateTransaction()
					.setKey(key)
					.setInitialBalance(new Hbar(1000))
					.setAccountMemo("account_" + i)
					.setMaxAttempts(50)
					.execute(client, Duration.of(5, ChronoUnit.MINUTES));

			final var receipt = transactionResponse.getReceipt(client);
			final var accountId = Objects.requireNonNull(receipt.accountId);
			accounts.add(accountId);
			logger.info("Payer Id: {}", accountId.toString());

			final var accountInfo = new AccountInfoQuery()
					.setAccountId(accountId)
					.execute(client);

			logger.info("Account Balance = {}", accountInfo.balance.toString());
			writeBytes(String.format("%s/%s.info", ONE_DRIVE_INPUT, accountId), accountInfo.toBytes());
		}
	}

	private void getTreasury() throws TimeoutException, PrecheckStatusException, HederaClientException {
		final var treasuryInfo = new AccountInfoQuery()
				.setAccountId(new AccountId(2))
				.execute(client);
		writeBytes(String.format("%s/treasury.info", ONE_DRIVE_INPUT), treasuryInfo.toBytes());

		final var operatorInfo = new AccountInfoQuery()
				.setAccountId(new AccountId(50))
				.execute(client);
		writeBytes(String.format("%s/operator.info", ONE_DRIVE_INPUT), operatorInfo.toBytes());
	}

	private void setupUI() throws IOException, KeyStoreException, TimeoutException, PrecheckStatusException,
			HederaClientException {
		final var storage = new File(DEFAULT_STORAGE);
		if (storage.exists()) {
			FileUtils.cleanDirectory(storage);
			FileUtils.deleteDirectory(storage);
		}

		if (storage.mkdirs()) {
			logger.info("Transaction tools directory created");
		}

		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original"), storage);
		FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + KEYS_STRING));
		FileUtils.cleanDirectory(new File(DEFAULT_ACCOUNTS));

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");
		properties.setSetupPhase(SetupPhase.TEST_PHASE);

		final var currentRelativePath = Paths.get("");
		if (new File(currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory" +
				"/TransactionTools/Keys/").mkdirs()) {
			logger.info("Keys path created");
		}

		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		final var pathname =
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions/OutputFiles/test1" +
						".council2@hederacouncil.org/";

		if (new File(pathname).exists()) {
			FileUtils.deleteDirectory(new File(pathname));
		}

		if (new File(pathname).mkdirs()) {
			logger.info("Output directory created");
		}

		final Map<String, String> emailMap = new HashMap<>();
		emailMap.put(currentRelativePath.toAbsolutePath() + ONE_DRIVE, "test1.council2@hederacouncil.org");

		properties.setOneDriveCredentials(emailMap);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		TestBase.setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.aes"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));

		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
		}

		final var keys = new File("src/test/resources/Keys").listFiles();
		assert keys != null;
		for (final var key : keys) {
			FileUtils.copyFile(key, new File(KEYS_FOLDER, key.getName()));
		}

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

		getTreasury();

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		mainWindowPage = new MainWindowPage(this);
		homePanePage = new HomePanePage(this);

		acceptInfos();

		createPanePage = new CreatePanePage(this);
		historyPanePage = new HistoryPanePage(this);
	}

	private void acceptInfos() {
		var infos = homePanePage.getInfoCards();
		while (!infos.isEmpty()) {
			final var info = infos.get(0);
			final var button = findButtonInPopup(info.getChildren(), "ACCEPT");
			if (button.getText().equals("ACCEPT")) {
				clickOn(button);
				// This seems to break in headless mode, not sure why.
				homePanePage.clickOnPopupButton("ACCEPT");
			}
			infos = homePanePage.getInfoCards();
		}
	}


}
