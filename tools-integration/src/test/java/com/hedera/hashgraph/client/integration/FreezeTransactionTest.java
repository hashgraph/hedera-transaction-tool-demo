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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.JsonArray;
import com.google.gson.JsonIOException;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.integration.pages.AccountsPanePage;
import com.hedera.hashgraph.client.integration.pages.CreatePanePage;
import com.hedera.hashgraph.client.integration.pages.HomePanePage;
import com.hedera.hashgraph.client.integration.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.client.ui.StartUI;
import com.hedera.hashgraph.client.ui.utilities.CreateTransactionType;
import com.hedera.hashgraph.client.ui.utilities.Utilities;
import com.hedera.hashgraph.sdk.AccountBalanceQuery;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.BadMnemonicException;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.FreezeTransaction;
import com.hedera.hashgraph.sdk.FreezeType;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.Mnemonic;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransferTransaction;
import javafx.scene.layout.VBox;
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
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.toEncryptedFile;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.NEW_FILES_VBOX;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * WARNING: THIS TEST CLASS SHOULD NOT BE RUN DURING AUTOMATIC TESTING. CLASS DEPENDS ON A LOCAL NETWORK THAT IS NOT
 * GUARANTEED TO EXIST
 */

public class FreezeTransactionTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(FreezeTransactionTest.class);

	private static Client client;
	private static PrivateKey genesisKey;

	private final String resources = new File("src/test/resources/Transactions - Documents/").getAbsolutePath().replace(
			System.getProperty("user.home") + "/", "") + "/";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

	private CreatePanePage createPanePage;
	private AccountsPanePage accountsPanePage;
	private HomePanePage homePanePage;
	private MainWindowPage mainWindowPage;

	private final List<VBox> publicKeyBoxes = new ArrayList<>();
	private final List<VBox> accountInfoBoxes = new ArrayList<>();
	private final List<VBox> batchBoxes = new ArrayList<>();
	private final List<VBox> transactionBoxes = new ArrayList<>();
	private final List<VBox> softwareBoxes = new ArrayList<>();
	private final List<VBox> systemBoxes = new ArrayList<>();
	private final List<VBox> freezeBoxes = new ArrayList<>();

	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	public UserAccessibleProperties properties;

	private static final List<String> testWords =
			Arrays.asList("dignity", "domain", "involve", "report",
					"sail", "middle", "rhythm", "husband",
					"usage", "pretty", "rate", "town",
					"account", "side", "extra", "outer",
					"eagle", "eight", "design", "page",
					"regular", "bird", "race", "answer");

	public static final String KEYS_STRING = "Keys";
	public static final String ACCOUNTS_STRING = "Accounts";

	public TestBase get() {
		return this;
	}

	@Before
	public void setUp() throws Exception {
		// Set up the client for network
//		setupClient();

		// Fund account 58
//		fundAccount58();

		setUpUI();

		FileUtils.cleanDirectory(new File("src/test/resources/Transactions - Documents/InputFiles"));
		FileUtils.cleanDirectory(
				new File("src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org"));

	}

	@Test
	public void freezeTransactionBuild_test() throws PrecheckStatusException, TimeoutException {
		// Check 58 has a balance
		Hbar balance = new AccountBalanceQuery()
				.setAccountId(new AccountId(0, 0, 58))
				.execute(client)
				.hbars;

		assertTrue(balance.toTinybars() > 0);

		// Create a FREEZE transaction
		final var transactionValidStart = Instant.now();
		final var startFreeze = new Timestamp(Instant.now().plusSeconds(100));

		TransactionId transactionId = new TransactionId(new AccountId(0, 0, 58), transactionValidStart);
		FreezeTransaction freezeTransaction = new FreezeTransaction().setTransactionId(transactionId)
				.setFreezeType(FreezeType.FREEZE_ONLY)
				.setStartTime(startFreeze.asInstant())
				.setMaxTransactionFee(Hbar.fromTinybars(100000000))
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.freeze();

		freezeTransaction.sign(genesisKey);

		client.setOperator(new AccountId(0, 0, 58), genesisKey);
		var response = freezeTransaction.execute(client);
		assert response != null;
		// Assert network is down
		balance = new AccountBalanceQuery()
				.setAccountId(new AccountId(0, 0, 58))
				.execute(client)
				.hbars;
	}

	@Test
	public void integration_test() throws KeyStoreException, IOException, TimeoutException, InterruptedException,
			HederaClientException {
		// Launch tools
		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);
		mainWindowPage.clickOnCreateButton();

		// Create a zip transaction
		var date = DateUtils.addMinutes(new Date(), 2);
		var file = new File("src/test/resources/hundredThousandBytes.zip").getAbsolutePath();
		createPanePage.selectTransaction(CreateTransactionType.FILE_UPDATE.getTypeString())
				.setUpdateFileID("0.0.150")
				.setStartDateTime(date)
				.setMemo("Freeze integration test")
				.setFeePayerAccount(58)
				.setChunkSize(1024)
				.setInterval(100000000)
				.setContents(file);
		createPanePage.createAndExport(resources).clickOnPopupButton("CONTINUE");

		final var txtFile = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil" +
						".org/hundredThousandBytes.txt");
		assertTrue(txtFile.exists());
		final var zipFile = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil" +
						".org/hundredThousandBytes.zip");
		assertTrue(zipFile.exists());

		//Close app
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();


		// Sign zip

		final var destTxt = new File("src/test/resources/Transactions - Documents/InputFiles/hundredThousandBytes.txt");
		final var destZip = new File("src/test/resources/Transactions - Documents/InputFiles/hundredThousandBytes.zip");
		Files.deleteIfExists(destTxt.toPath());
		Files.deleteIfExists(destZip.toPath());

		FileUtils.moveFile(txtFile, destTxt);
		FileUtils.moveFile(zipFile, destZip);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		signSingleBox();

		// Collate and Submit file update
		sleep(50000);
		// Create prepare upgrade

		// Sign transaction

		// Submit transaction

	}

	private void signSingleBox() throws HederaClientException {
		var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		assertEquals(1, newFiles.size());

		final var children = ((VBox) newFiles.get(0)).getChildren();
		var sign = TestUtil.findButtonInPopup(children, "SIGN\u2026");
		var addMore = TestUtil.findButtonInPopup(children, "ADD MORE");
		clickOn(addMore);

		homePanePage.clickOnKeyCheckBox("genesis");
		var acceptButton = TestUtil.findButtonInPopup(Objects.requireNonNull(TestUtil.getPopupNodes()), "ACCEPT");
		clickOn(acceptButton);
		clickOn(sign);
		homePanePage.enterPasswordInPopup("123456789");
	}

	@After
	public void tearDown() throws Exception {
		FileUtils.cleanDirectory(new File("src/test/resources/Transactions - Documents/InputFiles"));
		FileUtils.cleanDirectory(
				new File("src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org"));
	}

	/**
	 * Read the integration file into a json array
	 *
	 * @return a json array that contains the IPs of the integration network
	 */
	private static JsonArray getIPS(String nodes) {
		// Read file into object
		try (var file = new FileReader(nodes)) {
			return JsonParser.parseReader(file).getAsJsonArray();
		} catch (JsonIOException | JsonSyntaxException | IOException cause) {
			logger.error(cause);
		}
		return new JsonArray();
	}

	public void remakeTransactionTools() {
		String toolsFolder =
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents/TransactionTools";
		if (!(new File(toolsFolder)).exists() && new File(toolsFolder).mkdirs()) {
			logger.info("Folder {} created", toolsFolder);
		}

		try {
			if (!new File(toolsFolder, ACCOUNTS_STRING).exists() &&
					new File(toolsFolder, ACCOUNTS_STRING).mkdirs()) {
				logger.info("Accounts folder created");
			}
			if (!new File(toolsFolder, KEYS_STRING).exists() &&
					new File(toolsFolder, KEYS_STRING).mkdirs()) {
				logger.info("{} folder created", KEYS_STRING);
			}
		} catch (Exception cause) {
			logger.error("Unable to remake Transaction folders.", cause);
		}
	}

	private void setUpUI() throws IOException, BadMnemonicException, HederaClientException, KeyStoreException {
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

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.aes"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));

		Map<String, String> emailMap = new HashMap<>();

		emailMap.put(
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		var mnemonic = Mnemonic.fromWords(testWords);
		properties.setMnemonicHashCode(mnemonic.words.hashCode());
		properties.setHash(TEST_PASSWORD.toCharArray());
		properties.setLegacy(false);
		var salt = Utilities.getSaltBytes(properties);
		var passwordBytes = SecurityUtilities.keyFromPassword(TEST_PASSWORD.toCharArray(), salt);
		toEncryptedFile(passwordBytes, Constants.DEFAULT_STORAGE + File.separator + Constants.MNEMONIC_PATH,
				mnemonic.toString());

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		var objectMapper = new ObjectMapper();
		var mapAsString = objectMapper.writeValueAsString(emailMap);
		assertNotNull(mapAsString);

		properties.setOneDriveCredentials(emailMap);
		properties.setHash("123456789".toCharArray());

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		setupTransactionDirectory(DEFAULT_STORAGE);

		var controller = new Controller();
		var version = controller.getVersion();
		properties.setVersionString(version);

		FileUtils.copyFile(new File("src/test/resources/KeyFiles/genesis.pem"),
				new File(DEFAULT_STORAGE + "/Keys/genesis.pem"));
		FileUtils.copyFile(new File("src/test/resources/KeyFiles/genesis.pub"),
				new File(DEFAULT_STORAGE + "/Keys/genesis.pub"));

		if (createPanePage == null) {
			createPanePage = new CreatePanePage(this);
		}
		if (mainWindowPage == null) {
			mainWindowPage = new MainWindowPage(get());
		}
		if (accountsPanePage == null) {
			accountsPanePage = new AccountsPanePage(get());
		}
		if (homePanePage == null) {
			homePanePage = new HomePanePage(get());
		}
		var rootFolder = new JFileChooser().getFileSystemView().getDefaultDirectory().toString();
		if (!new File(rootFolder).exists() && new File(rootFolder).mkdirs()) {
			logger.info("Tools root folder created");
		} else {
			logger.info("Tools root directory exists.");
		}
		var toolsFolder = new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents";
		if (!new File(toolsFolder).exists() && new File(toolsFolder).mkdirs()) {
			logger.info("Tools document directory created");
		} else {
			logger.info("Tools document directory exists.");
		}

		final var outputDirectory = new File(
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org");
		FileUtils.cleanDirectory(outputDirectory);

		TestUtil.copyCreatePaneKeys();
	}

	private void fundAccount58() throws TimeoutException, PrecheckStatusException, ReceiptStatusException {
		var transferTransaction = new TransferTransaction();
		final var transactionValidStart = Instant.now();
		TransactionId transactionId = new TransactionId(new AccountId(0, 0, 2), transactionValidStart);
		transferTransaction.setMaxTransactionFee(Hbar.fromTinybars(1000000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("test transfer")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.setTransactionValidDuration(Duration.ofSeconds(175));

		// Add transfers
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 2), Hbar.fromTinybars(-100000000));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 58), Hbar.fromTinybars(100000000));
		transferTransaction.freeze();
		transferTransaction.sign(genesisKey);
		var response = transferTransaction.execute(client);
		assert response != null;

		final var status = response.getReceipt(client).status;
		logger.info("Worker: Transaction: {}, final status: {}", transactionId.toString(), status);
	}

	private void setupClient() throws KeyStoreException {
		var keyStore =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/KeyFiles/genesis.pem");
		genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());

		Map<String, AccountId> network = new HashMap<>();
		var jsonArray = getIPS("src/test/resources/homeNodes.json");
		for (var jsonElement : jsonArray) {
			var node = jsonElement.getAsJsonObject();
			network.put(
					String.format("%s:%s", node.get("ipAddress").getAsString(), node.get("port").getAsString()),
					new AccountId(node.get("accountID").getAsInt()));
		}
		client = Client.forNetwork(network);
		final var treasury = new AccountId(0, 0, 2);
		client.setOperator(treasury, genesisKey);
	}


}
