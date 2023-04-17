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

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.testfx.api.FxRobotException;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_SYSTEM_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.HISTORY_MAP_JSON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.MAIN_TRANSACTIONS_SCROLLPANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NEW_FILES_VBOX;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;


public class HomePaneTest extends TestBase implements GenericFileReadWriteAware {

	protected static final String PRINCIPAL_TESTING_KEY = "principalTestingKey";
	protected static final String PASSWORD = "123456789";
	public static final int ONE_SECOND = 1000;

	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);

	private final List<VBox> publicKeyBoxes = new ArrayList<>();
	private final List<VBox> accountInfoBoxes = new ArrayList<>();
	private final List<VBox> batchBoxes = new ArrayList<>();
	private final List<VBox> transactionBoxes = new ArrayList<>();
	private final List<VBox> softwareBoxes = new ArrayList<>();
	private final List<VBox> systemBoxes = new ArrayList<>();
	private final List<VBox> freezeBoxes = new ArrayList<>();
	private final List<VBox> bundleBoxes = new ArrayList<>();
	private final Path currentRelativePath = Paths.get("");

	private HomePanePage homePanePage;
	private MainWindowPage mainWindowPage;


	private UserAccessibleProperties properties;

	@BeforeEach
	public void setUp() throws Exception {

		logger.info("Starting test class: {}", getClass().getSimpleName());
		TestUtil.buildFolders();

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

		final Map<String, String> emailMap = new HashMap<>();
		emailMap.put(
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties.setOneDriveCredentials(emailMap);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		//setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));

		final Controller controller = new Controller();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		final var historyMap = new File(DEFAULT_SYSTEM_FOLDER, HISTORY_MAP_JSON);
		Files.deleteIfExists(historyMap.toPath());

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		logger.info("Starting application");
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		homePanePage = new HomePanePage(this);
		mainWindowPage = new MainWindowPage(this);

		mainWindowPage.clickOnHistoryButton();
		mainWindowPage.clickOnHomeButton();

		logger.info("Counting boxes");
		initBoxes();

		logger.info("Starting test");
	}


	@Test
	public void clickOnBogusItem() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		assertThrows(FxRobotException.class, () -> clickOn("#exterminate"));
		sleep(100);
	}

	@Test
	public void uploadVersion_test() throws IOException, TimeoutException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final File outputDirectory = new File(currentRelativePath.toAbsolutePath().toString(),
				"src/test/resources/Transactions - Documents/OutputFiles/test1.council2@hederacouncil.org");
		if (outputDirectory.mkdirs()) {
			logger.info("Output directory created");
		}
		FileUtils.cleanDirectory(outputDirectory);
		properties.setSetupPhase(SetupPhase.TEST_PHASE);

		properties.setVersionString("bogus");
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		final var out = outputDirectory.listFiles();
		assert out != null;
		assertEquals(1, out.length);
		final var cleanVersion =
				properties.getVersionString().replace(":", "-").replace(".", "-").replace(",", "").replace(" ", "");
		assertEquals("SoftwareUpdated-" + cleanVersion, FilenameUtils.getBaseName(out[0].getName()));
		FileUtils.cleanDirectory(outputDirectory);


		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);
		final var out2 = outputDirectory.listFiles();
		assert out2 != null;
		assertEquals(0, out2.length);

	}

	@Test
	public void startHomePane_test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();
		assertEquals(totalBoxes, newFiles.size());
	}

	@Test
	public void verifySoftwareCard_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var storage = DEFAULT_STORAGE + KEYS_STRING;
		assertEquals(3, Objects.requireNonNull(new File(storage).listFiles(HomePaneTest::accept)).length);


		//The only one Software update box
		assertEquals(1, softwareBoxes.size());
		var n0 = (softwareBoxes.get(0)).getChildren().get(0);
		assertTrue(n0 instanceof Label);

		assertTrue(((Label) n0).getText().contains("Software"));


		// First has notes: 3 items and a hyperlink:
		final var vBox = softwareBoxes.get(0);
		assertEquals(3, vBox.getChildren().size());

		// Node 0 Label
		assertTrue(vBox.getChildren().get(0) instanceof Label);
		assertEquals("Software Update", ((Label) vBox.getChildren().get(0)).getText());

		// Node 1 VBox
		assertTrue(vBox.getChildren().get(1) instanceof HBox);
		final var children1 = ((HBox) vBox.getChildren().get(1)).getChildren();
		assertEquals(1, children1.size());

		assertTrue(children1.get(0) instanceof GridPane);
		final var gridPane = (GridPane) children1.get(0);

		final var children2 = gridPane.getChildren();

		n0 = children2.get(0);
		assertTrue(n0 instanceof Label);
		assertTrue(((Label) n0).getText().contains("9.4"));
		// Node 2: Label - "Highlights";
		n0 = children2.get(1);
		assertTrue(n0 instanceof Label);
		assertTrue(((Label) n0).getText().contains("Highlights"));
		// Nodes 3 to 5: Label - Items;
		n0 = children2.get(2);
		assertTrue(n0 instanceof Label);
		assertTrue(((Label) n0).getText().contains("Item 1"));
		n0 = children2.get(3);
		assertTrue(n0 instanceof Label);
		assertTrue(((Label) n0).getText().contains("Item 2"));
		n0 = children2.get(4);
		assertTrue(n0 instanceof Label);
		assertTrue(((Label) n0).getText().contains("Item 3"));
		// Node 6: Hbox - hyperlink;
		n0 = children2.get(5);
		assertTrue(n0 instanceof HBox);
		assertTrue(((HBox) n0).getChildren().get(1) instanceof Hyperlink);
		// Node 7: Hbox - Label;
		n0 = children2.get(6);
		assertTrue(n0 instanceof HBox);
		assertTrue(((HBox) n0).getChildren().get(1) instanceof Text);
		assertEquals("38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b",
				((Text) ((HBox) n0).getChildren().get(1)).getText().replace("\u00A0", "").replace("\n", ""));

		assertTrue(vBox.getChildren().get(2) instanceof VBox);
		n0 = ((VBox) vBox.getChildren().get(2)).getChildren().get(0);
		assertTrue(n0 instanceof Button);
		assertEquals("UPDATE", ((Button) n0).getText());
	}

	@Test
	public void findPublicKeysAndAcceptOne_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();


		final var storage = DEFAULT_STORAGE + KEYS_STRING;
		assertEquals(3, Objects.requireNonNull(new File(storage).listFiles(HomePaneTest::accept)).length);
		final var index = 0;

		sleep(ONE_SECOND);

		homePanePage.clickOn2ButtonBar(index, publicKeyBoxes.get(1));

		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		assertEquals(totalBoxes - 1, refreshFiles.size());

		final var historyMap = readJsonArray(DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON);
		assertEquals(1, historyMap.size());

	}

	@Test
	public void findPublicKeysAndRejectOne_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();

		sleep(ONE_SECOND);
		final var storage = DEFAULT_STORAGE + "Keys";
		assertEquals(3, Objects.requireNonNull(new File(storage).listFiles(HomePaneTest::accept)).length);

		homePanePage.clickOn2ButtonBar(1, publicKeyBoxes.get(1));

		sleep(ONE_SECOND);

		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		assertEquals(totalBoxes - 1, refreshFiles.size());

		final var historyMap = readJsonArray(DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON);
		assertEquals(1, historyMap.size());

	}

	@Test
	@Disabled("Test files are old, and need to be updated")
	public void findAccountInfosAndAcceptOne_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();

		final var storage = DEFAULT_STORAGE + File.separator + "Accounts";
		assertEquals(10,
				Objects.requireNonNull(new File(storage).listFiles(File::isFile)).length);

		sleep(ONE_SECOND);
		homePanePage.clickOn2ButtonBar(0, accountInfoBoxes.get(1));

		sleep(ONE_SECOND);

		homePanePage.enterStringInPopup("testAccount");
		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();

		assertEquals(totalBoxes - 1, refreshFiles.size());
		assertEquals(12,
				Objects.requireNonNull(new File(storage).listFiles(File::isFile)).length);
	}

	@Test
	public void findAccountInfosAndRejectOne_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();

		final var storage = DEFAULT_STORAGE + File.separator + "Accounts";
		assertEquals(10,
				Objects.requireNonNull(new File(storage).listFiles(File::isFile)).length);
		homePanePage.clickOn2ButtonBar(1, accountInfoBoxes.get(1));

		final var map = readJsonArray(DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON);

		assertEquals(1, map.size());
		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();

		assertEquals(totalBoxes - 1, refreshFiles.size());
		assertEquals(10, Objects.requireNonNull(new File(storage).listFiles(File::isFile)).length);

	}

	@Test
	public void acceptBatchTransaction_Test() throws IOException, HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		sleep(ONE_SECOND);
		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();

		final var gridPane = ((GridPane) ((HBox) batchBoxes.get(1).getChildren().get(1)).getChildren().get(0));
		assertEquals(8, gridPane.getRowCount());
		assertEquals(2, gridPane.getColumnCount());

		final var c2 = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
		c2.set(2029, Calendar.SEPTEMBER, 17, 19, 30, 0);
		final var sdf = new SimpleDateFormat("HH:mm:ss");
		final var localTime =
				sdf.format(c2.getTime()) + " " + TimeZone.getDefault().getDisplayName(false, TimeZone.SHORT);

		for (final var n :
				gridPane.getChildren()) {
			if (n instanceof Label) {
				final var text = ((Label) n).getText();
				if (text.contains("UTC")) {
					assertTrue(text.contains("19:30:00"));
					assertTrue(text.contains(localTime));

				}
			}
		}

		final var children = (batchBoxes.get(1)).getChildren();
		final var sign = TestUtil.findButtonInPopup(children, "SIGN\u2026");
		final var addMore = TestUtil.findButtonInPopup(children, "ADD MORE");

		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), children.get(2));
		clickOn(addMore);

		//ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), transactionBoxes.get(1).getChildren().get(2));

		homePanePage.clickOnKeyCheckBox(PRINCIPAL_TESTING_KEY);

		final var acceptButton = TestUtil.findButtonInPopup(Objects.requireNonNull(TestUtil.getPopupNodes()), "ACCEPT");
		clickOn(acceptButton);

		assert sign != null;
		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), sign);

		clickOn(sign);
		homePanePage.enterPasswordInPopup(PASSWORD)
				.waitForWindow();

		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();

		assertEquals(totalBoxes - 1, refreshFiles.size());

		final var out =
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org/";

		if (new File(out).mkdirs()) {
			logger.info("Output directory created");
		}

		final var zips = findByStringExtension(new File(out), "zip");
		assertEquals(4, zips.size());

		final var zip1 = zips.get(0);
		unzip(zip1);
		final var ext = new String[] { "tx", "txt", "sigpair" };
		final var output1 = new File(zip1.getAbsolutePath().replace(".zip", ""));
		final var transactionFiles = FileUtils.listFiles(output1, ext, false);
		assertEquals(22, transactionFiles.size());

		final var zip2 = zips.get(0);
		unzip(zip2);
		final var output2 = new File(zip1.getAbsolutePath().replace(".zip", ""));
		final var signatureFiles = FileUtils.listFiles(output2, ext, false);
		assertEquals(22, signatureFiles.size());

	}

	@Test
	public void declineBatchTransaction_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());
		sleep(ONE_SECOND);
		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();

		final var children = (batchBoxes.get(1)).getChildren();

		final var reject = TestUtil.findButtonInPopup(children, "DECLINE");

		assert reject != null;
		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), reject);

		clickOn(reject);

		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		assertEquals(totalBoxes - 1, refreshFiles.size());

		final var historyMap = readJsonArray(DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON);
		assertEquals(1, historyMap.size());

	}

	@Test
	@Disabled("Test files are old, and need to be updated")
	public void acceptTransaction_Test() throws IOException, HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var out =
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org/";

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();

		// Check the time and local time are correct
		int k = 0;
		boolean found = false;
		while (k < transactionBoxes.size()) {
			final var gridPane =
					((GridPane) ((HBox) transactionBoxes.get(k).getChildren().get(1)).getChildren().get(0));
			final var timestamp = new Timestamp(1675214610, 0);
			final var localDateTime = timestamp.asReadableLocalString();
			final var utcDateTime = timestamp.asUTCString().replace("_", " ");
			for (final var n : gridPane.getChildren()) {
				if (n instanceof Label) {
					final var text = ((Label) n).getText();
					if (text.contains("UTC") && (text.contains(utcDateTime)) && (text.contains(localDateTime))) {
						found = true;
						break;
					}
				}
			}
			if (found) {
				break;
			}
			k++;
		}

		assertTrue(found);
		final var children = (transactionBoxes.get(k)).getChildren();
		final var sign = TestUtil.findButtonInPopup(children, "SIGN\u2026");
		final var addMore = TestUtil.findButtonInPopup(children, "ADD MORE");

		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), children.get(2));
		clickOn(addMore);


		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), transactionBoxes.get(k).getChildren().get(2));

		homePanePage.clickOnKeyCheckBox(PRINCIPAL_TESTING_KEY);

		final var acceptButton = TestUtil.findButtonInPopup(Objects.requireNonNull(TestUtil.getPopupNodes()), "ACCEPT");
		clickOn(acceptButton);

		assert sign != null;
		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), sign);

		clickOn(sign);
		homePanePage.enterPasswordInPopup(PASSWORD)
				.waitForWindow();

		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		assertEquals(totalBoxes - 1, refreshFiles.size());

		final var listFiles = new File(out).listFiles();
		final var listZips = new File(out).listFiles((dir, name) -> name.endsWith(".zip"));

		assert listFiles != null;
		assert listZips != null;

		assertEquals(1, listZips.length);

		final var zip = listFiles[0];
		unzip(zip);

		final var output = new File(zip.getAbsolutePath().replace(".zip", ""));
		final var transactionFiles = output.listFiles();
		assert transactionFiles != null;
		assertEquals(2, transactionFiles.length);
	}

	@Test
	@Disabled("Test files are old, and need to be updated")
	public void declineTransaction_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();

		final var children = (transactionBoxes.get(1)).getChildren();
		final var reject = TestUtil.findButtonInPopup(children, "DECLINE");

		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), transactionBoxes.get(1).getChildren().get(2));

		clickOn(reject);

		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		assertEquals(totalBoxes - 1, refreshFiles.size());
		final var historyMap = readJsonArray(DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON);
		assertEquals(1, historyMap.size());

	}

	@Test
	public void freezeTransactions_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		assertEquals(4, freezeBoxes.size());
		VBox abort = null;
		VBox freeze = null;
		VBox prepare = null;
		VBox freezeUpgrade = null;

		for (final VBox freezeBox : freezeBoxes) {
			final var children = freezeBox.getChildren();
			assertTrue(children.get(0) instanceof Label);
			switch (((Label) children.get(0)).getText()) {
				case "Abort Freeze Transaction":
					abort = freezeBox;
					break;
				case "Freeze Only Transaction":
					freeze = freezeBox;
					break;
				case "Prepare Upgrade Transaction":
					prepare = freezeBox;
					break;
				case "Freeze and Upgrade Transaction":
					freezeUpgrade = freezeBox;
					break;
				default:
					throw new IllegalStateException("Unexpected value: " + ((Label) children.get(0)).getText());
			}
		}
		assertNotNull(abort);
		assertNotNull(freeze);
		assertNotNull(prepare);
		assertNotNull(freezeUpgrade);

		final var abortHBox = (HBox) abort.getChildren().get(1);
		final var freezeHBox = (HBox) freeze.getChildren().get(1);
		final var prepareHBox = (HBox) prepare.getChildren().get(1);
		final var freezeUpgradeHBox = (HBox) freezeUpgrade.getChildren().get(1);

		assertEquals(2, abortHBox.getChildren().size());
		assertEquals(2, freezeHBox.getChildren().size());
		assertEquals(2, prepareHBox.getChildren().size());
		assertEquals(2, freezeUpgradeHBox.getChildren().size());

		assertTrue(abortHBox.getChildren().get(0) instanceof GridPane);
		assertTrue(freezeHBox.getChildren().get(0) instanceof GridPane);
		assertTrue(prepareHBox.getChildren().get(0) instanceof GridPane);
		assertTrue(freezeUpgradeHBox.getChildren().get(0) instanceof GridPane);

		assertEquals(12, ((GridPane) abortHBox.getChildren().get(0)).getChildren().size());
		assertEquals(14, ((GridPane) freezeHBox.getChildren().get(0)).getChildren().size());
		assertEquals(14, ((GridPane) prepareHBox.getChildren().get(0)).getChildren().size());
		assertEquals(18, ((GridPane) freezeUpgradeHBox.getChildren().get(0)).getChildren().size());
	}

	//@Test
	public void acceptSystemTransaction_Test() throws IOException, HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		sleep(ONE_SECOND);
		final var out =
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org/";

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		final var totalBoxes = newFiles.size();
		final var children = systemBoxes.get(1).getChildren();

		// Check the time and local time are correct

		final var gridPane = ((GridPane) ((HBox) children.get(1)).getChildren().get(0));
		final var timestamp = new Timestamp(1659724521, 0);
		final var localDateTime = timestamp.asReadableLocalString();
		final var utcDateTime = timestamp.asUTCString().replace("_", " ");
		final var expirationTimestamp = new Timestamp(1685348108, 0);
		final var expirationLocalDateTime = expirationTimestamp.asReadableLocalString();
		final var expirationUtcDateTime = expirationTimestamp.asUTCString().replace("_", " ");

		for (final var n : gridPane.getChildren()) {
			if (n instanceof Label) {
				final var text = ((Label) n).getText();
				if (text.contains("UTC")) {
					assertTrue(text.contains(utcDateTime) || text.contains(expirationUtcDateTime));
					assertTrue(text.contains(localDateTime) || text.contains(expirationLocalDateTime));

				}
			}
		}


		final var sign = TestUtil.findButtonInPopup(children, "SIGN\u2026");
		final var addMore = TestUtil.findButtonInPopup(children, "ADD MORE");

		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), children.get(2));
		clickOn(addMore);


		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), transactionBoxes.get(1).getChildren().get(2));

		homePanePage.clickOnKeyCheckBox(PRINCIPAL_TESTING_KEY);

		final var acceptButton = TestUtil.findButtonInPopup(Objects.requireNonNull(TestUtil.getPopupNodes()), "ACCEPT");
		clickOn(acceptButton);

		assert sign != null;
		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), sign);

		clickOn(sign);
		homePanePage.enterPasswordInPopup(PASSWORD)
				.waitForWindow();

		final var refreshFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();

		assertEquals(totalBoxes - 1, refreshFiles.size());

		final var listFiles = new File(out).listFiles();
		final var listZips = new File(out).listFiles((dir, name) -> name.endsWith(".zip"));

		assert listFiles != null;
		assert listZips != null;

		assertEquals(1, listZips.length);

		final var zip = listZips[0];
		unzip(zip);
		final var ext = new String[] { "sig", "tx" };
		final var output = new File(zip.getAbsolutePath().replace(".zip", ""));
		final var transactionFiles = FileUtils.listFiles(output, ext, false);
		assertEquals(2, transactionFiles.size());

		final var zip2 = findByStringExtension(output, "sig");
		assertEquals(1, zip2.size());
	}

	@Test
	public void nicknameExists_Test() {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		final var nicknames = findAll("0.0.70-bvqyx");
		sleep(ONE_SECOND);
		final var nicknameLabels = new HashSet<Label>();
		for (final var nickname : nicknames) {
			if (nickname instanceof Label) {
				nicknameLabels.add((Label) nickname);
			}
		}
		assertEquals(2, nicknameLabels.size()); // sender and fee payer

		final var badNicknames = findAll("badNick");
		assertEquals(0, badNicknames.size());

	}

	@Test
	public void nicknameNegative_Tests() throws TimeoutException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		mainWindowPage.clickOnAccountsButton();
		sleep(ONE_SECOND);
		clickOn("ninetyFourT");

		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var continueButton = TestUtil.findButtonInPopup(popupNodes, "CONTINUE");

		assertNotNull(continueButton);

		clickOn(continueButton);
		// restart app
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		final var nicknames = findAll("nineFour (0.0.94-ioaex)");
		assertEquals(0, nicknames.size());
	}

	@Test
	@Disabled("Test files are old, and need to be updated")
	public void transactionSignHistory_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		boolean found = false;
		int k = 0;

		// Check the time and local time are correct
		while (k < transactionBoxes.size()) {
			final var gridPane =
					((GridPane) ((HBox) transactionBoxes.get(k).getChildren().get(1)).getChildren().get(0));
			final var timestamp = new Timestamp(1675214610, 0);
			final var localDateTime = timestamp.asReadableLocalString();
			final var utcDateTime = timestamp.asUTCString().replace("_", " ");

			for (final var n : gridPane.getChildren()) {
				if (n instanceof Label) {
					final var text = ((Label) n).getText();
					if (text.contains("UTC") && (text.contains(utcDateTime)) && (text.contains(localDateTime))) {
						found = true;
						break;
					}
				}
			}
			if (found) {
				break;
			}
			k++;
		}

		assertTrue(found);

		//DECLINE
		final var children = (transactionBoxes.get(k)).getChildren();
		final var reject = TestUtil.findButtonInPopup(children, "DECLINE");

		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), reject);

		moveTo(reject);
		sleep(ONE_SECOND);
		clickOn(reject);

		// make sure history order is correct
		final var historyMap = readJsonArray(DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON);
		assertTrue(historyMap.size() > 0);
		final var transactionHistory = historyMap.get(0).getAsJsonObject();
		final var actions = transactionHistory.get("actions").getAsJsonArray();
		assertEquals(3, actions.size());
		final var element0 = actions.get(0);
		assertTrue(element0.getAsString().toUpperCase(Locale.ROOT).contains("DECLINE"));
		final var element1 = actions.get(1);
		assertTrue(element1.getAsString().toUpperCase(Locale.ROOT).contains("ACCEPT"));
		final var element2 = actions.get(2);
		assertTrue(element2.getAsString().toUpperCase(Locale.ROOT).contains("DECLINE"));
		logger.info("Done");

	}

	@Test
	@Disabled("Test files are old, and need to be updated")
	public void transactionCancel_Test() throws HederaClientException {
		final var walker = StackWalker.getInstance();
		final var methodName = walker.walk(frames -> frames
				.findFirst()
				.map(StackWalker.StackFrame::getMethodName));

		assertTrue(methodName.isPresent());
		logger.info("Starting test method: {}", methodName.get());

		boolean found = false;
		int k = 0;

		// Check the time and local time are correct
		while (k < transactionBoxes.size()) {
			final var gridPane =
					((GridPane) ((HBox) transactionBoxes.get(k).getChildren().get(1)).getChildren().get(0));
			final var timestamp = new Timestamp(1675214610, 0);
			final var localDateTime = timestamp.asReadableLocalString();
			final var utcDateTime = timestamp.asUTCString().replace("_", " ");

			for (final var n : gridPane.getChildren()) {
				if (n instanceof Label) {
					final var text = ((Label) n).getText();
					if (text.contains("UTC") && (text.contains(utcDateTime)) && (text.contains(localDateTime))) {
						found = true;
						break;
					}
				}
			}
			if (found) {
				break;
			}
			k++;
		}

		assertTrue(found);

		//DECLINE
		final var children = (transactionBoxes.get(k)).getChildren();
		final var cancel = TestUtil.findButtonInPopup(children, "CANCEL");

		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), cancel);

		moveTo(cancel);
		sleep(ONE_SECOND);
		clickOn(cancel);

		// make sure history order is correct
		final var historyMap = readJsonArray(DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON);
		assertTrue(historyMap.size() > 0);
		final var transactionHistory = historyMap.get(0).getAsJsonObject();
		final var actions = transactionHistory.get("actions").getAsJsonArray();
		assertEquals(2, actions.size());
		final var element0 = actions.get(0);
		assertTrue(element0.getAsString().toUpperCase(Locale.ROOT).contains("DECLINE"));
		final var element1 = actions.get(1);
		assertTrue(element1.getAsString().toUpperCase(Locale.ROOT).contains("ACCEPT"));
		logger.info("Done");
	}

	private void initBoxes() {
		publicKeyBoxes.clear();
		accountInfoBoxes.clear();
		batchBoxes.clear();
		transactionBoxes.clear();
		softwareBoxes.clear();
		systemBoxes.clear();
		freezeBoxes.clear();
		bundleBoxes.clear();

		final var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();

		logger.info("========>>> {}", newFiles.size());
		separateBoxes(newFiles, publicKeyBoxes, accountInfoBoxes, batchBoxes, transactionBoxes, softwareBoxes,
				systemBoxes, freezeBoxes, bundleBoxes);

		assertEquals(newFiles.size(),
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size()
						+ softwareBoxes.size() + systemBoxes.size() + freezeBoxes.size() + bundleBoxes.size());
	}

	private List<File> findByStringExtension(final File dir, final String ext) {
		final var extensions = new String[] { ext };
		return new ArrayList<>(FileUtils.listFiles(dir, extensions, false));
	}

	private void separateBoxes(final ObservableList<Node> newFiles,
			final List<VBox> publicKeyBoxes,
			final List<VBox> accountInfoBoxes,
			final List<VBox> batchBoxes,
			final List<VBox> transactionBoxes,
			final List<VBox> softwareBoxes,
			final List<VBox> systemBoxes,
			final List<VBox> freezeBoxes,
			final List<VBox> bundleBoxes) {
		for (final var box : newFiles) {
			assertTrue(box instanceof VBox);

			final var lines = ((VBox) box).getChildren();
			if (lines.size() >= 3) {
				assertTrue(lines.get(0) instanceof Label);
				final var l = ((Label) lines.get(0)).getText();
				if (l.contains("Batch")) {
					batchBoxes.add((VBox) box);
				} else if (l.contains("Transaction") && !(l.contains("ZippedTransactions") || l.contains(
						"Freeze") || l.contains("Upgrade"))) {
					transactionBoxes.add((VBox) box);
				} else if (l.contains("Account Information")) {
					accountInfoBoxes.add((VBox) box);
				} else if (l.contains("Public Key")) {
					publicKeyBoxes.add((VBox) box);
				} else if (l.contains("Software")) {
					softwareBoxes.add((VBox) box);
				} else if (l.contains("Restore") || l.contains("Remove")) {
					systemBoxes.add((VBox) box);
				} else if (l.contains("Freeze") || l.contains("Upgrade")) {
					freezeBoxes.add((VBox) box);
				} else if (l.contains("Information")) {
					bundleBoxes.add((VBox) box);
				} else {
					logger.info("here");
				}
			}
		}
	}

	private static void ensureVisible(final ScrollPane scrollPane, final Node node) {
		final var viewport = scrollPane.getViewportBounds();
		final var contentHeight =
				scrollPane.getContent().localToScene(scrollPane.getContent().getBoundsInLocal()).getHeight();
		final var nodeMinY = node.localToScene(node.getBoundsInLocal()).getMinY();
		final var nodeMaxY = node.localToScene(node.getBoundsInLocal()).getMaxY();

		double vValueDelta = 0;
		final var vValueCurrent = scrollPane.getVvalue();

		if (nodeMaxY < 0) {
			// located above (remember, top left is (0,0))
			vValueDelta = (nodeMinY - viewport.getHeight()) / contentHeight;
		} else if (nodeMinY > viewport.getHeight()) {
			// located below
			vValueDelta = (nodeMinY) / contentHeight;
		}
		scrollPane.setVvalue(vValueCurrent + vValueDelta);
	}

	private void unzip(final File zip) throws IOException {
		final var fileZip = zip.getAbsolutePath();
		final var destDir = new File(fileZip.replace(".zip", ""));
		if (destDir.mkdirs()) {
			logger.info("Destination directory created");
		}
		final var buffer = new byte[1024];
		final var zis = new ZipInputStream(new FileInputStream(fileZip));
		var zipEntry = zis.getNextEntry();
		while (zipEntry != null) {
			final var newFile = newFile(destDir, zipEntry);
			final var fos = new FileOutputStream(newFile);
			int len;
			while ((len = zis.read(buffer)) > 0) {
				fos.write(buffer, 0, len);
			}
			fos.close();
			zipEntry = zis.getNextEntry();
		}
		zis.closeEntry();
		zis.close();
	}

	private File newFile(final File destinationDir, final ZipEntry zipEntry) throws IOException {
		final var destFile = new File(destinationDir, zipEntry.getName());

		final var destDirPath = destinationDir.getCanonicalPath();
		final var destFilePath = destFile.getCanonicalPath();

		if (!destFilePath.startsWith(destDirPath + File.separator)) {
			throw new IOException("Entry is outside of the target dir: " + zipEntry.getName());
		}

		return destFile;
	}

	@AfterEach
	public void tearDown() throws IOException, TimeoutException {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();

		publicKeyBoxes.clear();
		accountInfoBoxes.clear();
		batchBoxes.clear();
		transactionBoxes.clear();
		softwareBoxes.clear();
		systemBoxes.clear();
		freezeBoxes.clear();

		final var currentRelativePath = Paths.get("");
		final var s = currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory";
		if ((new File(s)).exists()) {
			FileUtils.deleteDirectory(new File(s));
		}

		final var out =
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org";
		if (new File(out).exists()) {
			FileUtils.cleanDirectory(new File(out));
		}
		final var historyMap = new File(DEFAULT_SYSTEM_FOLDER, HISTORY_MAP_JSON);
		Files.deleteIfExists(historyMap.toPath());

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

	}

	private static boolean accept(final File pathname) {
		return pathname.isFile() && !pathname.getName().contains(".DS");
	}

}
