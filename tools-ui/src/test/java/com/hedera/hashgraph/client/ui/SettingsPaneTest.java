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
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.SettingsPanePage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.ToggleSwitch;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxRobotException;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.AUTO_RENEW_PERIOD_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.GENERATE_RECORD_SLIDER;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.LOAD_STORAGE_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NODE_ID_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_EMAIL_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_PATH_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TRANSACTION_FEE_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_HOURS_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_MINUTES_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_SECONDS_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TX_VALID_DURATION_TF;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class SettingsPaneTest extends TestBase {

	private SettingsPanePage settingsPanePage;
	private final MainWindowPage mainWindowPage = new MainWindowPage(this);
	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	public UserAccessibleProperties properties;

	@Before
	public void setUp() throws Exception {

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		if (!new File(DEFAULT_STORAGE + "/Files/").exists()) {
			if (new File(DEFAULT_STORAGE + "/Files/").mkdirs()) {
				logger.info("Files folder created");
			}
		}

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");

		if (new File(
				currentRelativePath.toAbsolutePath().toString(), "src/test/resources/Transactions - " +
				"Documents/OutputFiles/test1.council2@hederacouncil.org/").mkdirs()) {
			logger.info("Output path created");
		}

		if (new File(
				currentRelativePath.toAbsolutePath().toString(), "src/test/resources/OneDrive/SecondDrive" +
				"/InputFiles/").mkdirs()) {
			logger.info("Second input folder created");
		}

		if (new File(
				currentRelativePath.toAbsolutePath().toString(), "src/test/resources/OneDrive/SecondDrive" +
				"/OutputFiles/test@testemail.net").mkdirs()) {
			logger.info("Second output folder created");
		}

		if (new File(Constants.CUSTOM_NETWORK_FOLDER).exists()) {
			FileUtils.cleanDirectory(new File(Constants.CUSTOM_NETWORK_FOLDER));
		}

		final Map<String, String> emailMap = new HashMap<>();

		emailMap.put(
				currentRelativePath.toAbsolutePath() + File.separator + "src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties.resetProperties();
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		properties.setOneDriveCredentials(emailMap);
		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		setupTransactionDirectory(DEFAULT_STORAGE);

		final Controller controller = new Controller();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));


		if (new File(DEFAULT_STORAGE + "/History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "/History"));
		}

		settingsPanePage = new SettingsPanePage(this);

		properties.setHash("123456789".toCharArray());
		startApplication();
		mainWindowPage.clickOnSettingsButton();

	}

	public void startApplication() throws Exception {
		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);
	}

	@Test(expected = FxRobotException.class)
	public void clickOnBogusItem_Test() {
		clickOn("#exterminate");
	}

	@Test
	public void checkAllFieldsHaveValues_Test() {
		try {
			assertFalse(((TextField) find(LOAD_STORAGE_TF)).getText().isEmpty());
			assertTrue(((TextField) find(ONEDRIVE_PATH_TF)).getText().isEmpty());
			assertTrue(((TextField) find(ONEDRIVE_EMAIL_TF)).getText().isEmpty());
			assertFalse(((TextField) find(NODE_ID_TF)).getText().isEmpty());
			assertFalse(((TextField) find(TX_VALID_DURATION_TF)).getText().isEmpty());
			assertFalse(((TextField) find(AUTO_RENEW_PERIOD_TF)).getText().isEmpty());
			assertFalse(((TextField) find(TVS_HOURS_TF)).getText().isEmpty());
			assertFalse(((TextField) find(TVS_MINUTES_TF)).getText().isEmpty());
			assertFalse(((TextField) find(TVS_SECONDS_TF)).getText().isEmpty());
			assertFalse(((TextField) find(TRANSACTION_FEE_TF)).getText().isEmpty());

			assertEquals(properties.getPreferredStorageDirectory(), ((TextField) find(LOAD_STORAGE_TF)).getText());
			final var defaultNodeID =
					Identifier.parse(properties.getDefaultNodeID()).toNicknameAndChecksum(new JsonObject());
			assertEquals(defaultNodeID, ((TextField) find(NODE_ID_TF)).getText());
			assertEquals(properties.getTxValidDuration(),
					Integer.parseInt(((TextField) find(TX_VALID_DURATION_TF)).getText().replaceAll(" ", "")));
			assertEquals(properties.getAutoRenewPeriod(), Long.parseLong(
					((TextField) find(AUTO_RENEW_PERIOD_TF)).getText().replaceAll(" ", "")));
			assertEquals(properties.getDefaultHours(),
					Integer.parseInt(((TextField) find(TVS_HOURS_TF)).getText().replaceAll(" ", "")));
			assertEquals(properties.getDefaultMinutes(),
					Integer.parseInt(((TextField) find(TVS_MINUTES_TF)).getText().replaceAll(" ", "")));
			assertEquals(properties.getDefaultSeconds(),
					Integer.parseInt(((TextField) find(TVS_SECONDS_TF)).getText().replaceAll(" ", "")));
			assertEquals(properties.getDefaultTxFee(), 100000000 *
					Long.parseLong(((TextField) find(TRANSACTION_FEE_TF)).getText().replaceAll(" ", "")));
			assertEquals(properties.getGenerateRecord(),
					((ToggleSwitch) find(GENERATE_RECORD_SLIDER)).isSelected());
		} catch (final Exception e) {
			e.printStackTrace();
			logger.error(e);
		}

	}

	@Test
	public void changeValuesAndCheckPreferences_Test() {
		try {
			settingsPanePage.setNodeID("5")
					.setAutoRenewPeriod("7500000")
					.setTransactionFee("2")
					.setTransactionVD("111")
					.setHours("7")
					.setMinutes("13")
					.setSeconds("37");

			assertEquals("0.0.5", properties.getDefaultNodeID());
			assertEquals(111, properties.getTxValidDuration());
			assertEquals(7500000, properties.getAutoRenewPeriod());
			assertEquals(7, properties.getDefaultHours());
			assertEquals(13, properties.getDefaultMinutes());
			assertEquals(37, properties.getDefaultSeconds());
			assertEquals(200000000, properties.getDefaultTxFee());
			assertEquals(properties.getGenerateRecord(),
					((ToggleSwitch) find(GENERATE_RECORD_SLIDER)).isSelected());
		} catch (final Exception e) {
			logger.error(e);
		}

	}

	/**
	 * 1. press edit
	 * a. verify cancel button appears
	 * 2. press cancel
	 * a. verify box appears again
	 * 3. press addAction_test
	 * a. verify cancel button appears
	 * 4. press cancel
	 * a.verify box appears
	 * 5. press addAction_test button
	 * 6. addAction_test another folder
	 * 7. repeat steps 1 through 4
	 */

	@Test
	public void cancelAddFolder_Tests() {
		Node node = find("#transactionFoldersVBoxSP");

		assertTrue(node instanceof VBox);
		settingsPanePage.pressEditFolder((HBox) (((VBox) find("#transactionFoldersVBoxSP")).getChildren()).get(0));

		assertEquals(1, ((VBox) node).getChildren().size());
		assertTrue(find("#cancelAddToEmailMapButton").isVisible());

		settingsPanePage.cancelAddToMap()
				.pressAddFolder();

		node = find("#transactionFoldersVBoxSP");
		assertTrue(node instanceof VBox);
		assertEquals(1, ((VBox) node).getChildren().size());
		assertTrue(find("#cancelAddToEmailMapButton").isVisible());

		settingsPanePage.cancelAddToMap();
		assertFalse(find("#addPathGridPane").isVisible());

		node = find("#transactionFoldersVBoxSP");
		assertTrue(node instanceof VBox);
		assertEquals(1, ((VBox) node).getChildren().size());

		settingsPanePage.pressAddFolder()
				.setPathAndEmail("src/test/resources/OneDrive/SecondDrive/", "test@testemail.net")
				.pressConfirmAddFolder();

		node = find("#transactionFoldersVBoxSP");
		assertTrue(node instanceof VBox);

		assertEquals(2, ((VBox) node).getChildren().size());

		settingsPanePage.pressEditFolder((HBox) (((VBox) find("#transactionFoldersVBoxSP")).getChildren()).get(0));
		node = find("#transactionFoldersVBoxSP");

		assertTrue(node instanceof VBox);
		assertEquals(2, ((VBox) node).getChildren().size());

		settingsPanePage.cancelAddToMap();
		assertFalse(find("#addPathGridPane").isVisible());

		node = find("#transactionFoldersVBoxSP");
		assertTrue(node instanceof VBox);

		assertEquals(2, ((VBox) node).getChildren().size());

	}

	@Test
	public void addFolderNoStructureCancel_Tests() {
		final Node node = find("#transactionFoldersVBoxSP");
		assertTrue(node instanceof VBox);
		settingsPanePage.pressAddFolder()
				.setPath("src/test/resources");
		final ObservableList<Node> popupNodes = TestUtil.getPopupNodes();
		assert popupNodes != null;
		assertEquals(1, popupNodes.size());
		assertTrue(popupNodes.get(0) instanceof VBox);
		final var children = ((VBox) popupNodes.get(0)).getChildren();

		assertTrue(children.get(0) instanceof Label);
		assertTrue(children.get(1) instanceof HBox);

		final var buttons = ((HBox) ((HBox) children.get(1)).getChildren().get(1)).getChildren();
		assertEquals(2, buttons.size());
		assertTrue(buttons.get(0) instanceof Button);
		assertTrue(buttons.get(1) instanceof Button);

		clickOn(buttons.get(1));

		assertTrue(find(ONEDRIVE_EMAIL_TF).isDisabled());

		assertFalse(new File("src/test/resources/InputFiles").exists());
		assertFalse(new File("src/test/resources/OutputFiles").exists());
		assertFalse(new File("src/test/resources/OutputFiles/test@testemail.net").exists());
	}

	@Test
	public void addFolderNoStructureCreate_Tests() {
		final Node node = find("#transactionFoldersVBoxSP");
		assertTrue(node instanceof VBox);
		settingsPanePage.pressAddFolder()
				.setPath("src/test/resources")
				.createPopup();

		assertFalse(find(ONEDRIVE_EMAIL_TF).isDisabled());

		settingsPanePage.setEmail("test@testemail.net").pressConfirmAddFolder().createPopup();
		assertTrue(new File("src/test/resources/InputFiles").exists());
		assertTrue(new File("src/test/resources/OutputFiles").exists());
		assertTrue(new File("src/test/resources/OutputFiles/test@testemail.net").exists());

	}

	@Test
	public void defaultNetworks_Test() {
		settingsPanePage.openNetworksCombobox("TESTNET");
		assertTrue(find("#deleteCustomNetworkButton").isDisabled());
		assertEquals("TESTNET", properties.getCurrentNetwork());
		settingsPanePage.openNetworksCombobox("PREVIEWNET");
		assertTrue(find("#deleteCustomNetworkButton").isDisabled());
		assertEquals("PREVIEWNET", properties.getCurrentNetwork());
	}

	@Test
	public void addCustomNetwork_test() {
		settingsPanePage.addNetwork("custom", "src/test/resources/customNetwork.json");
		assertFalse(find("#deleteCustomNetworkButton").isDisabled());
		assertEquals("custom", properties.getCurrentNetwork());
		clickOn("#addCustomNetworkButton");
		settingsPanePage.addNetworkNickname("custom");
		var nodes = TestUtil.getPopupNodes();
		assertTrue(nodes.get(1).isVisible());
		assertTrue(nodes.get(1) instanceof Label);
		assertEquals("The network nickname already exists.", ((Label) nodes.get(1)).getText());

		type(KeyCode.A);
		nodes = TestUtil.getPopupNodes();
		assertFalse(nodes.get(1).isVisible());
		type(KeyCode.ENTER);
		nodes = TestUtil.getPopupNodes();
		assertFalse(nodes.get(1).isVisible());

		settingsPanePage.addCustomLocation("src/test/resources/customNetwork_bad.json");
		settingsPanePage.clickOnButton("CONTINUE");

		final var newNodes = TestUtil.getPopupNodes();
		assertNotNull(newNodes);
		assertTrue(newNodes.get(0) instanceof VBox);
		final var children = ((VBox) newNodes.get(0)).getChildren();
		assertEquals("The json file does not contain a valid network", ((Label) children.get(0)).getText());
		clickOn(TestUtil.findButtonInPopup(newNodes, "CONTINUE"));

		settingsPanePage.openNetworksCombobox("TESTNET");
		settingsPanePage.openNetworksCombobox("custom");

		assertFalse(find("#deleteCustomNetworkButton").isDisabled());
		clickOn("#deleteCustomNetworkButton");

		final var deleteNetworkNodes = TestUtil.getPopupNodes();
		clickOn(TestUtil.findButtonInPopup(deleteNetworkNodes, "CONTINUE"));

		assertTrue(find("#deleteCustomNetworkButton").isDisabled());
		assertFalse(new File(Constants.CUSTOM_NETWORK_FOLDER, "custom.json").exists());

		settingsPanePage.addNetwork("customb", "src/test/resources/fakeFile.json");
		nodes = TestUtil.getPopupNodes();
		assertTrue(nodes.get(1).isVisible());
		assertTrue(nodes.get(1) instanceof Label);
		assertEquals("The location specified does not exist. Please try again.", ((Label) nodes.get(1)).getText());
		assertTrue(Objects.requireNonNull(TestUtil.findButtonInPopup(nodes, "CONTINUE")).isDisabled());
	}

	@After
	public void tearDown() throws IOException {
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

		if (new File("src/test/resources/InputFiles").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/InputFiles"));
		}
		if (new File("src/test/resources/OutputFiles").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/OutputFiles"));
		}

	}
}
