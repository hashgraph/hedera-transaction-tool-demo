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
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.PasswordAuthenticator;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.SettingsPanePage;
import javafx.scene.Node;
import javafx.scene.control.TextField;
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
import static org.junit.Assert.assertTrue;

public class SettingsPaneTest extends TestBase {

	private SettingsPanePage settingsPanePage;
	private MainWindowPage mainWindowPage;
	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	public UserAccessibleProperties properties;

	@Before
	public void setUp() throws Exception {

		if (new File(DEFAULT_STORAGE).exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		if (!new File(DEFAULT_STORAGE + "/Files/").exists()) {
			if (new File(DEFAULT_STORAGE + "/Files/").mkdirs()) {
				logger.info("Files folder created");
			}
		}

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");

		if (new File(
				currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org/").mkdirs()) {
			logger.info("Output path created");
		}

		if (new File(
				currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/OneDrive/SecondDrive" +
						"/InputFiles" +
						"/").mkdirs()) {
			logger.info("Second input folder created");
		}

		if (new File(
				currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/OneDrive/SecondDrive" +
						"/OutputFiles" +
						"/test@testemail.net").mkdirs()) {
			logger.info("Second output folder created");
		}


		Map<String, String> emailMap = new HashMap<>();

		emailMap.put(
				currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties.resetProperties();
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		properties.setOneDriveCredentials(emailMap);
		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		setupTransactionDirectory(DEFAULT_STORAGE);

		Controller controller = new Controller();
		var version = controller.getVersion();
		properties.setVersionString(version);

		org.apache.commons.io.FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));
		org.apache.commons.io.FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		org.apache.commons.io.FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));


		if (new File(DEFAULT_STORAGE + "/History").exists()) {
			org.apache.commons.io.FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "/History"));
		}

		settingsPanePage = new SettingsPanePage(this);
		mainWindowPage = new MainWindowPage(this);
		PasswordAuthenticator passwordAuthenticator = new PasswordAuthenticator();
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
			final var defaultNodeID = Identifier.parse(properties.getDefaultNodeID()).toNicknameAndChecksum(new JsonObject());
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
		} catch (Exception e) {
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
		} catch (Exception e) {
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
		try {
			Node node;

			settingsPanePage.pressEditFolder((HBox) (((VBox) find("#transactionFoldersVBox")).getChildren()).get(0));
			node = find("#transactionFoldersVBox");

			assertTrue(node instanceof VBox);
			assertEquals(0, ((VBox) node).getChildren().size());
			assertTrue(find("#cancelAddToEmailMapButton").isVisible());

			settingsPanePage.cancelAddToMap()
					.pressAddFolder();

			node = find("#transactionFoldersVBox");
			assertTrue(node instanceof VBox);
			assertEquals(1, ((VBox) node).getChildren().size());
			assertTrue(find("#cancelAddToEmailMapButton").isVisible());

			settingsPanePage.cancelAddToMap();
			assertFalse(find("#addPathGridPane").isVisible());

			node = find("#transactionFoldersVBox");
			assertTrue(node instanceof VBox);
			assertEquals(1, ((VBox) node).getChildren().size());

			settingsPanePage.pressAddFolder()
					.setPathAndEmail("src/test/resources/OneDrive/SecondDrive/", "test@testemail.net")
					.pressConfirmAddFolder();

			node = find("#transactionFoldersVBox");
			assertTrue(node instanceof VBox);

			assertEquals(2, ((VBox) node).getChildren().size());

			settingsPanePage.pressEditFolder((HBox) (((VBox) find("#transactionFoldersVBox")).getChildren()).get(0));
			node = find("#transactionFoldersVBox");

			assertTrue(node instanceof VBox);
			assertEquals(1, ((VBox) node).getChildren().size());

			settingsPanePage.cancelAddToMap();
			assertFalse(find("#addPathGridPane").isVisible());

			node = find("#transactionFoldersVBox");
			assertTrue(node instanceof VBox);

			assertEquals(2, ((VBox) node).getChildren().size());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@After
	public void tearDown() throws IOException {
		Path currentRelativePath = Paths.get("");
		String s = currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/testDirectory";
		if ((new File(s)).exists()) {
			FileUtils.deleteDirectory(new File(s));
		}

		String out =
				currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org";
		org.apache.commons.io.FileUtils.cleanDirectory(new File(out));

		properties.resetProperties();

		if (new File(DEFAULT_STORAGE).exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}
}
