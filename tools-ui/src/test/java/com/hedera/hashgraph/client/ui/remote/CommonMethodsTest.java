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

package com.hedera.hashgraph.client.ui.remote;

import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.client.ui.StartUI;
import com.hedera.hashgraph.client.ui.TestBase;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static org.junit.Assert.assertTrue;

public class CommonMethodsTest extends TestBase {
	private static final Logger logger = LogManager.getLogger(CommonMethodsTest.class);

	@Before
	public void setUp() throws Exception {
		TestUtil.buildFolders();

		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original"), new File(DEFAULT_STORAGE));
		FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + KEYS_STRING));
		FileUtils.deleteDirectory(new File(DEFAULT_STORAGE + "/Accounts/0.0.56"));

		final UserAccessibleProperties properties =
				new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");
		final Path currentRelativePath = Paths.get("");
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

		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
		}
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

	}

	@After
	public void tearDown() throws Exception {
		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}

	@Test
	public void getTimeLabel_test() throws TimeoutException {
		logger.info("Starting TimeLabel test");
		final var properties = new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");
		properties.setSetupPhase(SetupPhase.TEST_PHASE);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		logger.info("UI initialized");

		final var label = CommonMethods.getTimeLabel(new Timestamp(206711586, 0), false);
		assertTrue(label.getText().contains("11:53:06 UTC"));
		final var label2 = CommonMethods.getTimeLabel(new Timestamp(206711586, 0), true);
		assertTrue(label2.getText().contains("1976-07-20 11:53:06 UTC"));

		logger.info("TimeLabel test finished");
	}
}
