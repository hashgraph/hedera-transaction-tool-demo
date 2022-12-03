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

import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.PasswordAuthenticator;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import com.hedera.hashgraph.sdk.proto.AccountID;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.prefs.Preferences;

import static com.hedera.hashgraph.client.core.constants.Constants.PREFERRED_STORAGE_DIRECTORY;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ControllerTest {
	private MainController controller;
	private static final String CURRENT_RELATIVE_PATH = Paths.get("").toAbsolutePath().toString();
	private static final Logger logger = LogManager.getLogger(ControllerTest.class);
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

	@BeforeEach
	void setUp() {
		try {
			System.gc();
			logger.info("Starting test class: {}", getClass().getSimpleName());

			// TODO this isn't a subclass of TestBase?
//			buildFolders();


			final Preferences preferences = Preferences.userNodeForPackage(MainController.class);
			preferences.clear();
			assertEquals(0, preferences.keys().length);
			final String defaultStorage =
					Paths.get("").toAbsolutePath() + "/src/test/resources/testDirectory/TransactionTools";
			preferences.put(PREFERRED_STORAGE_DIRECTORY, defaultStorage);
			controller = new MainController();
			controller.setProperties(new UserAccessibleProperties(defaultStorage + "/Files/user", ""));
			controller.resetProperties();
			controller.setVersionString(controller.getVersion());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void storeMapInPreferences_test() {
		try {
			final Map<String, String> testMap = new HashMap<>();
			testMap.put("test1@somewhere.com", "/here/there/everyWhere");
			testMap.put("test2@somewhere.else.com", "somewhere/nowhere/everywhere");

			controller.setOneDriveCredentials(testMap);
			final Map<String, String> recoveredMap = controller.getOneDriveCredentials();

			for (final String key :
					testMap.keySet()) {
				assertEquals(testMap.get(key), recoveredMap.get(key));
			}

			for (final String key :
					recoveredMap.keySet()) {
				assertEquals(recoveredMap.get(key), testMap.get(key));
			}

		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void lastTransactionsDirectory_test() {
		try {
			controller.setLastBrowsedDirectory(new File(CURRENT_RELATIVE_PATH));
			assertEquals(CURRENT_RELATIVE_PATH, controller.getLastTransactionsDirectory());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void preferredStorageDirectory_test() {
		try {
			controller.setPreferredStorageDirectory(CURRENT_RELATIVE_PATH);
			assertEquals(CURRENT_RELATIVE_PATH, controller.getPreferredStorageDirectory());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void defaultTxFee_test() {
		try {
			controller.setDefaultTxFee(123456789);
			assertEquals(123456789, controller.getDefaultTxFee());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void defaultNodeID_test() {
		try {
			controller.setDefaultNodeID(new Identifier(
					AccountID.newBuilder().setRealmNum(2).setShardNum(1).setAccountNum(3).build()).toReadableString());
			assertEquals("1.2.3", controller.getDefaultNodeID());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void txValidDuration_test() {
		try {
			controller.setTxValidDuration(120);
			assertEquals(120, controller.getTxValidDuration());

			controller.setTxValidDuration(-1);
			assertEquals(120, controller.getTxValidDuration());

			controller.setTxValidDuration(200);
			assertEquals(120, controller.getTxValidDuration());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void setupPhase_test() {
		try {
			assertEquals(SetupPhase.INITIAL_SETUP_PHASE, controller.getSetupPhase());
			controller.setSetupPhase(SetupPhase.NORMAL_OPERATION_PHASE);
			assertEquals(SetupPhase.NORMAL_OPERATION_PHASE, controller.getSetupPhase());
		} catch (final Exception e) {
			logger.error(e);
		}

	}

	@Test
	void generateRecord_test() {
		try {
			assertFalse(controller.getGenerateRecord());
			controller.setGenerateRecord(true);
			assertTrue(controller.getGenerateRecord());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void autoRenewPeriod_test() {
		try {
			assertEquals(7000000, controller.getAutoRenewPeriod());
			controller.setAutoRenewPeriod(500);
			assertEquals(7000000, controller.getAutoRenewPeriod());
			controller.setAutoRenewPeriod(8000001);
			assertEquals(7000000, controller.getAutoRenewPeriod());
			controller.setAutoRenewPeriod(7500000);
			assertEquals(7500000, controller.getAutoRenewPeriod());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void defaultSubmissionTime_test() {

		try {
			assertEquals(1, controller.getDefaultHours());
			assertEquals(0, controller.getDefaultMinutes());
			assertEquals(0, controller.getDefaultSeconds());

			controller.setDefaultHours(-1);
			assertEquals(1, controller.getDefaultHours());

			controller.setDefaultMinutes(-1);
			assertEquals(0, controller.getDefaultMinutes());

			controller.setDefaultSeconds(-1);
			assertEquals(0, controller.getDefaultSeconds());

			controller.setDefaultHours(29);
			assertEquals(1, controller.getDefaultHours());

			controller.setDefaultMinutes(61);
			assertEquals(0, controller.getDefaultMinutes());

			controller.setDefaultSeconds(61);
			assertEquals(0, controller.getDefaultSeconds());

			controller.setDefaultHours(13);
			assertEquals(13, controller.getDefaultHours());

			controller.setDefaultMinutes(21);
			assertEquals(21, controller.getDefaultMinutes());

			controller.setDefaultSeconds(31);
			assertEquals(31, controller.getDefaultSeconds());
		} catch (final Exception e) {
			logger.error(e);
		}
	}


	@Test
	void version_test() {
		try {
			assertFalse(controller.getVersion().isEmpty());
			assertNotEquals("", controller.getVersion());
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void hash_test() {
		try {
			final PasswordAuthenticator passwordAuthenticator = new PasswordAuthenticator();
			assertEquals("", controller.getHash());

			controller.setHash("12345679".toCharArray());
			final String hash = controller.getHash();
			assertTrue(passwordAuthenticator.authenticate("12345679".toCharArray(), hash));
		} catch (final Exception e) {
			logger.error(e);
		}
	}

	@Test
	void lastIndex_test() {
		try {
			assertEquals(0, controller.getLastIndex());
			controller.incrementIndex();
			assertEquals(1, controller.getLastIndex());
			controller.incrementIndex();
			assertEquals(2, controller.getLastIndex());
		} catch (final Exception e) {
			logger.error(e);
		}

	}

	@AfterEach
	void tearDown() throws IOException {
		if (new File(DEFAULT_STORAGE).exists()) {
			org.apache.commons.io.FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}
}