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
	private Controller controller;
	private final static String CURRENT_RELATIVE_PATH = Paths.get("").toAbsolutePath().toString();
	private static final Logger logger = LogManager.getLogger(ControllerTest.class);
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

	@BeforeEach
	void setUp() {
		try {
			if (new File(DEFAULT_STORAGE).exists()) {
				org.apache.commons.io.FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
			}
			Preferences preferences = Preferences.userNodeForPackage(Controller.class);
			preferences.clear();
			assertEquals(0, preferences.keys().length);
			String defaultStorage =
					Paths.get("").toAbsolutePath().toString() + "/src/test/resources/testDirectory/TransactionTools";
			preferences.put(PREFERRED_STORAGE_DIRECTORY, defaultStorage);
			controller = new Controller();
			controller.properties = new UserAccessibleProperties(defaultStorage + "/Files/user.properties", "");
			controller.properties.resetProperties();
			controller.setVersionString(controller.getVersion());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void storeMapInPreferences_test() {
		try {
			Map<String, String> testMap = new HashMap<>();
			testMap.put("test1@somewhere.com", "/here/there/everyWhere");
			testMap.put("test2@somewhere.else.com", "somewhere/nowhere/everywhere");

			controller.properties.setOneDriveCredentials(testMap);
			Map<String, String> recoveredMap = controller.properties.getOneDriveCredentials();

			for (String key :
					testMap.keySet()) {
				assertEquals(testMap.get(key), recoveredMap.get(key));
			}

			for (String key :
					recoveredMap.keySet()) {
				assertEquals(recoveredMap.get(key), testMap.get(key));
			}

			// todo: Add different types to the test
			// todo: Revisit names
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void lastTransactionsDirectory_test() {
		try {
			controller.setLastTransactionsDirectory(new File(CURRENT_RELATIVE_PATH));
			assertEquals(CURRENT_RELATIVE_PATH, controller.getLastTransactionsDirectory());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void preferredStorageDirectory_test() {
		try {
			controller.setPreferredStorageDirectory(CURRENT_RELATIVE_PATH);
			assertEquals(CURRENT_RELATIVE_PATH, controller.getPreferredStorageDirectory());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void defaultTxFee_test() {
		try {
			controller.properties.setDefaultTxFee(123456789);
			assertEquals(123456789, controller.properties.getDefaultTxFee());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void defaultNodeID_test() {
		try {
			controller.properties.setDefaultNodeID(new Identifier(
					AccountID.newBuilder().setRealmNum(2).setShardNum(1).setAccountNum(3).build()).toReadableString());
			assertEquals("1.2.3", controller.properties.getDefaultNodeID());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void txValidDuration_test() {
		try {
			controller.properties.setTxValidDuration(120);
			assertEquals(120, controller.properties.getTxValidDuration());

			controller.properties.setTxValidDuration(-1);
			assertEquals(120, controller.properties.getTxValidDuration());

			controller.properties.setTxValidDuration(200);
			assertEquals(120, controller.properties.getTxValidDuration());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void setupPhase_test() {
		try {
			assertEquals(SetupPhase.INITIAL_SETUP_PHASE, controller.getSetupPhase());
			controller.setSetupPhase(SetupPhase.NORMAL_OPERATION_PHASE);
			assertEquals(SetupPhase.NORMAL_OPERATION_PHASE, controller.getSetupPhase());
		} catch (Exception e) {
			logger.error(e);
		}

	}

	@Test
	void generateRecord_test() {
		try {
			assertFalse(controller.properties.getGenerateRecord());
			controller.properties.setGenerateRecord(true);
			assertTrue(controller.properties.getGenerateRecord());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void autoRenewPeriod_test() {
		try {
			assertEquals(7000000, controller.properties.getAutoRenewPeriod());
			controller.properties.setAutoRenewPeriod(500);
			assertEquals(7000000, controller.properties.getAutoRenewPeriod());
			controller.properties.setAutoRenewPeriod(8000001);
			assertEquals(7000000, controller.properties.getAutoRenewPeriod());
			controller.properties.setAutoRenewPeriod(7500000);
			assertEquals(7500000, controller.properties.getAutoRenewPeriod());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void defaultSubmissionTime_test() {

		try {
			assertEquals(1, controller.properties.getDefaultHours());
			assertEquals(0, controller.properties.getDefaultMinutes());
			assertEquals(0, controller.properties.getDefaultSeconds());

			controller.properties.setDefaultHours(-1);
			assertEquals(1, controller.properties.getDefaultHours());

			controller.properties.setDefaultMinutes(-1);
			assertEquals(0, controller.properties.getDefaultMinutes());

			controller.properties.setDefaultSeconds(-1);
			assertEquals(0, controller.properties.getDefaultSeconds());

			controller.properties.setDefaultHours(29);
			assertEquals(1, controller.properties.getDefaultHours());

			controller.properties.setDefaultMinutes(61);
			assertEquals(0, controller.properties.getDefaultMinutes());

			controller.properties.setDefaultSeconds(61);
			assertEquals(0, controller.properties.getDefaultSeconds());

			controller.properties.setDefaultHours(13);
			assertEquals(13, controller.properties.getDefaultHours());

			controller.properties.setDefaultMinutes(21);
			assertEquals(21, controller.properties.getDefaultMinutes());

			controller.properties.setDefaultSeconds(31);
			assertEquals(31, controller.properties.getDefaultSeconds());
		} catch (Exception e) {
			logger.error(e);
		}
	}


	@Test
	void version_test() {
		try {
			assertFalse(controller.getVersion().isEmpty());
			assertNotEquals("", controller.getVersion());
		} catch (Exception e) {
			logger.error(e);
		}
	}

	@Test
	void hash_test() {
		try {
			PasswordAuthenticator passwordAuthenticator = new PasswordAuthenticator();
			assertEquals("", controller.properties.getHash());

			controller.properties.setHash("12345679".toCharArray());
			String hash = controller.properties.getHash();
			assertTrue(passwordAuthenticator.authenticate("12345679".toCharArray(), hash));
		} catch (Exception e) {
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
		} catch (Exception e) {
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