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

package com.hedera.hashgraph.client.core.props;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.security.PasswordAuthenticator;
import com.hedera.hashgraph.sdk.Hbar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.HashMap;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HOURS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_MINUTES;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_NODE_ID;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_SECONDS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_TX_FEE;
import static com.hedera.hashgraph.client.core.constants.Constants.GENERATE_RECORD;
import static com.hedera.hashgraph.client.core.constants.Constants.PREFERRED_STORAGE_DIRECTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.TX_VALID_DURATION;
import static com.hedera.hashgraph.client.core.constants.Constants.VAL_NUM_TRANSACTION_DEFAULT_FEE;
import static com.hedera.hashgraph.client.core.constants.Constants.VAL_NUM_TRANSACTION_VALID_DURATION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;


class UserAccessiblePropertiesTest {

	private static final Logger logger = LogManager.getLogger(UserAccessibleProperties.class);

	UserAccessibleProperties properties;

	@BeforeEach
	void setUp() {
		properties = new UserAccessibleProperties("src/test/resources/test.properties", "test");
	}

	@AfterEach
	void tearDown() {
		if (new File("src/test/resources/test.properties").exists()) {
			logger.info(String.format("Test properties deleted :%s",
					new File("src/test/resources/test.properties").delete()));
		}
	}

	@Test
	void setPreferredStorageDirectory_Test() {
		String dir =
				System.getProperty("user.home") + File.separator + "Documents" + File.separator + "TransactionTools";

		properties.setPreferredStorageDirectory(
				System.getProperty("user.home") + File.separator + "Documents" + File.separator + "TransactionTools");
		assertEquals(dir, properties.getPreferredStorageDirectory());
	}

	@Test
	void setDefaultTxFee_Test() {
		properties.setDefaultTxFee(12345678999L);
		assertEquals(12345678999L, properties.getDefaultTxFee());
	}

	@Test
	void setDefaultNodeID_Test() {
		properties.setDefaultNodeID("0.0.5");
		assertEquals("0.0.5", properties.getDefaultNodeID());

		properties.setDefaultNodeID(new Identifier(3, 2, 1));
		assertEquals("3.2.1", properties.getDefaultNodeID());
	}

	@Test
	void setTxValidDuration_Test() {
		properties.setTxValidDuration(123);
		assertEquals(123, properties.getTxValidDuration());
	}

	@Test
	void setGenerateRecord_Test() {
		properties.setGenerateRecord(false);
		assertFalse(properties.getGenerateRecord());
	}

	@Test
	void setAutoRenewPeriod_Test() {
		properties.setAutoRenewPeriod(12345678999L);
		assertEquals(7000000, properties.getAutoRenewPeriod());
	}

	@Test
	void setDefaultHours_Test() {
		properties.setDefaultHours(21);
		assertEquals(21, properties.getDefaultHours());
	}

	@Test
	void setDefaultMinutes_Test() {
		properties.setDefaultMinutes(47);
		assertEquals(47, properties.getDefaultMinutes());
	}

	@Test
	void setDefaultSeconds_Test() {
		properties.setDefaultSeconds(37);
		assertEquals(37, properties.getDefaultSeconds());
	}

	@Test
	void setOneDriveCredentials_Test() {
		properties.addOneDriveCredential("/newPath", "new@email.com");
		Map<String, String> credentials = properties.getOneDriveCredentials();
		assertEquals(1, credentials.size());
		Assertions.assertTrue(properties.findEmail("new@email.com"));
		properties.addOneDriveCredential("/newPath/Another", "newer@email.com");
		credentials = properties.getOneDriveCredentials();
		assertEquals(2, credentials.size());
		Assertions.assertTrue(properties.credentialsMapCollision("/newPath", "new@email.com"));
		Assertions.assertTrue(properties.credentialsMapCollision("/newishPath", "new@email.com"));
		assertFalse(properties.credentialsMapCollision("/newestPath", "newest@email.com"));

		assertEquals("new@email.com", properties.getEmailFromMap("/newPath"));
		assertEquals("new@email.com", properties.getEmailFromMap("/newPath/"));

		properties.removeOneDriveCredential("/newPath");
		credentials = properties.getOneDriveCredentials();
		assertEquals(1, credentials.size());
		assertFalse(properties.findEmail("new@email.com"));
		properties.resetCredentialsMap();
		credentials = properties.getOneDriveCredentials();
		assertEquals(0, credentials.size());

	}

	@Test
	void setHash_Test() throws InvalidKeySpecException, NoSuchAlgorithmException, HederaClientException {
		char[] pass = "testPassword".toCharArray();
		properties.setHash(pass);
		PasswordAuthenticator passwordAuthenticator = new PasswordAuthenticator();
		Assertions.assertTrue(passwordAuthenticator.authenticate(pass, properties.getHash()));
	}

	@Test
	void resetProperties_Test() {
		properties.setPreferredStorageDirectory(
				System.getProperty(
						"user.home") + File.separator + "Documents" + File.separator + "TransactionToolsTest");
		properties.setDefaultTxFee(100000001L);
		properties.setDefaultNodeID("0.0.5");
		properties.setTxValidDuration(123);
		properties.setGenerateRecord(true);
		properties.setDefaultHours(21);
		properties.setDefaultMinutes(47);
		properties.setDefaultSeconds(37);

		JsonObject propertiesJson = properties.readProperties();

		Assertions.assertTrue(propertiesJson.has(PREFERRED_STORAGE_DIRECTORY));
		assertEquals(
				System.getProperty("user.home") + File.separator + "Documents" + File.separator +
						"TransactionToolsTest",
				propertiesJson.get(PREFERRED_STORAGE_DIRECTORY).getAsString());

		Assertions.assertTrue(propertiesJson.has(DEFAULT_TX_FEE));
		assertEquals("100000001", propertiesJson.get(DEFAULT_TX_FEE).getAsString());

		Assertions.assertTrue(propertiesJson.has(DEFAULT_NODE_ID));
		assertEquals("0.0.5", propertiesJson.get(DEFAULT_NODE_ID).getAsString());

		Assertions.assertTrue(propertiesJson.has(TX_VALID_DURATION));
		assertEquals("123", propertiesJson.get(TX_VALID_DURATION).getAsString());

		Assertions.assertTrue(propertiesJson.has(GENERATE_RECORD));
		Assertions.assertTrue(propertiesJson.get(GENERATE_RECORD).getAsBoolean());

		Assertions.assertTrue(propertiesJson.has(DEFAULT_HOURS));
		assertEquals(21, propertiesJson.get(DEFAULT_HOURS).getAsInt());

		Assertions.assertTrue(propertiesJson.has(DEFAULT_MINUTES));
		assertEquals(47, propertiesJson.get(DEFAULT_MINUTES).getAsInt());

		Assertions.assertTrue(propertiesJson.has(DEFAULT_SECONDS));
		assertEquals(37, propertiesJson.get(DEFAULT_SECONDS).getAsInt());

		properties.resetProperties();
		assertEquals(
				System.getProperty("user.home") + File.separator + "Documents" + File.separator + "TransactionTools",
				properties.getPreferredStorageDirectory());
		assertEquals(VAL_NUM_TRANSACTION_DEFAULT_FEE, properties.getDefaultTxFee());
		assertEquals("0.0.3", properties.getDefaultNodeID());
		assertEquals(VAL_NUM_TRANSACTION_VALID_DURATION, properties.getTxValidDuration());
		assertFalse(properties.getGenerateRecord());
		assertEquals(1, properties.getDefaultHours());
		assertEquals(0, properties.getDefaultMinutes());
		assertEquals(0, properties.getDefaultSeconds());

		properties.loadProperties(propertiesJson);
		assertEquals(
				System.getProperty("user.home") + File.separator + "Documents" + File.separator +
						"TransactionToolsTest",
				properties.getPreferredStorageDirectory());
		assertEquals(100000001, properties.getDefaultTxFee());
		assertEquals("0.0.5", properties.getDefaultNodeID());
		assertEquals(123, properties.getTxValidDuration());
		Assertions.assertTrue(properties.getGenerateRecord());
		assertEquals(21, properties.getDefaultHours());
		assertEquals(47, properties.getDefaultMinutes());
		assertEquals(37, properties.getDefaultSeconds());


	}


	@Test
	void feePayerProperty_test() {
		properties.setPreferredFeePayerAccountProperty(new Identifier(0, 0, 2));
		assertEquals(new Identifier(0, 0, 2).asJSON(), properties.getFeePayerAccountProperty());
	}

	@Test
	void getKeyLocation_test() {
		properties.setKeyLocationProperty("testLocation");
		assertEquals("testLocation", properties.getKeyLocationProperty());
	}

	@Test
	void getPreferredRealm_test() {
		assertEquals(0, properties.getPreferredRealmProperty());
		properties.setPreferredRealmProperty(15);
		assertEquals(15, properties.getPreferredRealmProperty());
	}

	@Test
	void getPreferredShard_test() {
		assertEquals(0, properties.getPreferredShardProperty());
		properties.setPreferredShardProperty(15);
		assertEquals(15, properties.getPreferredShardProperty());
	}

	@Test
	void nodeAccountProperty_test() {
		properties.setNodeAccountProperty(new Identifier(0, 0, 8));
		assertEquals(new Identifier(0, 0, 8).asJSON(), properties.getNodeAccountProperty());
	}

	@Test
	void getNetwork_test() {
		assertEquals("MAINNET", properties.getNetworkProperty());
		properties.setNetworkProperty(NetworkEnum.INTEGRATION);
		assertEquals("INTEGRATION", properties.getNetworkProperty());
	}

	@Test
	void transactionFee_test() {
		properties.setTransactionFeeProperty(Hbar.from(1000));
		assertEquals(Hbar.from(1000), properties.getTransactionFeeProperty());
	}

	@Test
	void getPreferredStorage_test() {
		assertEquals(
				System.getProperty("user.home") + File.separator + "Documents" + File.separator + "TransactionTools",
				properties.getPreferredStorageDirectory());
		properties.setPreferredStorageDirectory("src/test/resources");
		assertEquals("src/test/resources", properties.getPreferredStorageDirectory());
	}

	@Test
	void versionString_test() {
		properties.setVersionString("0.12.4 test string");
		assertEquals("0.12.4 test string", properties.getVersionString());
	}

	@Test
	void browsedDirectory_test() {
		assertEquals(
				System.getProperty("user.home") + File.separator + "Documents" + File.separator + "TransactionTools",
				properties.getLastBrowsedDirectory());
		properties.setLastBrowsedDirectory(new File("src/test/resources/Files"));
		assertEquals(System.getProperty("user.dir") + File.separator + "src/test/resources/Files",
				properties.getLastBrowsedDirectory());
		properties.setLastBrowsedDirectory(new File("src/test/resources/Keys/genesis.pem"));
		assertEquals(System.getProperty("user.dir") + File.separator + "src/test/resources/Keys",
				properties.getLastBrowsedDirectory());
	}

	@Test
	void getSetupPhase_test() {
		assertEquals(SetupPhase.INITIAL_SETUP_PHASE, properties.getSetupPhase());
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		assertEquals(SetupPhase.TEST_PHASE, properties.getSetupPhase());
	}

	@Test
	void autoRenew_test() {
		assertEquals(Constants.MINIMUM_AUTO_RENEW_PERIOD, properties.getAutoRenewPeriod());
		properties.setAutoRenewPeriod(1000L);
		assertEquals(Constants.MINIMUM_AUTO_RENEW_PERIOD, properties.getAutoRenewPeriod());
		properties.setAutoRenewPeriod(Constants.MAXIMUM_AUTO_RENEW_PERIOD + 1000L);
		assertEquals(Constants.MINIMUM_AUTO_RENEW_PERIOD, properties.getAutoRenewPeriod());
		properties.setAutoRenewPeriod(Constants.MINIMUM_AUTO_RENEW_PERIOD + 1000L);
		assertEquals(Constants.MINIMUM_AUTO_RENEW_PERIOD + 1000L, properties.getAutoRenewPeriod());
	}

	@Test
	void setHours_test() {
		assertEquals(1, properties.getDefaultHours());
		properties.setDefaultHours(-5);
		assertEquals(1, properties.getDefaultHours());
		properties.setDefaultHours(25);
		assertEquals(1, properties.getDefaultHours());
		properties.setDefaultHours(20);
		assertEquals(20, properties.getDefaultHours());
	}

	@Test
	void setMinutes_test() {
		assertEquals(0, properties.getDefaultMinutes());
		properties.setDefaultMinutes(-5);
		assertEquals(0, properties.getDefaultMinutes());
		properties.setDefaultMinutes(70);
		assertEquals(0, properties.getDefaultMinutes());
		properties.setDefaultMinutes(23);
		assertEquals(23, properties.getDefaultMinutes());
	}

	@Test
	void setSeconds_test() {
		assertEquals(0, properties.getDefaultSeconds());
		properties.setDefaultSeconds(-5);
		assertEquals(0, properties.getDefaultSeconds());
		properties.setDefaultSeconds(70);
		assertEquals(0, properties.getDefaultSeconds());
		properties.setDefaultSeconds(23);
		assertEquals(23, properties.getDefaultSeconds());
	}

	@Test
	void emailMap_test() {
		Map<String, String> emailMap = new HashMap<>();
		emailMap.put("/src/test/resources/Transactions - Documents/", "test1.council2@hederacouncil.org");
		emailMap.put("/src/test/resources/Transactions/", "test1.council1@hederacouncil.org");

		Map<String, String> testMap = properties.getOneDriveCredentials();
		assertTrue(testMap.isEmpty());
		properties.setOneDriveCredentials(emailMap);

		testMap = properties.getOneDriveCredentials();
		assertFalse(testMap.isEmpty());
		assertEquals(2, testMap.size());

		assertTrue(testMap.containsKey("/src/test/resources/Transactions - Documents/"));
		assertEquals("test1.council2@hederacouncil.org", testMap.get("/src/test/resources/Transactions - Documents/"));

		assertTrue(testMap.containsKey("/src/test/resources/Transactions/"));
		assertEquals("test1.council1@hederacouncil.org", testMap.get("/src/test/resources/Transactions/"));

		assertTrue(properties.findEmail("test1.council1@hederacouncil.org"));
		assertFalse(properties.findEmail("test1.council2@hed"));

		properties.removeOneDriveCredential("/src/test/resources/Transactions/");
		testMap = properties.getOneDriveCredentials();
		assertFalse(testMap.isEmpty());
		assertEquals(1, testMap.size());

		assertTrue(testMap.containsKey("/src/test/resources/Transactions - Documents/"));
		assertEquals("test1.council2@hederacouncil.org", testMap.get("/src/test/resources/Transactions - Documents/"));

		assertFalse(testMap.containsKey("/src/test/resources/Transactions/"));
	}
}