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
import com.hedera.hashgraph.client.core.constants.JsonConstants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.security.PasswordAuthenticator;
import com.hedera.hashgraph.sdk.Hbar;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNT_INFO_MAP;
import static com.hedera.hashgraph.client.core.constants.Constants.CURRENT_NETWORK;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.CUSTOM_FEE_PAYERS;
import static com.hedera.hashgraph.client.core.constants.Constants.CUSTOM_NETWORKS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_AUTO_RENEW_PERIOD;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_FEE_PAYER;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HOURS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_MINUTES;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_NODE_ID;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_SECONDS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_TX_FEE;
import static com.hedera.hashgraph.client.core.constants.Constants.GENERATE_RECORD;
import static com.hedera.hashgraph.client.core.constants.Constants.HASH;
import static com.hedera.hashgraph.client.core.constants.Constants.LAST_TRANSACTIONS_DIRECTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.LEGACY;
import static com.hedera.hashgraph.client.core.constants.Constants.MAXIMUM_AUTO_RENEW_PERIOD;
import static com.hedera.hashgraph.client.core.constants.Constants.MINIMUM_AUTO_RENEW_PERIOD;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_HASH_CODE;
import static com.hedera.hashgraph.client.core.constants.Constants.NETWORKS;
import static com.hedera.hashgraph.client.core.constants.Constants.PREFERRED_STORAGE_DIRECTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.SALT_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.SETUP_PHASE;
import static com.hedera.hashgraph.client.core.constants.Constants.TX_VALID_DURATION;
import static com.hedera.hashgraph.client.core.constants.Constants.VAL_NUM_TRANSACTION_DEFAULT_FEE;
import static com.hedera.hashgraph.client.core.constants.Constants.VAL_NUM_TRANSACTION_VALID_DURATION;
import static com.hedera.hashgraph.client.core.constants.Constants.VERSION;

public class UserAccessibleProperties {

	private UserProperties properties;
	private final String location;
	private final String message;

	public UserAccessibleProperties(String location, String message) {
		this.properties = new UserProperties(location, message);
		this.location = location;
		this.message = message;
	}


	public void setKeyLocationProperty(String location) {
		properties.setProperty(JsonConstants.PREFERRED_KEY_LOCATION, location);
	}

	public String getKeyLocationProperty() {
		return properties.getProperty(JsonConstants.PREFERRED_KEY_LOCATION, null);
	}


	public JsonObject getFeePayerAccountProperty() {
		var feePayer = properties.getProperty(JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME, null);
		return Identifier.parse(feePayer).asJSON().getAsJsonObject();
	}

	public void setPreferredFeePayerAccountProperty(Identifier account) {
		properties.setProperty(JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME, account);
	}

	public long getPreferredRealmProperty() {
		return Long.parseLong(properties.getProperty(JsonConstants.PREFERRED_REALM, "0"));
	}

	public void setPreferredRealmProperty(long realm) {
		properties.setProperty(JsonConstants.PREFERRED_REALM, realm);
	}

	public long getPreferredShardProperty() {
		return Long.parseLong(properties.getProperty(JsonConstants.PREFERRED_SHARD, "0"));
	}

	public void setPreferredShardProperty(long realm) {
		properties.setProperty(JsonConstants.PREFERRED_SHARD, realm);
	}

	public JsonObject getNodeAccountProperty() {
		var node = properties.getProperty(JsonConstants.PREFERRED_NODE_ACCOUNT, null);
		return Identifier.parse(node).asJSON().getAsJsonObject();
	}

	public void setNodeAccountProperty(Identifier node) {
		properties.setProperty(JsonConstants.PREFERRED_NODE_ACCOUNT, node);
	}

	public String getNetworkProperty() {
		return properties.getProperty(NETWORKS, "MAINNET");
	}

	public void setNetworkProperty(NetworkEnum network) {
		properties.setProperty(NETWORKS, network.toString());
	}

	public Hbar getTransactionFeeProperty() {
		return properties.getHBarProperty(JsonConstants.PREFERRED_TRANSACTION_FEE, Hbar.fromTinybars(100000000));
	}

	public void setTransactionFeeProperty(Hbar fee) {
		properties.setProperty(JsonConstants.PREFERRED_TRANSACTION_FEE, fee);
	}

	public String getPreferredStorageDirectory() {
		var defaultStorage =
				System.getProperty("user.home") + File.separator + "Documents" + File.separator + "TransactionTools";
		return properties.getProperty(PREFERRED_STORAGE_DIRECTORY, defaultStorage);
	}

	public void setPreferredStorageDirectory(String directory) {
		properties.setProperty(PREFERRED_STORAGE_DIRECTORY, directory);
	}

	public String getLastBrowsedDirectory() {
		var defaultStorage =
				System.getProperty("user.home") + File.separator + "Documents" + File.separator + "TransactionTools";
		return properties.getProperty(LAST_TRANSACTIONS_DIRECTORY, defaultStorage);
	}

	public void setLastBrowsedDirectory(File directory) {
		if (directory.isDirectory()) {
			properties.setProperty(LAST_TRANSACTIONS_DIRECTORY, directory.getAbsolutePath());
		} else {
			properties.setProperty(LAST_TRANSACTIONS_DIRECTORY, new File(directory.getParent()).getAbsolutePath());
		}
	}

	public long getDefaultTxFee() {
		return properties.getLongProperty(DEFAULT_TX_FEE, VAL_NUM_TRANSACTION_DEFAULT_FEE);
	}

	public void setDefaultTxFee(long defaultTxFee) {
		properties.setProperty(DEFAULT_TX_FEE, defaultTxFee);
	}

	public String getDefaultNodeID() {
		return properties.getProperty(DEFAULT_NODE_ID, "0.0.3");
	}

	public void setDefaultNodeID(String defaultNodeID) {
		properties.setProperty(DEFAULT_NODE_ID, defaultNodeID);
	}

	public void setDefaultNodeID(Identifier defaultNodeID) {
		properties.setProperty(DEFAULT_NODE_ID, defaultNodeID.toReadableString());
	}

	public long getTxValidDuration() {
		return properties.getLongProperty(TX_VALID_DURATION, VAL_NUM_TRANSACTION_VALID_DURATION);
	}

	public void setTxValidDuration(long txValidDuration) {
		if (txValidDuration >= 0 && txValidDuration <= VAL_NUM_TRANSACTION_VALID_DURATION) {
			properties.setProperty(TX_VALID_DURATION, txValidDuration);
		}
	}

	public SetupPhase getSetupPhase() {
		return SetupPhase.fromInt(properties.getIntProperty(SETUP_PHASE, 0));
	}

	public void setSetupPhase(SetupPhase phase) {
		properties.setProperty(SETUP_PHASE, phase.getValue());
	}

	public boolean getGenerateRecord() {
		return properties.getBooleanProperty(GENERATE_RECORD, false);
	}

	public void setGenerateRecord(boolean generateRecord) {
		properties.setProperty(GENERATE_RECORD, generateRecord);
	}

	public long getAutoRenewPeriod() {
		return properties.getLongProperty(DEFAULT_AUTO_RENEW_PERIOD, MINIMUM_AUTO_RENEW_PERIOD);
	}

	public void setAutoRenewPeriod(long period) {
		if (period < MINIMUM_AUTO_RENEW_PERIOD || period > MAXIMUM_AUTO_RENEW_PERIOD) {
			period = MINIMUM_AUTO_RENEW_PERIOD;
		}
		properties.setProperty(DEFAULT_AUTO_RENEW_PERIOD, period);
	}

	public int getDefaultHours() {
		return properties.getIntProperty(DEFAULT_HOURS, 1);

	}

	public void setDefaultHours(int h) {
		if (h < 0 || h > 23) {
			h = 1;
		}
		properties.setProperty(DEFAULT_HOURS, h);

	}

	public int getDefaultMinutes() {
		return properties.getIntProperty(DEFAULT_MINUTES, 0);
	}

	public void setDefaultMinutes(int m) {
		if (m < 0 || m > 59) {
			m = 0;
		}
		properties.setProperty(DEFAULT_MINUTES, m);

	}

	public int getDefaultSeconds() {
		return properties.getIntProperty(DEFAULT_SECONDS, 0);
	}

	public void setDefaultSeconds(int s) {
		if (s < 0 || s > 59) {
			s = 0;
		}
		properties.setProperty(DEFAULT_SECONDS, s);
	}

	public Map<String, String> getOneDriveCredentials() {
		return properties.getMapProperty("credentials", new HashMap<>());
	}

	public void setOneDriveCredentials(Map<String, String> credentialsMap) {
		properties.setProperty("credentials", credentialsMap);
	}

	public String getHash() {
		return properties.getProperty(HASH, "");
	}

	public void setHash(char[] password) throws HederaClientException {
		try {
			var passwordAuthenticator = new PasswordAuthenticator();
			setHashString(passwordAuthenticator.hash(password));
			setSalt(true);
		} catch (Exception exception) {
			throw new HederaClientException(exception);
		}
	}

	public void setHashString(String hashString) {
		properties.setProperty(HASH, hashString);
	}

	public void setMnemonicHashCode(int hashCode) {
		properties.setProperty(MNEMONIC_HASH_CODE, hashCode);
	}

	public int getMnemonicHashCode() {
		return properties.getIntProperty(MNEMONIC_HASH_CODE, 0);
	}

	// region Credential map operations
	public void resetCredentialsMap() {
		setOneDriveCredentials(new HashMap<>());
	}

	public void addOneDriveCredential(String path, String email) {
		var map = getOneDriveCredentials();
		if (map == null) {
			map = new HashMap<>();
		}
		map.put(path, email);
		setOneDriveCredentials(map);
	}

	public void removeOneDriveCredential(String path) {
		var map = getOneDriveCredentials();
		if (map == null) {
			map = new HashMap<>();
		}
		map.remove(path);
		setOneDriveCredentials(map);
	}

	public boolean credentialsMapCollision(String path, String email) {
		var map = getOneDriveCredentials();
		if (map == null) {
			map = new HashMap<>();
		}
		var keyBoolean = map.containsKey(path);
		var valueBoolean = findEmail(email);
		return keyBoolean || valueBoolean;
	}

	public boolean findEmail(String email) {
		var map = getOneDriveCredentials();
		if (map == null) {
			map = new HashMap<>();
		}
		var valueBoolean = false;
		for (var entry : map.entrySet()) {
			if (entry.getValue().equals(email)) {
				valueBoolean = true;
				break;
			}
		}
		return valueBoolean;
	}

	public String getEmailFromMap(String path) {
		var map = getOneDriveCredentials();
		var email = "";
		var path2 = (path.endsWith("/")) ? path.substring(0, Math.max(0, path.length() - 1)) : path.concat("/");
		if (map.containsKey(path)) {
			email = map.get(path);
		} else if (map.containsKey(path2)) {
			email = map.get(path2);
		}
		return email;
	}

	public void setVersionString(String version) {
		properties.setProperty(VERSION, version);
	}

	public String getVersionString() {
		return properties.getProperty(VERSION, "");
	}

	public boolean isKey(String candidateKey) {
		var candidatePath = new File(candidateKey).getPath();
		for (var key : getOneDriveCredentials().keySet()) {
			if (candidatePath.equals(new File(key).getPath())) {
				return true;
			}
		}
		return false;
	}

	/**
	 * The infoMap is a map that has nicknames as keys and filepaths as values. It is used to store the location of all
	 * the account info files that have been accepted by the app
	 *
	 * @return a Map of nicknames to account info locations
	 */
	public Map<String, String> getAccountInfoMap() {
		return properties.getMapProperty(ACCOUNT_INFO_MAP, new HashMap<>());
	}

	/**
	 * Stores a map that has nicknames as keys and filepaths as values, in the app properties
	 *
	 * @param infoMap
	 * 		a Map of nicknames to account info locations
	 */
	public void setAccountInfoMap(Map<String, String> infoMap) {
		properties.setProperty(ACCOUNT_INFO_MAP, infoMap);
	}

	//endregion

	//region Properties operations
	public void resetProperties() {
		properties.cleanProperties();
	}

	public void loadProperties(JsonObject jsonObject) {
		properties = new UserProperties(location, message);
		properties.jsonToProperties(jsonObject);
	}

	public JsonObject readProperties() {
		return properties.propertiesToJson();
	}

	public boolean hasSalt() {
		return properties.getBooleanProperty(SALT_PROPERTY, false);
	}

	public void setSalt(boolean salt) {
		properties.setProperty(SALT_PROPERTY, salt);
	}

	public boolean isLegacy() {
		return properties.getBooleanProperty(LEGACY, true);
	}

	public void setLegacy(boolean legacy) {
		properties.setProperty(LEGACY, legacy);
	}

	/**
	 * Gets the set of networks
	 *
	 * @return a Set of strings
	 */
	public Set<String> getCustomNetworks() {
		return properties.getSetProperty(CUSTOM_NETWORKS, new HashSet<>());
	}

	/**
	 * Sets the networks
	 *
	 * @param networks
	 * 		a set of strings
	 */
	public void setCustomNetworks(Set<String> networks) {
		properties.setSetProperty(CUSTOM_NETWORKS, networks);
	}

	public void setCurrentNetwork(String network, Set<String> defaulNetworks) {
		var networks = getCustomNetworks();
		if (!(networks.contains(network) || defaulNetworks.contains(network))) {
			return;
		}
		properties.setProperty(CURRENT_NETWORK, network);
	}

	public String getCurrentNetwork() {
		return properties.getProperty(CURRENT_NETWORK, "MAINNET");
	}

	public Identifier getDefaultFeePayer() {
		var idString = properties.getProperty(DEFAULT_FEE_PAYER, "");
		return idString.equals("") ? Identifier.ZERO : Identifier.parse(idString);
	}

	public void setDefaultFeePayer(Identifier feePayer) {
		properties.setProperty(DEFAULT_FEE_PAYER, feePayer.toReadableString());
	}

	public Set<Identifier> getCustomFeePayers() {
		var idStrings = properties.getSetProperty(CUSTOM_FEE_PAYERS, new HashSet<>());
		return idStrings.stream().map(Identifier::parse).collect(Collectors.toSet());
	}

	public void setCustomFeePayers(Set<Identifier> identifiers) {
		var payers = identifiers.stream().map(Identifier::toReadableString).collect(Collectors.toSet());
		properties.setSetProperty(CUSTOM_FEE_PAYERS, payers);
	}

	public void addCustomFeePayer(Identifier identifier) {
		var customFeePayers = getCustomFeePayers();
		customFeePayers.add(identifier);
		setCustomFeePayers(Collections.unmodifiableSet(customFeePayers));
	}

	public void removeCustomFeePayer(Identifier identifier) {
		var payers = getCustomFeePayers();
		payers.remove(identifier);
		setCustomFeePayers(Collections.unmodifiableSet(payers));
	}


	// endregion


}
