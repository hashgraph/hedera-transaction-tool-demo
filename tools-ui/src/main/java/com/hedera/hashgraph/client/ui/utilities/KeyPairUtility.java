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


package com.hedera.hashgraph.client.ui.utilities;

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.KeyStore;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.ui.popups.NewPasswordPopup;
import com.hedera.hashgraph.client.ui.popups.PasswordBox;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import org.apache.commons.collections4.map.PassiveExpiringMap;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nullable;
import java.io.File;
import java.security.KeyPair;
import java.security.KeyStoreException;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.KEY_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.ui.popups.PasswordBox.display;

public class KeyPairUtility {
	private static final Logger logger = LogManager.getLogger(KeyPairUtility.class);
	public static final String ERROR_RECOVERING_PASSWORD_TITLE = "Error recovering password";
	public static final String ERROR_RECOVERING_PASSWORD_MESSAGE =
			"The key is not associated with the current recovery phrase. The password cannot be changed";
	public static final String CONTINUE = "CONTINUE";


	private final PassiveExpiringMap<String, char[]> expiringMap;
	private final long expirationTime;

	/**
	 * Default Constructor
	 */
	public KeyPairUtility() {
		this.expirationTime = 300L;
		final PassiveExpiringMap.ConstantTimeToLiveExpirationPolicy<String, char[]> expirationPolicy =
				new PassiveExpiringMap.ConstantTimeToLiveExpirationPolicy<>(expirationTime,
						TimeUnit.SECONDS);
		expiringMap = new PassiveExpiringMap<>(expirationPolicy);
	}

	/**
	 * Constructor: Used for testing purposes
	 *
	 * @param expirationTime
	 * 		the time it takes for passwords to expire.
	 */
	public KeyPairUtility(final long expirationTime) {
		this.expirationTime = expirationTime;
		final PassiveExpiringMap.ConstantTimeToLiveExpirationPolicy<String, char[]> expirationPolicy =
				new PassiveExpiringMap.ConstantTimeToLiveExpirationPolicy<>(expirationTime,
						TimeUnit.SECONDS);
		expiringMap = new PassiveExpiringMap<>(expirationPolicy);
	}

	/**
	 * Checks if a key name is in the map.
	 *
	 * @param name
	 * 		the name of the key
	 * @return true if the key is still in the map
	 */
	public boolean isInMap(final String name) {
		return expiringMap.containsKey(name);
	}

	/**
	 * Returns the number of keys that can be decrypted.
	 *
	 * @return the size of the map
	 */
	public int mapSize() {
		return expiringMap.size();
	}

	public Pair<String, KeyPair> getAccountKeyPair(final File pemFile) {
		final var message = "Enter your password to sign transactions, using the key: ".concat(pemFile.getName());
		final var keyPair = getKeyPairFromPEM(pemFile, message);
		if (keyPair == null) {
			return null;
		}
		return Pair.of(pemFile.getName(), keyPair);
	}

	/**
	 * Decripts a key pair file
	 *
	 * @param pemFile
	 * 		the file that stores a pem file
	 * @param message
	 * 		the message that will be used to request the password from the user
	 * @return a decrypted keypair
	 */
	public KeyPair getKeyPairFromPEM(final File pemFile, final String message) {
		KeyPair keyPair = null;
		try {
			keyPair = getKeyPair(pemFile, message);
		} catch (final Exception e) {
			PopupMessage.display("Error loading key", String.format("Unable to load private key: %s", e.getMessage()));
			logger.error(e.getMessage());
		}
		return keyPair;
	}

	/**
	 * Given a pemFile, resets the password and returns it
	 *
	 * @param pemFile
	 * 		the file where the pem file is stored;
	 * @return the new password
	 */
	public static char[] resetPassword(final String pemFile) throws HederaClientException, KeyStoreException {
		final var properties = new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");

		// Check Hashcode
		final var storedHashCode = properties.getMnemonicHashCode();
		final var hashCode = Ed25519KeyStore.getMnemonicHashCode(pemFile);
		if (hashCode == null) {
			logger.error("Hashcode is null");
			PopupMessage.display(ERROR_RECOVERING_PASSWORD_TITLE, ERROR_RECOVERING_PASSWORD_MESSAGE, CONTINUE);
			return new char[0];
		}
		if (hashCode != storedHashCode) {
			logger.info("The key is not associated with the current mnemonic");
			PopupMessage.display(ERROR_RECOVERING_PASSWORD_TITLE, ERROR_RECOVERING_PASSWORD_MESSAGE, CONTINUE);
			return new char[0];
		}

		// get password bytes
		final var token = properties.getHash();
		final var decoder = Base64.getDecoder();
		final var index = Ed25519KeyStore.getIndex(pemFile);
		final var tokenBytes = decoder.decode(token);
		if (tokenBytes.length < Constants.SALT_LENGTH + KEY_LENGTH / 8) {
			logger.error("Token size check failed");
			return new char[0];
		}
		final var salt = Arrays.copyOfRange(tokenBytes, 0, Constants.SALT_LENGTH);

		// load mnemonic
		final var mnemonicPwd =
				PasswordBox.display("Password", "Please enter your recovery phrase password.", "", false);
		final var mnemonic = SecurityUtilities.fromEncryptedFile(mnemonicPwd, salt,
				properties.getPreferredStorageDirectory() + File.separator + MNEMONIC_PATH);

		// Store key with new password
		final var newPassword = NewPasswordPopup.display();
		SecurityUtilities.generateAndStoreKey(pemFile, "Transaction Tool UI", mnemonic, index, newPassword);
		return newPassword;
	}

	@Nullable
	private KeyPair getKeyPair(final File pemFile, final String message) {

		KeyPair keyPair = useStored(pemFile);
		if (keyPair != null) {
			return keyPair;
		}
		var pwd = display("Enter password", message, pemFile.getAbsolutePath(), true);
		if (pwd == null || pwd.length == 0) {
			return null;
		}
		while (keyPair == null) {
			final var path = pemFile.getPath();
			try {
				keyPair = getKeyPair(path, pwd);
				expiringMap.put(path, pwd);
			} catch (final HederaClientException e) {
				expiringMap.remove(path);
				pwd = askForPasswordAgain(pemFile);
				if (pwd == null || Arrays.equals(new char[0], pwd)) {
					return null;
				}
			}
		}
		return keyPair;
	}

	/**
	 * Check if any of the keys of the expiring map can be used to decrypt the KeyPair
	 *
	 * @param pem
	 * 		the file that contains an encrypted key pair
	 * @return a decrypted key pair if the map contains the right key. Null otherwise
	 */
	private KeyPair useStored(final File pem) {
		KeyPair keyPair = null;
		int count = 0;
		logger.info("Trying stored passwords.");
		final var path = pem.getPath();
		if (expiringMap.containsKey(path)) {
			try {
				keyPair = getKeyPair(path, expiringMap.get(path));
			} catch (final HederaClientException e) {
				logger.error("Cannot decrypt pem");
				expiringMap.remove(path);
			}
		}
		final Set<char[]> knownPwds = new HashSet<>(expiringMap.values());
		for (final char[] chars : knownPwds) {
			try {
				keyPair = getKeyPair(path, chars);
				logger.info("Password found");
				expiringMap.put(path, chars);
				return keyPair;
			} catch (final HederaClientException e) {
				logger.info("Trying password {}", count);
				count++;
			}
		}
		logger.info("Password not found. Asking the user");
		return keyPair;
	}

	@Nullable
	private char[] askForPasswordAgain(final File pemFile) {
		return display("Error", "The password entered does not match " + pemFile.getName() + ". Please try again.",
				pemFile.getAbsolutePath(),
				true);
	}

	private KeyPair getKeyPair(final String path, final char[] pwd) throws HederaClientException {
		final KeyStore keyPairs;
		try {
			keyPairs = Ed25519KeyStore.read(pwd, path);
		} catch (final KeyStoreException e) {
			throw new HederaClientException(e);
		}
		return (!keyPairs.isEmpty()) ? keyPairs.get(0) : null;
	}

}
