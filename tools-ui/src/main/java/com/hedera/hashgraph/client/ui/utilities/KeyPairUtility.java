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
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nullable;
import java.io.File;
import java.security.KeyPair;
import java.security.KeyStoreException;
import java.util.Arrays;
import java.util.Base64;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.KEY_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MNEMONIC_PATH;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.ui.popups.PasswordBox.display;

public class KeyPairUtility {
	private static final Logger logger = LogManager.getLogger(KeyPairUtility.class);


	/**
	 * Constructor
	 */
	public KeyPairUtility() {
	}

	public Pair<String, KeyPair> getAccountKeyPair(File pemFile) {
		var message = "Enter your password to sign transactions, using the key: ".concat(pemFile.getName());
		var keyPair = getKeyPairFromPEM(pemFile, message);
		if (keyPair == null) {
			return null;
		}
		return Pair.of(pemFile.getName(), keyPair);
	}

	public KeyPair getKeyPairFromPEM(File pemFile, String message) {
		KeyPair keyPair = null;
		try {
			keyPair = getKeyPair(pemFile, message);
		} catch (Exception e) {
			PopupMessage.display("Error loading key", String.format("Unable to load private key: %s", e.getMessage()));
			logger.error(e.getMessage());
		}
		return keyPair;
	}


	@Nullable
	private KeyPair getKeyPair(File pemFile, String message) {
		KeyPair keyPair = null;
		var pwd = display("Enter password", message, pemFile.getAbsolutePath(), true);
		if (pwd == null || pwd.length == 0) {
			return null;
		}
		while (keyPair == null) {
			try {
				keyPair = getKeyPair(pemFile.getPath(), pwd);
				Arrays.fill(pwd, 'x');
			} catch (HederaClientException e) {
				pwd = askForPasswordAgain(pemFile);
				if (pwd == null || Arrays.equals(new char[0], pwd)) {
					return null;
				}
			}
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
	public static char[] resetPassword(String pemFile) throws HederaClientException, KeyStoreException {
		var properties = new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");

		// Check Hashcode
		var storedHashCode = properties.getMnemonicHashCode();
		var hashCode = Ed25519KeyStore.getMnemonicHashCode(pemFile);
		if (hashCode == null) {
			logger.error("Hashcode is null");
			PopupMessage.display("Error recovering password",
					"The key is not associated with the current recovery phrase. The password cannot be changed",
					"CONTINUE");
			return new char[0];
		}
		if (hashCode != storedHashCode) {
			logger.info("The key is not associated with the current mnemonic");
			PopupMessage.display("Error recovering password",
					"The key is not associated with the current recovery phrase. The password cannot be changed",
					"CONTINUE");
			return new char[0];
		}

		// get password bytes
		var token = properties.getHash();
		var decoder = Base64.getDecoder();
		var index = Ed25519KeyStore.getIndex(pemFile);
		var tokenBytes = decoder.decode(token);
		if (tokenBytes.length < Constants.SALT_LENGTH + KEY_LENGTH / 8) {
			logger.error("Token size check failed");
			return new char[0];
		}
		var salt = Arrays.copyOfRange(tokenBytes, 0, Constants.SALT_LENGTH);

		// load mnemonic
		var mnemonicPwd = PasswordBox.display("Password", "Please enter your recovery phrase password.", "", false);
		var mnemonic = SecurityUtilities.fromEncryptedFile(mnemonicPwd, salt,
				properties.getPreferredStorageDirectory() + File.separator + MNEMONIC_PATH);

		// Store key with new password
		var newPassword = NewPasswordPopup.display();
		SecurityUtilities.generateAndStoreKey(pemFile, "Transaction Tool UI", mnemonic, index, newPassword);
		return newPassword;
	}

	@Nullable
	private char[] askForPasswordAgain(File pemFile) {
		return display("Error", "The password entered does not match " + pemFile.getName() + ". Please try again.",
				pemFile.getAbsolutePath(),
				true);
	}

	private KeyPair getKeyPair(String path, char[] pwd) throws HederaClientException {
		final KeyStore keyPairs;
		try {
			keyPairs = Ed25519KeyStore.read(pwd, path);
		} catch (KeyStoreException e) {
			throw new HederaClientException(e);
		}
		return (!keyPairs.isEmpty()) ? keyPairs.get(0) : null;
	}


}
