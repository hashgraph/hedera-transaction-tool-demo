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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.ui.utilities;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.KeyStore;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nullable;
import java.io.File;
import java.security.KeyPair;
import java.security.KeyStoreException;
import java.util.Arrays;

import static com.hedera.hashgraph.client.ui.popups.PasswordBox.display;
import static com.hedera.hashgraph.client.ui.utilities.Utilities.showErrorAlert;

public class KeyPairUtility {

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
			showErrorAlert("Not able to load private key. Error: " + e.getMessage());
		}
		return keyPair;
	}

	@Nullable
	private KeyPair getKeyPair(File pemFile, String message) {
		KeyPair keyPair = null;
		var pwd = display("Enter password", message, "", true);
		if (pwd == null || pwd.length == 0) {
			return null;
		}
		while (keyPair == null) {
			try {
				keyPair = getKeyPair(pemFile.getPath(), pwd);
				Arrays.fill(pwd, 'x');
			} catch (HederaClientException e) {
				pwd = askForPasswordAgain(pemFile);
				if (pwd == null) {
					return null;
				}
			}
		}
		return keyPair;
	}

	@Nullable
	private char[] askForPasswordAgain(File pemFile) {
		return display("Error", "The password entered does not match " + pemFile.getName() + ". Please try again.",
				"", true);
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
