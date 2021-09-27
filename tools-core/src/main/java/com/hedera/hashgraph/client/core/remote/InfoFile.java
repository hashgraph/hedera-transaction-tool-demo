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

package com.hedera.hashgraph.client.core.remote;


import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PublicKey;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static org.apache.commons.io.FileUtils.contentEquals;

public class InfoFile extends RemoteFile implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(InfoFile.class);

	private Identifier accountID;
	private Timestamp timestamp;    // Note: Using the file creation time as the timestamp for the account info
	private Key key;
	private final List<FileActions> actions = Arrays.asList(FileActions.ACCEPT, FileActions.DECLINE);

	public InfoFile(FileDetails file) {
		super(file);
		if (file == null) {
			return;
		}
		try {
			var accountInfo = AccountInfo.fromBytes(readBytes(file.getFullPath()));
			this.key = accountInfo.key;
			this.accountID = new Identifier(accountInfo.accountId);
			this.timestamp = new Timestamp(file.getAttributes().creationTime().toMillis() / 1000, 0);
		} catch (IOException | HederaClientException e) {
			logger.error(e);
			this.setValid(false);
		}
	}

	public Identifier getAccountID() {
		return accountID;
	}

	public Timestamp getTimestamp() {
		return timestamp;
	}

	public Key getKey() {
		return key;
	}

	public boolean canSign(Set<PublicKey> keys) {
		final var b = canSign(this.key, keys);
		return b;
	}

	private boolean canSign(Key key, Set<PublicKey> keys) {
		if (key instanceof KeyList) {
			var keyList = (KeyList) key;
			int threshold = (keyList.threshold != null) ? keyList.threshold : keyList.size();
			var count = 0;
			for (Key componentKey : keyList) {
				if (canSign(componentKey, keys)) {
					count++;
				}
				if (count >= threshold) {
					return true;
				}
			}
		}
		if (key instanceof PublicKey) {
			return keys.contains((PublicKey) key);
		}
		return false;
	}

	public boolean exists() {
		return new File(Constants.DEFAULT_STORAGE,
				"Accounts/" + getAccountID().toReadableString() + "." + INFO_EXTENSION).exists();
	}

	public boolean duplicate() {
		if (!this.exists()) {
			return false;
		}

		try {
			return contentEquals(
					new File(ACCOUNTS_INFO_FOLDER + this.accountID.toReadableString() + ".info"),
					new File(getPath()));
		} catch (IOException e) {
			logger.error(e);
		}
		return false;
	}

	@Override
	public boolean isExpired() {
		return duplicate();
	}

	@Override
	public GridPane buildGridPane() {
		var details = new GridPane();

		List<Label> messages = new ArrayList<>();
		var l = new Label(exists() ? String.format(
				"We have found new information regarding account %s. Would you like to import it to your records?",
				accountID.toReadableString()) : String.format(
				"Would you like to import information regarding account %s to your records?",
				accountID.toReadableString()));
		l.setWrapText(true);
		messages.add(l);

		if (isHistory()) {
			try {
				messages = getHistory("account");
			} catch (HederaClientException e) {
				logger.error(e);
			}
		}
		var count = 0;
		for (var message : messages) {
			details.add(message, 0, count++);
		}
		return details;
	}

	@Override
	public List<FileActions> getActions() {
		return actions;
	}

	@Override
	public boolean equals(Object o) {
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
