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
import javafx.beans.property.SimpleStringProperty;

import java.util.Collections;
import java.util.List;

import static com.hedera.hashgraph.client.core.constants.Constants.*;

public class KeysTableRow {

	private final SimpleStringProperty keyName;
	private SimpleStringProperty accountList;
	private String index;
	private boolean signer;
	private String mnemonic;
	private String iconFile;

	public KeysTableRow(final SimpleStringProperty keyName, final SimpleStringProperty accountList, final String index,
			final boolean signer, final boolean mnemonic) {
		this.keyName = keyName;
		this.accountList = accountList;
		this.index = index;
		this.signer = signer;
		if (mnemonic && signer) {
			this.mnemonic = "\u2713";
		} else if (!mnemonic && signer) {
			this.mnemonic = "\u2718";
		} else {
			this.mnemonic = "";
		}
		if (!signer) {
			this.iconFile = "icons/1GreyKey.png";
		} else if (mnemonic) {
			this.iconFile = "icons/2GreenKeys.png";
		} else {
			this.iconFile = "icons/2BlueKeys.png";
		}
	}

	public KeysTableRow(final String keyName, final String index, final boolean signer, final boolean mnemonic) {
		this(new SimpleStringProperty(keyName), new SimpleStringProperty(NO_ACCOUNT_FOUND_TEXT), index, signer, mnemonic);
	}

	public KeysTableRow(
			final String keyName, final String accountList, final String index, final boolean signer,
			final boolean mnemonic) {
		this(new SimpleStringProperty(keyName), new SimpleStringProperty(accountList), index, signer, mnemonic);
	}

	public KeysTableRow(final String keyName, final List<String> accounts, final String index, final boolean signer,
			final boolean mnemonic) {
		this(keyName, index, signer, mnemonic);
		this.accountList = new SimpleStringProperty(getStringAccountList(accounts));
	}

	public String getKeyName() {
		return keyName.get();
	}

	public SimpleStringProperty keyNameProperty() {
		return keyName;
	}

	public void setKeyName(final String keyName) {
		this.keyName.set(keyName);
	}

	public String getAccountList() {
		return accountList.get();
	}

	public SimpleStringProperty accountListProperty() {
		return accountList;
	}

	public void setAccountList(final String accountList) {
		this.accountList.set(accountList);
	}

	public String getIndex() {
		return index;
	}

	public void setIndex(final String index) {
		this.index = index;
	}

	public boolean isSigner() {
		return signer;
	}

	public void setSigner(final boolean signer) {
		this.signer = signer;
	}

	public String getMnemonic() {
		return mnemonic;
	}

	public void setMnemonic(final boolean mnemonic) {
		if (mnemonic && signer) {
			this.mnemonic = "\u2713";
		} else if (!mnemonic && signer) {
			this.mnemonic = "\u2718";
		} else {
			this.mnemonic = "";
		}
	}

	public String getIconFile() {
		return iconFile;
	}

	public void setIconFile(final String iconFile) {
		this.iconFile = iconFile;
	}

	private String getStringAccountList(final List<String> accounts) {
		Collections.sort(accounts);
		if (accounts.isEmpty()) {
			return NO_ACCOUNT_FOUND_TEXT;
		}

		final var builder = new StringBuilder();
		accounts.forEach(account -> {
			if (builder.length() != 0) {
				builder.append(", ");
			}
			builder.append(account);
		});

		return builder.toString();
	}
}
