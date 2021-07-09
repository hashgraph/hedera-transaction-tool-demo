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

import javafx.beans.property.SimpleStringProperty;

import java.util.List;

public class KeysTableRow {

	private SimpleStringProperty keyName;
	private SimpleStringProperty accountList;
	private String index;
	private boolean signer;
	private String mnemonic;
	private String iconFile;

	public KeysTableRow(SimpleStringProperty keyName, SimpleStringProperty accountList, String index,
			boolean signer, boolean mnemonic) {
		this.keyName = keyName;
		this.accountList = accountList;
		this.index = index;
		this.signer = signer;
		this.mnemonic = (mnemonic && signer) ? "\u2713" : ((!mnemonic && signer) ? "\u2718" : "");
		this.iconFile = (!signer) ? "icons/1GreyKey.png" : (mnemonic) ? "icons/2GreenKeys.png" : "icons/2BlueKeys.png";
	}

	public KeysTableRow(String keyName, String index, boolean signer, boolean mnemonic) {
		this(new SimpleStringProperty(keyName), new SimpleStringProperty("No account found"), index, signer, mnemonic);
	}

	public KeysTableRow(String keyName, String accountList, String index, boolean signer, boolean mnemonic) {
		this(new SimpleStringProperty(keyName), new SimpleStringProperty(accountList), index, signer, mnemonic);
	}

	public KeysTableRow(String keyName, List<String> accounts, String index, boolean signer, boolean mnemonic) {
		this(keyName, index, signer, mnemonic);
		this.accountList = new SimpleStringProperty(getStringAccountList(accounts));
	}

	public String getKeyName() {
		return keyName.get();
	}

	public SimpleStringProperty keyNameProperty() {
		return keyName;
	}

	public void setKeyName(String keyName) {
		this.keyName.set(keyName);
	}

	public String getAccountList() {
		return accountList.get();
	}

	public SimpleStringProperty accountListProperty() {
		return accountList;
	}

	public void setAccountList(String accountList) {
		this.accountList.set(accountList);
	}

	public String getIndex() {
		return index;
	}

	public void setIndex(String index) {
		this.index = index;
	}

	public boolean isSigner() {
		return signer;
	}

	public void setSigner(boolean signer) {
		this.signer = signer;
	}

	public String getMnemonic() {
		return mnemonic;
	}

	public void setMnemonic(boolean mnemonic) {
		this.mnemonic = (mnemonic && signer) ? "\u2713" : ((!mnemonic && signer) ? "\u2718" : "");
	}

	public String getIconFile() {
		return iconFile;
	}

	public void setIconFile(String iconFile) {
		this.iconFile = iconFile;
	}

	private String getStringAccountList(List<String> accounts) {
		if (accounts == null || accounts.isEmpty()) {
			return "No account found";
		}

		var builder = new StringBuilder();
		accounts.forEach(account -> {
			if (builder.length() != 0) {
				builder.append(", ");
			}
			builder.append(account);
		});

		return builder.toString();
	}
}
