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


import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.Hbar;
import org.jetbrains.annotations.NotNull;

public class AccountLineInformation implements Comparable<AccountLineInformation> {
	private String nickname;
	private Identifier account;
	private Hbar balance;
	private String signer;
	private long date;

	public AccountLineInformation(String nickname, Identifier account, Hbar balance, long date, boolean signer) {
		this.nickname = nickname;
		this.account = account;
		this.balance = balance;
		this.date = date;
		this.signer = signer ? "Yes" : "No";
	}

	public String getNickname() {
		return nickname;
	}

	public void setNickname(String nickname) {
		this.nickname = nickname;
	}

	public Identifier getAccount() {
		return account;
	}

	public void setAccount(Identifier account) {
		this.account = account;
	}

	public Hbar getBalance() {
		return balance;
	}

	public void setBalance(Hbar balance) {
		this.balance = balance;
	}

	public String isSigner() {
		return signer;
	}

	public void setSigner(boolean signer) {
		this.signer = signer ? "Yes" : "No";
	}

	public long getDate() {
		return date;
	}

	public void setDate(long date) {
		this.date = date;
	}

	@Override
	public String toString() {
		return "AccountLineInformation{" +
				"nickname='" + nickname + '\'' +
				", account='" + account.toReadableString() + '\'' +
				", balance='" + balance + '\'' +
				", date='" + date + '\'' +
				", signer=" + signer +
				'}';
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof AccountLineInformation)) {
			return false;
		}
		var line = (AccountLineInformation) obj;
		return this.nickname.equals(line.getNickname()) &&
				this.account.equals(line.getAccount()) &&
				this.balance.equals(line.getBalance()) &&
				this.date == line.getDate()
				&& this.signer.equals(line.signer);
	}

	@Override
	public int hashCode() {
		return nickname.hashCode() + account.hashCode() + balance.hashCode() + Long.hashCode(
				date) + signer.hashCode();
	}

	@Override
	public int compareTo(@NotNull AccountLineInformation o) {
		return this.getAccount().compareTo(o.getAccount());
	}
}
