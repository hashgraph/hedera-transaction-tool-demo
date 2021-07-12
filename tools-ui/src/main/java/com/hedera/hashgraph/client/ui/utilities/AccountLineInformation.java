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


import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.Hbar;

public class AccountLineInformation implements Comparable {
	private String nickname;
	private Identifier account;
	private Hbar balance;
	private String signer;

	public AccountLineInformation(String nickname, Identifier account, Hbar balance, boolean signer) {
		this.nickname = nickname;
		this.account = account;
		this.balance = balance;
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

	@Override
	public String toString() {
		return "AccountLineInformation{" +
				"nickname='" + nickname + '\'' +
				", account='" + account.toReadableString() + '\'' +
				", balance='" + balance + '\'' +
				", signer=" + signer +
				'}';
	}

	@Override
	public int compareTo(Object o) {
		if (o == null) {
			throw new NullPointerException("Cannot compare to a null object");
		}
		if (!(o instanceof AccountLineInformation)) {
			throw new HederaClientRuntimeException("Incompatible types");
		}
		return this.getAccount().compareTo(((AccountLineInformation) o).getAccount());

	}

	@Override
	public boolean equals(Object obj) {
		if (!super.equals(obj)) {
			return false;
		}
		if (!(obj instanceof AccountLineInformation)) {
			return false;
		}
		var line = (AccountLineInformation) obj;
		return this.nickname.equals(line.getNickname()) &&
				this.account.equals(line.getAccount()) &&
				this.balance.equals(line.getBalance())
				&& this.signer.equals(line.signer);
	}

	@Override
	public int hashCode() {
		return super.hashCode() + nickname.hashCode() + account.hashCode() + balance.hashCode() + signer.hashCode();
	}
}
