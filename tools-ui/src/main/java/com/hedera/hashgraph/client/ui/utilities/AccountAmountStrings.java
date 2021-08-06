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

import com.google.gson.JsonElement;
import com.hedera.hashgraph.sdk.proto.AccountID;


public class AccountAmountStrings {
	private String accountID;
	private String amount;

	public AccountAmountStrings(String accountID, String amount) {
		if (accountID.contains(".")) {
			this.accountID = accountID;
		} else {
			this.accountID = String.format("0.0.%s", accountID);
		}

		var temp = amount.replace("-", "").replace(" ", "").replace("\u0127", "").replace(".", "");

		this.amount = ((amount.contains("-")) ? "- " : "") + Utilities.setHBarFormat(
				Long.parseLong(temp.substring(0, Math.min(temp.length(), 19))));

	}

	public AccountAmountStrings(JsonElement transferElement) {
		this(transferElement.getAsJsonObject().get("accountID").getAsString(),
				transferElement.getAsJsonObject().get("amount").getAsString());
	}

	public String getAccountID() {
		return accountID;
	}

	public AccountID getAccountIDAsAccountID() {
		var strings = accountID.split("\\.");
		return AccountID.newBuilder().setShardNum(Long.parseLong(strings[0])).setRealmNum(
				Long.parseLong(strings[1])).setAccountNum(Long.parseLong(strings[2])).build();
	}

	public void setAccountID(String accountID) {
		if (accountID.contains(".")) {
			this.accountID = accountID;
		} else {
			this.accountID = String.format("0.0.%s", accountID);
		}
	}

	public String getAmount() {
		return amount;
	}

	public long getAmountAsLong() {
		var sign = (amount.contains("-")) ? -1L : 1L;
		var temp = (amount.contains(".")) ? amount : amount + ".00000000";
		return sign * Long.parseLong(temp.replace("\u0127", "")
				.replace(" ", "")
				.replace(".", "")
				.replace("-", ""));
	}

	public void setAmount(String amount) {
		this.amount = Utilities.setHBarFormat(Long.parseLong(amount));
	}

	public AccountAmountStrings negate() {

		if (this.amount.contains("-")) {
			return new AccountAmountStrings(this.accountID, this.amount.replace("-", ""));
		} else {
			return new AccountAmountStrings(this.accountID, String.format("- %d", getAmountAsLong()));
		}
	}
}
