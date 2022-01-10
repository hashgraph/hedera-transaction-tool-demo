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
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.utils.CommonMethods;

import java.util.regex.Pattern;

import static com.hedera.hashgraph.client.core.constants.Constants.FULL_ACCOUNT_CHECKSUM_REGEX;


public class AccountAmountStrings {
	private String accountID;
	private String amount;

	public AccountAmountStrings(String accountID, String amount) {
		this.accountID = parseAccountString(accountID);
		this.amount = parseAmountString(amount);
	}

	private String parseAmountString(String amount) {
		try {
			var temp = amount.replace("-", "").replace(" ", "").replace("\u0127", "").replace(".", "");
			return (amount.contains("-") ? "- " : "") + Utilities.setHBarFormat(
					Long.parseLong(temp.substring(0, Math.min(temp.length(), 19))));
		} catch (NumberFormatException e) {
			throw new HederaClientRuntimeException(
					String.format("Bad amount format: Cannot parse \"%s\" to an hbar amount", amount));
		}
	}

	private String parseAccountString(String accountID) {
		var pattern = Pattern.compile(FULL_ACCOUNT_CHECKSUM_REGEX);
		var matcher = pattern.matcher(accountID);
		if (matcher.find()) {
			return accountID;
		}
		return Identifier.parse(accountID).toReadableStringAndChecksum();
	}

	public String getAccountID() {
		return accountID;
	}

	public String getStrippedAccountID() {
		final var value = this.accountID;
		return CommonMethods.removeNickname(value);
	}

	public JsonElement getAccountAsJSON() {
		return Identifier.parse(accountID).asJSON();
	}

	public void setAccountID(String accountID) {
		this.accountID = parseAccountString(accountID);
	}

	public String getAmount() {
		return amount;
	}

	public long getAmountAsLong() {
		var sign = amount.contains("-") ? -1L : 1L;
		var temp = amount.contains(".") ? amount : amount + ".00000000";
		return sign * Long.parseLong(temp.replace("\u0127", "")
				.replace(" ", "")
				.replace(".", "")
				.replace("-", ""));
	}

	public void setAmount(String amount) {
		this.amount = parseAmountString(amount);
	}

	public AccountAmountStrings negate() {
		if (this.amount.contains("-")) {
			return new AccountAmountStrings(this.accountID, this.amount.replace("-", ""));
		} else {
			return new AccountAmountStrings(this.accountID, String.format("- %d", getAmountAsLong()));
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof AccountAmountStrings)) {
			return false;
		}

		return this.accountID.equals(
				((AccountAmountStrings) obj).getAccountID()) && getAmountAsLong() == ((AccountAmountStrings) obj).getAmountAsLong();
	}

	@Override
	public int hashCode() {
		return this.accountID.hashCode() + this.amount.hashCode();
	}
}
