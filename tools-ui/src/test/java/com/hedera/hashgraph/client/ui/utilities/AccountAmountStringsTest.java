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
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AccountAmountStringsTest {

	@Test
	void getStrippedAccount_test() {
		var accountAmmount = new AccountAmountStrings("nickname (654654.9946546.3345667-atest)", "8098098080");
		var strippedAccount = accountAmmount.getStrippedAccountID();
		assertEquals("654654.9946546.3345667-atest", strippedAccount);
	}

	@Test
	void constructor_test() {
		var account = "nickname (654654.9946546.3345667-cdtxd)";
		var amount = "123 654.98 765 432 ħ";

		var accountAmount1 = new AccountAmountStrings(account, amount);
		assertEquals(account, accountAmount1.getAccountID());
		assertEquals(amount, accountAmount1.getAmount());
		assertEquals(12365498765432L, accountAmount1.getAmountAsLong());

		var id = Identifier.parse(account);
		assertEquals(id.asJSON(), accountAmount1.getAccountAsJSON());

		var accountNoNickname = "654654.9946546.3345667-cdtxd";
		var accountAmount2 = new AccountAmountStrings(accountNoNickname, amount);
		assertEquals(accountNoNickname, accountAmount2.getAccountID());
		assertEquals(amount, accountAmount2.getAmount());
		assertEquals(12365498765432L, accountAmount2.getAmountAsLong());

		var accountNoChecksum = "654654.9946546.3345667";
		var accountAmount3 = new AccountAmountStrings(accountNoChecksum, amount);
		assertEquals(accountNoNickname, accountAmount3.getAccountID());
		assertEquals(amount, accountAmount3.getAmount());
		assertEquals(12365498765432L, accountAmount3.getAmountAsLong());

		var accountJustNumber = "3345667";
		var accountAmount4 = new AccountAmountStrings(accountJustNumber, amount);
		assertEquals("0.0.3345667-fskwk", accountAmount4.getAccountID());
		assertEquals(amount, accountAmount4.getAmount());
		assertEquals(12365498765432L, accountAmount4.getAmountAsLong());

		var badAccount = "9946546.3345667";
		Exception exception1 =
				assertThrows(HederaClientRuntimeException.class, () -> new AccountAmountStrings(badAccount, amount));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"9946546.3345667\" cannot be parsed",
				exception1.getMessage());

		var badAmount = "bad amount";
		Exception exception2 =
				assertThrows(HederaClientRuntimeException.class, () -> new AccountAmountStrings(account, badAmount));
		assertEquals("Hedera Client Runtime: Bad amount format: Cannot parse \"bad amount\" to an hbar amount",
				exception2.getMessage());

	}

	@Test
	void setters_test() {
		var accountAmount = new AccountAmountStrings("nickname (654654.9946546.3345667-cdtxd)", "123 654.98 765 432 ħ");

		var account = "testNickname (1,2,345-atest)";
		accountAmount.setAccountID(account);
		assertEquals(account, accountAmount.getAccountID());

		var accountNoNickname = "654654.9946546.3345667-cdtxd";
		accountAmount.setAccountID(accountNoNickname);
		assertEquals(accountNoNickname, accountAmount.getAccountID());

		var accountNoChecksum = "654654.9946546.3345667";
		accountAmount.setAccountID(accountNoChecksum);
		assertEquals(accountNoNickname, accountAmount.getAccountID());

		var accountJustNumber = "3345667";
		accountAmount.setAccountID(accountJustNumber);
		assertEquals("0.0.3345667-fskwk", accountAmount.getAccountID());

		var anotherAmount = "123456789";
		accountAmount.setAmount(anotherAmount);
		assertEquals("1.23 456 789 ħ", accountAmount.getAmount());

		var badAccount = "9946546.3345667";
		Exception exception1 =
				assertThrows(HederaClientRuntimeException.class, () -> accountAmount.setAccountID(badAccount));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"9946546.3345667\" cannot be parsed",
				exception1.getMessage());

		var badAmount = "bad amount";
		Exception exception2 =
				assertThrows(HederaClientRuntimeException.class, () -> accountAmount.setAmount(badAmount));
		assertEquals("Hedera Client Runtime: Bad amount format: Cannot parse \"bad amount\" to an hbar amount",
				exception2.getMessage());

	}

	@Test
	void negate_test() {
		var account = "nickname (654654.9946546.3345667-cdtxd)";
		var positiveAmount = "123 654.98 765 432 ħ";
		var negativeAmount = "- 123 654.98 765 432 ħ";

		var accountAmount1 = new AccountAmountStrings(account, positiveAmount);
		var accountAmount2 = new AccountAmountStrings(account, negativeAmount);

		assertEquals(accountAmount1, accountAmount2.negate());
		assertEquals(accountAmount2, accountAmount1.negate());

	}

	@Test
	void hashCode_test() {
		var account = "nickname (654654.9946546.3345667-cdtxd)";
		var positiveAmount = "123 654.98 765 432 ħ";
		var negativeAmount = "- 123 654.98 765 432 ħ";

		var accountAmount1 = new AccountAmountStrings(account, positiveAmount);
		var accountAmount2 = new AccountAmountStrings(account, negativeAmount);

		assertEquals(21471835, accountAmount2.hashCode());
		assertEquals(-928570296, accountAmount1.hashCode());

	}
}