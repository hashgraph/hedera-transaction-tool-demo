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
import org.junit.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

public class AccountLineInformationTest {

	@Test
	public void gettersSetters_test() {
		var line =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false);
		assertEquals("nickname", line.getNickname());
		assertEquals(new Identifier(0, 0, 100100), line.getAccount());
		assertEquals(new Hbar(159), line.getBalance());
		assertEquals(1632923362000L, line.getDate());
		assertEquals("No", line.isSigner());

		line.setNickname("nickname2");
		assertEquals("nickname2", line.getNickname());

		line.setAccount(new Identifier(1, 2, 3));
		assertEquals(new Identifier(1, 2, 3), line.getAccount());

		line.setBalance(new Hbar(2));
		assertEquals(new Hbar(2), line.getBalance());

		line.setDate(123456987L);
		assertEquals(123456987L, line.getDate());

		line.setSigner(true);
		assertEquals("Yes", line.isSigner());

		assertEquals(new AccountLineInformation("nickname2", new Identifier(1, 2, 3),
				new Hbar(2), 123456987L, true), line);
	}

	@Test
	public void toString_test() {
		var line =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false);
		assertEquals(
				"AccountLineInformation{nickname='nickname', account='0.0.100100', balance='159 ℏ', " +
						"date='1632923362000', signer=No}",
				line.toString());
	}

	@Test
	public void equals_test() {
		var line1 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false);
		var line2 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false);
		var line3 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, true);
		var line4 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362100L, false);
		var line5 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(153), 1632923362000L, false);
		var line6 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 10010),
						new Hbar(159), 1632923362000L, false);
		var line7 =
				new AccountLineInformation("nick_name", new Identifier(0, 0, 10010),
						new Hbar(159), 1632923362000L, false);
		assertEquals(line1, line2);
		assertNotEquals(line1, line3);
		assertNotEquals(line1, line4);
		assertNotEquals(line1, line5);
		assertNotEquals(line1, line6);
		assertNotEquals(line1, line7);

		assertFalse(line1.equals("not a line"));

		assertEquals(0, line1.compareTo(line2));
		assertEquals(0, line1.compareTo(line3));
		assertEquals(0, line1.compareTo(line4));
		assertEquals(0, line1.compareTo(line5));
		assertEquals(1, line1.compareTo(line6));
		assertEquals(-1, line7.compareTo(line1));
	}

	@Test
	public void hashCode_test() {
		var line1 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false);
		var line2 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false);
		var line3 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, true);
		var line4 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362100L, false);
		var line5 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(153), 1632923362000L, false);
		var line6 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 10010),
						new Hbar(159), 1632923362000L, false);
		var line7 =
				new AccountLineInformation("nick_name", new Identifier(0, 0, 10010),
						new Hbar(159), 1632923362000L, false);
		assertEquals(-373256064, line1.hashCode());
		assertEquals(-373256064, line2.hashCode());
		assertEquals(-373169818, line3.hashCode());
		assertEquals(-373256420, line4.hashCode());
		assertEquals(-973256064, line5.hashCode());
		assertEquals(-373346154, line6.hashCode());
		assertEquals(1733904911, line7.hashCode());
	}
}