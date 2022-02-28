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

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.Hbar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

public class AccountLineInformationTest {
	private static final Logger logger = LogManager.getLogger(AccountLineInformationTest.class);

	@Test
	public void gettersSetters_test() throws HederaClientException {
		final var line =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "MAINNET");
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
				new Hbar(2), 123456987L, true, "MAINNET"), line);
	}

	@Test
	public void toString_test() {
		final var line =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		assertEquals(
				"AccountLineInformation{nickname='nickname', account='0.0.100100', ledger='MAINNET', balance='159 ℏ'," +
						" " +
						"date='1632923362000', signer=No}",
				line.toString());
	}

	@Test
	public void equals_test() {
		final var line1 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		final var line2 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		final var line3 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, true, "MAINNET");
		final var line4 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362100L, false, "MAINNET");
		final var line5 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(153), 1632923362000L, false, "MAINNET");
		final var line6 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 10010),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		final var line7 =
				new AccountLineInformation("nick_name", new Identifier(0, 0, 10010),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		assertEquals(line1, line2);
		assertNotEquals(line1, line3);
		assertNotEquals(line1, line4);
		assertNotEquals(line1, line5);
		assertNotEquals(line1, line6);
		assertNotEquals(line1, line7);

		assertNotEquals("not a line", line1);

		assertEquals(0, line1.compareTo(line2));
		assertEquals(0, line1.compareTo(line3));
		assertEquals(0, line1.compareTo(line4));
		assertEquals(0, line1.compareTo(line5));
		assertEquals(1, line1.compareTo(line6));
		assertEquals(-1, line7.compareTo(line1));
	}

	@Test
	public void hashCode_test() {
		final var line1 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		final var line2 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		final var line3 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, true, "MAINNET");
		final var line4 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362100L, false, "MAINNET");
		final var line5 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(153), 1632923362000L, false, "MAINNET");
		final var line6 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 10010),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		final var line7 =
				new AccountLineInformation("nick_name", new Identifier(0, 0, 10010),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		assertEquals(-1792439098, line1.hashCode());
		assertEquals(-1792439098, line2.hashCode());
		assertEquals(-1792352852, line3.hashCode());
		assertEquals(-1792439454, line4.hashCode());
		assertEquals(-1792444864, line5.hashCode());
		assertEquals(-1792529188, line6.hashCode());
		assertEquals(314721877, line7.hashCode());
	}

	@Test
	public void ledger_test() {
		final var line1 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "MAINNET");
		final var line2 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "PREVIEWNET");
		final var line3 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, "TESTNET");
		final var line4 =
				new AccountLineInformation("nickname", new Identifier(0, 0, 100100),
						new Hbar(159), 1632923362000L, false, null);

		assertNotEquals(line1, line2);
		assertNotEquals(line1, line3);
		assertNotEquals(line1, line4);
		assertNotEquals(line3, line4);

	}
}