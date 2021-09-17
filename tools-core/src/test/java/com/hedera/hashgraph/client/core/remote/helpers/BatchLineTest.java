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

package com.hedera.hashgraph.client.core.remote.helpers;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;


class BatchLineTest {

	@Test
	void parse_test() throws HederaClientException {
		var line = "0.0.15299,16666700000000,1/8/28";
		var hours = 6;
		var minutes = 35;

		var parsed = BatchLine.parse(line, hours, minutes);
		assertEquals("{\"receiverAccountID\":{\"realmNum\":0,\"shardNum\":0,\"accountNum\":15299}," +
						"\"amount\":16666700000000,\"dateTime\":\"{\\\"seconds\\\":1830926100,\\\"nanos\\\":0}\"," +
						"\"memo" +
						"\":\"\"}",
				parsed.toString());

		assertEquals(new Timestamp(1830926100, 0), parsed.getDate());
		assertEquals(new Identifier(0, 0, 15299), parsed.getReceiverAccountID());
		assertEquals(16666700000000L, parsed.getAmount());

		assertEquals(1074973882, parsed.hashCode());

		Exception exception = assertThrows(HederaClientException.class, () -> BatchLine.parse(line, 30, minutes));
		assertEquals("Hedera Client: Out of range exception: hour", exception.getMessage());

		exception = assertThrows(HederaClientException.class, () -> BatchLine.parse(line, hours, 74));
		assertEquals("Hedera Client: Out of range exception: minute", exception.getMessage());

		var bad_date = "0.0.15299,16666700000000,1/DD/28";
		exception = assertThrows(HederaClientException.class, () -> BatchLine.parse(bad_date, hours, minutes));
		assertEquals("Hedera Client: Cannot parse date", exception.getMessage());

		parsed.setDate(new Timestamp(1830826100, 1000));
		assertEquals(new Timestamp(1830826100, 1000), parsed.getDate());
		assertNotEquals(1281602566, parsed.hashCode());

		parsed.setMemo("memo line");
		assertEquals("memo line", parsed.getMemo());

	}

	@Test
	void compareBatchLines_test() throws HederaClientException {
		var line0 = "0.0.15299,16666700000000,1/8/28";
		var hours0 = 6;
		var minutes0 = 35;

		var parsed0 = BatchLine.parse(line0, hours0, minutes0);


		assertEquals(parsed0, BatchLine.parse(line0, hours0, minutes0));
		assertNotEquals(parsed0, BatchLine.parse(line0, 5, minutes0));
		assertNotEquals(parsed0, BatchLine.parse(line0, hours0, 40));
		assertNotEquals("test", parsed0);


		var line1 = "0.0.15299,16666700000000,2/8/28";
		var line2 = "0.0.5299,16666700000000,1/8/28";
		var line3 = "0.0.15299,26666700000000,1/8/28";

		assertEquals(0, parsed0.compareTo(BatchLine.parse(line0, hours0, minutes0)));
		assertTrue(parsed0.compareTo(BatchLine.parse(line0, hours0, 30)) > 0);
		assertTrue(parsed0.compareTo(BatchLine.parse(line0, hours0, 45)) < 0);
		assertEquals(0, parsed0.compareTo(BatchLine.parse(line0, hours0, minutes0)));
		assertTrue(parsed0.compareTo(BatchLine.parse(line1, hours0, minutes0)) < 0);
		assertTrue(parsed0.compareTo(BatchLine.parse(line2, hours0, minutes0)) > 0);
		assertTrue(parsed0.compareTo(BatchLine.parse(line3, hours0, minutes0)) < 0);
	}

	@Test
	void newParse_test() throws HederaClientException {
		var line = "0.0.15299,16666700000000,1/8/28, \"memo line\"";
		var hours = 6;
		var minutes = 35;

		var parsed = BatchLine.parse(line, hours, minutes);
		assertEquals("{\"receiverAccountID\":{\"realmNum\":0,\"shardNum\":0,\"accountNum\":15299}," +
						"\"amount\":16666700000000,\"dateTime\":\"{\\\"seconds\\\":1830926100,\\\"nanos\\\":0}\"," +
						"\"memo" +
						"\":\"memo line\"}",
				parsed.toString());

		assertEquals(new Timestamp(1830926100, 0), parsed.getDate());
		assertEquals(new Identifier(0, 0, 15299), parsed.getReceiverAccountID());
		assertEquals(16666700000000L, parsed.getAmount());
		assertEquals("memo line", parsed.getMemo());

		var line0 = "0.0.15299,16666700000000,1/8/28, another memo ";
		var hours0 = 6;
		var minutes0 = 35;

		var line1 = "0.0.15299,16666700000000,1/8/28, yet another memo ";
		final var parsed0 = BatchLine.parse(line0, hours0, minutes0);
		final var parsed1 = BatchLine.parse(line1, hours0, minutes0);
		assertNotEquals(parsed0, parsed1);
		assertTrue(parsed0.compareTo(parsed1) < 0);

	}
}