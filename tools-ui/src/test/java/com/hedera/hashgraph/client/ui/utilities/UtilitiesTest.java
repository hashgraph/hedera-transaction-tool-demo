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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.sdk.Hbar;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Disabled("Temporarily disabling tests")
public class UtilitiesTest {


	@Test
	public void isNotLong() {
//		final var aLong = "123456789123";
//		assertFalse(Utilities.isNotLong(aLong));
//		final var notALong = "testString";
//		assertTrue(Utilities.isNotLong(notALong));
	}

	@Test
	public void timestampFormat_test() {
		final var instant = Instant.ofEpochMilli(1633443320000L);
		assertTrue(Utilities.instantToLocalTimeDate(instant).contains("2021-10-05"));
		assertTrue(Utilities.instantToLocalTimeDate(instant).contains("15:20"));

		final var timeStamp = new Timestamp(1633443320, 0);
//		assertTrue(Utilities.timestampToString(timeStamp).contains("2021-10-05"));
//		assertTrue(Utilities.timestampToString(timeStamp).contains("15:20"));
	}

	@Test
	public void hBarFormatting_test() {
//		assertEquals("6 515 165 164.65 165 165 ħ", Utilities.setHBarFormat(651516516465165165L));
//		assertEquals("6 515 165 164 ħ", Utilities.setHBarFormat(651516516400000000L));
//		assertEquals("0.00 000 005 ħ", Utilities.setHBarFormat(5L));
//		assertEquals("0 ħ", Utilities.setHBarFormat(0L));

//		assertEquals("6 515 165 164.65 165 165", Utilities.setCurrencyFormat(651516516465165165L));
//		assertEquals("6 515 165 164", Utilities.setCurrencyFormat(651516516400000000L));
//		assertEquals("0.00 000 005", Utilities.setCurrencyFormat(5L));
//		assertEquals("0", Utilities.setCurrencyFormat(0L));

//		assertEquals("651516516465165165", Utilities.stripHBarFormat("6 515 165 164.65 165 165 ħ"));
//		assertEquals("651516516400000000", Utilities.stripHBarFormat("6 515 165 164 ħ"));
//		assertEquals("5", Utilities.stripHBarFormat("0.00 000 005 ħ"));
//		assertEquals("0", Utilities.stripHBarFormat("0 ħ"));

//		assertEquals(Hbar.fromTinybars(651516516465165165L), Utilities.string2Hbar("6 515 165 164.65 165 165 ħ"));
//		assertEquals(Hbar.fromTinybars(651516516400000000L), Utilities.string2Hbar("6 515 165 164 ħ"));
//		assertEquals(Hbar.fromTinybars(5), Utilities.string2Hbar("0.00 000 005 ħ"));
//		assertEquals(Hbar.fromTinybars(0), Utilities.string2Hbar("0 ħ"));


	}

	@Test
	public void parseAccountNumbers() {

		final var testString = "";
		final var parseList = Utilities.parseAccountNumbers(testString, "MAINNET");
		assertTrue(parseList.isEmpty());

		final var testString0 = "1,2 ,3";
		final var parseList0 = Utilities.parseAccountNumbers(testString0, "MAINNET");

		assertEquals(3, parseList0.size());
		assertTrue(parseList0.contains(new Identifier(0, 0, 1, "MAINNET").asAccount()));
		assertTrue(parseList0.contains(new Identifier(0, 0, 2, "MAINNET").asAccount()));
		assertTrue(parseList0.contains(new Identifier(0, 0, 3, "MAINNET").asAccount()));

		final var testString1 = "1,2 ,k";
		final var parseList1 = Utilities.parseAccountNumbers(testString1, "MAINNET");
		assertTrue(parseList1.isEmpty());

		final var testString2 = "1-3";
		final var parseList2 = Utilities.parseAccountNumbers(testString2, "MAINNET");
		assertTrue(parseList2.contains(new Identifier(0, 0, 1, "MAINNET").asAccount()));
		assertTrue(parseList2.contains(new Identifier(0, 0, 2, "MAINNET").asAccount()));
		assertTrue(parseList2.contains(new Identifier(0, 0, 3, "MAINNET").asAccount()));

		final var testString3 = "1-2,3";
		final var parseList3 = Utilities.parseAccountNumbers(testString3, "MAINNET");
		assertTrue(parseList3.contains(new Identifier(0, 0, 1, "MAINNET").asAccount()));
		assertTrue(parseList3.contains(new Identifier(0, 0, 2, "MAINNET").asAccount()));
		assertTrue(parseList3.contains(new Identifier(0, 0, 3, "MAINNET").asAccount()));

		final var testString4 = "0.1";
		final var parseList4 = Utilities.parseAccountNumbers(testString4, "MAINNET");
		assertTrue(parseList4.isEmpty());

		final var testString5 = "1.2.1-1.2.3";
		final var parseList5 = Utilities.parseAccountNumbers(testString5, "MAINNET");
		assertTrue(parseList5.contains(new Identifier(1, 2, 1, "MAINNET").asAccount()));
		assertTrue(parseList5.contains(new Identifier(1, 2, 2, "MAINNET").asAccount()));
		assertTrue(parseList5.contains(new Identifier(1, 2, 3, "MAINNET").asAccount()));

		final var testString6 = "1.2.1-2.2.3";
		final var parseList6 = Utilities.parseAccountNumbers(testString6, "MAINNET");
		assertTrue(parseList6.isEmpty());

		final var testString7 = "1.2.1-1.k.3";
		final var parseList7 = Utilities.parseAccountNumbers(testString7, "MAINNET");
		assertTrue(parseList7.isEmpty());

		final var testString8 = "l.2.1-1.2.3";
		final var parseList8 = Utilities.parseAccountNumbers(testString8, "MAINNET");
		assertTrue(parseList8.isEmpty());

		final var testString9 = "121-";
		final var parseList9 = Utilities.parseAccountNumbers(testString9, "MAINNET");
		assertTrue(parseList9.isEmpty());

		final var testString10 = "121-156-6";
		final var parseList10 = Utilities.parseAccountNumbers(testString10, "MAINNET");
		assertTrue(parseList10.isEmpty());
	}

	@Test
	public void difference() {
		final JsonObject j1 = new JsonObject();
		j1.addProperty("prop1", 1);
		j1.addProperty("prop2", 2L);
		j1.addProperty("prop3", "tres");
		j1.addProperty("prop4", 4.008);

		final JsonObject j2 = new JsonObject();
		j2.addProperty("prop1", 1);
		j2.addProperty("prop2", 2L);
		j2.addProperty("prop3", "tres");
		j2.addProperty("prop4", 4.008);

		assertTrue(Utilities.difference(j1, j2).isEmpty());

		j2.addProperty("prop1", "ONE");
		assertEquals(1, Utilities.difference(j1, j2).size());
		assertTrue(Utilities.difference(j1, j2).contains("prop1"));

		j2.addProperty("prop5", "A new one");
		assertEquals(2, Utilities.difference(j1, j2).size());
		assertTrue(Utilities.difference(j1, j2).contains("prop1"));
		assertTrue(Utilities.difference(j1, j2).contains("prop5"));

	}
}