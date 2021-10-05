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

import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.sdk.Hbar;
import org.junit.Test;

import java.time.Instant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class UtilitiesTest {


	@Test
	public void isNotLong() {
		var aLong = "123456789123";
		assertFalse(Utilities.isNotLong(aLong));
		var notALong = "testString";
		assertTrue(Utilities.isNotLong(notALong));
	}

	@Test
	public void timestampFormat_test() {
		var instant = Instant.ofEpochMilli(1633443320000L);
		assertTrue(Utilities.instantToLocalTimeDate(instant).contains("2021-10-05"));
		assertTrue(Utilities.instantToLocalTimeDate(instant).contains("15:20"));

		var timeStamp = new Timestamp(1633443320,0);
		assertTrue(Utilities.timestampToString(timeStamp).contains("2021-10-05"));
		assertTrue(Utilities.timestampToString(timeStamp).contains("15:20"));
	}

	@Test
	public void hBarFormatting_test() {
		assertEquals("6 515 165 164.65 165 165 ħ", Utilities.setHBarFormat(651516516465165165L));
		assertEquals("6 515 165 164 ħ", Utilities.setHBarFormat(651516516400000000L));
		assertEquals("0.00 000 005 ħ", Utilities.setHBarFormat(5L));
		assertEquals("0 ħ", Utilities.setHBarFormat(0L));

		assertEquals("6 515 165 164.65 165 165", Utilities.setCurrencyFormat(651516516465165165L));
		assertEquals("6 515 165 164", Utilities.setCurrencyFormat(651516516400000000L));
		assertEquals("0.00 000 005", Utilities.setCurrencyFormat(5L));
		assertEquals("0", Utilities.setCurrencyFormat(0L));

		assertEquals("651516516465165165", Utilities.stripHBarFormat("6 515 165 164.65 165 165 ħ"));
		assertEquals("651516516400000000", Utilities.stripHBarFormat("6 515 165 164 ħ"));
		assertEquals("5", Utilities.stripHBarFormat("0.00 000 005 ħ"));
		assertEquals("0", Utilities.stripHBarFormat("0 ħ"));

		assertEquals(Hbar.fromTinybars(651516516465165165L), Utilities.string2Hbar("6 515 165 164.65 165 165 ħ"));
		assertEquals(Hbar.fromTinybars(651516516400000000L), Utilities.string2Hbar("6 515 165 164 ħ"));
		assertEquals(Hbar.fromTinybars(5), Utilities.string2Hbar("0.00 000 005 ħ"));
		assertEquals(Hbar.fromTinybars(0), Utilities.string2Hbar("0 ħ"));


	}
}