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

package com.hedera.hashgraph.client.core.utils;

import org.junit.jupiter.api.Test;

import java.util.Calendar;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TimeUtilsTest {

	@Test
	void excelDateTimeToCalendar() {
		final var dateString = "2025-12-25T08:32:11";
		var date = TimeUtils.excelDateTimeToCalendar(dateString);
		assertNotNull(date);
		assertEquals(2025, date.get(Calendar.YEAR));
		assertEquals(11, date.get(Calendar.MONTH));
		assertEquals(25, date.get(Calendar.DAY_OF_MONTH));
		assertEquals(32, date.get(Calendar.MINUTE));
		assertEquals(11, date.get(Calendar.SECOND));

		final var shortDate = "04/18/52";
		date = TimeUtils.excelDateTimeToCalendar(shortDate);
		assertNotNull(date);
		assertEquals(1952, date.get(Calendar.YEAR));
		assertEquals(3, date.get(Calendar.MONTH));

		final var badDate = "665465T564";
		date = TimeUtils.excelDateTimeToCalendar(badDate);
		assertNull(date);
	}

	@Test
	void notNegativeValueCheck() {
		assertDoesNotThrow(() -> TimeUtils.notNegativeValueCheck(1, "aField"));

		final Exception exception1 =
				assertThrows(IllegalArgumentException.class, () -> TimeUtils.notNegativeValueCheck(-1, "aField"));
		assertEquals("aField must be greater than zero", exception1.getMessage());

	}
}
