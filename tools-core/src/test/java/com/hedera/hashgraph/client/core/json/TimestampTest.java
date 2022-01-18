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

package com.hedera.hashgraph.client.core.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import org.junit.jupiter.api.Test;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TimestampTest {

	@Test
	void constructors_test() throws ParseException {
		final var t0 = new Timestamp(123654, 11111);

		final var t1 = new JsonObject();
		t1.addProperty("seconds", 123654);
		t1.addProperty("nanos", 11111);
		assertEquals(t0, new Timestamp(t1));

		final var t2 = Duration.ofSeconds(123654, 11111);
		assertEquals(t0, new Timestamp(t2));

		final var t3 = t0.asRFCString();
		assertEquals(t0.getSeconds(), new Timestamp(t3).getSeconds());

		final var array = new JsonArray();
		array.add(t1);
		array.add(t3);

		final var t4 = array.get(0);
		assertEquals(t0, new Timestamp(t4));
		final var t5 = array.get(0);
		assertEquals(t0.getSeconds(), new Timestamp(t5).getSeconds());

		final var t6 = new JsonObject();
		t6.addProperty("seconds", 123654);
		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> new Timestamp(t6));
		assertEquals("Hedera Client Runtime: Cannot load json timestamp object", e.getMessage());
	}

	@Test
	void seconds_Test() {
		final var timestamp0 = new Timestamp();
		final var timestamp1 = new Timestamp(10);
		assertEquals(10, timestamp1.getSeconds() - timestamp0.getSeconds());

		timestamp0.setSeconds(timestamp1.getSeconds());
		timestamp0.setNanos(timestamp1.getNanos());

		assertEquals(timestamp1, timestamp0);
	}

	@Test
	void plusUnit_Test() {
		final var timestamp0 = new Timestamp(1603506625, 0);
		final var timestamp1 = timestamp0.plusSeconds(100);
		assertEquals(100, timestamp1.getSeconds() - timestamp0.getSeconds());

		timestamp0.setNanos(999999990);
		final var timestamp2 = timestamp0.plusNanos(5);
		assertEquals(timestamp0.getSeconds(), timestamp2.getSeconds());
		assertEquals(999999995, timestamp2.getNanos());

		final var timestamp3 = timestamp0.plusNanos(15);
		assertEquals(timestamp0.getSeconds() + 1, timestamp3.getSeconds());
		assertEquals(5, timestamp3.getNanos());

		timestamp3.tick();
		assertEquals(6, timestamp3.getNanos());

		final var timestamp4 = timestamp0.plusNanos(9);
		timestamp4.tick();
		assertEquals(timestamp0.getSeconds() + 1, timestamp4.getSeconds());
		assertEquals(0, timestamp4.getNanos());
	}

	@Test
	void isValid_Test() {
		final var timestamp = new Timestamp(100, -20);
		assertFalse(timestamp.isValid());
	}

	@Test
	void asInstant_Test() {
		final var instant = Instant.now();
		final var timestamp = new Timestamp(instant.getEpochSecond(), instant.getNano());
		assertEquals(instant, timestamp.asInstant());
	}

	@Test
	void duration_Test() {
		final var timestamp0 = new Timestamp(1603506625, 0);
		final var duration = Duration.ofMillis(1603506625000L);
		assertEquals(timestamp0, new Timestamp(duration));
	}

	@Test
	void asDuration_Test() {
		final var duration = Duration.ofSeconds(979898997, 78787);
		final var t = new Timestamp(duration);
		assertEquals(duration, t.asDuration());
	}

	@Test
	void asNanos_Test() {
		final var timestamp = new Timestamp(100, 100);
		assertEquals(100000000100L, timestamp.asNanos());
	}

	@Test
	void asString_Test() {
		final var timestamp = new Timestamp(1603506625, 0);
		assertEquals("2020-10-24T02:30:25", timestamp.asRFCString());
		assertEquals("2020-10-24_02:30:25", timestamp.asUTCString());

		final var sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		final var local =
				sdf.format(new Date(1603506625000L)) + " " + TimeZone.getDefault().getDisplayName(false,
						TimeZone.SHORT);
		assertEquals(local, timestamp.asReadableLocalString());

		final var calendar0 = Calendar.getInstance();
		calendar0.setTime(new Date(1603506625000L));
		assertEquals(calendar0, timestamp.asCalendar());

		final var calendar1 = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
		calendar1.setTime(new Date(1603506625000L));
		assertEquals(calendar1, timestamp.asCalendarUTC());
	}

	@Test
	void asJSON_Test() {
		final var timestamp1 = new Timestamp(1603506625, 123654447);
		final var timestampJson = new JsonObject();
		timestampJson.addProperty("seconds", 1603506625);
		timestampJson.addProperty("nanos", 123654447);

		assertEquals(timestampJson, timestamp1.asJSON());

		assertEquals("{\"seconds\":1603506625,\"nanos\":123654447}", timestamp1.toString());
	}

	@SuppressWarnings("SimplifiableAssertion")
	@Test
	void equals_Test() {
		final var timestamp = new Timestamp();
		assertEquals(timestamp, timestamp);
		//noinspection ConstantConditions
		final Timestamp nullTimestamp = null;
		assertFalse(timestamp.equals(nullTimestamp));
		//noinspection EqualsBetweenInconvertibleTypes
		assertFalse(timestamp.equals("A string"));
	}

	@Test
	void hash_Test() {
		final var timestamp = new Timestamp(1603506625, 123654447);
		assertEquals(-1707246769, timestamp.hashCode());
	}

}
