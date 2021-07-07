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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.core.json;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.util.Calendar;
import java.util.Date;
import java.util.Objects;
import java.util.TimeZone;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.NANOS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.SECONDS;


public class Timestamp {

	@JsonProperty(required = true)
	private long seconds;

	@JsonProperty(required = true)
	private int nanos;

	public Timestamp() {
		this(Instant.now());
	}

	public Timestamp(final int plusSeconds) {
		final var now = Instant.now().plusSeconds(plusSeconds);
		this.seconds = now.getEpochSecond();
		this.nanos = now.getNano();
	}

	public Timestamp(final Instant now) {
		this.seconds = now.getEpochSecond();
		this.nanos = now.getNano();
	}

	public Timestamp(final long seconds, final int nanos) {
		this.seconds = seconds;
		this.nanos = nanos;
	}

	public Timestamp(final JsonObject jsonObject) {
		if (!jsonObject.has(SECONDS) || !jsonObject.has(NANOS)) {
			throw new HederaClientRuntimeException("Cannot load json timestamp object");
		}
		this.seconds = jsonObject.get(SECONDS).getAsLong();
		this.nanos = jsonObject.get(NANOS).getAsInt();
	}

	public Timestamp(Duration duration) {
		this(duration.getSeconds(), duration.getNano());
	}

	public Timestamp(String rfcTimeString) throws ParseException {
		var sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		var date = sdf.parse(rfcTimeString);
		this.seconds = date.getTime() / 1000;
		this.nanos = 0;
	}

	public Timestamp(JsonElement jsonElement) throws ParseException {

		var timestamp = jsonElement.isJsonPrimitive() ? new Timestamp(jsonElement.getAsString()) : new Timestamp(
				jsonElement.getAsJsonObject());
		this.seconds = timestamp.getSeconds();
		this.nanos = timestamp.getNanos();
	}

	public long getSeconds() {
		return seconds;
	}

	public Timestamp plusSeconds(long seconds) {
		return new Timestamp(this.seconds + seconds, this.nanos);
	}

	public Timestamp plusNanos(int nanos) {
		if (this.nanos + nanos < 1000000000) {
			return new Timestamp(this.seconds, this.nanos + nanos);
		} else {
			return new Timestamp(this.seconds + 1, (this.nanos + nanos) - 1000000000);
		}
	}

	public void setSeconds(final long seconds) {
		this.seconds = seconds;
	}

	public int getNanos() {
		return nanos;
	}

	public void setNanos(final int nanos) {
		this.nanos = nanos;
	}

	public boolean isValid() {
		return seconds >= 0 && nanos >= 0 && nanos < 1_000_000_000;
	}

	public Instant asInstant() {
		return Instant.ofEpochSecond(seconds, nanos);
	}

	public long asNanos() {
		return seconds * 1000000000 + nanos;
	}

	public Duration asDuration() {
		return Duration.ofSeconds(this.getSeconds()).plusNanos(this.getNanos());
	}

	public String asUTCString() {
		var date = new Date(seconds * 1000);
		var sdf = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		return sdf.format(date);
	}

	public String asReadableLocalString() {
		var date = new Date(seconds * 1000);
		var sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		return sdf.format(date) + " " + TimeZone.getDefault().getDisplayName(false, TimeZone.SHORT);
	}

	public String asRFCString() {
		var date = new Date(seconds * 1000);
		var sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		return sdf.format(date);
	}


	public Calendar asCalendar() {
		var cal = Calendar.getInstance();
		cal.setTime(new Date(this.getSeconds() * 1000 + this.getNanos() / 1000000));
		return cal;
	}

	public Calendar asCalendarUTC() {
		var cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
		cal.setTime(new Date(this.getSeconds() * 1000 + this.getNanos() / 1000000));
		return cal;
	}

	public JsonObject asJSON() {
		var jsonObject = new JsonObject();
		jsonObject.addProperty(SECONDS, this.seconds);
		jsonObject.addProperty(NANOS, this.nanos);
		return jsonObject;
	}

	public void tick() {
		if (nanos != 999999999) {
			nanos++;
		} else {
			seconds++;
			nanos = 0;
		}
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		final var timestamp = (Timestamp) o;
		return seconds == timestamp.seconds &&
				nanos == timestamp.nanos;
	}

	@Override
	public int hashCode() {
		return Objects.hash(seconds, nanos);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.JSON_STYLE)
				.append(SECONDS, seconds)
				.append(NANOS, nanos)
				.toString();
	}
}
