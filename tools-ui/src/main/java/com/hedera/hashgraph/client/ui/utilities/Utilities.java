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

import com.google.common.collect.Maps;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.HbarUnit;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.lang.reflect.Type;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.LongStream;

public class Utilities {
	private static final Logger LOG = LogManager.getLogger(Utilities.class);
	public static final String RED_BORDER_STYLE = "-fx-border-color: red";

	// TODO if there isn't a specific format for all dates, then this isn't very useful. Perhaps move this into Timestamp
	/**
	 * Given an instant returns the formatted date time string
	 *
	 * @param instant
	 * 		a date represented as an Instant
	 * @return a formatted string
	 */
	@NotNull
	public static String instantToLocalTimeDate(@NotNull final Instant instant) {
		// Convert the instant to a ZonedDateTime, using the local zone
		final var ldt = ZonedDateTime.ofInstant(instant, ZoneId.systemDefault());
		final var localDateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z");
		return localDateFormatter.format(ldt);
	}

//	for these three, I need to know the desired format
//			sometimes it uses ',' and sometimes ' '

	public static String setCurrencyFormat(final long amount) {
		final var symbols = new DecimalFormatSymbols();
		symbols.setGroupingSeparator(' ');
		final var formatInt = new DecimalFormat("###,###,###,###,###,###", symbols);
		final var formatFrac = new DecimalFormat("00,000,000", symbols);
		final var amountHBars = (amount - amount % 100000000) / 100000000d;
		final var amountTinyBars = (double) (amount % 100000000);

		if (amount % 100000000 == 0) {
			return formatInt.format(amountHBars).trim();
		} else {
			return formatInt.format(amountHBars).trim() + "." + formatFrac.format(amountTinyBars);
		}
	}

	public static String stripHBarFormat(final String hBars) {
		if (hBars.isBlank()) {
			return "0";
		}

		final var tiny = (hBars.contains(".")) ? hBars : hBars.concat(".00000000");
		final var amount = Long.parseLong(tiny.replace(HbarUnit.HBAR.getSymbol(), "")
				.replace(".", "")
				.replace(" ", ""));
		return String.valueOf(amount);
	}






	/**
	 * Parses a String into a list of accounts. Strings may be of the form "1-5, 8, 12-13"
	 *
	 * @param text
	 * 		a string containing a range of accounts, or a comma separated list of accounts, or a combination
	 * @return a list of accounts
	 */
	public static List<AccountId> parseAccountNumbers(final String text, final String network) {
		final var ids = new ArrayList<AccountId>();
		final var ranges = new ArrayList<String>();
		final var singles = new ArrayList<String>();

		if (text == null || text.isBlank()) {
			return ids;
		}

		final var split = text.replace("\\s", "").split("[,]+");

		for (final String s : split) {
			if (s.contains("-")) {
				ranges.add(s);
				continue;
			}
			singles.add(s);
		}


		for (final String single : singles) {
			try {
				final AccountId accountId = Identifier.parse(single, network).asAccount();
				ids.add(accountId);
			} catch (final Exception ex) {
				LOG.error("String {} cannot be parsed.", single);
				return new ArrayList<>();
			}
		}

		for (final var s : ranges) {
			final var range = s.split("-");
			if (range.length != 2) {
				LOG.error("String {} cannot be parsed into a range", s);
				return new ArrayList<>();
			}

			try {
				final Identifier start = Identifier.parse(range[0], network);
				final Identifier end = Identifier.parse(range[1], network);

				if (end.getShardNum() != start.getShardNum() || end.getRealmNum() != start.getRealmNum()) {
					LOG.error("Cannot parse range: shards and realms must match");
					return new ArrayList<>();
				}

				LongStream.rangeClosed(Math.min(start.getAccountNum(), end.getAccountNum()),
								Math.max(start.getAccountNum(), end.getAccountNum()))
						.mapToObj(i -> new Identifier(start.getShardNum(), end.getRealmNum(), i, network).asAccount())
						.forEach(ids::add);
			} catch (final Exception ex) {
				LOG.error("Cannot parse account");
				return new ArrayList<>();
			}
		}

		return ids;
	}

	/**
	 * Calculate the difference between two json objects
	 *
	 * @param j1
	 * 		the first json object
	 * @param j2
	 * 		the second json object
	 * @return a set containing the keys that are different from one to the second
	 */
	public static List<String> difference(final JsonObject j1, final JsonObject j2) {
		final Gson g = new Gson();
		final Type mapType = new TypeToken<Map<String, Object>>() {
		}.getType();
		final Map<String, Object> first = g.fromJson(j1, mapType);
		final Map<String, Object> second = g.fromJson(j2, mapType);
		final var diff = Maps.difference(first, second);

		final Set<String> keys = new HashSet<>();
		if (!diff.entriesDiffering().isEmpty()) {
			keys.addAll(diff.entriesDiffering().keySet());
		}
		if (!diff.entriesOnlyOnLeft().isEmpty()) {
			keys.addAll(diff.entriesOnlyOnLeft().keySet());
		}
		if (!diff.entriesOnlyOnRight().isEmpty()) {
			keys.addAll(diff.entriesOnlyOnRight().keySet());
		}
		final List<String> sorted = new ArrayList<>(keys);
		Collections.sort(sorted);
		return sorted;
	}
}
