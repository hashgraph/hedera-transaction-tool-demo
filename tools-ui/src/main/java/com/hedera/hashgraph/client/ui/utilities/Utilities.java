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
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
import javafx.scene.control.CheckBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Pattern;
import java.util.stream.LongStream;

import static com.hedera.hashgraph.client.core.constants.Constants.KEY_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SALT_LENGTH;

public class Utilities {

	private static final Logger logger = LogManager.getLogger(Utilities.class);
	public static final String RED_BORDER_STYLE = "-fx-border-color: red";
	public static final String HBAR_STRING = "\u0127";

	private Utilities() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Check if a string contains a long
	 *
	 * @param strNum
	 * 		a numeric string
	 * @return true if the string is not a long
	 */
	public static boolean isNotLong(final String strNum) {
		if (strNum == null) {
			return false;
		}
		try {
			Long.parseLong(strNum);
		} catch (final NumberFormatException | NullPointerException nfe) {
			return true;
		}
		return false;
	}

	/**
	 * Given an instant returns the formatted date time string
	 *
	 * @param instant
	 * 		a date represented as an Instant
	 * @return a formatted string
	 */
	public static String instantToLocalTimeDate(final Instant instant) {
		final var ldt = LocalDateTime.ofInstant(instant, ZoneOffset.UTC);
		final var transactionValidStart = Date.from(ldt.atZone(ZoneId.of("UTC")).toInstant());
		final var localDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		final var tz = TimeZone.getDefault();
		return localDateFormat.format(transactionValidStart) + " " + tz.getDisplayName(true, TimeZone.SHORT);
	}

	/**
	 * Given a timestamp returns the formatted date time string
	 *
	 * @param timestamp
	 * 		a date represented as a Timestamp
	 * @return a formatted string
	 */
	public static String timestampToString(final Timestamp timestamp) {
		return instantToLocalTimeDate(timestamp.asInstant());
	}

	public static String setHBarFormat(final long amount) {
		final var symbols = new DecimalFormatSymbols();
		symbols.setGroupingSeparator(' ');
		final var formatInt = new DecimalFormat("###,###,###,###,###,###", symbols);
		final var formatFrac = new DecimalFormat("00,000,000", symbols);
		final var amountHBars = (amount - amount % 100000000) / 100000000d;
		final var amountTinyBars = (double) (amount % 100000000);

		if (amount % 100000000 == 0) {
			return formatInt.format(amountHBars).trim() + " " + HBAR_STRING;
		} else {
			return formatInt.format(amountHBars).trim() + "." + formatFrac.format(amountTinyBars) + " " + HBAR_STRING;
		}
	}

	public static String setCurrencyFormat(final long amount) {
		final var currency = setHBarFormat(amount);
		return currency.replace(" " + HBAR_STRING, "");
	}

	public static String stripHBarFormat(final String hBars) {
		if (hBars.equals("")) {
			return "0";
		}

		final var tiny = (hBars.contains(".")) ? hBars : hBars.concat(".00000000");
		final var amount = Long.parseLong(tiny.replace(HBAR_STRING, "")
				.replace(".", "")
				.replace(" ", ""));
		return String.valueOf(amount);
	}

	/**
	 * Convert a string to Hbar
	 *
	 * @param hBars
	 * 		a currency string
	 * @return the number of Hbars represented by the string
	 */
	public static Hbar string2Hbar(final String hBars) {
		if ("".equals(hBars) || hBars == null) {
			return new Hbar(0);
		}
		final var split = hBars.replace(" ", "").replace(HBAR_STRING, "").split("\\.");
		final var bars = Double.parseDouble(split[0]);
		final var tiny =
				split.length == 2 ? Double.parseDouble("0." + split[1].substring(0, Math.min(8, split[1].length())))
						: 0;
		return Hbar.from(BigDecimal.valueOf(bars).add(BigDecimal.valueOf(tiny)));
	}

	/**
	 * Given an account info, returns a list of string keys
	 *
	 * @param info
	 * 		the account info
	 * @param controller
	 * 		the controller
	 * @return a List of strings: If the controller has information about the public key, it uses the nickname,
	 * 		otherwise
	 * 		it shows the complete hex.
	 */
	public static List<String> getKeysFromInfo(final AccountInfo info, final Controller controller) {
		final var flatKey = EncryptionUtils.flatPubKeys(Collections.singletonList(info.key));
		final List<String> knownKeys = new ArrayList<>();
		for (final var key : flatKey) {
			final var keyName = controller.showKeyString(key);
			if (keyName.endsWith(PUB_EXTENSION)) {
				knownKeys.add(keyName);
			}
		}
		return knownKeys;
	}

	/**
	 * Retrieves the salt from the properties
	 *
	 * @param properties
	 * 		the properties file
	 * @return the salt
	 */
	public static byte[] getSaltBytes(final UserAccessibleProperties properties) {
		if (properties.hasSalt()) {
			final var token = properties.getHash();
			final var decoder = Base64.getDecoder();

			final var tokenBytes = decoder.decode(token);
			if (tokenBytes.length < Constants.SALT_LENGTH + KEY_LENGTH / 8) {
				logger.error("Token size check failed");
			}
			return Arrays.copyOfRange(tokenBytes, 0, Constants.SALT_LENGTH);
		}
		return new byte[SALT_LENGTH];
	}


	/**
	 * Parses a String into a list of accounts. Strings may be of the form "1-5, 8, 12-13"
	 *
	 * @param text
	 * 		a string containing a range of accounts, or a comma separated list of accounts, or a combination
	 * @return a list of accounts
	 */
	public static List<AccountId> parseAccountNumbers(final String text, final String network) {
		final List<AccountId> ids = new ArrayList<>();
		final List<String> ranges = new ArrayList<>();
		final List<String> singles = new ArrayList<>();

		if (text == null || "".equals(text)) {
			return ids;
		}

		final var split = text.replace("\\s", "").split("[\\s,]+");

		for (final String s : split) {
			if (s.contains("-")) {
				ranges.add(s);
				continue;
			}
			singles.add(s);
		}


		for (final String single : singles) {
			if (isNotIdentifier(single)) {
				logger.error("String {} cannot be parsed.", single);
				return new ArrayList<>();
			}
			final AccountId accountId = Identifier.parse(single, network).asAccount();
			ids.add(accountId);
		}

		for (final var s : ranges) {
			final var range = s.split("-");
			if (range.length != 2) {
				logger.error("String {} cannot be parsed into a range", s);
				return new ArrayList<>();
			}

			if (isNotIdentifier(range[0]) || isNotIdentifier(range[1])) {
				logger.error("Cannot parse account");
				return new ArrayList<>();
			}

			final Identifier start = Identifier.parse(range[0], network);
			final Identifier end = Identifier.parse(range[1], network);


			if (end.getShardNum() != start.getShardNum() || end.getRealmNum() != start.getRealmNum()) {
				logger.error("Cannot parse range: shards and realms must match");
				return new ArrayList<>();
			}

			LongStream.rangeClosed(Math.min(start.getAccountNum(), end.getAccountNum()),
							Math.max(start.getAccountNum(), end.getAccountNum()))
					.mapToObj(i -> new Identifier(start.getShardNum(), end.getRealmNum(), i, network).asAccount())
					.forEach(ids::add);
		}

		return ids;
	}

	/**
	 * Calculate the difference betweeen two json objects
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

	private static boolean isNotIdentifier(final String s) {
		try {
			Identifier.parse(s, "");
			return false;
		} catch (final Exception e) {
			return true;
		}
	}

	public static void checkBoxListener(final Set<File> signersSet, final File keyFile, final String baseName,
			final CheckBox checkBox) {
		checkBox.selectedProperty().addListener((observableValue, aBoolean, t1) -> {
			if (Boolean.TRUE.equals(t1)) {
				logger.info("Added {} to list of signing keys", baseName);
				signersSet.add(keyFile);
			} else {
				logger.info("Removed {} from list of signing keys", baseName);
				signersSet.remove(keyFile);
			}
		});
	}

	public static boolean isValidHost(String host) {
		String ipv4Pattern =
				"^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
						"(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
						"(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\." +
						"(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$";
		String domainPattern =
				"^(https?://)?" + // optional http or https
						"(([a-zA-Z0-9\\-]+\\.)+[a-zA-Z]{2,})$"; // domain name
		return Pattern.compile(ipv4Pattern).matcher(host).matches() ||
				Pattern.compile(domainPattern).matcher(host).matches();
	}

	public static boolean isValidPort(String port) {
		try {
			int portNumber = Integer.parseInt(port);
			return portNumber > 0 && portNumber <= 65535;
		} catch (NumberFormatException e) {
			return false;
		}
	}
}
