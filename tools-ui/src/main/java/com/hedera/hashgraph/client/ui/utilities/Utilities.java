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
import javafx.animation.PauseTransition;
import javafx.scene.control.Control;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.Pane;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
	public static boolean isNotLong(String strNum) {
		if (strNum == null) {
			return false;
		}
		try {
			Long.parseLong(strNum);
		} catch (NumberFormatException | NullPointerException nfe) {
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
		var ldt = LocalDateTime.ofInstant(instant, ZoneOffset.UTC);
		var transactionValidStart = Date.from(ldt.atZone(ZoneId.of("UTC")).toInstant());
		var localDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		var tz = TimeZone.getDefault();
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
		var symbols = new DecimalFormatSymbols();
		symbols.setGroupingSeparator(' ');
		var formatInt = new DecimalFormat("###,###,###,###,###,###", symbols);
		var formatFrac = new DecimalFormat("00,000,000", symbols);
		var amountHBars = (amount - amount % 100000000) / 100000000d;
		var amountTinyBars = (double) (amount % 100000000);

		if (amount % 100000000 == 0) {
			return formatInt.format(amountHBars).trim() + " " + HBAR_STRING;
		} else {
			return formatInt.format(amountHBars).trim() + "." + formatFrac.format(amountTinyBars) + " " + HBAR_STRING;
		}
	}

	public static String setCurrencyFormat(final long amount) {
		var currency = setHBarFormat(amount);
		return currency.replace(" " + HBAR_STRING, "");
	}

	public static String stripHBarFormat(final String hBars) {
		if (hBars.equals("")) {
			return "0";
		}

		var tiny = (hBars.contains(".")) ? hBars : hBars.concat(".00000000");
		var amount = Long.parseLong(tiny.replace(HBAR_STRING, "")
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
	public static Hbar string2Hbar(String hBars) {
		if ("".equals(hBars) || hBars == null) {
			return new Hbar(0);
		}
		var split = hBars.replace(" ", "").replace(HBAR_STRING, "").split("\\.");
		var bars = Double.parseDouble(split[0]);
		var tiny =
				split.length == 2 ? Double.parseDouble("0." + split[1].substring(0, Math.min(8, split[1].length())))
						: 0;
		return Hbar.from(BigDecimal.valueOf(bars).add(BigDecimal.valueOf(tiny)));
	}

	/**
	 * Shows an informational tooltip when the user presses a button
	 *
	 * @param owner
	 * 		pane that will show the tooltip
	 * @param control
	 * 		the node that will be attached to (typically a button)
	 * @param tooltipText
	 * 		the text that will be displayed
	 */
	public static void showTooltip(Pane owner, Control control, String tooltipText) {
		var customTooltip = new Tooltip();
		var p = control.localToScene(15.0, 15.0);
		customTooltip.setText(tooltipText);
		customTooltip.setStyle("-fx-background-color: white; -fx-text-fill: black;");
		customTooltip.setMaxWidth(300);
		customTooltip.setWrapText(true);
		control.setTooltip(customTooltip);

		customTooltip.setAutoHide(true);

		if (customTooltip.isShowing()) {
			customTooltip.hide();
		} else {
			customTooltip.show(owner, p.getX()
					+ control.getScene().getX() + control.getScene().getWindow().getX(), p.getY()
					+ control.getScene().getY() + control.getScene().getWindow().getY());
		}

		var pt = new PauseTransition(new javafx.util.Duration(5000));
		pt.setOnFinished(e -> customTooltip.hide());
		pt.play();
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
	public static List<String> getKeysFromInfo(AccountInfo info, Controller controller) {
		var flatKey = EncryptionUtils.flatPubKeys(Collections.singletonList(info.key));
		List<String> knownKeys = new ArrayList<>();
		for (var key : flatKey) {
			var keyName = controller.showKeyString(key);
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
	public static byte[] getSaltBytes(UserAccessibleProperties properties) {
		if (properties.hasSalt()) {
			var token = properties.getHash();
			var decoder = Base64.getDecoder();

			var tokenBytes = decoder.decode(token);
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
	public static List<AccountId> parseAccountNumbers(String text) {
		List<AccountId> ids = new ArrayList<>();
		List<String> ranges = new ArrayList<>();
		List<String> singles = new ArrayList<>();

		if (text == null || "".equals(text)) {
			return ids;
		}

		var split = text.replace("\\s", "").split("[\\s,]+");

		for (String s : split) {
			if (s.contains("-")) {
				ranges.add(s);
				continue;
			}
			singles.add(s);
		}

		for (String single : singles) {
			try {
				ids.add(Identifier.parse(single).asAccount());
			} catch (Exception e) {
				logger.error("Cannot parse {} into an account", single);
				return new ArrayList<>();
			}
		}


		for (var s : ranges) {
			var range = s.split("-");
			if (range.length != 2) {
				logger.error("String {} cannot be parsed into a range", s);
				return new ArrayList<>();
			}
			Identifier start;
			try {
				start = Identifier.parse(range[0]);
			} catch (Exception e) {
				logger.error("Cannot parse {} into an account", range[0]);
				return new ArrayList<>();
			}
			Identifier end;
			try {
				end = Identifier.parse(range[1]);
			} catch (Exception e) {
				logger.error("Cannot parse {} into an account", range[1]);
				return new ArrayList<>();
			}

			if (end.getShardNum() != start.getShardNum() || end.getRealmNum() != start.getRealmNum()) {
				logger.error("Cannot parse range: shards and realms must match");
				return new ArrayList<>();
			}

			LongStream.rangeClosed(Math.min(start.getAccountNum(), end.getAccountNum()),
					Math.max(start.getAccountNum(), end.getAccountNum()))
					.mapToObj(i -> new Identifier(start.getShardNum(), end.getRealmNum(), i).asAccount())
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
	public static List<String> difference(JsonObject j1, JsonObject j2) {
		Gson g = new Gson();
		Type mapType = new TypeToken<Map<String, Object>>() {
		}.getType();
		Map<String, Object> first = g.fromJson(j1, mapType);
		Map<String, Object> second = g.fromJson(j2, mapType);
		var diff = Maps.difference(first, second);

		Set<String> keys = new HashSet<>();
		if (!diff.entriesDiffering().isEmpty()) {
			keys.addAll(diff.entriesDiffering().keySet());
		}
		if (!diff.entriesOnlyOnLeft().isEmpty()) {
			keys.addAll(diff.entriesOnlyOnLeft().keySet());
		}
		if (!diff.entriesOnlyOnRight().isEmpty()) {
			keys.addAll(diff.entriesOnlyOnRight().keySet());
		}
		List<String> sorted = new ArrayList<>(keys);
		Collections.sort(sorted);
		return sorted;
	}
}
