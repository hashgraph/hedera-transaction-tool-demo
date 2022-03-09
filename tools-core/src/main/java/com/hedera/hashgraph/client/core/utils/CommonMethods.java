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

import com.codahale.passpol.BreachDatabase;
import com.codahale.passpol.PasswordPolicy;
import com.codahale.passpol.Status;
import com.google.gson.JsonArray;
import com.google.gson.JsonIOException;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.security.AddressChecksums;
import com.hedera.hashgraph.client.core.security.PasswordInput;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.scene.control.Label;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import static com.google.common.base.Splitter.fixedLength;
import static com.hedera.hashgraph.client.core.constants.Constants.FULL_ACCOUNT_CHECKSUM_REGEX;
import static com.hedera.hashgraph.client.core.constants.Constants.FULL_ACCOUNT_REGEX;
import static com.hedera.hashgraph.client.core.constants.Constants.INTEGRATION_NODES_JSON;
import static com.hedera.hashgraph.client.core.constants.Constants.MAX_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MIN_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.NUMBER_REGEX;
import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static org.apache.commons.lang3.StringUtils.valueOf;

public class CommonMethods implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(CommonMethods.class);

	private CommonMethods() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Reads the network and fee payer properties of a json input and sets up a client for the transaction
	 *
	 * @param input
	 * 		a json object tha contains the NETWORK  and TRANSACTION_FEE properties
	 * @return a client object
	 */
	public static Client setupClient(final JsonObject input) throws HederaClientException {
		if (!input.has(NETWORK_FIELD_NAME) || !input.has(TRANSACTION_FEE_FIELD_NAME)) {
			throw new HederaClientException("Missing critical fields in the JSON input to set up the client");
		}
		final var client = getClient(NetworkEnum.valueOf(input.get(NETWORK_FIELD_NAME).getAsString()));
		client.setDefaultMaxQueryPayment(JsonUtils.jsonToHBars(input.getAsJsonObject(TRANSACTION_FEE_FIELD_NAME)));
		return client;
	}

	/**
	 * Given a Network, returns a client for that network
	 *
	 * @param networkEnum
	 * 		one of MAINNET, PREVIEWNET, TESTNET, or INTEGRATION
	 * @return a client with nodes set to the requested network
	 */
	public static Client getClient(final NetworkEnum networkEnum) {
		final Client client;
		switch (networkEnum) {
			case MAINNET:
				client = Client.forMainnet();
				break;
			case PREVIEWNET:
				client = Client.forPreviewnet();
				break;
			case TESTNET:
				client = Client.forTestnet();
				break;
			case INTEGRATION:
				final Map<String, AccountId> network = new HashMap<>();
				final var jsonArray = getIntegrationIPs(INTEGRATION_NODES_JSON);
				for (final var jsonElement : jsonArray) {
					final var node = jsonElement.getAsJsonObject();
					network.put(node.get("IP").getAsString(), new AccountId(node.get("number").getAsInt()));
				}
				client = Client.forNetwork(network);
				break;
			case CUSTOM:
				Map<String, AccountId> net1 = new HashMap<>();
				net1.put("127.0.0.1:50211", new AccountId(0,0,3));
				client = Client.forNetwork(net1);
				break;
			case UNKNOWN:
			default:
				throw new IllegalStateException("Unexpected value: " + networkEnum);
		}
		return client;
	}

	/**
	 * Given a  network string, returns a client for that network
	 *
	 * @param networkString
	 * 		can either be a file path or a network enum
	 * @return a client
	 */
	public static Client getClient(final String networkString) {
		if (!new File(networkString).exists()) {
			return getClient(NetworkEnum.valueOf(networkString.toUpperCase(Locale.ROOT)));
		}
		logger.info("Loading nodes from {}", networkString);
		final Map<String, AccountId> network = new HashMap<>();
		final var jsonArray = getIntegrationIPs(networkString);
		for (final var jsonElement : jsonArray) {
			final var node = jsonElement.getAsJsonObject();
			network.put(node.get("IP").getAsString(), new AccountId(node.get("number").getAsInt()));
		}
		return Client.forNetwork(network);
	}

	/**
	 * Return a mnemonic object, either by loading it from file or generating it. If the mnemonic is generated, it is
	 * then stored in a password-protected file.
	 *
	 * @param file
	 * 		file where the mnemonic is stored, or the output directory where the new file will be stored
	 * @return a mnemonic
	 * @throws HederaClientException
	 * 		if the file cannot be opened
	 */
	public static Mnemonic setupRecoveryPhrase(final String file) throws HederaClientException {

		final var mnemonicFile = new File(file);
		logger.info("Setting up recovery phrase using file {}", mnemonicFile.getAbsolutePath());
		final Mnemonic mnemonic;

		if (!mnemonicFile.exists()) {
			final var message = String.format("Cannot open file %s", file);
			throw new HederaClientException(message);
		}
		logger.info("File {} found", mnemonicFile.getAbsolutePath());

		// Get or generate a new mnemonic
		if (mnemonicFile.isDirectory()) {
			// generate mnemonic (file is a directory)
			mnemonic = Mnemonic.generate24();

			// store the mnemonic
			final var password =
					PasswordInput.readPasswordAndConfirm("Please enter a password for the new recovery phrase: ",
							"Please confirm the password: ");
			if (password != null) {
				SecurityUtilities.toEncryptedFile(SecurityUtilities.keyFromPasswordLegacy(password),
						file + "/recovery.aes",
						mnemonic.toString());
				logger.info("Recovery phrase stored as: {}/recovery.aes", file);

				// Show it to the screen (don't save it to the logs)
				logger.info("A new recovery phrase has been generated. Please keep a copy it in a secure place");
				System.out.println(String.join(", ", mnemonic.words));
			} else {
				throw new HederaClientRuntimeException("Cannot store the recovery phrase with an empty password");
			}
		} else {
			// load mnemonic
			final var mnemonicPassword = PasswordInput.readPasswordFromStdIn("Enter the recovery phrase's password: ");
			if (mnemonicPassword != null) {
				mnemonic =
						SecurityUtilities.fromEncryptedFile(SecurityUtilities.keyFromPasswordLegacy(mnemonicPassword),
								file);
			} else {
				throw new HederaClientRuntimeException("Cannot read the recovery phrase with an empty password");
			}
		}
		return mnemonic;
	}

	/**
	 * Creates a single key from a mnemonic and stores it in a file. The method returns a json object containing a key
	 * object with the key's public key.
	 *
	 * @param mnemonic
	 * 		the recovery phrase required to create the key
	 * @param password
	 * 		the password used to store the key
	 * @param index
	 * 		the index of the key
	 * @param location
	 * 		storage directory for the key
	 * @return a json object with a key list of size one.
	 * @throws HederaClientException
	 * 		if there are problems generating the key or if there are issues with key retrieval.
	 */
	public static JsonObject createSingleKeyAsJson(final Mnemonic mnemonic, final char[] password, final int index,
			final String location) throws HederaClientException {
		logger.info("Creating new key");
		if (mnemonic == null) {
			throw new HederaClientException(ErrorMessages.MNEMONIC_CANNOT_BE_NULL_ERROR_MESSAGE);
		}

		if (password == null || password.length == 0) {
			throw new HederaClientException(ErrorMessages.PASSWORD_CANNOT_BE_EMPTY_ERROR_MESSAGE);
		}

		final var keyName = buildKeyName("KeyStore", location);
		if (SecurityUtilities.generateAndStoreKey(keyName, "Hedera CLI Tool", mnemonic, index,
				password)) {
			logger.info("Key {} has been generated with index {}", keyName, index);
		} else {
			logger.error("Could not generate key. Please check error log for details");
			throw new HederaClientException("Key not generated");
		}
		final var singleKeyJson = new JsonObject();
		final String pubKey;
		try {
			pubKey = new String(
					Files.readAllBytes(Path.of(keyName.replace(PK_EXTENSION, PUB_EXTENSION))));
		} catch (final IOException e) {
			logger.error(e);
			throw new HederaClientException("Could not load public key from file");
		}
		singleKeyJson.addProperty("Ed25519", pubKey);
		return singleKeyJson;
	}

	/**
	 * Builds a unique name for a key in the output directory
	 *
	 * @param keyName
	 * 		the name for the given key
	 * @param outputPath
	 * 		the output directory
	 * @return new key output path
	 */
	public static String buildKeyName(final String keyName, final String outputPath) {
		var number = 0;
		if (new File(outputPath).mkdirs()) {
			logger.info("Output directory {} created", outputPath);
		}
		while (true) {
			final var name = String.format("%s/%s-%d.%s", outputPath, keyName, number, PK_EXTENSION);
			if (!new File(name).exists()) {
				return name;
			}
			number++;
		}
	}


	/**
	 * Check if a json file has all the required fields
	 *
	 * @param input
	 * 		json object input
	 * @param requiredFields
	 * 		list of required fields
	 * @return true if the file has all the required files, false if any fields are missing
	 */
	public static List<String> checkJsonInput(final JsonObject input, final List<String> requiredFields) {
		final List<String> missingFields = new ArrayList<>();

		for (final var requiredField : requiredFields) {
			if (!input.has(requiredField)) {
				missingFields.add(requiredField);
			}
		}
		if (!missingFields.isEmpty()) {
			for (final var s : missingFields) {
				logger.info("Missing required field {}", s);
			}
		}
		return missingFields;
	}

	/**
	 * Checks if the input contains all required fields
	 *
	 * @param input
	 * 		a json object
	 * @param fields
	 * 		a list of fields
	 * @return true if all the fields exist
	 */
	public static boolean verifyFieldExist(final JsonObject input, final String... fields) {
		var count = 0;
		for (final var field : fields) {
			if (!input.has(field)) {
				count++;
				logger.error(ErrorMessages.MISSING_FIELD_ERROR_MESSAGE, field);
			}
		}
		return (count == 0);
	}

	public static boolean verifyOneOfExists(final JsonObject input, final String... fields) {
		var count = 0;
		for (final var field : fields) {
			if (input.has(field)) {
				count++;
				logger.error(ErrorMessages.MISSING_FIELD_ERROR_MESSAGE, field);
			}
		}
		return (count > 0);
	}

	/**
	 * If the account corresponding to the id provided has a nickname, this method returns it. Otherwise, it returns the
	 * account id as a readable String
	 *
	 * @param accountNumber
	 * 		an account ID
	 * @return a String that represents the account ID
	 */
	public static String nicknameOrNumber(final Identifier accountNumber, final JsonObject accounts) {
		final var name = accountNumber.toReadableString();
		final var nickname = (accounts.has(name)) ? accounts.get(name).getAsString() : "";
		final var formattedName = String.format("%s-%s", name, AddressChecksums.checksum(name));
		if (name.equals(nickname) || "".equals(nickname)) {
			return formattedName;
		} else {
			return String.format("%s (%s-%s)", nickname.replace(" ", "\u00A0"), name,
					AddressChecksums.checksum(name));
		}
	}

	/**
	 * Converts a timestamp into a label with both local and utc times
	 *
	 * @param timestamp
	 * 		the timestamp in question
	 * @param full
	 * 		true if date is included
	 * @return a label
	 */
	public static Label getTimeLabel(final Timestamp timestamp, final boolean full) {
		final var utcTimeDate = timestamp.asUTCString().replace("_", " ");
		final var localTimeDate = timestamp.asReadableLocalString();
		final var gmt = localTimeDate.contains(utcTimeDate);
		final var fullTimeString =
				(gmt) ? String.format("%s (UTC)", localTimeDate) : String.format("%s%n(%s UTC)", localTimeDate,
						utcTimeDate);


		final var hoursUTC = timestamp.asCalendarUTC().get(Calendar.HOUR_OF_DAY);
		final var minutesUTC = timestamp.asCalendarUTC().get(Calendar.MINUTE);
		final var secondsUTC = timestamp.asCalendarUTC().get(Calendar.SECOND);

		final var hourTimeString =
				(gmt) ? String.format("%s (UTC)", localTimeDate.substring(localTimeDate.indexOf(" "))) :
						String.format("%s (%02d:%02d:%02d UTC)", localTimeDate.substring(localTimeDate.indexOf(" ")),
								hoursUTC,
								minutesUTC, secondsUTC);

		return new Label((full) ? fullTimeString : hourTimeString);
	}

	/**
	 * Get the longest common substring
	 *
	 * @param firstString
	 * 		a String
	 * @param secondString
	 * 		another String
	 * @return the longest sequence common to both inputs
	 */
	public static String getLCSubStr(final String firstString, final String secondString) {
		final var m = firstString.length();
		final var n = secondString.length();
		final var suffix = new int[m + 1][n + 1];
		var len = 0;
		int row = 0;
		int col = 0;

		for (var i = 0; i <= m; i++) {
			for (var j = 0; j <= n; j++) {
				if (i == 0 || j == 0) {
					suffix[i][j] = 0;
				} else if (firstString.charAt(i - 1) == secondString.charAt(j - 1)) {
					suffix[i][j] = suffix[i - 1][j - 1] + 1;
					if (len < suffix[i][j]) {
						len = suffix[i][j];
						row = i;
						col = j;
					}
				} else {
					suffix[i][j] = 0;
				}
			}
		}

		if (len == 0) {
			logger.info("No Common Substring");
			return "";
		}

		final StringBuilder resultStr = new StringBuilder();
		while (suffix[row][col] != 0) {
			resultStr.insert(0, firstString.charAt(row - 1));
			--len;
			row--;
			col--;
		}

		return resultStr.toString();
	}

	/**
	 * Read the integration file into a json array
	 *
	 * @return a json array that contains the IPs of the integration network
	 */
	private static JsonArray getIntegrationIPs(final String nodes) {
		// Read file into object
		try (final var file = new FileReader(nodes)) {
			return JsonParser.parseReader(file).getAsJsonArray();
		} catch (final JsonIOException | JsonSyntaxException | IOException cause) {
			logger.error(cause);
		}
		return new JsonArray();
	}


	/**
	 * Calculate the longest common prefix of a list of words
	 *
	 * @param strings
	 * 		a list of strings
	 * @return the longest string with which all the input starts (can be empty)
	 */
	public static String longestCommonPrefix(final List<String> strings) {
		if (strings.isEmpty()) {
			return "";
		}
		if (strings.size() == 1) {
			return strings.get(0);
		}

		Collections.sort(strings);
		final var prefix = new StringBuilder();
		for (var i = 0; i < strings.get(0).length(); i++) {
			if (strings.get(0).charAt(i) == strings.get(strings.size() - 1).charAt(i)) {
				prefix.append(strings.get(0).charAt(i));
			} else {
				break;
			}
		}
		return prefix.toString();
	}

	/**
	 * Given a string with multiple words, separate it in multiple lines, each with at most size words
	 *
	 * @param digest
	 * 		the original string
	 * @param size
	 * 		the number of words per line
	 * @return a string
	 */
	public static String splitStringDigest(final String digest, final int size) {
		final var splits = digest.split("[ ]");
		final var builder = new StringBuilder();
		var separator = "";
		var count = 0;
		for (final var split : splits) {
			count++;
			builder.append(separator);
			builder.append(split);

			separator = (count % size == 0) ? "\n" : "\u00A0";
		}
		return builder.toString();
	}

	public static void checkFiles(final String... filePath) throws HederaClientException {
		for (final String path : filePath) {
			if (!new File(path).exists()) {
				throw new HederaClientException(String.format("File %s does not exist",
						path));
			}
		}
	}

	public static boolean badPassword(final char[] password) {
		final var passwordPolicy =
				new PasswordPolicy(BreachDatabase.anyOf(BreachDatabase.top100K(), BreachDatabase.haveIBeenPwned()),
						MIN_PASSWORD_LENGTH,
						MAX_PASSWORD_LENGTH);
		final var check = passwordPolicy.check(valueOf(password));
		if (check.equals(Status.TOO_LONG)) {
			logger.info("The password length exceeds the upper limit of 1024 characters. Please try again");
			return true;
		}
		if (check.equals(Status.TOO_SHORT)) {
			logger.info("The password length is under the lower limit of 10 characters. Please try again");
			return true;
		}
		if (check.equals(Status.BREACHED)) {
			logger.info("The chosen password has been breached. Please try again.");
			return true;
		}

		return false;
	}


	/**
	 * Given a string convert it to hbars
	 *
	 * @param hBarString
	 * 		a String representing a number of hbars
	 * @return hbar amount
	 * @throws HederaClientException
	 * 		if the string cannot be parsed
	 */
	public static Hbar fromString(final String hBarString) throws HederaClientException {
		final var trimmed = (hBarString.contains(" ")) ? hBarString.split(" ")[0] : hBarString;
		if (hBarString.contains("t")) {
			return Hbar.fromTinybars(Long.parseLong(trimmed));
		}
		final var split = trimmed.split("\\.");
		if (split.length == 1) {
			return Hbar.fromTinybars(Long.parseLong(split[0]) * 100000000);
		}
		if (split.length == 2) {
			final StringBuilder tiny = new StringBuilder(split[1]);
			while (tiny.length() < 8) {
				tiny.append("0");
			}
			return Hbar.fromTinybars(Long.parseLong(split[0]) * 100000000 + Long.parseLong(tiny.toString()));
		}
		throw new HederaClientException(String.format("Cannot parse String \"%s\" to hbars", hBarString));
	}

	@NotNull
	public static Client getClient(final JsonArray customNetwork) {
		final Map<String, AccountId> networkMap = new HashMap<>();
		for (final var jsonElement : customNetwork) {
			final var node = jsonElement.getAsJsonObject();
			final var accountID = Identifier.parse(node.get("accountID").getAsString()).asAccount();
			final var ip = node.get("ipAddress").getAsString() + ":" + node.get("port").getAsInt();
			networkMap.put(ip, accountID);
		}
		return Client.forNetwork(networkMap);
	}

	/**
	 * Given a string divide into groups of four strings. Primarily used to increase readability of hashes and digests
	 *
	 * @param digest
	 * 		a string.
	 * @return a partitioned string
	 */
	@NotNull
	public static String splitString(final String digest) {
		var count = 0;
		final var splitDigest = new StringBuilder();
		for (final var token : fixedLength(4).split(digest)) {
			if (count != 0) {
				splitDigest.append(" ");
			}
			splitDigest.append(token);
			count++;
		}
		return splitDigest.toString();
	}

	/**
	 * Given a string that may or may not represent an account id with nickname and checksum, return just the account id
	 * and the checksum
	 *
	 * @param value
	 * 		any String
	 * @return an account and checksum string if the pattern is found. An empty string otherwise
	 */
	public static String removeNickname(final String value) {
		final var patternFull = Pattern.compile(FULL_ACCOUNT_CHECKSUM_REGEX);
		final var matcherFull = patternFull.matcher(value);
		if (matcherFull.find()) {
			return matcherFull.group(0);
		}
		final var patternAccount = Pattern.compile(FULL_ACCOUNT_REGEX);
		final var matcherAccount = patternAccount.matcher(value);
		if (matcherAccount.find()) {
			final var identifier = Identifier.parse(matcherAccount.group(0));
			return identifier.toReadableStringAndChecksum();
		}
		final var patternDecimal = Pattern.compile(NUMBER_REGEX);
		final var matcherDecimal = patternDecimal.matcher(value);
		if (matcherDecimal.find()) {
			final var identifier = Identifier.parse(matcherDecimal.group(0));
			return identifier.toReadableStringAndChecksum();
		}
		return "";
	}
}

