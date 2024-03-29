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

import com.google.common.hash.Hashing;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.constants.Messages;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.Ed25519PrivateKey;
import com.hedera.hashgraph.client.core.security.PasswordInput;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import net.i2p.crypto.eddsa.EdDSAPrivateKey;
import net.i2p.crypto.eddsa.EdDSAPublicKey;
import net.i2p.crypto.eddsa.spec.EdDSANamedCurveTable;
import net.i2p.crypto.eddsa.spec.EdDSAParameterSpec;
import net.i2p.crypto.eddsa.spec.EdDSAPrivateKeySpec;
import net.i2p.crypto.eddsa.spec.EdDSAPublicKeySpec;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyPair;
import java.security.KeyStoreException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static java.lang.System.arraycopy;

public class EncryptionUtils {

	private EncryptionUtils() {
		throw new IllegalStateException("Utility class");
	}

	private static final Logger logger = LogManager.getLogger(EncryptionUtils.class);
	private static final String ED_25519 = "Ed25519";
	private static final String KEY_LIST = "keyList";
	private static final String THRESHOLD_KEY = "thresholdKey";
	private static final String THRESHOLD = "threshold";

	/**
	 * Build a KeyPair from Ed25519PrivateKey
	 */
	public static KeyPair buildKeyPairFromMainNetPrivateKey(final Ed25519PrivateKey ed25519PrivateKey) {
		return buildKeyPairFromMainNetPriKeyEncHex(ed25519PrivateKey.toString());
	}

	/**
	 * Build a KeyPair from a priKeyEncHex String generated by hedera key-gen tool
	 */
	private static KeyPair buildKeyPairFromMainNetPriKeyEncHex(final String priKeyEncHex) {
		final var privateKeyBytes = Hex.decode(priKeyEncHex);
		EdDSAPrivateKey privateKey;
		final EdDSAPublicKey publicKey;
		try {
			// try encoded first
			final var encodedPrivateKey = new PKCS8EncodedKeySpec(privateKeyBytes);
			privateKey = new EdDSAPrivateKey(encodedPrivateKey);
		} catch (final InvalidKeySpecException e) {
			// key is invalid (likely not encoded)
			// try non encoded
			final var edDSAPrivateKeySpec =
					new EdDSAPrivateKeySpec(privateKeyBytes, EdDSANamedCurveTable.ED_25519_CURVE_SPEC);
			privateKey = new EdDSAPrivateKey(edDSAPrivateKeySpec);
		}

		final EdDSAParameterSpec spec = EdDSANamedCurveTable.getByName(EdDSANamedCurveTable.ED_25519);

		publicKey = new EdDSAPublicKey(
				new EdDSAPublicKeySpec(privateKey.getAbyte(), spec));
		return new KeyPair(publicKey, privateKey);
	}


	/**
	 * Converts a com.hedera.hashgraph.sdk.proto.Key object to a json object
	 *
	 * @param keyObject
	 * 		a key
	 * @return a json object representing the key
	 */
	public static JsonObject keyToJson(final Key keyObject) {
		final var jsonObject = new JsonObject();

		if (keyObject == null) {
			return jsonObject;
		}

		if (keyObject instanceof PublicKey) {
			jsonObject.addProperty(ED_25519, trimTo64(keyObject));
			return jsonObject;
		}

		if (((KeyList) keyObject).getThreshold() == null) {
			final var keyAsArray = keyListToJsonArray((KeyList) keyObject);
			if (keyAsArray.size() == 1) {
				return keyAsArray.get(0).getAsJsonObject();
			} else {
				jsonObject.add(KEY_LIST, keyAsArray);
			}
		} else {
			jsonObject.add(THRESHOLD_KEY, keyListToThreshold((KeyList) keyObject));
		}


		return jsonObject;
	}


	/**
	 * Converts a com.hedera.hashgraph.sdk.KeyList object to a json object
	 *
	 * @param keyList
	 * 		a key list key
	 * @return a json object representing the threshold key
	 */
	private static JsonArray keyListToJsonArray(final KeyList keyList) {

		final var jsonArray = new JsonArray();
		for (final var key : keyList) {
			jsonArray.add(keyToJson(key));
		}
		return jsonArray;
	}

	/**
	 * Converts a com.hedera.hashgraph.sdk.KeyList object to a json object
	 *
	 * @param key
	 * 		a threshold key
	 * @return a json object representing the threshold key
	 */
	private static JsonObject keyListToThreshold(final KeyList key) {
		final var jsonObject = new JsonObject();
		jsonObject.addProperty(THRESHOLD, key.getThreshold());
		jsonObject.add(KEY_LIST, keyListToJsonArray(key));
		return jsonObject;
	}

	/**
	 * Converts a json object to a com.hedera.hashgraph.sdk.Key object
	 *
	 * @param jsonObject
	 * 		a json object representing a key
	 * @return the key represented by the json object
	 */
	public static Key jsonToKey(final JsonObject jsonObject) {
		if (jsonObject.has(ED_25519)) {
			final var ed = jsonObject.get(ED_25519).getAsString();
			return PublicKey.fromString(ed);
		} else if (jsonObject.has("key")) {
			final var pubKeyFile = jsonObject.get("key").getAsString();
			return publicKeyFromFile(pubKeyFile);
		} else if (jsonObject.has(KEY_LIST)) {
			if (jsonObject.get(KEY_LIST) instanceof JsonArray) {
				return jsonKeyListToSDKKeyList(jsonObject.getAsJsonArray(KEY_LIST));
			}
		} else if (jsonObject.has(THRESHOLD_KEY)) {
			return jsonThresholdKeyToSDKKeyList(jsonObject.get(THRESHOLD_KEY).getAsJsonObject());
		}
		// If nothing worked, return an empty key
		return new KeyList();
	}

	/**
	 * Load a public key string from a file
	 *
	 * @param file
	 * 		the file that contains the public key
	 * @return a PublicKey
	 */
	public static PublicKey publicKeyFromFile(final String file) {
		final PublicKey publicKey;
		if (!new File(file).exists()) {
			throw new HederaClientRuntimeException(String.format("Public key file %s cannot be found", file));
		}
		try {
			publicKey = PublicKey.fromString(new String(Files.readAllBytes(Path.of(file))));
		} catch (final Exception e) {
			logger.error(e);
			throw new HederaClientRuntimeException(String.format("Cannot load public key %s", file));
		}
		return publicKey;
	}

	/**
	 * Converts a json object to a com.hedera.hashgraph.sdk.KeyList object
	 *
	 * @param keyListJson
	 * 		a json object representing a key list
	 * @return the key list represented by the json object
	 */
	private static KeyList jsonKeyListToSDKKeyList(final JsonArray keyListJson) {
		final var keyList = new KeyList();
		for (final var element : keyListJson) {
			final Key key = jsonToKey(element.getAsJsonObject());
			keyList.add(key);
		}
		return keyList;
	}

	/**
	 * Converts a json object to a com.hedera.hashgraph.sdk.proto.KeyList object
	 *
	 * @param thresholdKeyJson
	 * 		a json object representing a threshold key
	 * @return the threshold key represented by the json object
	 */
	private static Key jsonThresholdKeyToSDKKeyList(final JsonObject thresholdKeyJson) {
		final KeyList thresholdKey;
		if (thresholdKeyJson.has(KEY_LIST)) {
			thresholdKey = jsonKeyListToSDKKeyList(thresholdKeyJson.getAsJsonArray(KEY_LIST));
		} else {
			throw new HederaClientRuntimeException("Missing keyList in threshold key");
		}

		if (thresholdKeyJson.has(THRESHOLD)) {
			final var threshold = thresholdKeyJson.get(THRESHOLD).getAsInt();
			if (threshold > thresholdKey.size()) {
				throw new HederaClientRuntimeException("Threshold cannot be larger than the number of keys");
			}
			thresholdKey.setThreshold(threshold);
		} else {
			throw new HederaClientRuntimeException("Missing threshold in threshold key");
		}
		return thresholdKey;
	}

	/**
	 * Trims a sdk public key to the 64 bytes in a regular public key
	 *
	 * @param key
	 * 		a Key object
	 * @return a trimmed string
	 */
	public static String trimTo64(final Key key) {
		final var bytes = key.toString().getBytes();
		final var trimmed = new byte[64];
		arraycopy(bytes, bytes.length - 64, trimmed, 0, 64);
		return new String(trimmed);
	}

	/**
	 * Request password from Std input and return a private key
	 *
	 * @param privateKeyFileName
	 * 		pem location
	 * @return a private key
	 */
	public static PrivateKey getPrivateKeyFromPEM(final String privateKeyFileName) throws KeyStoreException {
		final var password = PasswordInput.readPasswordFromStdIn(
				String.format(Messages.REQUEST_PASSWORD_MESSAGE, FilenameUtils.getBaseName(privateKeyFileName)));
		final var keyStore = Ed25519KeyStore.read(password, privateKeyFileName);
		return PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());
	}


	/**
	 * Calculates the SHA-384 digest of a file
	 *
	 * @param path
	 * 		path to the file
	 * @return SHA-384 digest of the file
	 */
	public static String getChecksum(final String path) {
		try {
			return com.google.common.io.Files.asByteSource(new File(path)).hash(Hashing.sha384()).toString();
		} catch (final IOException e) {
			logger.error(e);
			return ErrorMessages.COULD_NOT_CALCULATE_HASH_OF_THE_FILE;
		}
	}

	public static Set<String> flatPubKeysString(final List<Key> keyList) {
		final var byteStrings = flatPubKeys(keyList);
		return byteStrings.stream().map(byteString -> Hex.toHexString(byteString.toByteArray())).collect(
				Collectors.toCollection(HashSet::new));
	}

	/**
	 * Given a list of keys, creates a set of all the different public keys in the list (dedup)
	 *
	 * @param keysList
	 * 		A list of Keys
	 * @return a set of byte strings that represent the different public keys in the list
	 */
	public static Set<ByteString> flatPubKeys(final List<Key> keysList) {
		final Set<ByteString> result = new HashSet<>();
		for (final var key : keysList) {
			if (key instanceof PublicKey) {
				result.add(ByteString.copyFrom(key.toBytes()));
				continue;
			}
			try {
				result.addAll(flatPubKey((KeyList) key));
			} catch (final Exception e) {
				logger.error(e);
			}
		}
		return result;
	}

	private static Set<ByteString> flatPubKey(final KeyList keyList) {
		final Set<ByteString> result = new HashSet<>();
		for (final var key : keyList) {
			if (key instanceof PublicKey) {
				result.add(ByteString.copyFrom(key.toBytes()));
			} else {
				result.addAll(flatPubKey((KeyList) key));
			}
		}

		return result;
	}

	public static void storePubKey(final String filename, final EdDSAPublicKey key) throws IOException {
		try (final var fos = new FileOutputStream(filename)) {
			fos.write(Hex.encode(key.getAbyte()));
			SecurityUtilities.ownerReadWritePermissions(filename);
		}
	}

	public static JsonObject info2Json(final AccountInfo info) {
		final var jsonObject = new JsonObject();
		jsonObject.add("accountID", new Identifier(info.accountId).asJSON());
		jsonObject.addProperty("contractAccountID", info.contractAccountId);
		if (info.proxyAccountId != null) {
			jsonObject.add("proxyAccountID", new Identifier(info.proxyAccountId).asJSON());
		}
		jsonObject.add("key", keyToJson(info.key));
		jsonObject.addProperty("balance", info.balance.toTinybars());

		jsonObject.addProperty("generateSendRecordThreshold", info.sendRecordThreshold.toTinybars());
		jsonObject.addProperty("generateReceiveRecordThreshold", info.receiveRecordThreshold.toTinybars());
		jsonObject.add("expirationTime", new Timestamp(info.expirationTime).asJSON());
		jsonObject.add("autoRenewPeriod", new Timestamp(info.autoRenewPeriod).asJSON());

		return jsonObject;
	}


	/**
	 * Given a file, returns a formatted string that represents the SHA file checksum
	 *
	 * @param file
	 * 		the file
	 * @return a hex string that has been split in groups of 4 characters for ease of comparison
	 */
	public static String getFileDigest(final File file) {
		final var digest = getChecksum(file.getAbsolutePath());
		if (digest.equals(ErrorMessages.COULD_NOT_CALCULATE_HASH_OF_THE_FILE)) {
			return "";
		}
		return CommonMethods.splitString(digest);
	}

}
