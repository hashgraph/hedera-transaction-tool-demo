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

import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.KeyDesignErrorCodes;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.sdk.Key;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static net.i2p.crypto.eddsa.spec.EdDSANamedCurveTable.ED_25519;

public class KeyStructureUtility implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(KeyStructureUtility.class);
	private static final String KEYS_STRING = File.separator + "Keys" + File.separator;
	private static final String THRESHOLD_KEY = "thresholdKey";
	private static final String THRESHOLD = "threshold";
	private static final String KEYS = "keys";
	private static final String KEY_LIST = "keyList";
	private final Controller controller;
	// contains all .pub files' content and file name
	private Map<String, Path> pubFiles = new HashMap<>();
	// is loaded when the key structure is shown
	private final Map<TreeItem<String>, JsonObject> keyJsonItems = new HashMap<>();

	public KeyStructureUtility(final Controller controller) {
		this.controller = controller;
		loadPubKeys();
	}

	public KeyStructureUtility(final Map<String, Path> pubFiles) {
		this.pubFiles = pubFiles;
		this.controller = null;
	}

	/**
	 * search for .pub or .txt files in preferredStorageDirectory recursively, and load their Hex String and Path into
	 * pubFiles map
	 */
	public void loadPubKeys() {
		pubFiles = new HashMap<>();
		final var keysFolder = new File(controller.getPreferredStorageDirectory() + KEYS_STRING);
		if (!keysFolder.exists()) {
			return;
		}
		try {
			final var publicKeys = keysFolder.listFiles((dir, name) -> isPubFile(name));
			for (final var publicKey : Objects.requireNonNull(publicKeys)) {
				final var path = publicKey.toPath();
				pubFiles.put(new String(Files.readAllBytes(path)), path);
			}
		} catch (final Exception ex) {
			logger.error(ex);
		}
		logger.debug("Public keys loaded");
	}

	private boolean isPubFile(final String path) {
		return path.endsWith(PUB_EXTENSION);
	}

	private KeyDesignErrorCodes checkJsonKey(final JsonObject jsonObject) {
		if (jsonObject.has("ed25519")) {
			final var key = jsonObject.get("ed25519").getAsString();
			if ("".equals(key)) {
				return KeyDesignErrorCodes.BAD_KEY;
			}
		} else if (jsonObject.has(THRESHOLD_KEY)) {
			final var thresholdKey = jsonObject.getAsJsonObject(THRESHOLD_KEY);
			if (!thresholdKey.has(THRESHOLD) || !thresholdKey.has(KEYS)) {
				return KeyDesignErrorCodes.INCOMPLETE_THRESHOLD_KEY;
			}
			final var threshold = thresholdKey.get(THRESHOLD).getAsInt();
			final var list = thresholdKey.getAsJsonObject(KEYS).getAsJsonArray(KEYS);
			if (threshold <= 0 || threshold > list.size()) {
				return KeyDesignErrorCodes.BAD_THRESHOLD;
			}
			return checkKeyList(list);
		} else if (jsonObject.has(KEY_LIST)) {
			final var keyList = jsonObject.getAsJsonObject(KEY_LIST);
			final var list = keyList.getAsJsonArray(KEYS);
			return checkKeyList(list);
		}
		return KeyDesignErrorCodes.OK;
	}

	private KeyDesignErrorCodes checkKeyList(final JsonArray list) {
		for (final var element : list) {
			final var keyDesignErrorCode = checkJsonKey(element.getAsJsonObject());
			if (!keyDesignErrorCode.equals(KeyDesignErrorCodes.OK)) {
				return keyDesignErrorCode;
			}
		}
		return KeyDesignErrorCodes.OK;
	}

	private TreeItem<String> showKey(final JsonObject keyJson) {
		var node = new TreeItem<String>();
		if (keyJson == null) {
			return node;
		}
		if (hasThresholdKey(keyJson)) {
			node = showThresholdKey(keyJson.getAsJsonObject(THRESHOLD_KEY));
		} else if (hasKeyList(keyJson)) {
			node = showKeyList(keyJson.get(KEY_LIST).getAsJsonArray());
		} else {
			node = showSimpleKey(keyJson);
		}
		keyJsonItems.put(node, keyJson);
		return node;
	}

	private TreeItem<String> showSimpleKey(final JsonObject keyJson) {
		return (keyJson.has("Ed25519")) ?
				showHexString(keyJson.get("Ed25519").getAsString()) :
				new TreeItem<>("Load Error");

	}

	/**
	 * Build the key TreeView based on the json supplied. If the JsonElement is a JsonObject it is a Key object.
	 * If the JsonElement is a JsonPrimitive, it is just a string representation of the key.
	 *
	 * @param keyJson The key
	 * @return A TreeView representation of the key
	 */
	public TreeView<String> buildKeyTreeView(final JsonElement keyJson) {
		final var keyTreeView = new TreeView<String>();
		final var root = new TreeItem<String>();
		final var rootJson = new JsonObject();
		keyJsonItems.put(root, rootJson);
		if (keyJson instanceof JsonObject) {
			final var keyItem = showKey(((JsonObject)keyJson));
			root.getChildren().add(keyItem);
		} else if (keyJson instanceof JsonPrimitive) {
			root.getChildren().add(new TreeItem<>(((JsonPrimitive)keyJson).getAsString()));
		}
		keyTreeView.setRoot(root);
		keyTreeView.setShowRoot(false);
		return keyTreeView;
	}

	public TreeView<String> buildKeyTreeView(final Key key) {
		return buildKeyTreeView(EncryptionUtils.keyToJson(key));
	}

	private TreeItem<String> showHexString(final String hexString) {
		if (hexString.length() < 15) {
			return new TreeItem<>("Empty");
		}

		if (controller != null && pubFiles.containsKey(hexString)) {
			// If local machine has this pubKey file, show its file name
			return new TreeItem<>(pubFiles.get(hexString).getFileName().toString());
		} else {
			return new TreeItem<>(hexString);
		}
	}

	public String showKeyString(final ByteString key) {
		final var hexString = Hex.toHexString((key.toByteArray()));
		if (hexString.length() < 15) {
			return "Empty";
		}

		if (controller != null && controller.getPubFiles().containsKey(hexString)) {
			// If local machine has this pubKey file, show its file name
			return controller.getPubFiles().get(hexString).getFileName().toString();
		} else {
			return hexString;
		}
	}

	public Map<String, Path> getPubFiles() {
		return pubFiles;
	}

	private TreeItem<String> showThresholdKey(final JsonObject thresholdKeyJson) {
		final var keyListJson = thresholdKeyJson.get(KEY_LIST).getAsJsonArray();
		final var node = new TreeItem<>(getThresholdKeyDescription(thresholdKeyJson));
		node.getChildren().addAll(getKeyListElements(keyListJson));
		node.setExpanded(true);
		return node;
	}

	private String getThresholdKeyDescription(final JsonObject thresholdKeyJson) {
		if (thresholdKeyJson.has(THRESHOLD)) {
			return "ThresholdKey (" + thresholdKeyJson.get(THRESHOLD).getAsString()
					+ "/" + thresholdKeyJson.get(KEY_LIST).getAsJsonArray().size() + ")";
		}
		throw new HederaClientRuntimeException("The threshold cannot be found. Likely it was set to zero");
	}

	private TreeItem<String> showKeyList(final JsonArray keyListJson) {
		final var node = new TreeItem<>(getKeyListDescription(keyListJson));
		node.setExpanded(true);
		node.getChildren().addAll(getKeyListElements(keyListJson));
		node.setExpanded(true);
		return node;
	}

	private String getKeyListDescription(final JsonArray keyListJson) {
		return "KeyList (" + keyListJson.size() + ")";
	}

	private TreeItem<String>[] getKeyListElements(final JsonArray keyListJson) {
		final var results = new TreeItem[keyListJson.size()];
		var i = 0;
		for (final var ele : keyListJson) {
			results[i++] = showKey(ele.getAsJsonObject());
		}
		return results;
	}

	private boolean hasThresholdKey(final JsonObject jsonObject) {
		return jsonObject != null && jsonObject.has(THRESHOLD_KEY);
	}

	private boolean hasKeyList(final JsonObject jsonObject) {
		return jsonObject != null && jsonObject.has(KEY_LIST);
	}

	public String jsonKeyToPrettyString(final JsonObject keyJson) {
		final var cleanKey = replaceAvailableHexfromKey(keyJson);
		final var gson = new GsonBuilder().setPrettyPrinting().create();
		final var jsonString = gson.toJson(cleanKey);
		return jsonString.replace("\"", "")
				.replace(ED_25519 + ": ", "")
				.replace(THRESHOLD_KEY + ": ", "")
				.replace(KEY_LIST + ": ", "");
	}

	public JsonObject replaceAvailableHexfromKey(final JsonObject keyJson) {
		var cleanKey = new JsonObject();
		if (keyJson.has(ED_25519)) {
			cleanKey = handlePublicKey(keyJson);
		}
		if (keyJson.has(KEY_LIST)) {
			cleanKey = handleKeyList(keyJson.getAsJsonArray(KEY_LIST));
		}
		if (keyJson.has(THRESHOLD_KEY)) {
			cleanKey = handleThresholdKey(keyJson.getAsJsonObject(THRESHOLD_KEY));
		}

		return cleanKey;
	}

	private JsonObject handleThresholdKey(final JsonObject thresholdJson) {
		final JsonObject cleanObject = new JsonObject();
		cleanObject.addProperty(THRESHOLD, thresholdJson.get(THRESHOLD).getAsInt());
		final JsonObject cleanArray = handleKeyList(thresholdJson.get(KEY_LIST).getAsJsonArray());
		cleanObject.add(KEY_LIST, cleanArray);
		return cleanObject;
	}

	private JsonObject handleKeyList(final JsonArray jsonArray) {
		final JsonObject object = new JsonObject();
		final JsonArray cleanArray = new JsonArray();
		for (final JsonElement element : jsonArray) {
			cleanArray.add(replaceAvailableHexfromKey(element.getAsJsonObject()));
		}
		object.add(KEY_LIST, cleanArray);
		return object;
	}

	private JsonObject handlePublicKey(final JsonObject keyJson) {
		final JsonObject cleanKey = new JsonObject();
		final var ed = keyJson.get(ED_25519).getAsString();
		final var value = (pubFiles.containsKey(ed)) ? FilenameUtils.getBaseName(pubFiles.get(ed).toFile().getName()) : ed;
		cleanKey.addProperty(ED_25519, value);
		return cleanKey;
	}


}
