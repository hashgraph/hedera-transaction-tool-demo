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

package com.hedera.hashgraph.client.ui.utilities;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.KeyDesignErrorCodes;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.sdk.Key;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;

public class KeyStructureUtility implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(KeyStructureUtility.class);
	private static final String KEYS_STRING = "/Keys/";
	private final Controller controller;
	// contains all .pub files' content and file name
	private Map<String, Path> pubFiles = new HashMap<>();
	// is loaded when the key structure is shown
	private final Map<TreeItem<String>, JsonObject> keyJsonItems = new HashMap<>();

	public KeyStructureUtility(Controller controller) {
		this.controller = controller;
		loadPubKeys();
	}

	// region load pubKeys

	/**
	 * search for .pub or .txt files in preferredStorageDirectory recursively, and load their Hex String and Path into
	 * pubFiles map
	 */
	public void loadPubKeys() {
		pubFiles = new HashMap<>();
		var keysFolder = new File(controller.getPreferredStorageDirectory() + KEYS_STRING);
		if (!keysFolder.exists()) {
			return;
		}
		try {
			var publicKeys = keysFolder.listFiles((dir, name) -> isPubFile(name));
			for (var publicKey : Objects.requireNonNull(publicKeys)) {
				var path = publicKey.toPath();
				pubFiles.put(new String(Files.readAllBytes(path)), path);
			}
		} catch (Exception ex) {
			logger.error(ex);
		}
		logger.debug("Public keys loaded");
	}

	private boolean isPubFile(String path) {
		return path.endsWith(PUB_EXTENSION);
	}

	// endregion


	private KeyDesignErrorCodes checkJsonKey(JsonObject jsonObject) {
		if (jsonObject.has("ed25519")) {
			var key = jsonObject.get("ed25519").getAsString();
			if ("".equals(key)) {
				return KeyDesignErrorCodes.BAD_KEY;
			}
		} else if (jsonObject.has("thresholdKey")) {
			var thresholdKey = jsonObject.getAsJsonObject("thresholdKey");
			if (!thresholdKey.has("threshold") || !thresholdKey.has("keys")) {
				return KeyDesignErrorCodes.INCOMPLETE_THRESHOLD_KEY;
			}
			var threshold = thresholdKey.get("threshold").getAsInt();
			var list = thresholdKey.getAsJsonObject("keys").getAsJsonArray("keys");
			if (threshold <= 0 || threshold > list.size()) {
				return KeyDesignErrorCodes.BAD_THRESHOLD;
			}
			return checkKeyList(list);
		} else if (jsonObject.has("keyList")) {
			var keyList = jsonObject.getAsJsonObject("keyList");
			var list = keyList.getAsJsonArray("keys");
			return checkKeyList(list);
		}
		return KeyDesignErrorCodes.OK;
	}

	private KeyDesignErrorCodes checkKeyList(JsonArray list) {
		for (var element : list) {
			final var keyDesignErrorCode = checkJsonKey(element.getAsJsonObject());
			if (!keyDesignErrorCode.equals(KeyDesignErrorCodes.OK)) {
				return keyDesignErrorCode;
			}
		}
		return KeyDesignErrorCodes.OK;
	}

	private TreeItem<String> showKey(JsonObject keyJson) {
		TreeItem<String> node;
		if (hasThresholdKey(keyJson)) {
			node = showThresholdKey(keyJson.getAsJsonObject("thresholdKey"));
		} else if (hasKeyList(keyJson)) {
			node = showKeyList(keyJson.get("keyList").getAsJsonArray());
		} else {
			node = showSimpleKey(keyJson);
		}
		keyJsonItems.put(node, keyJson);
		return node;
	}

	private TreeItem<String> showSimpleKey(JsonObject keyJson) {

		return (keyJson.has("Ed25519")) ?
				showHexString(keyJson.get("Ed25519").getAsString()) :
				new TreeItem<>("Load Error");

	}

	public TreeView<String> buildKeyTreeView(JsonObject keyJson) {
		var keyTreeView = new TreeView<String>();
		var root = new TreeItem<String>();
		var rootJson = new JsonObject();
		keyJsonItems.put(root, rootJson);
		var keyItem = showKey(keyJson);
		root.getChildren().add(keyItem);
		keyTreeView.setRoot(root);
		keyTreeView.setShowRoot(false);
//		keyTreeView.setCellFactory(tc -> {
//			TreeCell<String> cell = new TreeCell<>();
//			Text text = new Text();
//			cell.setGraphic(text);
//			cell.setPrefHeight(Control.USE_COMPUTED_SIZE);
//			text.textProperty().bind(cell.itemProperty());
//			return cell;
//		});


		return keyTreeView;
	}

	public TreeView<String> buildKeyTreeView(Key key) throws IOException {
		return buildKeyTreeView(EncryptionUtils.keyToJson(key));
	}

	private TreeItem<String> showHexString(String hexString) {
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

	public String showKeyString(ByteString key) {
		var hexString = Hex.toHexString((key.toByteArray()));
		if (hexString.length() < 15) {
			return "Empty";
		}

		if (controller != null && controller.keyStructureUtility.pubFiles.containsKey(hexString)) {
			// If local machine has this pubKey file, show its file name
			return controller.keyStructureUtility.pubFiles.get(hexString).getFileName().toString();
		} else {
			return hexString;
		}
	}

	private TreeItem<String> showThresholdKey(JsonObject thresholdKeyJson) {
		var keyListJson = thresholdKeyJson.get("keyList").getAsJsonArray();
		var node = new TreeItem<>(getThresholdKeyDescription(thresholdKeyJson));
		node.getChildren().addAll(getKeyListElements(keyListJson));
		node.setExpanded(true);
		return node;
	}

	private String getThresholdKeyDescription(JsonObject thresholdKeyJson) {
		if (thresholdKeyJson.has("threshold")) {
			return "ThresholdKey (" + thresholdKeyJson.get("threshold").getAsString()
					+ "/" + thresholdKeyJson.get("keyList").getAsJsonArray().size() + ")";
		}
		throw new HederaClientRuntimeException("The threshold cannot be found. Likely it was set to zero");
	}

	private TreeItem<String> showKeyList(JsonArray keyListJson) {
		var node = new TreeItem<>(getKeyListDescription(keyListJson));
		node.setExpanded(true);
		node.getChildren().addAll(getKeyListElements(keyListJson));
		node.setExpanded(true);
		return node;
	}

	private String getKeyListDescription(JsonArray keyListJson) {
		return "KeyList (" + keyListJson.size() + ")";
	}

	private TreeItem<String>[] getKeyListElements(JsonArray keyListJson) {
		var results = new TreeItem[keyListJson.size()];
		var i = 0;
		for (var ele : keyListJson) {
			results[i++] = showKey(ele.getAsJsonObject());
		}
		return results;
	}

	// endregion

	private boolean hasThresholdKey(JsonObject jsonObject) {
		return jsonObject != null && jsonObject.has("thresholdKey");
	}

	private boolean hasKeyList(JsonObject jsonObject) {
		return jsonObject != null && jsonObject.has("keyList");
	}

}
