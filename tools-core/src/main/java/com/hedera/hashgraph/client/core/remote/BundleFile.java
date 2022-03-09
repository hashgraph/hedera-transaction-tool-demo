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

package com.hedera.hashgraph.client.core.remote;

import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.PublicKey;
import javafx.scene.Node;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Messages.BUNDLE_TITLE_MESSAGE_FORMAT;

public class BundleFile extends RemoteFile implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(BundleFile.class);

	private final Map<InfoKey, File> accountInfoMap = new TreeMap<>();
	private final Map<PubInfoKey, File> publicKeyMap = new TreeMap<>();
	private final Map<AccountId, String> existingInfosMap = new HashMap<>();
	private final Map<PublicKey, String> existingPubKeysMap = new HashMap<>();
	private final List<FileActions> actions = Arrays.asList(FileActions.ACCEPT, FileActions.DECLINE);
	private final CheckBox accountCheckBox = new CheckBox("Replace existing account nicknames");
	private final CheckBox keyCheckBox = new CheckBox("Replace existing public key nicknames");

	public BundleFile(final FileDetails file) {
		super(file);
		if (file == null) {
			return;
		}

		loadExistingInfos();
		loadExistingPublicKeys();
		accountInfoMap.clear();
		publicKeyMap.clear();

		try {
			final var tempFile = Files.createTempDirectory("tmpDirPrefix").toFile();
			unZip(file.getFullPath(), tempFile.getAbsolutePath());
			final var files = tempFile.listFiles();
			loadMaps(files);
			setValid(!(publicKeyMap.isEmpty() && accountInfoMap.isEmpty()));
		} catch (final IOException | HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * Loads files fom the map
	 *
	 * @param files
	 * 		an array of files
	 * @throws HederaClientException
	 * 		if the info file cannot be read
	 * @throws HederaClientException
	 * 		if the info file cannot be parsed
	 */
	private void loadMaps(final File[] files) throws HederaClientException, InvalidProtocolBufferException {
		for (final var file : files) {
			if (file.getName().toLowerCase(Locale.ROOT).contains("macosx") ||
					file.getName().toLowerCase(Locale.ROOT).contains("ds_store")) {
				continue;
			}
			if (file.isDirectory()) {
				loadMaps(file.listFiles());
				continue;
			}
			final var name = file.getName();
			switch (FilenameUtils.getExtension(name)) {
				case INFO_EXTENSION:
					final var infoKey = new InfoKey(file);
					accountInfoMap.put(infoKey, file);
					break;
				case PUB_EXTENSION:
					final var pubInfoKey = new PubInfoKey(file);
					publicKeyMap.put(pubInfoKey, file);
					break;
				default:
					logger.error("Unexpected value: {}", name);
			}
		}
	}

	private void loadExistingInfos() {
		try {
			final var nicknames =
					new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			final var entries = nicknames.entrySet();
			for (final var entry : entries) {
				existingInfosMap.put(Identifier.parse(entry.getKey()).asAccount(), entry.getValue().getAsString());
			}
		} catch (final HederaClientException e) {
			logger.error("Exception {} on line {}", e.getMessage(), e.getStackTrace()[0].getLineNumber());
		}
	}

	private void loadExistingPublicKeys() {
		final var existingPublicKeys = new File(KEYS_FOLDER).listFiles(
				(dir, name) -> PUB_EXTENSION.equals(FilenameUtils.getExtension(name)));
		if (existingPublicKeys == null) {
			return;
		}
		for (final var existingPublicKey : existingPublicKeys) {
			existingPubKeysMap.put(EncryptionUtils.publicKeyFromFile(existingPublicKey.getAbsolutePath()),
					FilenameUtils.getBaseName(existingPublicKey.getName()));
		}
	}

	public Map<InfoKey, File> getAccountInfoMap() {
		return accountInfoMap;
	}

	public Map<PubInfoKey, File> getPublicKeyMap() {
		return publicKeyMap;
	}

	public boolean replaceAccountNicknames() {
		return accountCheckBox.isSelected();
	}

	@Override
	public List<FileActions> getActions() {
		return actions;
	}

	@Override
	public GridPane buildGridPane() {
		final var details = new GridPane();

		final List<Node> messages = new ArrayList<>();
		if (!publicKeyMap.isEmpty()) {
			final var strings =
					publicKeyMap.keySet().stream().map(PubInfoKey::toString).collect(Collectors.toList());
			messages.addAll(addMap("Public key", strings));
			keyCheckBox.setSelected(true);
			messages.add(keyCheckBox);
		}
		if (!accountInfoMap.isEmpty()) {
			final var strings =
					accountInfoMap.keySet().stream().map(InfoKey::toString).collect(Collectors.toList());
			messages.addAll(addMap("Account information", strings));
			accountCheckBox.setSelected(true);
			messages.add(accountCheckBox);
		}

		var count = 0;
		for (final var message : messages) {
			details.add(message, 0, count++);
		}

		accountCheckBox.setDisable(isHistory());
		keyCheckBox.setDisable(isHistory());

		details.setVgap(10);
		return details;
	}

	@Override
	public JsonObject toJson() {
		final var toJson = super.toJson();
		final var publicKeys = new JsonObject();
		publicKeyMap.forEach((key, value) -> publicKeys.addProperty(key.nickname, value.getAbsolutePath()));
		final var accounts = new JsonObject();
		accountInfoMap.forEach((key, value) -> accounts.addProperty(key.nickname, value.getAbsolutePath()));
		toJson.add("publicKeys", publicKeys);
		toJson.add("accountInfos", accounts);
		toJson.addProperty("replaceKeys", keyCheckBox.isSelected());
		toJson.addProperty("replaceAccounts", accountCheckBox.isSelected());
		return toJson;
	}

	public List<Integer> getInfos() throws HederaClientException {
		final List<Integer> infos = new ArrayList<>();
		for (final var file : accountInfoMap.values()) {
			infos.add(Arrays.hashCode(readBytes(file)));
		}
		return infos;
	}

	public List<Integer> getPublicKeys() throws HederaClientException {
		final List<Integer> keys = new ArrayList<>();
		for (final var file : publicKeyMap.values()) {
			keys.add(Arrays.hashCode(readBytes(file)));
		}
		return keys;
	}

	@Override
	public boolean equals(final Object o) {
		if (!(o instanceof BundleFile)) {
			return false;
		}
		try {
			return getInfos().equals(((BundleFile) o).getInfos()) &&
					getPublicKeys().equals(((BundleFile) o).getPublicKeys());
		} catch (final HederaClientException e) {
			logger.error(e);
		}
		return false;
	}

	@Override
	public int hashCode() {
		var code = 0;
		for (final var entry : accountInfoMap.entrySet()) {
			code += entry.getKey().hashCode();
		}
		try {
			for (final var info : getInfos()) {
				code += info;
			}
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
		for (final var entry : publicKeyMap.entrySet()) {
			code += entry.getKey().hashCode();
		}
		try {
			for (final var publicKey : getPublicKeys()) {
				code += publicKey;
			}
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
		return code;
	}

	/**
	 * Add the names (nicknames) of the presented information to the list of messages
	 *
	 * @param text
	 * 		the type of file in the set
	 * @param set
	 * 		a set of nicknames
	 */
	private List<Label> addMap(final String text, final List<String> set) {
		final List<Label> labels = new ArrayList<>();
		final var label = new Label(String.format(BUNDLE_TITLE_MESSAGE_FORMAT, text));
		label.setWrapText(true);
		labels.add(label);
		final var list = new Label("\t\u2022\t" + String.join("\n\t\u2022\t", set));
		list.setWrapText(true);
		labels.add(list);
		return labels;
	}

	public class PubInfoKey implements Comparable<PubInfoKey> {
		private final String nickname;
		private final PublicKey publicKey;
		private final String oldNickname;

		public PubInfoKey(final File file) {
			this.nickname = FilenameUtils.getBaseName(file.getName());
			this.publicKey = EncryptionUtils.publicKeyFromFile(file.getAbsolutePath());
			this.oldNickname = existingPubKeysMap.getOrDefault(publicKey, "");
		}

		public String getNickname() {
			return nickname;
		}

		public String getOldNickname() {
			return oldNickname;
		}

		public PublicKey getPublicKey() {
			return publicKey;
		}

		@Override
		public boolean equals(final Object obj) {
			if (!(obj instanceof PubInfoKey)) {
				return false;
			}
			return Arrays.equals(publicKey.toBytes(), ((PubInfoKey) obj).getPublicKey().toBytes());
		}

		@Override
		public int hashCode() {
			return nickname.hashCode() + Arrays.hashCode(publicKey.toBytes());
		}

		@Override
		public int compareTo(@NotNull final PubInfoKey o) {
			return nickname.compareTo(o.nickname);
		}

		@Override
		public String toString() {
			return oldNickname.equals("") || oldNickname.equals(nickname) ?
					nickname :
					String.format("%s -> replaces the previous nickname: \"%s\"", nickname, oldNickname);
		}


	}

	public class InfoKey implements Comparable<InfoKey> {
		private final String nickname;
		private final Identifier id;
		private final String oldNickname;

		public InfoKey(final File file) throws HederaClientException, InvalidProtocolBufferException {
			final var info = AccountInfo.fromBytes(readBytes(file));
			final var accountMemo = info.accountMemo;

			this.id = new Identifier(info.accountId);
			this.nickname = accountMemo != null && !"".equals(accountMemo) ?
					accountMemo :
					FilenameUtils.getBaseName(file.getName());
			this.oldNickname =
					existingInfosMap.getOrDefault(info.accountId, "");

		}

		public String getNickname() {
			return replaceAccountNicknames() || "".equals(oldNickname) ? nickname : oldNickname;
		}

		public Identifier getId() {
			return id;
		}

		@Override
		public boolean equals(final Object obj) {
			if (!(obj instanceof InfoKey)) {
				return false;
			}
			return this.id.equals(((InfoKey) obj).getId()) && this.nickname.equals(((InfoKey) obj).getNickname());
		}

		@Override
		public int hashCode() {
			return id.hashCode() + nickname.hashCode();
		}

		@Override
		public String toString() {
			// case 1 info not in app; nickname == account number -> xx.xx.xxx-aaaaa
			// case 2 info not in app; nickname != account number -> nickname (xx.xx.xxx-aaaaa)
			// case 3 info in app -> nickname (xx.xx.xxx-aaaaa) replaces oldnickname (xx.xx.xxx-aaaaa)

			final var idChecksum = id.toReadableStringAndChecksum();
			if (!existingInfosMap.containsKey(id.asAccount())) {
				return nickname.equals(id.toReadableString()) ?
						idChecksum :
						String.format("%s (%s)", nickname, idChecksum);
			}
			return oldNickname.equals(nickname) ?
					String.format("%s (%s)", nickname, idChecksum) :
					String.format("%s (%s) -> replaces the previous nickname: \"%s\"", nickname, idChecksum,
							oldNickname);
		}

		@Override
		public int compareTo(final InfoKey other) {
			return this.id.compareTo(other.getId());
		}


	}
}
