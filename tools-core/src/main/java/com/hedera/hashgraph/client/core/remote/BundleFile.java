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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;

public class BundleFile extends RemoteFile implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(BundleFile.class);
	private static final String MESSAGE_FORMAT = "The administrator has shared information regarding the following %s.";

	private final Map<InfoKey, File> accountInfoMap = new TreeMap<>();
	private final Map<String, File> publicKeyMap = new TreeMap<>();
	private final Map<AccountId, String> existingInfosMap = new HashMap<>();
	private final List<FileActions> actions = Arrays.asList(FileActions.ACCEPT, FileActions.DECLINE);
	private final CheckBox checkBox = new CheckBox("Replace existing nicknames");

	public BundleFile(final FileDetails file) {
		super(file);
		if (file == null) {
			return;
		}

		loadExistingInfos();

		try {
			final var tempFile = Files.createTempDirectory("tmpDirPrefix").toFile();
			unZip(file.getFullPath(), tempFile.getAbsolutePath());
			final var files = tempFile.listFiles();
			for (final File unzipped : files) {
				final var name = unzipped.getName();
				switch (FilenameUtils.getExtension(name)) {
					case INFO_EXTENSION:
						accountInfoMap.put(new InfoKey(unzipped), unzipped);
						break;
					case PUB_EXTENSION:
						publicKeyMap.put(name, unzipped);
						break;
					default:
						logger.error("Unexpected value: {}", name);
				}
			}
			setValid(!(publicKeyMap.isEmpty() && accountInfoMap.isEmpty()));
		} catch (final IOException | HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	private void loadExistingInfos() {
		try {
			final var nicknames =
					new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			final var entries = nicknames.entrySet();
			for (final Map.Entry<String, JsonElement> entry : entries) {
				existingInfosMap.put(Identifier.parse(entry.getKey()).asAccount(), entry.getValue().getAsString());
			}

		} catch (final HederaClientException e) {
			logger.error("Exception {} on line {}", e.getMessage(), e.getStackTrace()[0].getLineNumber());
		}

	}

	public Map<InfoKey, File> getAccountInfoMap() {
		return accountInfoMap;
	}

	public Map<String, File> getPublicKeyMap() {
		return publicKeyMap;
	}

	public boolean replaceNicknames() {
		return checkBox.isSelected();
	}

	@Override
	public List<FileActions> getActions() {
		return actions;
	}

	@Override
	public GridPane buildGridPane() {
		final var details = new GridPane();

		final List<Label> messages = new ArrayList<>();
		if (!publicKeyMap.isEmpty()) {
			messages.addAll(addMap("public keys", new ArrayList<>(publicKeyMap.keySet())));
		}
		if (!accountInfoMap.isEmpty()) {
			final List<String> strings = new ArrayList<>();
			for (final InfoKey infoKey : accountInfoMap.keySet()) {
				strings.add(infoKey.toString());
			}
			messages.addAll(addMap("accounts", strings));
		}

		var count = 0;
		for (final var message : messages) {
			details.add(message, 0, count++);
		}

		if (!accountInfoMap.isEmpty()) {
			checkBox.setSelected(true);
			details.add(checkBox, 0, count);
		}

		details.setVgap(10);
		return details;
	}

	@Override
	public boolean equals(final Object o) {
		if (!(o instanceof BundleFile)) {
			return false;
		}
		return super.equals(o) &&
				this.accountInfoMap.equals(((BundleFile) o).getAccountInfoMap()) &&
				this.publicKeyMap.equals(((BundleFile) o).getPublicKeyMap());
	}

	@Override
	public int hashCode() {
		var code = super.hashCode();
		for (final Map.Entry<InfoKey, File> entry : accountInfoMap.entrySet()) {
			code += entry.getKey().hashCode() + entry.getValue().hashCode();
		}
		for (final Map.Entry<String, File> entry : publicKeyMap.entrySet()) {
			code += entry.getKey().hashCode() + entry.getValue().hashCode();
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
		final Label label = new Label(String.format(MESSAGE_FORMAT, text));
		label.setWrapText(true);
		labels.add(label);
		final Label list = new Label("\t\u2022\t" + String.join("\n\t\u2022\t", set));
		list.setWrapText(true);
		labels.add(list);
		return labels;
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
			return replaceNicknames() || "".equals(oldNickname) ? nickname : oldNickname;
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
