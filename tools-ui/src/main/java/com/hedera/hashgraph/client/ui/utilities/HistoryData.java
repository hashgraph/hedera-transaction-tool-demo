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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.helpers.MetadataAction;
import com.hedera.hashgraph.client.core.remote.helpers.UserComments;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class HistoryData implements Comparable<HistoryData> {
	private static final Logger logger = LogManager.getLogger(HistoryData.class);

	private static final String FILENAME_PROPERTY = "filename";
	private static final String REMOTE_FILE_PARAMETER = "remoteFile";
	private static final String COMMENTS_PARAMETER = "comments";
	private static final String ACTIONS_PARAMETER = "actions";
	private static final String CODE_PARAMETER = "code";
	private static final String HISTORY_BOOLEAN_PARAMETER = "historyBoolean";
	private static final String TITLE_PROPERTY = "title";
	private static final String EXPIRED_PARAMETER = "isExpired";

	private String title;
	private String fileName;
	private final String remoteFilePath;
	private int code;
	private final JsonObject comments;
	private final List<MetadataAction> actions = new ArrayList<>();
	private final MetadataAction lastAction;
	private boolean history = false;
	private boolean expired = false;

	public HistoryData(final RemoteFile remoteFile) throws FileNotFoundException {
		this.title = remoteFile.getTitle();
		this.fileName = remoteFile.getName();
		this.code = remoteFile.hashCode();
		this.remoteFilePath = remoteFile.getPath();
		this.comments = new UserComments().parse(remoteFile.getCommentsFile()).asJsonObject();
		this.actions.addAll(remoteFile.getSigningHistory());
		this.lastAction = Collections.max(actions);
		this.expired = remoteFile.isExpired();
	}

	public HistoryData(final JsonObject object) throws JsonProcessingException {
		this.title = object.get(TITLE_PROPERTY).getAsString();
		this.fileName = object.get(FILENAME_PROPERTY).getAsString();
		this.code = object.get(CODE_PARAMETER).getAsInt();
		this.remoteFilePath = object.get(REMOTE_FILE_PARAMETER).getAsString();
		this.comments = object.get(COMMENTS_PARAMETER).getAsJsonObject();
		for (final var element : object.getAsJsonArray(ACTIONS_PARAMETER)) {
			try {
				actions.add(new MetadataAction(element.getAsString()));
			} catch (final HederaClientException e) {
				logger.error("Cannot parse action");
			}
		}
		this.lastAction = Collections.max(actions);
		this.history = object.get(HISTORY_BOOLEAN_PARAMETER).getAsBoolean();
		this.expired = object.get(EXPIRED_PARAMETER).getAsBoolean();
	}

	public FileType getType() {
		try {
			return FileType.getType(FilenameUtils.getExtension(fileName));
		} catch (final HederaClientException e) {
			return FileType.UNKNOWN;
		}
	}

	public void addAction(final MetadataAction action) {
		this.actions.add(action);
		this.history = true;
	}

	public String getLastAction() {
		return lastAction.toReadableString();
	}

	public String getLastComment() {
		return (comments.has("comment")) ? comments.get("comment").getAsString() : "";
	}

	public String getPreparedBy() {

		final var author = (comments.has("author")) ? comments.get("author").getAsString() : "";
		if ("".equals(author)) {
			return author;
		}

		final var split = author.split("@");

		if (split.length == 1) {
			return author;
		}

		if (split.length == 2) {
			return String.format("%s (%s)", split[0], split[1]);
		}

		return author;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(final String fileName) {
		this.fileName = fileName;
	}

	public String getRemoteFilePath() {
		return remoteFilePath;
	}

	public boolean isExpired() {
		return expired;
	}

	public int getCode() {
		return code;
	}

	public void setCode(final int code) {
		this.code = code;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(final String title) {
		this.title = title;
	}

	public boolean isHistory() {
		return history;
	}

	public void setHistory(final boolean history) {
		this.history = history;
	}

	public Long lastAction() {
		return (Collections.max(actions)).getTimeStamp().getSeconds();
	}

	public JsonObject asJson() {
		final var asJson = new JsonObject();
		final JsonArray array = new JsonArray();
		actions.stream().map(MetadataAction::toString).forEach(array::add);

		asJson.addProperty(TITLE_PROPERTY, this.title);
		asJson.addProperty(FILENAME_PROPERTY, this.fileName);
		asJson.addProperty(CODE_PARAMETER, this.code);
		asJson.addProperty(REMOTE_FILE_PARAMETER, this.remoteFilePath);
		asJson.add(COMMENTS_PARAMETER, this.comments);
		asJson.add(ACTIONS_PARAMETER, array);
		asJson.addProperty(HISTORY_BOOLEAN_PARAMETER, history);
		asJson.addProperty(EXPIRED_PARAMETER, expired);
		return asJson;
	}

	@Override
	public String toString() {
		return this.asJson().toString();
	}

	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof HistoryData)) {
			return false;
		}
		return this.fileName.equals(((HistoryData) obj).getFileName()) &&
				this.remoteFilePath.equals(((HistoryData) obj).getRemoteFilePath()) &&
				this.code == ((HistoryData) obj).getCode();
	}

	@Override
	public int hashCode() {
		return this.fileName.hashCode() +
				this.remoteFilePath.hashCode() +
				this.code;
	}

	@Override
	public int compareTo(@NotNull final HistoryData o) {
		return Long.compare(lastAction(), o.lastAction());
	}
}
