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
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.BatchFile;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.TransactionFile;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.MetadataAction;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import static com.hedera.hashgraph.client.core.enums.FileType.BATCH;
import static com.hedera.hashgraph.client.core.enums.FileType.TRANSACTION;
import static com.hedera.hashgraph.client.core.enums.FileType.UNKNOWN;

public class HistoryData implements Comparable<HistoryData> {
	private static final Logger logger = LogManager.getLogger(HistoryData.class);

	private static final String FILENAME_PROPERTY = "filename";
	private static final String REMOTE_FILE_PARAMETER = "remoteFile";
	private static final String ACTIONS_PARAMETER = "actions";
	private static final String CODE_PARAMETER = "code";
	private static final String HISTORY_BOOLEAN_PARAMETER = "historyBoolean";
	private static final String TITLE_PROPERTY = "title";
	private static final String EXPIRED_PARAMETER = "isExpired";
	private static final String FEE_PAYER_PROPERTY = "feePayer";
	private static final String EXPIRATION_DATE = "expirationDate";

	private String title;
	private String fileName;
	private final String remoteFilePath;
	private int code;
	private final List<MetadataAction> actions = new ArrayList<>();
	private final MetadataAction lastAction;
	private final Identifier feePayer;
	private final Timestamp expirationDate;

	private boolean history = false;
	private boolean expired = false;

	public HistoryData(final RemoteFile remoteFile) {
		this.title = remoteFile.getTitle();
		this.fileName = remoteFile.getName();
		this.code = remoteFile.hashCode();
		this.remoteFilePath = remoteFile.getPath();
		this.actions.addAll(remoteFile.getSigningHistory());
		this.feePayer = remoteFile.getType().equals(TRANSACTION) ?
				((TransactionFile) remoteFile).getFeePayerAccountId() :
				remoteFile.getType().equals(BATCH) ?
						((BatchFile) remoteFile).getFeePayerAccountID() :
						Identifier.ZERO;
		this.expirationDate = remoteFile.getType().equals(TRANSACTION) ?
				((TransactionFile) remoteFile).getExpiration() :
				remoteFile.getType().equals(BATCH) ?
						((BatchFile) remoteFile).getFirstTransactionTimeStamp() :
						new Timestamp();
		this.lastAction = !actions.isEmpty() ? Collections.max(actions) : new MetadataAction();
		this.expired = remoteFile.isExpired();
	}

	public HistoryData(final JsonObject object) throws JsonProcessingException, HederaClientException {
		this.title = object.get(TITLE_PROPERTY).getAsString();
		this.fileName = object.get(FILENAME_PROPERTY).getAsString();
		this.code = object.get(CODE_PARAMETER).getAsInt();
		this.remoteFilePath = object.get(REMOTE_FILE_PARAMETER).getAsString();
		for (final var element : object.getAsJsonArray(ACTIONS_PARAMETER)) {
			try {
				actions.add(new MetadataAction(element.getAsString()));
			} catch (final HederaClientException e) {
				logger.error("Cannot parse action");
			}
		}
		this.feePayer = Identifier.parse(object.get(FEE_PAYER_PROPERTY).getAsJsonObject());
		this.expirationDate = new Timestamp(object.get(EXPIRATION_DATE).getAsJsonObject());
		this.lastAction = !actions.isEmpty() ? Collections.max(actions) : new MetadataAction();
		this.history = object.get(HISTORY_BOOLEAN_PARAMETER).getAsBoolean();
		this.expired = object.get(EXPIRED_PARAMETER).getAsBoolean();
	}

	public HistoryData(final String path) throws IOException, HederaClientException {
		this(new RemoteFile().getSingleRemoteFile(FileDetails.parse(new File(path))));
	}

	public FileType getType() {
		try {
			return FileType.getType(FilenameUtils.getExtension(fileName));
		} catch (final HederaClientException e) {
			return UNKNOWN;
		}
	}

	public void addAction(final MetadataAction action) {
		this.actions.add(action);
		this.history = true;
	}

	public Actions getActions() {
		return lastAction.getActions();
	}

	public long getLasActionSeconds() {
		return lastAction.getTimeStamp().getSeconds();
	}

	public String getLastAction() {
		final var actionString = Actions.ACCEPT == lastAction.getActions() ?
				TRANSACTION.equals(getType()) || BATCH.equals(getType()) ?
						"Signed" :
						"Accepted" :
				"Declined";
		final var stamp = lastAction.getTimeStamp().asDate();
		final var format = Locale.US.equals(Locale.getDefault()) ? "MM/dd/yyyy hh:mm:ss aa" : "dd/MM/yyyy HH:mm:ss";
		final var sdf = new SimpleDateFormat(format);

		return String.format("%s on %s", actionString,
				sdf.format(stamp) + " " + TimeZone.getDefault().getDisplayName(false, TimeZone.SHORT));
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

	public Timestamp lastAction() {
		Timestamp t = new Timestamp(0, 0);
		for (final var action : actions) {
			if (action.getTimeStamp().compareTo(t) > 0) {
				t = action.getTimeStamp();
			}
		}
		return t;
	}

	public String getFeePayer() {
		return feePayer.toReadableStringAndChecksum();
	}

	public String getExpirationDate() {
		final var stamp = expirationDate.asDate();
		final var format = Locale.US.equals(Locale.getDefault()) ? "MM/dd/yyyy\nhh:mm:ss aa" : "dd/MM/yyyy\nHH:mm:ss";
		final var sdf = new SimpleDateFormat(format);
		return sdf.format(stamp) + " " + TimeZone.getDefault().getDisplayName(false, TimeZone.SHORT);
	}

	public JsonObject asJson() {
		final var asJson = new JsonObject();
		final var array = new JsonArray();
		actions.stream().map(MetadataAction::toString).forEach(array::add);

		asJson.addProperty(TITLE_PROPERTY, this.title);
		asJson.addProperty(FILENAME_PROPERTY, this.fileName);
		asJson.addProperty(CODE_PARAMETER, this.code);
		asJson.addProperty(REMOTE_FILE_PARAMETER, this.remoteFilePath);
		asJson.add(ACTIONS_PARAMETER, array);
		asJson.add(FEE_PAYER_PROPERTY, feePayer.asJSON());
		asJson.add(EXPIRATION_DATE, expirationDate.asJSON());
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
		return lastAction().compareTo(o.lastAction());
	}

	public LocalDate getExpirationLocalDate() {
		return expirationDate.asInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}

	public LocalDate getActionLocalDate() {
		return lastAction.getTimeStamp().asInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}
}
