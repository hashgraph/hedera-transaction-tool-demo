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

package com.hedera.hashgraph.client.core.remote.helpers;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.hedera.hashgraph.client.core.constants.JsonConstants;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.INCOMPATIBLE_TYPES_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.NULL_OBJECT_COMPARISON_ERROR_MESSAGE;

public class MetadataAction implements Comparable<MetadataAction> {
	private static final String TIMESTAMP_STRING = "timestamp";
	private static final String KEY_NAME = "keyName";
	private static final String USER_COMMENTS = "userComments";
	private static final String ACTION_STRING = "action";
	private Timestamp timeStamp;
	private Actions actions;
	private String userComments;
	private String keyName;

	public MetadataAction(final Timestamp timestamp, final Actions actions, final String userComments,
			final String keyName) {
		this.timeStamp = new Timestamp(timestamp.getSeconds(), 0);
		this.actions = actions;
		this.userComments = userComments;
		this.keyName = keyName;
	}

	public MetadataAction(final String line) throws HederaClientException {
		final JsonObject metadata;
		try {
			metadata = JsonParser.parseString(line).getAsJsonObject();
		} catch (final Exception e) {
			throw new HederaClientException("Cannot parse object");
		}

		if (!metadata.has(TIMESTAMP_STRING) || !metadata.has(ACTION_STRING)) {
			throw new HederaClientException("Invalid metadata");
		}

		final var time = metadata.getAsJsonObject(TIMESTAMP_STRING);
		this.timeStamp = new Timestamp(time.get(JsonConstants.SECONDS).getAsLong(), 0);
		this.actions = Actions.valueOf(metadata.get(ACTION_STRING).getAsString());
		this.userComments = (metadata.has(USER_COMMENTS)) ? metadata.get(USER_COMMENTS).getAsString() : "";
		this.keyName = metadata.has(KEY_NAME) ? metadata.get(KEY_NAME).getAsString() : "";
	}

	public MetadataAction() {
		this.timeStamp = new Timestamp();
		this.actions = Actions.DECLINE;
		this.userComments = "";
		this.keyName = "";
	}

	public Timestamp getTimeStamp() {
		return timeStamp;
	}

	public void setTimeStamp(final Timestamp timeStamp) {
		this.timeStamp = timeStamp;
	}

	public Actions getActions() {
		return actions;
	}

	public void setActions(final Actions actions) {
		this.actions = actions;
	}

	public String getUserComments() {
		return userComments;
	}

	public void setUserComments(final String userComments) {
		this.userComments = userComments;
	}

	public String getKeyName() {
		return keyName;
	}

	public void setKeyName(final String keyName) {
		this.keyName = keyName;
	}

	public JsonObject toJson() {
		final var jsonObject = new JsonObject();
		jsonObject.add(TIMESTAMP_STRING, this.timeStamp.asJSON());
		jsonObject.addProperty(ACTION_STRING, this.actions.getAction());
		jsonObject.addProperty(USER_COMMENTS, this.userComments);
		jsonObject.addProperty(KEY_NAME, this.keyName);

		return jsonObject;
	}

	@Override
	public String toString() {
		return toJson().toString();
	}

	@Override
	public boolean equals(final Object o) {
		if (o == null) {
			throw new NullPointerException(NULL_OBJECT_COMPARISON_ERROR_MESSAGE);
		}

		if (getClass() != o.getClass()) {
			throw new HederaClientRuntimeException(INCOMPATIBLE_TYPES_ERROR_MESSAGE);
		}

		if (this.timeStamp.getSeconds() != ((MetadataAction) o).getTimeStamp().getSeconds()) {
			return false;
		}

		return this.keyName.equals(((MetadataAction) o).getKeyName());
	}

	@Override
	public int hashCode() {
		return super.hashCode() + timeStamp.hashCode() + keyName.hashCode();
	}

	@Override
	public int compareTo(@NotNull final MetadataAction o) {
		if (getClass() != o.getClass()) {
			throw new HederaClientRuntimeException(INCOMPATIBLE_TYPES_ERROR_MESSAGE);
		}

		if (this.timeStamp.getSeconds() != o.getTimeStamp().getSeconds()) {
			return Long.compare(timeStamp.getSeconds(), o.getTimeStamp().getSeconds());
		}

		return this.keyName.compareTo(o.getKeyName());
	}

	public String toReadableString() {
		return StringUtils.capitalize(actions.toString()) + " on " + timeStamp.asReadableLocalString();
	}
}
