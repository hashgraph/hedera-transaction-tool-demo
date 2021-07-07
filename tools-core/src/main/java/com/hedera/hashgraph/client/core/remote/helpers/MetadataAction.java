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

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.INCOMPATIBLE_TYPES_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.NULL_OBJECT_COMPARISON_ERROR_MESSAGE;

public class MetadataAction implements Comparable {
	private Timestamp timestamp;
	private Actions action;
	private String userComments;
	private String keyName;

	public MetadataAction(Timestamp timestamp, Actions action, String userComments, String keyName) {
		this.timestamp = new Timestamp(timestamp.getSeconds(), 0);
		this.action = action;
		this.userComments = userComments;
		this.keyName = keyName;
	}

	public MetadataAction(String line) throws HederaClientException {
		JsonObject metadata;
		try {
			metadata = JsonParser.parseString(line).getAsJsonObject();
		} catch (Exception e) {
			throw new HederaClientException("Cannot parse object");
		}

		if (!metadata.has("timestamp") || !metadata.has("action")) {
			throw new HederaClientException("Invalid metadata");
		}

		var time = metadata.getAsJsonObject("timestamp");
		this.timestamp = new Timestamp(time.get(JsonConstants.SECONDS).getAsLong(), 0);
		this.action = Actions.valueOf(metadata.get("action").getAsString());
		this.userComments = (metadata.has("userComments")) ? metadata.get("userComments").getAsString() : "";
		this.keyName = metadata.has("keyName") ? metadata.get("keyName").getAsString() : "";
	}

	public Timestamp getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(Timestamp timestamp) {
		this.timestamp = timestamp;
	}

	public Actions getAction() {
		return action;
	}

	public void setAction(Actions action) {
		this.action = action;
	}

	public String getUserComments() {
		return userComments;
	}

	public void setUserComments(String userComments) {
		this.userComments = userComments;
	}

	public String getKeyName() {
		return keyName;
	}

	public void setKeyName(String keyName) {
		this.keyName = keyName;
	}

	public JsonObject toJson() {
		var jsonObject = new JsonObject();
		jsonObject.add("timestamp", this.timestamp.asJSON());
		jsonObject.addProperty("action", this.action.getAction());
		jsonObject.addProperty("userComments", this.userComments);
		jsonObject.addProperty("keyName", this.keyName);

		return jsonObject;
	}

	@Override
	public String toString() {
		return toJson().toString();
	}

	@Override
	public int compareTo(Object o) {
		if (o == null) {
			throw new NullPointerException(NULL_OBJECT_COMPARISON_ERROR_MESSAGE);
		}

		if (getClass() != o.getClass()) {
			throw new HederaClientRuntimeException(INCOMPATIBLE_TYPES_ERROR_MESSAGE);
		}

		if (this.timestamp.getSeconds() != ((MetadataAction) o).getTimestamp().getSeconds()) {
			return Long.compare(timestamp.getSeconds(), ((MetadataAction) o).getTimestamp().getSeconds());
		}

		return this.keyName.compareTo(((MetadataAction) o).getKeyName());
	}
}
