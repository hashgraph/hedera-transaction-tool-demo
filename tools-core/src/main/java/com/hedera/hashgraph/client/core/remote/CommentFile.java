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

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;

public class CommentFile extends RemoteFile {
	private static final Logger logger = LogManager.getLogger(CommentFile.class);

	@JsonProperty(value = "timestamp", required = true)
	private Timestamp timestamp;

	@JsonProperty(value = "linkedFile", required = true)
	private String linkedFile;

	public CommentFile(FileDetails file) {
		super(file);
		if (file == null) {
			return;
		}
		this.timestamp = new Timestamp(file.getAttributes().creationTime().toInstant());
		var files = new File(file.getPath()).listFiles();
		assert files != null;
		for (var file1 : files) {
			if (file1.getName().endsWith(Constants.TXT_EXTENSION)) {
				continue;
			}
			if (FilenameUtils.getBaseName(file1.getName()).equals(FilenameUtils.getBaseName(getName()))) {
				this.linkedFile = file1.getAbsolutePath();
			}
		}
	}


	public JsonObject getContents() {
		var contents = new JsonObject();
		if (new File(getPath()).exists()) {
			try {
				contents = readJsonObject(getPath());
			} catch (HederaClientException e) {
				logger.error(e);
			}
		}
		return contents;
	}


	public Timestamp getTimestamp() {
		return timestamp;
	}

	public String getLinkedFile() {
		return linkedFile;
	}

	public boolean hasLinkedFile() {
		return linkedFile != null;
	}

	@Override
	public boolean equals(Object o) {
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
