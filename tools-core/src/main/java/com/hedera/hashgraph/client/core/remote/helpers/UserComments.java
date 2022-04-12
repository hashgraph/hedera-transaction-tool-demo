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

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

public class UserComments {

	private static final Logger logger = LogManager.getLogger(UserComments.class);
	public static final String AUTHOR_STRING = "Author";
	public static final String CONTENTS_STRING = "Contents";
	public static final String TIMESTAMP_STRING = "Timestamp";

	@JsonProperty()
	private String author;

	@JsonProperty()
	private String comment;

	@JsonProperty()
	private String timestamp;

	public UserComments() {
		// Default constructor
		author = "";
		comment = "";
		timestamp = "";
	}

	public String getAuthor() {
		return author;
	}

	public String getComment() {
		return comment;
	}

	public String getTimestamp() {
		return timestamp;
	}

	public void toFile(final String location) {

		try (final var writer = new BufferedWriter(new FileWriter(location))) {
			writer.write(this.toString());
		} catch (final IOException e) {
			logger.error(e);
		}
	}

	public UserComments parse(final File filename) throws FileNotFoundException {

		// Read file into object
		final var file = new FileReader(filename);
		final var comments = (JsonObject) JsonParser.parseReader(file);

		final var userCommentsBuilder = new Builder();
		userCommentsBuilder.withAuthor(
				(comments.has(AUTHOR_STRING)) ? comments.get(AUTHOR_STRING).getAsString() : "");
		userCommentsBuilder.withComment(
				(comments.has(CONTENTS_STRING)) ? comments.get(CONTENTS_STRING).getAsString() : "");
		userCommentsBuilder.withTimestamp(
				(comments.has(TIMESTAMP_STRING)) ? comments.get(TIMESTAMP_STRING).getAsString() : "");
		return userCommentsBuilder.build();
	}

	public UserComments parse(final RemoteFile remoteFile) throws FileNotFoundException {
		if (remoteFile != null) {
			final var file = new File(remoteFile.getParentPath(), remoteFile.getName());
			return parse(file);
		}
		return new UserComments();
	}

	public JsonObject asJsonObject() {
		final var object = new JsonObject();
		object.addProperty("author", author);
		object.addProperty("comment", comment);
		object.addProperty("timestamp", timestamp);
		return object;
	}

	public static final class Builder {
		private String author;
		private String comment;
		private String timestamp = "";

		public Builder() {
			// Default constructor
		}

		public Builder withAuthor(final String author) {
			this.author = Objects.requireNonNullElse(author, "");
			return this;
		}

		public Builder withComment(final String comment) {
			this.comment = Objects.requireNonNullElse(comment, "");
			return this;
		}

		public Builder withTimestamp(final String timestamp) {
			this.timestamp = Objects.requireNonNullElse(timestamp, "");
			return this;
		}


		public UserComments build() {
			final var userComments = new UserComments();
			userComments.comment = this.comment;
			userComments.author = this.author;

			if ("".equals(this.timestamp)) {
				final DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
				final var date = new Date();
				this.timestamp = dateFormat.format(date);
			}
			userComments.timestamp = this.timestamp;
			return userComments;
		}
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.JSON_STYLE)
				.append(AUTHOR_STRING, author)
				.append(CONTENTS_STRING, comment)
				.append(TIMESTAMP_STRING, timestamp)
				.toString();
	}
}
