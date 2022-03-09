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
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.security.Ed25519PublicKey;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Messages.PEM_EXISTS_MESSAGE;
import static org.apache.commons.io.FileUtils.contentEquals;


public class PublicKeyFile extends RemoteFile implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(PublicKeyFile.class);

	private Timestamp timestamp;
	private final List<FileActions> actions = Arrays.asList(FileActions.ACCEPT, FileActions.DECLINE);

	public PublicKeyFile() {
		super();
	}

	public PublicKeyFile(final FileDetails f) {
		super(f);
		final var l = f.getAttributes().lastModifiedTime().toMillis();
		this.timestamp = new Timestamp(l / 1000, (int) (l % 1000) * 1000000);
		if (this.isValid()) {
			setValid(validPubKey(this.getPath()));
		}
	}

	public Timestamp getTimestamp() {
		return timestamp;
	}

	public boolean exists() {
		if (!new File(KEYS_FOLDER).exists()) {
			return false;
		}
		final var fileList =
				new File(DEFAULT_STORAGE, "Keys").list(
						(dir, name) -> FilenameUtils.getBaseName(name).equals(getBaseName()));
		assert fileList != null;
		return fileList.length > 0;
	}

	public boolean duplicate() throws IOException {
		if (!new File(KEYS_FOLDER).exists()) {
			return false;
		}
		final var location = KEYS_FOLDER + getName();
		if (exists()) {
			// If I have a pem corresponding to this pub, I will not replace it
			if (getName().endsWith(PUB_EXTENSION) && new File(location.replace(PUB_EXTENSION, PK_EXTENSION)).exists()) {
				logger.info(PEM_EXISTS_MESSAGE);
				return true;
			}
			return contentEquals(new File(location), new File(getPath()));
		}

		return false;
	}

	private boolean validPubKey(final String file) {
		try {
			final var publicKeyString = new String(Files.readAllBytes(Paths.get(file)));
			Ed25519PublicKey.fromString(publicKeyString);
			return true;
		} catch (final Exception e) {
			logger.error("Cannot parse public key");
			return false;
		}
	}

	@Override
	public List<FileActions> getActions() {
		return actions;
	}

	@Override
	public GridPane buildGridPane() {
		final var detailsGridPane = new GridPane();
		List<Label> messages = new ArrayList<>();


		final var l = new Label(exists() ?
				String.format("Would you like to replace %s with a new version?", getName()) :
				String.format("Would you like to import the following public key: %s", getName()));

		l.setWrapText(true);
		messages.add(l);

		if (isHistory()) {
			try {
				messages = getHistory("key");
			} catch (final HederaClientException e) {
				logger.error(e);
			}
		}

		var count = 0;
		for (final var message : messages) {
			detailsGridPane.add(message, 0, count++);
		}

		return detailsGridPane;
	}

	@Override
	public boolean isExpired() {
		try {
			return duplicate();
		} catch (final IOException e) {
			logger.error(e);
		}
		return false;
	}

	@Override
	public boolean equals(final Object o) {
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return super.hashCode() + actions.hashCode() + timestamp.hashCode();
	}

	@Override
	public JsonObject toJson() {
		final var toJson = super.toJson();
		toJson.add("timestamp", timestamp.asJSON());
		return toJson;
	}
}
