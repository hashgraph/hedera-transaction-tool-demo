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

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.MetadataAction;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class MetadataFile extends RemoteFile {
	private static final Logger logger = LogManager.getLogger(MetadataFile.class);

	private final Set<MetadataAction> metadataActions = new HashSet<>();

	public MetadataFile(final FileDetails f) {
		super(f);
		if (!isValid()) {
			return;
		}
		List<String> lines = new ArrayList<>();
		final var location = new File(getPath());
		try {
			lines = Files.readAllLines(location.toPath());
		} catch (final Exception e) {
			logger.error(e);
		}

		for (final var line : lines) {
			try {
				metadataActions.add(new MetadataAction(line));
			} catch (final HederaClientException e) {
				logger.error(e);
			}
		}
	}

	public void addAction(final MetadataAction metadataAction) {
		metadataActions.add(metadataAction);
		toFile(new File(Constants.DEFAULT_HISTORY, getName()));
		setValid(true);
	}

	public MetadataFile(final String name) throws HederaClientException {
		super(new File(Constants.DEFAULT_HISTORY,
				FilenameUtils.getBaseName(name) + "." + Constants.METADATA_EXTENSION).getAbsolutePath());
		if (!isValid()) {
			return;
		}
		final List<String> lines;
		final var location = new File(Constants.DEFAULT_HISTORY,
				FilenameUtils.getBaseName(name) + "." + Constants.METADATA_EXTENSION);
		try {
			lines = Files.readAllLines(location.toPath());
		} catch (final Exception e) {
			throw new HederaClientException(e);
		}

		for (final var line : lines) {
			metadataActions.add(new MetadataAction(line));
		}

	}

	public List<MetadataAction> getMetadataActions() {
		return new ArrayList<>(new TreeSet<>(metadataActions));
	}

	private void toFile(final File location) {
		final List<MetadataAction> actionList = new ArrayList<>(new TreeSet<>(metadataActions));
		try {
			final var fr = new FileWriter(location, false);
			final var br = new BufferedWriter(fr);
			final var pr = new PrintWriter(br);
			for (final var metadataAction : actionList) {
				pr.println(metadataAction.toString());
			}
			pr.close();
			br.close();
			fr.close();
		} catch (final IOException e) {
			logger.error(e);
		}
	}

}
