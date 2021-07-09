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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
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

	public MetadataFile() {
		super();
	}

	public MetadataFile(FileDetails f) {
		super(f);
		if (!isValid()) {
			return;
		}
		List<String> lines = new ArrayList<>();
		var location = new File(getPath());
		try {
			lines = Files.readAllLines(location.toPath());
		} catch (Exception e) {
			logger.error(e);
		}

		for (var line : lines) {
			try {
				metadataActions.add(new MetadataAction(line));
			} catch (HederaClientException e) {
				logger.error(e);
			}
		}
	}

	public void addAction(MetadataAction metadataAction) {
		metadataActions.add(metadataAction);
		toFile(new File(Constants.DEFAULT_HISTORY, getName()));
		setValid(true);
	}

	public MetadataFile(String name) throws HederaClientException {
		super(new File(Constants.DEFAULT_HISTORY,
				FilenameUtils.getBaseName(name) + "." + Constants.METADATA_EXTENSION).getAbsolutePath());
		if (!isValid()) {
			return;
		}
		List<String> lines;
		var location = new File(Constants.DEFAULT_HISTORY,
				FilenameUtils.getBaseName(name) + "." + Constants.METADATA_EXTENSION);
		try {
			lines = Files.readAllLines(location.toPath());
		} catch (Exception e) {
			throw new HederaClientException(e);
		}

		for (var line : lines) {
			metadataActions.add(new MetadataAction(line));
		}

	}

	public List<MetadataAction> getMetadataActions() {
		return new ArrayList<>(new TreeSet<>(metadataActions));
	}

	private void toFile(File location) {
		List<MetadataAction> actionList = new ArrayList<>(new TreeSet<>(metadataActions));
		try {
			var fr = new FileWriter(location, false);
			var br = new BufferedWriter(fr);
			var pr = new PrintWriter(br);
			for (var metadataAction : actionList) {
				pr.println(metadataAction.toString());
			}
			pr.close();
			br.close();
			fr.close();
		} catch (IOException e) {
			logger.error(e);
		}
	}

	@Override
	public boolean equals(Object o) {
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return super.hashCode() + metadataActions.hashCode();
	}
}
