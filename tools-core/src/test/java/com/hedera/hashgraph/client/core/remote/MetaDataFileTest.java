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

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.MetadataAction;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class MetaDataFileTest extends TestBase {
	private static final Logger logger = LogManager.getLogger(MetaDataFileTest.class);

	@Before
	public void setUp() throws Exception {
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
	}

	@Test
	public void constructor_test() throws IOException {
		final var file = new File("src/test/resources/Files/0.0.2.meta");
		final var info = FileDetails.parse(file);

		final var metaDataFile = new MetadataFile(info);
		assertNotNull(metaDataFile);
		assertTrue(metaDataFile.isValid());

		final var badFile = new File("src/test/resources/Files/0.0.2.meta");
		final var badInfo = FileDetails.parse(badFile);
		assertFalse(new InfoFile(badInfo).isValid());

	}

	@Test
	public void addAction_test() throws IOException, HederaClientException {
		final var fileCopy = new File("src/test/resources/Files/0.0.2-copy.meta");
		FileUtils.copyFile(new File("src/test/resources/Files/0.0.2.meta"),
				fileCopy);

		final var info = FileDetails.parse(fileCopy);

		final var metaDataFile = new MetadataFile(info);
		final var initialActions = metaDataFile.getMetadataActions();
		assertEquals(1, initialActions.size());


		final var action = new MetadataAction("{\"timestamp\":{\"seconds\":1610396976,\"nanos\":203273000}," +
				"\"action\":\"ACCEPT\",\"location\":\"\\/Users\\/davidmatusevich\\/Hedera Council\\/Transactions - " +
				"Documents\",\"userComments\":\"a comment from the user\",\"keyName\":\"\"}");
		metaDataFile.addAction(action);

		Files.deleteIfExists(fileCopy.toPath());
	}
}
