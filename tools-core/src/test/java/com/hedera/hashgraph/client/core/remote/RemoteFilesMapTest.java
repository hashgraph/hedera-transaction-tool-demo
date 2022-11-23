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

import com.google.common.graph.Network;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.fileservices.FileAdapterFactory;
import com.hedera.hashgraph.client.core.interfaces.FileService;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.UserComments;
import com.hedera.hashgraph.client.core.transactions.*;
import com.hedera.hashgraph.sdk.*;
import javafx.util.Pair;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;

import java.io.File;
import java.nio.file.Files;
import java.time.Duration;
import java.time.Instant;
import java.util.*;

import static com.hedera.hashgraph.client.core.constants.Constants.*;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_LOAD_TRANSACTION_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_VALIDATE_INPUT_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.*;
import static com.hedera.hashgraph.client.core.constants.Messages.TRANSACTION_CREATED_MESSAGE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;


public class RemoteFilesMapTest extends TestBase implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(RemoteFilesMapTest.class);

	private static final String TEMP_STORAGE = "";

	@BeforeAll
	public void setUp() throws Exception {
	}

	@AfterAll
	public void tearDown() throws Exception {
	}

	@Test
	public void constructor_test() throws Exception {
		final var fileService =
				FileAdapterFactory.getAdapter("src/test/resources/Files/RemoteFilesMapTests/TestCouncil1/");
		assertNotNull(fileService);
		final var remoteFilesMap = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-19T13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(
				fileService);
		final var files = remoteFilesMap.getFiles();
		assertNotNull(files);
		assertEquals(30, files.size());
		assertEquals(30, remoteFilesMap.size());
		assertEquals(1, remoteFilesMap.countType(FileType.SOFTWARE_UPDATE));

		final var notExpired = remoteFilesMap.getFilesNotExpired();
		assertNotNull(notExpired);
		assertTrue(files.size() >= notExpired.size());

		final List<RemoteFile> expired = new ArrayList<>();
		for (final RemoteFile file : files) {
			if (!notExpired.contains(file)) {
				expired.add(file);
			}
		}

		for (final RemoteFile remoteFile : expired) {
			assertTrue(remoteFile.isExpired());
		}

		final var badDir = new File("src/test/resources/Files/RemoteFilesMapTests/TestCouncil_fake/InputFiles");
		if (badDir.mkdirs()) {
			logger.info("Directory created");
		}
		final var fileService_fake = FileAdapterFactory.getAdapter(badDir.getParent());
		assertNotNull(fileService_fake);


		FileUtils.deleteDirectory(badDir);

		final var remoteFilesMap_fake = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-19T13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(
				fileService_fake);
		assertEquals(0, remoteFilesMap_fake.size());

		final var remoteFilesMap_badVersion = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(fileService);

		assertEquals(0, remoteFilesMap_badVersion.countType(FileType.SOFTWARE_UPDATE));

	}

	@Test
	public void utils_test() throws HederaClientException {
		final var fileService1 =
				FileAdapterFactory.getAdapter("src/test/resources/Files/RemoteFilesMapTests/TestCouncil1/");
		assert fileService1 != null;
		final var remoteFilesMap = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-19T13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(
				fileService1);

		// exists
		assertTrue(remoteFilesMap.exists("0-0-2_1678312256-0.tx"));
		assertFalse(remoteFilesMap.exists("dummyname.tx"));

		// remove
		assertTrue(remoteFilesMap.remove("0-0-2_1678312256-0.tx"));
		assertFalse(remoteFilesMap.remove("dummyname.tx"));
		assertFalse(remoteFilesMap.exists("0-0-2_1678312256-0.tx"));

		// add
		final var remoteFile = new RemoteFile(
				"src/test/resources/Files/RemoteFilesMapTests/TestCouncil1/InputFiles/0-0-2_1678312256-0.tx");
		assertTrue(remoteFile.isValid());
		remoteFilesMap.add(remoteFile);
		assertTrue(remoteFilesMap.exists("0-0-2_1678312256-0.tx"));

		// addAll
		final var fileService2 =
				FileAdapterFactory.getAdapter("src/test/resources/Files/RemoteFilesMapTests/TestCouncil2/");
		assert fileService2 != null;
		final var remoteFilesMap2 = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-19T13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(
				fileService2);
		remoteFilesMap.addAll(remoteFilesMap2);

		assertEquals(61, remoteFilesMap.size());

		final var fileService3 =
				FileAdapterFactory.getAdapter("src/test/resources/Files/RemoteFilesMapTests/TestCouncil1/");
		assert fileService3 != null;
		final var remoteFilesMap3 = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-19T13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(
				fileService3);
		remoteFilesMap.addAll(remoteFilesMap3);

		assertEquals(61, remoteFilesMap.size());

		assertEquals(16, remoteFilesMap2.getFiles(FileType.ACCOUNT_INFO).size());
		assertEquals(11, remoteFilesMap3.getFiles(FileType.PUBLIC_KEY).size());
		assertEquals(7, remoteFilesMap2.getFiles(FileType.COMMENT).size());
		assertEquals(7, remoteFilesMap2.getFiles(FileType.TRANSACTION).size());
		assertEquals(1, remoteFilesMap.getFiles(FileType.BATCH).size());
		assertEquals(1, remoteFilesMap.getFiles(FileType.LARGE_BINARY).size());
		assertEquals(1, remoteFilesMap.getFiles(FileType.METADATA).size());
		assertEquals(1, remoteFilesMap.getFiles(FileType.SOFTWARE_UPDATE).size());

		assertEquals(16, remoteFilesMap2.countType(FileType.ACCOUNT_INFO));
		assertEquals(11, remoteFilesMap3.countType(FileType.PUBLIC_KEY));
		assertEquals(7, remoteFilesMap2.countType(FileType.COMMENT));
		assertEquals(7, remoteFilesMap2.countType(FileType.TRANSACTION));
		assertEquals(1, remoteFilesMap.countType(FileType.BATCH));
		assertEquals(1, remoteFilesMap.countType(FileType.LARGE_BINARY));
		assertEquals(1, remoteFilesMap.countType(FileType.METADATA));
		assertEquals(1, remoteFilesMap.countType(FileType.SOFTWARE_UPDATE));
	}

	@Test
	public void addAllNotExpired_test() throws HederaClientException {
		final var fileService1 =
				FileAdapterFactory.getAdapter("src/test/resources/Files/RemoteFilesMapTests/TestCouncil1/");
		assert fileService1 != null;
		final var remoteFilesMap = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-19T13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(
				fileService1);

		assertTrue(remoteFilesMap.getFilesNotExpired().size() < remoteFilesMap.getFiles().size());

		final var fileService2 =
				FileAdapterFactory.getAdapter("src/test/resources/Files/RemoteFilesMapTests/TestCouncil2/");
		assert fileService2 != null;
		final var remoteFilesMap2 = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-19T13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(
				fileService2);
		//The files used in these tests were not expired at the time the test were created, but
		//since then, have expired. For now, we'll use the numbers it gives (25 not expired, 31 total)
		assertEquals(remoteFilesMap2.getFilesNotExpired().size(), 25);
		assertEquals(remoteFilesMap2.getFiles().size(), 31);

		//remoteFilesMap has 28 unexpired files
		remoteFilesMap2.addAllNotExpired(remoteFilesMap);
		assertEquals(remoteFilesMap2.getFilesNotExpired().size(), 53);
		assertEquals(remoteFilesMap2.getFiles().size(), 59);

	}
}
