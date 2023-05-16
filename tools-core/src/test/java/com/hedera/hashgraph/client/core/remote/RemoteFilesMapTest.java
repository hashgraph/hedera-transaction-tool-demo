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

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.fileservices.FileAdapterFactory;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;


class RemoteFilesMapTest extends TestBase implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(RemoteFilesMapTest.class);

	@BeforeEach
	public void setUp() throws Exception {

	}

	@AfterEach
	public void tearDown() throws Exception {

	}

	@Test
	void constructor_test() throws Exception {
		final var fileService =
				FileAdapterFactory.getAdapter("src/test/resources/Files/RemoteFilesMapTests/TestCouncil1/");
		assertNotNull(fileService);
		final var remoteFilesMap = new RemoteFilesMap(
				"Version: 0.1.0-rc.1, UTC-BUILD-TIME: 2021-05-19T13:41:44+0000, COMMIT-ID: 8c817a2").fromFile(
				fileService);
		final var files = remoteFilesMap.getFiles();
		assertNotNull(files);
		// If dmg is used instead of pkg, then this should still expect 30 because a dmg
		// should be added to the resources (be sure to remove the pkg or it still breaks in
		// odd ways)
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
	void utils_test() throws HederaClientException {
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
	@Disabled("Need to recreate files as these have expired")
	void addAllNotExpired_test() throws HederaClientException {
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
		assertEquals(remoteFilesMap2.getFilesNotExpired().size(), remoteFilesMap2.getFiles().size());

		remoteFilesMap2.addAllNotExpired(remoteFilesMap);
		assertEquals(remoteFilesMap2.getFilesNotExpired().size(), remoteFilesMap2.getFiles().size());

	}
}
