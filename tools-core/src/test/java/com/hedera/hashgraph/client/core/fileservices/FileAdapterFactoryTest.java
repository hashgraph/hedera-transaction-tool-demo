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

package com.hedera.hashgraph.client.core.fileservices;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.interfaces.FileService;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FileAdapterFactoryTest {
	private static final Logger logger = LogManager.getLogger(FileAdapterFactoryTest.class);

	@BeforeEach
	void setUp() {
		if (new File(System.getProperty("user.home"), "resources/Test").mkdirs()) {
			logger.info("Test resources directory created");
		}
	}

	@AfterEach
	void tearDown() throws IOException {
		FileUtils.deleteDirectory(new File(System.getProperty("user.home"), "resources/Test"));
	}

	@Test
	void getAdapterExceptions_Test() {
		final Exception exception0 = assertThrows(HederaClientException.class, () -> FileAdapterFactory.getAdapter(""));
		assertEquals("Hedera Client: Path cannot be null", exception0.getMessage());

		final Exception exception1 =
				assertThrows(HederaClientException.class, () -> FileAdapterFactory.getAdapter(null));
		assertEquals("Hedera Client: Path cannot be null", exception1.getMessage());
	}

	@Test
	void getAdapter_Test() throws HederaClientException {
		FileService adapter = FileAdapterFactory.getAdapter("test/nonExistent/path");
		assertNotNull(adapter);
		assertEquals("test/nonExistent/path", adapter.getPath());
		assertEquals("nonExistent", adapter.getName());
		assertFalse(adapter.exists());
		final File testResourcesDirectory = new File(System.getProperty("user.home"), "resources/Test");

		adapter = FileAdapterFactory.getAdapter(testResourcesDirectory.getAbsolutePath());
		assertNotNull(adapter);
		assertEquals(testResourcesDirectory.getPath(), adapter.getPath());
		assertEquals(FilenameUtils.getBaseName(testResourcesDirectory.getParent()), adapter.getName());
		assertTrue(adapter.exists());

	}


}