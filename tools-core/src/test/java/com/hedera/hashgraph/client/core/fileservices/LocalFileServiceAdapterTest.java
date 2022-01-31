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
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocalFileServiceAdapterTest {
	private final String TEST_PATH = "src/test/resources/Keys/";
	private final String TEST_PATH_RESOURCES = "src/test/resources/";

	@TempDir
	static Path sharedTempDir;

	@Test
	void listFiles() throws HederaClientException {
		final var fileArray = new File(TEST_PATH).listFiles();
		assert fileArray != null;

		final var localFileServiceAdapter = new LocalFileServiceAdapter(TEST_PATH);

		final var fileDetailsList = localFileServiceAdapter.listFiles();
		assertEquals(fileArray.length, fileDetailsList.size());

		final var fileList = localFileServiceAdapter.listFilePaths();
		assertEquals(fileArray.length, fileList.size());
		for (final String file : fileList) {
			assertTrue(new File(file).exists());
		}
	}

	@Test
	void listFiles_badFile() {
		assertThrows(HederaClientException.class, () -> {
			final var localFileServiceAdapter = new LocalFileServiceAdapter(TEST_PATH + "genesis.pem");
			final var x = localFileServiceAdapter.listFiles();
		});
	}

	@Test
	void upload_download_test() throws HederaClientException {
		final var localFileServiceAdapter2 = new LocalFileServiceAdapter(TEST_PATH_RESOURCES);
		final File testFile = new File(TEST_PATH_RESOURCES, "recovery.aes");
		localFileServiceAdapter2.upload(testFile.getAbsolutePath(), "Keys");

		final var localFileServiceAdapter = new LocalFileServiceAdapter(TEST_PATH);
		assertTrue(localFileServiceAdapter.exists("recovery.aes"));

		final File tempFile = localFileServiceAdapter.download("genesis.pub", sharedTempDir.toString());
		assertTrue(tempFile.exists());

		localFileServiceAdapter.rename("recovery.aes", "other.aes");
		final FileDetails other = localFileServiceAdapter.find("other.aes");
		assertTrue(new File(other.getFullPath()).exists());

	}

	@Test
	void exists_test() {
		final var localFileServiceAdapter = new LocalFileServiceAdapter(TEST_PATH);
		assertTrue(localFileServiceAdapter.exists());

		final var localFileServiceAdapter_bad = new LocalFileServiceAdapter(TEST_PATH + "bad");
		assertFalse(localFileServiceAdapter_bad.exists());
	}

	@AfterEach
	void tearDown() throws IOException {
		Files.deleteIfExists(new File(TEST_PATH, "other.aes").toPath());
	}
}