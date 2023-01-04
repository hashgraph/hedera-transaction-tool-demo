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

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.TestBase;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

import static com.hedera.hashgraph.client.core.testHelpers.TestHelpers.getJsonInputCT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Disabled("Temporarily disabling tests")
public class RemoteFileTest extends TestBase implements GenericFileReadWriteAware {

	int sender = 2;
	int receiver = 50;
	String filename;

	@BeforeEach
	public void setUp() throws Exception {
		final var testJson = getJsonInputCT(50, sender, receiver, new Timestamp(20).asInstant());
		writeJsonObject("src/test/resources/Files/testJson.json", testJson);
		final ToolTransaction transaction = new ToolTransferTransaction(testJson);
		filename = transaction.store("src/test/resources/Files/testTransfer.tx");
	}

	@AfterEach
	public void tearDown() throws Exception {
		Files.deleteIfExists(Path.of("src/test/resources/Files/testJson.json"));
		Files.deleteIfExists(Path.of("src/test/resources/Files/testTransfer.tx"));
	}

	@Test
	public void constructor_test() throws HederaClientException {
		final var remoteFile = new RemoteFile(filename);
		assertNotNull(remoteFile);
		assertTrue(remoteFile.isValid());
		assertFalse(remoteFile.isExpired());
		assertFalse(remoteFile.isHistory());
	}

	@Test
	public void getters_test() throws HederaClientException, IOException {
		final var remoteFile = new RemoteFile(filename);
		assertEquals("src/test/resources/Files/testTransfer.tx", remoteFile.getPath());
		assertEquals("testTransfer.tx", remoteFile.getName());
		assertEquals(FileType.TRANSACTION, remoteFile.getType());
		assertEquals("testTransfer", remoteFile.getBaseName());
		final var file = new File(filename);
		final var info = FileDetails.parse(file);

		assertEquals(info.getAttributes().lastModifiedTime().to(TimeUnit.SECONDS), remoteFile.getDate());
	}
}
