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

package com.hedera.hashgraph.client.cli.options;

import com.hedera.hashgraph.client.cli.ToolsMain;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ConvertToKeyCommandTest implements GenericFileReadWriteAware {

	@Test
	void happyPath_test() throws Exception {
		final String[] args = { "convert-key", "-k", "src/test/resources/Keys/jsonKey.json", "-p" };
		ToolsMain.main(args);

		assertTrue(new File("src/test/resources/Keys/jsonKey.pem").exists());
		assertTrue(new File("src/test/resources/Keys/jsonKey.pub").exists());
	}

	@Test
	void badJson_test() {
		final String[] args0 = { "convert-key", "-k", "src/test/resources/Keys/badJson.json", "-p" };
		Exception e0 = assertThrows(HederaClientException.class, () -> ToolsMain.main(args0));
		assertEquals("Hedera Client: Cannot find key file src/test/resources/Keys/badJson.json", e0.getMessage());
		assertFalse(new File("src/test/resources/Keys/badJson.pem").exists());


		final String[] args1 = { "convert-key", "-k", "src/test/resources/Keys/badJsonKey.json", "-p" };
		Exception e1 = assertThrows(HederaClientException.class, () -> ToolsMain.main(args1));
		assertEquals("Hedera Client: Cannot find operator information", e1.getMessage());
		assertFalse(new File("src/test/resources/Keys/badJsonKey.pem").exists());

		final String[] args2 = { "convert-key", "-k", "src/test/resources/Keys/badJsonKey1.json", "-p" };
		Exception e2 = assertThrows(HederaClientException.class, () -> ToolsMain.main(args2));
		assertEquals("Hedera Client: Cannot find private key", e2.getMessage());
		assertFalse(new File("src/test/resources/Keys/badJsonKey1.pem").exists());

		final String[] args3 = { "convert-key", "-k", "src/test/resources/Keys/badJsonKey2.json", "-p" };
		Exception e3 = assertThrows(HederaClientException.class, () -> ToolsMain.main(args3));
		assertEquals("Hedera Client: Cannot find public key", e3.getMessage());
		assertFalse(new File("src/test/resources/Keys/badJsonKey1.pem").exists());

	}

	@AfterEach
	void tearDown() throws IOException {
		Files.deleteIfExists(Path.of("src/test/resources/Keys/jsonKey.pem"));
		Files.deleteIfExists(Path.of("src/test/resources/Keys/jsonKey.pub"));
	}
}