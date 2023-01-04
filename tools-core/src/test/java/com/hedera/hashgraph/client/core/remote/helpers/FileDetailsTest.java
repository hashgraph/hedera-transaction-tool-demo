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

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class FileDetailsTest {

	@Test
	void parse_test() throws IOException, HederaClientException {
		final var badFile = new File("src/test/resources/Files/0.0.3.info");
		final Exception exception = assertThrows(HederaClientException.class, () -> FileDetails.parse(badFile));

		assertEquals("java.nio.file.NoSuchFileException: src/test/resources/Files/0.0.3.info", exception.getMessage());


		final var file = new File("src/test/resources/Files/0.0.2.info");
		final var fileDetails = FileDetails.parse(file);
		assertEquals("0.0.2", fileDetails.getBaseName());
		assertEquals("0.0.2.info", fileDetails.getName());
		assertEquals(file.getParent(), fileDetails.getPath());
		assertEquals("src/test/resources/Files/0.0.2.info", fileDetails.getFullPath());
		assertEquals("info", fileDetails.getExtension());
		final var attributes = Files.readAttributes(file.toPath(), BasicFileAttributes.class);
		assertEquals(attributes.creationTime(), fileDetails.getAttributes().creationTime());
		assertEquals(attributes.lastModifiedTime(), fileDetails.getAttributes().lastModifiedTime());
	}
}
