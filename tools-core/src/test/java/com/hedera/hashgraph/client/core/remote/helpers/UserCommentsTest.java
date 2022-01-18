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

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;

import static org.junit.jupiter.api.Assertions.assertEquals;

class UserCommentsTest {

	@Test
	void parse_test() throws FileNotFoundException {
		final var comments = new File("src/test/resources/Files/commentsFile.txt");
		final var parsed = new UserComments().parse(comments);
		assertEquals("test1.council2@hederacouncil.org", parsed.getAuthor());
		assertEquals("Contents of the comment", parsed.getComment());
		assertEquals("2021-04-19 16:51:20", parsed.getTimestamp());

		assertEquals("{\"Author\":\"test1.council2@hederacouncil.org\",\"Contents\":\"Contents of the comment\"," +
				"\"Timestamp\":\"2021-04-19 16:51:20\"}", parsed.toString());

		parsed.toFile("src/test/resources/Files/commentsFile_new.txt");

		final var parsed_new = new UserComments().parse(new File("src/test/resources/Files/commentsFile_new.txt"));
		assertEquals("test1.council2@hederacouncil.org", parsed_new.getAuthor());
		assertEquals("Contents of the comment", parsed_new.getComment());
		assertEquals("2021-04-19 16:51:20", parsed_new.getTimestamp());

		assertEquals("{\"Author\":\"test1.council2@hederacouncil.org\",\"Contents\":\"Contents of the comment\"," +
				"\"Timestamp\":\"2021-04-19 16:51:20\"}", parsed_new.toString());
	}

	@AfterEach
	void tearDown() throws IOException {
		Files.deleteIfExists(new File("src/test/resources/Files/commentsFile_new.txt").toPath());
	}
}