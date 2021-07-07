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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import org.apache.commons.io.FileUtils;
import org.junit.Test;

import java.io.File;
import java.nio.file.Files;

import static junit.framework.TestCase.assertNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class CommentFileTest extends TestBase {

	@Test
	public void buildComment_test() throws Exception {
		var comment1 = FileDetails.parse(new File("src/test/resources/Files/commentsFile.txt"));
		var file1 = new CommentFile(comment1);
		assertEquals("test1.council2@hederacouncil.org", file1.getContents().get("Author").getAsString());
		assertEquals("Contents of the comment", file1.getContents().get("Contents").getAsString());
		assertEquals("2021-04-19 16:51:20", file1.getContents().get("Timestamp").getAsString());

		assertEquals(comment1.getAttributes().creationTime().toMillis() / 1000, file1.getTimestamp().getSeconds());

		assertNull(file1.getLinkedFile());
		assertFalse(file1.hasLinkedFile());

		FileUtils.copyFile(new File("src/test/resources/Files/commentsFile.txt"),
				new File("src/test/resources/Files/commentsFile2.txt"));

		var comment2 = FileDetails.parse(new File("src/test/resources/Files/commentsFile2.txt"));
		Files.delete(new File("src/test/resources/Files/commentsFile2.txt").toPath());
		var file2 = new CommentFile(comment2);

		assertEquals(new JsonObject(), file2.getContents());

		var comment3 = FileDetails.parse(new File("src/test/resources/Files/badComment.txt"));
		var file3 = new CommentFile(comment3);

		assertEquals(new JsonObject(), file3.getContents());
	}


}
