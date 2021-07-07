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

package com.hedera.hashgraph.client.core.enums;

import com.hedera.hashgraph.client.core.constants.Constants;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class FileTypeTest {

	@Test
	void toString_test() {
		FileType type = FileType.TRANSACTION;
		assertEquals("Signing Transaction", type.toString());

		type = FileType.BATCH;
		assertEquals("Batch Transactions", type.toString());

		type = FileType.LARGE_BINARY;
		assertEquals("Large File Update", type.toString());

		type = FileType.SOFTWARE_UPDATE;
		assertEquals("SOFTWARE_UPDATE", type.toString());

		type = FileType.ACCOUNT_INFO;
		assertEquals("ACCOUNT_INFO", type.toString());

		type = FileType.PUBLIC_KEY;
		assertEquals("PUBLIC_KEY", type.toString());

		type = FileType.COMMENT;
		assertEquals("COMMENT", type.toString());

		type = FileType.CONFIG;
		assertEquals("CONFIG", type.toString());

		type = FileType.METADATA;
		assertEquals("METADATA", type.toString());

		type = FileType.UNKNOWN;
		assertEquals("UNKNOWN", type.toString());

	}


	@Test
	void toKind_test() {
		FileType type = FileType.TRANSACTION;
		assertEquals("Transaction", type.toKind());

		type = FileType.BATCH;
		assertEquals("Batch Transaction", type.toKind());

		type = FileType.LARGE_BINARY;
		assertEquals("File Contents Update", type.toKind());

		type = FileType.SOFTWARE_UPDATE;
		assertEquals("Software Update", type.toKind());

		type = FileType.ACCOUNT_INFO;
		assertEquals("Account Information", type.toKind());

		type = FileType.PUBLIC_KEY;
		assertEquals("Public Key", type.toKind());

		type = FileType.COMMENT;
		assertEquals("", type.toKind());

		type = FileType.CONFIG;
		assertEquals("", type.toKind());

		type = FileType.METADATA;
		assertEquals("", type.toKind());

		type = FileType.UNKNOWN;
		assertEquals("", type.toKind());
	}

	@Test
	void getExtension_test() {
		FileType type = FileType.TRANSACTION;
		assertEquals(Constants.TRANSACTION_EXTENSION, type.getExtension());

		type = FileType.BATCH;
		assertEquals(Constants.BATCH_TRANSACTION_EXTENSION, type.getExtension());

		type = FileType.LARGE_BINARY;
		assertEquals(Constants.LARGE_BINARY_EXTENSION, type.getExtension());

		type = FileType.SOFTWARE_UPDATE;
		assertEquals(Constants.SOFTWARE_UPDATE_EXTENSION, type.getExtension());

		type = FileType.ACCOUNT_INFO;
		assertEquals(Constants.INFO_EXTENSION, type.getExtension());

		type = FileType.PUBLIC_KEY;
		assertEquals(Constants.PUB_EXTENSION, type.getExtension());

		type = FileType.COMMENT;
		assertEquals(Constants.COMMENT_EXTENSION, type.getExtension());

		type = FileType.CONFIG;
		assertEquals(Constants.CONFIGURATION_EXTENSION, type.getExtension());

		type = FileType.METADATA;
		assertEquals(Constants.METADATA_EXTENSION, type.getExtension());

		type = FileType.UNKNOWN;
		assertEquals("", type.getExtension());

	}
}