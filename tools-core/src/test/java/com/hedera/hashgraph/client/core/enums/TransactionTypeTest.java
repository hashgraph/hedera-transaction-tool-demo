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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class TransactionTypeTest {

	@Test
	void testToString() {
		TransactionType type = TransactionType.CRYPTO_TRANSFER;
		assertEquals("Transfer Transaction", type.toString());

		type = TransactionType.CRYPTO_CREATE;
		assertEquals("Create New Account Transaction", type.toString());

		type = TransactionType.CRYPTO_UPDATE;
		assertEquals("Update Account Transaction", type.toString());

		type = TransactionType.SYSTEM_DELETE_UNDELETE;
		assertEquals("Content Transaction", type.toString());

		type = TransactionType.FILE_UPDATE;
		assertEquals("File Update Transaction", type.toString());

		type = TransactionType.FILE_APPEND;
		assertEquals("File Append Transaction", type.toString());
	}
}