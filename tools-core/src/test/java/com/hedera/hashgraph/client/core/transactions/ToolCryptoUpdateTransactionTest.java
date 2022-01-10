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

package com.hedera.hashgraph.client.core.transactions;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.JsonConstants;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.testHelpers.TestHelpers;
import com.hedera.hashgraph.sdk.AccountUpdateTransaction;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_TO_UPDATE;
import static com.hedera.hashgraph.client.core.json.Identifier.ACCOUNT_NUM;
import static java.nio.file.Files.deleteIfExists;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolCryptoUpdateTransactionTest implements GenericFileReadWriteAware {

	@BeforeEach
	void setUp() throws IOException {
		deleteIfExists(Path.of("src/test/resources/Files/TransactionFileTests/temp.tx"));
	}

	@AfterEach
	void tearDown() throws IOException {
		deleteIfExists(Path.of("src/test/resources/Files/TransactionFileTests/temp.tx"));
	}


	@Test
	void createUpdate_test() throws HederaClientException {
		final var startTime = new Timestamp(20).asInstant();
		final JsonObject jsonObject = TestHelpers.buildUpdateJson(2, startTime);
		jsonObject.addProperty(JsonConstants.AUTO_RENEW_PERIOD_FIELD_NAME, 1776000);
		final Exception exception0 =
				assertThrows(HederaClientException.class, () -> new ToolCryptoUpdateTransaction(jsonObject));
		assertEquals("Hedera Client: Cannot validate input", exception0.getMessage());

		jsonObject.addProperty(ACCOUNT_TO_UPDATE, 90);
		final Exception exception1 =
				assertThrows(HederaClientException.class, () -> new ToolCryptoUpdateTransaction(jsonObject));
		assertEquals("Hedera Client: Cannot validate input", exception1.getMessage());

		final JsonObject account = new JsonObject();
		account.addProperty(ACCOUNT_NUM, 78);
		jsonObject.add(ACCOUNT_TO_UPDATE, account);
		var transaction = new ToolCryptoUpdateTransaction(jsonObject);
		assertEquals(TransactionType.CRYPTO_UPDATE, transaction.transactionType);
		assertTrue(transaction.transaction instanceof AccountUpdateTransaction);
		assertEquals(startTime.getEpochSecond(),
				Objects.requireNonNull(transaction.getTransactionId().validStart).getEpochSecond());
		assertNull(((AccountUpdateTransaction) transaction.transaction).getKey());

		final JsonObject key = readJsonObject(new File("src/test/resources/Keys/sampleJsonKey"));
		jsonObject.add(JsonConstants.NEW_KEY_FIELD_NAME, key);
		transaction = new ToolCryptoUpdateTransaction(jsonObject);
		assertNotNull(((AccountUpdateTransaction) transaction.transaction).getKey());
	}

}