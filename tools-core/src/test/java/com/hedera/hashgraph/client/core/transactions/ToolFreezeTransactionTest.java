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
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.FreezeTransaction;
import com.hedera.hashgraph.sdk.FreezeType;
import org.bouncycastle.util.encoders.Hex;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_FILE_HASH_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_FILE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_START_TIME_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FREEZE_TYPE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.REALM_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.SHARD_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolFreezeTransactionTest {

	@Test
	void buildExceptionsPart1_test() {
		final var testJson = getBasicJsonObject();
		final Exception exception0 =
				assertThrows(HederaClientException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client: Cannot validate input", exception0.getMessage());

		testJson.addProperty(FREEZE_TYPE_FIELD_NAME, "random");
		testJson.addProperty(FREEZE_START_TIME_FIELD_NAME, "random");
		final Exception exception1 =
				assertThrows(HederaClientException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client: Cannot validate input", exception1.getMessage());

		testJson.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_ONLY");
		testJson.remove(FREEZE_START_TIME_FIELD_NAME);
		final Exception exception2 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Start time must be specified", exception2.getMessage());

		testJson.add(FREEZE_START_TIME_FIELD_NAME, new Timestamp(Instant.now().minusSeconds(100)).asJSON());
		final Exception exception2b =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Start time cannot be in the past", exception2b.getMessage());

		testJson.addProperty(FREEZE_TYPE_FIELD_NAME, "PREPARE_UPGRADE");
		testJson.remove(FREEZE_START_TIME_FIELD_NAME);
		final Exception exception3 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: File ID must be specified", exception3.getMessage());

		testJson.add(FREEZE_FILE_ID_FIELD_NAME, new Identifier(0, 0, 123).asJSON());
		final Exception exception4 =
				assertThrows(HederaClientException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client: Cannot validate input", exception4.getMessage());

		testJson.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "");
		final Exception exception5 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Empty file hash", exception5.getMessage());

	}

	@Test
	void buildExceptionsPart2_test() {
		final var testJson = getBasicJsonObject();

		testJson.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_ONLY");
		final Exception exception0 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Start time must be specified", exception0.getMessage());

		testJson.add(FREEZE_START_TIME_FIELD_NAME, new Timestamp(Instant.now().minusSeconds(100)).asJSON());
		final Exception exception1 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Start time cannot be in the past", exception1.getMessage());

		testJson.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		testJson.add(FREEZE_START_TIME_FIELD_NAME, new Timestamp(Instant.now().plusSeconds(100)).asJSON());
		testJson.add(FREEZE_FILE_ID_FIELD_NAME, new Identifier(0, 0, 123).asJSON());
		final Exception exception2 =
				assertThrows(HederaClientException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client: Cannot validate input", exception2.getMessage());

		testJson.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "");
		final Exception exception3 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Empty file hash", exception3.getMessage());

		testJson.addProperty(FREEZE_TYPE_FIELD_NAME, "TELEMETRY_UPGRADE");
		testJson.remove(FREEZE_START_TIME_FIELD_NAME);
		testJson.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");

		final Exception exception4 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Start time must be specified", exception4.getMessage());

		testJson.add(FREEZE_START_TIME_FIELD_NAME, new Timestamp(Instant.now().minusSeconds(100)).asJSON());
		final Exception exception5 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Start time cannot be in the past", exception5.getMessage());

		testJson.add(FREEZE_START_TIME_FIELD_NAME, new Timestamp(Instant.now().plusSeconds(100)).asJSON());
		testJson.add(FREEZE_FILE_ID_FIELD_NAME, new Identifier(0, 0, 123).asJSON());
		testJson.remove(FREEZE_FILE_HASH_FIELD_NAME);
		final Exception exception6 =
				assertThrows(HederaClientException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client: Cannot validate input", exception6.getMessage());

		testJson.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "");
		final Exception exception7 =
				assertThrows(HederaClientRuntimeException.class, () -> new ToolFreezeTransaction(testJson));
		assertEquals("Hedera Client Runtime: Empty file hash", exception7.getMessage());


	}

	@Test
	void buildFromFile_test() throws HederaClientException, IOException {
		final var startTime = new Timestamp(Instant.now().plusSeconds(100));
		final var fileIdentifier = new Identifier(0, 0, 123);

		final var input = getBasicJsonObject();
		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_ONLY");
		input.add(FREEZE_START_TIME_FIELD_NAME, startTime.asJSON());

		final var freezeOnly = new ToolFreezeTransaction(input);
		final var fileFreezeOnlyFile = freezeOnly.store("src/test/resources");
		final var freezeOnlyFromFile = new ToolFreezeTransaction(new File(fileFreezeOnlyFile));
		assertEquals(freezeOnly, freezeOnlyFromFile);
		Files.deleteIfExists(Path.of(fileFreezeOnlyFile));

		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_ABORT");
		input.remove(FREEZE_START_TIME_FIELD_NAME);

		final var abort = new ToolFreezeTransaction(input);
		final var abortFile = abort.store("src/test/resources");
		final var abortFromFile = new ToolFreezeTransaction(new File(abortFile));
		assertEquals(abort, abortFromFile);
		Files.deleteIfExists(Path.of(abortFile));

		input.addProperty(FREEZE_TYPE_FIELD_NAME, "PREPARE_UPGRADE");
		input.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");

		final var prepareUpgrade = new ToolFreezeTransaction(input);
		final var prepareUpgradeFile = prepareUpgrade.store("src/test/resources");
		final var prepareUpgradeFromFile = new ToolFreezeTransaction(new File(prepareUpgradeFile));
		assertEquals(prepareUpgrade, prepareUpgradeFromFile);
		Files.deleteIfExists(Path.of(prepareUpgradeFile));

		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		input.add(FREEZE_START_TIME_FIELD_NAME, startTime.asJSON());

		final var freezeUpgrade = new ToolFreezeTransaction(input);
		final var freezeUpgradeFile = freezeUpgrade.store("src/test/resources");
		final var freezeUpgradeFromFile = new ToolFreezeTransaction(new File(freezeUpgradeFile));
		assertEquals(freezeUpgrade, freezeUpgradeFromFile);
		Files.deleteIfExists(Path.of(freezeUpgradeFile));

		input.addProperty(FREEZE_TYPE_FIELD_NAME, "TELEMETRY_UPGRADE");
		final var telemetryUpgrade = new ToolFreezeTransaction(input);
		final var telemetryUpgradeFile = telemetryUpgrade.store("src/test/resources");
		final var telemetryUpgradeFromFile = new ToolFreezeTransaction(new File(telemetryUpgradeFile));
		assertEquals(telemetryUpgrade, telemetryUpgradeFromFile);
		Files.deleteIfExists(Path.of(telemetryUpgradeFile));
	}

	@Test
	void buildFreezeOnly_test() throws HederaClientException {
		final var startFreeze = new Timestamp(Instant.now().plusSeconds(100));

		final var inputFreezeOnly = getBasicJsonObject();
		inputFreezeOnly.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_ONLY");
		inputFreezeOnly.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());

		final var freezeOnly = new ToolFreezeTransaction(inputFreezeOnly);
		assertEquals(FreezeType.FREEZE_ONLY, freezeOnly.getFreezeType());
		assertEquals(startFreeze, freezeOnly.getStartTime());
		final var freezeOnlyTransaction = (FreezeTransaction) freezeOnly.getTransaction();
		assertEquals(FreezeType.FREEZE_ONLY, freezeOnlyTransaction.getFreezeType());
		assertEquals(startFreeze.asInstant(), freezeOnlyTransaction.getStartTime());
	}

	@Test
	void buildFreezeAbort_test() throws HederaClientException {
		// Not clear if freeze abort needs a start time
		final var inputFreezeAbort = getBasicJsonObject();
		inputFreezeAbort.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_ABORT");

		final var freezeAbort = new ToolFreezeTransaction(inputFreezeAbort);
		assertEquals(FreezeType.FREEZE_ABORT, freezeAbort.getFreezeType());
		final var freezeAbortTransaction = (FreezeTransaction) freezeAbort.getTransaction();
		assertEquals(FreezeType.FREEZE_ABORT, freezeAbortTransaction.getFreezeType());
	}

	@Test
	void buildPrepareUpgrade() throws HederaClientException {
		final var fileIdentifier = new Identifier(0, 0, 123, "mainnet");

		final var inputPrepareUpgrade = getBasicJsonObject();
		inputPrepareUpgrade.addProperty(FREEZE_TYPE_FIELD_NAME, "PREPARE_UPGRADE");
		inputPrepareUpgrade.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		inputPrepareUpgrade.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");

		final var prepareUpgrade = new ToolFreezeTransaction(inputPrepareUpgrade);
		assertEquals(FreezeType.PREPARE_UPGRADE, prepareUpgrade.getFreezeType());
		assertEquals(fileIdentifier, prepareUpgrade.getFileID());
		assertEquals("123abc", prepareUpgrade.getFileHash());

		final var prepareUpgradeTransaction = (FreezeTransaction) prepareUpgrade.getTransaction();
		assertEquals(FreezeType.PREPARE_UPGRADE, prepareUpgradeTransaction.getFreezeType());
		assertEquals(fileIdentifier.asFile(), prepareUpgradeTransaction.getFileId());
		assertArrayEquals(Hex.decode("123abc"), prepareUpgradeTransaction.getFileHash());
	}

	@Test
	void buildFreezeUpgrade_test() throws HederaClientException {
		final var fileIdentifier = new Identifier(0, 0, 123, "MAINNET");
		final var startFreeze = new Timestamp(Instant.now().plusSeconds(100));

		final var inputFreezeUpgrade = getBasicJsonObject();
		inputFreezeUpgrade.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		inputFreezeUpgrade.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		inputFreezeUpgrade.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");
		inputFreezeUpgrade.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());

		final var freezeUpgrade = new ToolFreezeTransaction(inputFreezeUpgrade);
		assertEquals(FreezeType.FREEZE_UPGRADE, freezeUpgrade.getFreezeType());
		assertEquals(fileIdentifier, freezeUpgrade.getFileID());
		assertEquals("123abc", freezeUpgrade.getFileHash());
		assertEquals(startFreeze, freezeUpgrade.getStartTime());

		final var freezeUpgradeTransaction = (FreezeTransaction) freezeUpgrade.getTransaction();
		assertEquals(FreezeType.FREEZE_UPGRADE, freezeUpgradeTransaction.getFreezeType());
		assertEquals(fileIdentifier.asFile(), freezeUpgradeTransaction.getFileId());
		assertArrayEquals(Hex.decode("123abc"), freezeUpgradeTransaction.getFileHash());
		assertEquals(startFreeze.asInstant(), freezeUpgradeTransaction.getStartTime());
	}

	@Test
	void buildTelemetryUpgrade_test() throws HederaClientException {
		final var fileIdentifier = new Identifier(0, 0, 123, "Mainnet");
		final var startFreeze = new Timestamp(Instant.now().plusSeconds(100));

		final var inputTelemetryUpgrade = getBasicJsonObject();
		inputTelemetryUpgrade.addProperty(FREEZE_TYPE_FIELD_NAME, "TELEMETRY_UPGRADE");
		inputTelemetryUpgrade.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		inputTelemetryUpgrade.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");
		inputTelemetryUpgrade.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());

		final var telemetryUpgrade = new ToolFreezeTransaction(inputTelemetryUpgrade);
		assertEquals(FreezeType.TELEMETRY_UPGRADE, telemetryUpgrade.getFreezeType());
		assertEquals(fileIdentifier, telemetryUpgrade.getFileID());
		assertEquals("123abc", telemetryUpgrade.getFileHash());
		assertEquals(startFreeze, telemetryUpgrade.getStartTime());

		final var freezeUpgradeTransaction = (FreezeTransaction) telemetryUpgrade.getTransaction();
		assertEquals(FreezeType.TELEMETRY_UPGRADE, freezeUpgradeTransaction.getFreezeType());
		assertEquals(fileIdentifier.asFile(), freezeUpgradeTransaction.getFileId());
		assertArrayEquals(Hex.decode("123abc"), freezeUpgradeTransaction.getFileHash());
		assertEquals(startFreeze.asInstant(), freezeUpgradeTransaction.getStartTime());

	}

	@Test
	void getSigningAccounts_test() throws HederaClientException {
		final var fileIdentifier = new Identifier(0, 0, 123);
		final var startFreeze = new Timestamp(Instant.now().plusSeconds(100));

		final var input = getBasicJsonObject();
		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		input.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());
		input.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");

		final var transaction = new ToolFreezeTransaction(input);
		final var signers = transaction.getSigningAccounts();

		assertTrue(signers.contains(new AccountId(0, 0, 50)));
	}

	@Test
	void asJson_test() throws HederaClientException {
		final var fileIdentifier = new Identifier(0, 0, 123, "mainNet");
		final var startFreeze = new Timestamp(Instant.now().plusSeconds(100));

		final var input = getBasicJsonObject();
		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		input.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());
		input.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");

		final var prepareUpgrade = new ToolFreezeTransaction(input);
		final var output = prepareUpgrade.asJson();
		assertTrue(output.has(FREEZE_TYPE_FIELD_NAME));
		assertEquals("FREEZE_UPGRADE", output.get(FREEZE_TYPE_FIELD_NAME).getAsString());
		assertTrue(output.has(FREEZE_FILE_ID_FIELD_NAME));
		assertEquals(fileIdentifier.asJSON(), output.get(FREEZE_FILE_ID_FIELD_NAME).getAsJsonObject());
		assertTrue(output.has(FREEZE_FILE_HASH_FIELD_NAME));
		assertEquals("123abc", output.get(FREEZE_FILE_HASH_FIELD_NAME).getAsString());
		assertTrue(output.has(FREEZE_START_TIME_FIELD_NAME));
		assertEquals(startFreeze.asJSON(), output.get(FREEZE_START_TIME_FIELD_NAME).getAsJsonObject());

	}

	@Test
	void testEquals_test() throws HederaClientException {
		final var fileIdentifier = new Identifier(0, 0, 123);
		final var startFreeze = new Timestamp(Instant.now().plusSeconds(100));

		final var input = getBasicJsonObject();
		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		input.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());
		input.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");

		final var transaction0 = new ToolFreezeTransaction(input);
		final var other = new ToolFreezeTransaction(input);
		assertEquals(transaction0, other);

		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_ABORT");
		final var differentOP = new ToolFreezeTransaction(input);
		assertNotEquals(transaction0, differentOP);

		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		input.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.plusSeconds(1).asJSON());
		final var differentStart = new ToolFreezeTransaction(input);
		assertNotEquals(transaction0, differentStart);

		input.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());
		input.add(FREEZE_FILE_ID_FIELD_NAME, new Identifier(0, 0, 122).asJSON());
		final var differentFile = new ToolFreezeTransaction(input);
		assertNotEquals(transaction0, differentFile);

		input.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abd");
		final var differentHash = new ToolFreezeTransaction(input);
		assertNotEquals(transaction0, differentHash);
	}

	@Test
	void hashCode_test() throws HederaClientException {
		final var fileIdentifier = new Identifier(0, 0, 123);
		final var startFreeze = new Timestamp(1761418184, 123);
		final var validStart = new Timestamp(1761418084, 123);

		final var input = getBasicJsonObject();
		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		input.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());
		input.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abc");
		input.add(TRANSACTION_VALID_START_FIELD_NAME, validStart.asJSON());

		final var transaction0 = new ToolFreezeTransaction(input);
		final var other = new ToolFreezeTransaction(input);
		assertEquals(transaction0.hashCode(), other.hashCode());

		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_ABORT");
		final var differentOP = new ToolFreezeTransaction(input);
		assertEquals(2034111097, differentOP.hashCode());

		input.addProperty(FREEZE_TYPE_FIELD_NAME, "FREEZE_UPGRADE");
		input.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.plusSeconds(1).asJSON());
		final var differentStart = new ToolFreezeTransaction(input);
		assertEquals(-1656949329, differentStart.hashCode());

		input.add(FREEZE_START_TIME_FIELD_NAME, startFreeze.asJSON());
		input.add(FREEZE_FILE_ID_FIELD_NAME, new Identifier(0, 0, 122).asJSON());
		final var differentFile = new ToolFreezeTransaction(input);
		assertEquals(-1639517521, differentFile.hashCode());

		input.add(FREEZE_FILE_ID_FIELD_NAME, fileIdentifier.asJSON());
		input.addProperty(FREEZE_FILE_HASH_FIELD_NAME, "123abd");
		final var differentHash = new ToolFreezeTransaction(input);
		assertEquals(301134255, differentHash.hashCode());
	}

	@NotNull
	private JsonObject getBasicJsonObject() {
		final var testJson = new JsonObject();
		final var feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 100000000);

		final var feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, 50);

		testJson.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccount);

		final var node = new JsonObject();
		node.addProperty(REALM_NUMBER, 0);
		node.addProperty(SHARD_NUMBER, 0);
		node.addProperty(ACCOUNT_NUMBER, 3);

		testJson.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		testJson.add(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp().asJSON());

		testJson.add(NODE_ID_FIELD_NAME, node);
		testJson.addProperty(NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());

		testJson.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, 120);

		testJson.addProperty(MEMO_FIELD_NAME, "a memo to go with the transaction");
		return testJson;
	}


}
