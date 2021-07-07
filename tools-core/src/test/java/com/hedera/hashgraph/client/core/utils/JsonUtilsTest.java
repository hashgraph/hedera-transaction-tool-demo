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

package com.hedera.hashgraph.client.core.utils;

import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
import org.junit.jupiter.api.Test;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.POSITIVE_NUMBER_CURRENCY_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JsonUtilsTest implements GenericFileReadWriteAware {

	@Test
	void accountInfoToJson() throws HederaClientException, InvalidProtocolBufferException {
		AccountInfo info = AccountInfo.fromBytes(readBytes("src/test/resources/Files/0.0.2.info"));
		assertNotNull(info);

		JsonObject infoJson = JsonUtils.accountInfoToJson(info);

		assertTrue(infoJson.has("accountId"));
		assertEquals(info.accountId.toString(), infoJson.get("accountId").getAsString());

		assertTrue(infoJson.has("contractAccountId"));
		assertEquals(info.contractAccountId, infoJson.get("contractAccountId").getAsString());

		assertTrue(infoJson.has("deleted"));
		assertEquals(info.isDeleted, infoJson.get("deleted").getAsBoolean());

		if (info.proxyAccountId != null) {
			assertTrue(infoJson.has("proxyAccountId"));
			assertEquals(info.proxyAccountId.toString(), infoJson.get("proxyAccountId").getAsString());
		}

		assertTrue(infoJson.has("proxyReceived"));
		assertEquals(String.valueOf(info.proxyReceived), infoJson.get("proxyReceived").getAsString());

		assertTrue(infoJson.has("key"));
		assertEquals(String.valueOf(info.key), infoJson.get("key").getAsString());

		assertTrue(infoJson.has("balance"));
		assertEquals(String.valueOf(info.balance), infoJson.get("balance").getAsString());

		assertTrue(infoJson.has("sendRecordThreshold"));
		assertEquals(String.valueOf(info.sendRecordThreshold), infoJson.get("sendRecordThreshold").getAsString());

		assertTrue(infoJson.has("receiveRecordThreshold"));
		assertEquals(String.valueOf(info.receiveRecordThreshold), infoJson.get("receiveRecordThreshold").getAsString());

		assertTrue(infoJson.has("receiverSignatureRequired"));
		assertEquals(info.isReceiverSignatureRequired, infoJson.get("receiverSignatureRequired").getAsBoolean());

		assertTrue(infoJson.has("expirationTime"));
		assertEquals(String.valueOf(info.expirationTime), infoJson.get("expirationTime").getAsString());

		assertTrue(infoJson.has("autoRenewPeriod"));
		assertEquals(new Timestamp(info.autoRenewPeriod).asJSON(), infoJson.get("autoRenewPeriod").getAsJsonObject());

		assertTrue(infoJson.has("liveHashes"));
		assertEquals(info.liveHashes.toString(), infoJson.get("liveHashes").getAsString());
	}

	@Test
	void hBarsToJsonObject() {
		var testBars0 = Hbar.fromTinybars(1234567890);
		var jsonBars0 = JsonUtils.hBarsToJsonObject(testBars0);

		assertTrue(jsonBars0.has(H_BARS));
		assertEquals(12, jsonBars0.get(H_BARS).getAsLong());

		assertTrue(jsonBars0.has(TINY_BARS));
		assertEquals(34567890, jsonBars0.get(TINY_BARS).getAsInt());

		var testBars1 = Hbar.fromTinybars(12345);
		var jsonBars1 = JsonUtils.hBarsToJsonObject(testBars1);

		assertTrue(jsonBars1.has(H_BARS));
		assertEquals(0, jsonBars1.get(H_BARS).getAsLong());

		assertTrue(jsonBars1.has(TINY_BARS));
		assertEquals(12345, jsonBars1.get(TINY_BARS).getAsInt());

		var testBars2 = Hbar.fromTinybars(-12345);
		var jsonBars2 = JsonUtils.hBarsToJsonObject(testBars2);

		assertTrue(jsonBars2.has(TINY_BARS));
		assertEquals(-12345, jsonBars2.get(TINY_BARS).getAsInt());

		assertThrows(NumberFormatException.class, () -> JsonUtils.jsonToHBars(jsonBars2));

		var jsonBars3 = new JsonObject();
		jsonBars3.addProperty(H_BARS, 0);
		jsonBars3.addProperty(TINY_BARS, 1234567890);

		Hbar testBars3 = JsonUtils.jsonToHBars(jsonBars3);
		JsonObject jsonBars31 = JsonUtils.hBarsToJsonObject(testBars3);
		assertTrue(jsonBars31.has(H_BARS));
		assertEquals(12, jsonBars31.get(H_BARS).getAsLong());

		assertTrue(jsonBars31.has(TINY_BARS));
		assertEquals(34567890, jsonBars31.get(TINY_BARS).getAsInt());

		JsonObject jsonBars4 = new JsonObject();
		jsonBars4.addProperty(H_BARS, 0);
		jsonBars4.addProperty(TINY_BARS, -1000000);

		Exception exception = assertThrows(HederaClientRuntimeException.class, () -> JsonUtils.jsonToHBars(jsonBars4));
		assertEquals(String.format("Hedera Client Runtime: %s", POSITIVE_NUMBER_CURRENCY_ERROR_MESSAGE),
				exception.getMessage());

		JsonObject jsonBars41 = new JsonObject();
		jsonBars41.addProperty(H_BARS, -10000);
		jsonBars41.addProperty(TINY_BARS, 1000000);

		Exception e = assertThrows(HederaClientRuntimeException.class, () -> JsonUtils.jsonToHBars(jsonBars41));
		assertEquals(String.format("Hedera Client Runtime: %s", POSITIVE_NUMBER_CURRENCY_ERROR_MESSAGE),
				e.getMessage());


	}
}