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

import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MetadataActionTest {

	@Test
	void getAction_test() throws HederaClientException {
		final var line = "{\"timestamp\":{\"seconds\":1610038031,\"nanos\":56883000},\"action\":\"ACCEPT\"," +
				"\"location\":\"\\/Hedera Council\\/Transactions - Documents\"," +
				"\"userComments\":\"user comments here\",\"keyName\":\"keyStore.pem\"}";

		final var actionLine = new MetadataAction(line);

		final var actionJson = actionLine.toJson();
		assertTrue(actionJson.has("timestamp"));
		assertEquals(1610038031L, actionJson.get("timestamp").getAsJsonObject().get("seconds").getAsLong());
		assertEquals(1610038031L, actionLine.getTimeStamp().getSeconds());
		assertEquals(0, actionJson.get("timestamp").getAsJsonObject().get("nanos").getAsLong());
		assertEquals(0, actionLine.getTimeStamp().getNanos());


		assertTrue(actionJson.has("userComments"));
		assertEquals("user comments here", actionJson.get("userComments").getAsString());
		assertEquals("user comments here", actionLine.getUserComments());

		assertTrue(actionJson.has("action"));
		assertEquals("ACCEPT", actionJson.get("action").getAsString());
		assertEquals(Actions.ACCEPT, actionLine.getActions());

		assertTrue(actionJson.has("keyName"));
		assertEquals("keyStore.pem", actionJson.get("keyName").getAsString());
		assertEquals("keyStore.pem", actionLine.getKeyName());


		assertEquals("{\"timestamp\":{\"seconds\":1610038031,\"nanos\":0},\"action\":\"ACCEPT\"," +
				"\"userComments\":\"user comments here\",\"keyName\":\"keyStore.pem\"}", actionLine.toString());

		Exception exception =
				assertThrows(HederaClientException.class, () -> new MetadataAction("iuhdfgiufhdifughidufhgiudfhiguh"));
		assertEquals("Hedera Client: Cannot parse object", exception.getMessage());

		final var badLine0 = "{\"action\":\"ACCEPT\"," +
				"\"location\":\"\\/Hedera Council\\/Transactions - Documents\"," +
				"\"userComments\":\"user comments here\",\"keyName\":\"keyStore.pem\"}";
		exception =
				assertThrows(HederaClientException.class, () -> new MetadataAction(badLine0));
		assertEquals("Hedera Client: Invalid metadata", exception.getMessage());

		final var badLine1 =
				"{\"timestamp\":{\"seconds\":1610038031,\"nanos\":56883000}, \"location\":\"\\/Hedera " +
						"Council\\/Transactions - Documents\", \"userComments\":\"user comments here\"," +
						"\"keyName\":\"keyStore.pem\"}";

		exception =
				assertThrows(HederaClientException.class, () -> new MetadataAction(badLine1));
		assertEquals("Hedera Client: Invalid metadata", exception.getMessage());

	}

	@Test
	void compare_test() {
		final var action0 =
				new MetadataAction(new Timestamp(1610038031, 0), Actions.DECLINE, "user comments", "someKey");

		assertEquals(0, action0.compareTo(
				new MetadataAction(new Timestamp(1610038031L, 0), Actions.DECLINE, "user comments", "someKey")));
		assertEquals(0, action0.compareTo(
				new MetadataAction(new Timestamp(1610038031L, 1000), Actions.DECLINE, "user comments", "someKey")));
		assertTrue(action0.compareTo(
				new MetadataAction(new Timestamp(2610038031L, 0), Actions.DECLINE, "user comments", "someKey")) < 0);
		assertTrue(action0.compareTo(new MetadataAction(new Timestamp(1610038031L, 0), Actions.DECLINE, "user comments",
				"someOtherKey")) < 0);
		assertEquals(0, action0.compareTo(action0));
	}

}