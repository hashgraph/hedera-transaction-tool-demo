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

package com.hedera.hashgraph.client.core.json;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.ContractId;
import com.hedera.hashgraph.sdk.FileId;
import com.hedera.hashgraph.sdk.proto.AccountID;
import com.hedera.hashgraph.sdk.proto.ContractID;
import com.hedera.hashgraph.sdk.proto.FileID;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class IdentifierTest {

	@Test
	void parse_Test() throws HederaClientException {
		// Parse string
		Identifier identifierFromString1 = Identifier.parse("0.2.56");
		assertEquals(0L, identifierFromString1.getShardNum());
		assertEquals(2L, identifierFromString1.getRealmNum());
		assertEquals(56L, identifierFromString1.getAccountNum());

		Identifier identifierFromString2 = Identifier.parse("5665");
		assertEquals(0L, identifierFromString2.getRealmNum());
		assertEquals(0L, identifierFromString2.getShardNum());
		assertEquals(5665L, identifierFromString2.getAccountNum());


		HederaClientRuntimeException exception0 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse(""));
		assertEquals("Hedera Client Runtime: The provided string was null or empty", exception0.getMessage());

		HederaClientRuntimeException exception1 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse("notANumber"));
		assertEquals("java.lang.NumberFormatException: For input string: \"notANumber\"", exception1.getMessage());

		HederaClientRuntimeException exception2 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse(".5655"));
		assertEquals("Hedera Client Runtime: .5655 cannot be parsed as an account ID", exception2.getMessage());

		HederaClientRuntimeException exception3 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse("0.notANumber.23"));
		assertEquals("java.lang.NumberFormatException: For input string: \"notANumber\"", exception3.getMessage());


		// Parse json
		JsonObject accountJson1 = new JsonObject();
		accountJson1.addProperty("realmNum", 789L);
		accountJson1.addProperty("shardNum", 98498L);
		accountJson1.addProperty("accountNum", 5131584984613L);

		Identifier identifierFromJson1 = Identifier.parse(accountJson1);
		assertEquals(789L, identifierFromJson1.getShardNum());
		assertEquals(98498L, identifierFromJson1.getRealmNum());
		assertEquals(5131584984613L, identifierFromJson1.getAccountNum());

		JsonObject accountJson2 = new JsonObject();
		accountJson2.addProperty("accountNum", 98984L);

		Identifier identifierFromJson2 = Identifier.parse(accountJson2);
		assertEquals(0L, identifierFromJson2.getShardNum());
		assertEquals(0L, identifierFromJson2.getRealmNum());
		assertEquals(98984L, identifierFromJson2.getAccountNum());

		JsonObject accountJson4 = new JsonObject();
		accountJson4.addProperty("realmNum", -789L);
		accountJson4.addProperty("shardNum", 98498L);
		accountJson4.addProperty("accountNum", 5131584984613L);

		HederaClientException exception4 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson4));
		assertEquals("Hedera Client: Invalid realm number", exception4.getMessage());

		JsonObject accountJson5 = new JsonObject();
		accountJson5.addProperty("realmNum", 789L);
		accountJson5.addProperty("shardNum", -98498L);
		accountJson5.addProperty("accountNum", 5131584984613L);

		HederaClientException exception5 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson5));
		assertEquals("Hedera Client: Invalid shard number", exception5.getMessage());

		JsonObject accountJson6 = new JsonObject();
		accountJson6.addProperty("realmNum", 789L);
		accountJson6.addProperty("shardNum", 98498L);
		accountJson6.addProperty("accountNum", -5131584984613L);

		HederaClientException exception6 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson6));
		assertEquals("Hedera Client: Invalid account number", exception6.getMessage());

		JsonObject accountJson7 = new JsonObject();
		accountJson7.addProperty("realmNum", 789L);
		accountJson7.addProperty("shardNum", 98498L);
		accountJson7.addProperty("fileNum", -5131584984613L);

		HederaClientException exception7 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson7));
		assertEquals("Hedera Client: Invalid file number", exception7.getMessage());

		JsonObject accountJson8 = new JsonObject();
		accountJson8.addProperty("realmNum", 789L);
		accountJson8.addProperty("shardNum", 98498L);
		accountJson8.addProperty("contractNum", -5131584984613L);

		HederaClientException exception8 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson8));
		assertEquals("Hedera Client: Invalid contract number", exception8.getMessage());

		JsonObject accountJson9 = new JsonObject();
		accountJson9.addProperty("fileNum", "notANumber");

		HederaClientException exception9 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson9));
		assertEquals("java.lang.NumberFormatException: For input string: \"notANumber\"", exception9.getMessage());

		JsonObject accountJson10 = new JsonObject();
		accountJson10.addProperty("someProperty", "notANumber");

		HederaClientException exception10 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson10));
		assertEquals("Hedera Client: Invalid json object", exception10.getMessage());
	}

	@Test
	void constructors_Test() {
		AccountId accountId = new AccountId(0, 1, 2);
		Identifier identifierFromAccountId = new Identifier(accountId);
		assertEquals(0L, identifierFromAccountId.getShardNum());
		assertEquals(1L, identifierFromAccountId.getRealmNum());
		assertEquals(2L, identifierFromAccountId.getAccountNum());

		assertEquals(accountId, identifierFromAccountId.asAccount());

		AccountID accountID = AccountID.newBuilder().setShardNum(0).setRealmNum(1).setAccountNum(2).build();
		Identifier identifierFromAccountID = new Identifier(accountID);
		assertEquals(0L, identifierFromAccountID.getShardNum());
		assertEquals(1L, identifierFromAccountID.getRealmNum());
		assertEquals(2L, identifierFromAccountID.getAccountNum());

		FileId fileId = new FileId(4, 5, 6);
		Identifier identifierFromFileId = new Identifier(fileId);
		assertEquals(4L, identifierFromFileId.getShardNum());
		assertEquals(5L, identifierFromFileId.getRealmNum());
		assertEquals(6L, identifierFromFileId.getAccountNum());

		assertEquals(fileId, identifierFromFileId.asFile());

		FileID fileID = FileID.newBuilder().setShardNum(0).setRealmNum(1).setFileNum(2).build();
		Identifier identifierFromFileID = new Identifier(fileID);
		assertEquals(0L, identifierFromFileID.getShardNum());
		assertEquals(1L, identifierFromFileID.getRealmNum());
		assertEquals(2L, identifierFromFileID.getAccountNum());

		ContractId contractId = new ContractId(4, 5, 6);
		Identifier identifierFromContractId = new Identifier(contractId);
		assertEquals(4L, identifierFromContractId.getShardNum());
		assertEquals(5L, identifierFromContractId.getRealmNum());
		assertEquals(6L, identifierFromContractId.getAccountNum());

		assertEquals(contractId, identifierFromContractId.asContract());

		ContractID contractID = ContractID.newBuilder().setShardNum(0).setRealmNum(1).setContractNum(2).build();
		Identifier identifierFromContractID = new Identifier(contractID);
		assertEquals(0L, identifierFromContractID.getShardNum());
		assertEquals(1L, identifierFromContractID.getRealmNum());
		assertEquals(2L, identifierFromContractID.getAccountNum());
	}

	@Test
	void isValid_Test() {
		Identifier identifier = new Identifier();
		identifier.setAccountNum(0);
		identifier.setRealmNum(0);
		identifier.setShardNum(0);
		assertFalse(identifier.isValid());
	}

	@Test
	void toString_Test() {
		Identifier identifier = new Identifier(2,6,9);
		assertEquals("{\"realmNum\":6,\"shardNum\":2,\"accountNum\":9}", identifier.toString());
		assertEquals("2.6.9", identifier.toReadableString());
	}

	@Test
	void asJSON() {
		JsonObject accountJson = new JsonObject();
		accountJson.addProperty("shardNum", 5);
		accountJson.addProperty("realmNum", 7);
		accountJson.addProperty("accountNum", 13);

		Identifier identifier = new Identifier(5,7,13);
		assertEquals(accountJson, identifier.asJSON());
	}

	@Test
	void equals_test(){
		Identifier id1 = new Identifier(1,2,3);
		Identifier id2 = new Identifier(1,2,3);
		Identifier id3 = new Identifier(3,6,9);

		assertTrue(id1.equals(id2));
		assertTrue(id1.equals(id1));
		assertFalse(id1.equals(id3));

		assertFalse(id1.equals("1.2.3"));
		assertFalse(id1.equals(null));
	}

	@Test
	void hash_test(){
		Identifier id2 = new Identifier(1,2,3);
		assertEquals(30817, id2.hashCode());
	}
}