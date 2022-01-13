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
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class IdentifierTest {

	@Test
	void parse_Test() {
		// Parse string
		final var identifierFromString1 = Identifier.parse("0.2.56");
		assertEquals(0L, identifierFromString1.getShardNum());
		assertEquals(2L, identifierFromString1.getRealmNum());
		assertEquals(56L, identifierFromString1.getAccountNum());

		final var identifierFromString2 = Identifier.parse("5665");
		assertEquals(0L, identifierFromString2.getRealmNum());
		assertEquals(0L, identifierFromString2.getShardNum());
		assertEquals(5665L, identifierFromString2.getAccountNum());


		final var exception0 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse(""));
		assertEquals("Hedera Client Runtime: The provided string was null or empty", exception0.getMessage());

		final var exception1 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse("notANumber"));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"notANumber\" cannot be parsed",
				exception1.getMessage());

		final var exception2 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse(".5655"));
		assertEquals("Hedera Client Runtime: Bad account format: Address \".5655\" cannot be parsed",
				exception2.getMessage());

		final var exception3 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse("0.notANumber.23"));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"0.notANumber.23\" cannot be parsed",
				exception3.getMessage());

		final var exception4 =
				assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse("0.0.23-abcde"));
		assertEquals("Hedera Client Runtime: Bad account checksum: Provided \"abcde\", should be \"armfn\"",
				exception4.getMessage());
	}

	@Test
	void parseTextWithChecksum_test() {
		/*
		Possible strings:
		- "N" where N is a number
		- "N1.N2.N3" where N1, N2 and N3 are numbers
		- "N1.N2.N3-xxxxx" where where N1, N2 and N3 are numbers and "xxxxx" is the checksum of the entity
		- "nickname (N1.N2.N3-xxxxx)" where nickname is a string name assigned to the account, where N1, N2
		   and N3
		 */

		final var acct1 = "75798";
		final var acct2 = "0.0.75798";
		final var acct3 = "0.0.75798-arbyi";
		final var acct4 = "payer (0.0.75798-arbyi)";

		final var id = new Identifier(0, 0, 75798);
		assertEquals(id, Identifier.parse(acct1));
		assertEquals(id, Identifier.parse(acct2));
		assertEquals(id, Identifier.parse(acct3));
		assertEquals(id, Identifier.parse(acct4));

		final var acct5 = "payer (0.0.something-arbyi)";
		final Exception e = assertThrows(HederaClientRuntimeException.class, () -> Identifier.parse(acct5));
		assertEquals("Hedera Client Runtime: Bad account format: Address \"payer (0.0.something-arbyi)\" cannot be " +
				"parsed", e.getMessage());
	}

	@Test
	void parseJson_Test() throws HederaClientException {
		// Parse json
		final var accountJson1 = new JsonObject();
		accountJson1.addProperty("realmNum", 789L);
		accountJson1.addProperty("shardNum", 98498L);
		accountJson1.addProperty("accountNum", 513158498L);

		final var identifierFromJson1 = Identifier.parse(accountJson1);
		assertEquals(789L, identifierFromJson1.getShardNum());
		assertEquals(98498L, identifierFromJson1.getRealmNum());
		assertEquals(513158498L, identifierFromJson1.getAccountNum());

		final var accountJson2 = new JsonObject();
		accountJson2.addProperty("accountNum", 98984L);

		final var identifierFromJson2 = Identifier.parse(accountJson2);
		assertEquals(0L, identifierFromJson2.getShardNum());
		assertEquals(0L, identifierFromJson2.getRealmNum());
		assertEquals(98984L, identifierFromJson2.getAccountNum());

		final var accountJson4 = new JsonObject();
		accountJson4.addProperty("realmNum", -789L);
		accountJson4.addProperty("shardNum", 98498L);
		accountJson4.addProperty("accountNum", 5131584984613L);

		final var exception4 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson4));
		assertEquals("Hedera Client: Invalid field realmNum", exception4.getMessage());

		final var accountJson5 = new JsonObject();
		accountJson5.addProperty("realmNum", 789L);
		accountJson5.addProperty("shardNum", -98498L);
		accountJson5.addProperty("accountNum", 5131584984613L);

		final var exception5 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson5));
		assertEquals("Hedera Client: Invalid field shardNum", exception5.getMessage());

		final var accountJson6 = new JsonObject();
		accountJson6.addProperty("realmNum", 789L);
		accountJson6.addProperty("shardNum", 98498L);
		accountJson6.addProperty("accountNum", -5131584984613L);

		final var exception6 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson6));
		assertEquals("Hedera Client: Invalid account number", exception6.getMessage());

		final var accountJson7 = new JsonObject();
		accountJson7.addProperty("realmNum", 789L);
		accountJson7.addProperty("shardNum", 98498L);
		accountJson7.addProperty("fileNum", -5131584984613L);

		final var exception7 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson7));
		assertEquals("Hedera Client: Invalid file number", exception7.getMessage());

		final var accountJson8 = new JsonObject();
		accountJson8.addProperty("realmNum", 789L);
		accountJson8.addProperty("shardNum", 98498L);
		accountJson8.addProperty("contractNum", -5131584984613L);

		final var exception8 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson8));
		assertEquals("Hedera Client: Invalid contract number", exception8.getMessage());

		final var accountJson9 = new JsonObject();
		accountJson9.addProperty("fileNum", "notANumber");

		final var exception9 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson9));
		assertEquals("java.lang.NumberFormatException: For input string: \"notANumber\"", exception9.getMessage());

		final var accountJson10 = new JsonObject();
		accountJson10.addProperty("someProperty", "notANumber");

		final var exception10 =
				assertThrows(HederaClientException.class, () -> Identifier.parse(accountJson10));
		assertEquals("Hedera Client: Invalid json object", exception10.getMessage());
	}

	@Test
	void constructors_Test() {
		final var accountId = new AccountId(0, 1, 2);
		final var identifierFromAccountId = new Identifier(accountId);
		assertEquals(0L, identifierFromAccountId.getShardNum());
		assertEquals(1L, identifierFromAccountId.getRealmNum());
		assertEquals(2L, identifierFromAccountId.getAccountNum());

		assertEquals(accountId, identifierFromAccountId.asAccount());

		final var accountID = AccountID.newBuilder().setShardNum(0).setRealmNum(1).setAccountNum(2).build();
		final var identifierFromAccountID = new Identifier(accountID);
		assertEquals(0L, identifierFromAccountID.getShardNum());
		assertEquals(1L, identifierFromAccountID.getRealmNum());
		assertEquals(2L, identifierFromAccountID.getAccountNum());

		final var fileId = new FileId(4, 5, 6);
		final var identifierFromFileId = new Identifier(fileId);
		assertEquals(4L, identifierFromFileId.getShardNum());
		assertEquals(5L, identifierFromFileId.getRealmNum());
		assertEquals(6L, identifierFromFileId.getAccountNum());

		assertEquals(fileId, identifierFromFileId.asFile());

		final var fileID = FileID.newBuilder().setShardNum(0).setRealmNum(1).setFileNum(2).build();
		final var identifierFromFileID = new Identifier(fileID);
		assertEquals(0L, identifierFromFileID.getShardNum());
		assertEquals(1L, identifierFromFileID.getRealmNum());
		assertEquals(2L, identifierFromFileID.getAccountNum());

		final var contractId = new ContractId(4, 5, 6);
		final var identifierFromContractId = new Identifier(contractId);
		assertEquals(4L, identifierFromContractId.getShardNum());
		assertEquals(5L, identifierFromContractId.getRealmNum());
		assertEquals(6L, identifierFromContractId.getAccountNum());

		assertEquals(contractId, identifierFromContractId.asContract());

		final var contractID = ContractID.newBuilder().setShardNum(0).setRealmNum(1).setContractNum(2).build();
		final var identifierFromContractID = new Identifier(contractID);
		assertEquals(0L, identifierFromContractID.getShardNum());
		assertEquals(1L, identifierFromContractID.getRealmNum());
		assertEquals(2L, identifierFromContractID.getAccountNum());
	}

	@Test
	void isValid_Test() {
		final var identifier = new Identifier();
		identifier.setAccountNum(0);
		identifier.setRealmNum(0);
		identifier.setShardNum(0);
		assertFalse(identifier.isValid());
	}

	@Test
	void toString_Test() {
		final var identifier = new Identifier(2, 6, 9);
		assertEquals("{\"realmNum\":6,\"shardNum\":2,\"accountNum\":9}", identifier.toString());
		assertEquals("2.6.9", identifier.toReadableString());
	}

	@Test
	void asJSON() {
		final var accountJson = new JsonObject();
		accountJson.addProperty("shardNum", 5);
		accountJson.addProperty("realmNum", 7);
		accountJson.addProperty("accountNum", 13);

		final var identifier = new Identifier(5, 7, 13);
		assertEquals(accountJson, identifier.asJSON());
	}

	@SuppressWarnings("SimplifiableAssertion")
	@Test
	void equals_test() {
		final var id1 = new Identifier(1, 2, 3);
		final var id2 = new Identifier(1, 2, 3);
		final var id3 = new Identifier(3, 6, 9);

		assertEquals(id1, id2);
		assertEquals(id1, id1);
		assertNotEquals(id1, id3);

		//noinspection EqualsBetweenInconvertibleTypes
		assertFalse(id1.equals("1.2.3"));
		//noinspection ConstantConditions
		assertFalse(id1.equals(null));
	}

	@Test
	void hash_test() {
		final var id2 = new Identifier(1, 2, 3);
		assertEquals(30817, id2.hashCode());
	}

	@Test
	void toNicknameAndChecksum() {
		final var accounts = new JsonObject();
		accounts.addProperty("0.0.1", "first");
		accounts.addProperty("0.0.2", "second");
		accounts.addProperty("0.0.3", "third");
		accounts.addProperty("0.0.4", "fourth");
		accounts.addProperty("0.0.5", "fifth");
		accounts.addProperty("0.0.6", "sixth");

		var account = Identifier.parse("0.0.3");
		final var accountString = account.toNicknameAndChecksum(accounts);
		assertEquals("third (0.0.3-tzfmz)", accountString);

		account = Identifier.parse("0.0.78");
		assertEquals("0.0.78-wjvid", account.toNicknameAndChecksum(accounts));
	}

	@Test
	void compare_test() {
		final var id0 = new Identifier(1, 2, 369);
		final var id1 = new Identifier(1, 2, 369);
		assertEquals(0, id0.compareTo(id1));
		assertEquals(0, id0.compareTo(new Identifier(1, 2, 369)));
		assertEquals(-1, id0.compareTo(new Identifier(2, 2, 369)));
		assertEquals(-1, id0.compareTo(new Identifier(1, 3, 369)));
		assertEquals(-1, id0.compareTo(new Identifier(1, 2, 370)));
		assertEquals(1, id0.compareTo(new Identifier(0, 2, 369)));
		assertEquals(1, id0.compareTo(new Identifier(1, 1, 369)));
		assertEquals(1, id0.compareTo(new Identifier(1, 2, 360)));

	}
}
