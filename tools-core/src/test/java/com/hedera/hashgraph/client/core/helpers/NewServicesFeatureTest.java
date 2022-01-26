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

package com.hedera.hashgraph.client.core.helpers;

import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.proto.AccountID;
import com.hedera.hashgraph.sdk.proto.CryptoGetInfoResponse;
import com.hedera.hashgraph.sdk.proto.Key;
import org.junit.jupiter.api.Test;

import java.io.File;

import static org.junit.jupiter.api.Assertions.assertEquals;

class NewServicesFeatureTest implements GenericFileReadWriteAware {
	@Test
	void newAccounInfo_test() throws HederaClientException, InvalidProtocolBufferException {
		final var info = AccountInfo.fromBytes(readBytes(new File("src/test/resources/AccountInfos/0.0.2.info")));
		final AccountID account =
				AccountID.newBuilder()
						.setAccountNum(info.accountId.num)
						.setRealmNum(info.accountId.realm)
						.setShardNum(info.accountId.shard)
						.build();
		final Key key = Key.newBuilder()
				.setEd25519(ByteString.copyFrom(readBytes("src/test/resources/Keys/genesis.pub")))
				.build();
		final CryptoGetInfoResponse.AccountInfo newInfo =
				CryptoGetInfoResponse.AccountInfo.newBuilder()
						.setAccountID(account)
						.setBalance(info.balance.toTinybars())
						.setKey(key)
						.setMemo("Treasury test")
						.build();

		writeBytes("src/test/resources/AccountInfos/0.0.2_2.info", newInfo.toByteArray());

		final var testInfo = AccountInfo.fromBytes(readBytes(new File("src/test/resources/AccountInfos/0.0.2_2.info")));
		assertEquals("Treasury test", testInfo.accountMemo);
	}
}
