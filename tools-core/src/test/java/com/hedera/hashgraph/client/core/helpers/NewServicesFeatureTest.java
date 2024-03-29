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
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.Ed25519PrivateKey;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountBalanceQuery;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.hedera.hashgraph.sdk.proto.AccountID;
import com.hedera.hashgraph.sdk.proto.CryptoGetInfoResponse;
import com.hedera.hashgraph.sdk.proto.Key;
import io.github.cdimascio.dotenv.Dotenv;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;
import org.junit.Ignore;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.security.KeyStoreException;
import java.util.Objects;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class NewServicesFeatureTest implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(NewServicesFeatureTest.class);

	@Test
	void newAccounInfo_test() throws HederaClientException, InvalidProtocolBufferException {
		final var info = AccountInfo.fromBytes(readBytes(new File("src/test/resources/AccountInfos/0.0.2.info")));
		final var account =
				AccountID.newBuilder()
						.setAccountNum(info.accountId.num)
						.setRealmNum(info.accountId.realm)
						.setShardNum(info.accountId.shard)
						.build();
		final var key = Key.newBuilder()
				.setEd25519(ByteString.copyFrom(readBytes("src/test/resources/Keys/genesis.pub")))
				.build();
		final var newInfo =
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

	@Test
	void networkField_test() throws KeyStoreException, PrecheckStatusException, TimeoutException,
			HederaClientException, ReceiptStatusException {
		final var dotenv = Dotenv.configure().directory("../").ignoreIfMissing().load();
		final var myAccountId = AccountId.fromString(dotenv.get("TEST_ACCOUNT_ID"));
		final var privateKey = dotenv.get("TEST_PRIVATE_KEY");
		final var myPrivateKey = PrivateKey.fromString(privateKey);
		final var keyStore = new Ed25519KeyStore.Builder()
				.withPassword(Constants.TEST_PASSWORD.toCharArray()).build();
		keyStore.insertNewKeyPair(Ed25519PrivateKey.fromBytes(Hex.decode(privateKey.startsWith("0x") ?
				privateKey.substring(2) : privateKey)));

		final var client = CommonMethods.getClient(NetworkEnum.TESTNET);
		logger.info(client.getNetwork());
		client.setOperator(myAccountId, myPrivateKey);

		final var key = EncryptionUtils.jsonToKey(readJsonObject("src/test/resources/KeyFiles/jsonKeyList.json"));
		final var transactionResponse = new AccountCreateTransaction()
				.setKey(key)
				.setInitialBalance(new Hbar(0))
				.setAccountMemo("Test payer account")
				.execute(client);

		final var receipt = transactionResponse.getReceipt(client);
		assertNotNull(receipt.accountId);
		final var payerId = receipt.accountId;
		logger.info("Payer Id: {}", payerId.toString());


		final var accountInfo = new AccountInfoQuery()
				.setAccountId(payerId)
				.execute(client);

		logger.info("account ledger: \"{}\"", Hex.toHexString(accountInfo.ledgerId.toBytes()));
		logger.info("account ledger (decoded): \"{}\"",
				new String(accountInfo.ledgerId.toBytes(), StandardCharsets.UTF_8));

		final var ledgerId = client.getLedgerId();
		if (ledgerId != null) {
			logger.info("client ledger: \"{}\"", Hex.toHexString(ledgerId.toBytes()));
		}
	}

	@Test
	void checkOldInfo_test() throws HederaClientException, InvalidProtocolBufferException {
		final var oldInfo = AccountInfo.fromBytes(readBytes("src/test/resources/infos/0.0.2.info"));
		assertNotNull(oldInfo.ledgerId);
		logger.info(Hex.toHexString(oldInfo.ledgerId.toBytes()));
	}
}
