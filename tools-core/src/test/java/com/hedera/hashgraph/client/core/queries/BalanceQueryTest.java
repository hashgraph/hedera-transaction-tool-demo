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

package com.hedera.hashgraph.client.core.queries;

import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.HbarUnit;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.concurrent.TimeoutException;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;

class BalanceQueryTest {

	private static final Logger logger = LogManager.getLogger(BalanceQueryTest.class);

	@Test
	void getBalance_test() throws HederaClientException, PrecheckStatusException, TimeoutException, IOException {
		var query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(new AccountId(0, 0, 101))
				.withNetwork(NetworkEnum.MAINNET.getName())
				.build();
		var balance = query.getBalance();
		assertEquals(Hbar.from(80, HbarUnit.TINYBAR), balance);

		query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(new AccountId(0, 0, 2))
				.withNetwork(NetworkEnum.MAINNET.getName())
				.build();
		balance = query.getBalance();
		logger.info("Balance for Mainnet: {}", balance);
		assertTrue(balance.toTinybars() > 0);

		query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(new AccountId(0, 0, 2))
				.withNetwork(NetworkEnum.TESTNET.getName())
				.build();
		balance = query.getBalance();
		logger.info("Balance for Testnet: {}", balance);

		assertTrue(balance.toTinybars() > 0);

		query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(new AccountId(0, 0, 2))
				.withNetwork(NetworkEnum.PREVIEWNET.getName())
				.build();
		balance = query.getBalance();
		logger.info("Balance for PreviewNet: {}", balance);
		assertTrue(balance.toTinybars() > 0);
	}
}
