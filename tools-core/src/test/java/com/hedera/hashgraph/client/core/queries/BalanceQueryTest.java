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

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Test;

import java.io.File;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.CUSTOM_NETWORKS;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;

public class BalanceQueryTest {

	private static Logger logger = LogManager.getLogger(BalanceQueryTest.class);

	@Test
	public void getBalance_test() throws HederaClientException, PrecheckStatusException, TimeoutException {
		BalanceQuery query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(new AccountId(0, 0, 101))
				.withNetwork("mainnet")
				.build();
		var balance = query.getBalance();
		assertEquals(new Hbar(0), balance);

		query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(new AccountId(0, 0, 2))
				.withNetwork("mainnet")
				.build();
		balance = query.getBalance();
		assertTrue(balance.toTinybars() > 0);

		query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(new AccountId(0, 0, 2))
				.withNetwork("testnet")
				.build();
		balance = query.getBalance();
		assertTrue(balance.toTinybars() > 0);

		query = BalanceQuery.Builder.aBalanceQuery()
				.withAccountId(new AccountId(0, 0, 2))
				.withNetwork("preview")
				.build();
		balance = query.getBalance();
		assertTrue(balance.toTinybars() > 0);

		if (new File(CUSTOM_NETWORKS).mkdirs()) {
			logger.info("Custom networks folder created");
		}



	}
}