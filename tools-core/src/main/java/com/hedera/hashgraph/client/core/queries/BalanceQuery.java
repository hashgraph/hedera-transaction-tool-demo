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

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountBalanceQuery;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;

import java.io.File;
import java.util.Locale;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.CUSTOM_NETWORK_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;

public class BalanceQuery implements GenericFileReadWriteAware {
	private final String network;
	private final AccountId accountId;

	private BalanceQuery(final String network, final AccountId accountId) {
		this.network = network;
		this.accountId = accountId;
	}

	public Hbar getBalance() throws PrecheckStatusException, TimeoutException, HederaClientException {
		if (NetworkEnum.isNetwork(network.toUpperCase(Locale.ROOT)) &&
				!NetworkEnum.INTEGRATION.getName().equals(network.toUpperCase(Locale.ROOT))) {
			try (final Client client = Client.forName(network.toLowerCase(Locale.ROOT))) {
				return new AccountBalanceQuery().setAccountId(accountId)
						.execute(client)
						.hbars;
			}
		}
		try (final Client client = getClient()) {
			return new AccountBalanceQuery().setAccountId(accountId)
					.execute(client)
					.hbars;
		}
	}

	private Client getClient() throws HederaClientException {
		final var customNetwork = readJsonArray(CUSTOM_NETWORK_FOLDER + File.separator + network + "." + JSON_EXTENSION);
		return CommonMethods.getClient(customNetwork);
	}

	public static final class Builder {
		private String network;
		private AccountId accountId;

		private Builder() {
		}

		public static Builder aBalanceQuery() {
			return new Builder();
		}

		public Builder withNetwork(final String network) {
			this.network = network;
			return this;
		}

		public Builder withAccountId(final AccountId accountId) {
			this.accountId = accountId;
			return this;
		}

		public BalanceQuery build() {
			return new BalanceQuery(network, accountId);
		}
	}
}
