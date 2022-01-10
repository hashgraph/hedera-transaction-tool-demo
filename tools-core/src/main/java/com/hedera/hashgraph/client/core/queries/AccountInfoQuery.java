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
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.CUSTOM_NETWORK_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;

public class AccountInfoQuery implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(AccountInfoQuery.class);
	private String network = "mainnet";
	private AccountId feePayer;
	private Hbar fee = Hbar.from(1);

	private final List<PrivateKey> signingKeys;
	private final Client client;

	private AccountInfoQuery(final String network, final AccountId feePayer, final Hbar fee,
			final List<PrivateKey> signingKeys) {
		this.network = network;
		this.feePayer = feePayer;
		this.fee = fee;
		this.signingKeys = signingKeys;
		this.client = getClient();
	}

	private Client getClient() {

		if (NetworkEnum.isNetwork(network.toUpperCase(Locale.ROOT)) && !NetworkEnum.INTEGRATION.getName().equals(
				network.toUpperCase(Locale.ROOT))) {
			return Client.forName(network.toLowerCase(Locale.ROOT));
		}

		try {
			return CommonMethods.getClient(
					readJsonArray(CUSTOM_NETWORK_FOLDER + File.separator + network + "." + JSON_EXTENSION));
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
		return null;

	}

	public AccountInfo getInfo(final AccountId account) throws PrecheckStatusException, TimeoutException {
		for (final PrivateKey signingKey : signingKeys) {
			client.setOperator(feePayer, signingKey);
		}

		client.setDefaultMaxQueryPayment(fee);

		return new com.hedera.hashgraph.sdk.AccountInfoQuery()
				.setAccountId(account)
				.execute(client);
	}

	public static final class Builder {
		private String network = "mainnet";
		private AccountId feePayer;
		private Hbar fee = new Hbar(1);
		private List<PrivateKey> signingKeys;

		private Builder() {
		}

		public static Builder anAccountInfoQuery() {
			return new Builder();
		}

		public Builder withNetwork(final String network) {
			this.network = network;
			return this;
		}

		public Builder withFeePayer(final AccountId feePayer) {
			this.feePayer = feePayer;
			return this;
		}

		public Builder withFee(final Hbar fee) {
			this.fee = fee;
			return this;
		}

		public Builder withSigningKeys(final List<PrivateKey> signingKeys) {
			this.signingKeys = signingKeys;
			return this;
		}

		public AccountInfoQuery build() {
			return new AccountInfoQuery(network, feePayer, fee, signingKeys);
		}
	}
}
