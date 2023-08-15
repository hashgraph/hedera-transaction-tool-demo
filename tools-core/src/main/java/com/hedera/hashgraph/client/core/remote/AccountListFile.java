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

package com.hedera.hashgraph.client.core.remote;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.helpers.AccountList;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

public class AccountListFile extends RemoteFile {
	private static final Logger logger = LogManager.getLogger(AccountListFile.class);
	private static final String NODES_STRING = "nodes";
	private static final String ACCOUNTS_STRING = "accounts";

	@JsonProperty()
	private AccountList nodes;

	@JsonProperty()
	private AccountList accounts;

	private AccountListFile() {}

	public AccountListFile(final FileDetails file) {
		super(file);
		if (file == null) {
			return;
		}

		var contents = new JsonObject();
		if (new File(getPath()).exists()) {
			try {
				contents = readJsonObject(getPath());
				if (contents.has(NODES_STRING)) {
					nodes = new AccountList(contents.get(NODES_STRING).getAsJsonObject());
				}
				if (contents.has(ACCOUNTS_STRING)) {
					accounts = new AccountList(contents.get(ACCOUNTS_STRING).getAsJsonObject());
				}
			} catch (final HederaClientException e) {
				logger.error(e);
			}
		}
	}

	public AccountList getNodes() {
		return nodes;
	}

	public AccountList getAccounts() {
		return accounts;
	}

	public void toFile(final String location) {
		try (final var writer = new BufferedWriter(new FileWriter(location))) {
			writer.write(this.toString());
		} catch (final IOException e) {
			logger.error(e);
		}
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.JSON_STYLE)
				.append(NODES_STRING, nodes)
				.append(ACCOUNTS_STRING, accounts)
				.toString();
	}

	public static final class Builder {
		private AccountList nodes;
		private AccountList accounts;

		public Builder() {
			// Default constructor
		}

		public Builder withNodes(final String input, final List<String> list) {
			this.nodes = new AccountList(input, list);
			return this;
		}

		public Builder withAccounts(final String input, final List<String> list) {
			this.accounts = new AccountList(input, list);
			return this;
		}

		public AccountListFile build() {
			// If 0 or 1 node, AND no accounts, return null as no AccountListFile will be created.
			if ((nodes == null || nodes.getList().size() == 1) && accounts == null) return null;
			final var accountLists = new AccountListFile();
			accountLists.nodes = this.nodes;
			accountLists.accounts = this.accounts;
			return accountLists;
		}
	}
}
