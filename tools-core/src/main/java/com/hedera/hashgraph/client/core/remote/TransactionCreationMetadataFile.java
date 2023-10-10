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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
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
import java.util.Objects;

public class TransactionCreationMetadataFile extends RemoteFile {
	private static final Logger logger = LogManager.getLogger(TransactionCreationMetadataFile.class);
	public static final String NODES_STRING = "nodes";
	public static final String ACCOUNTS_STRING = "accounts";
	public static final String IS_UPDATE_ACCOUNT_FEE_PAYER = "isUpdateAccountFeePayer";

	private AccountList nodes;

	private AccountList accounts;
	private boolean isUpdateAccountFeePayer = false;

	private TransactionCreationMetadataFile() {
		setType(FileType.TRANSACTION_CREATION_METADATA);
	}

	public TransactionCreationMetadataFile(final FileDetails file) {
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
				if (contents.has(IS_UPDATE_ACCOUNT_FEE_PAYER)) {
					isUpdateAccountFeePayer = contents.get(IS_UPDATE_ACCOUNT_FEE_PAYER).getAsBoolean();
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

	public boolean isUpdateAccountFeePayer() {
		return isUpdateAccountFeePayer;
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
		var stringBuilder = new ToStringBuilder(this, ToStringStyle.JSON_STYLE);
		if (nodes != null) {
			stringBuilder.append(NODES_STRING, nodes);
		}
		if (accounts != null) {
			stringBuilder.append(ACCOUNTS_STRING, accounts);
		}
		if (isUpdateAccountFeePayer) {
			stringBuilder.append(IS_UPDATE_ACCOUNT_FEE_PAYER, isUpdateAccountFeePayer);
		}
		return stringBuilder.toString();
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof TransactionCreationMetadataFile) {
			return Objects.equals(((TransactionCreationMetadataFile) o).accounts, accounts)
					&& Objects.equals(((TransactionCreationMetadataFile) o).nodes, nodes)
					&& Objects.equals(((TransactionCreationMetadataFile) o).isUpdateAccountFeePayer,
					isUpdateAccountFeePayer);
		}
		return false;
	}

	@Override
	public int hashCode() {
		int base = 13;
		int hash = base + nodes.hashCode();
		hash = base * hash + accounts.hashCode();
		return hash;
	}

	public static final class Builder {
		private String parentPath;
		private String name;
		private AccountList nodes;
		private AccountList accounts;
		private boolean isUpdateAccountFeePayer;

		public Builder() {
			// Default constructor
		}

		public Builder withLocation(final String location) {
			final var file = new File(location);
			name = file.getName();
			parentPath = file.getParent();
			return this;
		}

		public Builder withNodes(final String input, final List<Identifier> list) {
			if (list != null && list.size() > 1) {
				this.nodes = new AccountList(input, list);
			}
			return this;
		}

		public Builder withAccounts(final String input, final List<Identifier> list) {
			if (list != null && list.size() > 1) {
				this.accounts = new AccountList(input, list);
			}
			return this;
		}

		public Builder withIsUpdateAccountFeePayer(boolean flag) {
			this.isUpdateAccountFeePayer = flag;
			return this;
		}

		public TransactionCreationMetadataFile build() {
			// If no value is valid, return null
			if (nodes == null && accounts == null && !isUpdateAccountFeePayer) return null;

			final var tcm = new TransactionCreationMetadataFile();
			tcm.setParentPath(parentPath);
			tcm.setName(name);
			tcm.nodes = this.nodes;
			tcm.accounts = this.accounts;
			tcm.isUpdateAccountFeePayer = this.isUpdateAccountFeePayer;
			return tcm;
		}
	}
}
