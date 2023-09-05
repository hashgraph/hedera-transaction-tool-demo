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
		return new ToStringBuilder(this, ToStringStyle.JSON_STYLE)
				.append(NODES_STRING, nodes)
				.append(ACCOUNTS_STRING, accounts)
				.append(IS_UPDATE_ACCOUNT_FEE_PAYER, isUpdateAccountFeePayer)
				.toString();
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

		public Builder withNodes(final String input, final List<String> list) {
			this.nodes = new AccountList(input, list);
			return this;
		}

		public Builder withAccounts(final String input, final List<String> list) {
			this.accounts = new AccountList(input, list);
			return this;
		}

		public Builder withIsUpdateAccountFeePayer(boolean flag) {
			this.isUpdateAccountFeePayer = flag;
			return this;
		}

		public TransactionCreationMetadataFile build() {
			// If 0 or 1 node, AND 0 or 1 accounts, AND fee payer is NOT the updated account
			// return null as no AccountListFile will be created.
			if ((nodes == null || nodes.getList().size() == 1)
					&& (accounts == null || accounts.getList().size() == 1)
					&& !isUpdateAccountFeePayer) return null;
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
