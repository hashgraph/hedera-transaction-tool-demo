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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.core.json;


import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.ContractId;
import com.hedera.hashgraph.sdk.FileId;
import com.hedera.hashgraph.sdk.proto.AccountID;
import com.hedera.hashgraph.sdk.proto.ContractID;
import com.hedera.hashgraph.sdk.proto.FileID;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.INCOMPATIBLE_TYPES_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.NULL_OBJECT_COMPARISON_ERROR_MESSAGE;

public class Identifier implements Comparable {

	public static final String REALM_NUM = "realmNum";
	public static final String SHARD_NUM = "shardNum";
	public static final String ACCOUNT_NUM = "accountNum";
	@JsonProperty(defaultValue = "0")
	private long realmNum;

	@JsonProperty(defaultValue = "0")
	private long shardNum;

	@JsonProperty(required = true)
	@JsonAlias({ "fileNum", "contractNum" })
	private long accountNum;

	public Identifier() {
	}

	public Identifier(final long shardNum, final long realmNum, final long accountNum) {
		this.realmNum = realmNum;
		this.shardNum = shardNum;
		this.accountNum = accountNum;
	}

	public Identifier(AccountID accountID) {
		this(accountID.getShardNum(), accountID.getRealmNum(), accountID.getAccountNum());
	}

	public Identifier(AccountId accountId) {
		if (accountId != null) {
			this.shardNum = accountId.shard;
			this.realmNum = accountId.realm;
			this.accountNum = accountId.num;
		}
	}

	public Identifier(FileID fileID) {
		this(fileID.getShardNum(), fileID.getRealmNum(), fileID.getFileNum());
	}

	public Identifier(FileId fileId) {
		this(fileId.shard, fileId.realm, fileId.num);
	}

	public Identifier(ContractID contractID) {
		this(contractID.getShardNum(), contractID.getRealmNum(), contractID.getContractNum());
	}

	public Identifier(ContractId contractId) {
		this(contractId.shard, contractId.realm, contractId.num);
	}

	public static Identifier parse(JsonObject jsonObject) throws HederaClientException {
		handleShardOrRealmNumber(jsonObject, REALM_NUM);
		handleShardOrRealmNumber(jsonObject, SHARD_NUM);

		long num = handleNumber(jsonObject);
		if (num == -1) {
			throw new HederaClientException("Invalid json object");
		}

		return Identifier.parse(String.format("%d.%d.%d",
				jsonObject.get(REALM_NUM).getAsLong(),
				jsonObject.get(SHARD_NUM).getAsLong(),
				num));
	}

	private static long handleNumber(JsonObject jsonObject) throws HederaClientException {
		long num = -1;
		try {
			if (jsonObject.has(ACCOUNT_NUM)) {
				num = jsonObject.get(ACCOUNT_NUM).getAsLong();
				if (num < 0) {
					throw new HederaClientException("Invalid account number");
				}
			}
			if (jsonObject.has("fileNum")) {
				num = jsonObject.get("fileNum").getAsLong();
				if (num < 0) {
					throw new HederaClientException("Invalid file number");
				}
			}
			if (jsonObject.has("contractNum")) {
				num = jsonObject.get("contractNum").getAsLong();
				if (num < 0) {
					throw new HederaClientException("Invalid contract number");
				}
			}
		} catch (NumberFormatException e) {
			throw new HederaClientException(e);
		}
		return num;
	}

	private static void handleShardOrRealmNumber(JsonObject jsonObject, String field) throws HederaClientException {
		if (jsonObject.has(field)) {
			var num = jsonObject.get(field).getAsLong();
			if (num < 0) {
				throw new HederaClientException(String.format("Invalid field %s", field));
			}
		} else {
			jsonObject.addProperty(field, 0);
		}
	}

	public static Identifier parse(final String id) {

		if (id == null || id.isEmpty()) {
			throw new HederaClientRuntimeException("The provided string was null or empty");
		}

		if (!id.contains(".")) {
			return new Identifier(0, 0, componentToLong(id));
		}

		var parts = id.split("\\.");

		if (parts.length == 1) {
			return new Identifier(0, 0, componentToLong(parts[0]));
		} else if (parts.length == 3) {
			final var realmId = componentToLong(parts[0]);
			final var shardId = componentToLong(parts[1]);
			final var accountId = componentToLong(parts[2]);
			return new Identifier(realmId, shardId, accountId);
		}

		throw new HederaClientRuntimeException(String.format("%s cannot be parsed as an account ID", id));
	}

	private static long componentToLong(final String component) {
		try {
			return Long.parseLong(component);
		} catch (NumberFormatException ex) {
			throw new HederaClientRuntimeException(ex);
		}
	}

	public long getRealmNum() {
		return realmNum;
	}

	public void setRealmNum(final long realmNum) {
		this.realmNum = realmNum;
	}

	public long getShardNum() {
		return shardNum;
	}

	public void setShardNum(final long shardNum) {
		this.shardNum = shardNum;
	}

	public long getAccountNum() {
		return accountNum;
	}

	public void setAccountNum(final long accountNum) {
		this.accountNum = accountNum;
	}

	public boolean isValid() {
		return realmNum >= 0 && shardNum >= 0 && accountNum > 0;
	}


	public AccountId asAccount() {
		return new AccountId(shardNum, realmNum, accountNum);
	}

	public ContractId asContract() {
		return new ContractId(shardNum, realmNum, accountNum);
	}

	public FileId asFile() {
		return new FileId(shardNum, realmNum, accountNum);
	}

	public String toReadableString() {
		return String.format("%d.%d.%d", shardNum, realmNum, accountNum);
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		final var identifier = (Identifier) o;
		return realmNum == identifier.realmNum &&
				shardNum == identifier.shardNum &&
				accountNum == identifier.accountNum;
	}

	@Override
	public int hashCode() {
		return Objects.hash(shardNum, realmNum, accountNum);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.JSON_STYLE)
				.append(REALM_NUM, realmNum)
				.append(SHARD_NUM, shardNum)
				.append(ACCOUNT_NUM, accountNum)
				.toString();
	}

	public JsonElement asJSON() {
		var id = new JsonObject();
		id.addProperty(REALM_NUM, realmNum);
		id.addProperty(SHARD_NUM, shardNum);
		id.addProperty(ACCOUNT_NUM, accountNum);
		return id;
	}

	@Override
	public int compareTo(Object o) {
		if (this == o) {
			return 0;
		}
		if (o == null) {
			throw new NullPointerException(NULL_OBJECT_COMPARISON_ERROR_MESSAGE);
		}
		if (getClass() != o.getClass()) {
			throw new HederaClientRuntimeException(INCOMPATIBLE_TYPES_ERROR_MESSAGE);
		}

		if (this.equals(o)) {
			return 0;
		}

		final var identifier = (Identifier) o;
		if (this.realmNum != identifier.getRealmNum()) {
			return Long.compare(this.realmNum, identifier.getRealmNum());
		}

		if (this.shardNum != identifier.getShardNum()) {
			return Long.compare(this.shardNum, identifier.getShardNum());
		}

		return Long.compare(this.accountNum, identifier.getAccountNum());
	}
}
