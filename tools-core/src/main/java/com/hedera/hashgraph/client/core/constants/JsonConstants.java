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

package com.hedera.hashgraph.client.core.constants;

public class JsonConstants {
	private JsonConstants() {
		throw new IllegalStateException("Constants class");
	}

	public static final String PREFERRED_KEY_LOCATION = "keyLocation";
	public static final String PREFERRED_REALM = "preferredRealm";
	public static final String PREFERRED_SHARD = "preferredShard";
	public static final String PREFERRED_NODE_ACCOUNT = "preferredNode";
	public static final String PREFERRED_TRANSACTION_FEE = "preferredTransactionFee";

	public static final String FEE_PAYER_ACCOUNT_FIELD_NAME = "feePayerAccount";
	public static final String SECONDS = "seconds";
	public static final String NANOS = "nanos";
	public static final String ACCOUNT_TO_UPDATE = "accountIdToUpdate";
	public static final String ENTITY_TO_DEL_UNDEL = "entityToDelUnDel";
	public static final String FILE_CONTRACT_SWITCH = "fileContractSwitch";
	public static final String DEL_UNDEL_SWITCH = "deleteUnDeleteSwitch";
	public static final String EXPIRATION_DATE_TIME = "expirationTime";
	public static final String FILE_ID_FIELD_NAME = "fileID";
	public static final String CONTENTS_FIELD_NAME = "newContentsLocation";

	public static final String TRANSACTION_FEE_FIELD_NAME = "transactionFee";
	public static final String ACCOUNT_ID_FIELD_NAME = "accountID";
	public static final String NODE_ID_FIELD_NAME = "nodeAccountID";
	public static final String NETWORK_FIELD_NAME = "network";

	public static final String REALM_NUMBER = "realmNum";
	public static final String SHARD_NUMBER = "shardNum";
	public static final String ACCOUNT_NUMBER = "accountNum";

	public static final String TRANSACTION_VALID_DURATION_FIELD_NAME = "transactionValidDuration";
	public static final String TRANSACTION_VALID_START_FIELD_NAME = "transactionValidStart";
	public static final String TRANSACTION_VALID_START_READABLE_FIELD_NAME = "transactionValidStart_RFC";
	public static final String MEMO_FIELD_NAME = "memo";

	public static final String FEE_PAYER_KEY_LOCATION = "feePayerKeyLocation";

	public static final String NEW_KEY_FIELD_NAME = "newKey";
	public static final String INITIAL_BALANCE_FIELD_NAME = "initialBalance";
	public static final String AUTO_RENEW_PERIOD_FIELD_NAME = "autoRenewPeriod";
	public static final String RECEIVER_SIGNATURE_REQUIRED_FIELD_NAME = "receiverSignatureRequired";

	public static final String TRANSFERS = "transfers";
	public static final String ACCOUNT = "account";
	public static final String AMOUNT = "amount";

	public static final String H_BARS = "hBars";
	public static final String TINY_BARS = "tinyBars";

}
