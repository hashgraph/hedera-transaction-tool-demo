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

package com.hedera.hashgraph.client.core.enums;

public enum TransactionType {
	CRYPTO_TRANSFER,
	CRYPTO_CREATE,
	CRYPTO_UPDATE,
	SYSTEM_DELETE_UNDELETE,
	FILE_UPDATE,
	FILE_APPEND,
	FREEZE,
	NODE_CREATE,
	NODE_UPDATE,
	NODE_DELETE;


	@Override
	public String toString() {
		switch (this){
			case CRYPTO_TRANSFER:
				return "Transfer Transaction";
			case CRYPTO_CREATE:
				return "Create New Account Transaction";
			case CRYPTO_UPDATE:
				return "Update Account Transaction";
			case SYSTEM_DELETE_UNDELETE:
				return "Content Transaction";
			case FILE_UPDATE:
				return "File Update Transaction";
			case FILE_APPEND:
				return "File Append Transaction";
			case FREEZE:
				return "Freeze Transaction";
			case NODE_CREATE:
				return "Node Create Transaction";
			case NODE_UPDATE:
				return "Node Update Transaction";
			case NODE_DELETE:
				return "Node Delete Transaction";
		}
		return super.toString();
	}
}
