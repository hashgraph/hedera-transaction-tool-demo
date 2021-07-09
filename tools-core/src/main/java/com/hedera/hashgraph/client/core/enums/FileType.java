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

import com.hedera.hashgraph.client.core.constants.Constants;

public enum FileType {
	TRANSACTION(Constants.TRANSACTION_EXTENSION),
	BATCH(Constants.BATCH_TRANSACTION_EXTENSION),
	LARGE_BINARY(Constants.LARGE_BINARY_EXTENSION),
	SOFTWARE_UPDATE(Constants.SOFTWARE_UPDATE_EXTENSION),
	ACCOUNT_INFO(Constants.INFO_EXTENSION),
	PUBLIC_KEY(Constants.PUB_EXTENSION),
	COMMENT(Constants.COMMENT_EXTENSION),
	CONFIG(Constants.CONFIGURATION_EXTENSION),
	METADATA(Constants.METADATA_EXTENSION),
	UNKNOWN("");

	private final String extension;

	FileType(final String extension) {
		this.extension = extension;
	}

	public String getExtension() {
		return extension;
	}

	@Override
	public String toString() {
		switch (this) {
			case TRANSACTION:
				return "Signing Transaction";
			case BATCH:
				return "Batch Transactions";
			case LARGE_BINARY:
				return "Large File Update";
			default:
				return super.toString();

		}
	}

	public String toKind() {
		switch (this) {
			case TRANSACTION:
				return "Transaction";
			case BATCH:
				return "Batch Transaction";
			case COMMENT:
			case CONFIG:
				return "";
			case ACCOUNT_INFO:
				return "Account Information";
			case PUBLIC_KEY:
				return "Public Key";
			case SOFTWARE_UPDATE:
				return "Software Update";
			case LARGE_BINARY:
				return "File Contents Update";
			case METADATA:
			case UNKNOWN:
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + this);
		}
		return "";
	}
}
