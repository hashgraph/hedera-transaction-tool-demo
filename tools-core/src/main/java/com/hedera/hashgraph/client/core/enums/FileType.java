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

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;

import static com.hedera.hashgraph.client.core.constants.Constants.BATCH_TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.BUNDLE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.COMMENT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.CONFIGURATION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.LARGE_BINARY_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.METADATA_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SOFTWARE_UPDATE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;

public enum FileType {
	TRANSACTION(TRANSACTION_EXTENSION),
	BATCH(BATCH_TRANSACTION_EXTENSION),
	LARGE_BINARY(LARGE_BINARY_EXTENSION),
	SOFTWARE_UPDATE(SOFTWARE_UPDATE_EXTENSION),
	ACCOUNT_INFO(INFO_EXTENSION),
	PUBLIC_KEY(PUB_EXTENSION),
	COMMENT(COMMENT_EXTENSION),
	CONFIG(CONFIGURATION_EXTENSION),
	METADATA(METADATA_EXTENSION),
	BUNDLE(BUNDLE_EXTENSION),
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
			case BUNDLE:
				return "Information Bundle";
			case METADATA:
			case UNKNOWN:
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + this);
		}
		return "";
	}

	public static FileType getType(final String extension) throws HederaClientException {
		for (final var type :
				FileType.values()) {
			if (type.getExtension().equals(extension)) {
				return type;
			}
		}
		throw new HederaClientException(String.format("Unrecognized extension %s", extension));
	}
}
