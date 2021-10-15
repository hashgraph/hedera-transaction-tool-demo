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

import java.util.stream.Stream;

public enum AccountInfoFields {
	accountID("Account ID"),
	contractAccountID("Contract ID"),
	deleted("Deleted"),
	proxyAccountID("Proxy ID"),
	proxyReceived("Proxy Received"),
	key("Key"),
	balance("Balance"),
	generateSendRecordThreshold("Send Record Threshold"),
	generateReceiveRecordThreshold("Receive Record Threshold"),
	receiverSigRequired("Receiver Signature Required"),
	expirationTime("Expiration Date"),
	autoRenewPeriod("Auto Renew"),
	liveHashes("Live Hashes"),
	tokenRelationships("Token Relationships"),
	memo("Memo"),
	ownedNfts("Owned NFTs"),
	max_automatic_token_associations("Maximum Token Associations");
	private String name = "";

	AccountInfoFields(final String name) {
		if (Stream.of("Account ID", "Contract ID", "Deleted", "Proxy ID", "Proxy Received", "Key", "Balance",
				"Send Record Threshold", "Receive Record Threshold", "Receiver Signature Required",
				"Expiration Date", "Auto Renew", "Live Hashes", "Token Relationships", "Memo", "Owned NFTs",
				"Maximum Token Associations").anyMatch(
				s -> s.equalsIgnoreCase(name))) {
			this.name = name;
		}
	}

	public String getName() {
		return name;
	}

}
