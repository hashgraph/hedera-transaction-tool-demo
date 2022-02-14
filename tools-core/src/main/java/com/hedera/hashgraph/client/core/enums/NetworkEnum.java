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

import com.hedera.hashgraph.sdk.LedgerId;

import java.util.stream.Stream;

public enum NetworkEnum {
	MAINNET("MAINNET"),
	PREVIEWNET("PREVIEWNET"),
	TESTNET("TESTNET"),
	INTEGRATION("INTEGRATION"),
	CUSTOM("CUSTOM"),
	UNKNOWN("");

	private String name = "";

	NetworkEnum(final String name) {
		if (Stream.of("mainnet", "previewnet", "integration", "testnet", "custom").anyMatch(
				s -> s.equalsIgnoreCase(name))) {
			this.name = name;
		}
	}

	public static NetworkEnum from(LedgerId ledgerId) {
		if (ledgerId==null) return UNKNOWN;
		switch (ledgerId.toString()){
			case "30783031":
				return NetworkEnum.TESTNET;
			default:
				return NetworkEnum.UNKNOWN;
		}
	}

	public String getName() {
		return name;
	}

	public static boolean isNetwork(final String network) {
		return (Stream.of("mainnet", "previewnet", "integration", "testnet", "custom").anyMatch(
				s -> s.equalsIgnoreCase(network)));
	}

}
