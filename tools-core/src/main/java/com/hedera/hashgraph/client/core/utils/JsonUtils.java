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

package com.hedera.hashgraph.client.core.utils;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;

import java.math.BigDecimal;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.POSITIVE_NUMBER_CURRENCY_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;

public class JsonUtils {

	private JsonUtils() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Converts an AccountInfo object to a Json object
	 *
	 * @param accountInfo
	 * 		an account
	 * @return a Json object
	 */
	public static JsonObject accountInfoToJson(final AccountInfo accountInfo) {
		final var jsonObject = new JsonObject();

		jsonObject.addProperty("accountId", accountInfo.accountId.toString());
		jsonObject.addProperty("contractAccountId", accountInfo.contractAccountId);
		jsonObject.addProperty("deleted", accountInfo.isDeleted);
		if (accountInfo.proxyAccountId != null) {
			jsonObject.addProperty("proxyAccountId", accountInfo.proxyAccountId.toString());
		}
		jsonObject.addProperty("proxyReceived", String.valueOf(accountInfo.proxyReceived));
		jsonObject.addProperty("key", String.valueOf(accountInfo.key));
		jsonObject.addProperty("balance", String.valueOf(accountInfo.balance));
		jsonObject.addProperty("sendRecordThreshold", String.valueOf(accountInfo.sendRecordThreshold));
		jsonObject.addProperty("receiveRecordThreshold", String.valueOf(accountInfo.receiveRecordThreshold));
		jsonObject.addProperty("receiverSignatureRequired", accountInfo.isReceiverSignatureRequired);
		jsonObject.addProperty("expirationTime", String.valueOf(accountInfo.expirationTime));
		jsonObject.add("autoRenewPeriod", new Timestamp(accountInfo.autoRenewPeriod).asJSON());
		jsonObject.addProperty("liveHashes", accountInfo.liveHashes.toString());

		return jsonObject;
	}

	public static JsonObject hBarsToJsonObject(final Hbar hbar) {
		final var jsonObject = new JsonObject();
		final var decimal = hbar.getValue();
		jsonObject.addProperty(H_BARS, decimal.longValue());
		jsonObject.addProperty(TINY_BARS, (decimal.remainder(BigDecimal.ONE)).multiply(new BigDecimal(100000000L)).setScale(0));
		return jsonObject;
	}

	public static Hbar jsonToHBars(final JsonObject hBars) {
		var bigDecimal = BigDecimal.ZERO;
		if (hBars.has(TINY_BARS)) {
			final var tiny = hBars.get(TINY_BARS).getAsString().replace(" ", "").replace("_", "");
			if (Long.parseLong(tiny) < 0) {
				throw new HederaClientRuntimeException(POSITIVE_NUMBER_CURRENCY_ERROR_MESSAGE);
			}
			bigDecimal = BigDecimal.valueOf(Double.parseDouble(tiny) / 100000000L);
		}

		if (hBars.has(H_BARS)) {
			final var bars = hBars.get(H_BARS).getAsString().replace(" ", "").replace("_", "");
			if (Long.parseLong(bars) < 0) {
				throw new HederaClientRuntimeException(POSITIVE_NUMBER_CURRENCY_ERROR_MESSAGE);
			}
			bigDecimal = bigDecimal.add(BigDecimal.valueOf(Long.parseLong(bars)));
		}
		return new Hbar(bigDecimal);
	}
}
