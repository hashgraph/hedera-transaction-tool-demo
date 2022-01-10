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

package com.hedera.hashgraph.client.core.transactions;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.security.KeyStoreException;
import java.util.Optional;
import java.util.function.BooleanSupplier;

import static com.hedera.hashgraph.client.core.testHelpers.TestHelpers.getJsonInputCT;
import static java.lang.Boolean.parseBoolean;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SignaturePairTest {
	public static BooleanSupplier isInCircleCi = () ->
			parseBoolean(Optional.ofNullable(System.getenv("IN_CIRCLE_CI")).orElse("false"));
	private static final String RESOURCES_DIRECTORY =
			((isInCircleCi.getAsBoolean()) ? "/repo/tools-cli/" : "") + "src/test/resources/";

	@Test
	void write() throws KeyStoreException, HederaClientException {
		final JsonObject testJson = getJsonInputCT(50, 2, 50, new Timestamp(2000).asInstant());
		final ToolTransferTransaction toolTransaction = new ToolTransferTransaction(testJson);
		final PrivateKey privateKey = EncryptionUtils.getPrivateKeyFromPEM(RESOURCES_DIRECTORY + "Keys/genesis.pem");
		final PublicKey publicKey = privateKey.getPublicKey();

		final SignaturePair signaturePair = new SignaturePair(publicKey, toolTransaction.sign(privateKey));
		signaturePair.write(RESOURCES_DIRECTORY + "tempFile.sigpair");

		final SignaturePair newPair = new SignaturePair(RESOURCES_DIRECTORY + "tempFile.sigpair");
		assertEquals(signaturePair.getPublicKey(), newPair.getPublicKey());
		assertEquals(signaturePair.getSignature(), signaturePair.getSignature());

		new File(RESOURCES_DIRECTORY + "tempFile.sigpair").delete();
	}
}