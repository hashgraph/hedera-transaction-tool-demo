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

package com.hedera.hashgraph.client.core.security;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.sdk.Mnemonic;
import org.bouncycastle.openpgp.PGPException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SecurityUtilitiesTest {
	private static final String PASSWORD = "123456789";
	private static final String TEST_AES_PATH = "src/test/resources/test.aes";

	@AfterEach
	void tearDown() {
		new File(TEST_AES_PATH).deleteOnExit();
	}

	@Test
	void encryptDecryptFile() throws HederaClientException, IOException {
		final var testString = Mnemonic.generate24().toString();
		final var key = SecurityUtilities.keyFromPasswordLegacy(PASSWORD.toCharArray());

		SecurityUtilities.toEncryptedFile(key, TEST_AES_PATH, testString);
		assertTrue(new File(TEST_AES_PATH).exists());

		final var path = Paths.get(TEST_AES_PATH);
		final var data = Files.readAllBytes(path);
		final var header = "AES|256|CBC|PKCS5Padding|".getBytes(UTF_8);

		final var readHeader = Arrays.copyOfRange(data, 0, header.length);
		assertArrayEquals(header, readHeader);

		final var decrypted = SecurityUtilities.fromEncryptedFile(key, TEST_AES_PATH);
		assertEquals(testString, decrypted.toString());
	}

	@Test
	void readLegacyMnemonic_test() throws HederaClientException {
		final var key = SecurityUtilities.keyFromPasswordLegacy(PASSWORD.toCharArray());
		final var decrypted = SecurityUtilities.fromEncryptedFile(key, "src/test/resources/legacy.aes");

		final String legacyString =
				"egg scale cement between rocket aerobic alert parent portion sail narrow win valley country target " +
						"confirm humor exile nation trial brisk journey crumble invest";

		assertEquals(legacyString, decrypted.toString());

	}

	@Test
	void verifyFile_test() throws PGPException, IOException, HederaClientException {
		boolean verifyFile = SecurityUtilities.verifyFile("src/test/resources/Files/TransactionTools-1.1.0-rc.1.pkg",
				"src/test/resources/Files/TransactionTools-1.1.0-rc.1.gpg",
				"src/test/resources/Keys/verification – Public.asc");
		assertTrue(verifyFile);

		verifyFile = SecurityUtilities.verifyFile("src/test/resources/Files/TransactionTools-1.1.0-rc.1.pkg",
				"src/test/resources/Files/badSignature.gpg",
				"src/test/resources/Keys/verification – Public.asc");
		assertFalse(verifyFile);
	}
}