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

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.security.KeyStoreException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Ed25519KeyStoreTest {

	public static final String PASSWORD = "123456789";

	@Test
	void read() throws KeyStoreException {
		final char[] password = PASSWORD.toCharArray();
		final Ed25519KeyStore keyStore0 = new Ed25519KeyStore.Builder().withPassword(password).build();
		keyStore0.insertNewKeyPair();
		keyStore0.write("src/test/resources/Keys/test0.pem");

		final Ed25519KeyStore keyStore = Ed25519KeyStore.read(password, "src/test/resources/Keys/test0.pem");

		assertEquals(keyStore0.get(0).getPrivate(), keyStore.get(0).getPrivate());
		assertEquals(keyStore0.get(0).getPublic(), keyStore.get(0).getPublic());
	}

	@AfterEach
	void tearDown() {
		if (new File("src/test/resources/Keys/test0.pem").exists()) {
			new File("src/test/resources/Keys/test0.pem").deleteOnExit();
		}
	}
}