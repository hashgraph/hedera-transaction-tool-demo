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

import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PasswordAuthenticatorTest {

	@Test
	void test_encrypt_decrypt() throws InvalidKeySpecException, NoSuchAlgorithmException {
		final PasswordAuthenticator passwordAuthenticator = new PasswordAuthenticator();
		final var hash = passwordAuthenticator.hash("alpha biome".toCharArray());
		assertFalse(passwordAuthenticator.authenticate("biome alpha".toCharArray(), hash));
		assertTrue(passwordAuthenticator.authenticate("alpha biome".toCharArray(), hash));
		assertFalse(passwordAuthenticator.authenticateLegacy("alpha biome".toCharArray(), hash));
		assertFalse(passwordAuthenticator.authenticate("alpha biome".toCharArray(), hash.substring(1)));


	}
}