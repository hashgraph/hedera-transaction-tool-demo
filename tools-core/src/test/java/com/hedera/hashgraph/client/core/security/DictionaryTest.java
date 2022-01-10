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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DictionaryTest {

	@Test
	void valid_test() {
		final Dictionary dictionary = new Dictionary();
		assertTrue(dictionary.valid("pass"));
		assertFalse(dictionary.valid("abecee"));
	}

	@Test
	void suggested_test() {
		final Dictionary dictionary = new Dictionary();
		var list = dictionary.suggestWords("ab");
		assertEquals(10, list.size());

		list = dictionary.suggestWords("abs");
		assertEquals(4, list.size());

		list = dictionary.suggestWords("abso");
		assertEquals(1, list.size());

		list = dictionary.suggestWords("absp");
		assertEquals(0, list.size());
	}
}