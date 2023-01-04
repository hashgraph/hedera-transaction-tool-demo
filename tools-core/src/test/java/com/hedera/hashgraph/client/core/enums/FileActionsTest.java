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

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

@Disabled("Temporarily disabling tests")
class FileActionsTest {

	@Test
	void testToString() {
		FileActions action = FileActions.ACCEPT;
		assertEquals("ACCEPT", action.toString());

		action = FileActions.ADD_MORE;
		assertEquals("ADD MORE", action.toString());

		action = FileActions.BROWSE;
		assertEquals("BROWSE", action.toString());

		action = FileActions.SIGN;
		assertEquals("SIGN", action.toString());

		action = FileActions.DECLINE;
		assertEquals("DECLINE", action.toString());

		action = FileActions.UPDATE;
		assertEquals("UPDATE", action.toString());

	}
}