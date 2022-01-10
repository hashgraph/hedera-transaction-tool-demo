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

public enum SetupPhase {
	PASSWORD_RECOVERY_PHASE(-1),
	INITIAL_SETUP_PHASE(0),
	NORMAL_OPERATION_PHASE(9),
	TEST_PHASE(13);

	private final int value;

	SetupPhase(final int value) {
		this.value = value;
	}

	public int getValue() {
		return value;
	}

	public static SetupPhase fromInt(final int p) {
		switch (p) {
			case -1:
				return PASSWORD_RECOVERY_PHASE;
			case 9:
				return NORMAL_OPERATION_PHASE;
			case 13:
				return TEST_PHASE;
			case 0:
			default:
				return INITIAL_SETUP_PHASE;
		}
	}
}
