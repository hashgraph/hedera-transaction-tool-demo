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

public enum Actions {
	ACCEPT("ACCEPT"),
	DECLINE("DECLINE");

	private final String action;

	Actions(String action) {
		this.action = action;
	}

	public String getAction() {
		return action;
	}

	@Override
	public String toString() {
		switch (this){
			case ACCEPT:
				return "accepted";
			case DECLINE:
				return "declined";
		}
		return super.toString();
	}


}
