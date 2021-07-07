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

package com.hedera.hashgraph.client.core.exceptions;

public class HederaClientRuntimeException extends RuntimeException {
	public HederaClientRuntimeException() {
		super();
	}

	public HederaClientRuntimeException(String message) {
		super("Hedera Client Runtime: " + message);
	}

	public HederaClientRuntimeException(String message, Throwable cause) {
		super("Hedera Client Runtime: " + message, cause);
	}

	public HederaClientRuntimeException(Throwable cause) {
		super(cause);
	}

	protected HederaClientRuntimeException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
