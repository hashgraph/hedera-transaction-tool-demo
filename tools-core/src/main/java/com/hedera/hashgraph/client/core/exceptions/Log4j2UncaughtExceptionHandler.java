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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class Log4j2UncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {

	private static final Logger logger = LogManager.getLogger("app.err");

	private static final Log4j2UncaughtExceptionHandler INSTANCE = new Log4j2UncaughtExceptionHandler();

	public static Log4j2UncaughtExceptionHandler get() {
		return INSTANCE;
	}

	@Override
	public void uncaughtException(Thread t, Throwable e) {
		final var errorMessage =
				String.format("ERROR: uncaught exception occurs, check app.err log file for details. %nMessage: %s",
						e.getMessage());
		logger.error(errorMessage);
		final var thread = t.toString();
		logger.error("Uncaught exception {} on thread {}", e.getMessage(), thread);
	}

}
