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

import com.hedera.hashgraph.client.core.constants.Constants;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.Arrays;

public class PasswordInput {

	private static final Logger logger = LogManager.getLogger(PasswordInput.class);

	private PasswordInput() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Request a password from the user.If the console is not open, the method assumes that we are in a testing
	 * environment and returns the default password
	 *
	 * @param message
	 * 		the password message
	 * @param args
	 * 		the rest of the arguments
	 * @return a char array
	 */
	public static char[] readPasswordFromStdIn(String message, Object... args) {
		if (System.console() != null) {
			return System.console().readPassword(message, args);
		} else {
			logger.info("Cannot open console: using test password");
			return Constants.TEST_PASSWORD.toCharArray();
		}
	}

	/**
	 * Request a new password from the user, and confirm. If the passwords are different, retry. If the console is not
	 * open, the method assumes that we are in a testing environment and returns the default password
	 *
	 * @param message
	 * 		the new password message
	 * @param confirmation
	 * 		the confirmation message
	 * @param args
	 * 		the rest of the arguments
	 * @return a char array
	 */
	public static char[] readPasswordAndConfirm(String message, String confirmation,
			Object... args) {

		while (true) {
			final char[] password;
			final char[] confirmedPassword;
			logger.info("Getting password and confirmation");
			if (System.console() != null) {
				logger.info("Getting password with message: {}", message);
				password = System.console().readPassword(message, args);
				logger.info("Getting confirmation with message: {}", confirmation);
				confirmedPassword = System.console().readPassword(confirmation, args);
			} else {
				logger.info("Cannot open console: Using test password");
				password = Constants.TEST_PASSWORD.toCharArray();
				confirmedPassword = Constants.TEST_PASSWORD.toCharArray();
			}

			if (Arrays.areEqual(password, confirmedPassword)) {
				logger.info("Password and confirmation match");
				return password;
			}
			logger.error("Passwords don't match");
		}
	}
}
