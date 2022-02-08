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

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.util.Arrays;
import java.util.Base64;

public final class PasswordAuthenticator {

	private static final Logger logger = LogManager.getLogger(PasswordAuthenticator.class);

	private static final String ALGORITHM = "PBKDF2WithHmacSHA1";

	private final int keyLength; //key length in bits
	private final int iterationCount;

	public PasswordAuthenticator(final int length, final int count) {
		this.keyLength = length;
		this.iterationCount = count;
	}

	public PasswordAuthenticator() {
		this(Constants.KEY_LENGTH, Constants.PBKDF2_ITERATION_COUNT);
	}

	/***
	 * Calculates the hash of a char array
	 * @param password a char array that contains the password
	 * @return a string with the salt and the hash of the password
	 */
	public String hash(final char[] password) {
		final var salt = SecurityUtilities.generateRandomBytes(Constants.SALT_LENGTH);
		final var hash = SecurityUtilities.generateArgon2id(password, salt);
		final var tokenBytes = new byte[salt.length + hash.length];

		System.arraycopy(salt, 0, tokenBytes, 0, salt.length);
		System.arraycopy(hash, 0, tokenBytes, salt.length, hash.length);

		final var encoder = Base64.getEncoder();

		return encoder.encodeToString(tokenBytes);

	}

	/**
	 * Takes a password and a token and verifies that the token corresponds to the hashed password
	 *
	 * @param password
	 * 		user input as char array
	 * @param token
	 * 		the hashed password that was stored
	 * @return true if the password corresponds to the token
	 */
	public boolean authenticateLegacy(final char[] password, final String token) throws InvalidKeySpecException,
			NoSuchAlgorithmException {
		final var decoder = Base64.getDecoder();

		final var tokenBytes = decoder.decode(token);
		if (tokenBytes.length != Constants.SALT_LENGTH + keyLength / 8) {
			logger.error("Token size check failed");
			return false;
		}
		final var salt = Arrays.copyOfRange(tokenBytes, 0, Constants.SALT_LENGTH);
		final var hash = Arrays.copyOfRange(tokenBytes, salt.length, tokenBytes.length);

		final KeySpec spec = new PBEKeySpec(password, salt, iterationCount, keyLength);
		final var factory = SecretKeyFactory.getInstance(ALGORITHM);
		final var check = factory.generateSecret(spec).getEncoded();

		return Arrays.equals(hash, check);

	}

	/**
	 * Takes a password and a token and verifies that the token corresponds to the hashed password
	 *
	 * @param password
	 * 		user input as char array
	 * @param token
	 * 		the hashed password that was stored
	 * @return true if the password corresponds to the token
	 */
	public boolean authenticate(final char[] password, final String token) {
		final var decoder = Base64.getDecoder();

		final var tokenBytes = decoder.decode(token);
		if (tokenBytes.length != Constants.SALT_LENGTH + keyLength / 8) {
			logger.error("Token size check failed");
			return false;
		}
		final var salt = Arrays.copyOfRange(tokenBytes, 0, Constants.SALT_LENGTH);
		final var hash = Arrays.copyOfRange(tokenBytes, salt.length, tokenBytes.length);
		final var check = SecurityUtilities.generateArgon2id(password, salt);

		return Arrays.equals(hash, check);
	}


}
