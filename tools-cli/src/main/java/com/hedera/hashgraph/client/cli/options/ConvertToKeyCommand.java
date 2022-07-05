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

package com.hedera.hashgraph.client.cli.options;

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.Ed25519PrivateKey;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.PrivateKey;
import net.i2p.crypto.eddsa.EdDSAPublicKey;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.util.encoders.Hex;
import picocli.CommandLine;

import java.io.File;
import java.io.IOException;
import java.security.KeyStoreException;
import java.util.Arrays;

import static com.hedera.hashgraph.client.core.security.PasswordInput.readPasswordFromStdIn;

@CommandLine.Command(name = "convert-key", aliases = { "ck" }, description = "Convert json key to PEM")
public class ConvertToKeyCommand implements ToolCommand {
	private static final Logger logger = LogManager.getLogger(ConvertToKeyCommand.class);

	@CommandLine.Option(names = { "-k", "--key" }, description = "Location of the fee payer's key as a json file",
			required = true)
	private String key = "";

	@CommandLine.Option(names = { "-p", "--policy" }, description = "Disable password policy check")
	private boolean policy = false;

	@Override
	public void execute() throws HederaClientException, IOException {
		if (!new File(key).exists()) {
			throw new HederaClientException(String.format("Cannot find key file %s", key));
		}

		final var jsonObject = readJsonObject(key);
		if (!jsonObject.has("operator")) {
			throw new HederaClientException("Cannot find operator information");
		}

		final var operator = jsonObject.get("operator").getAsJsonObject();
		if (!operator.has("privateKey")) {
			throw new HederaClientException("Cannot find private key");
		}

		if (!operator.has("publicKey")) {
			throw new HederaClientException("Cannot find public key");
		}

		var password =
				readPasswordFromStdIn("Enter new password for the new encrypted file (more than 10 characters long)");

		if (!policy) {
			while (CommonMethods.badPassword(password)) {
				password = readPasswordFromStdIn(
						"Enter new password for the new encrypted file (more than 10 characters long)");
			}
		}

		while (true) {
			final var passwordConfirmation =
					readPasswordFromStdIn("Enter the password again for confirmation");
			if (passwordConfirmation.length == 0) {
				return;
			}
			if (Arrays.equals(password, passwordConfirmation)) {
				break;
			}
			logger.info(
					"The password does not match your original input. Please try again or press <ENTER> to quit.");
		}


		final var privateKey = operator.get("privateKey").getAsString();
		final var operatorKey = PrivateKey.fromBytes(Hex.decode(privateKey));
		final var publicKey = operatorKey.getPublicKey();

		final var publicKeyString = operator.get("publicKey").getAsString();
		if (publicKeyString.equals(publicKey.toString())) {
			logger.info("Public Keys match");
		}

		final var pemName = new File(key).getParent() + File.separator + FilenameUtils.getBaseName(
				key) + "." + Constants.PK_EXTENSION;
		final var pubName = new File(key).getParent() + File.separator + FilenameUtils.getBaseName(
				key) + "." + Constants.PUB_EXTENSION;

		final var ed25519PrivateKey = Ed25519PrivateKey.fromBytes(operatorKey.toBytes());
		final Ed25519KeyStore ed25519KeyStore;

		try {
			ed25519KeyStore = new Ed25519KeyStore.Builder().withPassword(password)
					.build();
			ed25519KeyStore.insertNewKeyPair(ed25519PrivateKey);
			ed25519KeyStore.write(pemName);
		} catch (final KeyStoreException e) {
			logger.error(e.getMessage());
			throw new HederaClientException(e);
		}

		EncryptionUtils.storePubKey(pubName, (EdDSAPublicKey) ed25519KeyStore.get(0).getPublic());


		logger.info("Private key converted and stored");
	}

}
