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
import net.i2p.crypto.eddsa.EdDSAPublicKey;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import picocli.CommandLine;

import java.io.File;
import java.io.IOException;
import java.security.KeyStoreException;
import java.util.Arrays;

import static com.hedera.hashgraph.client.core.security.PasswordInput.readPasswordFromStdIn;

/**
 * This command will create keys that can be imported into the tool. These keys will not be recoverable.
 * This method of generation was created for creating keys for testing.
 */
@CommandLine.Command(name = "create-key", aliases = { "nk" }, description = "Create new PEM key")
public class CreateKeyCommand implements ToolCommand {
	private static final Logger logger = LogManager.getLogger(CreateKeyCommand.class);

	@CommandLine.Option(names = { "-o", "--output" }, description = "Location and name of the key to save",
			required = true)
	private String output = "";

	@CommandLine.Option(names = { "-p", "--policy" }, description = "Disable password policy check")
	private boolean policy = false;

	@Override
	public void execute() throws HederaClientException, IOException {
		var privateKey = Ed25519PrivateKey.generate();

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

		final var pemName = new File(output).getPath() + "." + Constants.PK_EXTENSION;
		final var pubName = new File(output).getPath() + "." + Constants.PUB_EXTENSION;

		final Ed25519KeyStore ed25519KeyStore;

		try {
			ed25519KeyStore = new Ed25519KeyStore.Builder().withPassword(password)
					.build();
			ed25519KeyStore.insertNewKeyPair(privateKey);
			ed25519KeyStore.write(pemName);
		} catch (final KeyStoreException e) {
			logger.error(e.getMessage());
			throw new HederaClientException(e);
		}

		EncryptionUtils.storePubKey(pubName, (EdDSAPublicKey) ed25519KeyStore.get(0).getPublic());

		logger.info("Private key converted and stored");
	}
}
