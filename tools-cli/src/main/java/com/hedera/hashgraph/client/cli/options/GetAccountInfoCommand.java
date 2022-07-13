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

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.JsonUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import io.grpc.StatusRuntimeException;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import picocli.CommandLine;

import java.io.File;
import java.security.KeyStoreException;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.security.PasswordInput.readPasswordFromStdIn;

@CommandLine.Command(name = "get-account-info", aliases = { "gi" }, description = "Get account information")
public class GetAccountInfoCommand implements ToolCommand {
	private static final  Logger logger = LogManager.getLogger(GetAccountInfoCommand.class);

	@CommandLine.Option(names = { "-a", "--account" }, description = "Account number(s) of the accounts that require " +
			"infos", split = "[, \\t\\n\\x0B\\f\\r]", required = true)
	private String[] accounts;

	@CommandLine.Option(names = { "-p", "--payer" }, description = "Account number of the fee payer account", required
			= true)
	private String feePayer;

	@CommandLine.Option(names = { "-k", "--key" }, description = "Location of the fee payer's key", required = true)
	private String key = "";

	@CommandLine.Option(names = { "-n", "--network" }, description = "The Hedera network the transaction will be " +
			"submitted to (one of MAINNET, PREVIEWNET, TESTNET, or <location of custom nodes file>)")
	private String submissionClient = "mainnet";

	@CommandLine.Option(names = { "-o", "--output-directory" }, description = "Folder where the account information " +
			"will be stored")
	private String out = "";

	@CommandLine.Option(names = { "-h", "--help" }, usageHelp = true, description = "Display this help and exit")
	private boolean help;


	@Override
	public void execute() throws HederaClientException {

		var directory = System.getProperty("user.dir");
		if (!"".equals(out)) {
			if (new File(out).mkdirs()) {
				logger.info("Directory {} created", out);
			}
			directory = out;
		}

		if (!new File(key).exists()) {
			throw new HederaClientException(String.format("The file %s doesn't exist", key));
		}

		final var password =
				readPasswordFromStdIn(String.format("Enter the password for key %s", FilenameUtils.getBaseName(key)));
		final Ed25519KeyStore keyStore;
		try {
			keyStore = Ed25519KeyStore.read(password, key);
		} catch (final KeyStoreException e) {
			logger.error(e.getMessage());
			throw new HederaClientException(e);
		}
		final var privateKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());

		try (final var client = CommonMethods.getClient(submissionClient)) {
			client.setOperator(Identifier.parse(feePayer).asAccount(), privateKey);
			for (final var account : accounts) {
				if ("".equals(account)) {
					// account for empty strings
					continue;
				}
				final var id = Identifier.parse(account).asAccount();
				final AccountInfo accountInfo = getAccountInfo(client, id);
				writeJsonObject(String.format("%s/%s.json", directory, id), JsonUtils.accountInfoToJson(accountInfo));
				writeBytes(String.format("%s/%s.info", directory, id), accountInfo.toBytes());

				logger.info("Account information for account {} has been written to file {}/{}.info", id, directory,
						id);
			}
		} catch (final TimeoutException e) {
			logger.error(e.getMessage());
			throw new HederaClientException(e);
		}
	}

	private AccountInfo getAccountInfo(final Client client, final AccountId id) {
		final AccountInfo accountInfo;
		try {
			accountInfo = new AccountInfoQuery()
					.setAccountId(id)
					.execute(client);
		} catch (final TimeoutException e) {
			logger.error(e.getMessage());
			throw new HederaClientRuntimeException(e.getMessage());
		} catch (final PrecheckStatusException e) {
			logger.error("The transaction did not pass pre-check");
			throw new HederaClientRuntimeException(e.getMessage());
		} catch (final StatusRuntimeException e) {
			logger.error("Could not connect to the network");
			throw new HederaClientRuntimeException(e);
		}
		return accountInfo;
	}
}
