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

import com.hedera.hashgraph.client.cli.ToolsMain;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountCreateTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfoQuery;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.Mnemonic;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.ReceiptStatusException;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransferTransaction;
import io.github.cdimascio.dotenv.Dotenv;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.zeroturnaround.zip.ZipUtil;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.security.KeyStoreException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.TimeoutException;
import java.util.function.BooleanSupplier;

import static java.lang.Boolean.parseBoolean;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CollateCommandTest implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(CollateCommandTest.class);
	private static final BooleanSupplier isInCircleCi = () ->
			parseBoolean(Optional.ofNullable(System.getenv("IN_CIRCLE_CI")).orElse("false"));
	private static final String RESOURCES_DIRECTORY =
			((isInCircleCi.getAsBoolean()) ? "/repo/tools-cli/" : "") + "src/test/resources/";
	private final int threshold = 3;
	private final int size = 5;

	@BeforeEach
	void setUp() throws IOException {
		final var files = Objects.requireNonNull(new File(RESOURCES_DIRECTORY + "collation_test").listFiles(
				(dir, name) -> name.contains("_unzipped")),
		RESOURCES_DIRECTORY + "collation_test directory does not exist.");
		for (final var file : files) {
			if (file.isDirectory()) {
				FileUtils.deleteDirectory(file);
			}
		}
	}

	@Test
	@Disabled("Account info is missing, verification won't pass.")
	void collate_zips_test() throws Exception {
		final String[] args = { "collate", "-f", RESOURCES_DIRECTORY + "collation_test" };
		ToolsMain.main(args);

		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/0-0-2_1678312256-0.txsig").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/testCSV_Node-0-0-3.zip").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/testCSV_Node-0-0-4.zip").exists());

	}

	@Test
	@Disabled("Account information is missing, the accounts present don't match the key information in the signed files.")
	void collate_small_zips_test() throws Exception {
		final var out1 = new File(
				RESOURCES_DIRECTORY + File.separator + "Collation_small/Node-0-0-3_0_0_94@1884367800_10000-0_0_1053" +
						".txsig");
		Files.deleteIfExists(out1.toPath());

		final var out2 = new File(
				RESOURCES_DIRECTORY + File.separator + "Collation_small/Node-0-0-4_0_0_94@1884367800_10000-0_0_1053" +
						".txsig");
		Files.deleteIfExists(out2.toPath());

		final var verification = new File(RESOURCES_DIRECTORY + File.separator + "Collation_small/verification.csv");
		if (verification.exists()) {
			Files.deleteIfExists(verification.toPath());
		}

		final String[] args = { "collate", "-f", RESOURCES_DIRECTORY + "Collation_small" ,
				"-a", RESOURCES_DIRECTORY + "infos" };
		ToolsMain.main(args);

		assertTrue(out1.exists());
		assertTrue(out2.exists());
		assertTrue(verification.exists());

		Files.deleteIfExists(out1.toPath());
		Files.deleteIfExists(out2.toPath());
		Files.deleteIfExists(verification.toPath());
	}

	@Test
	void collate_zips_different_test() {
		final String[] args = { "collate", "-f", RESOURCES_DIRECTORY + "Collation_different" };
		final var exception = assertThrows(HederaClientException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client: Transactions don't match", exception.getMessage());
	}

	@Test
	void badRoot_test() {
		final String[] args = { "collate", "-f", "src/test/resource/" + "collation_test" };
		final Exception exception = assertThrows(HederaClientException.class, () -> ToolsMain.main(args));
		assertEquals("Hedera Client: Cannot find the transactions root folder", exception.getMessage());
	}

	@Test
	@Disabled("Account information is missing, the accounts present don't match the key information in the signed files.")
	void verifyWithInfos_test() throws Exception {
		final String[] args =
				{ "collate", "-f", RESOURCES_DIRECTORY + "collation_test", "-a",
						RESOURCES_DIRECTORY + "infos" };
		ToolsMain.main(args);

		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/0-0-2_1678312256-0.txsig").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/testCSV_Node-0-0-3.zip").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/testCSV_Node-0-0-4.zip").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/verification.csv").exists());
	}


	@Test
	@Disabled("Account information is missing, the accounts present don't match the key information in the signed files.")
	void verifyWithKeys_test() throws Exception {
		final String[] args =
				{ "collate", "-f", RESOURCES_DIRECTORY + "collation_test",
						"-a", RESOURCES_DIRECTORY + "infos",
						"-k", RESOURCES_DIRECTORY + "Keys" };
		ToolsMain.main(args);

		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/0-0-2_1678312256-0.txsig").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/testCSV_Node-0-0-3.zip").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/testCSV_Node-0-0-4.zip").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "/collation_test/verification.csv").exists());

	}

	@Test
	void integration_test() throws Exception {
		// Create 5 keys

		final var keyList = generateKeys(size, threshold);

		// Create account with threshold key
		final var newAccount = createAccount(keyList);

		// Create and store transfer with account as sender
		String location = generateAndStoreTransaction(newAccount);

		// Sign transfer with 3 random keys
		final var list = Arrays.asList(0, 1, 2, 3, 4);
		Collections.shuffle(list, new Random());
		createSignatures(threshold, size, location, list);

		final File[] filesBefore = new File(RESOURCES_DIRECTORY, "Integration").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equalsIgnoreCase(Constants.ZIP_EXTENSION));
		assertNotNull(filesBefore);

		// Collate and verify
		final String[] args =
				{ "collate", "-f", RESOURCES_DIRECTORY + "Integration",
						"-k", RESOURCES_DIRECTORY + "TempKeys",
						"-a", RESOURCES_DIRECTORY + "infos" };
		ToolsMain.main(args);

		final File[] filesAfter = new File(RESOURCES_DIRECTORY, "Integration").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equalsIgnoreCase(Constants.ZIP_EXTENSION));
		assertNotNull(filesAfter);

		assertEquals(filesBefore.length, filesAfter.length);

		for (final File file : filesBefore) {
			assertTrue(file.exists());
		}


		FileUtils.cleanDirectory(new File(RESOURCES_DIRECTORY, "Integration"));

		location = generateAndStoreTransaction(newAccount);
		createSignatures(threshold, size, location, list);
		ToolsMain.main(args);

		// Finally submit and check results
		final String[] argsSubmit =
				{ "submit",
						"-t", location.replace(Constants.TRANSACTION_EXTENSION,	Constants.SIGNED_TRANSACTION_EXTENSION),
						"-n", NetworkEnum.TESTNET.getName(),
						"-o", "src/test/resources/Integration" };
		ToolsMain.main(argsSubmit);

		final var receipt = TransactionReceipt.fromBytes(readBytes(location.replace(Constants.TRANSACTION_EXTENSION,
				Constants.RECEIPT_EXTENSION)));
		assertEquals(Status.SUCCESS, receipt.status);

		Files.deleteIfExists(
				new File(RESOURCES_DIRECTORY, String.format("infos/%s.info", newAccount)).toPath());

	}

	private void createSignatures(final int threshold, final int size, final String location,
			final List<Integer> list) throws KeyStoreException, HederaClientException, IOException {
		final var keys = new File("src/test/resources/TempKeys/").listFiles(
				(dir, name) -> name.contains("testKey") && Constants.PK_EXTENSION.equals(
						FilenameUtils.getExtension(name)));
		assert keys != null;
		assertEquals(size, keys.length);

		for (int i = 0; i < threshold; i++) {
			final var keyStore =
					Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), keys[list.get(i)].getPath());
			final var privateKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());
			final var transaction = Transaction.fromBytes(readBytes(location));
			final var signature = transaction.sign(privateKey).getSignatures().get(new AccountId(0, 0, 3)).get(
					privateKey.getPublicKey());
			final var sigPair = new SignaturePair(privateKey.getPublicKey(), signature);
			final var signatureFile = String.format("src/test/resources/Integration/%s.%s",
					FilenameUtils.getBaseName(location),
					Constants.SIGNATURE_EXTENSION);
			sigPair.write(signatureFile);
			final var toPack = new File[] { new File(location), new File(signatureFile) };

			for (final var file : toPack) {
				assert file.exists();
				assert file.isFile();
			}
			final File finalZip = new File(String.format("src/test/resources/Integration/%s-%s.%s",
					FilenameUtils.getBaseName(location),
					FilenameUtils.getBaseName(keys[i].getName()),
					Constants.ZIP_EXTENSION));
			ZipUtil.packEntries(toPack, finalZip);
			Files.deleteIfExists(new File(signatureFile).toPath());
		}
		// delete transaction
		Files.deleteIfExists(new File(location).toPath());
	}

	private String generateAndStoreTransaction(final AccountId newAccount) throws HederaClientException {
		final String location;
		final var transactionId =
				new TransactionId(newAccount, Instant.now().plusSeconds(10));

		final var transferTransaction = new TransferTransaction();

		transferTransaction.setMaxTransactionFee(new Hbar(1000000))
				.setTransactionId(transactionId)
				.setTransactionMemo("memo")
				.setNodeAccountIds(Collections.singletonList(new AccountId(0, 0, 3)))
				.setTransactionValidDuration(Duration.ofSeconds(175));

		transferTransaction.addHbarTransfer(newAccount, new Hbar(-1));
		transferTransaction.addHbarTransfer(new AccountId(0, 0, 75), new Hbar(1));
		transferTransaction.freeze();

		location =
				String.format("src/test/resources/Integration/%s.tx", transactionId.toString());
		writeBytes(location, transferTransaction.toBytes());
		return location;
	}

	private AccountId createAccount(final KeyList keyList) throws TimeoutException,
			PrecheckStatusException, ReceiptStatusException, HederaClientException {
		final var dotenv = Dotenv.configure().directory("../").ignoreIfMissing().load();
		final var myAccountId = AccountId.fromString(dotenv.get("TEST_ACCOUNT_ID"));
		final var myPrivateKey = PrivateKey.fromString(dotenv.get("TEST_PRIVATE_KEY"));

		final var client = Client.forTestnet();
		client.setOperator(myAccountId, myPrivateKey);

		final var transactionResponse = new AccountCreateTransaction()
				.setKey(keyList)
				.setInitialBalance(new Hbar(10))
				.execute(client);

		final var newAccount = transactionResponse.getReceipt(client).accountId;
		assert newAccount != null;
		final var response = new AccountInfoQuery().setAccountId(newAccount).execute(client);
		writeBytes(String.format("src/test/resources/infos/%s.info", newAccount), response.toBytes());
		assertEquals(new Hbar(10), response.balance);
		assertEquals(size, ((KeyList) response.key).size());
		assertEquals(threshold, ((KeyList) response.key).getThreshold());
		return newAccount;
	}

	@NotNull
	private KeyList generateKeys(final int size, final int threshold) throws IOException {
		if (new File(RESOURCES_DIRECTORY + "TempKeys/").mkdirs()) {
			logger.info("Temporary keys folder created");
		}
		final var mnemonic = Mnemonic.generate24();
		final var keyName = RESOURCES_DIRECTORY + "TempKeys/testKey-";
		if (new File(RESOURCES_DIRECTORY + "out").exists()) {
			FileUtils.deleteDirectory(new File(RESOURCES_DIRECTORY + "out"));
		}

		final var keyList = new KeyList();
		for (var i = 0; i < size; i++) {
			SecurityUtilities.generateAndStoreKey(String.format("%s%d.pem", keyName, i), "Hedera CLI Tool", mnemonic, i,
					Constants.TEST_PASSWORD.toCharArray());
			final var pubKey = EncryptionUtils.publicKeyFromFile(
					String.format("%s/TempKeys/testKey-%d.pub", RESOURCES_DIRECTORY, i));
			keyList.add(pubKey);
		}
		keyList.setThreshold(threshold);
		return keyList;
	}

	@AfterEach
	void tearDown() throws IOException {
		new File(RESOURCES_DIRECTORY, "collation_test/0-0-2_1678312256-0.txsig").deleteOnExit();
		new File(RESOURCES_DIRECTORY, "collation_test/testCSV_Node-0-0-3.zip").deleteOnExit();
		new File(RESOURCES_DIRECTORY, "collation_test/testCSV_Node-0-0-4.zip").deleteOnExit();
		new File(RESOURCES_DIRECTORY, "collation_test/verification.csv").deleteOnExit();
		if (new File(RESOURCES_DIRECTORY, "TempKeys/").exists()) {
			FileUtils.cleanDirectory(new File(RESOURCES_DIRECTORY, "TempKeys/"));
		}
		if (new File(RESOURCES_DIRECTORY, "Integration/").exists()) {
			FileUtils.cleanDirectory(new File(RESOURCES_DIRECTORY, "Integration/"));
		}
	}
}
