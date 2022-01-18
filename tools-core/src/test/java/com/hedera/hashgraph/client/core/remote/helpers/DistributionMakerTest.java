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

package com.hedera.hashgraph.client.core.remote.helpers;

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.sdk.AccountId;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.zip.ZipFile;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DistributionMakerTest implements GenericFileReadWriteAware {

	public static final String OUTPUT = "src/test/resources/Files/output/user";

	@Test
	void buildBundleLegacy_Test() throws HederaClientException, KeyStoreException {
		final var storageLocation = "src/test/resources/Files";
		final var maker =
				new DistributionMaker(new AccountId(1001), new AccountId(1001), new AccountId(12), new Timestamp(125,
						0), 98765432, "", storageLocation, OUTPUT);

		assertNotNull(maker);
		final var keyPairs =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");
		for (var i = 0; i < 1000; i++) {
			final var line =
					new BatchLine.Builder()
							.withAmount("123654")
							.withReceiverAccountID(new Identifier(0, 0, 1003 + i).toReadableString())
							.withTimeStamp("12/12/21", 3, 45)
							.build();

			maker.buildBundle(line, (!keyPairs.isEmpty()) ? keyPairs.get(0) : null);
		}

		maker.pack();
		final var csv = readCSV("src/test/resources/Files/output/user/Files_summary.csv");
		final var transactions = getZipNames("src/test/resources/Files/output/user/Files_transactions.zip");
		final var signatures = getZipNames("src/test/resources/Files/output/user/Files_signatures.zip");

		assertNotNull(transactions);
		assertNotNull(signatures);

		assertEquals(transactions.size(), signatures.size());
		final Set<String> names = new HashSet<>();
		for (final var signature : signatures) {
			names.add(FilenameUtils.getBaseName(signature));
		}
		for (final var transaction : transactions) {
			names.add(FilenameUtils.getBaseName(transaction));
		}
		assertNotNull(names);
		assertEquals(signatures.size(), names.size());

		for (final var transaction : csv) {
			final var nameString = transaction.get(0).replace("\"", "");
			final var jsonString = transaction.get(1).replace("\"\"", "\"").replace(";", ",");
			if ("Filename".equals(nameString)) {
				continue;
			}
			assertTrue(names.contains(FilenameUtils.getBaseName(nameString)));
			assertTrue(jsonString.contains("98765432")); // transaction fee
			assertTrue(jsonString.contains("123654")); // amount
			assertTrue(jsonString.contains("1639280700")); // start time in seconds
			assertTrue(jsonString.contains("2021-12-12T03:45:00")); // start date time
		}
	}

	@Test
	void buildBundle_Test() throws HederaClientException, KeyStoreException {
		final var storageLocation = "src/test/resources/Files";
		final var maker =
				new DistributionMaker(new AccountId(1001), new AccountId(1007), new AccountId(12), new Timestamp(125,
						0), 98765432, "a memo line", storageLocation, OUTPUT);

		assertNotNull(maker);
		final var keyPairs =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");
		for (var i = 0; i < 1000; i++) {
			final var line =
					new BatchLine.Builder()
							.withAmount("123654")
							.withReceiverAccountID(new Identifier(0, 0, 1003 + i).toReadableString())
							.withTimeStamp("12/12/21", 3, 45)
							.build();

			maker.buildBundle(line, (!keyPairs.isEmpty()) ? keyPairs.get(0) : null);
		}

		maker.pack();
		final var csv = readCSV("src/test/resources/Files/output/user/Files_summary.csv");
		final var transactions = getZipNames("src/test/resources/Files/output/user/Files_transactions.zip");
		final var signatures = getZipNames("src/test/resources/Files/output/user/Files_signatures.zip");

		assertNotNull(transactions);
		assertNotNull(signatures);

		assertEquals(transactions.size(), signatures.size());
		final Set<String> names = new HashSet<>();
		for (final var signature : signatures) {
			names.add(FilenameUtils.getBaseName(signature));
		}
		for (final var transaction : transactions) {
			names.add(FilenameUtils.getBaseName(transaction));
		}
		assertNotNull(names);
		assertEquals(signatures.size(), names.size());

		for (final var transaction : csv) {
			final var nameString = transaction.get(0).replace("\"", "");
			final var jsonString = transaction.get(1).replace("\"\"", "\"").replace(";", ",");
			if ("Filename".equals(nameString)) {
				continue;
			}
			assertTrue(names.contains(FilenameUtils.getBaseName(nameString)));
			assertTrue(jsonString.contains("98765432")); // transaction fee
			assertTrue(jsonString.contains("123654")); // amount
			assertTrue(jsonString.contains("1639280700")); // start time in seconds
			assertTrue(jsonString.contains("2021-12-12T03:45:00")); // start date time
			assertTrue(jsonString.contains(
					"\"accountID\":{\"realmNum\":0,\"shardNum\":0,\"accountNum\":1001},\"amount\":-123654}"));
			// the transfer from sender
			assertTrue(jsonString.contains(
					"\"feePayerAccount\":{\"realmNum\":0,\"shardNum\":0,\"accountNum\":1007},")); //fee payer
			assertTrue(jsonString.contains("\"memo\":\"a memo line\"")); // the memo
		}
	}

	private List<String> getZipNames(final String zip) throws HederaClientException {
		final List<String> zipNames = new ArrayList<>();
		try (final var zipFile = new ZipFile(zip)) {
			final var zipEntries = zipFile.entries();
			while (zipEntries.hasMoreElements()) {
				zipNames.add(zipEntries.nextElement().getName());
			}
		} catch (final IOException e) {
			throw new HederaClientException("Invalid zip");
		}
		return zipNames;
	}

	@BeforeEach
	void setUp() throws IOException {
		if (new File(OUTPUT).exists()) {
			FileUtils.deleteDirectory(new File(OUTPUT));
		}
		if (new File("src/test/resources/Files_transactions").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/Files_transactions"));
		}
		if (new File("src/test/resources/Files_signatures").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/Files_signatures"));
		}
	}

	@AfterEach
	void tearDown() throws IOException {
		if (new File(OUTPUT).exists()) {
			FileUtils.deleteDirectory(new File(OUTPUT));
		}
		if (new File("src/test/resources/Files_transactions").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/Files_transactions"));
		}
		if (new File("src/test/resources/Files_signatures").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/Files_signatures"));
		}
	}
}