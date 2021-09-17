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
import com.hedera.hashgraph.client.core.utils.TimeUtils;
import com.hedera.hashgraph.sdk.AccountId;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DistributionMakerTest implements GenericFileReadWriteAware {

	public static final String OUTPUT = "src/test/resources/Files/output/user";

	@Test
	void buildBundle_Test() throws HederaClientException, KeyStoreException {
		String storageLocation = "src/test/resources/Files";
		var maker = new DistributionMaker(new AccountId(1001), new AccountId(12), new Timestamp(125, 0), 98765432,
				storageLocation, OUTPUT);

		assertNotNull(maker);
		var keyPairs =
				Ed25519KeyStore.read(Constants.TEST_PASSWORD.toCharArray(), "src/test/resources/Keys/genesis.pem");
		for (var i = 0; i < 1000; i++) {
			BatchLine line =
					new BatchLine.Builder().withAmount("123654").withReceiverAccountID(
							new Identifier(0, 0, 1003 + i).toReadableString()).withTimeStamp("12/12/21"
							, 3, 45).build();
			maker.buildBundle(line, (!keyPairs.isEmpty()) ? keyPairs.get(0) : null);
		}

		maker.pack();
		var csv = readCSV("src/test/resources/Files/output/user/Files_summary.csv");
		var transactions = getZipNames("src/test/resources/Files/output/user/Files_transactions.zip");
		var signatures = getZipNames("src/test/resources/Files/output/user/Files_signatures.zip");

		assertNotNull(transactions);
		assertNotNull(signatures);

		assertEquals(transactions.size(), signatures.size());
		Set<String> names = new HashSet<>();
		for (String signature : signatures) {
			names.add(FilenameUtils.getBaseName(signature));
		}
		for (String transaction : transactions) {
			names.add(FilenameUtils.getBaseName(transaction));
		}
		assertNotNull(names);
		assertEquals(signatures.size(), names.size());

		for (List<String> transaction : csv) {
			String nameString = transaction.get(0).replace("\"", "");
			String jsonString = transaction.get(1).replace("\"\"", "\"").replace(";", ",");
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

	private List<String> getZipNames(String zip) throws HederaClientException {
		List<String> zipNames = new ArrayList<>();
		try (ZipFile zipFile = new ZipFile(zip)) {
			Enumeration<? extends ZipEntry> zipEntries = zipFile.entries();
			while (zipEntries.hasMoreElements()) {
				zipNames.add(zipEntries.nextElement().getName());
			}
		} catch (IOException e) {
			throw new HederaClientException("Invalid zip");
		}
		return zipNames;
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