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

package com.hedera.hashgraph.client.core.remote;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import javafx.scene.control.CheckBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

public class BundleFileTest extends TestBase {
	private static final Logger logger = LogManager.getLogger(BundleFileTest.class);

	@BeforeEach
	public void setUp() throws Exception {
		FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
		if (new File(ACCOUNTS_MAP_FILE).mkdirs()) {
			logger.info("System folder created");
		}
		Files.deleteIfExists(Path.of(ACCOUNTS_MAP_FILE));
		Files.copy(Path.of("src/test/resources/accountMapFile.json"), Path.of(ACCOUNTS_MAP_FILE));

		if (new File(ACCOUNTS_INFO_FOLDER).exists()) {
			FileUtils.deleteDirectory(new File(ACCOUNTS_INFO_FOLDER));
		}

		if (new File(ACCOUNTS_INFO_FOLDER).mkdirs()) {
			logger.info("Accounts info folder created");
		}
		FileUtils.copyDirectory(new File("src/test/resources/AccountInfos"), new File(ACCOUNTS_INFO_FOLDER));
		logger.info("Info files copied");
		Files.move(Path.of(ACCOUNTS_INFO_FOLDER, "0.0.1.info"), Path.of(ACCOUNTS_INFO_FOLDER, "zero1.info"));
		Files.move(Path.of(ACCOUNTS_INFO_FOLDER, "0.0.2.info"), Path.of(ACCOUNTS_INFO_FOLDER, "treasury.info"));
		Files.move(Path.of(ACCOUNTS_INFO_FOLDER, "0.0.3.info"), Path.of(ACCOUNTS_INFO_FOLDER, "node1.info"));
		Files.move(Path.of(ACCOUNTS_INFO_FOLDER, "0.0.5.info"), Path.of(ACCOUNTS_INFO_FOLDER, "node3.info"));
		Files.move(Path.of(ACCOUNTS_INFO_FOLDER, "0.0.6.info"), Path.of(ACCOUNTS_INFO_FOLDER, "node4.info"));

	}

	@AfterEach
	public void tearDown() throws Exception {
		FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
	}

	@Test
	public void constructor_test() throws IOException, HederaClientException {
		var file = new File("src/test/resources/Files/bundleFileTests/mixed.zip");
		var details = FileDetails.parse(file);

		BundleFile bundleFile = new BundleFile(details);
		assertEquals(6, bundleFile.getPublicKeyMap().size());
		assertEquals(5, bundleFile.getAccountInfoMap().size());

		file = new File("src/test/resources/Files/bundleFileTests/infoBundle.zip");
		details = FileDetails.parse(file);

		bundleFile = new BundleFile(details);
		assertEquals(0, bundleFile.getPublicKeyMap().size());
		assertEquals(5, bundleFile.getAccountInfoMap().size());

		file = new File("src/test/resources/Files/bundleFileTests/publicKeyBundle.zip");
		details = FileDetails.parse(file);

		bundleFile = new BundleFile(details);
		assertEquals(6, bundleFile.getPublicKeyMap().size());
		assertEquals(0, bundleFile.getAccountInfoMap().size());

		file = new File("src/test/resources/Files/bundleFileTests/testBundle.zip");
		details = FileDetails.parse(file);

		bundleFile = new BundleFile(details);
		assertEquals(6, bundleFile.getPublicKeyMap().size());
		assertEquals(5, bundleFile.getAccountInfoMap().size());
	}

	@Test
	public void buildGridpane_test() throws HederaClientException {
		var file = new File("src/test/resources/Files/bundleFileTests/mixed.zip");
		var details = FileDetails.parse(file);

		var children = new BundleFile(details).buildGridPane().getChildren();

		assertFalse(children.isEmpty());
		assertEquals(6, children.size());
		assertTrue(children.get(5) instanceof CheckBox);
		assertTrue(((CheckBox) children.get(5)).isSelected());

		file = new File("src/test/resources/Files/bundleFileTests/infoBundle.zip");
		details = FileDetails.parse(file);

		children = new BundleFile(details).buildGridPane().getChildren();
		assertFalse(children.isEmpty());
		assertEquals(3, children.size());
		assertTrue(children.get(2) instanceof CheckBox);
		assertTrue(((CheckBox) children.get(2)).isSelected());

		file = new File("src/test/resources/Files/bundleFileTests/publicKeyBundle.zip");
		details = FileDetails.parse(file);

		children = new BundleFile(details).buildGridPane().getChildren();
		assertFalse(children.isEmpty());
		assertEquals(3, children.size());

		file = new File("src/test/resources/Files/bundleFileTests/testBundle.zip");
		details = FileDetails.parse(file);

		children = new BundleFile(details).buildGridPane().getChildren();
		assertFalse(children.isEmpty());
		assertEquals(6, children.size());
		assertTrue(children.get(5) instanceof CheckBox);
		assertTrue(((CheckBox) children.get(5)).isSelected());
	}

	@Test
	public void nickname_test() throws HederaClientException {
		final var file = new File("src/test/resources/Files/bundleFileTests/mixed.zip");
		final var details = FileDetails.parse(file);

		final BundleFile bundleFile = new BundleFile(details);
		for (final Map.Entry<BundleFile.InfoKey, File> entry : bundleFile.getAccountInfoMap().entrySet()) {
			if (entry.getValue().getName().equals("AccountOne.info")) {
				assertEquals("AccountOne (0.0.1-dfkxr) -> replaces the previous nickname: \"zero1\"",
						entry.getKey().toString());
			}
			if (entry.getValue().getName().equals("0.0.2.info")) {
				assertEquals("Treasury test (0.0.2-lpifi) -> replaces the previous nickname: \"treasury\"",
						entry.getKey().toString());
			}
			if (entry.getValue().getName().equals("node1.info")) {
				assertEquals("node1 (0.0.3-tzfmz)", entry.getKey().toString());
			}
			if (entry.getValue().getName().equals("0.0.4.info")) {
				assertEquals("0.0.4 (0.0.4-cjcuq)", entry.getKey().toString());
			}
			if (entry.getValue().getName().equals("AnotherNode.info")) {
				assertEquals("AnotherNode (0.0.6-tcxjy) -> replaces the previous nickname: \"node4\"",
						entry.getKey().toString());
			}
		}


	}

	@Test
	public void equals_test() throws HederaClientException {
		final var mixed = new File("src/test/resources/Files/bundleFileTests/mixed.zip");
		final var mixedDetails = FileDetails.parse(mixed);

		final BundleFile mixedBundle = new BundleFile(mixedDetails);

		final var test = new File("src/test/resources/Files/bundleFileTests/testBundle.zip");
		final var testDetails = FileDetails.parse(test);

		final BundleFile testBundle = new BundleFile(testDetails);

		assertEquals(mixedBundle, testBundle);
		assertNotEquals(mixedBundle.hashCode(), testBundle.hashCode());

		final var other = new File("src/test/resources/Files/bundleFileTests/publicKeyBundle.zip");
		final var otherDetails = FileDetails.parse(other);
		final BundleFile otherBundle = new BundleFile(otherDetails);
		assertNotEquals(mixedBundle, otherBundle);
		assertNotEquals(mixedBundle.hashCode(), otherBundle.hashCode());

		assertEquals(-1133878251, mixedBundle.hashCode());
		assertEquals(1500381343, otherBundle.hashCode());
	}

	@Test
	public void bug221_test() throws HederaClientException {
		final var infoBug = new File("src/test/resources/Files/bundleFileTests/info.zip");
		final var fileDetails = FileDetails.parse(infoBug);

		final BundleFile bundleFile = new BundleFile(fileDetails);
		assertEquals(9, bundleFile.getInfos().size());

	}
}
