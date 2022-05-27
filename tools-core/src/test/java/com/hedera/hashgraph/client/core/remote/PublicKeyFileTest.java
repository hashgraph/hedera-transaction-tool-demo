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

import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import javafx.scene.control.Label;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_KEYS;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class PublicKeyFileTest extends TestBase {
	private static final Logger logger = LogManager.getLogger(InfoFileTest.class);

	@Before
	public void setUp() throws Exception {
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
		FileUtils.copyFile(new File("src/test/resources/Files/genesis.meta"),
				new File(DEFAULT_HISTORY, "genesis.meta"));

		if (new File(DEFAULT_KEYS).mkdirs()) {
			logger.info("Accounts folder created");
		}
		Files.deleteIfExists(new File(DEFAULT_KEYS, "genesis.pub").toPath());
		Files.deleteIfExists(new File(DEFAULT_KEYS, "genesis.pem").toPath());
	}

	@Test
	public void constructor_test() throws IOException, HederaClientException {
		final var file = new File("src/test/resources/Keys/genesis.pub");
		final var info = FileDetails.parse(file);

		final var publicKeyFile = new PublicKeyFile(info);
		assertNotNull(publicKeyFile);
		assertTrue(publicKeyFile.isValid());

		assertEquals(2, publicKeyFile.getActions().size());
		assertTrue(publicKeyFile.getActions().contains(FileActions.ACCEPT));
		assertTrue(publicKeyFile.getActions().contains(FileActions.DECLINE));


		final var badFile = new File("src/test/resources/Files/0.0.2.meta");
		final var badInfo = FileDetails.parse(badFile);
		assertFalse(new PublicKeyFile(badInfo).isValid());

	}

	@Test
	public void buildGridPane_test() throws IOException, HederaClientException {
		final var file = new File("src/test/resources/Keys/genesis.pub");
		final var info = FileDetails.parse(file);

		var publicKeyFile = new PublicKeyFile(info);
		assertFalse(publicKeyFile.duplicate());

		var gridPane = publicKeyFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		var label = (Label) gridPane.getChildren().get(0);
		assertEquals("Would you like to import the following public key: genesis.pub", label.getText());

		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pub"),
				new File(DEFAULT_KEYS, "genesis.pub"));

		publicKeyFile = new PublicKeyFile(info);
		gridPane = publicKeyFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		label = (Label) gridPane.getChildren().get(0);
		assertEquals(
				"Would you like to replace genesis.pub with a new version?",
				label.getText());

		assertTrue(publicKeyFile.duplicate());
		assertTrue(publicKeyFile.isExpired());

		publicKeyFile.setHistory(true);
		gridPane = publicKeyFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		label = (Label) gridPane.getChildren().get(0);
		assertTrue(label.getText().contains("Information regarding key genesis.pub was accepted on 2021-01-07 "));
	}

	@Test
	public void duplicate_test() throws IOException, HederaClientException {
		final var file = new File("src/test/resources/Keys/genesis.pub");
		final var info = FileDetails.parse(file);


		var publicKeyFile = new PublicKeyFile(info);
		assertFalse(publicKeyFile.duplicate());
		assertFalse(publicKeyFile.isExpired());

		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pub"),
				new File(DEFAULT_KEYS, "genesis.pub"));
		FileUtils.copyFile(new File("src/test/resources/Keys/genesis.pem"),
				new File(DEFAULT_KEYS, "genesis.pem"));

		publicKeyFile = new PublicKeyFile(info);
		assertTrue(publicKeyFile.duplicate());
		assertTrue(publicKeyFile.isExpired());
	}

	@After
	public void tearDown() throws Exception {
		Files.deleteIfExists(new File(DEFAULT_HISTORY, "genesis.meta").toPath());
		Files.deleteIfExists(new File(DEFAULT_KEYS, "genesis.pub").toPath());
		Files.deleteIfExists(new File(DEFAULT_KEYS, "genesis.pub").toPath());
	}
}
