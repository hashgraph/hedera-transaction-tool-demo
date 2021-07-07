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

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Collections;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class InfoFileTest extends TestBase {
	private static final Logger logger = LogManager.getLogger(InfoFileTest.class);

	@Before
	public void setUp() throws Exception {
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
		FileUtils.copyFile(new File("src/test/resources/Files/0.0.2.meta"),
				new File(DEFAULT_HISTORY, "0.0.2.meta"));

		if (new File(Constants.DEFAULT_ACCOUNTS).mkdirs()) {
			logger.info("Accounts folder created");
		}
		Files.deleteIfExists(new File(DEFAULT_ACCOUNTS, "0.0.2.info").toPath());
	}

	@Test
	public void constructor_test() throws IOException {
		final var file = new File("src/test/resources/Files/0.0.2.info");
		var info = FileDetails.parse(file);

		var infoFile = new InfoFile(info);
		assertNotNull(infoFile);
		assertTrue(infoFile.isValid());
		assertEquals("0.0.2", infoFile.getAccountID().toReadableString());
		assertEquals("0.0.2", infoFile.getBaseName());
		assertEquals(FilenameUtils.getFullPath(file.getAbsolutePath()), infoFile.getParentPath() + File.separator);
		assertEquals(28, EncryptionUtils.flatPubKeys(Collections.singletonList(infoFile.getKey())).size());
		assertTrue(infoFile.getActions().contains(FileActions.ACCEPT));
		assertTrue(infoFile.getActions().contains(FileActions.DECLINE));

		var badFile = new File("src/test/resources/Files/0.0.2.meta");
		var badInfo = FileDetails.parse(badFile);
		assertFalse(new InfoFile(badInfo).isValid());

	}

	@Test
	public void buildGridPane_test() throws IOException {
		final var file = new File("src/test/resources/Files/0.0.2.info");
		var info = FileDetails.parse(file);

		var infoFile = new InfoFile(info);
		var gridPane = infoFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		var label = (Label) gridPane.getChildren().get(0);
		assertEquals("Would you like to import information regarding account 0.0.2 to your records?", label.getText());

		FileUtils.copyFile(new File("src/test/resources/Files/0.0.2.info"),
				new File(DEFAULT_ACCOUNTS, "0.0.2.info"));

		infoFile = new InfoFile(info);
		gridPane = infoFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		label = (Label) gridPane.getChildren().get(0);
		assertEquals(
				"We have found new information regarding account 0.0.2. Would you like to import it to your records?",
				label.getText());

		assertTrue(infoFile.duplicate());
		assertTrue(infoFile.isExpired());

		infoFile.setHistory(true);
		gridPane = infoFile.buildGridPane();
		assertEquals(1, gridPane.getColumnCount());
		assertEquals(1, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		label = (Label) gridPane.getChildren().get(0);
		assertTrue(label.getText().contains("Information regarding account 0.0.2.info was accepted on 2021-01-07 "));

	}

	@After
	public void tearDown() throws Exception {
		Files.delete(new File(DEFAULT_HISTORY + File.separator + "0.0.2.meta").toPath());
	}
}
