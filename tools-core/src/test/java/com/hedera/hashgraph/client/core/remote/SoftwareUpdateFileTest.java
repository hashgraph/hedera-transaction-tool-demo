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
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.text.Text;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Date;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

public class SoftwareUpdateFileTest extends TestBase {

	private static final Logger logger = LogManager.getLogger(SoftwareUpdateFileTest.class);

	@Before
	public void setUp() throws Exception {
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
	}

	@Test
	public void constructor_test() throws IOException {
		final var file = new File("src/test/resources/Files/TransactionTools-1.1.0-rc.1.pkg");
		var update = FileDetails.parse(file);

		var softwareUpdateFile = new SoftwareUpdateFile(update);
		assertNotNull(softwareUpdateFile);

		assertEquals("1.1.0", softwareUpdateFile.getVersion());
		assertEquals(new Timestamp(update.getAttributes().creationTime().toInstant()),
				softwareUpdateFile.getTimestamp());

		var digest = EncryptionUtils.getFileDigest(file);
		assertNotEquals("", softwareUpdateFile.getDigest());

		assertEquals("TransactionTools-1.1.0-rc.1", softwareUpdateFile.getBaseName());

		assertEquals(digest, softwareUpdateFile.getDigest().replace("\u00a0", " ").replace("\n", " "));

		var empty = new SoftwareUpdateFile();
		assertEquals("1.0.0", empty.getVersion());
		assertEquals("", empty.getDigest());
	}

	@Test
	public void expired_test() throws IOException {
		final var file = new File("src/test/resources/Files/TransactionTools-1.1.0-rc.1.pkg");
		var update = FileDetails.parse(file);

		var softwareUpdateFile = new SoftwareUpdateFile(update);
		softwareUpdateFile.setOldVersion("0.1");
		assertFalse(softwareUpdateFile.isExpired());

		softwareUpdateFile.setOldVersion("1.9.3");
		assertTrue(softwareUpdateFile.isExpired());

		softwareUpdateFile.setOldVersion("bad"); // a bad software version defaults to 0.0.0
		assertFalse(softwareUpdateFile.isExpired());

		softwareUpdateFile.setOldVersion("1.1");
		softwareUpdateFile.setOldStamp(0L);
		softwareUpdateFile.setNewStamp(new Date().getTime());
		assertFalse(softwareUpdateFile.isExpired());
		softwareUpdateFile.setOldStamp(new Date().getTime() + 123456L);
		assertTrue(softwareUpdateFile.isExpired());


	}

	@Test
	public void actions_test() throws IOException {
		final var file = new File("src/test/resources/Files/TransactionTools-1.1.0-rc.1.pkg");
		var update = FileDetails.parse(file);

		var softwareUpdateFile = new SoftwareUpdateFile(update);
		assertEquals(1, softwareUpdateFile.getActions().size());

	}

	@Test
	public void compareTo_test() throws IOException {
		final var file1 = new File("src/test/resources/Files/TransactionTools-1.1.0-rc.1.pkg");
		var update1 = FileDetails.parse(file1);
		var softwareUpdateFile1 = new SoftwareUpdateFile(update1);

		final var file2 = new File("src/test/resources/Files/TransactionTools-1.1.0-rc.1.pkg");
		var update2 = FileDetails.parse(file2);
		var softwareUpdateFile2 = new SoftwareUpdateFile(update2);

		assertEquals(0, softwareUpdateFile1.compareTo(softwareUpdateFile2));

		final var file3 = new File("src/test/resources/Files/TransactionTools-1.1.1.pkg");
		FileUtils.copyFile(file1, file3);
		var update3 = FileDetails.parse(file3);
		var softwareUpdateFile3 = new SoftwareUpdateFile(update3);

		assertEquals(-1, softwareUpdateFile1.compareTo(softwareUpdateFile3));
		assertEquals(1, softwareUpdateFile3.compareTo(softwareUpdateFile2));

		final var file5 = new File("src/test/resources/Files/TransactionTools-1.0.8.pkg");
		FileUtils.copyFile(file1, file5);
		var update5 = FileDetails.parse(file5);
		var softwareUpdateFile5 = new SoftwareUpdateFile(update5);

		assertEquals(1, softwareUpdateFile1.compareTo(softwareUpdateFile5));
		assertEquals(-1, softwareUpdateFile5.compareTo(softwareUpdateFile2));

		final var file6 = new File("src/test/resources/Files/TransactionTools-7.3.8.pkg");
		FileUtils.copyFile(file1, file6);
		var update6 = FileDetails.parse(file6);
		var softwareUpdateFile6 = new SoftwareUpdateFile(update6);

		assertEquals(-1, softwareUpdateFile1.compareTo(softwareUpdateFile6));
		assertEquals(1, softwareUpdateFile6.compareTo(softwareUpdateFile2));

		Files.deleteIfExists(file3.toPath());
		Files.deleteIfExists(file5.toPath());
		Files.deleteIfExists(file6.toPath());

	}

	@Test
	public void buildGridPane_test() throws IOException, HederaClientException {
		final var file = new File("src/test/resources/Files/TransactionTools-1.1.0-rc.1.pkg");
		var update = FileDetails.parse(file);

		var softwareUpdateFile = new SoftwareUpdateFile(update);

		var gridPane = softwareUpdateFile.buildGridPane();
		assertNotNull(gridPane);
		assertEquals(7, gridPane.getChildren().size());

		assertTrue(gridPane.getChildren().get(0) instanceof Label);
		assertEquals("This will upgrade your application to Version 1.1.0",
				((Label) gridPane.getChildren().get(0)).getText());

		assertTrue(gridPane.getChildren().get(6) instanceof HBox);
		var hBoxChildren = ((HBox) gridPane.getChildren().get(6)).getChildren();
		assertEquals(2, hBoxChildren.size());
		assertTrue(hBoxChildren.get(1) instanceof Text);
		assertEquals(softwareUpdateFile.getDigest(), ((Text) hBoxChildren.get(1)).getText());

		softwareUpdateFile.setHistory(true);

		FileUtils.copyFile(new File("src/test/resources/Files/TransactionTools-1.1.0-rc.1.meta"),
				new File(Constants.DEFAULT_HISTORY, "TransactionTools-1.1.0-rc.1.meta"));

		var metadataActions = new MetadataFile(softwareUpdateFile.getName()).getMetadataActions();
		var digest = metadataActions.get(0).getUserComments();
		assertEquals(digest, softwareUpdateFile.getDigest().replace("\u00a0", " ").replace("\n", " "));

		var gridPaneHistory = softwareUpdateFile.buildGridPane();
		assertNotNull(gridPaneHistory);
		assertEquals(7, gridPaneHistory.getChildren().size());


		assertTrue(gridPaneHistory.getChildren().get(0) instanceof Label);
		assertTrue(((Label) gridPaneHistory.getChildren().get(0)).getText().contains(
				"The application was updated to  Version 1.1.0 on "));

		assertTrue(gridPaneHistory.getChildren().get(6) instanceof HBox);
		var hBoxChildrenHistory = ((HBox) gridPaneHistory.getChildren().get(6)).getChildren();
		assertEquals(2, hBoxChildrenHistory.size());
		assertTrue(hBoxChildrenHistory.get(1) instanceof Text);
		assertEquals(softwareUpdateFile.getDigest(), ((Text) hBoxChildrenHistory.get(1)).getText());

		Files.deleteIfExists(new File(Constants.DEFAULT_HISTORY, "TransactionTools-1.1.0-rc.1.meta").toPath());
	}
}
