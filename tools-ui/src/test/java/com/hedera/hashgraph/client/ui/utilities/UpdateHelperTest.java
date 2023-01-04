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

package com.hedera.hashgraph.client.ui.utilities;

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.zeroturnaround.zip.ZipUtil;

import javax.swing.filechooser.FileSystemView;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Disabled("Temporarily disabling tests")
class UpdateHelperTest {

	private static final Logger logger = LogManager.getLogger(UpdateHelperTest.class);

	private static final String DOCUMENTS =
			FileSystemView.getFileSystemView().getDefaultDirectory().getPath() + File.separator + "Documents";
	private static final String TRANSACTION_TOOLS = "TransactionTools";
	public static final String OLD_TOOLS_FOLDER = "src/test/resources/UpdateTestResources/TransactionTools_Large";
	public static final String NEW_STYLE_TOOLS_FOLDER =
			"src/test/resources/UpdateTestResources/TransactionTools_new_Large";

	@BeforeEach
	void setUp() throws IOException {
		final var testDir = new File(DOCUMENTS, TRANSACTION_TOOLS + "_test");
		if (testDir.exists()) {
			FileUtils.deleteDirectory(testDir);
		}
		if (new File(DOCUMENTS, TRANSACTION_TOOLS).exists()) {
			FileUtils.moveDirectory(new File(DOCUMENTS, TRANSACTION_TOOLS), testDir);
		}
		FileUtils.copyDirectory(new File(OLD_TOOLS_FOLDER), new File(DOCUMENTS, TRANSACTION_TOOLS));
	}

	@AfterEach
	void tearDown() throws IOException {
		FileUtils.deleteDirectory(new File(DOCUMENTS, TRANSACTION_TOOLS));
		if (new File(DOCUMENTS, TRANSACTION_TOOLS + "_test").exists()) {
			FileUtils.moveDirectory(new File(DOCUMENTS, TRANSACTION_TOOLS + "_test"),
					new File(DOCUMENTS, TRANSACTION_TOOLS));
		}
	}

	@Test
	void isValid() throws IOException {
		UpdateHelper helper = new UpdateHelper("nonexistent");
		assertFalse(helper.isValid());
		assertFalse(helper.isUpdated());

		helper = new UpdateHelper(DOCUMENTS);
		assertTrue(helper.isValid());
		assertFalse(helper.isUpdated());

		helper = new UpdateHelper(DOCUMENTS, TRANSACTION_TOOLS);
		assertTrue(helper.isValid());
		assertFalse(helper.isUpdated());

		FileUtils.deleteDirectory(new File(DOCUMENTS, TRANSACTION_TOOLS));
		if (new File(NEW_STYLE_TOOLS_FOLDER).exists()) {
			FileUtils.copyDirectory(new File(NEW_STYLE_TOOLS_FOLDER), new File(DOCUMENTS, TRANSACTION_TOOLS));
		}

		helper = new UpdateHelper(DOCUMENTS, TRANSACTION_TOOLS);
		assertTrue(helper.isValid());
		assertTrue(helper.isUpdated());

	}

	@Test
	void handleAccounts_test() throws HederaClientException, IOException {
		final UpdateHelper helper = new UpdateHelper(DOCUMENTS, TRANSACTION_TOOLS);
		assertTrue(helper.isValid());
		assertFalse(helper.isUpdated());

		final var accountsFolder = DOCUMENTS + File.separator + TRANSACTION_TOOLS + File.separator + "Accounts";
		final var accountFolders =
				new File(accountsFolder).listFiles((dir, name) -> dir.isDirectory() && name.contains("."));
		assert accountFolders != null;
		final var keysBefore =
				new File(DOCUMENTS + File.separator + TRANSACTION_TOOLS + File.separator + "Keys").listFiles(
						File::isFile);
		assert keysBefore != null;

		helper.handleAccounts();

		final var infoFiles = new File(accountsFolder).listFiles(File::isFile);
		assert infoFiles != null;
		final var keysAfter =
				new File(DOCUMENTS + File.separator + TRANSACTION_TOOLS + File.separator + "Keys").listFiles(
						File::isFile);
		assert keysAfter != null;

		assertEquals(accountFolders.length, (infoFiles.length - 1) / 2);
		final var archive = new File(accountsFolder + File.separator + "Archive.zip");
		final var tempDir = Files.createTempDirectory("test");
		assertTrue(archive.exists());
		assertEquals(keysBefore.length + 4, keysAfter.length);

		ZipUtil.unpack(archive, tempDir.toFile());
		assertEquals(accountFolders.length, Objects.requireNonNull(tempDir.toFile().listFiles()).length - 1);
	}

	@Test
	void handleKeys_test() throws IOException {
		final UpdateHelper helper = new UpdateHelper(DOCUMENTS, TRANSACTION_TOOLS);
		assertTrue(helper.isValid());
		assertFalse(helper.isUpdated());
		assertTrue(new File(DOCUMENTS, TRANSACTION_TOOLS + File.separator + "Keys/recovery.aes").exists());
		assertFalse(new File(DOCUMENTS, TRANSACTION_TOOLS + File.separator + Constants.MNEMONIC_PATH).exists());

		helper.handleKeys();

		assertFalse(new File(DOCUMENTS, TRANSACTION_TOOLS + File.separator + "Keys/recovery.aes").exists());
		assertTrue(new File(DOCUMENTS, TRANSACTION_TOOLS + File.separator + Constants.MNEMONIC_PATH).exists());

	}

	@Test
	void handleHistory_test() throws Exception {
		final UpdateHelper helper = new UpdateHelper(DOCUMENTS, TRANSACTION_TOOLS);
		assertTrue(helper.isValid());
		assertFalse(helper.isUpdated());
		final var accountsFolder = DOCUMENTS + File.separator + TRANSACTION_TOOLS + File.separator + "History";
		final File[] history = new File(accountsFolder).listFiles(
				(dir, name) -> TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(name)));
		helper.handleHistory();
		final var archive =
				new File(DOCUMENTS + File.separator + TRANSACTION_TOOLS + File.separator + "Files/History_Archive.zip");
		assertTrue(archive.exists());
		final File[] history2 = new File(accountsFolder).listFiles(
				(dir, name) -> TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(name)));

		final var tempDir = Files.createTempDirectory("historyTest");
		ZipUtil.unpack(archive, tempDir.toFile());

		final var length = Objects.requireNonNull(tempDir.toFile().listFiles()).length > 0 ? Objects.requireNonNull(
				Objects.requireNonNull(tempDir.toFile().listFiles())[0].listFiles()).length : 0;

		assertEquals(Objects.requireNonNull(history).length, Objects.requireNonNull(history2).length + length);

	}


}
