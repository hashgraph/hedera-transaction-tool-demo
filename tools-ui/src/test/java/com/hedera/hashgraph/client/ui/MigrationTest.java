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

package com.hedera.hashgraph.client.ui;

import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.pages.AccountsPanePage;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.testfx.api.FxToolkit;
import org.zeroturnaround.zip.ZipUtil;

import javax.swing.filechooser.FileSystemView;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUBLIC_KEY_LOCATION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class MigrationTest extends TestBase {

	private static final String DOCUMENTS =
			FileSystemView.getFileSystemView().getDefaultDirectory().getPath() + File.separator + "Documents";
	private static final String TRANSACTION_TOOLS = "TransactionTools";
	public static final String TRANSACTION_TOOL_PATH = DOCUMENTS + File.separator + TRANSACTION_TOOLS + File.separator;
	public static final String OLD_TOOLS_FOLDER = "src/test/resources/UpdateTestResources/TransactionTools_Large";

	@Before
	public void setUp() throws IOException {
		final var testDir = new File(DOCUMENTS, TRANSACTION_TOOLS + "_test");
		if (testDir.exists()) {
			FileUtils.deleteDirectory(testDir);
		}
		if (new File(DOCUMENTS, TRANSACTION_TOOLS).exists()) {
			FileUtils.moveDirectory(new File(DOCUMENTS, TRANSACTION_TOOLS), testDir);
		}
		FileUtils.copyDirectory(new File(OLD_TOOLS_FOLDER), new File(DOCUMENTS, TRANSACTION_TOOLS));

		final var properties = new UserAccessibleProperties(TRANSACTION_TOOL_PATH + "Files/user.properties", "");
		properties.setPreferredStorageDirectory(TRANSACTION_TOOL_PATH);
		properties.setSetupPhase(SetupPhase.TEST_PHASE);

		final Controller controller = new Controller();
		final var version = controller.getVersion();
		properties.setVersionString(version);

		final Path currentRelativePath = Paths.get("");
		final Map<String, String> emailMap = new HashMap<>();
		emailMap.put(currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents/",
				"test1.council2@hederacouncil.org");

		properties.setOneDriveCredentials(emailMap);

	}

	@After
	public void tearDown() throws Exception {
		FileUtils.deleteDirectory(new File(DOCUMENTS, TRANSACTION_TOOLS));
	}

	@Test
	public void handleMigration_test() throws Exception {

		final var accountsFolder = TRANSACTION_TOOL_PATH + "Accounts";
		final var accountFolders = new File(accountsFolder).listFiles(
				pathname -> pathname.isDirectory() && pathname.getName().contains("."));
		assertNotNull(accountFolders);

		final var tTools = new File(DOCUMENTS, TRANSACTION_TOOLS);
		assertTrue(tTools.exists());
		assertFalse(new File(tTools.getAbsolutePath(), "Files/.System").exists());


		final var sizeBefore = Objects.requireNonNull(
				new File(TRANSACTION_TOOL_PATH, "History").listFiles(
						(dir, name) -> TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(name)))).length;


		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		assertTrue(new File(tTools.getAbsolutePath(), "Files/.System").exists());

		final var accountFoldersAfter = new File(accountsFolder).listFiles(File::isDirectory);
		assertNotNull(accountFoldersAfter);
		assertEquals(0, accountFoldersAfter.length);

		final AccountsPanePage accountsPanePage = new AccountsPanePage(this);
		final MainWindowPage mainWindowPage = new MainWindowPage(this);

		mainWindowPage.clickOnAccountsButton();
		final var accountFiles = new File(accountsFolder).listFiles(
				(dir, name) -> INFO_EXTENSION.equals(FilenameUtils.getExtension(name)));
		assertNotNull(accountFiles);

		assertEquals(accountFolders.length, accountFiles.length);

		final var accounts = accountsPanePage.getAccounts();
		assertEquals(accounts.size(), accountFolders.length);

		final var sizeAfter = Objects.requireNonNull(
				new File(TRANSACTION_TOOL_PATH, "History").listFiles(
						(dir, name) -> TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(name)))).length;

		final var archive = new File(TRANSACTION_TOOL_PATH + "Files/History_Archive.zip");
		assertTrue(archive.exists());
		final var tempDir = Files.createTempDirectory("historyTest");
		ZipUtil.unpack(archive, tempDir.toFile());

		final var after =
				sizeAfter + (Objects.requireNonNull(tempDir.toFile().listFiles()).length > 0 ? Objects.requireNonNull(
						Objects.requireNonNull(tempDir.toFile().listFiles())[0].listFiles()).length : 0);

		assertEquals(sizeBefore, after);
		assertTrue(new File(DEFAULT_STORAGE, PUBLIC_KEY_LOCATION).exists());
	}
}
