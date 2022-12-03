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

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.Window;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.testfx.api.FxRobot;
import org.testfx.api.FxToolkit;
import org.testfx.framework.junit5.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javax.annotation.Nullable;
import javax.swing.JFileChooser;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.KEY_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.SALT_LENGTH;

public class TestBase extends ApplicationTest {
	public static final Path BASE_DIRECTORY = Paths.get("src","test","resources");
	public static final Path STORAGE_DIRECTORY = BASE_DIRECTORY.resolve("TransactionTools");
	public static final Path ACCOUNTS_DIRECTORY = STORAGE_DIRECTORY.resolve("Accounts");
	public static final Path HISTORY_DIRECTORY = STORAGE_DIRECTORY.resolve("History");
	public static final Path KEYS_DIRECTORY = STORAGE_DIRECTORY.resolve("Keys");
	public static final Path LOGS_DIRECTORY = STORAGE_DIRECTORY.resolve("Logs");
	public static final Path FILES_DIRECTORY = STORAGE_DIRECTORY.resolve("Files");
	public static final Path DELETED_ACCOUNTS_DIRECTORY = STORAGE_DIRECTORY.resolve(Paths.get("Deleted", "Accounts"));
	public static final Path SYSTEM_DIRECTORY = FILES_DIRECTORY.resolve(".System");

	public static final String KEYS_STRING = "Keys";
	public static final String ACCOUNTS_STRING = "Accounts";
	private static final FxRobot FX_ROBOT = new FxRobot();
	private static final Logger LOG = LogManager.getLogger(TestBase.class);

	@BeforeAll
	public static void setupHeadlessMode() {
		//Comment this line while testing on local system. All tests on circle ci should run headless.
		// TODO this can be an environment variable passed in from the action
//		System.setProperty("headless", "true");

		if (Boolean.getBoolean("headless")) {
			System.setProperty("testfx.robot", "glass");
			System.setProperty("testfx.headless", "true");
			System.setProperty("prism.order", "sw");
			System.setProperty("prism.text", "t2k");
			System.setProperty("java.awt.headless", "true");
			System.setProperty("headless.geometry", "1600x1200-64");
		}
	}

	@AfterAll
	public static void afterTest() throws Exception {
		ensureEventQueueComplete();
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();
	}

	public static void ensureEventQueueComplete() {
		WaitForAsyncUtils.waitForFxEvents(1);
	}

	/**
	 * Check if keys have been created using an old version of the app and fixes them to avoid timeouts
	 *
	 * @param defaultStorage
	 * 		Storage location
	 */
	public static void fixMissingMnemonicHashCode(final String defaultStorage) throws KeyStoreException, IOException {
		final File[] keyFiles = new File(defaultStorage, "Keys").listFiles((dir, name) -> name.endsWith("pem"));
		assert keyFiles != null;
		for (final File keyFile : keyFiles) {
			final Integer mnemonicHash =
					Ed25519KeyStore.getMnemonicHashCode(keyFile.getAbsolutePath());
			LOG.info("{} has hash {}", keyFile.getAbsolutePath(), mnemonicHash);
			if (mnemonicHash == null) {
				final BufferedWriter output =
						new BufferedWriter(new FileWriter(keyFile.getAbsolutePath(), true));
				output.append("Recovery Phrase Hash: -915976044");
				output.close();
				LOG.info("Added dummy hash to: {}", keyFile.getName());
			}
		}
	}

	@Override
	public void start(final Stage stage) {
		stage.show();
	}

	/* Helper method to retrieve Java FX GUI components. */
	public <T extends Node> T find(final String query) {

		try {
			return (T) lookup(query).queryAll().iterator().next();
		} catch (final Exception e) {
			return null;
		}
	}

	/* Helper method to retrieve Java FX GUI components. */
	public Set<Node> findAll(final String query) {
		return lookup(query).queryAll();
	}

	public boolean exists(final String query) {
		try {
			final Node x = lookup(query).queryAll().iterator().next();
			return x != null;
		} catch (final Exception e) {
			return false;
		}
	}

	// TODO this is a lot like the copy thing down below
	public void remakeTransactionTools() {
		final String toolsFolder =
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents/TransactionTools";
		if (!(new File(toolsFolder)).exists() && new File(toolsFolder).mkdirs()) {
			LOG.info("Folder {} created", toolsFolder);
		}

		try {
			if (!new File(toolsFolder, ACCOUNTS_STRING).exists() &&
					new File(toolsFolder, ACCOUNTS_STRING).mkdirs()) {
				LOG.info("Accounts folder created");
			}
			if (!new File(toolsFolder, KEYS_STRING).exists() &&
					new File(toolsFolder, KEYS_STRING).mkdirs()) {
				LOG.info("{} folder created", KEYS_STRING);
			}
		} catch (final Exception cause) {
			LOG.error("Unable to remake Transaction folders.", cause);
		}
	}

	/**
	 * Get a salt from the token
	 *
	 * @param token
	 * 		a string that contains a salt and a password salt
	 * @return the salt
	 */
	public byte[] getSalt(final String token, final boolean legacy) {

		if (legacy) {
			return new byte[SALT_LENGTH];
		}

		final var decoder = Base64.getDecoder();

		final var tokenBytes = decoder.decode(token);
		if (tokenBytes.length < Constants.SALT_LENGTH + KEY_LENGTH / 8) {
			LOG.error("Token size check failed");
		}
		return Arrays.copyOfRange(tokenBytes, 0, Constants.SALT_LENGTH);
	}

	public void ensureVisible(final Node node) {
		Node p = node.getParent();
		while (!(p instanceof ScrollPane)) {
			try {
				p = p.getParent();
			} catch (final Exception e) {
				//not inside a scroll pane
				LOG.error(e.getMessage());
				return;
			}
		}

		final var scrollPane = (ScrollPane) p;
		final var viewport = scrollPane.getViewportBounds();
		final var contentHeight =
				scrollPane.getContent().localToScene(scrollPane.getContent().getBoundsInLocal()).getHeight();
		final var nodeMinY = node.localToScene(node.getBoundsInLocal()).getMinY();
		final var nodeMaxY = node.localToScene(node.getBoundsInLocal()).getMaxY();

		double vValueDelta = 0;
		final var vValueCurrent = scrollPane.getVvalue();

		if (nodeMaxY < 0) {
			// currently, located above (remember, top left is (0,0))
			vValueDelta = (nodeMinY - viewport.getHeight()) / contentHeight;
		} else if (nodeMinY > viewport.getHeight()) {
			// currently, located below
			vValueDelta = (nodeMinY) / contentHeight;
		}
		scrollPane.setVvalue(vValueCurrent + vValueDelta);
	}

	/**
	 * Set up the required folder structure for the tools
	 *
	 * @param location
	 * 		root folder
	 */
	public static void setupTransactionDirectory(final String location) throws IOException {
		final File directory = new File(location);
		if (!directory.exists()) {
			if (!directory.mkdirs()) {
				LOG.info("Directory already exists");
			}
		}

		if (new File(String.format("%s/Files/UserFiles", location)).mkdirs()) {
			LOG.info("User files folder has been created");
		}
		if (new File(String.format("%s/Files/.System", location)).mkdirs()) {
			LOG.info("System files folder has been created");
		}
		if (new File(String.format("%s/Keys/Archive", location)).mkdirs()) {
			LOG.info("Keys archive folder has been created");
		}
		if (new File(String.format("%s/History/", location)).mkdirs()) {
			LOG.info("History folder has been created");
		}
		if (new File(String.format("%s/logs/", location)).mkdirs()) {
			LOG.info("Log folder has been created");
		}
		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.aes"),
				new File(location, "Files/.System/recovery.aes"));
	}





	/**
	 * Returns an {@link ObservableList} of all the {@link Node} objects
	 * for the first modal window found in the list of open windows.
	 *
	 * @return an ObservableList
	 */
	@Nullable
	public static ObservableList<Node> getPopupNodes() {
		final var actualAlertDialog = findModalWindow();
		if (actualAlertDialog != null) {
			final Node dialogPane = actualAlertDialog.getScene().getRoot();
			if (dialogPane != null) {
				if (dialogPane instanceof VBox || dialogPane instanceof HBox) {
					return ((Pane)dialogPane).getChildren();
				}
			}
		}
		return null;
	}

	/**
	 * Find the first modal window in the list of open windows.
	 *
	 * @return a Stage
	 */
	public static Stage findModalWindow() {
		// Get a list of windows but ordered from top[0] to bottom[n] ones.
		// It is needed to get the first found modal window.
		final List<Window> allWindows = new ArrayList<>(FX_ROBOT.robotContext().getWindowFinder().listWindows());
		Collections.reverse(allWindows);

		return (Stage) allWindows
				.stream()
				.filter(window -> window instanceof Stage)
				.filter(window -> ((Stage) window).getModality() == Modality.APPLICATION_MODAL)
				.findFirst()
				.orElse(null);
	}

	/**
	 * Build the file structure needed to run the tests.
	 *
	 * @throws IOException
	 */
	public static void buildFolders() throws IOException {
		deleteFolders();

		if (ACCOUNTS_DIRECTORY.toFile().mkdirs()) {
			LOG.info("Accounts folder created");
		}
		if (DELETED_ACCOUNTS_DIRECTORY.toFile().mkdirs()) {
			LOG.info("Deleted accounts folder created");
		}
		if (SYSTEM_DIRECTORY.toFile().mkdirs()) {
			LOG.info("System folder created");
		}
		if (HISTORY_DIRECTORY.toFile().mkdirs()) {
			LOG.info("History folder created");
		}
		if (KEYS_DIRECTORY.toFile().mkdirs()) {
			LOG.info("Keys folder created");
		}
		if (LOGS_DIRECTORY.toFile().mkdirs()) {
			LOG.info("Logs folder created");
		}
	}

	/**
	 * Clear the storage directory.
	 *
	 * @throws IOException
	 */
	public static void deleteFolders() throws IOException {
		var dir = STORAGE_DIRECTORY.toFile();
		if (dir.exists()) {
			FileUtils.deleteDirectory(dir);
		}
	}

	// TODO I don't know what this was to accomplish. Once I figure it out, then fix it
//	//	public static void copyCreatePaneKeys() {
////		final var createPaneFolderSuffix = "CreatePaneTest";
////		final var testResourceFolder = "/src/test/resources";
////		final var createPaneFolder =
////				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents" + File.separator + createPaneFolderSuffix;
////		final var testDirectory = new File(createPaneFolder);
////		if (!(testDirectory).exists() && testDirectory.mkdirs()) {
////			logger.info("Test folder created");
////		}
////		final var keysDirectory = new File(testDirectory, "Keys");
////		if (!(keysDirectory).exists() && keysDirectory.mkdirs()) {
////			logger.info("Keys folder created");
////		}
////
////		final var sourceCreatePaneTestDirectory = Paths.get(
////				"").toAbsolutePath() + testResourceFolder + File.separator + createPaneFolderSuffix + File.separator +
////				"Keys";
////		logger.info("Test keys directory : {}", sourceCreatePaneTestDirectory);
////	}
//	these aren't the same, what is going on here??'
//	// defaultdirectory?/Documents/CreatePaneTest/Keys
//	// /src/test/resources/CreatePaneTest/Keys
//
//	this is my first attempt, without knowing what it is for
//	/**
//	 * Initialize keys folder with test keys.
//	 */
//	public static void copyCreatePaneKeys() {
//		final var createPaneFolderSuffix = "CreatePaneTest";
//		// TODO why use the default directory and not a predefined one within resources?
//		final var createPaneFolder =
//				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents" + File.separator + createPaneFolderSuffix;
//		final var testDirectory = new File(createPaneFolder);
//		if (!(testDirectory).exists() && testDirectory.mkdirs()) {
////			LOG.info("Test folder created");
//		}
//		final var keysDirectory = new File(testDirectory, "Keys");
//		if (!(keysDirectory).exists() && keysDirectory.mkdirs()) {
////			LOG.info("Keys folder created");
//		}
//
//		final var sourceCreatePaneTestDirectory = BASE_DIRECTORY.resolve(
//				Paths.get(createPaneFolderSuffix, "Keys"));
////		LOG.info("Test keys directory : {}", sourceCreatePaneTestDirectory);
//	}
}
