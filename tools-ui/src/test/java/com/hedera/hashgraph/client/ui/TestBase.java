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
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.stage.Stage;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.testfx.api.FxToolkit;
import org.testfx.framework.junit5.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javax.swing.JFileChooser;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.security.KeyStoreException;
import java.util.Arrays;
import java.util.Base64;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.KEY_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.SALT_LENGTH;

public class TestBase extends ApplicationTest {

	public static final String KEYS_STRING = "Keys";
	public static final String ACCOUNTS_STRING = "Accounts";
	private static final Logger logger = LogManager.getLogger(TestBase.class);

	@BeforeClass
	public static void setupHeadlessMode() {
		//Comment this line while testing on local system. All tests on circle ci should run headless.
		//System.setProperty("headless", "true");

		if (Boolean.getBoolean("headless")) {
			System.setProperty("testfx.robot", "glass");
			System.setProperty("testfx.headless", "true");
			System.setProperty("prism.order", "sw");
			System.setProperty("prism.text", "t2k");
			System.setProperty("java.awt.headless", "true");
			System.setProperty("headless.geometry", "1600x1200-64");
		}
	}


	//@AfterAll
	@AfterClass
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
			logger.info("{} has hash {}", keyFile.getAbsolutePath(), mnemonicHash);
			if (mnemonicHash == null) {
				final BufferedWriter output =
						new BufferedWriter(new FileWriter(keyFile.getAbsolutePath(), true));
				output.append("Recovery Phrase Hash: -915976044");
				output.close();
				logger.info("Added dummy hash to: {}", keyFile.getName());
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

	public void remakeTransactionTools() {
		final String toolsFolder =
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents/TransactionTools";
		if (!(new File(toolsFolder)).exists() && new File(toolsFolder).mkdirs()) {
			logger.info("Folder {} created", toolsFolder);
		}

		try {
			if (!new File(toolsFolder, ACCOUNTS_STRING).exists() &&
					new File(toolsFolder, ACCOUNTS_STRING).mkdirs()) {
				logger.info("Accounts folder created");
			}
			if (!new File(toolsFolder, KEYS_STRING).exists() &&
					new File(toolsFolder, KEYS_STRING).mkdirs()) {
				logger.info("{} folder created", KEYS_STRING);
			}
		} catch (final Exception cause) {
			logger.error("Unable to remake Transaction folders.", cause);
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
			logger.error("Token size check failed");
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
				logger.error(e.getMessage());
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
				logger.info("Directory already exists");
			}
		}

		if (new File(String.format("%s/Files/UserFiles", location)).mkdirs()) {
			logger.info("User files folder has been created");
		}
		if (new File(String.format("%s/Files/.System", location)).mkdirs()) {
			logger.info("System files folder has been created");
		}
		if (new File(String.format("%s/Keys/Archive", location)).mkdirs()) {
			logger.info("Keys archive folder has been created");
		}
		if (new File(String.format("%s/History/", location)).mkdirs()) {
			logger.info("History folder has been created");
		}
		if (new File(String.format("%s/logs/", location)).mkdirs()) {
			logger.info("Log folder has been created");
		}
		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.aes"),
				new File(location, "Files/.System/recovery.aes"));
	}
}
