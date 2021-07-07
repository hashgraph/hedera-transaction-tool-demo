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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.ui;

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.ui.pages.HomePanePage;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.MAIN_TRANSACTIONS_SCROLLPANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NEW_FILES_VBOX;
import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

@Ignore
public class HomePaneHistoryTest extends TestBase implements GenericFileReadWriteAware {

	private final Path currentRelativePath = Paths.get("");
	private static final String MNEMONIC_PATH = "/Keys/recovery.aes";
	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;
	public UserAccessibleProperties properties;
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private final List<VBox> publicKeyBoxes = new ArrayList<>();
	private final List<VBox> accountInfoBoxes = new ArrayList<>();
	private final List<VBox> batchBoxes = new ArrayList<>();
	private final List<VBox> transactionBoxes = new ArrayList<>();
	private final List<VBox> softwareBoxes = new ArrayList<>();
	private final List<VBox> systemBoxes = new ArrayList<>();
	private final List<VBox> fileUpdateBoxes = new ArrayList<>();

	HomePanePage homePanePage;

	@Before
	public void setUp() throws Exception {

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		if (new File(DEFAULT_STORAGE).mkdirs()) {
			logger.info("Transaction tools directory created");
		}

		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original"), new File(DEFAULT_STORAGE));
		FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + KEYS_STRING));
		FileUtils.deleteDirectory(new File(DEFAULT_STORAGE + "/Accounts/0.0.56"));

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");

		if (new File(currentRelativePath.toAbsolutePath() + "/src/test/resources/testDirectory" +
				"/TransactionTools/Keys/").mkdirs()) {
			logger.info("Keys path created");
		}


		// Special case for test: Does not ask for password during setup
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		final var pathname =
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org/";

		if (new File(pathname).exists()) {
			FileUtils.deleteDirectory(new File(pathname));
		}

		if (new File(pathname).mkdirs()) {
			logger.info("Output directory created");
		}

		Map<String, String> emailMap = new HashMap<>();
		emailMap.put(
				currentRelativePath.toAbsolutePath() + "/src/test/resources/Transactions - Documents - " +
						"empty/",
				"test1.council2@hederacouncil.org");

		properties.setOneDriveCredentials(emailMap);

		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);
		setupTransactionDirectory(DEFAULT_STORAGE);

		FileUtils.copyFile(new File("src/test/resources/storedMnemonic.txt"),
				new File(DEFAULT_STORAGE + MNEMONIC_PATH));


		if (new File(DEFAULT_STORAGE + "History").exists()) {
			FileUtils.cleanDirectory(new File(DEFAULT_STORAGE + "History"));
			FileUtils.copyDirectory(new File("src/test/resources/History-large"), new File(DEFAULT_STORAGE + "History"
			));
		}

		Arrays.stream(Objects.requireNonNull(new File(DEFAULT_STORAGE + "History").listFiles(
				(dir, name) -> !name.endsWith("meta")))).map(File::getAbsolutePath).forEachOrdered(logger::info);

		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/principalTestingKey.pub"));

		TestBase.fixMissingMnemonicHashCode(DEFAULT_STORAGE);

		var controller = new Controller();
		var version = controller.getVersion();
		properties.setVersionString(version);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		homePanePage = new HomePanePage(this);

		var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		separateBoxes(newFiles, publicKeyBoxes, accountInfoBoxes, batchBoxes, transactionBoxes, softwareBoxes,
				systemBoxes, fileUpdateBoxes);

		assertEquals(newFiles.size(),
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size() + softwareBoxes.size() + systemBoxes.size() + fileUpdateBoxes.size());


	}

	@After
	public void tearDown() throws IOException {
		publicKeyBoxes.clear();
		accountInfoBoxes.clear();
		batchBoxes.clear();
		transactionBoxes.clear();
		softwareBoxes.clear();
		systemBoxes.clear();

		var currentRelativePath = Paths.get("");
		var s = currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/testDirectory";
		if ((new File(s)).exists()) {
			FileUtils.deleteDirectory(new File(s));
		}

		var out =
				currentRelativePath.toAbsolutePath().toString() + "/src/test/resources/Transactions - " +
						"Documents/OutputFiles/test1.council2@hederacouncil.org";
		if (new File(out).exists()) {
			FileUtils.cleanDirectory(new File(out));
		}

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}

	@Test
	public void historyTest() {
		var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		assertEquals(0, newFiles.size());

		var historyFiles = ((VBox) find("#historyFilesViewVBox")).getChildren();
		assertTrue(historyFiles.get(1) instanceof HBox);
		assertTrue(historyFiles.get(historyFiles.size() - 1) instanceof HBox);
		var topBox = (HBox) historyFiles.get(1);
		var pagesBox = (HBox) historyFiles.get(historyFiles.size() - 1);
		var historyVBox = (VBox) historyFiles.get(historyFiles.size() - 2);

		var files = historyVBox.getChildren();

		separateBoxes(files, publicKeyBoxes, accountInfoBoxes, batchBoxes, transactionBoxes, softwareBoxes,
				systemBoxes, fileUpdateBoxes);

		assertEquals(10,
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size() + softwareBoxes.size() + systemBoxes.size() + fileUpdateBoxes.size());

		var top = ((HBox) find("#chooseLength")).getChildren();
		var one = findButton(1, top);
		var five = findButton(5, top);
		var ten = findButton(10, top);
		var twenty = findButton(20, top);

		assertNotNull(one);
		assertNotNull(five);
		assertNotNull(ten);
		assertNotNull(twenty);

		var bottom = pagesBox.getChildren();
		assertEquals(3, bottom.size());
		assert (bottom.get(0) instanceof Button);
		assert (bottom.get(1) instanceof Button);
		assert (bottom.get(2) instanceof Button);

		clickOn(twenty);

		historyFiles = ((VBox) find("#historyFilesViewVBox")).getChildren();
		assertTrue(historyFiles.get(1) instanceof HBox);
		assertTrue(historyFiles.get(historyFiles.size() - 1) instanceof HBox);
		topBox = (HBox) historyFiles.get(1);
		pagesBox = (HBox) historyFiles.get(historyFiles.size() - 1);
		historyVBox = (VBox) historyFiles.get(historyFiles.size() - 2);
		files = historyVBox.getChildren();
		separateBoxes(files, publicKeyBoxes, accountInfoBoxes, batchBoxes, transactionBoxes, softwareBoxes,
				systemBoxes, fileUpdateBoxes);

		assertEquals(20,
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size() + softwareBoxes.size() + systemBoxes.size() + fileUpdateBoxes.size());

		top = ((HBox) find("#chooseLength")).getChildren();
		one = findButton(1, top);
		five = findButton(5, top);
		ten = findButton(10, top);
		twenty = findButton(20, top);

		assertNotNull(one);
		assertNotNull(five);
		assertNotNull(ten);
		assertNotNull(twenty);

		bottom = pagesBox.getChildren();
		assertEquals(2, bottom.size());
		assert (bottom.get(0) instanceof Button);
		assert (bottom.get(1) instanceof Button);

		ensureVisible(find(MAIN_TRANSACTIONS_SCROLLPANE), bottom.get(1));
		clickOn(bottom.get(1));

		historyFiles = ((VBox) find("#historyFilesViewVBox")).getChildren();
		assertTrue(historyFiles.get(1) instanceof HBox);
		assertTrue(historyFiles.get(historyFiles.size() - 1) instanceof HBox);
		historyVBox = (VBox) historyFiles.get(historyFiles.size() - 2);

		files = historyVBox.getChildren();

		for (var file : files) {
			if (!(file instanceof VBox)) {
				continue;
			}
			final var node = ((VBox) file).getChildren().get(0);
			assertTrue(node instanceof Label);
			logger.info(String.format("Currently shown box: %s", ((Label) node).getText()));
		}

		separateBoxes(files, publicKeyBoxes, accountInfoBoxes, batchBoxes, transactionBoxes, softwareBoxes,
				systemBoxes, fileUpdateBoxes);

		assertEquals(3,
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size() + softwareBoxes.size() + systemBoxes.size() + fileUpdateBoxes.size());

		topBox = (HBox) historyFiles.get(1);
		top = ((HBox) find("#chooseLength")).getChildren();
		one = findButton(1, top);
		clickOn(one);

		assertTrue(historyFiles.get(1) instanceof HBox);
		assertTrue(historyFiles.get(historyFiles.size() - 1) instanceof HBox);
		assertEquals(4, historyFiles.size());

	}

	@Test
	public void filterTest() {
		var newFiles = ((VBox) find(NEW_FILES_VBOX)).getChildren();
		assertEquals(0, newFiles.size());

		var historyFiles = ((VBox) find("#historyFilesViewVBox")).getChildren();
		assertTrue(historyFiles.get(1) instanceof HBox);
		assertTrue(historyFiles.get(historyFiles.size() - 1) instanceof HBox);
		var topBox = (HBox) historyFiles.get(1);
		HBox pagesBox;
		var historyVBox = (VBox) historyFiles.get(historyFiles.size() - 2);

		var files = historyVBox.getChildren();
		separateBoxes(files, publicKeyBoxes, accountInfoBoxes, batchBoxes, transactionBoxes, softwareBoxes,
				systemBoxes, fileUpdateBoxes);

		assertEquals(10,
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size() + softwareBoxes.size() + systemBoxes.size(),
				fileUpdateBoxes.size());

		ObservableList<Node> top;
		assertTrue(topBox.getChildren().get(0) instanceof VBox);

		var filters = ((VBox) find("#filterVBox")).getChildren();
		assertEquals(2, filters.size());
		assertTrue(filters.get(0) instanceof HBox);
		var buttons = ((HBox) filters.get(0)).getChildren();
		assertTrue(buttons.get(0) instanceof Label);
		assertTrue(buttons.get(1) instanceof Button);

		assertTrue(filters.get(1) instanceof VBox);

		homePanePage.filter("batch");

		historyFiles = ((VBox) find("#historyFilesViewVBox")).getChildren();
		pagesBox = (HBox) historyFiles.get(historyFiles.size() - 1);
		historyVBox = (VBox) historyFiles.get(historyFiles.size() - 2);

		files = historyVBox.getChildren();
		separateBoxes(files, publicKeyBoxes, accountInfoBoxes, batchBoxes, transactionBoxes, softwareBoxes,
				systemBoxes, fileUpdateBoxes);

		for (var file : files) {
			if (!(file instanceof VBox)) {
				continue;
			}
			final var node = ((VBox) file).getChildren().get(0);
			assertTrue(node instanceof Label);
			logger.info(String.format("Currently shown box: %s", ((Label) node).getText()));
		}

		assertEquals(1,
				publicKeyBoxes.size() + accountInfoBoxes.size() + batchBoxes.size() + transactionBoxes.size() + softwareBoxes.size() + systemBoxes.size() +
						fileUpdateBoxes.size());

		assertEquals(1, pagesBox.getChildren().size());

		top = ((HBox) find("#chooseLength")).getChildren();
		var one = findButton(1, top);
		var five = findButton(5, top);
		var ten = findButton(10, top);
		var twenty = findButton(20, top);

		assertNotNull(one);
		assertNotNull(five);
		assertNotNull(ten);
		assertNotNull(twenty);

		homePanePage.filter("transaction");

		pagesBox = (HBox) historyFiles.get(historyFiles.size() - 1);

		top = ((HBox) find("#chooseLength")).getChildren();
		one = findButton(1, top);
		five = findButton(5, top);
		ten = findButton(10, top);
		twenty = findButton(20, top);

		assertNotNull(one);
		assertNotNull(five);
		assertNotNull(ten);
		assertNotNull(twenty);

		assertEquals(3, pagesBox.getChildren().size());
	}

	// region Helper methods
	private Button findButton(int i, ObservableList<Node> top) {
		return (Button) top.stream().filter(node -> node instanceof Button && ((Button) node).getText().equals(
				String.valueOf(i))).findFirst().orElse(null);
	}

	private void separateBoxes(ObservableList<Node> newFiles, List<VBox> publicKeyBoxes, List<VBox> accountInfoBoxes,
			List<VBox> batchBoxes, List<VBox> transactionBoxes, List<VBox> softwareBoxes, List<VBox> systemBoxes,
			List<VBox> fileUpdateBoxes) {
		publicKeyBoxes.clear();
		accountInfoBoxes.clear();
		batchBoxes.clear();
		transactionBoxes.clear();
		softwareBoxes.clear();
		systemBoxes.clear();
		fileUpdateBoxes.clear();
		for (var box :
				newFiles) {
			if (!(box instanceof VBox)) {
				continue;
			}

			var lines = ((VBox) box).getChildren();
			if (lines.size() >= 3) {
				assertTrue(lines.get(0) instanceof Label);
				var l = ((Label) lines.get(0)).getText();
				if (l.contains("Batch")) {
					batchBoxes.add((VBox) box);
				} else if (l.contains("Transaction") && !l.contains("ZippedTransactions")) {
					transactionBoxes.add((VBox) box);
				} else if (l.contains("Account Information")) {
					accountInfoBoxes.add((VBox) box);
				} else if (l.contains("Content")) {
					systemBoxes.add((VBox) box);
				} else if (l.contains("File update")) {
					fileUpdateBoxes.add((VBox) box);
				}
			} else if (lines.size() == 2) {
				assertTrue(lines.get(0) instanceof Label);
				var l = ((Label) lines.get(0)).getText();
				if (l.contains("Account Information")) {
					accountInfoBoxes.add((VBox) box);
				} else if (l.contains("Public Key")) {
					publicKeyBoxes.add((VBox) box);
				} else if (l.contains("Software")) {
					softwareBoxes.add((VBox) box);
				}
			}
		}
	}

	private static void ensureVisible(ScrollPane scrollPane, Node node) {
		var viewport = scrollPane.getViewportBounds();
		var contentHeight =
				scrollPane.getContent().localToScene(scrollPane.getContent().getBoundsInLocal()).getHeight();
		var nodeMinY = node.localToScene(node.getBoundsInLocal()).getMinY();
		var nodeMaxY = node.localToScene(node.getBoundsInLocal()).getMaxY();

		double vValueDelta = 0;
		var vValueCurrent = scrollPane.getVvalue();

		if (nodeMaxY < 0) {
			// currently located above (remember, top left is (0,0))
			vValueDelta = (nodeMinY - viewport.getHeight()) / contentHeight;
		} else if (nodeMinY > viewport.getHeight()) {
			// currently located below
			vValueDelta = (nodeMinY) / contentHeight;
		}
		scrollPane.setVvalue(vValueCurrent + vValueDelta);
	}

	// endregion
}
