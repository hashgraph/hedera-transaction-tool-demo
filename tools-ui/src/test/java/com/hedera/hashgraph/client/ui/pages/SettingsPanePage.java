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

package com.hedera.hashgraph.client.ui.pages;

import com.hedera.hashgraph.client.ui.TestBase;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;

import javax.swing.JFileChooser;
import java.io.File;
import java.nio.file.Paths;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.ADD_FOLDER_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.AUTO_RENEW_PERIOD_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CANCEL_ADD_FOLDER_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CONFIRM_ADD_FOLDER_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.LOAD_STORAGE_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NODE_ID_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_EMAIL_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_PATH_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TRANSACTION_FEE_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_HOURS_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_MINUTES_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_SECONDS_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TX_VALID_DURATION_TF;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class SettingsPanePage {
	private final TestBase driver;

	public SettingsPanePage(TestBase driver) {
		this.driver = driver;
	}

	public SettingsPanePage verifyDataStorage(ObservableList<Node> children) {
		Label dataStorage = (Label) children.get(0);
		assertEquals(dataStorage.getText(), "Data Storage Directory:");
		HBox dataStorageField = (HBox) children.get(1);
		TextField text = (TextField) dataStorageField.getChildren().get(0);
		assertEquals(text.getText(), "");
		ImageView browse = (ImageView) dataStorageField.getChildren().get(1);
		driver.clickOn(browse);
		TestUtil.applyPath(Paths.get("").toAbsolutePath().toString() + "/src/test/resources");
		assertEquals(text.getText(), Paths.get("").toAbsolutePath().toString() + "/src/test/resources");
		return this;
	}

	public SettingsPanePage verifyDataStorage_headless(ObservableList<Node> children) {
		Label dataStorage = (Label) children.get(0);
		assertEquals(dataStorage.getText(), "Data Storage Directory:");
		HBox dataStorageField = (HBox) children.get(1);
		TextField text = (TextField) dataStorageField.getChildren().get(0);
		assertEquals(text.getText(), "");
		text.setText(Paths.get("").toAbsolutePath().toString() + "/src/test/resources");
		assertEquals(text.getText(), Paths.get("").toAbsolutePath().toString() + "/src/test/resources");
		return this;
	}

	public SettingsPanePage verifyAddressBookField(ObservableList<Node> children) {
		Label address = (Label) children.get(2);
		assertEquals(address.getText(), "AddressBook File:");
		HBox addrField = (HBox) children.get(3);
		TextField text = (TextField) addrField.getChildren().get(0);
		assertEquals(text.getText(), "");
		ImageView browse = (ImageView) addrField.getChildren().get(1);
		driver.clickOn(browse);
		TestUtil.applyPath(Paths.get("").toAbsolutePath().toString() + "/src/test/resources/testNodesInfo.json");
		assertEquals(text.getText(),
				Paths.get("").toAbsolutePath().toString() + "/src/test/resources/testNodesInfo.json");

		return this;
	}

	public SettingsPanePage verifyAddressBookField_headless(ObservableList<Node> children) {
		Label address = (Label) children.get(2);
		assertEquals(address.getText(), "AddressBook File:");
		HBox addrField = (HBox) children.get(3);
		TextField text = (TextField) addrField.getChildren().get(0);
		assertEquals(text.getText(), "");
		text.setText(Paths.get("").toAbsolutePath().toString() + "/src/test/resources/testNodesInfo.json");
		assertEquals(text.getText(),
				Paths.get("").toAbsolutePath().toString() + "/src/test/resources/testNodesInfo.json");

		return this;
	}

	public SettingsPanePage verifyDefaultNodeId(ObservableList<Node> children) {
		Label node = (Label) children.get(4);
		assertEquals(node.getText(), "Default Node ID:");
		TextField text = (TextField) children.get(5);
		assertEquals(text.getText(), "0.0.3");
		driver.clickOn(text).write("Random");
		assertEquals(text.getText(), "0.0.3");
		return this;
	}

	public SettingsPanePage verifyTxnValidDuration(ObservableList<Node> children) {
		Label node = (Label) children.get(7);
		assertEquals(node.getText(), "Transaction Valid Duration:");
		TextField text = (TextField) ((HBox) children.get(8)).getChildren().get(0);
		assertEquals(text.getText(), "120");
		driver.clickOn(text).write("150000");
		//assertEquals(text.getText(), "120150000");
		assertEquals(text.getText(), "120");
		driver.clickOn(text).write("Random");
		//assertEquals(text.getText(), "120150000");
		assertEquals(text.getText(), "120");
		return this;
	}

	public SettingsPanePage verifyGenerateRecord(ObservableList<Node> children) {
		CheckBox cb = (CheckBox) children.get(9);
		assertEquals(cb.getText(), "Generate Record");
		assertTrue(cb.isSelected());
		cb.setSelected(false);
		assertFalse(cb.isSelected());
		return this;
	}

	public SettingsPanePage verifySubmiTxn(ObservableList<Node> children) {
		Label label = (Label) children.get(11);
		assertTrue(label.getText().contains("Submit Transaction"));
		TextField stxn = (TextField) ((HBox) children.get(10)).getChildren().get(0);
		assertEquals(stxn.getText(), "48");
		driver.clickOn(stxn).write("Random");
		assertEquals(stxn.getText(), "48");
		driver.clickOn(stxn).write("1500");
		assertEquals(stxn.getText(), "48");
		return this;
	}

	public SettingsPanePage verifyDefTxnFee(ObservableList<Node> children) {
		Label label = (Label) children.get(12);
		assertEquals(label.getText(), "Default Transaction Fee");
		TextField stxn = (TextField) children.get(13);
		assertEquals(stxn.getText(), "100000000");
		driver.clickOn(stxn).write("Random");
		assertEquals(stxn.getText(), "100000000");
		driver.clickOn(stxn).write("10200");
		//assertEquals(stxn.getText(), "10000000010200");
		return this;
	}

	public SettingsPanePage verifyDefStartTime(ObservableList<Node> children) {
		Label label = (Label) children.get(15);
		assertTrue(label.getText().contains("Default start time for transactions"));
		TextField hours = (TextField) ((HBox) children.get(18)).getChildren().get(0);
		assertEquals(hours.getText(), "1");
		driver.clickOn(hours).write("Random");
		assertEquals(hours.getText(), "1");
		driver.clickOn(hours).write("100");
		//assertEquals(hours.getText(), "011");
		TextField mins = (TextField) ((HBox) children.get(18)).getChildren().get(2);
		assertEquals(mins.getText(), "00");
		driver.clickOn(mins).write("Random");
		assertEquals(mins.getText(), "00");
		return this;
	}

	public SettingsPanePage verifyVersion(ObservableList<Node> children) {
		Label label = (Label) children.get(16);
		assertTrue(label.getText().contains("Version Details"));
		Label version = (Label) children.get(17);
		assertTrue(version.getText().contains("VERSION"));
		return this;
	}

	public void verifyReset(Button reset) {
		String toolsFolder =
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents" +
						"/TransactionTools/";
		File toolsFolderFile = new File(toolsFolder);
		assertTrue(toolsFolderFile.exists());
		driver.clickOn(reset);
		ObservableList<Node> nodes = TestUtil.getPopupNodesReset();
		ButtonBar buttonbar = (ButtonBar) nodes.get(2);
		ObservableList<Node> buttons = buttonbar.getButtons();
		assertEquals(buttons.size(), 2);
		driver.clickOn("CONTINUE");
	}

	public boolean findFile(String name, File file) {
		File[] list = file.listFiles();
		if (list != null) {
			for (File fil : list) {
				if (fil.isDirectory()) {
					if (fil.getName().equals(name)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	public boolean findFileStartingWithName(String name, File file) {
		File[] list = file.listFiles();
		if (list != null) {
			for (File fil : list) {
				if (fil.isDirectory()) {
					if (fil.getName().contains(name + ".")) {
						return true;
					}
				}
			}
		}
		return false;
	}

	public SettingsPanePage setStorage(String storage) {
		driver.doubleClickOn(LOAD_STORAGE_TF);
		driver.write(storage);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setNodeID(String node) {
		driver.doubleClickOn(NODE_ID_TF);
		driver.write(node);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setTransactionVD(String transactionVD) {
		driver.doubleClickOn(TX_VALID_DURATION_TF);
		driver.write(transactionVD);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setAutoRenewPeriod(String autoRenewPeriod) {
		driver.doubleClickOn(AUTO_RENEW_PERIOD_TF);
		driver.write(autoRenewPeriod);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setHours(String hours) {
		driver.doubleClickOn(TVS_HOURS_TF);
		driver.write(hours);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setMinutes(String minutes) {
		driver.doubleClickOn(TVS_MINUTES_TF);
		driver.write(minutes);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setSeconds(String seconds) {
		driver.doubleClickOn(TVS_SECONDS_TF);
		driver.write(seconds);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setTransactionFee(String transactionFee) {
		driver.doubleClickOn(TRANSACTION_FEE_TF);
		driver.write(transactionFee);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage pressAddFolder() {
		driver.clickOn(ADD_FOLDER_BUTTON);
		return this;
	}

	public SettingsPanePage cancelAddToMap() {
		driver.clickOn(CANCEL_ADD_FOLDER_BUTTON);
		return this;
	}

	public SettingsPanePage pressEditFolder(HBox hBox) {
		Node edit = hBox.getChildren().get(1);
		assertTrue(edit instanceof Button);
		driver.clickOn(edit);
		return this;
	}

	public SettingsPanePage pressAddToMap(HBox hBox) {
		Node add = hBox.getChildren().get(2);
		assertTrue(add instanceof Button);
		driver.clickOn(add);
		return this;
	}

	public SettingsPanePage setPathAndEmail(String path, String email){
		driver.clickOn(ONEDRIVE_PATH_TF);
		driver.write(path);
		driver.clickOn(ONEDRIVE_EMAIL_TF);
		driver.write(email);
		return this;
	}

	public SettingsPanePage pressConfirmAddFolder() {
		driver.clickOn(CONFIRM_ADD_FOLDER_BUTTON);
		return this;
	}
}
