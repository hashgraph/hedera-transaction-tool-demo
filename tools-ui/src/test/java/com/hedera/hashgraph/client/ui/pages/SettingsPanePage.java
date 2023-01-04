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

package com.hedera.hashgraph.client.ui.pages;

import com.hedera.hashgraph.client.ui.TestBase;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.io.File;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.ADD_FOLDER_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.AUTO_RENEW_PERIOD_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CANCEL_ADD_FOLDER_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NODE_ID_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_EMAIL_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_PATH_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TRANSACTION_FEE_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_HOURS_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_MINUTES_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_SECONDS_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TX_VALID_DURATION_TF;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findButtonInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findTextFieldsInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SettingsPanePage {
	private final TestBase driver;

	public SettingsPanePage(final TestBase driver) {
		this.driver = driver;
	}

	public SettingsPanePage setNodeID(final String node) {
		final TextField nodeID = driver.find(NODE_ID_TF);
		nodeID.clear();
		driver.clickOn(NODE_ID_TF);
		driver.write(node);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setTransactionVD(final String transactionVD) {
		driver.doubleClickOn(TX_VALID_DURATION_TF);
		driver.write(transactionVD);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setAutoRenewPeriod(final String autoRenewPeriod) {
		driver.doubleClickOn(AUTO_RENEW_PERIOD_TF);
		driver.write(autoRenewPeriod);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setHours(final String hours) {
		driver.doubleClickOn(TVS_HOURS_TF);
		driver.write(hours);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setMinutes(final String minutes) {
		driver.doubleClickOn(TVS_MINUTES_TF);
		driver.write(minutes);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setSeconds(final String seconds) {
		driver.doubleClickOn(TVS_SECONDS_TF);
		driver.write(seconds);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setTransactionFee(final String transactionFee) {
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

	public SettingsPanePage pressEditFolder(final HBox hBox) {
		final Node edit = hBox.getChildren().get(1);
		assertTrue(edit instanceof Button);
		driver.clickOn(edit);
		return this;
	}

	public SettingsPanePage setPathAndEmail(final String path, final String email) {
		driver.clickOn(ONEDRIVE_PATH_TF);
		driver.write(path);
		driver.clickOn(ONEDRIVE_PATH_TF);
		driver.press(KeyCode.ENTER);
		driver.release(KeyCode.ENTER);
		driver.clickOn(ONEDRIVE_EMAIL_TF);
		driver.write(email);
		return this;
	}

	public SettingsPanePage pressConfirmAddFolder() {
		driver.clickOn("#confirmAddFolderButtonSP");
		return this;
	}

	public SettingsPanePage setPath(final String path) {
		final Node pathField = driver.find(ONEDRIVE_PATH_TF);
		((TextField) pathField).clear();
		driver.clickOn(ONEDRIVE_PATH_TF);
		driver.write(path);
		driver.clickOn(ONEDRIVE_PATH_TF);
		driver.press(KeyCode.ENTER);
		driver.release(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage setEmail(final String email) {
		driver.clickOn(ONEDRIVE_EMAIL_TF);
		driver.write(email);
		return this;
	}

	public SettingsPanePage createPopup() {
		final ObservableList<Node> popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var children = ((VBox) popupNodes.get(0)).getChildren();
		final var buttons = ((HBox) ((HBox) children.get(1)).getChildren().get(1)).getChildren();
		assertEquals(2, buttons.size());

		driver.clickOn(buttons.get(0));
		return this;
	}

	public SettingsPanePage openNetworksCombobox(final String network) {
		driver.clickOn("#networkChoicebox");
		driver.clickOn(network);
		return this;
	}

	public SettingsPanePage addNetwork(final String nickname, final String location) {
		driver.clickOn("#addCustomNetworkButton");
		addNetworkNickname(nickname);
		addCustomLocation(location);
		clickOnButton("CONTINUE");
		return this;
	}

	public SettingsPanePage clickOnButton(final String legend) {
		final ObservableList<Node> popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var button = findButtonInPopup(popupNodes, legend);
		driver.clickOn(button);
		return this;
	}

	public SettingsPanePage addNetworkNickname(final String nickname) {
		final ObservableList<Node> popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var textFields = findTextFieldsInPopup(popupNodes);
		assert textFields.size() == 2;
		driver.clickOn(textFields.get(0));
		driver.write(nickname);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public SettingsPanePage addCustomLocation(final String location) {
		final ObservableList<Node> popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var textFields = findTextFieldsInPopup(popupNodes);
		driver.clickOn(textFields.get(1));
		driver.write(new File(location).getAbsolutePath());
		driver.type(KeyCode.ENTER);
		return this;
	}


}
