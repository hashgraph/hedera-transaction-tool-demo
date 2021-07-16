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
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.ADD_FOLDER_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.AUTO_RENEW_PERIOD_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CANCEL_ADD_FOLDER_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CONFIRM_ADD_FOLDER_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NODE_ID_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_EMAIL_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ONEDRIVE_PATH_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TRANSACTION_FEE_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_HOURS_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_MINUTES_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TVS_SECONDS_TF;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TX_VALID_DURATION_TF;
import static org.junit.Assert.assertTrue;

public class SettingsPanePage {
	private final TestBase driver;

	public SettingsPanePage(TestBase driver) {
		this.driver = driver;
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

	public SettingsPanePage setPathAndEmail(String path, String email) {
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
