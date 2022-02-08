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

import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.HOME_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SETTINGS_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SIGNATURE_BUTTON;

public class MainWindowPage {
	private final TestBase driver;

	public MainWindowPage(final TestBase driver) {
		this.driver = driver;
	}

	public MainWindowPage clickOnHomeButton() {

		driver.clickOn(HOME_BUTTON);
		return new MainWindowPage(driver);
	}

	public MainWindowPage clickOnAccountsButton() {

		driver.clickOn(ACCOUNTS_BUTTON);
		return new MainWindowPage(driver);
	}

	public MainWindowPage clickOnKeysButton() {

		driver.clickOn(KEYS_BUTTON);
		return new MainWindowPage(driver);
	}

	public MainWindowPage clickOnCreateButton() {

		driver.find(CREATE_BUTTON);
		driver.clickOn(CREATE_BUTTON);
		return new MainWindowPage(driver);
	}

	public MainWindowPage clickOnSettingsButton() {

		driver.find(SETTINGS_BUTTON);
		driver.clickOn(SETTINGS_BUTTON);
		return new MainWindowPage(driver);
	}

	public MainWindowPage clickPopupContinue() {
		final var nodes = TestUtil.getPopupNodes();
		return this;
	}
}
