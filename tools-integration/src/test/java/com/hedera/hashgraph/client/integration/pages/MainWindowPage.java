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

package com.hedera.hashgraph.client.integration.pages;

import com.hedera.hashgraph.client.integration.TestBase;

@SuppressWarnings("UnusedReturnValue")
public class MainWindowPage {

	private final TestBase driver;
	private static final String ACCOUNTS_BUTTON = "#accountsButton";
	private static final String SETTINGS_PANE = "#settingsButton";

	public MainWindowPage(final TestBase driver) {
		this.driver = driver;
	}

	public MainWindowPage clickOnAccountsButton() {
		driver.clickOn(ACCOUNTS_BUTTON);
		return new MainWindowPage(driver);
	}

	public MainWindowPage clickOnSettingsButton() {
		driver.clickOn(SETTINGS_PANE);
		return new MainWindowPage(driver);
	}
}
