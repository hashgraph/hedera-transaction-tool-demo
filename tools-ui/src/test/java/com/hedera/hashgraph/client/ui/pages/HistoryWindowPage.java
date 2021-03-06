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

@SuppressWarnings("UnusedReturnValue")
public class HistoryWindowPage {
	private final TestBase driver;

	public HistoryWindowPage(final TestBase driver) {
		this.driver = driver;
	}

	public HistoryWindowPage rebuildHistory() {
		driver.find("#rebuild");
		driver.clickOn("#rebuild");
		return this;
	}

	public HistoryWindowPage clickOnResign(final String buttonText) {
		if (driver.find(buttonText).isVisible()) {
			driver.clickOn(buttonText);
		}
		return this;
	}

}
