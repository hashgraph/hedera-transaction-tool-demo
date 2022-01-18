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
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;

@SuppressWarnings("UnusedReturnValue")
public class SettingsPanePage {
	private final TestBase driver;

	public SettingsPanePage(final TestBase driver) {
		this.driver = driver;
	}


	public SettingsPanePage enterFeePayer(final String account) {
		final TextField textField = driver.find("#customFeePayerTextField");
		if (!textField.isVisible()) {
			driver.clickOn("#addCustomPayerButton");
		}
		assert textField.isVisible();

		driver.clickOn(textField);
		driver.write(account);
		driver.type(KeyCode.ENTER);

		return this;
	}
}
