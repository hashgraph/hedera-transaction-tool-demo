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

package com.hedera.hashgraph.client.ui.remote;

import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.ui.StartUI;
import com.hedera.hashgraph.client.ui.TestBase;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Test;
import org.testfx.api.FxToolkit;

import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static org.junit.Assert.assertTrue;

public class CommonMethodsTest extends TestBase {
	private static final Logger logger = LogManager.getLogger(CommonMethodsTest.class);

	@Test
	public void getTimeLabel_test() throws TimeoutException {
		logger.info("Starting TimeLabel test");
		var properties = new UserAccessibleProperties(DEFAULT_STORAGE + "Files/user.properties", "");
		properties.setSetupPhase(SetupPhase.TEST_PHASE);

		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		logger.info("UI initialized");

		var label = CommonMethods.getTimeLabel(new Timestamp(206711586, 0), false);
		assertTrue(label.getText().contains("11:53:06 UTC"));
		var label2 = CommonMethods.getTimeLabel(new Timestamp(206711586, 0), true);
		assertTrue(label2.getText().contains("1976-07-20 11:53:06 UTC"));

		logger.info("TimeLabel test finished");
	}
}
