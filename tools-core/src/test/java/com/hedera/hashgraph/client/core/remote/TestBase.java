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

package com.hedera.hashgraph.client.core.remote;

import javafx.scene.Node;
import javafx.stage.Stage;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.testfx.api.FxToolkit;
import org.testfx.framework.junit5.ApplicationTest;

import java.util.concurrent.TimeoutException;


public class TestBase extends ApplicationTest {

	@BeforeClass
	public static void setupHeadlessMode() throws TimeoutException {
		System.setProperty("testfx.robot", "glass");
		System.setProperty("testfx.headless", "true");
		System.setProperty("prism.order", "sw");
		System.setProperty("prism.text", "t2k");
		System.setProperty("java.awt.headless", "true");
		System.setProperty("headless.geometry", "1600x1200-64");
		FxToolkit.registerPrimaryStage();

	}

	@AfterClass
	public static void afterTest() {
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


	public boolean exists(final String query) {
		try {
			lookup(query).queryAll().iterator().next();
			return true;
		} catch (final Exception e) {
			return false;
		}
	}

}
