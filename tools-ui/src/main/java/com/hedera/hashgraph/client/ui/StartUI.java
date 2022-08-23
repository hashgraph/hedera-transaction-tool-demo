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

package com.hedera.hashgraph.client.ui;


import com.hedera.hashgraph.client.core.exceptions.Log4j2UncaughtExceptionHandler;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class StartUI extends Application {

	private static final Logger logger = LogManager.getLogger(StartUI.class);

	@Override
	public void start(final Stage primaryStage) throws Exception {
		final Parent root = FXMLLoader.load(getClass().getResource("UserInterface.fxml"));
		final var scene = new Scene(root, 1200, 950);

		Style.addStylesheets(scene);
		primaryStage.setScene(scene);
		primaryStage.show();
	}

	public static void main(final String[] args) {
		logger.info("Hedera UI Tool is starting.");
		Thread.setDefaultUncaughtExceptionHandler(Log4j2UncaughtExceptionHandler.get());
		launch(args);
	}
}
