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
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.testfx.api.FxRobot;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TestUtil {
	private static final TestBase driver = new TestBase();
	private static final FxRobot robot = new FxRobot();
	private static final Logger logger = LogManager.getLogger(TestUtil.class);

	/**
	 * Get a button from a node
	 *
	 * @param node
	 * 		the parent node
	 * @param legend
	 * 		the legend on the button
	 * @return a button
	 */
	public static Button getButton(final Node node, final String legend) {
		final var buttons = getButtons(node);
		for (final Button button : buttons) {
			if (legend.equals(button.getText()) && button.isVisible()) {
				return button;
			}
		}
		return null;
	}

	/**
	 * Gets a list of buttons from a node
	 *
	 * @param parent
	 * 		the parent node
	 * @return a list of buttons
	 */
	public static List<Button> getButtons(final Node parent) {
		final List<Button> buttons = new ArrayList<>();
		if (parent instanceof Button) {
			return Collections.singletonList((Button) parent);
		}
		ObservableList<Node> children = null;
		if (parent instanceof HBox) {
			children = ((HBox) parent).getChildren();
		}
		if (parent instanceof VBox) {
			children = ((VBox) parent).getChildren();
		}
		if (parent instanceof GridPane) {
			children = ((GridPane) parent).getChildren();
		}
		if (children != null) {
			for (final Node child : children) {
				buttons.addAll(getButtons(child));
			}
		}
		return buttons;
	}
}
