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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.integration.TestBase;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.Window;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.testfx.api.FxRobot;

import java.io.File;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AMOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_KEY_LOCATION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.REALM_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.SHARD_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSFERS;

public class TestUtil {
	private static final TestBase driver = new TestBase();

	private static FxRobot robot = new FxRobot();
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
	public static Button getButton(Node node, String legend) {
		var buttons = getButtons(node);
		for (Button button : buttons) {
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
	public static List<Button> getButtons(Node parent) {
		List<Button> buttons = new ArrayList<>();
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
			for (Node child : children) {
				buttons.addAll(getButtons(child));
			}
		}
		return buttons;
	}

	public static ObservableList<Node> getPopupNodes() {
		var actualAlertDialog = findModalWindow();
		if (actualAlertDialog != null) {
			final Node dialogPane = actualAlertDialog.getScene().getRoot();
			if (dialogPane != null) {
				if (dialogPane.getClass().isAssignableFrom(VBox.class)) {
					return ((VBox) dialogPane).getChildren();
				}
				if (dialogPane.getClass().isAssignableFrom(HBox.class)) {
					return ((HBox) dialogPane).getChildren();
				}
			}
		}
		return null;
	}

	public static Stage findModalWindow() {
		// Get a list of windows but ordered from top[0] to bottom[n] ones.
		// It is needed to get the first found modal window.
		final List<Window> allWindows = new ArrayList<>(robot.robotContext().getWindowFinder().listWindows());
		Collections.reverse(allWindows);

		return (Stage) allWindows
				.stream()
				.filter(window -> window instanceof Stage)
				.filter(window -> ((Stage) window).getModality() == Modality.APPLICATION_MODAL)
				.findFirst()
				.orElse(null);
	}
}
