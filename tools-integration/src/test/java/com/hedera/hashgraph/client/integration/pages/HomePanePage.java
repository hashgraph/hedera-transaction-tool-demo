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

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.integration.TestBase;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.CheckBox;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Objects;

import static com.hedera.hashgraph.client.integration.JavaFXIDs.MAIN_TRANSACTIONS_SCROLLPANE;
import static java.lang.Thread.sleep;

public class HomePanePage {
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private final TestBase driver;

	public HomePanePage(TestBase driver) {
		this.driver = driver;
	}

	public HomePanePage enterPasswordInPopup(String password) {
		var popupNodes = TestUtil.getPopupNodes();
		assert popupNodes != null;
		for (var n :
				popupNodes) {
			if (n instanceof PasswordField) {
				((PasswordField) n).setText(password);
			}
		}
		for (var n :
				TestUtil.getPopupNodes()) {
			if (n instanceof HBox) {
				for (var b :
						((HBox) n).getChildren()) {
					if (b instanceof Button && ((Button) b).getText().equalsIgnoreCase("confirm")) {
						driver.clickOn(b);
						return this;
					}
				}
			}
		}
		return this;
	}

	public HomePanePage enterStringInPopup(String text) {
		var textField = (TextField) Objects.requireNonNull(TestUtil.getPopupNodes()).get(1);
		textField.setText(text);
		driver.clickOn(textField).press(KeyCode.ENTER).release(KeyCode.ENTER);
		return this;
	}

	public HomePanePage filter(String query) {
		VBox filterBox = driver.find("#filterVBox");
		var pane = (GridPane) ((VBox) filterBox.getChildren().get(1)).getChildren().get(0);
		var checkboxes = pane.getChildren();
		for (var checkbox : checkboxes) {
			assert checkbox instanceof CheckBox;
			if (((CheckBox) checkbox).getText().toLowerCase().contains(query.toLowerCase())) {
				driver.clickOn(checkbox);
				return this;
			}
		}
		return this;
	}

	public HomePanePage clickOnKeyCheckBox(String keyName) throws HederaClientException {
		var testingKey = driver.findAll(keyName);

		for (var node : testingKey) {
			if (node instanceof CheckBox) {
				driver.clickOn(node);
				logger.info("Clicked on key {}", keyName);
				return this;
			}
		}
		throw new HederaClientException(String.format("The checkbox with legend %s could not be found", keyName));
	}

	private static void ensureVisible(ScrollPane scrollPane, Node node) {
		var viewport = scrollPane.getViewportBounds();
		var contentHeight =
				scrollPane.getContent().localToScene(scrollPane.getContent().getBoundsInLocal()).getHeight();
		var nodeMinY = node.localToScene(node.getBoundsInLocal()).getMinY();
		var nodeMaxY = node.localToScene(node.getBoundsInLocal()).getMaxY();

		double vValueDelta = 0;
		var vValueCurrent = scrollPane.getVvalue();

		if (nodeMaxY < 0) {
			// currently located above (remember, top left is (0,0))
			vValueDelta = (nodeMinY - viewport.getHeight()) / contentHeight;
		} else if (nodeMinY > viewport.getHeight()) {
			// currently located below
			vValueDelta = (nodeMinY) / contentHeight;
		}
		scrollPane.setVvalue(vValueCurrent + vValueDelta);
	}


	public HomePanePage clickOn2ButtonBar(int index, VBox vBox) {
		var v1 = (VBox) vBox.getChildren().get(2);
		var buttonBar = (ButtonBar) v1.getChildren().get(0);
		var button = (Button) buttonBar.getButtons().get(index);
		ensureVisible(driver.find(MAIN_TRANSACTIONS_SCROLLPANE), button);
		driver.clickOn(button);
		return this;
	}
}
