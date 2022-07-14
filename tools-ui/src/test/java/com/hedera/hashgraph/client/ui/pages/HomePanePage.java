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

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.ui.TestBase;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.MAIN_TRANSACTIONS_SCROLLPANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NEW_FILES_VBOX;
import static java.lang.Thread.sleep;

@SuppressWarnings("UnusedReturnValue")
public class HomePanePage {
	private static final Logger logger = LogManager.getLogger(HomePanePage.class);
	private final TestBase driver;

	public HomePanePage(final TestBase driver) {
		this.driver = driver;
	}

	public HomePanePage enterPasswordInPopup(final String password) {
		final var popupNodes = TestUtil.getPopupNodes();
		assert popupNodes != null;
		for (final var n : popupNodes) {
			if (n instanceof PasswordField) {
				((PasswordField) n).setText(password);
			}
		}
		for (final var n :
				TestUtil.getPopupNodes()) {
			if (n instanceof HBox) {
				for (final var b :
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

	public HomePanePage enterStringInPopup(final String text) {
		final var textField = (TextField) Objects.requireNonNull(TestUtil.getPopupNodes()).get(1);
		textField.setText(text);
		driver.clickOn(textField).press(KeyCode.ENTER).release(KeyCode.ENTER);
		return this;
	}

	public HomePanePage clickOnKeyCheckBox(final String keyName) throws HederaClientException {
		final var testingKey = driver.findAll(keyName);

		for (final var node : testingKey) {
			if (node instanceof CheckBox) {
				driver.clickOn(node);
				logger.info("Clicked on key {}", keyName);
				return this;
			}
		}
		throw new HederaClientException(String.format("The checkbox with legend %s could not be found", keyName));
	}

	public HomePanePage waitForWindow() {
		while (!"".equals(TestUtil.getModalWindowTitle())) {
			try {
				sleep(500);
			} catch (final InterruptedException e) {
				logger.error(e);
			}
		}
		return this;
	}

	public HomePanePage clickOnPopupButton(final String button) {
		final var node = TestUtil.findButtonInPopup(TestUtil.getPopupNodes(), button);
		driver.clickOn(node);
		return this;
	}

	private static void ensureVisible(final ScrollPane scrollPane, final Node node) {
		final var viewport = scrollPane.getViewportBounds();
		final var contentHeight =
				scrollPane.getContent().localToScene(scrollPane.getContent().getBoundsInLocal()).getHeight();
		final var nodeMinY = node.localToScene(node.getBoundsInLocal()).getMinY();
		final var nodeMaxY = node.localToScene(node.getBoundsInLocal()).getMaxY();

		double vValueDelta = 0;
		final var vValueCurrent = scrollPane.getVvalue();

		if (nodeMaxY < 0) {
			vValueDelta = (nodeMinY - viewport.getHeight()) / contentHeight;
		} else if (nodeMinY > viewport.getHeight()) {
			vValueDelta = (nodeMinY) / contentHeight;
		}
		scrollPane.setVvalue(vValueCurrent + vValueDelta);
	}

	public HomePanePage clickOn2ButtonBar(final int index, final VBox vBox) {
		final var v1 = (VBox) vBox.getChildren().get(2);
		final var buttonBar = (ButtonBar) v1.getChildren().get(0);
		final var button = (Button) buttonBar.getButtons().get(index);
		ensureVisible(driver.find(MAIN_TRANSACTIONS_SCROLLPANE), button);
		driver.clickOn(button);
		return this;
	}

	public List<VBox> getInfoCards() {
		final VBox vBox = driver.find(NEW_FILES_VBOX);
		final var children = vBox.getChildren();
		return children.stream().filter(
				child -> child instanceof VBox && getLabel(((VBox) child).getChildren(), "Account Information")).map(
				child -> (VBox) child).collect(Collectors.toList());
	}

	private boolean getLabel(final ObservableList<Node> nodes, final String title) {
		return nodes.stream().anyMatch(node -> node instanceof Label && ((Label) node).getText().equals(title) ||
				node instanceof HBox && getLabel(((HBox) node).getChildren(), title) ||
				node instanceof VBox && getLabel(((VBox) node).getChildren(), title) ||
				node instanceof GridPane && getLabel(((GridPane) node).getChildren(), title));
	}
}
