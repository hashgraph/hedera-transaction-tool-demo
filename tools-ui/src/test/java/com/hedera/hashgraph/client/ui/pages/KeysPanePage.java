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


import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.ui.TestBase;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.PK_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_RECOVER_KEYS_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_RECOVER_KEY_INDEX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_RECOVER_KEY_NICKNAME;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CANCEL_EXPORT_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CANCEL_RECOVER_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CHOOSE_EXPORT_LOCATION;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CURRENT_ACCOUNT_PANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_CANCEL_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_CREATE_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_EXPORT_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_GENERATE_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_MNEMONIC_ACCEPT_PASSWORD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_MNEMONIC_CLOSE_VIEW;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_MNEMONIC_PASSWORD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_RECOVERY_PHRASE_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_RECOVER_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NICKNAME;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASSWORD_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.RETYPE_PASSWORD_BOX;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static java.lang.Boolean.parseBoolean;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class KeysPanePage {
	private static final Logger logger = LogManager.getLogger(KeysPanePage.class);

	private final String defaultStorage =
			System.getProperty("user.home") + File.separator + "Documents" + File.separator + "TransactionTools";
	private final TestBase driver;

	public KeysPanePage(TestBase driver) {
		this.driver = driver;
	}

	public void typePassword(String s, PasswordField field) {
		field.setText(s);
		driver.clickOn(field);
		driver.type(KeyCode.SHIFT);
	}

	public KeysPanePage pressGenerateKeyButton() {
		driver.clickOn(KEYS_GENERATE_KEYS);
		return this;
	}

	public KeysPanePage enterNickName(String nickname) {
		((TextField) driver.find(NICKNAME)).clear();
		driver.clickOn(NICKNAME).write(nickname);
		return this;
	}

	public KeysPanePage enterPassword(String password) {
		((PasswordField) driver.find(PASSWORD_BOX)).clear();
		driver.clickOn(PASSWORD_BOX).write(password);
		return this;
	}

	public KeysPanePage enterPopupPassword(String password) {
		ObservableList<Node> nodes = getPopupNodes();
		for (Node n :
				nodes) {
			if (n instanceof PasswordField) {
				driver.clickOn(n);
				driver.write(password);
				driver.type(KeyCode.ENTER);
				return this;
			}
		}
		throw new HederaClientRuntimeException("Cannot find password field");
	}

	public KeysPanePage pressCancelPassword() {
		ObservableList<Node> nodes = getPopupNodes();
		ObservableList<Node> buttons = ((HBox) nodes.get(3)).getChildren();
		for (Node button : buttons) {
			if (((Button) button).getText().equalsIgnoreCase("cancel")) {
				driver.clickOn(button);
				return this;
			}
		}
		throw new HederaClientRuntimeException("Cannot find password field");
	}

	public KeysPanePage enterRepeatPassword(String password) {
		((PasswordField) driver.find(RETYPE_PASSWORD_BOX)).clear();
		driver.clickOn(RETYPE_PASSWORD_BOX).write(password);
		return this;
	}

	public KeysPanePage pressCreateKeysButton() {
		driver.clickOn(KEYS_CREATE_KEYS);
		return this;
	}

	public Node find(String s) {
		return driver.find(s);
	}

	public KeysPanePage closePopup(String query) {
		Set<Node> nodes = driver.findAll(query.toUpperCase());

		for (Node node : nodes) {
			if (node instanceof Button && node.isVisible()) {
				driver.clickOn(node);
			}
		}
		return this;
	}

	public KeysPanePage closeOKPopup() {
		ObservableList<Node> x = getPopupNodes();
		HBox hBox = null;
		if (x.get(0) instanceof VBox) {
			ObservableList<Node> y = ((VBox) x.get(0)).getChildren();
			hBox = (y.get(1) instanceof HBox) ? (HBox) y.get(1) : null;
		}
		assert hBox != null;
		for (Node node : hBox.getChildren()) {
			if (node instanceof Button) {
				driver.clickOn(node);
			}
		}
		return this;
	}

	public KeysPanePage scrollPane(double vValue) {
		ScrollPane scrollPane = driver.find(CURRENT_ACCOUNT_PANE);
		scrollPane.setVvalue(vValue);
		return this;
	}

	public KeysPanePage cancelCreateKeys() {
		driver.clickOn((Button) driver.find(KEYS_CANCEL_KEYS));
		return this;
	}

	public KeysPanePage clickOn(Button button) {
		driver.clickOn(button);
		return this;
	}

	public KeysPanePage pressExportKeysButton() {
		ScrollPane pane = (ScrollPane) driver.find(CURRENT_ACCOUNT_PANE);
		pane.setVvalue(1.0);
		driver.clickOn(KEYS_EXPORT_KEYS);
		return this;
	}

	public KeysPanePage selectOnTableRow(String query, TableView<String> table) {
		ObservableList<String> items = table.getItems();
		for (String line :
				items) {
			if (!line.isEmpty() && line.contains(query)) {
				table.getSelectionModel().select(line);
			}
		}
		return this;
	}

	public KeysPanePage exportKey(String query) {
		driver.clickOn(CHOOSE_EXPORT_LOCATION);
		driver.clickOn(query);
		return this;
	}

	public KeysPanePage pressCancelExportKeysButton() {
		driver.clickOn(CANCEL_EXPORT_KEYS);
		return this;
	}

	public KeysPanePage pressRecoverKeysButton() {
		driver.clickOn(KEYS_RECOVER_KEYS);
		return this;
	}

	public KeysPanePage enterIndex(int index) {
		((TextField) driver.find(ACCOUNTS_RECOVER_KEY_INDEX)).clear();
		driver.clickOn(ACCOUNTS_RECOVER_KEY_INDEX);
		driver.write(Integer.toString(index));
		driver.type(KeyCode.TAB);
		return this;
	}

	public KeysPanePage pressCancelRecoverKeysButton() {
		driver.clickOn(CANCEL_RECOVER_KEYS);
		return this;
	}

	public KeysPanePage enterRecoverNickname(String nickname) {
		((TextField) driver.find(ACCOUNTS_RECOVER_KEY_NICKNAME)).clear();
		driver.clickOn(ACCOUNTS_RECOVER_KEY_NICKNAME);
		driver.write(nickname);
		driver.type(KeyCode.TAB);
		return this;
	}

	public KeysPanePage pressRecoverKeys() {
		driver.clickOn(ACCOUNTS_RECOVER_KEYS_BUTTON);
		return this;
	}

	public KeysPanePage pressRecoveryPhrase() {
		driver.clickOn(KEYS_RECOVERY_PHRASE_BUTTON);
		return this;
	}

	public KeysPanePage scrollToBottom() {
		Node pane = driver.find("#currentAccountPane");
		if (pane instanceof ScrollPane) {
			((ScrollPane) pane).setVvalue(1.0);
		}
		return this;
	}

	public Label getPopupLabel() {
		VBox vBox = (VBox) getPopupNodes().get(0);
		for (Node node : vBox.getChildren()) {
			if (node instanceof Label) {
				return (Label) node;
			}
		}
		return new Label();
	}

	public List<Label> getPopupLabels() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		return getAllLabels(popupNodes);
	}

	List<Label> getAllLabels(ObservableList<Node> nodes) {
		List<Label> labels = new ArrayList<>();
		for (Node node : nodes) {
			if (node instanceof HBox) {
				labels.addAll(getAllLabels(((HBox) node).getChildren()));
			}
			if (node instanceof VBox) {
				labels.addAll(getAllLabels(((VBox) node).getChildren()));
			}
			if (node instanceof Label) {
				labels.add((Label) node);
			}
		}
		return labels;
	}

	public List<Button> getPopupButtons() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		return getAllButtons(popupNodes);
	}

	List<Button> getAllButtons(ObservableList<Node> nodes) {
		List<Button> buttons = new ArrayList<>();
		for (Node node : nodes) {
			if (node instanceof HBox) {
				buttons.addAll(getAllButtons(((HBox) node).getChildren()));
			}
			if (node instanceof VBox) {
				buttons.addAll(getAllButtons(((VBox) node).getChildren()));
			}
			if (node instanceof ButtonBar) {
				buttons.addAll(getAllButtons(((ButtonBar) node).getButtons()));
			}
			if (node instanceof Button) {
				buttons.add((Button) node);
			}
		}
		return buttons;
	}


	public List<PasswordField> getPopupPasswordFields() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		return getAllPasswordFields(popupNodes);
	}

	List<PasswordField> getAllPasswordFields(ObservableList<Node> nodes) {
		List<PasswordField> buttons = new ArrayList<>();
		for (Node node : nodes) {
			if (node instanceof HBox) {
				buttons.addAll(getAllPasswordFields(((HBox) node).getChildren()));
			}
			if (node instanceof VBox) {
				buttons.addAll(getAllPasswordFields(((VBox) node).getChildren()));
			}
			if (node instanceof PasswordField) {
				buttons.add((PasswordField) node);
			}
		}
		return buttons;
	}

	public KeysPanePage enterMnemonicPasswordAndEnter(String password) {
		enterMnemonicPassword(password);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public KeysPanePage enterMnemonicPassword(String password) {
		Node p = driver.find(KEYS_MNEMONIC_PASSWORD);
		assert p instanceof PasswordField;
		driver.clickOn(p);
		driver.write(password);
		return this;
	}

	public KeysPanePage pressCloseViewMnemonic() {
		driver.clickOn(KEYS_MNEMONIC_CLOSE_VIEW);
		return this;
	}

	public KeysPanePage pressEnterPassword() {
		driver.clickOn(KEYS_MNEMONIC_ACCEPT_PASSWORD);
		return this;
	}

	public KeysPanePage pressPopupButton(String legend) {
		ObservableList<Node> nodes = getPopupNodes();
		for (Node n : nodes) {
			if (n instanceof Button && legend.equals(((Button) n).getText())) {
				driver.clickOn(n);
			}
		}
		return this;
	}

	public KeysPanePage closePasswordPopup() {
		ObservableList<Node> nodes = getPopupNodes();
		assert nodes != null;
		HBox hBox = (HBox) nodes.get(3);
		Button button = (Button) hBox.getChildren().get(0);
		driver.clickOn(button);
		return this;
	}

	public KeysPanePage pressOnCreateKeysButton() {
		Node createKeys = driver.find("CREATE KEYS");
		driver.clickOn(createKeys);
		return this;
	}

	public KeysPanePage pressContinue() {
		ObservableList<Node> popupNodes;
		ObservableList<Node> buttons;
		popupNodes = getPopupNodes();
		assert popupNodes != null;
		assertEquals(3, popupNodes.size());
		assertTrue(popupNodes.get(2) instanceof ButtonBar);

		buttons = ((ButtonBar) popupNodes.get(2)).getButtons();
		assertEquals(1, buttons.size());
		assertEquals("CONTINUE", ((Button) buttons.get(0)).getText());
		driver.clickOn(buttons.get(0));
		return this;
	}

	public void importAllKeys(String pathToKey) throws IOException {
		File keyFile = new File(pathToKey);
		assertTrue(keyFile.isDirectory());
		if (parseBoolean(System.getProperty("headless"))) {
			logger.info("Cannot run headless test");
			File[] allFiles = new File(pathToKey).listFiles(
					(dir, name) -> name.endsWith(PUB_EXTENSION) || name.endsWith(TXT_EXTENSION) || name.endsWith(
							PK_EXTENSION));
			assert allFiles != null;
			for (File aFile : allFiles) {
				FileUtils.copyFile(aFile, new File(defaultStorage + "/Keys/" + aFile.getName()));
			}

		} else {
			driver.clickOn("#btnImportKeys");
			String fullPath = keyFile.getAbsolutePath();
			TestUtil.applyPath(fullPath);
			driver.push(KeyCode.CONTROL, KeyCode.A);
			driver.push(KeyCode.ENTER);
		}
	}
}
