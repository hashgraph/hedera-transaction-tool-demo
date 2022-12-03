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
import com.hedera.hashgraph.client.ui.utilities.AutoCompleteTextField;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_RECOVER_KEYS_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_RECOVER_KEY_INDEX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_RECOVER_KEY_NICKNAME;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CANCEL_RECOVER_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CURRENT_ACCOUNT_PANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_CANCEL_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_CREATE_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_GENERATE_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_MNEMONIC_CLOSE_VIEW;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_RECOVERY_PHRASE_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_RECOVER_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NICKNAME;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findButtonInPopup;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class KeysPanePage {

	private final TestBase driver;

	public KeysPanePage(final TestBase driver) {
		this.driver = driver;
	}

	public void typePassword(final String s, final PasswordField field) {
		field.setText(s);
		driver.clickOn(field);
		driver.type(KeyCode.SHIFT);
	}

	public KeysPanePage pressGenerateKeyButton() {
		driver.ensureVisible(driver.find(KEYS_GENERATE_KEYS));
		driver.clickOn(KEYS_GENERATE_KEYS);
		return this;
	}

	public KeysPanePage enterNickName(final String nickname) {
		((TextField) driver.find(NICKNAME)).clear();
		driver.clickOn(NICKNAME).write(nickname);
		return this;
	}

	public KeysPanePage enterPopupPassword(final String password) {
		final ObservableList<Node> nodes = driver.getPopupNodes();
		assert nodes != null;
		for (final Node n : nodes) {
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
		final ObservableList<Node> nodes = driver.getPopupNodes();
		assert nodes != null;
		final ObservableList<Node> buttons = ((HBox) nodes.get(3)).getChildren();
		for (final Node button : buttons) {
			if (((Button) button).getText().equalsIgnoreCase("cancel")) {
				driver.clickOn(button);
				return this;
			}
		}
		throw new HederaClientRuntimeException("Cannot find password field");
	}

	public KeysPanePage pressCreateKeysButton() {
		driver.clickOn(KEYS_CREATE_KEYS);
		return this;
	}

	public Node find(final String s) {
		return driver.find(s);
	}

	public KeysPanePage closePopup(final String query) {
		final Set<Node> nodes = driver.findAll(query.toUpperCase());

		for (final Node node : nodes) {
			if (node instanceof Button && node.isVisible()) {
				driver.clickOn(node);
			}
		}
		return this;
	}

	public KeysPanePage closeOKPopup() {
		final ObservableList<Node> x = driver.getPopupNodes();
		HBox hBox = null;
		assert x != null;
		if (x.get(0) instanceof VBox) {
			final ObservableList<Node> y = ((VBox) x.get(0)).getChildren();
			hBox = (y.get(1) instanceof HBox) ? (HBox) y.get(1) : null;
		}
		assert hBox != null;
		for (final Node node : hBox.getChildren()) {
			if (node instanceof Button) {
				driver.clickOn(node);
			}
		}
		return this;
	}

	public KeysPanePage scrollPane(final double vValue) {
		final ScrollPane scrollPane = driver.find(CURRENT_ACCOUNT_PANE);
		scrollPane.setVvalue(vValue);
		return this;
	}

	public KeysPanePage cancelCreateKeys() {
		driver.clickOn((Button) driver.find(KEYS_CANCEL_KEYS));
		return this;
	}

	public KeysPanePage clickOn(final Button button) {
		driver.clickOn(button);
		return this;
	}

	public KeysPanePage pressRecoverKeysButton() {
		driver.clickOn(KEYS_RECOVER_KEYS);
		return this;
	}

	public KeysPanePage enterIndex(final int index) {
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

	public KeysPanePage enterRecoverNickname(final String nickname) {
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

	public List<Label> getPopupLabels() {
		final var popupNodes = driver.getPopupNodes();
		assert popupNodes != null;
		return getAllLabels(popupNodes);
	}

	List<Label> getAllLabels(final ObservableList<Node> nodes) {
		final List<Label> labels = new ArrayList<>();
		for (final Node node : nodes) {
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
		final var popupNodes = driver.getPopupNodes();
		assert popupNodes != null;
		return getAllButtons(popupNodes);
	}

	List<Button> getAllButtons(final ObservableList<Node> nodes) {
		final List<Button> buttons = new ArrayList<>();
		for (final Node node : nodes) {
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
		final var popupNodes = driver.getPopupNodes();
		assert popupNodes != null;
		return getAllPasswordFields(popupNodes);
	}

	public List<Label> getPopupErrorFields() {
		final var popupNodes = driver.getPopupNodes();
		assert popupNodes != null;
		return getAllErrorFields(popupNodes);
	}


	List<PasswordField> getAllPasswordFields(final ObservableList<Node> nodes) {
		final List<PasswordField> passwordFields = new ArrayList<>();
		for (final Node node : nodes) {
			if (node instanceof HBox) {
				passwordFields.addAll(getAllPasswordFields(((HBox) node).getChildren()));
			}
			if (node instanceof VBox) {
				passwordFields.addAll(getAllPasswordFields(((VBox) node).getChildren()));
			}
			if (node instanceof PasswordField) {
				passwordFields.add((PasswordField) node);
			}
		}
		return passwordFields;
	}

	List<Label> getAllErrorFields(final ObservableList<Node> nodes) {
		final List<Label> labels = new ArrayList<>();
		for (final Node node : nodes) {
			if (node instanceof HBox) {
				labels.addAll(getAllErrorFields(((HBox) node).getChildren()));
			}
			if (node instanceof VBox) {
				labels.addAll(getAllErrorFields(((VBox) node).getChildren()));
			}
			if (node instanceof Label) {
				final var label = (Label) node;
				if (label.getStyle().contains("red")) {
					labels.add(label);
				}
			}
		}
		return labels;
	}

	public KeysPanePage pressCloseViewMnemonic() {
		driver.clickOn(KEYS_MNEMONIC_CLOSE_VIEW);
		return this;
	}

	public KeysPanePage pressPopupButton(final String legend) {
		final var buttons = getPopupButtons();
		for (final Button n : buttons) {
			if (n != null && legend.equals(n.getText())) {
				driver.clickOn(n);
				break;
			}
		}
		return this;
	}

	public KeysPanePage closePasswordPopup() {
		final ObservableList<Node> nodes = driver.getPopupNodes();
		assert nodes != null;
		final HBox hBox = (HBox) nodes.get(3);
		final Button button = (Button) hBox.getChildren().get(0);
		driver.clickOn(button);
		return this;
	}

	public KeysPanePage pressOnCreateKeysButton() {
		final Node createKeys = driver.find("CREATE KEYS");
		driver.clickOn(createKeys);
		return this;
	}

	public KeysPanePage pressContinue() {
		final ObservableList<Node> popupNodes;
		final ObservableList<Node> buttons;
		popupNodes = driver.getPopupNodes();
		assert popupNodes != null;
		assertEquals(3, popupNodes.size());
		assertTrue(popupNodes.get(2) instanceof ButtonBar);

		buttons = ((ButtonBar) popupNodes.get(2)).getButtons();
		assertEquals(1, buttons.size());
		assertEquals("CONTINUE", ((Button) buttons.get(0)).getText());
		driver.clickOn(buttons.get(0));
		return this;
	}

	public KeysPanePage pressPopupHyperlink() {
		final ObservableList<Node> popupNodes;
		popupNodes = driver.getPopupNodes();
		assert popupNodes != null;
		popupNodes.stream().filter(popupNode -> popupNode instanceof HBox && ((HBox) popupNode).getChildren().get(
				0) instanceof Hyperlink).map(popupNode -> (Hyperlink) ((HBox) popupNode).getChildren().get(0)).forEach(
				driver::clickOn);
		return this;
	}

	public KeysPanePage clickOnPopupButton(final String buttonName) {
		final var button = findButtonInPopup(Objects.requireNonNull(driver.getPopupNodes()), buttonName);
		clickOn(button);
		return this;
	}

	public KeysPanePage setWords(final List<String> words) {

		assertNotNull(words);

		for (final String w : words) {
			setNextWord(w);
		}
		return this;
	}

	public KeysPanePage setNextWord(final String word) {

		assertNotNull(word);
		final var nodes = driver.getPopupNodes();
		assert nodes != null;
		final GridPane fullGrid = (GridPane) ((VBox) nodes.get(1)).getChildren().get(0);
		final ObservableList<Node> full = fullGrid.getChildren();
		for (final Node n : full) {
			assert n instanceof TextField;
			if (((TextField) n).getText().isEmpty()) {
				driver.clickOn(n);
				driver.write(word);
				break;
			}
		}
		return this;
	}

	public KeysPanePage enterPasswordAndConfirm(final String password) {
		final var passwordFields = getPopupPasswordFields();
		passwordFields.get(0).clear();
		typePassword(password, passwordFields.get(0));
		typePassword(password, passwordFields.get(1));
		return this;
	}

	public KeysPanePage createKey(final String name, final String password) {
		return pressGenerateKeyButton()
				.enterNickName(name)
				.pressOnCreateKeysButton()
				.enterPopupPassword(password)
				.closePasswordPopup();

	}

	public KeysPanePage pressHyperlinkPassword(final String message) {
		final var popupNodes = driver.getPopupNodes();
		if (popupNodes != null) {
			final var links = TestUtil.findHyperlinksInPopup(popupNodes);
			for (final Hyperlink link : links) {
				if (message.equalsIgnoreCase(link.getText())) {
					driver.clickOn(link);
					break;
				}
			}
		}
		return this;
	}

	public KeysPanePage enteMnemonicInPopup(final String mnemonic) {
		final var mnemonicArray = mnemonic.toLowerCase(Locale.ROOT).split(" ");
		final var popupNodes = driver.getPopupNodes();
		if (popupNodes != null) {
			final var grid = TestUtil.findGridPanesInPopup(popupNodes);
			if (grid.size() == 1) {
				final var children = grid.get(0).getChildren();
				if (children.size() != mnemonicArray.length) {
					return this;
				}
				int counter = 0;
				for (final Node child : children) {
					if (child instanceof AutoCompleteTextField) {
						driver.clickOn(child);
						driver.write(mnemonicArray[counter++]);
					}
				}
			}
		}
		return this;
	}
}
