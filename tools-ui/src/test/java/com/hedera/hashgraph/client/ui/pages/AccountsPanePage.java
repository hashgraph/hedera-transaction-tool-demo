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
import com.hedera.hashgraph.client.ui.utilities.AccountLineInformation;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.controlsfx.control.table.TableRowExpanderColumn;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_SCROLL_PANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CURRENT_ACCOUNT_PANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.HIDDEN_ACCOUNT_INFO_TEXTFIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.IMPORT_ACCOUNT_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_CREATE_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_GENERATE_KEYS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NICKNAME;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASSWORD_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.RETYPE_PASSWORD_BOX;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static java.lang.Thread.sleep;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class AccountsPanePage {

	private final TestBase driver;

	public AccountsPanePage(final TestBase driver) {
		this.driver = driver;
	}

	public AccountsPanePage pressGenerateKeyButton() {
		final ScrollPane pane = driver.find(CURRENT_ACCOUNT_PANE);
		pane.setVvalue(1.0);
		driver.clickOn(KEYS_GENERATE_KEYS);
		return this;
	}

	public AccountsPanePage enterNickName(final String nickname) {
		((TextField) driver.find(NICKNAME)).clear();
		driver.clickOn(NICKNAME).write(nickname);
		return this;
	}

	public AccountsPanePage enterPassword(final String password) {
		((PasswordField) driver.find(PASSWORD_BOX)).clear();
		driver.clickOn(PASSWORD_BOX).write(password);
		return this;
	}

	public AccountsPanePage enterRepeatPassword(final String password) {
		((PasswordField) driver.find(RETYPE_PASSWORD_BOX)).clear();
		driver.clickOn(RETYPE_PASSWORD_BOX).write(password);
		return this;
	}

	public AccountsPanePage pressCreateKeysButton() {
		driver.clickOn(KEYS_CREATE_KEYS);
		return this;
	}

	public Node find(final String s) {
		return driver.find(s);
	}

	public AccountsPanePage closePopup(final String query) {
		final ObservableList<Node> nodes = getPopupNodes();
		assert nodes != null;
		driver.clickOn(query.toUpperCase());
		return this;
	}

	public AccountsPanePage enterAccountNickName(final String nickName) {
		final ObservableList<Node> nodes = getPopupNodes();
		assert nodes != null;
		final TextField textField = (TextField) nodes.get(1);
		textField.setText(nickName);
		return this;
	}

	public AccountsPanePage closeNicknamePopup() {
		final ObservableList<Node> nodes = getPopupNodes();
		assert nodes != null;
		final HBox hBox = (HBox) nodes.get(2);
		final HBox hBox1 = ((HBox) hBox.getChildren().get(1));
		final Button button = (Button) hBox1.getChildren().get(1);
		driver.clickOn(button);
		return this;
	}

	public AccountsPanePage deleteAccount(final String nickname) {
		driver.clickOn(nickname + "T");
		driver.clickOn("CONTINUE");
		return this;
	}

	public List<AccountLineInformation> getAccounts() {
		final List<AccountLineInformation> accounts = new ArrayList<>();
		final ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		final Node table = scrollPane.getContent();
		if (table instanceof TableView) {
			final var items = ((TableView) table).getItems();
			for (final Object item : items) {
				assertTrue(item instanceof AccountLineInformation);
				accounts.add((AccountLineInformation) item);
			}
		}
		return accounts;
	}

	public AccountsPanePage expandRow(final String nickname) {
		final ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		final Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		AccountLineInformation info = null;
		final TableView accountTable = (TableView) table;
		int row = 0;
		for (final Object item : accountTable.getItems()) {
			assertTrue(item instanceof AccountLineInformation);
			if (((AccountLineInformation) item).getNickname().equals(nickname)) {
				info = (AccountLineInformation) item;
				break;
			} else {
				row++;
			}
		}

		assertTrue(accountTable.getColumns().get(0) instanceof TableRowExpanderColumn);
		final TableRowExpanderColumn<AccountLineInformation> expanderColumn =
				(TableRowExpanderColumn<AccountLineInformation>) accountTable.getColumns().get(0);
		if (info != null && !expanderColumn.getExpandedProperty(info).get()) {
			expanderColumn.toggleExpanded(row);
		}

		return this;
	}

	public AccountsPanePage enterPasswordInPopup(final String text) throws HederaClientException {
		final var popupNodes = getPopupNodes();
		final var passwords = TestUtil.findPasswordInPopup(Objects.requireNonNull(popupNodes));
		if (passwords == null) {
			throw new HederaClientException("Unexpected popup");
		}
		final var continueButton = findButtonInPopup(popupNodes, "CONFIRM");
		passwords.setText(text);

		driver.clickOn(continueButton);
		return this;
	}

	/**
	 * Given a list of nodes that originate in a popup, find the button whose text is equal to the provided legend
	 *
	 * @param popupNodes
	 * 		a list of nodes
	 * @param legend
	 * 		the text in the button
	 * @return a button
	 */
	public static Button findButtonInPopup(final ObservableList<Node> popupNodes, final String legend) {
		for (final var popupNode : popupNodes) {
			if (popupNode instanceof Button && legend.equalsIgnoreCase(((Button) popupNode).getText())) {
				return (Button) popupNode;
			} else if (popupNode instanceof ButtonBar) {
				final var f = findButtonInPopup(((ButtonBar) popupNode).getButtons(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof VBox) {
				final var f = findButtonInPopup(((VBox) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof HBox) {
				final var f = findButtonInPopup(((HBox) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof GridPane) {
				final var f = findButtonInPopup(((GridPane) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			}
		}
		return null;
	}

	public AccountsPanePage pressPopupButton(final String legend) {
		final var popupNodes = getPopupNodes();
		final var button = findButtonInPopup(Objects.requireNonNull(popupNodes), legend);
		driver.clickOn(button);
		return this;
	}

	public AccountsPanePage selectRow(final String nickname) throws InterruptedException {
		sleep(1000);
		final ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		assertTrue(scrollPane.getContent() instanceof TableView);
		final TableView<AccountLineInformation> table = (TableView<AccountLineInformation>) scrollPane.getContent();
		AccountLineInformation info = null;
		for (final AccountLineInformation item : table.getItems()) {
			if (nickname.equals(item.getNickname())) {
				info = item;
			}
		}

		assertNotNull(info);
		info.setSelected(true);
		return this;
	}

	public AccountsPanePage requestSelectedInfo() throws InterruptedException {
		sleep(1000);
		final ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		assertTrue(scrollPane.getContent() instanceof TableView);
		final TableView<AccountLineInformation> table = (TableView<AccountLineInformation>) scrollPane.getContent();

		final var columns = table.getColumns();
		final var graphic = columns.get(3).getGraphic();
		driver.clickOn(((HBox) graphic).getChildren().get(1));
		return this;
	}

	public AccountsPanePage loadInfoFromHiddenTextField(final String location) {

		final TextField path = driver.find(HIDDEN_ACCOUNT_INFO_TEXTFIELD);
		path.setText(location);
		final Node x = find(IMPORT_ACCOUNT_BUTTON);
		driver.clickOn(x);
		//		driver.clickOn(path).press(KeyCode.ENTER).release(KeyCode.ENTER);

		return this;
	}

	public AccountsPanePage dontReplaceAccount() {
		final ObservableList<Node> popupNodes = getPopupNodes();
		assert popupNodes != null;
		final HBox buttons = (HBox) popupNodes.get(1);

		final Button decline = (Button) buttons.getChildren().get(0);

		driver.clickOn(decline);
		return this;
	}

	public AccountsPanePage replaceAccount() {
		final ObservableList<Node> popupNodes = getPopupNodes();
		assert popupNodes != null;
		final HBox buttons = (HBox) popupNodes.get(1);

		final Button replace = (Button) buttons.getChildren().get(1);

		driver.clickOn(replace);
		return this;
	}

	public AccountsPanePage scrollPane(final double vValue) {
		final ScrollPane scrollPane = driver.find(CURRENT_ACCOUNT_PANE);
		scrollPane.setVvalue(vValue);
		return this;
	}

	public AccountsPanePage clickOn(final Button button) {
		driver.clickOn(button);
		return this;
	}


	public AccountsPanePage clickOnSeeHistory() {
		driver.clickOn("History");
		return this;
	}

	public TableView getTableFromPopup(final ObservableList<Node> nodes) {
		for (final Node node : nodes) {
			if (node instanceof TableView) {
				return (TableView) node;
			}
			if (node instanceof HBox) {
				return getTableFromPopup(((HBox) node).getChildren());
			}
			if (node instanceof VBox) {
				return getTableFromPopup(((VBox) node).getChildren());
			}
		}
		return null;
	}


}
