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

import com.hedera.hashgraph.client.ui.TestBase;
import com.hedera.hashgraph.client.ui.utilities.AccountLineInformation;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
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
import static org.junit.Assert.assertTrue;

public class AccountsPanePage {

	private final TestBase driver;

	public AccountsPanePage(TestBase driver) {
		this.driver = driver;
	}

	public AccountsPanePage pressGenerateKeyButton() {
		ScrollPane pane = driver.find(CURRENT_ACCOUNT_PANE);
		pane.setVvalue(1.0);
		driver.clickOn(KEYS_GENERATE_KEYS);
		return this;
	}

	public AccountsPanePage enterNickName(String nickname) {
		((TextField) driver.find(NICKNAME)).clear();
		driver.clickOn(NICKNAME).write(nickname);
		return this;
	}

	public AccountsPanePage enterPassword(String password) {
		((PasswordField) driver.find(PASSWORD_BOX)).clear();
		driver.clickOn(PASSWORD_BOX).write(password);
		return this;
	}

	public AccountsPanePage enterRepeatPassword(String password) {
		((PasswordField) driver.find(RETYPE_PASSWORD_BOX)).clear();
		driver.clickOn(RETYPE_PASSWORD_BOX).write(password);
		return this;
	}

	public AccountsPanePage pressCreateKeysButton() {
		driver.clickOn(KEYS_CREATE_KEYS);
		return this;
	}

	public Node find(String s) {
		return driver.find(s);
	}

	public AccountsPanePage closePopup(String query) {
		ObservableList<Node> nodes = getPopupNodes();
		assert nodes != null;
		Button queryButton = null;
		driver.clickOn(query.toUpperCase());
		return this;
	}

	public AccountsPanePage pressImportAccountButton() {
		driver.clickOn("IMPORT ACCOUNT");
		return this;
	}

	public AccountsPanePage enterAccountNickName(String nickName) {
		ObservableList<Node> nodes = getPopupNodes();
		assert nodes != null;
		TextField textField = (TextField) nodes.get(1);
		textField.setText(nickName);
		return this;
	}

	public AccountsPanePage closeNicknamePopup() {
		ObservableList<Node> nodes = getPopupNodes();
		assert nodes != null;
		HBox hBox = (HBox) nodes.get(2);
		HBox hBox1 = ((HBox) hBox.getChildren().get(1));
		Button button = (Button) hBox1.getChildren().get(1);
		driver.clickOn(button);
		return this;
	}

	public AccountsPanePage setPemLocation(String location) {
		int i = 1;
		return enterAddressInBox(location, i);
	}

	public AccountsPanePage setPubLocation(String location) {
		int i = 2;
		return enterAddressInBox(location, i);
	}

	private AccountsPanePage enterAddressInBox(String location, int i) {
		ObservableList<Node> keysPopupNodes = getPopupNodes();

		Button x = ((Button) (((HBox) (((VBox) Objects.requireNonNull(keysPopupNodes).get(i)).getChildren()).get(
				1)).getChildren()).get(1));
		driver.clickOn(x);

		TestUtil.applyPath(location);

		return this;
	}

	public AccountsPanePage browseToAccountInfo(String location) {
		TestUtil.applyPath(location);
		return this;
	}

	public AccountsPanePage deleteAccount(String nickname) {
		driver.clickOn(nickname + "T");
		driver.clickOn("CONTINUE");
		return this;
	}

	public List<AccountLineInformation> getAccounts() {
		List<AccountLineInformation> accounts = new ArrayList<>();
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		Node table = scrollPane.getContent();
		if (table instanceof TableView) {
			var items = ((TableView) table).getItems();
			for (Object item : items) {
				assertTrue(item instanceof AccountLineInformation);
				accounts.add((AccountLineInformation) item);
			}
		}
		return accounts;
	}

	public AccountsPanePage expandRow(String nickname) {
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		AccountLineInformation info = null;
		final TableView accountTable = (TableView) table;
		int row = 0;
		for (Object item : accountTable.getItems()) {
			assertTrue(item instanceof AccountLineInformation);
			if (((AccountLineInformation) item).getNickname().equals(nickname)) {
				info = (AccountLineInformation) item;
				break;
			} else {
				row++;
			}
		}

		assertTrue(accountTable.getColumns().get(0) instanceof TableRowExpanderColumn);
		TableRowExpanderColumn<AccountLineInformation> expanderColumn =
				(TableRowExpanderColumn<AccountLineInformation>) accountTable.getColumns().get(0);
		if (info != null && !expanderColumn.getExpandedProperty(info).get()) {
			expanderColumn.toggleExpanded(row);
		}

		return this;
	}

	public AccountsPanePage enterKeysPathsInPopup(String pemLocation, String pubLocation) {

		ObservableList<Node> popupNodes = getPopupNodes();
		TextField pemField = (TextField) ((HBox) ((VBox) Objects.requireNonNull(popupNodes).get(1)).getChildren().get(
				1)).getChildren().get(0);
		TextField pubField = (TextField) ((HBox) ((VBox) popupNodes.get(2)).getChildren().get(1)).getChildren().get(0);

		driver.clickOn(pemField).write(pemLocation);
		driver.clickOn(pubField).write(pubLocation).press(KeyCode.ENTER).release(KeyCode.ENTER);

		return this;
	}

	public AccountsPanePage loadInfoFromHiddenTextField(String location) {

		TextField path = driver.find(HIDDEN_ACCOUNT_INFO_TEXTFIELD);
		path.setText(location);
		Node x = find(IMPORT_ACCOUNT_BUTTON);
		driver.clickOn(x);
		//		driver.clickOn(path).press(KeyCode.ENTER).release(KeyCode.ENTER);

		return this;
	}

	public AccountsPanePage dontReplaceAccount() {
		ObservableList<Node> popupNodes = getPopupNodes();
		assert popupNodes != null;
		HBox buttons = (HBox) popupNodes.get(1);

		Button decline = (Button) buttons.getChildren().get(0);

		driver.clickOn(decline);
		return this;
	}

	public AccountsPanePage replaceAccount() {
		ObservableList<Node> popupNodes = getPopupNodes();
		assert popupNodes != null;
		HBox buttons = (HBox) popupNodes.get(1);

		Button replace = (Button) buttons.getChildren().get(1);

		driver.clickOn(replace);
		return this;
	}

	public AccountsPanePage scrollPane(double vValue) {
		ScrollPane scrollPane = driver.find(CURRENT_ACCOUNT_PANE);
		scrollPane.setVvalue(vValue);
		return this;
	}

	public AccountsPanePage clickOn(Button button) {
		driver.clickOn(button);
		return this;
	}

	public AccountsPanePage scrollToBottom() {
		Node pane = driver.find("#currentAccountPane");
		if (pane instanceof ScrollPane) {
			((ScrollPane) pane).setVvalue(1.0);
		}
		return this;
	}


}
