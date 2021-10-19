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
import com.hedera.hashgraph.client.ui.utilities.AccountLineInformation;
import com.hedera.hashgraph.sdk.Hbar;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import org.controlsfx.control.table.TableRowExpanderColumn;

import java.util.List;
import java.util.Objects;

import static com.hedera.hashgraph.client.integration.TestUtil.findButtonInPopup;
import static com.hedera.hashgraph.client.integration.TestUtil.findCheckBoxInPopup;
import static com.hedera.hashgraph.client.integration.TestUtil.getPopupNodes;
import static java.lang.Thread.sleep;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class AccountsPanePage {
	private final TestBase driver;
	private final static String ACCOUNTS_SCROLL_PANE = "#accountsScrollPane";

	public AccountsPanePage(TestBase driver) {
		this.driver = driver;
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

	public AccountsPanePage requestNewBalance(String nickname) throws InterruptedException {
		sleep(1000);
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		AccountLineInformation info = null;
		final TableView<AccountLineInformation> accountTable = (TableView<AccountLineInformation>) table;
		assertTrue(accountTable.getColumns().get(0) instanceof TableRowExpanderColumn);
		TableRowExpanderColumn<AccountLineInformation> expanderColumn =
				(TableRowExpanderColumn<AccountLineInformation>) accountTable.getColumns().get(0);

		for (AccountLineInformation item : accountTable.getItems()) {
			assertNotNull(item);
			if (item.getNickname().equals(nickname)) {
				info = item;
				break;
			}
		}

		if (info != null && expanderColumn.getExpandedProperty(info).get()) {
			var node = expanderColumn.getExpandedNode(info);
			var button = TestUtil.getButton(node, "");
			driver.clickOn(button);
		}

		return this;
	}

	public Hbar getBalance(String nickname) throws HederaClientException {
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		Node table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		AccountLineInformation info = null;
		final TableView<AccountLineInformation> accountTable = (TableView<AccountLineInformation>) table;
		assertTrue(accountTable.getColumns().get(0) instanceof TableRowExpanderColumn);

		for (AccountLineInformation item : accountTable.getItems()) {
			assertNotNull(item);
			if (item.getNickname().equals(nickname)) {
				info = item;
				break;
			}
		}

		return info.getBalance();
	}

	public AccountsPanePage selectAllCheckBoxes() throws InterruptedException {
		sleep(1000);
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		assertTrue(scrollPane.getContent() instanceof TableView);
		TableView<AccountLineInformation> table = (TableView<AccountLineInformation>) scrollPane.getContent();

		var columns = table.getColumns();
		var graphic = columns.get(1).getGraphic();
		driver.clickOn(graphic);
		return this;
	}

	public AccountsPanePage requestSelectedBalances() throws InterruptedException {
		sleep(1000);
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		assertTrue(scrollPane.getContent() instanceof TableView);
		TableView<AccountLineInformation> table = (TableView<AccountLineInformation>) scrollPane.getContent();

		var columns = table.getColumns();
		var graphic = columns.get(5).getGraphic();
		driver.clickOn(((HBox) graphic).getChildren().get(1));
		return this;
	}

	public AccountsPanePage selectRow(String nickname) throws InterruptedException {
		sleep(1000);
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		assertTrue(scrollPane.getContent() instanceof TableView);
		TableView<AccountLineInformation> table = (TableView<AccountLineInformation>) scrollPane.getContent();
		AccountLineInformation info = null;
		for (AccountLineInformation item : table.getItems()) {
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
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		assertTrue(scrollPane.getContent() instanceof TableView);
		TableView<AccountLineInformation> table = (TableView<AccountLineInformation>) scrollPane.getContent();

		var columns = table.getColumns();
		var graphic = columns.get(3).getGraphic();
		driver.clickOn(((HBox) graphic).getChildren().get(1));
		return this;
	}

	public AccountsPanePage enterPasswordInPopup(String text) throws HederaClientException {
		final var popupNodes = Objects.requireNonNull(com.hedera.hashgraph.client.integration.TestUtil.getPopupNodes());
		var passwords = com.hedera.hashgraph.client.integration.TestUtil.findPasswordInPopup(popupNodes);
		if (passwords == null) {
			throw new HederaClientException("Unexpected popup");
		}
		var continueButton = findButtonInPopup(popupNodes, "CONFIRM");
		passwords.setText(text);

		driver.clickOn(continueButton);
		return this;
	}

	public void pressPopupButton(String legend) {
		final var popupNodes = Objects.requireNonNull(com.hedera.hashgraph.client.integration.TestUtil.getPopupNodes());
		var button = findButtonInPopup(popupNodes, legend);
		driver.clickOn(button);
	}

	public AccountsPanePage openAccordion() {
		driver.clickOn("Update accounts");
		return this;
	}

	public AccountsPanePage enterAccounts(String s) {
		var textField = driver.find("#accountsToUpdateTextField");
		assert textField instanceof TextField;
		((TextField) textField).setText(s);
		return this;
	}

	public AccountsPanePage clickRequestAccountsButton() {
		driver.clickOn("#selectAccountsButton");
		return this;
	}

	public AccountsPanePage acceptAllNickNames() {
		final var popupNodes = Objects.requireNonNull(getPopupNodes());
		CheckBox checkBox = findCheckBoxInPopup(popupNodes, "Apply to all");
		driver.clickOn(checkBox);
		Button button = findButtonInPopup(popupNodes, "ACCEPT");
		driver.clickOn(button);
		return this;
	}

	public List<AccountLineInformation> getAccounts() {
		ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		assertTrue(scrollPane.getContent() instanceof TableView);
		TableView<AccountLineInformation> table = (TableView<AccountLineInformation>) scrollPane.getContent();
		return table.getItems();
	}
}


