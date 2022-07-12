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

import com.hedera.hashgraph.client.ui.JavaFXIDs;
import com.hedera.hashgraph.client.ui.TestBase;
import com.hedera.hashgraph.client.ui.utilities.HistoryData;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableView;
import javafx.scene.layout.VBox;
import org.controlsfx.control.table.TableRowExpanderColumn;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.*;
import static org.junit.Assert.assertTrue;

@SuppressWarnings("UnusedReturnValue")
public class HistoryPanePage {
	private final TestBase driver;

	public HistoryPanePage(final TestBase driver) {
		this.driver = driver;
	}

	public HistoryPanePage rebuildHistory() {
		driver.find("#rebuild");
		driver.clickOn("#rebuild");
		return this;
	}

	public HistoryPanePage clickOnResign(final String buttonText) {
		if (driver.find(buttonText).isVisible()) {
			driver.clickOn(buttonText);
		}
		return this;
	}

	public HistoryPanePage expandRow(final String text){
		final ScrollPane scrollPane = driver.find(HISTORY_CONTENT_SCROLL_PANE);
		final var table = scrollPane.getContent();
		assertTrue(table instanceof TableView);

		HistoryData info = null;
		final var accountTable = (TableView<HistoryData>) table;
		var row = 0;
		for (final var item : accountTable.getItems()) {
			assertTrue(item instanceof HistoryData);
			if (item.getFileName().equals(text)) {
				info = item;
				break;
			} else {
				row++;
			}
		}

		assertTrue(accountTable.getColumns().get(0) instanceof TableRowExpanderColumn);
		final var expanderColumn =
				(TableRowExpanderColumn<HistoryData>) accountTable.getColumns().get(0);
		if (info != null && !expanderColumn.getExpandedProperty(info).get()) {
			expanderColumn.toggleExpanded(row);
		}

		return this;
	}

	public VBox getExpandedBox(){
		final ScrollPane scrollPane = driver.find(HISTORY_CONTENT_SCROLL_PANE);
		final var tableView = (TableView<HistoryData>) scrollPane.getContent();
		final var tableRowExpanderColumn = (TableRowExpanderColumn) tableView.getColumns().get(0);
		return  (VBox) tableRowExpanderColumn.getExpandedNode(tableView.getItems().get(0));
	}

}
