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

package com.hedera.hashgraph.client.ui.utilities;

import com.hedera.hashgraph.client.core.security.Dictionary;
import javafx.geometry.Side;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.CustomMenuItem;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

public class AutoCompleteTextField extends TextField {

	/** The popup used to select an entry. */
	private final ContextMenu entriesPopup;
	private String styleString;

	public void setStyleString(String styleString) {
		this.styleString = styleString;
	}

	/** Construct a new AutoCompleteTextField. */
	public AutoCompleteTextField() {
		super();
		var dictionary = new Dictionary();
		var noise = new AtomicBoolean(true);
		entriesPopup = new ContextMenu();
		textProperty().addListener((observableValue, s, s2) -> handleTextPropertyListener(dictionary, noise, s2));

		focusedProperty().addListener((observableValue, aBoolean, aBoolean2) -> entriesPopup.hide());

		addEventFilter(KeyEvent.KEY_PRESSED, event -> {
			if (event.getCode() == KeyCode.TAB) {
				event.consume();
			}
		});

		setOnKeyReleased(keyEvent -> handleKeyReleasedEvent(noise, keyEvent));


	}

	private void handleKeyReleasedEvent(AtomicBoolean noise, KeyEvent keyEvent) {
		if (keyEvent.getCode().equals(KeyCode.DOWN) && !noise.get()) {
			entriesPopup.getSkin().getNode().lookup(".menu-item").requestFocus();
			noise.set(true);
		}

		if (!keyEvent.getCode().equals(KeyCode.ENTER) && !keyEvent.getCode().equals(KeyCode.TAB)) {
			return;
		}

		var text = this.getText().toLowerCase().replace(" ", "");

		if ("".equals(text) || (!text.equals(getFirstItem()) && !isSingleItem())) {
			return;
		}

		setText(getFirstItem());
		goToNextTextField();
	}

	private void handleTextPropertyListener(Dictionary dictionary, AtomicBoolean noise, String s2) {
		var text = s2.toLowerCase().replace(" ", "");
		if (getText().length() != 0) {
			noise.set(false);
			var searchResult = dictionary.suggestWords(text);
			if (searchResult.isEmpty()) {
				setStyle(styleString + "-fx-text-fill: red");
				entriesPopup.hide();
			} else {
				handleNonEmptySearch(text, searchResult);
			}
		} else {
			entriesPopup.hide();
		}
	}

	private void handleNonEmptySearch(String text, List<String> searchResult) {
		setStyle(styleString + "-fx-text-fill: black");
		populatePopup(searchResult);
		if (!entriesPopup.isShowing()) {
			entriesPopup.show(AutoCompleteTextField.this, Side.BOTTOM, 0, 0);
		}
		if (searchResult.size() == 1) {
			if (text.equals(searchResult.get(0))) {
				entriesPopup.hide();
			} else {
				entriesPopup.getSkin().getNode().lookup(".menu-item").requestFocus();
			}
		}
	}

	/**
	 * Simulates a tab press
	 */
	private void goToNextTextField() {
		var grid = getParent();
		var fields = grid.getChildrenUnmodifiable();
		var count = 0;
		for (var field : fields) {
			count++;
			if (field.equals(this)) {
				break;
			}
		}
		if (count != fields.size()) {
			fields.get(count).requestFocus();
		}
	}

	/**
	 * Returns the first word in the popup.
	 *
	 * @return the text of the first word in the popup
	 */
	public String getFirstItem() {
		if (!entriesPopup.getItems().isEmpty()) {
			return ((Label) ((CustomMenuItem) entriesPopup.getItems().get(0)).getContent()).getText();
		}
		return "";
	}

	/**
	 * Returns true if the popup list has only one item
	 *
	 * @return a boolean
	 */
	public boolean isSingleItem() {
		return entriesPopup.getItems().size() == 1;
	}

	/**
	 * Populate the entry set with the given search results.  Display is limited to 10 entries, for performance.
	 *
	 * @param searchResult
	 * 		The set of matching strings.
	 */
	private void populatePopup(List<String> searchResult) {
		List<CustomMenuItem> menuItems = new LinkedList<>();
		// If you'd like more entries, modify this line.
		var maxEntries = 10;
		var count = Math.min(searchResult.size(), maxEntries);
		for (var i = 0; i < count; i++) {
			final var result = searchResult.get(i);
			var entryLabel = new Label(result);
			var item = new CustomMenuItem(entryLabel, true);
			item.setOnAction(actionEvent -> {
				setText(result);
				entriesPopup.hide();
			});

			menuItems.add(item);
		}
		entriesPopup.getItems().clear();
		entriesPopup.getItems().addAll(menuItems);

	}

}

