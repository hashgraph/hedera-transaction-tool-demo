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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.ui.utilities;

import javafx.geometry.Side;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.CustomMenuItem;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * Class extends a textfield to allow for an "autocorrect" from a provided list. As the user types in the the textbox, a
 * list of suggestions will appear. These suggestions will originate in the provided list of words.
 * This class will be used to suggest to the user one of the nicknames of the accounts on record. If the user types a
 * word that is not in the list, it will show in red.
 */
public class AutoCompleteNickname extends TextField {
	private final ContextMenu entriesPopup;
	private static final String styleString =
			"-fx-background-color: white;-fx-border-color: silver;-fx-background-radius: 10;" +
					"-fx-border-radius: 10;";
	private final Set<String> names;
	private String nickname = "";
	AtomicBoolean noise = new AtomicBoolean(false);
	AtomicBoolean valid = new AtomicBoolean(true);

	/**
	 * Constructor
	 *
	 * @param names
	 * 		The list of strings that the class will use to check if the proposed string exists
	 */
	public AutoCompleteNickname(Set<String> names) {
		super();
		this.names = names;
		setStyle(styleString);

		entriesPopup = new ContextMenu();
		textProperty().addListener((observableValue, s, s2) -> {
			var text = s2.toLowerCase().replace(" ", "");
			if (getText().length() == 0) {
				entriesPopup.hide();
			} else if (!noise.get()) {
				var searchResult = suggestWords(text);
				if (!searchResult.isEmpty()) {
					setStyle(styleString + "-fx-text-fill: black");
					valid.set(true);
					populatePopup(searchResult);
					if (!entriesPopup.isShowing()) {
						entriesPopup.show(AutoCompleteNickname.this, Side.BOTTOM, 0, 0);
					}
					if (searchResult.size() == 1) {
						if (text.equals(searchResult.get(0))) {
							entriesPopup.hide();
						} else {
							entriesPopup.getSkin().getNode().lookup(".menu-item").requestFocus();
						}
					}
				} else {
					setStyle(styleString + "-fx-text-fill: red");
					valid.set(false);
					entriesPopup.hide();
				}
			}
		});

		focusedProperty().addListener((observableValue, aBoolean, aBoolean2) -> entriesPopup.hide());
		setOnKeyReleased(keyEvent -> {
			if (keyEvent.getCode().equals(KeyCode.DOWN) && !noise.get()) {
				entriesPopup.getSkin().getNode().lookup(".menu-item").requestFocus();
				noise.set(true);
			}
			if (keyEvent.getCode().equals(KeyCode.ENTER) || keyEvent.getCode().equals(KeyCode.TAB)) {
				if (!valid.get()) {
					return;
				}
				noise.set(false);
				var text = this.getText().toLowerCase().replace(" ", "");

				final var firstItem = getFirstItem();
				if (!"".equals(text) && (text.equals(firstItem) || isSingleItem())) {
					setText(firstItem);
					nickname = firstItem;
				}
			}
		});


	}

	public void setDefault(String defaultName) {
		if (names.contains(defaultName)) {
			noise.set(true);
			this.setText(defaultName);
			noise.set(false);
		}
	}

	/**
	 * Returns the chosen nickname
	 *
	 * @return
	 */
	public String getNickname() {
		return nickname;
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

	/**
	 * Returns a list of words that start with the suggested text
	 *
	 * @param text
	 * 		a String containing the first few letters of a string
	 * @return A subset of the words list, whose members start with the input text
	 */
	public List<String> suggestWords(String text) {
		var filteredEntries =
				Arrays.stream(names.toArray(new String[0])).filter(
						e -> e.toLowerCase().contains(text.toLowerCase())).sorted(
						Comparator.naturalOrder()).collect(Collectors.toList());

		return filteredEntries.isEmpty() ? new ArrayList<>() : filteredEntries;
	}
}
