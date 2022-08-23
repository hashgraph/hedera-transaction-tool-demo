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

package com.hedera.hashgraph.client.core.constants;

import static com.hedera.hashgraph.client.core.constants.Constants.FIXED_CELL_SIZE;

public final class StyleConstants {

	public static final String MENU_BUTTON_HIGHLIGHT_COLOR =
			"-fx-background-color:  #f4f4f4; -fx-border-color:  #f4f4f4";
	public static final String WHITE_BUTTON_STYLE =
			"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: 10; " +
					"-fx-background-radius: 10;";
	public static final String DEBIT = "-fx-text-fill:RED";
	public static final String CREDIT = "-fx-text-fill:GREEN";
	public static final String HISTORY_BOX_STYLE =
			"-fx-background-color: aliceblue; -fx-border-color: grey; -fx-background-radius: 15; " +
					"-fx-border-radius: 15; -fx-border-width: 2";
	public static final String REGULAR_BOX_STYLE =
			"-fx-background-color: white; -fx-border-color: lightgrey; -fx-background-radius: 15; " +
					"-fx-border-radius: 15; -fx-border-width: 2";
	public static final String START_STYLE = "-fx-background-radius: 10; -fx-border-radius: 10;";
	public static final String MENU_BUTTON_STYLE =
			"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: 10; " +
					"-fx-background-radius: 10;";
	public static final String TEXTFIELD_ERROR = "-fx-text-fill: red; -fx-background-radius: 10;-fx-border-radius: 10";
	public static final String TEXTFIELD_DEFAULT =
			"-fx-text-fill: black; -fx-background-radius: 10;-fx-border-radius: 10";
	public static final String WHITE_BACKGROUND_STYLE = "-fx-background-color: white;";
	public static final String BOX_STYLE =
			"-fx-border-radius: 10; -fx-background-radius: 10; -fx-background-color: white; -fx-border-color: " +
					"lightgray";
	public static final String COLUMN_STYLE_STRING = "-fx-alignment: TOP-CENTER; -fx-padding: 10";
	public static final String FONT_SIZE_2 = "-fx-font-size: 2";
	public static final String TRANSPARENT_BUTTON_STYLE = "-fx-background-color: transparent; -fx-border-color: " +
			"transparent";
	public static final String WHITE_BORDER_STYLE = "-fx-border-color: white";
	public static final String CHOICEBOX_STYLE = "-fx-background-color: white; -fx-border-color: #c2c2c2; " +
			"-fx-background-radius: 10; " +
			"-fx-border-radius: 10";
	public static final String FX_TEXT_FILL_BLACK = "-fx-text-fill: black; -fx-background-color: white";
	public static final String WHITE_TREEVIEW_STYLE = "-fx-border-color: white; -fx-background-color: white";
	public static final String TABLE_STYLE = "-fx-font-size: " + FIXED_CELL_SIZE / 2;
	public static final String NULL_STYLE = null;
	public static final String FONT_SIZE_2_STYLE = "-fx-font-size: 2";
	public static final String TITLE_STYLE = "-fx-border-color: transparent;-fx-background-color: transparent";
	public static final String TYPE_FILTER_STYLE = "-fx-border-color: gray; -fx-border-radius: 10";
	public static final String TOOLTIP_BUTTON_STYLE = "-fx-background-color: transparent; -fx-border-color: " +
			"transparent";
	public static final String EXTRA_SIGNER_STYLE = "-fx-border-color: darkgray; -fx-border-radius: 5";

	private StyleConstants() {
	}
}
