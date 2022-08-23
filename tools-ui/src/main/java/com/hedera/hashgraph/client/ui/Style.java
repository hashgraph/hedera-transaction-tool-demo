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

package com.hedera.hashgraph.client.ui;

import javafx.scene.Scene;

public final class Style {

	public static final String HIGHLIGHT_BUTTON_CLASS = "highlightButton";

	public static final String INVERTED_HIGHLIGHT_BUTTON_CLASS = "invertedHighlightButton";

	private Style() {
	}

	public static void addStylesheets(final Scene scene) {
		scene.getStylesheets().add("app.css");
		scene.getStylesheets().add("tools.css");
	}
}
