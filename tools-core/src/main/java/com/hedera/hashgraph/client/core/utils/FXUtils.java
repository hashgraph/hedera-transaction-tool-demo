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

package com.hedera.hashgraph.client.core.utils;

import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;

import static com.hedera.hashgraph.client.core.utils.CommonMethods.showTooltip;

public class FXUtils {
	private static final Logger logger = LogManager.getLogger(FXUtils.class);

	private FXUtils() {
	}

	/**
	 * Setups a button with an icon
	 *
	 * @param imageLocation
	 * 		the relative location of the image of the icon to be used
	 * @return a button with the image provided
	 */
	public static Button formatButton(final String imageLocation, final int size) {
		final var button = new Button();
		try {
			final var image = new Image(imageLocation);
			final var imageView = new ImageView(image);
			imageView.setFitHeight(size);
			imageView.setPreserveRatio(true);
			button.setGraphic(imageView);
			button.setStyle("-fx-background-color: transparent; -fx-border-color: transparent");
		} catch (final Exception e) {
			logger.error(e.getMessage());
		}
		return button;
	}

	@NotNull
	public static HBox buildTransactionIDBox(final GridPane detailsGridPane, final String transactionID) {
		final var hbox = new HBox();
		hbox.setSpacing(5);
		hbox.setAlignment(Pos.BASELINE_LEFT);

		hbox.getChildren().add(new Label(transactionID));
		final var button = formatButton("Icons/copy-to-clipboard.png", 30);
		button.setOnAction(actionEvent -> {
			Toolkit.getDefaultToolkit()
					.getSystemClipboard()
					.setContents(new StringSelection(transactionID), null);
			showTooltip(detailsGridPane, button, "Copied!");
		});

		hbox.getChildren().add(button);
		return hbox;
	}
}
