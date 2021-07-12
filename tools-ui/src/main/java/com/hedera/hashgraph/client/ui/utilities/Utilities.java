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

import com.codahale.passpol.PasswordPolicy;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.sdk.Hbar;
import javafx.animation.PauseTransition;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.Control;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

public class Utilities {

	private static final Logger logger = LogManager.getLogger(Utilities.class);
	public static final String RED_BORDER_STYLE = "-fx-border-color: red";

	/***
	 *
	 * Displays an error popup with a given message
	 *
	 * @param info    The error message to display
	 */
	public static void showErrorAlert(String info) {
		logger.error(info);
		var alert = new Alert(Alert.AlertType.ERROR);
		alert.setContentText(info);
		alert.show();
	}

	/***
	 *
	 * Displays an error popup with a given message
	 *
	 * @param info    The error to display
	 */
	public static void showErrorAlert(Exception info) {
		logger.error(info);
		var alert = new Alert(Alert.AlertType.ERROR);
		alert.setContentText(info.getMessage());
		alert.show();
	}

	/**
	 * Check if a string contains a long
	 *
	 * @param strNum
	 * 		a numeric string
	 * @return true if the string is not a long
	 */
	public static boolean isNotLong(String strNum) {
		try {
			Long.parseLong(strNum);
		} catch (NumberFormatException | NullPointerException nfe) {
			return true;
		}
		return false;
	}

	/**
	 * Checks that a string represents an account ID
	 *
	 * @param accountID
	 * 		a string
	 * @return true if the string can be parsed to an account
	 */
	public static boolean checkAccount(String accountID) {
		var tokens = accountID.split("[.]");
		if (tokens.length > 3 || tokens.length == 2) {
			return false;
		}
		for (var t : tokens
		) {
			if (isNotLong(t)) {
				return false;
			}
		}
		return true;
	}

	public static String timestampToString(final Timestamp timestamp) {
		var sec = timestamp.getSeconds();
		var date = new Date(sec * 1000);
		DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		return dateFormat.format(date);
	}

	public static String setHBarFormat(long amount) {
		var symbols = new DecimalFormatSymbols();
		symbols.setGroupingSeparator(' ');
		var formatInt = new DecimalFormat("###,###,###,###,###,###", symbols);
		var formatFrac = new DecimalFormat("00,000,000", symbols);
		var amountHBars = (amount - amount % 100000000) / 100000000d;
		var amountTinyBars = (double) (amount % 100000000);

		if (amount % 100000000 == 0) {
			return formatInt.format(amountHBars).trim() + " \u0127";
		} else {
			return formatInt.format(amountHBars).trim() + "." + formatFrac.format(amountTinyBars) + " \u0127";
		}


	}

	public static String stripHBarFormat(String hBars) {
		if (hBars.equals("")) {
			return "0";
		}

		var tiny = (hBars.contains(".")) ? hBars : hBars.concat(".00000000");
		var amount = Long.parseLong(tiny.replace("\u0127", "")
				.replace(".", "")
				.replace(" ", ""));
		return String.valueOf(amount);
	}

	/**
	 * Convert a string to Hbar
	 *
	 * @param hBars
	 * 		a currency string
	 * @return the number of Hbars represented by the string
	 */
	public static Hbar string2Hbar(String hBars) {
		var bars = hBars.replace(" ", "").replace("\u0127", "");
		return Hbar.from(BigDecimal.valueOf(Double.parseDouble(bars)));
	}

	/**
	 * Deletes a directory and all of its children
	 *
	 * @param dir
	 * 		a directory file
	 * @return true if the directory has been successfully deleted
	 */
	public static boolean deleteDirectory(File dir) {
		if (dir.isDirectory()) {
			var children = dir.listFiles();
			assert children != null;
			for (var child : children) {
				var success = deleteDirectory(child);
				if (!success) {
					return false;
				}
			}
		} // either file or an empty directory

		logger.info("removing file or directory : " + dir.getName());

		return dir.delete();
	}

	/**
	 * Converts the contents of a text field into an amount in tiny bars
	 *
	 * @param t
	 * 		a TextField
	 * @return a long
	 * @throws HederaClientException
	 * 		if the text is not parsable
	 */
	public static long textFieldToTinyBars(TextField t) throws HederaClientException {
		var amountString = t.getText().replace(" ", "");
		if (isNotLong(amountString.replace(".", "").replace(" ", ""))) {
			throw new HederaClientException(String.format("%s cannot be resolved into a number", amountString));
		}

		if (!amountString.contains(".")) {
			return Long.parseLong(amountString.concat("00000000"));
		}

		var decimals = amountString.length() - amountString.lastIndexOf(".") - 1;
		if (decimals > 8) {
			amountString = amountString.substring(0, amountString.length() - (decimals - 8));
			return Long.parseLong(amountString.replace(".", ""));
		}

		for (var i = 0; i < 8 - decimals; i++) {
			amountString = amountString.concat("0");
		}
		return Long.parseLong(amountString.replace(".", ""));
	}

	/**
	 * Shows an informational tooltip when the user presses a button
	 *
	 * @param owner
	 * 		pane that will show the tooltip
	 * @param control
	 * 		the node that will be attached to (typically a button)
	 * @param tooltipText
	 * 		the text that will be displayed
	 */
	public static void showTooltip(Pane owner, Control control, String tooltipText) {
		var customTooltip = new Tooltip();
		var p = control.localToScene(15.0, 15.0);
		customTooltip.setText(tooltipText);
		customTooltip.setStyle("-fx-background-color: white; -fx-text-fill: black;");
		customTooltip.setMaxWidth(300);
		customTooltip.setWrapText(true);
		control.setTooltip(customTooltip);

		customTooltip.setAutoHide(true);

		if (customTooltip.isShowing()) {
			customTooltip.hide();
		} else {
			customTooltip.show(owner, p.getX()
					+ control.getScene().getX() + control.getScene().getWindow().getX(), p.getY()
					+ control.getScene().getY() + control.getScene().getWindow().getY());
		}

		var pt = new PauseTransition(new javafx.util.Duration(5000));
		pt.setOnFinished(e -> customTooltip.hide());
		pt.play();
	}

	/**
	 * Checks that the password policy is followed
	 *
	 * @param policy
	 * 		the password policy
	 * @param appPasswordField
	 * 		the password field where the user enters his new password
	 * @param checkPassword
	 * 		an image that is displayed if the password passes the policy
	 * @param passwordErrorLabel
	 * 		a label where an informative text will be displayed in case the password does not pass the policy
	 * @param reEnterPasswordField
	 * 		a password field where the user needs to re enter is new password for confirmation
	 */
	public static void checkPasswordPolicy(PasswordPolicy policy, PasswordField appPasswordField,
			ImageView checkPassword, Label passwordErrorLabel, PasswordField reEnterPasswordField) {
		final var password1 = appPasswordField.getText();
		switch (policy.check(password1)) {
			case OK:
				checkPassword.setVisible(true);
				passwordErrorLabel.setVisible(false);
				reEnterPasswordField.setDisable(false);
				reEnterPasswordField.requestFocus();
				break;
			case TOO_SHORT:
				passwordErrorLabel.setText("Passwords should be at least 10 characters long");
				passwordErrorLabel.setVisible(true);
				checkPassword.setVisible(false);
				reEnterPasswordField.setDisable(true);
				break;
			case TOO_LONG:
				passwordErrorLabel.setText("Passwords should be at most 1024 characters long");
				passwordErrorLabel.setVisible(true);
				checkPassword.setVisible(false);
				reEnterPasswordField.setDisable(true);
				break;
			case BREACHED:
				passwordErrorLabel.setText(
						"The password selected has been breached. Please select a more unique password");
				passwordErrorLabel.setVisible(true);
				checkPassword.setVisible(false);
				reEnterPasswordField.setDisable(true);
				break;
		}
	}

	/**
	 * Creates an "Encrypting..." progress popup
	 *
	 * @param bar
	 * 		a progress bar that is linked to the task
	 * @param cancelButton
	 * 		a button that cancels the task
	 * @return a popup stage.
	 */
	public static Stage setupProgressPopup(ProgressBar bar, Button cancelButton) {
		VBox layout = new VBox();
		layout.setAlignment(Pos.CENTER);
		layout.setSpacing(10);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setMaxWidth(400);

		Stage window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle("Encrypting");

		window.sizeToScene();
		window.setWidth(450);


		Label label1 = new Label();
		label1.setText("Encrypting tasks");
		label1.setStyle("-fx-font-size: 20");

		Label label2 = new Label("The recovery phrase is being encrypted. This might take a few minutes.");
		label2.setWrapText(true);
		label2.setStyle("-fx-font-size: 16");

		HBox box = new HBox();
		box.setPrefWidth(Region.USE_COMPUTED_SIZE);
		box.setPrefHeight(Region.USE_COMPUTED_SIZE);
		box.setAlignment(Pos.CENTER);
		box.getChildren().addAll(label2);

		cancelButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
						"-fx-border-radius: 10; -fx-background-radius: 10;");
		cancelButton.setMinWidth(200);

		bar.setPrefWidth(375);

		layout.getChildren().addAll(label1, box, bar, cancelButton);

		Scene scene = new Scene(layout);

		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.show();

		return window;
	}
}
