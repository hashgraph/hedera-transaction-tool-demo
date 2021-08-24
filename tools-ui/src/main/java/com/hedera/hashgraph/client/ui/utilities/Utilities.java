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

import com.codahale.passpol.BreachDatabase;
import com.codahale.passpol.PasswordPolicy;
import com.codahale.passpol.Status;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.ui.Controller;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Hbar;
import javafx.animation.PauseTransition;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.Control;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import static com.hedera.hashgraph.client.core.constants.Constants.GREEN_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.MAX_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.MIN_PASSWORD_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.RED_STYLE;

public class Utilities {

	private static final Logger logger = LogManager.getLogger(Utilities.class);
	public static final String RED_BORDER_STYLE = "-fx-border-color: red";
	public static final String HBAR_STRING = "\u0127";

	private Utilities() {
		throw new IllegalStateException("Utility class");
	}

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
		if (accountID.contains("(")) {
			return checkAccount(accountID.substring(accountID.indexOf("(") + 1, accountID.indexOf("-")));
		}
		if (accountID.contains("-")) {
			return checkAccount(accountID.substring(0, accountID.indexOf("-")));
		}

		var tokens = accountID.split("[.]");
		if (tokens.length > 3 || tokens.length == 2) {
			return false;
		}
		for (var t : tokens) {
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
			return formatInt.format(amountHBars).trim() + " " + HBAR_STRING;
		} else {
			return formatInt.format(amountHBars).trim() + "." + formatFrac.format(amountTinyBars) + " " + HBAR_STRING;
		}
	}

	public static String setCurrencyFormat(long amount) {
		var currency = setHBarFormat(amount);
		return currency.replace(" " + HBAR_STRING, "");
	}

	public static String stripHBarFormat(String hBars) {
		if (hBars.equals("")) {
			return "0";
		}

		var tiny = (hBars.contains(".")) ? hBars : hBars.concat(".00000000");
		var amount = Long.parseLong(tiny.replace(HBAR_STRING, "")
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
		var bars = hBars.replace(" ", "").replace(HBAR_STRING, "");
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

		logger.info("removing file or directory : {}", dir.getName());

		try {
			Files.deleteIfExists(dir.toPath());
			return true;
		} catch (IOException e) {
			logger.error("Directory {} cannot be deleted", dir.getAbsolutePath());
			return false;
		}
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
	 * @param appPasswordField
	 * 		the password field where the user enters his new password
	 * @param checkPassword
	 * 		an image that is displayed if the password passes the policy
	 * @param passwordErrorLabel
	 * 		a label where an informative text will be displayed in case the password does not pass the policy
	 * @param reEnterPasswordField
	 * 		a password field where the user needs to re-enter is new password for confirmation
	 */
	public static void checkPasswordPolicy(PasswordField appPasswordField, ImageView checkPassword,
			Label passwordErrorLabel, PasswordField reEnterPasswordField) {
		var policy = new PasswordPolicy(BreachDatabase.anyOf(BreachDatabase.top100K(), BreachDatabase.haveIBeenPwned()),
				MIN_PASSWORD_LENGTH, MAX_PASSWORD_LENGTH);
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

	public static void setupCharacterCount(PasswordField recoverAppPasswordField, Label recoverCharacterCount,
			ImageView recoverCheckPassword, Label recoverPasswordErrorLabel,
			PasswordField recoverReEnterPasswordField) {
		var policy = new PasswordPolicy(BreachDatabase.anyOf(BreachDatabase.top100K(), BreachDatabase.haveIBeenPwned()),
				MIN_PASSWORD_LENGTH, MAX_PASSWORD_LENGTH);
		final var length = recoverAppPasswordField.getText().length();
		recoverCharacterCount.setText(String.valueOf(length));
		recoverCheckPassword.setVisible(false);
		String style = length >= MIN_PASSWORD_LENGTH && length <= MAX_PASSWORD_LENGTH ? GREEN_STYLE : RED_STYLE;
		recoverCharacterCount.setStyle(style);
		if (Status.OK.equals(policy.check(recoverAppPasswordField.getText()))) {
			recoverCheckPassword.setVisible(true);
			recoverPasswordErrorLabel.setVisible(false);
			recoverReEnterPasswordField.setDisable(false);
		} else {
			recoverCheckPassword.setVisible(false);
			recoverPasswordErrorLabel.setVisible(true);
			recoverReEnterPasswordField.setDisable(true);
		}
	}

	/**
	 * Given an account info, returns a list of string keys
	 *
	 * @param info
	 * 		the account info
	 * @param controller
	 * 		the controller
	 * @return a List of strings: If the controller has information about the public key, it uses the nickname,
	 * 		otherwise
	 * 		it shows the complete hex.
	 */
	public static List<String> getKeysFromInfo(AccountInfo info, Controller controller) {
		var flatKey = EncryptionUtils.flatPubKeys(Collections.singletonList(info.key));
		List<String> knownKeys = new ArrayList<>();
		for (var key : flatKey) {
			var keyName = controller.showKeyString(key);
			if (keyName.endsWith(PUB_EXTENSION)) {
				knownKeys.add(keyName);
			}
		}
		return knownKeys;
	}

	/**
	 * Retrieves the password from the PasswordFields and destroys the arrays.
	 *
	 * @param accept
	 * 		button
	 * @param password
	 * 		the char array containing the password
	 * @param passwordField
	 * 		the field that contains the password
	 * @param confirmField
	 * 		the field that confirms the password
	 */
	public static void clearPasswordFields(Button accept, char[] password, PasswordField passwordField,
			PasswordField confirmField) {
		accept.setDisable(true);
		var filler = new char[password.length];
		Arrays.fill(filler, 'x');
		passwordField.clear();
		confirmField.clear();
		passwordField.setText(String.valueOf(filler));
		confirmField.setText(String.valueOf(filler));
		passwordField.setDisable(true);
		confirmField.setDisable(true);
	}
}
