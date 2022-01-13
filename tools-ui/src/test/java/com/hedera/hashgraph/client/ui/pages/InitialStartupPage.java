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
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCEPT_APP_PASSWORD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.APP_PASSWORD_FIELD_1;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.APP_PASSWORD_FIELD_2;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.EMAIL_TEXT_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FILES_PANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.GENERATE_KEYS_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.GENERATE_KEY_PAIR_BUTTON_BAR;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.HIDDEN_STARTUP_INFO_TEXTFIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEYS_PANE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.KEY_NICKNAME;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.LOAD_ADDRESS_BOOK_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.NETWORK_BUTTON_BAR;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASSPHRASE_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASSWORD_FIELD_ONE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASSWORD_FIELD_TWO;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.PASTE_FROM_CLIPBOARD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.STORAGE_BUTTON_BAR;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.USERNAME_BUTTON_BAR;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findButtonInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;


public class InitialStartupPage {

	private final TestBase driver;

	public InitialStartupPage(final TestBase driver) {
		this.driver = driver;
	}


	public InitialStartupPage skipUserName() {
		final Button b = findButtonInBar(USERNAME_BUTTON_BAR, "SKIP");
		driver.clickOn(b);
		return this;
	}

	public InitialStartupPage enterStringUsername(final String test) {
		final TextField userName = driver.find(EMAIL_TEXT_FIELD);
		driver.clickOn(userName);
		driver.write(test);
		return this;
	}

	public InitialStartupPage enterOneDriveFolder(final String s) {

		final TextField pathTextField = driver.find(JavaFXIDs.ONEDRIVE_PATH_TEXT_FIELD);
		pathTextField.clear();
		driver.clickOn(pathTextField);
		driver.write((System.getProperty("user.dir") + File.separator + s));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public InitialStartupPage acceptOneDrive() {
		final Button accept = driver.find("#confirmAddFolderButton");
		driver.clickOn(accept);
		return this;
	}

	public InitialStartupPage confirmUsername() {
		final Button b = findButtonInBar(USERNAME_BUTTON_BAR, "CONFIRM");
		driver.clickOn(b);
		return this;
	}

	public Button findButtonInBar(final String buttonBarName, final String buttonMessage) {
		final ButtonBar buttonBar = driver.find(buttonBarName);
		final ObservableList<Node> buttons = buttonBar.getButtons();
		for (final Node b :
				buttons) {
			if (((Button) b).getText().equals(buttonMessage)) {
				return (Button) b;
			}
		}
		return null;
	}

	public InitialStartupPage browseFolders() {
		final Button browse = findButtonInBar(STORAGE_BUTTON_BAR, "CHANGE");
		driver.clickOn(browse, MouseButton.PRIMARY);
		return this;
	}

	public InitialStartupPage acceptAddressBook() throws Exception {
		if (driver.find(FILES_PANE).isVisible()) {
			driver.clickOn(LOAD_ADDRESS_BOOK_BUTTON, MouseButton.PRIMARY);
		} else {
			throw new Exception("Address Button Bar not found");
		}
		return this;
	}

	public InitialStartupPage openBrowseFile() {
		final Button browse = findButtonInBar(NETWORK_BUTTON_BAR, "SELECT FILE");
		driver.clickOn(browse, MouseButton.PRIMARY);
		return this;
	}

	public InitialStartupPage enterNetworkAddress(final String file) throws InterruptedException {
		TestUtil.applyPath(file);
		return this;
	}

	public InitialStartupPage declineKey() throws Exception {
		if (driver.find(KEYS_PANE).isVisible() && driver.find(PASSPHRASE_BOX).isVisible()) {
			driver.clickOn(findButtonInBar(GENERATE_KEY_PAIR_BUTTON_BAR, "SKIP"), MouseButton.PRIMARY);
		} else {
			throw new Exception("No Key Pane found");
		}

		return this;
	}

	public InitialStartupPage enterFirstPassword(final String password) {
		driver.clickOn(PASSWORD_FIELD_ONE);
		driver.write(password);
		return this;
	}

	public InitialStartupPage enterSecondPassword(final String password) {
		driver.clickOn(PASSWORD_FIELD_TWO);
		driver.write(password);
		return this;
	}

	public InitialStartupPage clearSecondPassword() {
		final TextField password2 = driver.find(PASSWORD_FIELD_TWO);
		password2.clear();
		return this;
	}

	public InitialStartupPage generateKeys() {
		driver.clickOn(findButtonInBar(GENERATE_KEY_PAIR_BUTTON_BAR, "GENERATE KEYS"));
		return this;
	}

	public InitialStartupPage loadInfoFromHiddenTextField(final String location) {

		final TextField path = driver.find(HIDDEN_STARTUP_INFO_TEXTFIELD);
		path.setText(location);
		driver.clickOn(path).press(KeyCode.ENTER).release(KeyCode.ENTER);

		return this;
	}

	public void finishSetup() {
		driver.clickOn("CONTINUE");
	}

	public InitialStartupPage enterKeyNickname(final String testKey) {
		final TextField nickname = driver.find(KEY_NICKNAME);
		nickname.setText(testKey);
		driver.clickOn(nickname).press(KeyCode.ENTER).release(KeyCode.ENTER);

		return this;
	}

	public InitialStartupPage enterPassword(final String s) {
		final PasswordField passwordField = driver.find(APP_PASSWORD_FIELD_1);
		passwordField.clear();
		driver.clickOn(APP_PASSWORD_FIELD_1);
		driver.write(s);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public InitialStartupPage reEnterPassword(final String s) {
		final PasswordField passwordField = driver.find(APP_PASSWORD_FIELD_2);
		passwordField.clear();
		driver.clickOn(APP_PASSWORD_FIELD_2);
		driver.write(s);
		return this;
	}

	public InitialStartupPage acceptPassword() {
		final Button passwordButton = driver.find(ACCEPT_APP_PASSWORD);
		driver.clickOn(passwordButton);
		return this;
	}

	public InitialStartupPage clickOnGenerateMnemonic() {
		final Button generate = driver.find(GENERATE_KEYS_BUTTON);
		driver.clickOn(generate);
		return this;
	}

	public InitialStartupPage clickMnemonicPopupButton(final String legend) {
		final Set<Node> nodes = driver.findAll(legend);
		Button ok = new Button();
		for (final Node n :
				nodes) {
			if (n instanceof Button && n.isVisible()) {
				ok = (Button) n;
			}
		}
		if (ok.getText().contains(legend)) {
			driver.clickOn(ok);
		}

		return this;
	}

	public void clickOnFinishSetup() {
		driver.clickOn("FINISH");
	}

	public InitialStartupPage enterPasswordInPopup(final String password) {
		try {
			final PasswordField passwordField = (PasswordField) TestUtil.getPopupNodes().get(1);
			passwordField.setText(password);
			driver.clickOn(passwordField).press(KeyCode.ENTER).release(KeyCode.ENTER);
		} catch (final Exception e) {
			e.printStackTrace();
		}
		return this;
	}

	public Label getLabelFromPopup() {
		final ObservableList<Node> popupNodes = ((VBox) TestUtil.getPopupNodes().get(0)).getChildren();
		for (final Node n :
				popupNodes) {
			if (n instanceof Label) {
				return (Label) n;
			}
		}
		return new Label();
	}

	public InitialStartupPage closePopup() {
		Button button = null;
		final ObservableList<Node> popupNodes = ((VBox) TestUtil.getPopupNodes().get(0)).getChildren();
		for (final Node n :
				popupNodes) {
			if (n instanceof Button) {
				button = (Button) n;
			}
		}
		if (button != null) {
			driver.clickOn(button);
		}
		return this;
	}

	public InitialStartupPage pressPaste() {
		final Button paste = driver.find(PASTE_FROM_CLIPBOARD);
		driver.clickOn(paste);
		return this;
	}

	public InitialStartupPage enterNewPasswordInPopup(final String password) {
		final var passwordFields = getPopupPasswordFields();
		passwordFields.get(0).clear();
		typePassword(password, passwordFields.get(0));
		typePassword(password, passwordFields.get(1));
		final var continueButton = findButtonInPopup(Objects.requireNonNull(getPopupNodes()), "CONTINUE");
		driver.clickOn(continueButton);
		return this;
	}

	public void typePassword(final String s, final PasswordField field) {
		field.setText(s);
		driver.clickOn(field);
		driver.type(KeyCode.SHIFT);
	}

	private List<PasswordField> getPopupPasswordFields() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		return getAllPasswordFields(popupNodes);
	}

	private List<PasswordField> getAllPasswordFields(final ObservableList<Node> nodes) {
		final List<PasswordField> passwordFields = new ArrayList<>();
		for (final Node node : nodes) {
			if (node instanceof HBox) {
				passwordFields.addAll(getAllPasswordFields(((HBox) node).getChildren()));
			}
			if (node instanceof VBox) {
				passwordFields.addAll(getAllPasswordFields(((VBox) node).getChildren()));
			}
			if (node instanceof PasswordField) {
				passwordFields.add((PasswordField) node);
			}
		}
		return passwordFields;
	}

}
