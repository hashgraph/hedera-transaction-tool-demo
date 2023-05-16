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


import com.google.common.annotations.VisibleForTesting;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.ui.JavaFXIDs;
import com.hedera.hashgraph.client.ui.TestBase;
import com.hedera.hashgraph.client.ui.utilities.AutoCompleteNickname;
import com.hedera.hashgraph.sdk.FreezeType;
import javafx.scene.control.Accordion;
import javafx.scene.control.Button;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuButton;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import java.util.TimeZone;

import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_COMMENTS_AREA;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_DATE_PICKER;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_EDIT_KEY;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_FEE_PAYER_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_FILE_UPDATE_CONTENTS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_FILE_UPDATE_FILE_ID;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_HOURS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_INITIAL_BALANCE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_MAIN_CHOICE_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_MEMO_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_MINUTES;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_NANOS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_NODE_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_SECONDS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_SYSTEM_HOURS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_SYSTEM_MINUTES;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_SYSTEM_SECONDS;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSACTION_FEE;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_ACCEPT_FROM_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_ACCEPT_TO_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_FROM_ACCOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_FROM_AMOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_TO_ACCOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_TRANSFER_TO_AMOUNT;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.CREATE_UPDATE_ACCOUNT_ID;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ENTITY_ID_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FREEZE_DATE_PICKER;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FREEZE_FILE_HASH_TEXT_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FREEZE_FILE_IDTEXT_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FREEZE_HOUR_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FREEZE_MINUTE_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.FREEZE_SECONDS_FIELD;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SET_NOW_BUTTON;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SYSTEM_TIMEZONE_HBOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SYSTEM_TRANSACTION_ACTION_CHOICE_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SYSTEM_TRANSACTION_EXPIRATION_DATEPICKER;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.SYSTEM_TRANSACTION_TYPE_CHOICE_BOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.TIME_ZONE_HBOX;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.UPDATE_EDIT_KEY;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.TitledPaneEnum.ACCOUNTS;
import static com.hedera.hashgraph.client.ui.pages.CreatePanePage.TitledPaneEnum.PUBLIC_KEYS;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findButtonInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.findTextFieldsInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static java.lang.Thread.sleep;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;

@SuppressWarnings({ "UnusedReturnValue", "BusyWait" })
public class CreatePanePage {

	private final TestBase driver;

	private static final Logger logger = LogManager.getLogger(CreatePanePage.class);

	public CreatePanePage(final TestBase driver) {
		this.driver = driver;
	}

	public CreatePanePage selectTransaction(final String txType) {
		driver.clickOn(CREATE_MAIN_CHOICE_BOX);
		driver.clickOn(txType);
		return this;
	}

	public CreatePanePage loadTransaction(final String path) {
		final var n = driver.find("#loadTransactionTextField");
		assert n instanceof TextField;
		((TextField) n).setText(path);
		driver.clickOn(n);
		driver.press(KeyCode.ENTER);
		driver.release(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setComment(final String text) {
		driver.clickOn(CREATE_COMMENTS_AREA);
		driver.write(text);
		return this;
	}

	public CreatePanePage setDate(final String date) {
		driver.doubleClickOn(CREATE_DATE_PICKER);
		driver.clickOn(CREATE_DATE_PICKER);
		driver.write(date);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setHours(final int hours) {
		driver.doubleClickOn(CREATE_HOURS);
		driver.write(String.valueOf(hours));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setMinutes(final int minutes) {
		driver.doubleClickOn(CREATE_MINUTES);
		driver.write(String.valueOf(minutes));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setSeconds(final int seconds) {
		final var node = driver.find(CREATE_SECONDS);
		assert node instanceof TextField;
		driver.clickOn(node);
		((TextField) node).selectAll();
		driver.write(String.valueOf(seconds));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setMemo(final String memo) {
		driver.clickOn(CREATE_MEMO_FIELD);
		driver.write(memo);
		return this;
	}

	public CreatePanePage setFeePayerAccount(final long accountID) {
		driver.clickOn(CREATE_FEE_PAYER_FIELD);
		final var node = driver.find(CREATE_FEE_PAYER_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(String.valueOf(accountID));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setFeePayerAccount(final String accountID) {
		driver.clickOn(CREATE_FEE_PAYER_FIELD);
		final var node = driver.find(CREATE_FEE_PAYER_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(accountID);

		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setNodeAccount(final long accountID) {
		driver.clickOn(CREATE_NODE_FIELD);
		final var node = driver.find(CREATE_NODE_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(String.valueOf(accountID));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setNodeAccount(final String accountID) {
		driver.clickOn(CREATE_NODE_FIELD);
		final var node = driver.find(CREATE_NODE_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(accountID);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setInitialBalance(final long amount) {
		driver.ensureVisible(driver.find(CREATE_INITIAL_BALANCE));
		driver.doubleClickOn(CREATE_INITIAL_BALANCE);
		driver.write(String.valueOf(amount));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setCreateKey() {
		driver.ensureVisible(driver.find(CREATE_EDIT_KEY));
		driver.clickOn(CREATE_EDIT_KEY);
		return this;
	}

	public CreatePanePage setUpdateKey(final String account) {
		driver.ensureVisible(driver.find(UPDATE_EDIT_KEY));
		driver.clickOn(UPDATE_EDIT_KEY);
		clearTree();
		return doubleClickOnAccountKey(account);
	}

	private void clearTree() {
		final var node = driver.find("Threshold key (2 of 4)");
		if (node != null) {
			driver.clickOn(node);
			clickOnDeletePublicKeyButton();
		}
	}

	public CreatePanePage doubleClickOnAccountKey(final String account) {

		final var titledPane3 = getTitledPane(ACCOUNTS);
		final var inner3 = titledPane3.getContent();
		assert inner3 instanceof VBox;
		final var inner4 = ((VBox) inner3).getChildren();
		assert inner4 != null;
		assert inner4.size() == 1;
		assert inner4.get(0) instanceof ListView;
		final var accountList = (ListView<String>) inner4.get(0);
		for (final var item : accountList.getItems()) {
			if (item.contains(account)) {
				driver.doubleClickOn(item);
			}
		}
		return this;
	}

	public CreatePanePage clickOnAccountKey(final String account) {
		final var titledPane3 = getTitledPane(ACCOUNTS);
		final var inner3 = titledPane3.getContent();
		assert inner3 instanceof VBox;
		final var inner4 = ((VBox) inner3).getChildren();
		assert inner4 != null;
		assert inner4.size() == 1;
		assert inner4.get(0) instanceof ListView;
		final var accountList = (ListView<String>) inner4.get(0);
		for (final var item : accountList.getItems()) {
			if (item.contains(account)) {
				driver.clickOn(item);
			}
		}
		return this;
	}

	public TitledPane getTitledPane(final TitledPaneEnum paneEnum) {
		var popupNodes = getPopupNodes();
		while (popupNodes == null) popupNodes = getPopupNodes();
		assert popupNodes != null;
		assert popupNodes.size() == 2;
		assert popupNodes.get(1) instanceof VBox;
		final var vBox0 = (VBox) popupNodes.get(1);
		final var inner0 = vBox0.getChildren();
		assert inner0 != null;
		assert inner0.size() == 2;
		assert inner0.get(0) instanceof HBox;

		final var hBox1 = (HBox) inner0.get(0);
		final var inner1 = hBox1.getChildren();
		assert inner1 != null;
		assert inner1.get(0) instanceof Accordion;

		final var accordion2 = (Accordion) inner1.get(0);
		final var inner2 = accordion2.getPanes();
		assert inner2 != null;

		driver.clickOn(inner2.get(paneEnum.value));
		return inner2.get(paneEnum.value);
	}

	public TreeView<String> getTree() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var vBox0 = (VBox) popupNodes.get(1);
		final var inner0 = vBox0.getChildren();
		assert inner0 != null;

		final var hBox1 = (HBox) inner0.get(0);
		final var inner1 = hBox1.getChildren();
		assert inner1 != null;

		final var titledPane = (TitledPane) inner1.get(2);
		final var vBox = (VBox) titledPane.getContent();
		return (TreeView<String>) vBox.getChildren().get(0);
	}


	public Button getDeleteButton() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var vBox0 = (VBox) popupNodes.get(1);
		final var inner0 = vBox0.getChildren();
		final var vBox1 = (VBox) ((HBox) inner0.get(0)).getChildren().get(1);
		return (Button) vBox1.getChildren().get(1);
	}

	public Button getVisibleAddButton() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var vBox0 = (VBox) popupNodes.get(1);
		final var inner0 = vBox0.getChildren();
		final var vBox1 = (VBox) ((HBox) inner0.get(0)).getChildren().get(1);
		final var buttons = ((VBox) vBox1.getChildren().get(0)).getChildren();
		for (final var button : buttons) {
			if (button instanceof Button && button.isVisible()) {
				return (Button) button;
			}
		}
		return null;
	}

	public CreatePanePage createAndExport(final String folder) {
		logger.info("Exporting transaction to {}", folder);
		final GridPane gridPane = driver.find(JavaFXIDs.STORE_OR_SUBMIT_GRID_PANE);
		final var hBox = gridPane.getChildren().stream().filter(child -> child instanceof HBox).findFirst().map(
				child -> (HBox) child).orElse(new HBox());
		final var children = hBox.getChildren();

		try {
			for (final var n : children) {
				if (n instanceof MenuButton) {
					driver.clickOn(n);
					final var nodes = driver.findAll(folder);
					for (final var node : nodes) {
						if (node instanceof Label && !(node.getParent() instanceof GridPane)) {
							driver.clickOn(node);
						}
					}
				}
			}
		} catch (final Exception e) {
			logger.error(e);
		}

		return this;
	}

	public CreatePanePage addDebit(final long accountNum, final double amount) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_FROM_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_FROM_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_FROM_ACCOUNT);
		driver.write(Long.toString(accountNum));
		((TextField) driver.find(CREATE_TRANSFER_FROM_AMOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_FROM_AMOUNT);
		driver.write(String.format("%12.9f", amount));

		driver.clickOn(CREATE_TRANSFER_ACCEPT_FROM_BUTTON);

		return this;
	}

	public CreatePanePage setFromAccountTransfer(final long accountNum) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_FROM_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_FROM_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_FROM_ACCOUNT);
		driver.write(Long.toString(accountNum));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setFromAccountTransfer(final String accountNum) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_FROM_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_FROM_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_FROM_ACCOUNT);
		driver.write(accountNum);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setToAccountTransfer(final long accountNum) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_TO_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_TO_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_TO_ACCOUNT);
		driver.write(Long.toString(accountNum));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setToAccountTransfer(final String accountNum) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_TO_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_TO_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_TO_ACCOUNT);
		driver.write(accountNum);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage addCredit(final long accountNum, final double amount) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_TO_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_TO_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_TO_ACCOUNT);
		driver.write(Long.toString(accountNum));
		((TextField) driver.find(CREATE_TRANSFER_TO_AMOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_TO_AMOUNT);
		driver.write(String.format("%12.9f", amount));

		driver.clickOn(CREATE_TRANSFER_ACCEPT_TO_BUTTON);

		return this;
	}

	public CreatePanePage setUpdateAccount(final long accountNumber) {
		driver.doubleClickOn(CREATE_UPDATE_ACCOUNT_ID);
		driver.clickOn(CREATE_UPDATE_ACCOUNT_ID);
		final var node = driver.find(CREATE_UPDATE_ACCOUNT_ID);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.write(Long.toString(accountNumber));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setUpdateAccount(final String accountNumber) {
		driver.ensureVisible(driver.find(CREATE_UPDATE_ACCOUNT_ID));
		driver.doubleClickOn(CREATE_UPDATE_ACCOUNT_ID);
		driver.clickOn(CREATE_UPDATE_ACCOUNT_ID);
		final var node = driver.find(CREATE_UPDATE_ACCOUNT_ID);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.write(accountNumber);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setUpdateFileID(final String accountNumber) {
		driver.ensureVisible(driver.find(CREATE_FILE_UPDATE_FILE_ID));
		driver.doubleClickOn(CREATE_FILE_UPDATE_FILE_ID);
		driver.clickOn(CREATE_FILE_UPDATE_FILE_ID);
		final var node = driver.find(CREATE_FILE_UPDATE_FILE_ID);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.write(accountNumber);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setUpdateFileID(final long accountNumber) {
		driver.ensureVisible(driver.find(CREATE_FILE_UPDATE_FILE_ID));
		driver.doubleClickOn(CREATE_FILE_UPDATE_FILE_ID);
		driver.clickOn(CREATE_FILE_UPDATE_FILE_ID);
		final var node = driver.find(CREATE_FILE_UPDATE_FILE_ID);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.write(Long.toString(accountNumber));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setOperation(final OperationType type) {
		final ChoiceBox<String> choiceBox = driver.find(SYSTEM_TRANSACTION_ACTION_CHOICE_BOX);
		driver.clickOn(choiceBox);
		switch (type) {
			case delete:
				driver.clickOn("Remove Content");
				break;
			case undelete:
				driver.clickOn("Restore Content");
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + type);
		}

		return this;
	}

	public CreatePanePage setEntity(final SystemEntity entity) {
		final ChoiceBox<String> choiceBox = driver.find(SYSTEM_TRANSACTION_TYPE_CHOICE_BOX);
		driver.clickOn(choiceBox);
		switch (entity) {

			case file:
				driver.clickOn("File");
				break;
			case contract:
				driver.clickOn("Smart Contract");
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + entity);
		}
		return this;
	}

	public CreatePanePage setStartDate(final LocalDateTime date) {
		final var d = Date.from(date.atZone(ZoneId.systemDefault()).toInstant());
		return getCreatePanePage(d, CREATE_DATE_PICKER, CREATE_HOURS, CREATE_MINUTES, CREATE_SECONDS);
	}

	public CreatePanePage setExpirationDate(final LocalDateTime date) {
		final var d = Date.from(date.atZone(ZoneId.systemDefault()).toInstant());
		return getCreatePanePage(d, SYSTEM_TRANSACTION_EXPIRATION_DATEPICKER, CREATE_SYSTEM_HOURS,
				CREATE_SYSTEM_MINUTES, CREATE_SYSTEM_SECONDS);
	}

	public CreatePanePage setFreezeStartDate(final LocalDateTime date) {
		final var startTimeDate = Date.from(date.toInstant(OffsetDateTime.now().getOffset()));
		return getCreatePanePage(startTimeDate, FREEZE_DATE_PICKER, FREEZE_HOUR_FIELD, FREEZE_MINUTE_FIELD,
				FREEZE_SECONDS_FIELD);
	}

	private CreatePanePage getCreatePanePage(final Date date, final String datePicker, final String hours,
			final String minutes,
			final String seconds) {

		final var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getDefault());
		final var localDateTime = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();


		driver.ensureVisible(driver.find(datePicker));
		driver.doubleClickOn(datePicker);
		driver.doubleClickOn(datePicker);
		driver.write(datePickerFormat.format(date));
		driver.type(KeyCode.ENTER);

		//set Hours
		driver.doubleClickOn(hours);
		driver.write(String.valueOf(localDateTime.getHour()));
		driver.type(KeyCode.TAB);


		//set Minutes
		driver.doubleClickOn(minutes);
		driver.write(String.valueOf(localDateTime.getMinute()));
		driver.type(KeyCode.TAB);


		//set Seconds
		final var node = driver.find(seconds);
		assert node instanceof TextField;
		driver.clickOn(node);
		((TextField) node).selectAll();
		driver.write(String.valueOf(localDateTime.getSecond()));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setEntityID(final long id) {
		driver.ensureVisible(driver.find(ENTITY_ID_FIELD));
		driver.clickOn(ENTITY_ID_FIELD);
		final var node = driver.find(ENTITY_ID_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(String.valueOf(id));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setEntityID(final String id) {
		driver.ensureVisible(driver.find(ENTITY_ID_FIELD));
		driver.doubleClickOn(ENTITY_ID_FIELD);
		driver.write(id);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setContents(final String contentsPath) {
		driver.ensureVisible(driver.find(CREATE_FILE_UPDATE_CONTENTS));
		driver.clickOn(CREATE_FILE_UPDATE_CONTENTS);
		driver.write(contentsPath);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage clickOnPopupButton(final String legend) throws InterruptedException {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		final var button = findButtonInPopup(popupNodes, legend);
		if (button != null) {
			while (button.isDisable()) {
				sleep(100);
			}
			driver.clickOn(button);
			return this;
		}
		logger.error("Button not found");
		return this;
	}

	public CreatePanePage clickOnNowButton() {
		final TextField secs = driver.find(CREATE_SECONDS);
		while (true) {
			final var seconds = Integer.parseInt(secs.getText());
			if (!(seconds > 58 || seconds < 1)) {
				break;
			}
			driver.clickOn(SET_NOW_BUTTON);
		}
		return this;
	}

	public String getStartTimezone() {
		final HBox timeBox = driver.find(TIME_ZONE_HBOX);
		final var children = timeBox.getChildren();
		for (final var child : children) {
			if (child instanceof AutoCompleteNickname) {
				return ((AutoCompleteNickname) child).getText();
			}
		}
		return "";
	}

	public String getSystemTimezone() {
		final HBox timeBox = driver.find(SYSTEM_TIMEZONE_HBOX);
		final var children = timeBox.getChildren();
		for (final var child : children) {
			if (child instanceof AutoCompleteNickname) {
				return ((AutoCompleteNickname) child).getText();
			}
		}
		return "";
	}

	public CreatePanePage setStartTimezone(final String timezone) {
		final HBox timeBox = driver.find(TIME_ZONE_HBOX);
		final var children = timeBox.getChildren();
		driver.ensureVisible(timeBox);
		for (final var child : children) {
			if (child instanceof AutoCompleteNickname) {
				((AutoCompleteNickname) child).clear();
				driver.clickOn(child);
				driver.write(timezone);
				driver.type(KeyCode.ENTER);
				driver.type(KeyCode.ESCAPE);
			}
		}
		return this;
	}

	public CreatePanePage setSystemTimezone(final String timezone) {
		final HBox timeBox = driver.find(SYSTEM_TIMEZONE_HBOX);
		driver.ensureVisible(timeBox);
		final var children = timeBox.getChildren();
		for (final var child : children) {
			if (child instanceof AutoCompleteNickname) {
				((AutoCompleteNickname) child).clear();
				driver.clickOn(child);
				driver.write(timezone);
				driver.type(KeyCode.ENTER);
				driver.type(KeyCode.ESCAPE);
			}
		}
		return this;
	}

	public CreatePanePage saveKey() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		assert popupNodes.size() == 2;
		assert popupNodes.get(1) instanceof VBox;
		final var vBox0 = (VBox) popupNodes.get(1);
		final var inner0 = vBox0.getChildren();
		assert inner0 != null;
		assert inner0.size() == 2;
		assert inner0.get(0) instanceof HBox;

		final var hBox1 = (HBox) inner0.get(1);
		final var inner1 = hBox1.getChildren();
		assert inner1 != null;
		for (final var node : inner1) {
			if (node instanceof Button && ((Button) node).getText().equals("SAVE")) {
				driver.clickOn(node);
				break;
			}
		}
		return this;
	}

	public CreatePanePage clickOnKeyDesignerCancel() {
		final var popupNodes = getPopupNodes();
		assert popupNodes != null;
		assert popupNodes.size() == 2;
		assert popupNodes.get(1) instanceof VBox;
		final var vBox0 = (VBox) popupNodes.get(1);
		final var inner0 = vBox0.getChildren();
		assert inner0 != null;
		assert inner0.size() == 2;
		assert inner0.get(0) instanceof HBox;

		final var hBox1 = (HBox) inner0.get(1);
		final var inner1 = hBox1.getChildren();
		assert inner1 != null;
		for (final var node : inner1) {
			if (node instanceof Button && ((Button) node).getText().equals("CANCEL")) {
				driver.clickOn(node);
				break;
			}
		}
		return this;

	}

	public CreatePanePage setTransactionFee(final double transactionFee) {
		final var n = driver.find(CREATE_TRANSACTION_FEE);
		assert n instanceof TextField;
		((TextField) n).setText(Double.toString(transactionFee));
		driver.clickOn(n);
		driver.press(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setNanos(final String nanosString) {
		final var n = driver.find(CREATE_NANOS);
		assert n instanceof TextField;
		((TextField) n).setText(nanosString);
		driver.clickOn(n);
		driver.press(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setFreezeType(final FreezeType freezeOnly) {
		final var type = freezeOnly.equals(FreezeType.FREEZE_UPGRADE) ?
				"Freeze and upgrade" :
				freezeOnly.toString()
						.replace("_", " ")
						.toLowerCase(Locale.ROOT);

		driver.clickOn("#freezeTypeChoiceBox");
		driver.clickOn(StringUtils.capitalize(type));
		return this;
	}

	public CreatePanePage setFreezeFileId(final int i) {
		driver.ensureVisible(driver.find(FREEZE_FILE_IDTEXT_FIELD));
		driver.clickOn(FREEZE_FILE_IDTEXT_FIELD);
		driver.write(String.valueOf(i));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setFreezeHash(final String hash) {
		driver.ensureVisible(driver.find(FREEZE_FILE_HASH_TEXT_FIELD));
		driver.doubleClickOn(FREEZE_FILE_HASH_TEXT_FIELD);
		driver.write(hash);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setChunkSize(final int chunk) {
		driver.ensureVisible(driver.find("#chunkSizeTextField"));
		driver.doubleClickOn("#chunkSizeTextField");
		driver.write(String.valueOf(chunk));
		return this;
	}

	public CreatePanePage setInterval(final int interval) {
		driver.ensureVisible(driver.find("#intervalTextField"));
		driver.doubleClickOn("#intervalTextField");
		driver.write(String.valueOf(interval));
		return this;
	}

	public CreatePanePage setAutoRenew(final long seconds) {
		driver.ensureVisible(driver.find("#updateAutoRenew"));
		driver.doubleClickOn("#updateAutoRenew");
		driver.write(String.valueOf(seconds));
		return this;
	}

	public CreatePanePage clickOnPublicKey(final String string) {
		getTitledPane(PUBLIC_KEYS);
		final var nodes = driver.findAll(string);
		for (final var node : nodes) {
			if (node instanceof ListCell) {
				driver.clickOn(node);
			}
		}
		return this;
	}

	public CreatePanePage clickOnAddPublicKeyButton() {
		final var publicKeyButtons = getVisibleAddButton();
		driver.clickOn(publicKeyButtons);
		return this;
	}

	public CreatePanePage clickOnDeletePublicKeyButton() {
		final var deleteButton = getDeleteButton();
		driver.clickOn(deleteButton);
		return this;
	}

	public CreatePanePage clickOnAddAccountButton() {
		final var addButton = getVisibleAddButton();
		if (addButton != null) {
			driver.clickOn(addButton);
		}
		return this;
	}


	public int getKeyTreeSize() {
		expandTree();
		return getTree().getExpandedItemCount();
	}

	public void expandTree() {
		expandTree(getTree().getRoot());
	}

	private void expandTree(final TreeItem<String> treeItem) {
		if (!treeItem.isExpanded()) {
			treeItem.setExpanded(true);
		}
		for (final var child : treeItem.getChildren()) {
			if (child.getChildren().size() > 0) {
				expandTree(child);
			}
		}
	}

	public CreatePanePage selectKeyInTree(final String key) {
		expandTree();
		final var tree = getTree();
		final var item = findFromRoot(tree.getRoot(), key);
		final var model = tree.getSelectionModel();
		if (item != null) {
			final var row = tree.getRow(item);
			model.select(row);
		}
		return this;
	}

	private TreeItem<String> findFromRoot(final TreeItem<String> treeItem, final String key) {
		if (key.equals(treeItem.getValue())) {
			return treeItem;
		}
		if (!treeItem.getChildren().isEmpty()) {
			for (final var child : treeItem.getChildren()) {
				final var childResult = findFromRoot(child, key);
				if (childResult != null) {
					return childResult;
				}
			}
		}
		return null;
	}

	public CreatePanePage setThreshold(final int threshold) {
		final var nodes = getPopupNodes();
		final var fields = findTextFieldsInPopup(nodes);
		assertEquals(1, fields.size());
		fields.get(0).setText(Integer.toString(threshold));
		final var button = findButtonInPopup(nodes, "ACCEPT");
		if (button != null) {
			driver.clickOn(button);
		}
		return this;
	}

	public CreatePanePage deleteKeyFromTree(final String name) {
		expandTree();
		selectKeyInTree(name);
		clickOnDeletePublicKeyButton();
		return this;
	}

	@VisibleForTesting
	public CreatePanePage closePopup(final String legend) {
		final var popupNodes = getPopupNodes();
		final var close = findButtonInPopup(popupNodes, legend);
		driver.clickOn(close);
		return this;
	}

	public CreatePanePage setNewAccountMemo(final String memoString) {
		final var n = driver.find("#updateAccountMemoNew");
		driver.ensureVisible(n);
		assertTrue(n instanceof TextField);
		driver.doubleClickOn(n);
		driver.clickOn(n);
		driver.write(memoString);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setNewMaxTokenAss(final int associations) {
		final var n = driver.find("#updateMaxTokensNew");
		driver.ensureVisible(n);
		assertTrue(n instanceof TextField);
		driver.doubleClickOn(n);
		driver.write(String.valueOf(associations));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setAccountMemo(final String text) {
		final var n = driver.find("#createAccountMemo");
		driver.ensureVisible(n);
		assertTrue(n instanceof TextField);
		driver.doubleClickOn(n);
		driver.clickOn(n);
		driver.write(text);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setTokenAssociations(final int i) {
		final var n = driver.find("#createMaxTokenAssociations");
		driver.ensureVisible(n);
		assertTrue(n instanceof TextField);
		driver.doubleClickOn(n);
		driver.write(String.valueOf(i));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage submit() {
		driver.clickOn(JavaFXIDs.CREATE_SIGN_AND_SUBMIT_BUTTON);
		return this;
	}

	public CreatePanePage signWithPassword(final String testPassword) throws HederaClientException {
		final var passwords = TestUtil.findPasswordInPopup(Objects.requireNonNull(TestUtil.getPopupNodes()));
		if (passwords == null) {
			throw new HederaClientException("Unexpected popup");
		}
		driver.clickOn(passwords);
		driver.write(testPassword);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public enum OperationType {
		delete, undelete
	}

	public enum SystemEntity {
		file, contract
	}

	public enum TitledPaneEnum {
		ACCOUNTS(1), PUBLIC_KEYS(0);
		private final int value;

		TitledPaneEnum(final int i) {
			value = i;
		}
	}

}
