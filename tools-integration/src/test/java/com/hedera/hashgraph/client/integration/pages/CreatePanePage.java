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

package com.hedera.hashgraph.client.integration.pages;


import com.hedera.hashgraph.client.integration.TestBase;
import com.hedera.hashgraph.client.ui.utilities.AutoCompleteNickname;
import com.hedera.hashgraph.sdk.FreezeType;
import javafx.scene.Node;
import javafx.scene.control.Accordion;
import javafx.scene.control.Button;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuButton;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
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
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_CHOICE_BOX;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_COMMENTS_AREA;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_DATE_PICKER;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_EDIT_KEY;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_FEE_PAYER_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_FILE_UPDATE_CONTENTS;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_FILE_UPDATE_FILE_ID;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_HOURS;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_INITIAL_BALANCE;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_MAIN_CHOICE_BOX;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_MEMO_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_MINUTES;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_NANOS;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_NODE_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_SECONDS;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_SYSTEM_HOURS;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_SYSTEM_MINUTES;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_SYSTEM_SECONDS;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_TRANSACTION_FEE;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_TRANSFER_ACCEPT_FROM_BUTTON;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_TRANSFER_ACCEPT_TO_BUTTON;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_TRANSFER_FROM_ACCOUNT;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_TRANSFER_FROM_AMOUNT;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_TRANSFER_TO_ACCOUNT;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_TRANSFER_TO_AMOUNT;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.CREATE_UPDATE_ACCOUNT_ID;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.ENTITY_ID_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.FREEZE_DATE_PICKER;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.FREEZE_FILE_HASH_TEXT_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.FREEZE_FILE_IDTEXT_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.FREEZE_HOUR_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.FREEZE_MINUTE_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.FREEZE_SECONDS_FIELD;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.SET_NOW_BUTTON;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.SYSTEM_TIMEZONE_HBOX;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.SYSTEM_TRANSACTION_ACTION_CHOICE_BOX;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.SYSTEM_TRANSACTION_EXPIRATION_DATEPICKER;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.SYSTEM_TRANSACTION_TYPE_CHOICE_BOX;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.TIME_ZONE_HBOX;
import static com.hedera.hashgraph.client.integration.JavaFXIDs.UPDATE_EDIT_KEY;
import static com.hedera.hashgraph.client.integration.TestUtil.findButtonInPopup;
import static com.hedera.hashgraph.client.integration.TestUtil.getPopupNodes;
import static java.lang.Thread.sleep;

public class CreatePanePage {

	private final TestBase driver;

	private static final Logger logger = LogManager.getLogger(CreatePanePage.class);

	public CreatePanePage(TestBase driver) {
		this.driver = driver;
	}

	public CreatePanePage selectTransaction(String txType) {
		driver.clickOn(CREATE_MAIN_CHOICE_BOX);
		driver.clickOn(txType);
		return this;
	}

	public CreatePanePage loadTransaction(String path) {
		Node n = driver.find("#loadTransactionTextField");
		assert n instanceof TextField;
		((TextField) n).setText(path);
		driver.clickOn(n);
		driver.press(KeyCode.ENTER);
		driver.release(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setComment(String text) {
		driver.clickOn(CREATE_COMMENTS_AREA);
		driver.write(text);
		return this;
	}

	public CreatePanePage setDate(String date) {
		driver.doubleClickOn(CREATE_DATE_PICKER);
		driver.clickOn(CREATE_DATE_PICKER);
		driver.write(date);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setHours(int hours) {
		driver.doubleClickOn(CREATE_HOURS);
		driver.write(String.valueOf(hours));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setMinutes(int minutes) {
		driver.doubleClickOn(CREATE_MINUTES);
		driver.write(String.valueOf(minutes));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setSeconds(int seconds) {
		var node = driver.find(CREATE_SECONDS);
		assert node instanceof TextField;
		driver.clickOn(node);
		((TextField) node).selectAll();
		driver.write(String.valueOf(seconds));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setMemo(String memo) {
		driver.clickOn(CREATE_MEMO_FIELD);
		driver.write(memo);
		return this;
	}

	public CreatePanePage setFeePayerAccount(long accountID) {
		driver.clickOn(CREATE_FEE_PAYER_FIELD);
		var node = driver.find(CREATE_FEE_PAYER_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(String.valueOf(accountID));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setFeePayerAccount(String accountID) {
		driver.clickOn(CREATE_FEE_PAYER_FIELD);
		var node = driver.find(CREATE_FEE_PAYER_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(accountID);

		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setNodeAccount(long accountID) {
		driver.clickOn(CREATE_NODE_FIELD);
		var node = driver.find(CREATE_NODE_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(String.valueOf(accountID));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setNodeAccount(String accountID) {
		driver.clickOn(CREATE_NODE_FIELD);
		var node = driver.find(CREATE_NODE_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(accountID);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setInitialBalance(long amount) {
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

	public CreatePanePage setUpdateKey(String account) {
		driver.ensureVisible(driver.find(UPDATE_EDIT_KEY));
		driver.clickOn(UPDATE_EDIT_KEY);
		return clickOnAccountKey(account);
	}

	public CreatePanePage clickOnAccountKey(String account) {
		var titledPane3 = getTitledPane(TitledPaneEnum.ACCOUNTS);
		var inner3 = titledPane3.getContent();
		assert inner3 instanceof VBox;
		var inner4 = ((VBox) inner3).getChildren();
		assert inner4 != null;
		assert inner4.size() == 1;
		assert inner4.get(0) instanceof ListView;
		ListView<String> accountList = (ListView<String>) inner4.get(0);
		for (var item : accountList.getItems()) {
			if (item.contains(account)) {
				driver.doubleClickOn(item);
			}
		}
		return this;
	}

	public Button getCenterButton(CenterButtons button) {
		var popupNodes = getPopupNodes();
		assert popupNodes != null;
		assert popupNodes.size() == 2;
		assert popupNodes.get(1) instanceof VBox;
		var vBox0 = (VBox) popupNodes.get(1);
		var inner0 = vBox0.getChildren();
		assert inner0 != null;
		assert inner0.size() == 2;
		assert inner0.get(0) instanceof HBox;

		var hBox1 = (HBox) inner0.get(0);
		var inner1 = hBox1.getChildren();
		assert inner1 != null;
		assert inner1.get(1) instanceof VBox;

		var inner2 = ((VBox) inner1.get(1)).getChildren();
		assert inner2 != null;
		Button centerButton = null;
		for (var node : ((VBox) inner2.get(0)).getChildren()) {
			assert node instanceof Button;
			var text = ((Button) node).getText();
			if (text.isEmpty() && button.equals(CenterButtons.ARROW_BUTTON)) {
				centerButton = (Button) node;
			}
			if (!text.isEmpty() && button.equals(CenterButtons.CROSS_BUTTON)) {
				centerButton = (Button) node;
			}
		}
		return centerButton;

	}

	public TitledPane getTitledPane(TitledPaneEnum paneEnum) {
		var popupNodes = getPopupNodes();
		assert popupNodes != null;
		assert popupNodes.size() == 2;
		assert popupNodes.get(1) instanceof VBox;
		var vBox0 = (VBox) popupNodes.get(1);
		var inner0 = vBox0.getChildren();
		assert inner0 != null;
		assert inner0.size() == 2;
		assert inner0.get(0) instanceof HBox;

		var hBox1 = (HBox) inner0.get(0);
		var inner1 = hBox1.getChildren();
		assert inner1 != null;
		assert inner1.get(0) instanceof Accordion;

		var accordion2 = (Accordion) inner1.get(0);
		var inner2 = accordion2.getPanes();
		assert inner2 != null;

		driver.clickOn(inner2.get(paneEnum.value));
		return inner2.get(paneEnum.value);
	}

	public CreatePanePage createAndExport(String folder) {
		logger.info("Exporting transaction to {}", folder);
		HBox hBox = driver.find(CREATE_CHOICE_BOX);
		var children = hBox.getChildren();

		try {
			for (var n : children) {
				if (n instanceof MenuButton) {
					driver.clickOn(n);
					var nodes = driver.findAll(folder);
					for (var node : nodes) {
						if (node instanceof Label && !(node.getParent() instanceof GridPane)) {
							logger.info("click!");
							driver.clickOn(node);
						}
					}
				}
			}
		} catch (Exception e) {
			logger.error(e);
		}

		return this;
	}

	public CreatePanePage addDebit(long accountNum, double amount) {
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

	public CreatePanePage setFromAccountTransfer(long accountNum) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_FROM_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_FROM_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_FROM_ACCOUNT);
		driver.write(Long.toString(accountNum));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setFromAccountTransfer(String accountNum) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_FROM_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_FROM_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_FROM_ACCOUNT);
		driver.write(accountNum);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setToAccountTransfer(long accountNum) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_TO_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_TO_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_TO_ACCOUNT);
		driver.write(Long.toString(accountNum));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setToAccountTransfer(String accountNum) {
		driver.ensureVisible(driver.find(CREATE_TRANSFER_ACCEPT_TO_BUTTON));
		((TextField) driver.find(CREATE_TRANSFER_TO_ACCOUNT)).clear();
		driver.clickOn(CREATE_TRANSFER_TO_ACCOUNT);
		driver.write(accountNum);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage addCredit(long accountNum, double amount) {
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

	public CreatePanePage setUpdateAccount(long accountNumber) {
		driver.doubleClickOn(CREATE_UPDATE_ACCOUNT_ID);
		driver.clickOn(CREATE_UPDATE_ACCOUNT_ID);
		var node = driver.find(CREATE_UPDATE_ACCOUNT_ID);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.write(Long.toString(accountNumber));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setUpdateAccount(String accountNumber) {
		driver.ensureVisible(driver.find(CREATE_UPDATE_ACCOUNT_ID));
		driver.doubleClickOn(CREATE_UPDATE_ACCOUNT_ID);
		driver.clickOn(CREATE_UPDATE_ACCOUNT_ID);
		var node = driver.find(CREATE_UPDATE_ACCOUNT_ID);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.write(accountNumber);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setUpdateFileID(String accountNumber) {
		driver.ensureVisible(driver.find(CREATE_FILE_UPDATE_FILE_ID));
		driver.doubleClickOn(CREATE_FILE_UPDATE_FILE_ID);
		driver.clickOn(CREATE_FILE_UPDATE_FILE_ID);
		var node = driver.find(CREATE_FILE_UPDATE_FILE_ID);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.write(accountNumber);
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setUpdateFileID(long accountNumber) {
		driver.ensureVisible(driver.find(CREATE_FILE_UPDATE_FILE_ID));
		driver.doubleClickOn(CREATE_FILE_UPDATE_FILE_ID);
		driver.clickOn(CREATE_FILE_UPDATE_FILE_ID);
		var node = driver.find(CREATE_FILE_UPDATE_FILE_ID);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.write(Long.toString(accountNumber));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setOperation(OperationType type) {
		ChoiceBox<String> choiceBox = driver.find(SYSTEM_TRANSACTION_ACTION_CHOICE_BOX);
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

	public CreatePanePage setEntity(SystemEntity entity) {
		ChoiceBox<String> choiceBox = driver.find(SYSTEM_TRANSACTION_TYPE_CHOICE_BOX);
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

	public CreatePanePage setStartDate(LocalDateTime date) {
		var d = Date.from(date.atZone(ZoneId.systemDefault()).toInstant());
		return getCreatePanePage(d, CREATE_DATE_PICKER, CREATE_HOURS, CREATE_MINUTES, CREATE_SECONDS);
	}

	public CreatePanePage setExpirationDate(LocalDateTime date) {
		var d = Date.from(date.atZone(ZoneId.systemDefault()).toInstant());
		return getCreatePanePage(d, SYSTEM_TRANSACTION_EXPIRATION_DATEPICKER, CREATE_SYSTEM_HOURS,
				CREATE_SYSTEM_MINUTES, CREATE_SYSTEM_SECONDS);
	}

	public CreatePanePage setFreezeStartDate(LocalDateTime date) {
		var startTimeDate = Date.from(date.toInstant(OffsetDateTime.now().getOffset()));
		return getCreatePanePage(startTimeDate, FREEZE_DATE_PICKER, FREEZE_HOUR_FIELD, FREEZE_MINUTE_FIELD,
				FREEZE_SECONDS_FIELD);
	}

	private CreatePanePage getCreatePanePage(Date date, String datePicker, String hours, String minutes,
			String seconds) {

		var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getDefault());
		var localDateTime = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();


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
		var node = driver.find(seconds);
		assert node instanceof TextField;
		driver.clickOn(node);
		((TextField) node).selectAll();
		driver.write(String.valueOf(localDateTime.getSecond()));
		driver.type(KeyCode.TAB);
		return this;
	}

	public CreatePanePage setEntityID(long id) {
		driver.ensureVisible(driver.find(ENTITY_ID_FIELD));
		driver.clickOn(ENTITY_ID_FIELD);
		var node = driver.find(ENTITY_ID_FIELD);
		assert node instanceof TextField;
		((TextField) node).selectAll();
		driver.type(KeyCode.BACK_SPACE);
		driver.write(String.valueOf(id));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setEntityID(String id) {
		driver.ensureVisible(driver.find(ENTITY_ID_FIELD));
		driver.doubleClickOn(ENTITY_ID_FIELD);
		driver.write(id);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setContents(String contentsPath) {
		driver.ensureVisible(driver.find(CREATE_FILE_UPDATE_CONTENTS));
		driver.clickOn(CREATE_FILE_UPDATE_CONTENTS);
		driver.write(contentsPath);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage clickOnPopupButton(String legend) throws InterruptedException {
		var popupNodes = getPopupNodes();
		assert popupNodes != null;
		var button = findButtonInPopup(popupNodes, legend);
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
		TextField secs = driver.find(CREATE_SECONDS);
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
		HBox timeBox = driver.find(TIME_ZONE_HBOX);
		var children = timeBox.getChildren();
		for (var child : children) {
			if (child instanceof AutoCompleteNickname) {
				return ((AutoCompleteNickname) child).getText();
			}
		}
		return "";
	}

	public String getSystemTimezone() {
		HBox timeBox = driver.find(SYSTEM_TIMEZONE_HBOX);
		var children = timeBox.getChildren();
		for (var child : children) {
			if (child instanceof AutoCompleteNickname) {
				return ((AutoCompleteNickname) child).getText();
			}
		}
		return "";
	}

	public CreatePanePage setStartTimezone(String timezone) {
		HBox timeBox = driver.find(TIME_ZONE_HBOX);
		var children = timeBox.getChildren();
		driver.ensureVisible(timeBox);
		for (var child : children) {
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

	public CreatePanePage setSystemTimezone(String timezone) {
		HBox timeBox = driver.find(SYSTEM_TIMEZONE_HBOX);
		driver.ensureVisible(timeBox);
		var children = timeBox.getChildren();
		for (var child : children) {
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
		var popupNodes = getPopupNodes();
		assert popupNodes != null;
		assert popupNodes.size() == 2;
		assert popupNodes.get(1) instanceof VBox;
		var vBox0 = (VBox) popupNodes.get(1);
		var inner0 = vBox0.getChildren();
		assert inner0 != null;
		assert inner0.size() == 2;
		assert inner0.get(0) instanceof HBox;

		var hBox1 = (HBox) inner0.get(1);
		var inner1 = hBox1.getChildren();
		assert inner1 != null;
		for (var node : inner1) {
			if (node instanceof Button && ((Button) node).getText().equals("SAVE")) {
				driver.clickOn(node);
				break;
			}
		}
		return this;
	}

	public CreatePanePage clickOnKeyDesignerCancel() {
		var popupNodes = getPopupNodes();
		assert popupNodes != null;
		assert popupNodes.size() == 2;
		assert popupNodes.get(1) instanceof VBox;
		var vBox0 = (VBox) popupNodes.get(1);
		var inner0 = vBox0.getChildren();
		assert inner0 != null;
		assert inner0.size() == 2;
		assert inner0.get(0) instanceof HBox;

		var hBox1 = (HBox) inner0.get(1);
		var inner1 = hBox1.getChildren();
		assert inner1 != null;
		for (var node : inner1) {
			if (node instanceof Button && ((Button) node).getText().equals("CANCEL")) {
				driver.clickOn(node);
				break;
			}
		}
		return this;

	}

	public CreatePanePage setTransactionFee(double transactionFee) {
		Node n = driver.find(CREATE_TRANSACTION_FEE);
		assert n instanceof TextField;
		((TextField) n).setText(Double.toString(transactionFee));
		driver.clickOn(n);
		driver.press(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setNanos(String nanosString) {
		Node n = driver.find(CREATE_NANOS);
		assert n instanceof TextField;
		((TextField) n).setText(nanosString);
		driver.clickOn(n);
		driver.press(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setFreezeType(FreezeType freezeOnly) {
		var type = freezeOnly.equals(FreezeType.FREEZE_UPGRADE) ?
				"Freeze and upgrade" :
				freezeOnly.toString()
						.replace("_", " ")
						.toLowerCase(Locale.ROOT);

		driver.clickOn("#freezeTypeChoiceBox");
		driver.clickOn(StringUtils.capitalize(type));
		return this;
	}

	public CreatePanePage setFreezeFileId(int i) {
		driver.ensureVisible(driver.find(FREEZE_FILE_IDTEXT_FIELD));
		driver.clickOn(FREEZE_FILE_IDTEXT_FIELD);
		driver.write(String.valueOf(i));
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setFreezeHash(String hash) {
		driver.ensureVisible(driver.find(FREEZE_FILE_HASH_TEXT_FIELD));
		driver.doubleClickOn(FREEZE_FILE_HASH_TEXT_FIELD);
		driver.write(hash);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public CreatePanePage setStartDateTime(Date date) {
		var datePickerFormat = new SimpleDateFormat("MM/dd/yyyy");
		datePickerFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
		var sdf = new SimpleDateFormat("yyyy-MM-dd");
		sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
		var calendar = Calendar.getInstance();
		calendar.setTime(date);

		return this.setDate(datePickerFormat.format(date))
				.setHours(calendar.get(Calendar.HOUR_OF_DAY))
				.setMinutes(calendar.get(Calendar.MINUTE))
				.setSeconds(calendar.get(Calendar.SECOND))
				.setNanos("123456789");

	}

	public CreatePanePage setChunkSize(int i) {
		driver.ensureVisible(driver.find("#chunkSizeTextField"));
		driver.doubleClickOn("#chunkSizeTextField");
		driver.write(String.valueOf(i));
		return this;
	}

	public CreatePanePage setInterval(long interval) {
		driver.ensureVisible(driver.find("#intervalTextField"));
		driver.doubleClickOn("#intervalTextField");
		driver.write(String.valueOf(interval));
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

		TitledPaneEnum(int i) {
			value = i;
		}
	}

	public enum CenterButtons {
		ARROW_BUTTON, CROSS_BUTTON
	}

}
