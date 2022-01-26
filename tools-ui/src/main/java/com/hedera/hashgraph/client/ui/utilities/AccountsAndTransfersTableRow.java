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

import javafx.beans.property.SimpleStringProperty;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TableRow;
import javafx.scene.text.TextFlow;

public class AccountsAndTransfersTableRow extends TableRow<String> {

	private TextFlow accountType;
	private final SimpleStringProperty description;
	private final SimpleStringProperty paidBy;
	private CheckBox checkbox;
	private String TransactionId;
	private Button sign;

	public Button getSign() {
		return sign;
	}

	public void setSign(final Button sign) {
		this.sign = sign;
	}

	public String getTransactionId() {
		return TransactionId;
	}

	public void setTransactionId(final String transactionId) {
		TransactionId = transactionId;
	}

	AccountsAndTransfersTableRow(final CheckBox cb, final TextFlow accountType, final String description,
			final String paidBy,
			final Button sign) {
		this.accountType = accountType;
		this.description = new SimpleStringProperty(description);
		this.paidBy = new SimpleStringProperty(paidBy);
		this.checkbox = cb;
		this.sign = sign;
	}

	public TextFlow getAccountType() {
		return accountType;
	}

	public void setAccountType(final TextFlow accountType) {
		this.accountType = accountType;
	}

	public String getDescription() {
		return description.get();
	}

	public void setDescription(final String description) {
		this.description.set(description);
	}

	public String getPaidBy() {
		return paidBy.get();
	}

	public void setPaidBy(final String paidBy) {
		this.paidBy.set(paidBy);
	}

	public CheckBox getCheckbox() {
		return checkbox;
	}

	public void setCheckbox(final CheckBox checkbox) {
		this.checkbox = checkbox;
	}


}
