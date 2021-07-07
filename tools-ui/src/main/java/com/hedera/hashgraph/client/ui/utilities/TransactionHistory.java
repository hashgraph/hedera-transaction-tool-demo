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

import com.google.gson.JsonObject;

import java.util.Date;

public class TransactionHistory {
	private String title;
	private String transactionValidStart;
	private String type;
	private Date archiveDate;

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getTransactionValidStart() {
		return transactionValidStart;
	}

	public void setTransactionValidStart(String transactionValidStart) {
		this.transactionValidStart = transactionValidStart;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Date getArchiveDate() {
		return archiveDate;
	}

	public void setArchiveDate(Date archiveDate) {
		this.archiveDate = archiveDate;
	}

	public TransactionHistory(JsonObject transaction) {
		this((transaction.has("memo")) ? transaction.get("memo").getAsString() : "Unknown",
				transaction.has("transactionValidStart") ? new Date(1000 * (transaction.getAsJsonObject(
						"transactionValidStart")).get("seconds").getAsLong()).toString() : "Unknown",
				(transaction.has("type")) ? transaction.get("type").getAsString() : "Unknown",
				new Date((transaction.has("dated")) ? transaction.get("dated").getAsLong() : 0));
	}

	public TransactionHistory(String title, String transactionValidStart, String type, Date archiveDate) {
		this.title = title;
		this.transactionValidStart = transactionValidStart;
		this.type = type;
		this.archiveDate = archiveDate;
	}
}
