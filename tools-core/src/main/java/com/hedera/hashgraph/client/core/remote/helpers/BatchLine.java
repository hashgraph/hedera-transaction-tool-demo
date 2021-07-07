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

package com.hedera.hashgraph.client.core.remote.helpers;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Objects;
import java.util.TimeZone;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.INCOMPATIBLE_TYPES_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.NULL_OBJECT_COMPARISON_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.OUT_OF_RANGE_EXCEPTION_MESSAGE;

public class BatchLine implements Comparable {

	private final Identifier receiverAccountID;
	private final long amount;
	private Timestamp date;


	private BatchLine(Identifier receiverAccountID, long amount, Timestamp date) {
		this.receiverAccountID = receiverAccountID;
		this.amount = amount;
		this.date = date;
	}

	public static BatchLine parse(String line, int hour, int minutes) throws HederaClientException {
		if (hour > 23 || hour < 0) {
			throw new HederaClientException(OUT_OF_RANGE_EXCEPTION_MESSAGE + "hour");
		}

		if (minutes > 59 || minutes < 0) {
			throw new HederaClientException(OUT_OF_RANGE_EXCEPTION_MESSAGE + "minute");
		}

		var fields = line.replace("\"", "").replace(" ", "").split("[,]");
		if (fields.length < 3) {
			throw new HederaClientRuntimeException(String.format("Missing fields in: %s", line));
		}

		return new BatchLine.Builder()
				.withReceiverAccountID(fields[0])
				.withAmount(fields[1])
				.withTimeStamp(fields[2], hour, minutes)
				.build();
	}

	public Identifier getReceiverAccountID() {
		return receiverAccountID;
	}

	public long getAmount() {
		return amount;
	}

	public Timestamp getDate() {
		return date;
	}

	public void setDate(Timestamp date) {
		this.date = date;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.JSON_STYLE)
				.append("receiverAccountID", receiverAccountID)
				.append("amount", amount)
				.append("dateTime", date.toString())
				.toString();
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (!(o instanceof BatchLine)) {
			return false;
		}
		var batchLine = (BatchLine) o;
		return amount == batchLine.amount &&
				receiverAccountID.equals(batchLine.receiverAccountID) &&
				date.equals(batchLine.date);
	}


	@Override
	public int hashCode() {
		return Objects.hash(receiverAccountID, amount, date);
	}

	@Override
	public int compareTo(Object o) {
		if (o == null) {
			throw new NullPointerException(NULL_OBJECT_COMPARISON_ERROR_MESSAGE);
		}

		if (getClass() != o.getClass()) {
			throw new HederaClientRuntimeException(INCOMPATIBLE_TYPES_ERROR_MESSAGE);
		}

		if (this.equals(o)) {
			return 0;
		}

		if (!this.getDate().equals(((BatchLine) o).getDate())) {
			return this.getDate().asInstant().compareTo(((BatchLine) o).getDate().asInstant());
		}

		if (!this.getReceiverAccountID().equals(((BatchLine) o).getReceiverAccountID())) {
			return this.getReceiverAccountID().compareTo(((BatchLine) o).getReceiverAccountID());
		}

		return Long.compare(this.getAmount(), ((BatchLine) o).getAmount());

	}

	public static final class Builder {
		private Identifier receiverAccountID;
		private long amount;
		private Timestamp timestamp;

		public Builder() {
		}

		public Builder withReceiverAccountID(String stringAccountID) {
			var receiverID = Identifier.parse(stringAccountID);
			this.receiverAccountID = receiverID;
			return this;
		}

		public Builder withAmount(String amountString) {
			long amount;
			try {
				amount = Long.parseLong(amountString);
			} catch (NumberFormatException e) {
				throw new HederaClientRuntimeException(String.format("Invalid amount: %s", amountString));
			}

			if (amount <= 0) {
				throw new HederaClientRuntimeException("Amount cannot be negative or zero");
			}
			this.amount = amount;
			return this;
		}

		/**
		 * Build a timestamp from the information provided by the csv file
		 *
		 * @param excelDate
		 * 		Date string in the
		 * @param hour
		 * @param minutes
		 * @return
		 * @throws HederaClientException
		 */
		public Builder withTimeStamp(String excelDate, int hour, int minutes) throws HederaClientException {
			final var formatter = new SimpleDateFormat("MM/dd/yy");
			formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
			var calendar = Calendar.getInstance();
			try {
				calendar.setTime(formatter.parse(excelDate));
				calendar.add(Calendar.HOUR_OF_DAY, hour);
				calendar.add(Calendar.MINUTE, minutes);
				var millis = calendar.getTimeInMillis();
				this.timestamp = new Timestamp(millis / 1000, (int) (1000000 * millis % 1000));
			} catch (ParseException ex) {
				throw new HederaClientException("Cannot parse date");
			}
			return this;
		}

		public BatchLine build() {
			return new BatchLine(receiverAccountID, amount, timestamp);
		}


	}
}
