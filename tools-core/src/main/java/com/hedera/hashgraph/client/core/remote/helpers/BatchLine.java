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

package com.hedera.hashgraph.client.core.remote.helpers;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.jetbrains.annotations.NotNull;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Objects;
import java.util.TimeZone;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.INCOMPATIBLE_TYPES_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.OUT_OF_RANGE_EXCEPTION_MESSAGE;

public class BatchLine implements Comparable<BatchLine> {

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
	public int compareTo(@NotNull BatchLine o) {
		if (getClass() != o.getClass()) {
			throw new HederaClientRuntimeException(INCOMPATIBLE_TYPES_ERROR_MESSAGE);
		}

		if (this.equals(o)) {
			return 0;
		}

		if (!this.getDate().equals(o.getDate())) {
			return this.getDate().asInstant().compareTo(o.getDate().asInstant());
		}

		if (!this.getReceiverAccountID().equals(o.getReceiverAccountID())) {
			return this.getReceiverAccountID().compareTo(o.getReceiverAccountID());
		}

		return Long.compare(this.getAmount(), o.getAmount());
	}

	public static final class Builder {
		private Identifier receiverAccountID;
		private long amount;
		private Timestamp timestamp;

		public Builder() {
			// Empty default constructor
		}

		public Builder withReceiverAccountID(String stringAccountID) {
			this.receiverAccountID = Identifier.parse(stringAccountID);
			return this;
		}

		public Builder withAmount(String amountString) {
			long parsedAmount;
			try {
				parsedAmount = Long.parseLong(amountString);
			} catch (NumberFormatException e) {
				throw new HederaClientRuntimeException(String.format("Invalid amount: %s", amountString));
			}

			if (parsedAmount <= 0) {
				throw new HederaClientRuntimeException("Amount cannot be negative or zero");
			}
			this.amount = parsedAmount;
			return this;
		}

		/**
		 * Build a timestamp from the information provided by the csv file
		 *
		 * @param excelDate
		 * 		Date string in the
		 * @param hour
		 * 		the hour (0-23)
		 * @param minutes
		 * 		the minutes (0-59)
		 * @return a TimeStamp builder
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
