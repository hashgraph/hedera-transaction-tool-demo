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

package com.hedera.hashgraph.client.core.utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;


public class TimeUtils {

	private static final Logger logger = LogManager.getLogger(TimeUtils.class);
	private static final String DATE_TIME_FULL_PATTERN = "yyyy-MM-dd'T'HH:mm:ss";
	private static final String DATE_TIME_SHORT_MM_FIRST_PATTERN = "MM/dd/yy";

	private TimeUtils() {
		throw new IllegalStateException("Utility class");
	}

	public static Calendar excelDateTimeToCalendar(String excelDate) {

		final var applyingPattern = excelDate.contains(
				"T") ? DATE_TIME_FULL_PATTERN : DATE_TIME_SHORT_MM_FIRST_PATTERN;
		final var formatter = new SimpleDateFormat(applyingPattern);
		formatter.setTimeZone(TimeZone.getTimeZone("UTC"));

		var calendar = Calendar.getInstance();

		try {
			calendar.setTime(formatter.parse(excelDate));
		} catch (ParseException ex) {
			logger.error("Parsing error", ex);
			return null;
		}
		return calendar;
	}


	/**
	 * @throws IllegalArgumentException
	 * 		when the value is negative
	 */
	public static void notNegativeValueCheck(final long value, final String fieldName) throws IllegalArgumentException {
		if (value < 0) {
			throw new IllegalArgumentException(fieldName + " must be greater than zero");
		}
	}


}
