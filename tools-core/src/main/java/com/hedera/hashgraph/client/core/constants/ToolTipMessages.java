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

package com.hedera.hashgraph.client.core.constants;

public class ToolTipMessages {
	private ToolTipMessages() {
		throw new IllegalStateException("Constants class");
	}

	public static final String NOW_TOOLTIP_TEXT =
			"The transaction must be submitted to the network shortly after the time chosen (within 2 " +
					"minutes of that time). The \"now\" button sets it to the current time. It’s usually " +
					"best to increase the time after clicking \"now\".";
	public static final String ACCOUNTS_BOX_DESIGNER_TOOLTIP_TEXT =
			"Accounts that have been imported to the app. Double clicking on any account name will set its " +
					"key in the Design Box";
	public static final String PUBLIC_KEYS_BOX_DESIGNER_TOOLTIP_TEXT =
			"Keys that have been imported to the app, as well as the keys manually selected by the user. Drag and " +
					"drop them to the design box";
	public static final String FILTER_TOOLTIP_TEXT =
			"Select the file types to show in the History. If no filters are selected, all the History will be shown";
	public static final String VALID_DURATION_TOOLTIP_MESSAGE = "The period of time in " +
			"seconds for when the transaction is valid on the Hedera network.\n" +
			"Min: 30 seconds Max: 180 seconds";
	public static final String GENERATE_RECORD_TOOLTIP_MESSAGE =
			"Whether the transaction should generate a record or not";
	public static final String START_TIME_TOOLTIP_MESSAGE =
			"The start time of the transaction from which the transaction valid duration begins in " +
					"UTC format.";
	public static final String FOLDER_TOOLTIP_MESSAGES =
			"The shared folder must contain the InputFiles and OutputFiles directories.";
	public static final String TRANSACTION_FEE_TOOLTIP_MESSAGE = "The max transaction fee that will be offered";
	public static final String NODE_ID_TOOLTIP_MESSAGE =
			"The account ID of the node that will submit the transaction to the Hedera network";
	public static final String NETWORKS_TOOLTIP_MESSAGES =
			"The default network to which the app will submit all transactions and queries, unless otherwise " +
					"specified.";
	public static final String FEE_PAYER_TOOLTIP_MESSAGES =
			"The account that will be used as a fee payer for all queries, unless otherwise specified";
	public static final String AUTO_RENEW_PERIOD_TOOLTIP_MESSAGE =
			"The period of time in which the account will renew in seconds.\n" +
					"Min:7000000 seconds \n" +
					"Max: 8000000 seconds";
}