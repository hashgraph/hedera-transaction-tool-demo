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

public class Messages {
	private Messages() {
		throw new IllegalStateException("Constants class");
	}

	public static final String TRANSACTION_STATUS_MESSAGE = "Transaction status: {}";
	public static final String DELAY_MESSAGE = "Transaction will be sent in {} seconds";
	public static final String NEW_PASSWORD_CONFIRMATION_MESSAGE = "Please re-enter the password for confirmation";
	public static final String PEM_EXISTS_MESSAGE = "This key has a PEM in the Keys folder. Marked as duplicate";
	public static final String REQUEST_PASSWORD_MESSAGE = "Enter the password for key %s";
	public static final String REMOVE_DRIVE_MESSAGE =
			"This operation will delete information regarding this shared folder from your application and you will no" +
					" " +
					"longer access files from this location. Are you sure?";
	public static final String LINKED_PUBLIC_KEY_TOOLTIP_TEXT =
			"A key pair that is recoverable using the current application recovery phrase. This key can be used to " +
					"sign transactions.";
	public static final String UNLINKED_PUBLIC_KEY_TOOLTIP_TEXT =
			"A public key that does not have a corresponding private key";
	public static final String UNLINKED_PK_TOOLTIP_TEXT =
			"A key pair that is not recoverable using the current application recovery phrase. This key can be used " +
					"to sign transactions.";
	public static final String RESET_ALERT_MESSAGE =
			"This operation will reset all the settings, archive all folders and close the application.\n\nDo " +
					"you want to continue?";
	public static final String PASSWORD_NOT_FOUND_MESSAGE =
			"Your application credentials cannot be verified. You will be prompted to restore your " +
					"application. You will need your 24 word recovery phrase.";
	public static final String INITIAL_SETUP_RESET_MESSAGE = "Are you sure you want to reset the application setup?";
	public static final String TRANSACTION_CREATED_MESSAGE = "Create Account transaction has been successfully created";
	public static final String RESET_PASSWORD_MESSAGE =
			"The application will close now. Please restart the application manually and go through the initial setup" +
					" " +
					"again. When prompted to enter the recovery phrase, please use the one from your records.";
	public static final String WARNING_MESSAGE =
			"This process will change your password.\n You will need to have your recovery phrase at hand to enter it" +
					" " +
					"at the prompt.\n Please be advised that the Keys that were generated and stored with the old " +
					"password will have to be recovered. Please press the \"Recover Key\" button in the \"Accounts " +
					"and" +
					" " +
					"Keys\" page to change the password on your Keys";
	public static final String OUTPUT_FILE_CREATED_MESSAGE = "Output file {} created";
	public static final String FILE_HAS_BEEN_DELETED_MESSAGE = "File has been deleted";
	public static final String NICKNAME_IN_USE_MESSAGE =
			"The nickname \"%s\" is already in use. Please choose another one.";
	public static final String CONTINUE_MESSAGE = "CONTINUE";
}
