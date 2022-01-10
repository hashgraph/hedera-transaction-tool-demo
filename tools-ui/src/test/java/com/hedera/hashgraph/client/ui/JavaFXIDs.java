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

package com.hedera.hashgraph.client.ui;

public class JavaFXIDs {
	public static final String ONEDRIVE_PATH_TEXT_FIELD = "#pathTextField";

	public final static String EMAIL_TEXT_FIELD = "#emailTextField";
	public final static String GENERATE_KEYS_BUTTON = "#generateKeys";
	public final static String COPY_TO_CLIPBOARD = "#copyToClipBoardButton";
	public final static String FINISH_BOX = "#finishBox";
	public final static String PASTE_FROM_CLIPBOARD = "#pasteFromClipBoardButton";
	public final static String MNEMONIC_ERROR_MESSAGE = "#mnemonicErrorMessage";

	public final static String APP_PASSWORD_FIELD_1 = "#appPasswordField";
	public final static String APP_PASSWORD_FIELD_2 = "#reEnterPasswordField";

	// Main Pane
	// region IDs

	public final static String HOME_BUTTON = "#homeButton";
	public final static String SIGNATURE_BUTTON = "#signatureButton";
	public final static String CREATE_BUTTON = "#createButton";
	public final static String ACCOUNTS_BUTTON = "#accountsButton";
	public final static String KEYS_BUTTON = "#keysButton";
	public final static String SETTINGS_BUTTON = "#settingsButton";
	public final static String CREATE_ANCHOR_PANE = "#createAnchorPane";


	// Home Pane
	public final static String NEW_FILES_VBOX = "#newFilesViewVBox";
	public final static String HISTORY_FILES_VBOX = "#historyFilesVBox";
	public final static String MAIN_TRANSACTIONS_SCROLLPANE = "#homeFilesScrollPane";


	// Create Pane

	public final static String CREATE_MAIN_CHOICE_BOX = "#selectTransactionType";

	public final static String CREATE_TRANSFER_ACCEPT_FROM_BUTTON = "#acceptFromAccountButton";
	public final static String CREATE_TRANSFER_ACCEPT_TO_BUTTON = "#acceptToAccountButton";
	public final static String CREATE_COMMENTS_BOX = "#commentsVBox";
	public final static String CREATE_COMMON_FIELDS_BOX = "#commonFieldsVBox";
	public final static String CREATE_CREATE_BOX = "#createAccountVBox";
	public final static String CREATE_UPDATE_BOX = "#updateAccountVBox";
	public final static String CREATE_TRANSFER_BOX = "#transferCurrencyVBox";
	public final static String CREATE_CHOICE_BOX = "#createChoiceHBox";

	public final static String CREATE_SYSTEM_EXPIRATION_VBOX = "#systemExpirationVBox";

	public final static String CREATE_MEMO_FIELD = "#memoField";
	public final static String CREATE_FEE_PAYER_FIELD = "#feePayerAccountField";
	public final static String ENTITY_ID_FIELD = "#entityID";
	public final static String CREATE_NODE_FIELD = "#nodeAccountField";
	public final static String CREATE_HOURS = "#hourField";
	public final static String CREATE_MINUTES = "#minuteField";
	public final static String CREATE_SECONDS = "#secondsField";
	public final static String CREATE_NANOS = "#nanosField";
	public final static String CREATE_SYSTEM_HOURS = "#hourFieldSystem";
	public final static String CREATE_SYSTEM_MINUTES = "#minuteFieldSystem";
	public final static String CREATE_SYSTEM_SECONDS = "#secondsFieldSystem";
	public final static String CREATE_TRANSACTION_FEE = "#transactionFee";
	public final static String CREATE_INITIAL_BALANCE = "#createInitialBalance";
	public final static String CREATE_TRANSFER_TO_ACCOUNT = "#transferToAccountIDTextField";
	public final static String CREATE_TRANSFER_TO_AMOUNT = "#transferToAmountTextField";
	public final static String CREATE_TRANSFER_FROM_ACCOUNT = "#transferFromAccountIDTextField";
	public final static String CREATE_TRANSFER_FROM_AMOUNT = "#transferFromAmountTextField";
	public final static String CREATE_UPDATE_ACCOUNT_ID = "#updateAccountID";
	public final static String CREATE_UPDATE_AUTO_RENEW = "#updateAutoRenew";
	public final static String CREATE_UPDATE_ARPO_O = "#updateARPOriginal";
	public final static String CREATE_AUTO_RENEW = "#createAutoRenew";
	public final static String SYSTEM_TRANSACTION_ACTION_CHOICE_BOX = "#systemActionChoiceBox";
	public final static String SYSTEM_TRANSACTION_TYPE_CHOICE_BOX = "#systemTypeChoiceBox";
	public final static String SYSTEM_TRANSACTION_EXPIRATION_DATEPICKER = "#datePickerSystem";
	public final static String CREATE_FILE_UPDATE_FILE_ID = "#updateFileID";
	public final static String CREATE_FILE_UPDATE_CONTENTS = "#contentsTextField";

	public final static String CREATE_FROM_TABLE = "#fromTransferTable";
	public final static String CREATE_TO_TABLE = "#toTransferTable";

	public final static String CREATE_COMMENTS_AREA = "#createCommentsTextArea";

	public final static String CREATE_DATE_PICKER = "#datePicker";

	// Labels
	public final static String CREATE_TRANSFER_TOTAL_LABEL = "#totalTransferLabel";
	public final static String CREATE_LOCAL_TIME_LABEL = "#createUTCTimeLabel";
	public final static String SYSTEM_ENTITY_ID_LABEL = "#entityLabel";
	public final static String SYSTEM_TIMEZONE_HBOX = "#timeZoneSystemHBox";
	// Error messages
	public final static String CREATE_INVALID_DATE = "#invalidDate";
	public final static String CREATE_INVALID_FEE_PAYER = "#invalidFeePayer";
	public final static String CREATE_INVALID_NODE = "#invalidNode";
	public final static String CREATE_INVALID_CREATE_INITIAL_BALANCE = "#invalidCreateInitialBalance";
	public final static String CREATE_INVALID_CREATE_AUTO_RENEW = "#invalidCreateAutoRenew";
	public final static String CREATE_INVALID_CREATE_NEW_KEY = "#invalidCreateNewKey";
	public final static String CREATE_INVALID_UPDATE_AUTO_RENEW = "#invalidUpdatedAutoRenew";
	public final static String CREATE_INVALID_UPDATE_NEW_KEY = "#invalidUpdateNewKey";
	public final static String CREATE_INVALID_TRANSFER_FROM_ACCOUNT = "#errorInvalidFromAccount";
	public final static String CREATE_INVALID_TRANSFER_TO_ACCOUNT = "#errorInvalidToAccount";
	public final static String CREATE_INVALID_TRANSFER_LIST = "#invalidTransferList";
	public final static String CREATE_INVALID_TRANSFER_TOTAL = "#invalidTransferTotal";
	public final static String CREATE_INVALID_UPDATE_ACCOUNT = "#invalidUpdateAccountToUpdate";

	// Keys scroll panes
	public final static String CREATE_UPDATE_ORIGINAL_KEY = "#updateOriginalKey";
	public final static String CREATE_UPDATE_NEW_KEY = "#updateNewKey";
	public final static String CREATE_CREATE_KEY = "#createNewKey";

	public final static String PUBLIC_KEYS_VBOX = "#signingKeysVBox";

	public final static String KEYS_GENERATE_KEYS = "#btnCreateKeys";
	public final static String KEYS_RECOVER_KEYS = "#btnRegenerateKeys";
	public final static String KEYS_RECOVERY_PHRASE_BUTTON = "#btnShowMnemonicWords";
	public final static String KEYS_CREATE_KEYS = "#createKeysButton";
	public final static String KEYS_CANCEL_KEYS = "#cancelCreationButton";
	public final static String KEYS_MNEMONIC_CLOSE_VIEW = "#cancelEditMnemonicButton";

	public final static String CREATE_KEYS_VBOX = "#createKeysVBox";
	public final static String NICKNAME = "#nicknameTextBox";
	public final static String PASSWORD_BOX = "#passwordBox";
	public final static String RETYPE_PASSWORD_BOX = "#retypePasswordBox";
	public final static String HIDDEN_ACCOUNT_INFO_TEXTFIELD = "#hiddenPathAccount";
	public final static String CURRENT_ACCOUNT_PANE = "#currentAccountPane";
	public final static String ACCOUNTS_RECOVER_KEY_INDEX = "#recoverIndexField";
	public final static String ACCOUNTS_RECOVER_KEY_NICKNAME = "#recoverNicknameField";
	public final static String CANCEL_RECOVER_KEYS = "#cancelRecoverKeyButton";
	public final static String ACCOUNTS_RECOVER_KEYS_BUTTON = "#recoverKeysButton";
	public final static String ACCOUNTS_NICKNAME_ERROR_LABEL = "#nicknameErrorLabel";
	public final static String ACCOUNTS_SCROLL_PANE = "#accountsScrollPane";

	public final static String LOAD_STORAGE_TF = "#loadStorageTextField";
	public final static String ONEDRIVE_PATH_TF = "#pathTextFieldSP";
	public final static String ONEDRIVE_EMAIL_TF = "#emailTextFieldSP";
	public final static String NODE_ID_TF = "#nodeIDTextField";
	public final static String TX_VALID_DURATION_TF = "#txValidDurationTextField";
	public final static String AUTO_RENEW_PERIOD_TF = "#autoRenewPeriodTextField";
	public final static String TVS_HOURS_TF = "#hoursTextField";
	public final static String TVS_MINUTES_TF = "#minutesTextField";
	public final static String TVS_SECONDS_TF = "#secondsTextField";
	public final static String TRANSACTION_FEE_TF = "#defaultTransactionFee";
	public final static String GENERATE_RECORD_SLIDER = "#generateRecordSlider";
	public final static String ADD_FOLDER_BUTTON = "#addFolderButton";
	public final static String CANCEL_ADD_FOLDER_BUTTON = "#cancelAddToEmailMapButton";
	public final static String IMPORT_ACCOUNT_BUTTON = "#importAccountButton";

	public static final String ACCEPT_APP_PASSWORD = "#acceptPasswordButton";
	public static final String SIGNING_KEYS_VBOX = "#signingKeysVBox";

	// load keys
	public static final String CREATE_EDIT_KEY = "#createKeyButton";
	public static final String UPDATE_EDIT_KEY = "#updateKeyButton";

	// Time
	public static final String SET_NOW_BUTTON = "#setNowValidStart";
	public static final String TIME_ZONE_HBOX = "#timeZoneHBox";

	// Freeze
	public static final String FREEZE_DATE_PICKER = "#freezeDatePicker";
	public static final String FREEZE_HOUR_FIELD = "#freezeHourField";
	public static final String FREEZE_MINUTE_FIELD = "#freezeMinuteField";
	public static final String FREEZE_SECONDS_FIELD = "#freezeSecondsField";
	public static final String FREEZE_FILE_HASH_TEXT_FIELD = "#freezeFileHashTextField";
	public static final String FREEZE_FILE_IDTEXT_FIELD = "#freezeFileIDTextField";
}
