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

import java.io.File;

public class Constants {


	private Constants() {
		throw new IllegalStateException("Constants class");
	}

	public static final int MIN_PASSWORD_LENGTH = 10;
	public static final int MAX_PASSWORD_LENGTH = 1024;
	public static final int MNEMONIC_SIZE = 24;

	// region FILE EXTENSIONS
	public static final String PK_EXTENSION = "pem";
	public static final String PUB_EXTENSION = "pub";
	public static final String INFO_EXTENSION = "info";
	public static final String RECEIPT_EXTENSION = "rcpt";
	public static final String TRANSACTION_EXTENSION = "tx";
	public static final String SIGNATURE_EXTENSION = "sig";
	public static final String SIGNED_TRANSACTION_EXTENSION = "txsig";
	public static final String ZIP_EXTENSION = "zip";
	public static final String TXT_EXTENSION = "txt";
	public static final String METADATA_EXTENSION = "meta";
	public static final String JSON_EXTENSION = "json";
	public static final String BATCH_TRANSACTION_EXTENSION = "csv";
	public static final String SOFTWARE_UPDATE_EXTENSION = "pkg";
	public static final String LARGE_BINARY_EXTENSION = "lfu";
	public static final String COMMENT_EXTENSION = "txt";
	public static final String CONFIGURATION_EXTENSION = "cfg";
	public static final String AES_EXTENSION = "aes";
	public static final String GPG_EXTENSION = "asc";
	public static final String BUNDLE_EXTENSION = "zip";
	public static final String CONTENT_EXTENSION = "zip";
	// endregion


	// region PROPERTY NAMES
	public static final String LAST_TRANSACTIONS_DIRECTORY = "lastTransactionsDirectory";
	public static final String PREFERRED_STORAGE_DIRECTORY = "preferredStorageDirectory";
	public static final String DEFAULT_TX_FEE = "defaultTxFee";
	public static final String DEFAULT_NODE_ID = "defaultNodeID";
	public static final String TX_VALID_DURATION = "txValidDuration";
	public static final String DEFAULT_HOURS = "defaultHours";
	public static final String DEFAULT_MINUTES = "defaultMinutes";
	public static final String DEFAULT_SECONDS = "defaultSeconds";
	public static final String DEFAULT_AUTO_RENEW_PERIOD = "autoRenewPeriod";
	public static final String LAST_INDEX = "lastIndex";
	public static final String HASH = "hash";
	public static final String VERSION = "version";
	public static final String MNEMONIC_HASH_CODE = "hashCode";
	public static final String ACCOUNT_INFO_MAP = "accountInfoMap";
	public static final String USER_NAME = "userName";
	public static final String SETUP_PHASE = "setupPhase";
	public static final String GENERATE_RECORD = "generateRecord";
	public static final String SALT_PROPERTY = "salt";
	public static final String LEGACY = "legacy";
	public static final String CUSTOM_NETWORKS = "customNetwork";
	public static final String NETWORKS = "networks";
	public static final String CURRENT_NETWORK = "currentNetwork";
	public static final String INPUT_FILES = "InputFiles";
	public static final String OUTPUT_FILES = "OutputFiles";
	public static final String NO_ACCOUNT_FOUND_TEXT = "No account found";
// endregion

	// region APP DEFAULTS
	public static final boolean DEVELOPMENT = false;
	public static final int DRIVE_LIMIT = 32;
	public static final double RELOAD_PERIOD = 1.0; //One minute
	public static final int KEYS_COLUMNS = 5;
	public static final String TEST_PASSWORD = "123456789";
	public static final String COMMA_DELIMITER = ",";
	// endregion

	// region TRANSACTION DEFAULTS
	public static final int VAL_NUM_TRANSACTION_VALID_DURATION = 180;
	public static final long VAL_NUM_TRANSACTION_DEFAULT_FEE = 100000000;
	public static final int MINIMUM_AUTO_RENEW_PERIOD = 7000000;
	public static final int MAXIMUM_AUTO_RENEW_PERIOD = 8000000;
	public static final int COMMENT_FIELD_CHARACTER_LIMIT = 256;
	public static final int MAX_NUMBER_OF_NODES = 5;
	public static final int MAX_TOKEN_AUTOMATIC_ASSOCIATIONS = 1000;
	public static final int MAX_MEMO_BYTES = 100;

	// endregion

	// region FILES
	public static final String DOCUMENTS_FOLDER =
			System.getProperty("user.home") + File.separator + "Documents" + File.separator;
	public static final String DEFAULT_STORAGE = DOCUMENTS_FOLDER + "TransactionTools" + File.separator;
	public static final String INITIAL_MAP_LOCATION = DOCUMENTS_FOLDER + "initialMap.json";
	public static final String USER_PROPERTIES = "Files/user.properties";
	public static final String DEFAULT_HISTORY = DEFAULT_STORAGE + "History";
	public static final String DEFAULT_RECEIPTS = DEFAULT_STORAGE + "Receipts";
	public static final String DEFAULT_DELETED_ACCOUNTS = DEFAULT_STORAGE + "Deleted/Accounts";
	public static final String DEFAULT_ACCOUNTS = DEFAULT_STORAGE + "Accounts";
	public static final String DEFAULT_KEYS = DEFAULT_STORAGE + "Keys";
	public static final String DEFAULT_LOGS = DEFAULT_STORAGE + "logs";
	public static final String USER_PREFERENCE_FILE = "user.pref.xml";
	public static final String MNEMONIC_PATH = "Files/.System/recovery.aes";
	public static final String ACCOUNTS_MAP_FILE = DEFAULT_STORAGE + "Files/.System/accountMapFile.json";
	public static final String ACCOUNTS_INFO_FOLDER = DEFAULT_STORAGE + "Accounts/";
	public static final String KEYS_FOLDER = DEFAULT_STORAGE + "Keys/";
	public static final String DEFAULT_SYSTEM_FOLDER = DEFAULT_STORAGE + "Files/.System/";
	public static final String CUSTOM_NETWORK_FOLDER = DEFAULT_SYSTEM_FOLDER + "CustomNetworks";
	public static final String BALANCES_FILE = DEFAULT_SYSTEM_FOLDER + "balancesArray.json";
	public static final String INTEGRATION_NODES_JSON = "src/main/resources/IntegrationNodes.json";
	public static final String PUBLIC_KEY_LOCATION = "Files/.System/gpgPublicKey.asc";
	public static final String TEMP_FOLDER_LOCATION = System.getProperty("java.io.tmpdir");
	public static final String HISTORY_MAP_JSON = "historyMap.json";
	public static final String HISTORY_MAP = DEFAULT_SYSTEM_FOLDER + File.separator + HISTORY_MAP_JSON;

	// endregion

	// region STYLE
	public static final String MENU_BUTTON_HIGHLIGHT_COLOR =
			"-fx-background-color:  #f4f4f4; -fx-border-color:  #f4f4f4";
	public static final String WHITE_BUTTON_STYLE =
			"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: 10; " +
					"-fx-background-radius: 10;";
	public static final String DEBIT = "-fx-text-fill:RED";
	public static final String CREDIT = "-fx-text-fill:GREEN";

	public static final String HISTORY_BOX_STYLE =
			"-fx-background-color: aliceblue; -fx-border-color: grey; -fx-background-radius: 15; " +
					"-fx-border-radius: 15; -fx-border-width: 2";
	public static final String REGULAR_BOX_STYLE =
			"-fx-background-color: white; -fx-border-color: lightgrey; -fx-background-radius: 15; " +
					"-fx-border-radius: 15; -fx-border-width: 2";
	public static final String BLUE_BUTTON_STYLE =
			"-fx-background-color: #0b9dfd; -fx-border-color: #0b9dfd; -fx-text-fill: white; -fx-border-radius: " +
					"10; -fx-background-radius: 10;";


	// endregion

	// region Encryption Parameters
	public static final int GCM_IV_LENGTH = 12;
	public static final int GCM_TAG_LENGTH = 16;
	public static final int SALT_LENGTH = 16;
	public static final int PBKDF2_ITERATION_COUNT = 65536;
	public static final int KEY_LENGTH = 256;
	// endregion

	// regex
	public static final String FULL_ACCOUNT_CHECKSUM_REGEX = "\\d+.\\d+.\\d+-[a-z]{5}";
	public static final String FULL_ACCOUNT_REGEX = "^\\d+.\\d+.\\d+$";
	public static final String NUMBER_REGEX = "^[0-9]+$";


	public static final long TEST_EXPIRATION_TIME = 10L;


	public static final int LIMIT = 255;
	public static final double FIXED_CELL_SIZE = 30;
	public static final String SELECT_STRING = "SELECT";
	public static final String REGEX = "[^\\d]";
	public static final String TEMP_DIRECTORY = System.getProperty("java.io.tmpdir");
	public static final String START_STYLE = "-fx-background-radius: 10; -fx-border-radius: 10;";
	public static final String MENU_BUTTON_STYLE =
			"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; -fx-border-radius: 10; " +
					"-fx-background-radius: 10;";
	public static final String FILE_ID_PROPERTIES = "fileID";
	public static final String FILENAME_PROPERTY = "filename";
	public static final String FEE_PAYER_ACCOUNT_ID_PROPERTY = "feePayerAccountId";
	public static final String NODE_ID_PROPERTIES = "nodeID";
	public static final String CHUNK_SIZE_PROPERTIES = "chunkSize";
	public static final String FIRST_TRANSACTION_VALID_START_PROPERTY = "firsTransactionValidStart";
	public static final String VALID_INCREMENT_PROPERTY = "validIncrement";
	public static final String TRANSACTION_VALID_DURATION_PROPERTY = "transactionValidDuration";
	public static final String MEMO_PROPERTY = "memo";
	public static final String TRANSACTION_FEE_PROPERTY = "transactionFee";
	public static final String TEXTFIELD_ERROR = "-fx-text-fill: red; -fx-background-radius: 10;-fx-border-radius: 10";
	public static final String TEXTFIELD_DEFAULT =
			"-fx-text-fill: black; -fx-background-radius: 10;-fx-border-radius: 10";
	public static final String SELECT_FREEZE_TYPE = "SELECT FREEZE TYPE";
	public static final String ACCOUNT_PARSED = "Account {} parsed";
	public static final String REMAINING_TIME_MESSAGE =
			"The transaction will expire in %d seconds. This might not be enough time to sign, " +
					"collate, and submit it";
	public static final String NINE_ZEROS = "000000000";
	public static final String FREEZE_AND_UPGRADE = "Freeze and upgrade";
	public static final int MEMO_LENGTH = 99;

	public static final String BUTTON_CONTINUE = "CONTINUE";
	public static final String BUTTON_CANCEL = "CANCEL";
	public static final String BUTTON_ADD = "ADD MORE";

}
