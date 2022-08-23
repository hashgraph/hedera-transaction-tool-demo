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

package com.hedera.hashgraph.client.core.remote;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.constants.StyleConstants;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.remote.helpers.BatchLine;
import com.hedera.hashgraph.client.core.remote.helpers.DistributionMaker;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.ProgressPopup;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.concurrent.Task;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.awt.Desktop;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.security.KeyPair;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.MAX_NUMBER_OF_NODES;
import static com.hedera.hashgraph.client.core.constants.Constants.TEMP_FOLDER_LOCATION;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.constants.Constants.VAL_NUM_TRANSACTION_DEFAULT_FEE;
import static com.hedera.hashgraph.client.core.constants.Constants.VAL_NUM_TRANSACTION_VALID_DURATION;
import static com.hedera.hashgraph.client.core.enums.FileActions.ADD_MORE;
import static com.hedera.hashgraph.client.core.enums.FileActions.BROWSE;
import static com.hedera.hashgraph.client.core.enums.FileActions.DECLINE;
import static com.hedera.hashgraph.client.core.enums.FileActions.SIGN;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.getTimeLabel;

public class BatchFile extends RemoteFile {

	private static final Logger logger = LogManager.getLogger(BatchFile.class);
	public static final int SORTING_CONSTANT = 10000;
	private static final String SENDING_TIME = "sending time";
	private static final String SENDER_ACCOUNT = "sender account";
	private static final String NODE_IDS = "node ids";
	private static final String TRANSACTION_FEE = "transaction fee";
	private static final String DURATION = "transaction valid duration";
	public static final String ACCOUNT_ID = "account";
	private static final String FEE_PAYER_ACCOUNT = "fee payer account";
	private static final String MEMO_STRING = "memo";

	private static final int LEFT = 0;
	private static final int RIGHT = 1;
	public static final String NUMBER_OF_FIELDS = "Number of fields";


	private final UserAccessibleProperties properties =
			new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");

	private Identifier senderAccountID;
	private Identifier feePayerAccountID;
	private List<Identifier> nodeAccountID;
	private int hoursUTC;
	private int minutesUTC;
	private LocalDate firstTransaction;
	private List<BatchLine> transfers = new ArrayList<>();
	private String memo = "";
	private long transactionFee = VAL_NUM_TRANSACTION_DEFAULT_FEE;// default value
	private long txValidDuration = VAL_NUM_TRANSACTION_VALID_DURATION; // default value

	private final List<FileActions> actions = Arrays.asList(SIGN, DECLINE, ADD_MORE, BROWSE);

	public BatchFile() {
		super();
	}

	public BatchFile(final FileDetails fileDetails) {
		super(fileDetails);

		if (!isValid() || !FileType.BATCH.equals(getType())) {
			setValid(false);
			return;
		}
		final List<String> csvList;
		try {
			csvList = readCSVFromFile(new File(getParentPath(), getName()));
		} catch (final Exception e) {
			logger.error("Unable to parse: {}", e.getMessage());
			setValid(false);
			return;
		}

		// Sender line
		if (checkSender(csvList)) {
			return;
		}

		if (checkFeePayer(csvList)) {
			return;
		}

		if (checkMemo(csvList)) {
			return;
		}

		// Submission-time line
		if (checkTime(csvList)) {
			return;
		}

		// Nodes line
		if (checkNodes(csvList)) {
			return;
		}

		// Transaction fee line (optional: if missing use the app default)
		if (checkFee(csvList)) {
			return;
		}

		// Transaction valid duration line (optional: if missing uses the app default)
		if (checkTransactionValidDuration(csvList)) {
			return;
		}

		// Parse transfers
		checkTransfers(csvList);

		setShowAdditionalBoxes();
	}

	/**
	 * Finds the index of the beginning of the distributions
	 *
	 * @param csvList
	 * 		a list of strings that have been read from a batch file
	 * @return the index of the first line that contains a distribution
	 */
	private int getDistributionStart(final List<String> csvList) {
		var count = 0;
		for (final var s : csvList) {
			count++;
			if (s.toLowerCase(Locale.ROOT).startsWith(ACCOUNT_ID)) {
				break;
			}
		}
		return count;
	}

	/**
	 * Checks the sender line of the csv file
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the sender line is invalid
	 */
	private boolean checkSender(final List<String> csvList) {
		try {
			final var senderIDLine = getStrings(csvList, SENDER_ACCOUNT, "[,]", true);

			if (senderIDLine.length != 2) {
				errorBehavior(getName(), NUMBER_OF_FIELDS, SENDER_ACCOUNT);
				return true;
			}
			this.senderAccountID = Identifier.parse(senderIDLine[1]);
		} catch (final Exception e) {
			errorBehavior(getName(), e.getMessage(), SENDER_ACCOUNT);
			setValid(false);
			return true;
		}
		return false;
	}

	/**
	 * Checks the fee payer line of the csv file. If the line does not exist, uses the sender as fee payer
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the sender line is invalid
	 */
	private boolean checkFeePayer(final List<String> csvList) {
		try {
			final var feePayerIDLine = getStrings(csvList, FEE_PAYER_ACCOUNT, "[,]", true);

			if (feePayerIDLine.length == 0) {
				this.feePayerAccountID = this.senderAccountID;
				return false;
			}

			if (feePayerIDLine.length != 2) {
				errorBehavior(getName(), NUMBER_OF_FIELDS, FEE_PAYER_ACCOUNT);
				return true;
			}
			this.feePayerAccountID = Identifier.parse(feePayerIDLine[1]);
		} catch (final Exception e) {
			errorBehavior(getName(), e.getMessage(), FEE_PAYER_ACCOUNT);
			setValid(false);
			return true;
		}
		return false;
	}

	/**
	 * Checks the memo line of the csv file. If the line does not exist, the memo is empty
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the sender line is invalid
	 */
	private boolean checkMemo(final List<String> csvList) {
		try {
			final var memoLine = getStrings(csvList, MEMO_STRING, "[,]", false);

			if (memoLine.length == 0) {
				return false;
			}

			if (memoLine.length != 2) {
				errorBehavior(getName(), NUMBER_OF_FIELDS, MEMO_STRING);
				return true;
			}
			this.memo = memoLine[1];
		} catch (final Exception e) {
			errorBehavior(getName(), e.getMessage(), MEMO_STRING);
			setValid(false);
			return true;
		}
		return false;
	}

	/**
	 * Checks the time line of the csv file
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the time line is invalid
	 */
	private boolean checkTime(final List<String> csvList) {
		try {
			final var timeLine = getStrings(csvList, SENDING_TIME, "[,:]", true);
			if (timeLine.length != 3) {
				errorBehavior(getName(), NUMBER_OF_FIELDS, "time");
				return true;
			}
			this.hoursUTC = Integer.parseInt(timeLine[1]);
			if (hoursUTC > 23 || hoursUTC < 0) {
				timeErrorBehavior("hours", hoursUTC);
				return true;
			}

			this.minutesUTC = Integer.parseInt(timeLine[2]);
			if (minutesUTC > 59 || minutesUTC < 0) {
				timeErrorBehavior("minutes", minutesUTC);
				return true;
			}
		} catch (final NumberFormatException e) {
			errorBehavior(getName(), "time", "Number format");
			return true;
		}
		return false;
	}

	/**
	 * Checks the nodes line of the csv file
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the node line is invalid
	 */
	private boolean checkNodes(final List<String> csvList) {
		nodeAccountID = new ArrayList<>();
		try {
			final var nodeAccountIDLine = getStrings(csvList, NODE_IDS, "[,]", true);

			if (nodeAccountIDLine.length == 0) {
				setValid(false);
				errorBehavior(getName(), "missing node line", "nodes");
			}

			nodeAccountID = new ArrayList<>();
			for (var i = 1; i < nodeAccountIDLine.length; i++) {
				if (!nodeAccountID.contains(Identifier.parse(nodeAccountIDLine[i]))) {
					nodeAccountID.add(Identifier.parse(nodeAccountIDLine[i]));
				}
			}

			if (nodeAccountID.size() > MAX_NUMBER_OF_NODES) {
				logger.error("{} exceeds the maximum number of nodes allowed (Max = {})", nodeAccountID.size(),
						MAX_NUMBER_OF_NODES);
				setValid(false);
				return true;
			}
		} catch (final Exception e) {
			errorBehavior(getName(), "nodes", e.getLocalizedMessage());
			setValid(false);
			return true;
		}
		return false;
	}

	/**
	 * Checks the transaction fee line of the csv file
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the node line is invalid
	 */
	private boolean checkFee(final List<String> csvList) {
		try {
			final var feeLine = getStrings(csvList, TRANSACTION_FEE, "[,]", true);
			// If there is no transaction fee line, just use the default;
			if (feeLine.length == 0) {
				this.transactionFee = properties.getDefaultTxFee();
				return false;
			}
			if (feeLine.length != 2) {
				errorBehavior(getName(), NUMBER_OF_FIELDS, TRANSACTION_FEE);
				return true;
			}
			this.transactionFee = Long.parseLong(feeLine[1]);
		} catch (final Exception e) {
			errorBehavior(getName(), e.getMessage(), TRANSACTION_FEE);
			setValid(false);
			return true;
		}
		return false;
	}

	/**
	 * Checks the transaction valid duration line of the csv file
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the node line is invalid
	 */
	private boolean checkTransactionValidDuration(final List<String> csvList) {
		try {
			final var tvdLine = getStrings(csvList, DURATION, "[,]", true);
			// If there is no transaction valid duration line, just use the default;
			if (tvdLine.length == 0) {
				this.txValidDuration = properties.getTxValidDuration();
				return false;
			}
			if (tvdLine.length != 2) {
				errorBehavior(getName(), NUMBER_OF_FIELDS, DURATION);
				return true;
			}
			this.txValidDuration = Long.parseLong(tvdLine[1]);
		} catch (final Exception e) {
			errorBehavior(getName(), e.getMessage(), DURATION);
			setValid(false);
			return true;
		}
		return false;
	}

	/**
	 * Checks the transfer lines of the csv file
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 */
	private void checkTransfers(final List<String> csvList) {
		final var distributionStart = getDistributionStart(csvList);
		if (distributionStart == csvList.size()) {
			logger.error("Cannot find the start of the distribution");
			setValid(false);
			return;
		}

		try {
			transfers = new ArrayList<>();

			for (var i = distributionStart; i < csvList.size(); i++) {
				transfers.add(BatchLine.parse(csvList.get(i), hoursUTC, minutesUTC));
			}
			this.firstTransaction = getFirstDate();
		} catch (final Exception e) {
			logger.error(e);
			logger.error("Unable to parse transfers");
			setValid(false);
		}
		Collections.sort(transfers);
		transfers = dedup(transfers);
	}

	/**
	 * Finds and parses a line based on a query string
	 *
	 * @param csvList
	 * 		a list of strings
	 * @param queryString
	 * 		the string that the line must start with
	 * @param regex
	 * 		a regex string that determines how the line will be parsed
	 * @return an array of strings
	 */
	@NotNull
	private String[] getStrings(final List<String> csvList, final String queryString, final String regex,
			final boolean removeSpaces) {
		var strings = new String[0];
		final var query = removeSpaces ? queryString.replace(" ", "") : queryString;
		for (final var p : csvList) {
			final var s = removeSpaces ? p.replace(" ", "") : p;
			if (s.toLowerCase(Locale.ROOT).startsWith(query)) {
				strings = s.split(regex);
				final var returnString = new String[strings.length];
				// trim leading and trailing spaces
				for (int i = 0, stringsLength = strings.length; i < stringsLength; i++) {
					returnString[i] = strings[i].trim();
				}
				return returnString;
			}
		}
		return strings;
	}

	/**
	 * Logs a message if an error is detected in one of the time fields
	 *
	 * @param fieldName
	 * 		the field that has an error
	 * @param field
	 * 		the value of the field
	 */
	private void timeErrorBehavior(final String fieldName, final int field) {
		logger.error("Invalid {} field: {}", fieldName, field);
		setValid(false);
	}

	/**
	 * Logs an error if there is an error in a field of the csv
	 *
	 * @param filename
	 * 		the name of the file
	 * @param message
	 * 		the message
	 * @param field
	 * 		the field
	 */
	private void errorBehavior(final String filename, final String message, final String field) {
		logger.error("Incorrect {} in csv file ({}): File {} will not be displayed", field, message,
				filename);
		setValid(false);
	}

	/**
	 * Makes sure all transfers have a different transaction ID
	 *
	 * @param transfers
	 * 		the list of read transfers
	 * @return a list of transfers with distinct transaction valid starts
	 */
	private List<BatchLine> dedup(final List<BatchLine> transfers) {
		final Set<Timestamp> dedupSet = new HashSet<>();
		final List<BatchLine> newTransfers = new ArrayList<>();
		for (final var transfer : transfers) {
			var timestamp = transfer.getDate();
			final var seconds = timestamp.getSeconds();
			var nanos = timestamp.getNanos(); // added a fixed amount to make sorting by time easier
			timestamp = new Timestamp(seconds, nanos);
			while (dedupSet.contains(timestamp)) {
				timestamp = new Timestamp(seconds, (++nanos) * SORTING_CONSTANT);
			}
			dedupSet.add(timestamp);
			transfer.setDate(timestamp);
			newTransfers.add(transfer);
		}
		return newTransfers;
	}

	/**
	 * Reads a CSV file into a list of strings
	 *
	 * @param csvFile
	 * 		CSV file
	 * @return List of Strings
	 * @throws HederaClientException
	 * 		if there are IO exceptions
	 */
	private static List<String> readCSVFromFile(final File csvFile) throws HederaClientException {
		final List<String> csvList = new ArrayList<>();
		try (final var bufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(csvFile)))) {
			String sCurrentLine;
			while ((sCurrentLine = bufferedReader.readLine()) != null) {
				if (sCurrentLine.equals("")) {
					continue;
				}
				csvList.add(sCurrentLine.replace("\"", ""));
			}
		} catch (final IOException cause) {
			throw new HederaClientException(cause);
		}

		return csvList;
	}

	/**
	 * Get the date of the first transaction
	 */
	private LocalDate getFirstDate() {
		var first = Long.MAX_VALUE;
		for (final var b : transfers) {
			final var seconds = b.getDate().getSeconds();
			if (seconds < first) {
				first = seconds;
			}
		}
		if (Long.MAX_VALUE == first) {
			first = 0;
		}
		final var instant = new Timestamp(first, 0).asInstant();
		return LocalDate.ofInstant(instant, ZoneId.of("UTC"));
	}

	public Identifier getSenderAccountID() {
		return senderAccountID;
	}

	public Identifier getFeePayerAccountID() {
		return feePayerAccountID;
	}

	public String getMemo() {
		return memo;
	}

	public List<Identifier> getNodeAccountID() {
		return nodeAccountID;
	}

	public int getHoursUTC() {
		return hoursUTC;
	}

	public int getMinutesUTC() {
		return minutesUTC;
	}

	public LocalDate getFirstTransaction() {
		return firstTransaction;
	}

	public List<BatchLine> getTransfers() {
		return transfers;
	}

	public Timestamp getFirstTransactionTimeStamp() {
		final var localTime = LocalTime.of(getHoursUTC(), getMinutesUTC());
		final var localDateTime = LocalDateTime.of(getFirstDate(), localTime);
		return new Timestamp(localDateTime.atZone(ZoneId.of("UTC")).toInstant());

	}

	public long getTransactionFee() {
		return transactionFee;
	}

	public void setTransactionFee(final long transactionFee) {
		if (transactionFee < 0) {
			return;
		}
		this.transactionFee = transactionFee;
	}

	public void setTxValidDuration(long txValidDuration) {
		if (txValidDuration <= 0 || txValidDuration > VAL_NUM_TRANSACTION_VALID_DURATION) {
			txValidDuration = VAL_NUM_TRANSACTION_VALID_DURATION;
		}
		this.txValidDuration = txValidDuration;
	}

	public long getTxValidDuration() {
		return txValidDuration;
	}

	@Override
	public List<FileActions> getActions() {
		return actions;
	}

	@Override
	public Set<AccountId> getSigningAccounts() {
		return new HashSet<>(Collections.singleton(getSenderAccountID().asAccount()));
	}

	@Override
	public GridPane buildGridPane() {
		final var detailsGridPane = new GridPane();
		var row = 0;
		try {
			final var accounts = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(
					ACCOUNTS_MAP_FILE) : new JsonObject();
			detailsGridPane.add(new Label("Sender account: "), LEFT, row);
			detailsGridPane.add(
					new Label(CommonMethods.nicknameOrNumber(getSenderAccountID(), accounts)), RIGHT, row++);
			detailsGridPane.add(new Label("Fee payer account"), LEFT, row);
			detailsGridPane.add(new Label(CommonMethods.nicknameOrNumber(getFeePayerAccountID(), accounts)), RIGHT,
					row++);
		} catch (final HederaClientException e) {
			logger.error(e);
		}

		final var feeLabel = new Label("Single transaction maximum fee: ");
		feeLabel.setWrapText(true);
		detailsGridPane.add(feeLabel, LEFT, row);

		final var actualFeeLabel = new Label(Hbar.fromTinybars(getTransactionFee()).toString());
		actualFeeLabel.setWrapText(true);
		actualFeeLabel.setStyle(StyleConstants.DEBIT);
		detailsGridPane.add(actualFeeLabel, RIGHT, row++);

		final var firstTransactionLabel = new Label("First transaction date: ");
		firstTransactionLabel.setWrapText(true);
		firstTransactionLabel.setAlignment(Pos.CENTER_LEFT);
		detailsGridPane.add(firstTransactionLabel, LEFT, row);


		final var timestamp = getFirstTransactionTimeStamp();

		final var utcTime = getTimeLabel(timestamp, true);
		utcTime.setWrapText(true);
		detailsGridPane.add(utcTime, RIGHT, row++);

		final var utcLabel = new Label("All transactions will be sent at: ");
		detailsGridPane.add(utcLabel, LEFT, row);
		utcLabel.setWrapText(true);
		detailsGridPane.add(getTimeLabel(timestamp, false), RIGHT, row++);

		final var tvDuration = new Label("Transactions duration: ");
		detailsGridPane.add(tvDuration, LEFT, row);
		tvDuration.setWrapText(true);
		detailsGridPane.add(new Label(String.format("%d seconds", getTxValidDuration())), RIGHT, row++);

		final var nLabel = new Label("Transactions will be submitted to nodes: ");
		nLabel.setWrapText(true);
		detailsGridPane.add(nLabel, LEFT, row);

		JsonObject nicknames;
		try {
			nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (final HederaClientException e) {
			logger.error(e);
			nicknames = new JsonObject();
		}

		final var nodesString = new StringBuilder();
		for (final var n : getNodeAccountID()) {
			nodesString.append(n.toNicknameAndChecksum(nicknames)).append("\n");
		}

		detailsGridPane.add(new Label(nodesString.toString()), RIGHT, row++);

		if (!"".equals(getMemo())) {
			detailsGridPane.add(new Label("Memo: "), LEFT, row);
			final var memoLabel = new Label(getMemo());
			memoLabel.setWrapText(true);
			detailsGridPane.add(memoLabel, RIGHT, row++);
		}

		detailsGridPane.add(new Label("More details: "), LEFT, row);
		final var hyperlink = new Hyperlink("Click to open CSV file");
		hyperlink.setOnAction(actionEvent -> {
			try {
				Desktop.getDesktop().open(new File(getPath()));
			} catch (final IOException e) {
				logger.error(e);
			}
		});
		detailsGridPane.add(hyperlink, RIGHT, row);

		final var cc1 = new ColumnConstraints();
		cc1.prefWidthProperty().bind(detailsGridPane.widthProperty().divide(5).multiply(2));
		final var cc2 = new ColumnConstraints();
		cc2.prefWidthProperty().bind(detailsGridPane.widthProperty().divide(5).multiply(3));
		detailsGridPane.getColumnConstraints().addAll(cc1, cc2);
		detailsGridPane.setHgap(20);
		detailsGridPane.setVgap(10);


		return detailsGridPane;
	}

	@Override
	public String execute(final Pair<String, KeyPair> pair, final String user,
			final String output) throws HederaClientException {
		final var tempStorage =
				new File(TEMP_FOLDER_LOCATION,
						LocalDate.now().toString()).getAbsolutePath() + File.separator + RandomStringUtils.randomAlphanumeric(
						5) + File.separator + "Batch" + File.separator + FilenameUtils.getBaseName(
						pair.getLeft()) + File.separator;
		final DoubleProperty progress = new SimpleDoubleProperty(1);

		try {
			moveToHistory(Actions.ACCEPT, getCommentArea().getText(), pair.getLeft());
			setHistory(true);
		} catch (final HederaClientException e) {
			logger.error(e);
		}
		try {
			if (new File(tempStorage).exists()) {
				FileUtils.deleteDirectory(new File(tempStorage));
			}
		} catch (final IOException e) {
			logger.error(e);
			throw new HederaClientException(e);
		}

		final var transactionsProgressBar = new ProgressBar();
		transactionsProgressBar.progressProperty().bind(progress);
		transactionsProgressBar.setVisible(true);
		final var compressingProgressBar = new ProgressBar();
		compressingProgressBar.setProgress(-1);

		final var cancelButton = new Button("CANCEL");
		final var window =
				ProgressPopup.setupProgressPopup(transactionsProgressBar, compressingProgressBar, cancelButton);

		cancelButton.visibleProperty().bind(transactionsProgressBar.progressProperty().lessThan(1));

		for (final var nodeID : getNodeAccountID()) {
			final var storageLocation =
					tempStorage + "/" + getName().replace(".csv", "_") + FilenameUtils.getBaseName(
							pair.getLeft()) + "_Node-" + nodeID.toReadableString().replace(".", "-");
			final var maker = new DistributionMaker(getSenderAccountID().asAccount(),
					feePayerAccountID.asAccount(),
					nodeID.asAccount(),
					new Timestamp(getTxValidDuration(), 0),
					getTransactionFee(),
					memo,
					storageLocation,
					output + File.separator + user);


			final Task<Void> task = new Task<>() {
				// Create transactions and signatures
				@Override
				public Void call() {
					final var max = transfers.size();
					var i = 0;
					for (final var transfer : transfers) {
						try {
							maker.buildBundle(transfer, pair.getValue());
						} catch (final HederaClientException e) {
							logger.error(e);
						}
						updateProgress(++i, (long) max - 1);
					}
					transactionsProgressBar.setVisible(false);
					try {
						maker.pack();
					} catch (final HederaClientException e) {
						logger.error(e);
					}
					return null;
				}
			};

			transactionsProgressBar.progressProperty().bind(task.progressProperty());
			new Thread(task).start();

			task.setOnSucceeded(workerStateEvent -> window.close());


			cancelButton.setOnAction(actionEvent -> task.cancel());

			task.setOnCancelled(workerStateEvent -> {
				logger.info("Task interrupted");
				window.close();
			});
		}
		return "";
	}

	@Override
	public boolean isExpired() {
		return getFirstTransactionTimeStamp().asCalendar().before(Calendar.getInstance());
	}

	@Override
	public JsonObject toJson() {
		final var toJson = super.toJson();
		final var nodes = new JsonArray();
		nodeAccountID.stream().map(Identifier::asJSON).forEach(nodes::add);
		final var lines = new JsonArray();
		transfers.stream().map(BatchLine::asJSON).forEachOrdered(lines::add);

		toJson.add("senderAccountID", senderAccountID.asJSON());
		toJson.add("feePayerAccountID", feePayerAccountID.asJSON());
		toJson.add("nodeAccountID", nodes);
		toJson.addProperty("hoursUTC", hoursUTC);
		toJson.addProperty("minutesUTC", minutesUTC);
		toJson.add("transfers", lines);
		toJson.addProperty("memo", memo);

		return toJson;
	}
}
