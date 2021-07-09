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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.BatchLine;
import com.hedera.hashgraph.client.core.remote.helpers.DistributionMaker;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.concurrent.Task;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.MAX_NUMBER_OF_NODES;
import static com.hedera.hashgraph.client.core.constants.Constants.TEMP_FOLDER_LOCATION;
import static com.hedera.hashgraph.client.core.constants.Constants.VAL_NUM_TRANSACTION_DEFAULT_FEE;
import static com.hedera.hashgraph.client.core.constants.Constants.VAL_NUM_TRANSACTION_VALID_DURATION;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.enums.FileActions.ADD_MORE;
import static com.hedera.hashgraph.client.core.enums.FileActions.BROWSE;
import static com.hedera.hashgraph.client.core.enums.FileActions.DECLINE;
import static com.hedera.hashgraph.client.core.enums.FileActions.SIGN;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.getTimeLabel;

public class BatchFile extends RemoteFile {

	private static final Logger logger = LogManager.getLogger(BatchFile.class);
	public static final int SORTING_CONSTANT = 10000;


	private Identifier senderAccountID;
	private List<Identifier> nodeAccountID;
	private int hoursUTC;
	private int minutesUTC;
	private LocalDate firstTransaction;
	private List<BatchLine> transfers;
	private long transactionFee = VAL_NUM_TRANSACTION_DEFAULT_FEE;// default value
	private long txValidDuration = VAL_NUM_TRANSACTION_VALID_DURATION; // default value

	private final List<FileActions> actions = Arrays.asList(SIGN, DECLINE, ADD_MORE, BROWSE);

	public BatchFile() {
		super();
	}

	public BatchFile(FileDetails fileDetails) {
		super(fileDetails);

		if (!isValid() || !FileType.BATCH.equals(getType())) {
			setValid(false);
			return;
		}
		List<String> csvList;
		try {
			csvList = readCSVFromFile(new File(getParentPath(), getName()));
		} catch (Exception e) {
			logger.error("Unable to parse: {}", e.getMessage());
			setValid(false);
			return;
		}

		// Sender line
		if (checkSender(fileDetails, csvList)) {
			return;
		}

		// Submission time line
		if (checkTime(fileDetails, csvList)) {
			return;
		}

		// Nodes line
		if (checkNodes(csvList)) {
			return;
		}
		checkTransfers(csvList);
	}

	/**
	 * Checks the sender line of the csv file
	 *
	 * @param fileDetails
	 * 		the details of the file
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the sender line is invalid
	 */
	private boolean checkSender(FileDetails fileDetails, List<String> csvList) {
		try {
			var senderIDLine = csvList.get(0).replace(" ", "").split("[,]");
			if (senderIDLine.length != 2) {
				errorBehavior(fileDetails, "Number of fields", "sender ID");
				return true;
			}
			this.senderAccountID = Identifier.parse(senderIDLine[1]);
		} catch (Exception e) {
			logger.error("Unable to parse: {}", e.getMessage());
			setValid(false);
			return true;
		}
		return false;
	}

	/**
	 * Checks the time line of the csv file
	 *
	 * @param fileDetails
	 * 		the details of the file
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the time line is invalid
	 */
	private boolean checkTime(FileDetails fileDetails, List<String> csvList) {
		try {
			var timeLine = csvList.get(1).replace(" ", "").split("[,:]");
			if (timeLine.length != 3) {
				errorBehavior(fileDetails, "Number of fields", "time");
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
		} catch (NumberFormatException e) {
			errorBehavior(fileDetails, "time", "Number format");
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
	private void checkTransfers(List<String> csvList) {
		try {
			transfers = new ArrayList<>();
			for (var i = 4; i < csvList.size(); i++) {
				transfers.add(BatchLine.parse(csvList.get(i), hoursUTC, minutesUTC));
			}
			this.firstTransaction = getFirstDate();
		} catch (Exception e) {
			logger.error(e);
			logger.error("Unable to parse transfers");
			setValid(false);
		}
		Collections.sort(transfers);
		transfers = dedup(transfers);
	}

	/**
	 * Checks the nodes line of the csv file
	 *
	 * @param csvList
	 * 		an array of strings that contains the csv file
	 * @return true if the node line is invalid
	 */
	private boolean checkNodes(List<String> csvList) {
		nodeAccountID = new ArrayList<>();
		try {
			var nodeAccountIDLine = csvList.get(2).replace(" ", "").split("[,]");

			nodeAccountID = new ArrayList<>();
			for (var i = 1; i < nodeAccountIDLine.length; i++) {
				if (!nodeAccountID.contains(Identifier.parse(nodeAccountIDLine[i]))) {
					nodeAccountID.add((Identifier.parse(nodeAccountIDLine[i])));
				}
			}

			if (nodeAccountID.size() > MAX_NUMBER_OF_NODES) {
				logger.error("{} exceeds the maximum number of nodes allowed (Max = {})", nodeAccountID.size(),
						MAX_NUMBER_OF_NODES);
				setValid(false);
				return true;
			}
		} catch (Exception e) {
			logger.error(CANNOT_PARSE_ERROR_MESSAGE, e.getLocalizedMessage());
			setValid(false);
			return true;
		}
		return false;
	}

	/**
	 * Logs a message if an error is detected in one of the time fields
	 *
	 * @param fieldName
	 * 		the field that has an error
	 * @param field
	 * 		the value of the field
	 */
	private void timeErrorBehavior(String fieldName, int field) {
		logger.error("Invalid {} field: {}", fieldName, field);
		setValid(false);
	}

	/**
	 * Logs an error if there is an error in a field of the csv
	 *
	 * @param fileDetails
	 * 		the details of the file
	 * @param message
	 * 		the message
	 * @param field
	 * 		the field
	 */
	private void errorBehavior(FileDetails fileDetails, String message, String field) {
		logger.error("Incorrect {} in csv file ({}): File {} will not be displayed", field, message,
				fileDetails.getName());
		setValid(false);
	}

	private List<BatchLine> dedup(List<BatchLine> transfers) {
		Set<Timestamp> dedupSet = new HashSet<>();
		List<BatchLine> newTransfers = new ArrayList<>();
		for (var transfer : transfers) {
			var timestamp = transfer.getDate();
			var seconds = timestamp.getSeconds();
			var nanos = timestamp.getNanos() + SORTING_CONSTANT; // added a fixed amount to make sorting by time easier
			timestamp = new Timestamp(seconds, nanos);
			while (dedupSet.contains(timestamp)) {
				timestamp = new Timestamp(seconds, ++nanos);
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
	private static List<String> readCSVFromFile(File csvFile) throws HederaClientException {
		List<String> csvList = new ArrayList<>();
		try (var bufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(csvFile)))) {
			String sCurrentLine;
			while ((sCurrentLine = bufferedReader.readLine()) != null) {
				if (sCurrentLine.equals("")) {
					continue;
				}
				csvList.add(sCurrentLine.replace("\"", ""));
			}
		} catch (IOException cause) {
			throw new HederaClientException(cause);
		}

		return csvList;
	}

	/**
	 * Get the date of the first transaction
	 */
	private LocalDate getFirstDate() {
		var first = Long.MAX_VALUE;
		for (var b : transfers) {
			var seconds = b.getDate().getSeconds();
			if (seconds < first) {
				first = seconds;
			}
		}
		var instant = new Timestamp(first, 0).asInstant();
		return LocalDate.ofInstant(instant, ZoneId.of("UTC"));
	}

	public Identifier getSenderAccountID() {
		return senderAccountID;
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
		var localTime = LocalTime.of(getHoursUTC(), getMinutesUTC());
		var localDateTime = LocalDateTime.of(getFirstDate(), localTime);
		return new Timestamp(localDateTime.atZone(ZoneId.of("UTC")).toInstant());

	}

	public long getTransactionFee() {
		return transactionFee;
	}

	public void setTransactionFee(long transactionFee) {
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
		var detailsGridPane = new GridPane();
		detailsGridPane.add(new Label("Sender account: "), 0, 0);

		try {
			final var accounts = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(
					ACCOUNTS_MAP_FILE) : new JsonObject();
			detailsGridPane.add(
					new Label(CommonMethods.nicknameOrNumber(getSenderAccountID(), accounts)), 1,
					0);
		} catch (HederaClientException e) {
			logger.error(e);
		}
		var firstTransactionLabel = new Label("First transaction date: ");
		firstTransactionLabel.setWrapText(true);
		firstTransactionLabel.setAlignment(Pos.CENTER_LEFT);
		detailsGridPane.add(firstTransactionLabel, 0, 1);

		var timestamp = getFirstTransactionTimeStamp();

		var utcTime = getTimeLabel(timestamp, true);
		utcTime.setWrapText(true);
		detailsGridPane.add(utcTime, 1, 1);

		var utcLabel = new Label("All transactions will be sent at: ");
		detailsGridPane.add(utcLabel, 0, 2);
		utcLabel.setWrapText(true);

		detailsGridPane.add(getTimeLabel(timestamp, false), 1, 2);
		var nLabel = new Label("Transactions will be submitted to nodes: ");
		nLabel.setWrapText(true);
		detailsGridPane.add(nLabel, 0, 3);

		var nodesString = new StringBuilder();
		for (var n : getNodeAccountID()) {
			nodesString.append(n.toReadableString()).append(" ");
		}

		detailsGridPane.add(new Label(nodesString.toString()), 1, 3);
		detailsGridPane.add(new Label("More details: "), 0, 4);
		var hyperlink = new Hyperlink("Click to open CSV file");
		hyperlink.setOnAction(actionEvent -> {
			try {
				Desktop.getDesktop().open(new File(getPath()));
			} catch (IOException e) {
				logger.error(e);
			}
		});
		detailsGridPane.add(hyperlink, 1, 4);

		var cc1 = new ColumnConstraints();
		cc1.prefWidthProperty().bind(detailsGridPane.widthProperty().divide(5).multiply(2));
		var cc2 = new ColumnConstraints();
		cc2.prefWidthProperty().bind(detailsGridPane.widthProperty().divide(5).multiply(3));
		detailsGridPane.getColumnConstraints().addAll(cc1, cc2);
		detailsGridPane.setHgap(20);
		detailsGridPane.setVgap(10);


		return detailsGridPane;
	}

	@Override
	public String execute(Pair<String, KeyPair> pair, String user, String output) throws HederaClientException {
		var tempStorage =
				TEMP_FOLDER_LOCATION + (LocalDate.now()) + File.separator + RandomStringUtils.randomAlphanumeric(
						5) + File.separator + "Batch" + File.separator + FilenameUtils.getBaseName(
						pair.getLeft()) + File.separator;
		DoubleProperty progress = new SimpleDoubleProperty(1);

		try {
			moveToHistory(Actions.ACCEPT, getCommentArea().getText(), pair.getLeft());
			setHistory(true);
		} catch (HederaClientException e) {
			logger.error(e);
		}
		try {
			if (new File(tempStorage).exists()) {
				FileUtils.deleteDirectory(new File(tempStorage));
			}
		} catch (IOException e) {
			logger.error(e);
			throw new HederaClientException(e);
		}

		var transactionsProgressBar = new ProgressBar();
		transactionsProgressBar.progressProperty().bind(progress);
		transactionsProgressBar.setVisible(true);
		var compressingProgressBar = new ProgressBar();
		compressingProgressBar.setProgress(-1);

		var cancelButton = new Button("CANCEL");
		var window = setupProgressPopup(transactionsProgressBar, compressingProgressBar, cancelButton);

		cancelButton.visibleProperty().bind(transactionsProgressBar.progressProperty().lessThan(1));

		for (var nodeID : getNodeAccountID()) {
			var storageLocation =
					tempStorage + "/" + getName().replace(".csv", "_") + FilenameUtils.getBaseName(
							pair.getLeft()) + "_Node-" + nodeID.toReadableString().replace(".", "-");
			var maker = new DistributionMaker(getSenderAccountID().asAccount(), nodeID.asAccount(),
					new Timestamp(getTxValidDuration(), 0), getTransactionFee(), storageLocation,
					output + File.separator + user);


			Task<Void> task = new Task<>() {
				// Create transactions and signatures
				@Override
				public Void call() {
					final var max = transfers.size();
					var i = 0;
					for (var transfer : transfers) {
						try {
							maker.buildBundle(transfer, pair.getValue());
						} catch (HederaClientException e) {
							logger.error(e);
						}
						updateProgress(++i, (long) max - 1);
					}
					transactionsProgressBar.setVisible(false);
					try {
						maker.pack();
					} catch (HederaClientException e) {
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
	public boolean equals(Object o) {
		return super.equals(o);
	}

	private Stage setupProgressPopup(ProgressBar bar, ProgressBar bar2, Button cancelButton) {
		var layout = new VBox();
		layout.setAlignment(Pos.CENTER);
		layout.setSpacing(10);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setMaxWidth(400);

		var window = new Stage();

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle("Batch Transactions");

		window.sizeToScene();
		window.setWidth(450);


		var label1 = new Label();
		label1.setText("Batch Transactions");
		label1.setStyle("-fx-font-size: 20");

		var label2 = new Label(
				"Please wait while the transactions are being created and signed.");
		label2.setWrapText(true);
		label2.setStyle("-fx-font-size: 16");

		var label3 =
				new Label("Transactions are now being compressed and exported to the output folder");
		label3.setWrapText(true);
		label3.setStyle("-fx-font-size: 16");
		label3.setVisible(false);

		var box = new HBox();
		box.setPrefWidth(Region.USE_COMPUTED_SIZE);
		box.setPrefHeight(Region.USE_COMPUTED_SIZE);
		box.setAlignment(Pos.CENTER);
		box.getChildren().addAll(label2, label3);
		label2.managedProperty().bind(label2.visibleProperty());
		label3.managedProperty().bind(label3.visibleProperty());

		cancelButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
						"-fx-border-radius: 10; -fx-background-radius: 10;");
		cancelButton.setMinWidth(200);

		bar.setPrefWidth(375);
		bar2.setPrefWidth(375);

		bar.managedProperty().bind(bar.visibleProperty());
		bar2.managedProperty().bind(bar2.visibleProperty());
		bar2.visibleProperty().bind(bar.visibleProperty().not());

		label2.visibleProperty().bind(bar.visibleProperty());
		label3.visibleProperty().bind(bar.visibleProperty().not());

		layout.getChildren().addAll(label1, box, bar, bar2, cancelButton);

		var scene = new Scene(layout);

		scene.getStylesheets().add("tools.css");

		window.setScene(scene);

		window.show();

		return window;
	}
}
