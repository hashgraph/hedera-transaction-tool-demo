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
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.ProgressPopup;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.transactions.ToolFileAppendTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolFileUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.core.utils.FXUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.FileUpdateTransaction;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import com.opencsv.CSVWriter;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.concurrent.Task;
import javafx.scene.control.Button;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.zeroturnaround.zip.ZipUtil;

import javax.annotation.Nullable;
import java.awt.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyPair;
import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.CONTENT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.FILENAME_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.FILE_NAME_GROUP_SEPARATOR;
import static com.hedera.hashgraph.client.core.constants.Constants.FILE_NAME_INTERNAL_SEPARATOR;
import static com.hedera.hashgraph.client.core.constants.Constants.FIRST_TRANSACTION_VALID_START_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.IS_ZIP_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNATURE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TEMP_DIRECTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.constants.Constants.ZIP_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.CONTENTS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.CONTENT_PROPERTY;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FILE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.getTimeLabel;

public class LargeBinaryFile extends RemoteFile implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(LargeBinaryFile.class);

	private static final String TEMP_LOCATION = TEMP_DIRECTORY + File.separator + "content." + CONTENT_EXTENSION;
	public static final String CHUNK_SIZE_PROPERTY = "chunkSize";
	public static final String VALID_DURATION_PROPERTY = "validDuration";
	public static final String VALID_INCREMENT_PROPERTY = "validIncrement";
	public static final String TRANSACTION_FEE_PROPERTY = "transactionFee";
	public static final String MEMO_PROPERTY = "memo";
	public static final String FILE_ID_PROPERTY = "fileID";
	public static final String NODE_ID_PROPERTY = "nodeID";
	public static final String FEE_PAYER_ACCOUNT_ID_PROPERTY = "feePayerAccountId";

	private final UserAccessibleProperties properties =
			new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");

	private String filename;
	private Identifier fileID;
	private int chunkSize;
	private Identifier feePayerAccountId;
	private Duration transactionValidDuration;
	private Timestamp transactionValidStart;
	private Timestamp expiration;
	private long validIncrement;
	private Identifier nodeID;
	private long transactionFee;
	private String memo;
	private File content = null;
	private boolean isZip;
	private String checksum;

	private final List<FileActions> actions =
			Arrays.asList(FileActions.SIGN, FileActions.DECLINE, FileActions.ADD_MORE, FileActions.BROWSE);

	public LargeBinaryFile() {
		super();
	}

	public LargeBinaryFile(final FileDetails fileDetails) throws HederaClientException {
		super(fileDetails);

		final var destination = new File(TEMP_DIRECTORY, fileDetails.getBaseName()).getAbsolutePath();
		if (new File(destination).exists()) {
			try {
				FileUtils.deleteDirectory(new File(destination));
			} catch (final IOException e) {
				handleError(e.getMessage());
				return;
			}
		}

		try {
			unZip(fileDetails.getFullPath(), destination);
		} catch (final HederaClientException exception) {
			handleError(exception.getMessage());
			return;
		}
		logger.debug("File unzipped");


		// Check input
		final var jsons = new File(destination).listFiles((dir, name) -> name.endsWith(JSON_EXTENSION));
		if (jsons == null) {
			throw new HederaClientRuntimeException("Unable to read json files");
		}

		final var bins = new File(destination).listFiles((dir, name) -> name.endsWith(CONTENT_EXTENSION));
		if (bins == null) {
			throw new HederaClientRuntimeException("Unable to read binary files");
		}

		if (checkFiles(jsons, bins)) {
			return;
		}

		jsons[0].deleteOnExit();
		bins[0].deleteOnExit();
		new File(destination).deleteOnExit();

		final JsonObject details;
		try {
			details = readJsonObject(jsons[0].getPath());
		} catch (final HederaClientException exception) {
			handleError(exception.getMessage());
			return;
		}

		this.filename = details.get(FILENAME_PROPERTY).getAsString();

		// Check the filename sans extension, compared to the actual file name.  The fileName property could have
		// any extension, but the content will always be a zip.
		if (!FilenameUtils.removeExtension(filename).equals(FilenameUtils.removeExtension(bins[0].getName()))) {
			handleError("The binary file does not correspond to the file specified in the details");
			return;
		}

		final var fileIdentifier = getFileIdentifier(details);
		final var nodeIdentifier = getNodeIdentifier(details);
		final var payerIdentifier = getPayerIdentifier(details);
		final var tvStamp = getTransactionValidStamp(details);

		if (checkNotNulls(fileIdentifier, nodeIdentifier, payerIdentifier)) {
			return;
		}
		if (tvStamp == null) {
			return;
		}
		final var timestamp = getTimestamp(tvStamp);
		if (timestamp.equals(new Timestamp(0, 0))) {
			return;
		}

		this.fileID = fileIdentifier;
		this.chunkSize = details.has(CHUNK_SIZE_PROPERTY) ? details.get(CHUNK_SIZE_PROPERTY).getAsInt() : 1024;
		if (getChunkSize() > 1024) {
			throw new HederaClientException("Maximum chunk size is 1024 for unsigned file update transactions.");
		}
		this.feePayerAccountId = payerIdentifier;
		this.transactionValidDuration =
				Duration.ofSeconds(
						details.has(VALID_DURATION_PROPERTY) ? details.get(VALID_DURATION_PROPERTY).getAsLong() : 120);
		this.transactionValidStart = timestamp;
		// As long as appends have to wait for the previous transaction to finish, then this increment should be larger
		// than nanos.
		this.validIncrement = details.has(VALID_INCREMENT_PROPERTY) ?
				details.get(VALID_INCREMENT_PROPERTY).getAsLong() : 10L;
		this.nodeID = nodeIdentifier;
		this.transactionFee = details.has(TRANSACTION_FEE_PROPERTY) ?
				details.get(TRANSACTION_FEE_PROPERTY).getAsLong() : properties.getDefaultTxFee();
		this.memo = details.has(MEMO_PROPERTY) ? details.get(MEMO_PROPERTY).getAsString() : "";
		this.content = bins[0];
		// For backwards compatibility, if the zip property is not there, set it to true to indicate that the user
		// did manually zip the files beforehand as this tool only allowed zip files previously.
		this.isZip = details.has(IS_ZIP_PROPERTY) ? details.get(IS_ZIP_PROPERTY).getAsBoolean() : true;

		expiration = timestamp.plusSeconds(transactionValidDuration.getSeconds())
				.plusNanos(transactionValidDuration.getNano());

		setShowAdditionalBoxes();
	}

	private boolean checkFiles(final File[] jsons, final File[] bins) {
		var checkFiles = false;
		if (jsons.length != 1) {
			final var formattedError =
					String.format("There should be exactly one json file in zip archive. We found: %d", jsons.length);
			handleError(formattedError);
			checkFiles = true;
		}

		if (bins.length != 1) {
			final var formattedError =
					String.format("There should be exactly one binary file in the zip archive. We found: %d",
							bins.length);
			handleError(formattedError);
			checkFiles = true;
		}
		return checkFiles;
	}

	private boolean checkNotNulls(final Object... ids) {
		return Arrays.stream(ids).anyMatch(Objects::isNull);
	}

	public Identifier getFeePayerAccountId() {
		return feePayerAccountId;
	}

	/**
	 * Returns the file id
	 *
	 * @param details
	 * 		the json read from the zip provided by the user
	 * @return the file id if it exists and is correct, Null otherwise.
	 */
	@Nullable
	private Identifier getFileIdentifier(final JsonObject details) {
		final Identifier fileIdentifier;
		if (!details.has(FILE_ID_PROPERTY)) {
			handleError("Missing file ID in details file");
			return null;
		}
		final var fileJson = details.getAsJsonObject(FILE_ID_PROPERTY);

		try {
			fileIdentifier = Identifier.parse(fileJson);
		} catch (final Exception exception) {
			handleError(exception.getMessage());
			return null;
		}
		return fileIdentifier;
	}

	/**
	 * Returns the node id
	 *
	 * @param details
	 * 		the json read from the zip provided by the user
	 * @return the node id if it exists and is correct, Null otherwise.
	 */
	@Nullable
	private Identifier getNodeIdentifier(final JsonObject details) {
		final Identifier nodeIdentifier;
		if (!details.has(NODE_ID_PROPERTY)) {
			handleError("Missing node ID in details file");
			return null;
		}

		final var nodeJson = details.getAsJsonObject(NODE_ID_PROPERTY);

		try {
			nodeIdentifier = Identifier.parse(nodeJson);
		} catch (final Exception exception) {
			handleError(exception.getMessage());
			return null;
		}
		return nodeIdentifier;
	}

	/**
	 * Returns the fee payer id
	 *
	 * @param details
	 * 		the json read from the zip provided by the user
	 * @return the fee payer id if it exists and is correct, Null otherwise.
	 */
	@Nullable
	private Identifier getPayerIdentifier(final JsonObject details) {
		Identifier payerIdentifier = null;
		if (!details.has(FEE_PAYER_ACCOUNT_ID_PROPERTY)) {
			handleError("Missing fee payer ID in details file");
			return null;
		}

		final var payerJson = details.getAsJsonObject(FEE_PAYER_ACCOUNT_ID_PROPERTY);

		try {
			payerIdentifier = Identifier.parse(payerJson);
		} catch (final Exception exception) {
			handleError(exception.getMessage());
		}
		return payerIdentifier;
	}

	/**
	 * Returns the transaction valid start
	 *
	 * @param details
	 * 		the json read from the zip provided by the user
	 * @return a json object representing the transaction valid start if it exists and is correct, Null otherwise.
	 */
	@Nullable
	private JsonObject getTransactionValidStamp(final JsonObject details) {
		JsonObject tvStamp = null;
		if (!details.has(FIRST_TRANSACTION_VALID_START_PROPERTY)) {
			handleError("Missing transaction valid start");
		} else {
			tvStamp = details.getAsJsonObject(FIRST_TRANSACTION_VALID_START_PROPERTY);
		}
		return tvStamp;
	}

	/**
	 * Returns the transaction valid start
	 *
	 * @param tvStamp
	 * 		the json read from the zip provided by the user
	 * @return the transaction valid start if it exists and is correct, Null otherwise.
	 */
	private Timestamp getTimestamp(final JsonObject tvStamp) {
		var timestamp = new Timestamp(0, 0);
		if (tvStamp == null) {
			return timestamp;
		}
		try {
			if (tvStamp.has("seconds") && tvStamp.has("nanos")) {
				timestamp = new Timestamp(tvStamp.get("seconds").getAsLong(), tvStamp.get("nanos").getAsInt());
			}
		} catch (final Exception exception) {
			handleError(exception.getMessage());
		}

		if (!timestamp.isValid()) {
			handleError("Invalid first transaction start");
		}
		return timestamp;
	}

	/**
	 * logs the appropriate error and sets the valid parameter to false
	 *
	 * @param s
	 * 		the string to be displayed in the logs
	 */
	private void handleError(final String s) {
		logger.error(s);
		setValid(false);
	}

	public String getFilename() {
		return filename;
	}

	public int getChunkSize() {
		return chunkSize;
	}

	public int getTransactionValidDuration() {
		return (int) transactionValidDuration.getSeconds();
	}

	public Timestamp getTransactionValidStart() {
		return transactionValidStart;
	}

	@Override
	public Timestamp getExpiration() {
		return expiration;
	}

	@Override
	public boolean isExpired() {
		final var now = new Timestamp();
		return now.getSeconds() > getExpiration().getSeconds();
	}

	public long getValidIncrement() {
		return validIncrement;
	}

	public Identifier getNodeID() {
		return nodeID;
	}

	public long getTransactionFee() {
		return transactionFee;
	}

	public String getMemo() {
		return memo;
	}

	public File getContent() {
		return content;
	}

	private File getUnzippedContentDirectory() {
		var unZippedContent = new File(content.getParent(), FilenameUtils.removeExtension(content.getName()));
		ZipUtil.unpack(content, unZippedContent);
		return unZippedContent;
	}

	private File getUnzippedContent() throws HederaClientException {
		// Recreate the contents. This is done here as the contents is pointing to a temporary copy that may have been
		// altered.
		final var destination = new File(TEMP_DIRECTORY, getBaseName()).getAbsolutePath();

		//TODO this occurs for each key used to sign, which is a lot of extra work. But it will do for now.
		unZip(new File(getParentPath(), getName()).getAbsolutePath(), destination);

		final var bins = new File(destination).listFiles((dir, name) -> name.endsWith(CONTENT_EXTENSION));

		if (bins == null || bins.length != 1) {
			throw new HederaClientException("The contents of the transaction file (.lfu) has been corrupted. " +
					"It will need to be recreated.");
		}

		// If the contents weren't originally zipped, unzip them.
		if (!isZip) {
			final var c = new File(getUnzippedContentDirectory(), filename);
			c.deleteOnExit();
			return c;
		}
		bins[0].deleteOnExit();
		return bins[0];
	}

	public String getChecksum() {
		if (checksum == null) {
			final var tempLocation = new File(TEMP_DIRECTORY, getBaseName()).getAbsolutePath();
			var digest = EncryptionUtils.getFileDigest(new File(tempLocation, content.getName()));
			// If the contents was not originally a zip, get the checksum of the content in the zip.
			if (!isZip) {
				var unZippedContent = new File(tempLocation, FilenameUtils.removeExtension(content.getName()));
				ZipUtil.unpack(content, unZippedContent);
				digest = EncryptionUtils.getFileDigest(
						Path.of(tempLocation, FilenameUtils.removeExtension(content.getName()), getFilename()).toFile());
			}
			if ("".equals(digest)) {
				return "";
			}
			checksum = CommonMethods.splitStringDigest(digest, 6);
		}
		return checksum;
	}

	@Override
	public List<FileActions> getActions() {
		return actions;
	}

	@Override
	public Set<AccountId> getSigningAccounts() {
		return new HashSet<>(Collections.singleton(feePayerAccountId.asAccount()));
	}

	//TODO temp directory should have an added /transactiontool or something, then clear that when the app closes
	// and maybe also clear individual temp stuff as things get signed?
	@Override
	public String execute(final Pair<String, KeyPair> pair, final String user,
						  final String output, final Runnable onSucceed) throws HederaClientException {
		final var keyName = FilenameUtils.getBaseName(pair.getLeft());

		final var value = pair.getValue();
		if (value == null || !isValid()) {
			return "";
		}

		final var privateKey = PrivateKey.fromBytes(value.getPrivate().getEncoded());

		final DoubleProperty progress = new SimpleDoubleProperty(1);

		final var transactionsProgressBar = new ProgressBar();
		transactionsProgressBar.progressProperty().bind(progress);
		transactionsProgressBar.setVisible(true);
		final var compressingProgressBar = new ProgressBar();
		compressingProgressBar.setProgress(-1);

		final var cancelButton = new Button("CANCEL");
		final var window =
				ProgressPopup.setupProgressPopup(transactionsProgressBar, compressingProgressBar, cancelButton);

		cancelButton.visibleProperty().bind(transactionsProgressBar.progressProperty().lessThan(1));

		//TODO need to handle exceptions from the task
		var task = new ExecuteGroupedTask(keyName, privateKey, user, output);

		transactionsProgressBar.progressProperty().bind(task.progressProperty());
		new Thread(task).start();

		task.setOnSucceeded(workerStateEvent -> {
			try {
				moveToHistory(Actions.ACCEPT, getCommentArea().getText(), pair.getLeft());
				setHistory(true);
			} catch (final HederaClientException e) {
				logger.error(e);
			}
			try {
				onSucceed.run();
			} catch (Exception e) {
				throw new HederaClientRuntimeException(e);
			}
			window.close();
		});

		cancelButton.setOnAction(actionEvent -> task.cancel());

		task.setOnCancelled(workerStateEvent -> {
			logger.info("Task interrupted");
			window.close();
		});

		task.setOnFailed(workerStateEvent -> {
			//TODO this should be very visible to the user, so they don't think it passed
			logger.info("Task failed");
			window.close();
		});

		return "";
	}

	@NotNull
	private JsonObject getJsonInput() {
		final var input = new JsonObject();
		input.add(NODE_ID_FIELD_NAME, nodeID.asJSON());
		input.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccountId.asJSON());
		input.add(FILE_ID_FIELD_NAME, fileID.asJSON());
		input.addProperty(MEMO_FIELD_NAME, memo);
		input.addProperty(TRANSACTION_FEE_FIELD_NAME, transactionFee);
		input.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, transactionValidDuration.getSeconds());
		input.add(TRANSACTION_VALID_START_FIELD_NAME, transactionValidStart.asJSON());
		input.addProperty(CONTENTS_FIELD_NAME, TEMP_LOCATION);
		return input;
	}

	public List<ToolTransaction> createTransactionList() {
		final var tempStorage = new File(TEMP_DIRECTORY,
				LocalDate.now().toString()).getAbsolutePath() + File.separator + "LargeBinary"
				+ File.separator;
		try {
			final var tempStorageFile = new File(tempStorage);
			if (tempStorageFile.exists()) {
				FileUtils.cleanDirectory(new File(tempStorage));
			}

			if (tempStorageFile.mkdirs()) {
				logger.info("Created temp folder {}", tempStorage);
			}
			tempStorageFile.deleteOnExit();
		} catch (IOException e) {
			logger.info(String.format("Failed to clean directory: %s", tempStorage));
		}
		final List<ToolTransaction> transactions = new ArrayList<>();

		try (final var fileInputStream = new FileInputStream(getUnzippedContent())) {
			final var buffer = new byte[chunkSize];
			var count = 0;
			int inputStream;
			final var initialValidDuration = transactionValidStart.asDuration();
			while ((inputStream = fileInputStream.read(buffer)) > 0) {
				// The other transactions are appends
				final var trimmed = Arrays.copyOf(buffer, inputStream);
				final var filePath =
						String.format("%s%s-%05d", tempStorage,
								FilenameUtils.getBaseName(filename), count);

				writeBytes(filePath, trimmed);
				// As ToolTransaction saves the reference to input, this should not be reused.
				var input = getJsonInput();
				final var transaction = createTransactionForFilePath(
						input, count, initialValidDuration, filePath);
				transactions.add(transaction);
				count++;
			}
		} catch (final IOException | HederaClientException e) {
			logger.error(e.getMessage());
		}
		return transactions;
	}

	private ToolTransaction createTransactionForFilePath(JsonObject jsonObject,
														 long count,
														 Duration initialValidDuration,
														 String filePath) throws HederaClientException {
		final var incrementedTime =
				new Timestamp(initialValidDuration.plusSeconds(count*validIncrement));
		jsonObject.add(TRANSACTION_VALID_START_FIELD_NAME, incrementedTime.asJSON());
		jsonObject.addProperty(CONTENTS_FIELD_NAME, filePath);

		// The other transactions are appends
		return (count == 0) ?
				new ToolFileUpdateTransaction(jsonObject) :
				new ToolFileAppendTransaction(jsonObject);
	}

	@Override
	public GridPane buildGridPane() {
		final var detailsGridPane = super.buildGridPane();
		final var id = new TransactionId(feePayerAccountId.asAccount(), transactionValidStart.asInstant());
		detailsGridPane.add(FXUtils.buildTransactionIDBox(detailsGridPane, id.toString()), RIGHT, 0);

		final JsonObject nicknames = getAccountNicknames();

		final var feePayerLabel = new Label(CommonMethods.nicknameOrNumber(feePayerAccountId, nicknames));
		feePayerLabel.setWrapText(true);
		detailsGridPane.add(feePayerLabel, RIGHT, 1);

		final var text = new Text(Hbar.fromTinybars(transactionFee).toString().replace(" ", "\u00A0"));
		text.setFont(Font.font("Courier New", 17));
		text.setFill(Color.RED);
		detailsGridPane.add(text, RIGHT, 2);

		final var timeLabel = getTimeLabel(transactionValidStart, true);
		timeLabel.setWrapText(true);
		detailsGridPane.add(timeLabel, RIGHT, 3);

		if (!isHistory() && getTransactionCreationMetadata() != null
				&& getTransactionCreationMetadata().getNodes() != null
				&& !getTransactionCreationMetadata().getNodes().getList().isEmpty()) {
			final var nLabel = new Label("Transactions will be submitted to nodes: ");
			nLabel.setWrapText(true);
			detailsGridPane.add(nLabel, LEFT,4);

			final var nodesString = getTransactionCreationMetadata().getNodes().getList().stream()
					.map(identifier -> identifier.toNicknameAndChecksum(nicknames))
					.collect(Collectors.joining("\n"));

			final var scrollPane = new ScrollPane();
			scrollPane.setFitToWidth(true);
			final var label = new Label(nodesString);
			scrollPane.setMaxHeight(100);
			scrollPane.setContent(label);
			detailsGridPane.add(scrollPane, RIGHT, 4);
		} else {
			detailsGridPane.add(new Label("Node:"), LEFT, 4);
			detailsGridPane.add(new Label(nodeID.toNicknameAndChecksum(nicknames)), RIGHT, 4);
		}

		if (!"".equals(memo)) {
			detailsGridPane.add(new Label("Memo: "), LEFT, 5);
			detailsGridPane.add(new Label(memo), RIGHT, 5);
		}

		final var fileLink = new Hyperlink("Click for more details");
		fileLink.setOnAction(actionEvent -> {
			try {
				if (Desktop.isDesktopSupported()) {
					final var c = getUnzippedContentDirectory();
					c.deleteOnExit();
					Desktop.getDesktop().open(c.getAbsoluteFile());
				}
			} catch (final IOException e) {
				logger.error(e.getMessage());
			}
		});

		//TODO
//		final var printer = JsonFormat.printer();
//		printer.print(); wait I still need the know hte protobuf objects. which means hard coded again.

		detailsGridPane.add(new Label("File Id"), LEFT, 6);
		detailsGridPane.add(new Label(fileID.toReadableString()), RIGHT, 6);

		detailsGridPane.add(new Label("File contents"), LEFT, 7);
		detailsGridPane.add(fileLink, RIGHT, 7);

		detailsGridPane.add(new Label("File Hash"), LEFT, 8);
		final var checksumField = new Text(getChecksum());
		checksumField.setFont(Font.font("Courier New", 16));
		detailsGridPane.add(checksumField, RIGHT, 8);

		File unzippedContent = null;
		try {
			unzippedContent = getUnzippedContent();
		} catch (HederaClientException ex) {
			logger.error("Failed to unzip contents: " + ex.getMessage());
		}

		detailsGridPane.add(new Label("File size"), LEFT, 9);
		final var formattedContentSize = String.format("%d bytes", FileUtils.sizeOf(unzippedContent));
		detailsGridPane.add(new Label(formattedContentSize), RIGHT, 9);

		final var chunks = (int) FileUtils.sizeOf(unzippedContent) / getChunkSize() + ((FileUtils.sizeOf(
				getContent()) % getChunkSize() == 0) ? 0 : 1);

		if (chunks > 0) {
			detailsGridPane.add(new Label("Chunk size"), LEFT, 10);
			final var formattedChunkSize = String.format("%d bytes", getChunkSize());
			detailsGridPane.add(new Label(formattedChunkSize), RIGHT, 10);

			detailsGridPane.add(new Label("Number of transactions"), LEFT, 11);
			final var formattedChunkNumber = String.format("%d", chunks);
			detailsGridPane.add(new Label(formattedChunkNumber), RIGHT, 11);

			final var interval = new Label("Interval between transactions");
			interval.setWrapText(true);
			detailsGridPane.add(interval, LEFT, 12);
			final var formattedIntervalLength = String.format("%d seconds", getValidIncrement());
			detailsGridPane.add(new Label(formattedIntervalLength), RIGHT, 12);
		}

		return detailsGridPane;
	}

	@Override
	public JsonObject toJson() {
		final var toJson = super.toJson();
		toJson.addProperty(FILENAME_PROPERTY, filename);
		toJson.add(FILE_ID_PROPERTY, fileID.asJSON());
		toJson.addProperty(CHUNK_SIZE_PROPERTY, chunkSize);
		toJson.add(FEE_PAYER_ACCOUNT_ID_PROPERTY, feePayerAccountId.asJSON());
		toJson.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, transactionValidDuration.getSeconds());
		toJson.add(TRANSACTION_VALID_START_FIELD_NAME, transactionValidStart.asJSON());
		toJson.addProperty(VALID_INCREMENT_PROPERTY, validIncrement);
		// As this method isn't currently used, this will be left alone.
		toJson.add(NODE_ID_PROPERTY, nodeID.asJSON());
		toJson.addProperty(TRANSACTION_FEE_PROPERTY, transactionFee);
		toJson.addProperty(MEMO_PROPERTY, memo);
		toJson.addProperty(CONTENT_PROPERTY, content.getAbsolutePath());
		return toJson;
	}

	private class ExecuteGroupedTask extends Task<String> {
		static final String TRANSACTIONS_SUFFIX = FILE_NAME_GROUP_SEPARATOR + "transactions";
		static final String SIGNATURES_SUFFIX = FILE_NAME_GROUP_SEPARATOR + "signatures";
		static final int NANO_INCREMENT = 10;
		final String keyName;
		final PrivateKey privateKey;
		final String tempStorage;
		final String user;
		final String output;
		final int max;
		long currentCount = 0L;
		final List<String> fileList = new ArrayList<>();

		public ExecuteGroupedTask(final String keyName, final PrivateKey privateKey,
								  final String user, final String output) {
			this.privateKey = privateKey;
			this.keyName = keyName;
			tempStorage = new File(TEMP_DIRECTORY,
					LocalDate.now().toString()).getAbsolutePath() + File.separator + "LargeBinary"
							+ File.separator + keyName + File.separator;
			try {
				if (new File(tempStorage).exists()) {
					FileUtils.cleanDirectory(new File(tempStorage));
				}

				if (new File(tempStorage).mkdirs()) {
					logger.info("Created temp folder {}", tempStorage);
				}
			} catch (IOException e) {
				logger.info(String.format("Failed to clean directory: %s", tempStorage));
			}
			this.output = output;
			this.user = user;
			try {
				final var actualContent = getUnzippedContent();

				try (final var fileInputStream = new FileInputStream(actualContent)) {
					final var buffer = new byte[chunkSize];
					int inputStream;
					var fileCount = 0;
					while ((inputStream = fileInputStream.read(buffer)) > 0) {
						final var trimmed = Arrays.copyOf(buffer, inputStream);

						final var filePath =
								String.format("%s%s-%05d", tempStorage,
										FilenameUtils.getBaseName(filename), fileCount++);

						writeBytes(filePath, trimmed);
						fileList.add(filePath);
					}
				} catch (IOException ex) {
					//TODO
				}
			} catch (HederaClientException ex) {
				//TODO
			}
			if (getTransactionCreationMetadata() == null) {
				this.max = 1;
			} else {
				final var nodeListSize = getTransactionCreationMetadata().getNodes() == null ? 1
						: getTransactionCreationMetadata().getNodes().getList().size();
				this.max = nodeListSize * fileList.size();
			}
		}

		@Override
		protected String call() throws Exception {
			var result = "";
			final var transactionJson = getJsonInput();
			if (getTransactionCreationMetadata() == null) {
				result = processSingleTransaction(transactionJson);
			} else {
				if (getTransactionCreationMetadata().getNodes() != null) {
					for (final var nodeId : getTransactionCreationMetadata().getNodes().getList()) {
						transactionJson.add(NODE_ID_FIELD_NAME, nodeId.asJSON());
						var tempResult = processTransactionForNode(transactionJson);
						if ("".equals(result)) {
							result = tempResult;
						}
					}
				} else {
					result = processTransactionForNode(transactionJson);
				}
			}
			try {
				FileUtils.deleteDirectory(new File(tempStorage));
			} catch (IOException e) {
				logger.info(String.format("Directory could not be deleted: %s",tempStorage));
			}
			return result;
		}

		private String processTransaction(final JsonObject jsonObject)
				throws Exception {
			var result = "";
			long count = 0L;

			final var initialValidDuration = transactionValidStart.asDuration();

			for (var filePath : fileList) {
				final var transaction = createTransactionForFilePath(
						jsonObject, count, initialValidDuration, filePath);

				transaction.store(String.format("%s.%s",filePath,TRANSACTION_EXTENSION));
				final var signaturePair = new SignaturePair(privateKey.getPublicKey(),
						transaction.createSignature(privateKey));
				signaturePair.write(String.format("%s.%s",filePath,SIGNATURE_EXTENSION));
				count++;
			}

			return result;
		}

		private String processSingleTransaction(final JsonObject jsonObject) throws Exception {
			final var result = processTransaction(jsonObject);
			final var pathname = String.join(FILE_NAME_GROUP_SEPARATOR, fileID.toReadableString(),
					FilenameUtils.getBaseName(getName()),
					keyName) + "." + ZIP_EXTENSION;

			for (var file : fileList) {
				Files.deleteIfExists(Path.of(file));
			}
			zipMessages(pathname, "");

			return result;
		}

		private String processTransactionForNode(final JsonObject jsonObject) throws Exception {
			final var result = processTransaction(jsonObject);
			pack(Identifier.parse(jsonObject.getAsJsonObject(NODE_ID_FIELD_NAME)).toReadableString());
			return result;
		}

		public void pack(String nodeId) throws HederaClientException {
			final var baseName = getZipFileBaseName(nodeId);
			try {
				summarizeTransactions(baseName);
			} catch (final IOException e) {
				throw new HederaClientException(e);
			}
			zipMessages(String.format("%s%s.%s", baseName, TRANSACTIONS_SUFFIX, ZIP_EXTENSION),
					TRANSACTION_EXTENSION);
			zipMessages(String.format("%s%s.%s", baseName, SIGNATURES_SUFFIX, ZIP_EXTENSION),
					SIGNATURE_EXTENSION);
		}

		private void summarizeTransactions(final String baseName) throws HederaClientException, IOException {
			final var transactions =
					new File(tempStorage).listFiles((dir, name) -> name.endsWith(TRANSACTION_EXTENSION));
			if (transactions == null) {
				throw new HederaClientRuntimeException("Unable to read transactions list");
			}
			final var files = Arrays.asList(transactions);
			Collections.sort(files);

			final var summary = Path.of(output, user, baseName +
					FILE_NAME_GROUP_SEPARATOR + "summary.csv");

			Files.deleteIfExists(summary);
			try (final var printWriter = new PrintWriter(summary.toFile());
				 final var writer = new CSVWriter(printWriter)) {
				final var header = new String[] { "Filename", "Transaction Json" };
				writer.writeNext(header);
				for (final var file : files) {
					final List<String> dataList = new ArrayList<>();
					final var t = getTransaction(file);
					if (t != null) {
						dataList.add(file.getName());
						dataList.add(t.asJson().toString().replace(",", ";"));
						var data = new String[2];
						data = dataList.toArray(data);
						writer.writeNext(data);
					}
				}
			} catch (final IOException e) {
				throw new HederaClientException(e);
			}
		}

		// Super ugly, but get the transaction for the file, whether it is an update or append
		private ToolTransaction getTransaction(final File file) throws HederaClientException, InvalidProtocolBufferException {
			final var transaction = Transaction.fromBytes(readBytes(file.getAbsolutePath()));
			if (transaction instanceof FileUpdateTransaction) {
				return new ToolFileUpdateTransaction(file);
			} else if (transaction instanceof FileAppendTransaction) {
				return new ToolFileAppendTransaction(file);
			}
			return null;
		}

		private void zipMessages(final String messages, final String ext) throws HederaClientException {
			try {
				final var txToPack = new File(tempStorage).listFiles((dir, name) -> name.endsWith(ext));
				if (txToPack == null) {
					throw new HederaClientRuntimeException("Unable to read list of transactions to pack");
				}
				final var zipFile = Path.of(output, user, messages).toFile();

				if (new File(tempStorage).exists()) {
					Files.deleteIfExists(zipFile.toPath());
					ZipUtil.packEntries(txToPack, zipFile);
				}

				// Delete transactions that have now been zipped
				for (final File file : txToPack) {
					Files.deleteIfExists(file.toPath());
				}
			} catch (final IOException e) {
				throw new HederaClientException(e);
			}
		}

		private String getZipFileBaseName(final String nodeAccount) {
			// filename(seconds_feepayer_transaction.hash)_keyname_node
			return String.join(FILE_NAME_GROUP_SEPARATOR,
					fileID.toReadableString(),
					FilenameUtils.removeExtension(filename),
					keyName,
					String.format("%s%s%s", "Node", FILE_NAME_INTERNAL_SEPARATOR, nodeAccount));
		}
	}
}
