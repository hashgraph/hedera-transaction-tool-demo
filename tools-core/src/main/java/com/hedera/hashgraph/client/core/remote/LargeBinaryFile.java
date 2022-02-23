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
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.transactions.ToolFileAppendTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolFileUpdateTransaction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrivateKey;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.zeroturnaround.zip.ZipUtil;

import javax.annotation.Nullable;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
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

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.CONTENT_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.CONTENTS_FIELD_NAME;
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

	private static final String TEMP_DIRECTORY = System.getProperty("java.io.tmpdir");
	private static final String TEMP_LOCATION = TEMP_DIRECTORY + File.separator + "content." + CONTENT_EXTENSION;

	private String filename;
	private Identifier fileID;
	private int chunkSize;
	private Identifier feePayerAccountId;
	private Duration transactionValidDuration;
	private Timestamp transactionValidStart;
	private int validIncrement;
	private Identifier nodeID;
	private long transactionFee;
	private String memo;
	private File content = null;

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
		final var jsons = new File(destination).listFiles((dir, name) -> name.endsWith(".json"));
		assert jsons != null;
		if (jsons.length != 1) {
			final var formattedError =
					String.format("There should be exactly one json file in zip archive. We found: %d", jsons.length);
			handleError(formattedError);
			return;
		}

		final var bins = new File(destination).listFiles((dir, name) -> name.endsWith(CONTENT_EXTENSION));
		assert bins != null;
		if (bins.length != 1) {
			final var formattedError =
					String.format("There should be exactly one binary file in the zip archive. We found: %d",
							bins.length);
			handleError(formattedError);
			return;
		}

		final JsonObject details;
		try {
			details = readJsonObject(jsons[0].getPath());
		} catch (final HederaClientException exception) {
			handleError(exception.getMessage());
			return;
		}

		if (!details.get("filename").getAsString().equals(bins[0].getName())) {
			handleError("The binary file does not correspond to the file specified in the details");
			return;
		}

		final Identifier fileIdentifier = getFileIdentifier(details);
		final Identifier nodeIdentifier = getNodeIdentifier(details);
		final Identifier payerIdentifier = getPayerIdentifier(details);
		final JsonObject tvStamp = getTransactionValidStamp(details);
		final Timestamp timestamp = getTimestamp(tvStamp);

		if (checkNotNulls(fileIdentifier, nodeIdentifier, payerIdentifier, tvStamp, timestamp)) {
			return;
		}

		this.filename = details.get("filename").getAsString();
		this.fileID = fileIdentifier;
		this.chunkSize = details.has("chunkSize") ? details.get("chunkSize").getAsInt() : 1024;
		if (getChunkSize() > 1024) {
			throw new HederaClientException("Maximum chunk size is 1024 for unsigned file update transactions.");
		}
		this.feePayerAccountId = payerIdentifier;
		this.transactionValidDuration =
				Duration.ofSeconds(details.has("validDuration") ? details.get("validDuration").getAsLong() : 120);
		this.transactionValidStart = timestamp;
		this.validIncrement = details.has("validIncrement") ? details.get("validIncrement").getAsInt() : 100;
		this.nodeID = nodeIdentifier;
		this.transactionFee = details.has("transactionFee") ? details.get("transactionFee").getAsLong() : 200000000;
		this.memo = details.has("memo") ? details.get("memo").getAsString() : "";
		this.content = bins[0];

		setShowAdditionalBoxes();
	}

	private boolean checkNotNulls(final Object... ids) {
		return Arrays.stream(ids).anyMatch(Objects::isNull);
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
		if (!details.has("fileID")) {
			handleError("Missing file ID in details file");
			return null;
		}
		final var fileJson = details.getAsJsonObject("fileID");

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
		if (!details.has("nodeID")) {
			handleError("Missing node ID in details file");
			return null;
		}

		final var nodeJson = details.getAsJsonObject("nodeID");

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
		if (!details.has("feePayerAccountId")) {
			handleError("Missing fee payer ID in details file");
			return null;
		}

		final var payerJson = details.getAsJsonObject("feePayerAccountId");

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
		if (!details.has("firsTransactionValidStart")) {
			handleError("Missing transaction valid start");
		} else {
			tvStamp = details.getAsJsonObject("firsTransactionValidStart");
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
	@Nullable
	private Timestamp getTimestamp(final JsonObject tvStamp) {
		final Timestamp timestamp;
		try {
			timestamp = new Timestamp(tvStamp.get("seconds").getAsLong(), tvStamp.get("nanos").getAsInt());
		} catch (final Exception exception) {
			handleError(exception.getMessage());
			return null;
		}

		if (!timestamp.isValid()) {
			handleError("Invalid first transaction start");
			return null;
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

	public int getValidIncrement() {
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

	public String getChecksum() {
		final var digest = EncryptionUtils.getFileDigest(new File(getParentPath() + File.separator + getName()));
		if ("".equals(digest)) {
			return "";
		}
		return CommonMethods.splitStringDigest(digest, 6);
	}

	@Override
	public List<FileActions> getActions() {
		return actions;
	}

	@Override
	public Set<AccountId> getSigningAccounts() {
		return new HashSet<>(Collections.singleton(feePayerAccountId.asAccount()));
	}

	@Override
	public String execute(final Pair<String, KeyPair> pair, final String user,
			final String output) throws HederaClientException {
		try {
			moveToHistory(Actions.ACCEPT, getCommentArea().getText(), pair.getLeft());
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
		final List<File> toPack = new ArrayList<>();

		final var privateKey = PrivateKey.fromBytes(pair.getValue().getPrivate().getEncoded());
		final var tempStorage =
				new File(TEMP_DIRECTORY, LocalDate.now().toString()).getAbsolutePath() + File.separator + "LargeBinary"
						+ File.separator + FilenameUtils.getBaseName(pair.getLeft()) + File.separator;

		final var pathname = String.format("%s%s_%s.zip", tempStorage, FilenameUtils.getBaseName(this.getName()),
				pair.getKey().replace(".pem", ""));
		final var finalZip = new File(pathname);

		if (pair.getValue() == null || !isValid() || content == null) {
			return null;
		}

		try {
			if (new File(tempStorage).exists()) {
				org.apache.commons.io.FileUtils.cleanDirectory(new File(tempStorage));
			}

			if (new File(tempStorage).mkdirs()) {
				logger.info("Created temp folder {}", tempStorage);
			}

			try (final var fileInputStream = new FileInputStream(content)) {
				final var buffer = new byte[chunkSize];
				var count = 1;
				var inputStream = fileInputStream.read(buffer);

				final var input = new JsonObject();
				input.add(NODE_ID_FIELD_NAME, nodeID.asJSON());
				input.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccountId.asJSON());
				input.add(FILE_ID_FIELD_NAME, fileID.asJSON());
				input.addProperty(MEMO_FIELD_NAME, memo);
				input.addProperty(TRANSACTION_FEE_FIELD_NAME, transactionFee);
				input.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, transactionValidDuration.getSeconds());
				input.add(TRANSACTION_VALID_START_FIELD_NAME, transactionValidStart.asJSON());
				input.addProperty(CONTENTS_FIELD_NAME, TEMP_LOCATION);

				var incrementedTime = transactionValidStart;

				var trimmed = Arrays.copyOf(buffer, inputStream);
				writeBytes(TEMP_LOCATION, trimmed);

				// First transaction is an update
				final var updateTransaction = new ToolFileUpdateTransaction(input);
				updateTransaction.sign(privateKey);
				final var filePath = String.format("%s%s-00000.%s", tempStorage, FilenameUtils.getBaseName(filename),
						SIGNED_TRANSACTION_EXTENSION);
				writeBytes(filePath, updateTransaction.getTransaction().toBytes());
				toPack.add(new File(filePath));

				while (inputStream > 0) {

					// The other transactions are appends
					trimmed = Arrays.copyOf(buffer, inputStream);
					writeBytes(TEMP_LOCATION, trimmed);

					incrementedTime =
							new Timestamp(incrementedTime.asDuration().plusNanos((long) count * validIncrement));
					input.add(TRANSACTION_VALID_START_FIELD_NAME, incrementedTime.asJSON());

					final var appendTransaction = new ToolFileAppendTransaction(input);
					appendTransaction.sign(privateKey);
					final var appendFilePath =
							String.format("%s%s-%05d.%s", tempStorage, FilenameUtils.getBaseName(filename), count,
									SIGNED_TRANSACTION_EXTENSION);
					writeBytes(appendFilePath, appendTransaction.getTransaction().toBytes());
					toPack.add(new File(appendFilePath));
					count++;
					inputStream = fileInputStream.read(buffer);
				}
			}
			// Zip transactions

			var toPackArray = new File[toPack.size()];
			toPackArray = toPack.toArray(toPackArray);
			ZipUtil.packEntries(toPackArray, finalZip);
			toPack.forEach(file -> {
				try {
					if (Files.deleteIfExists(file.toPath())) {
						logger.info("{} deleted", file.getAbsolutePath());
					}
				} catch (final IOException e) {
					logger.error(e);
				}
			});

			final var outputFile = new File(output + File.separator + user, finalZip.getName());
			Files.deleteIfExists(outputFile.toPath());
			FileUtils.moveFile(finalZip, outputFile);
			return outputFile.getAbsolutePath();
		} catch (final IOException e) {
			logger.error(e.getMessage());
			throw new HederaClientException(e);
		}
	}

	@Override
	public GridPane buildGridPane() {
		final var detailsGridPane = super.buildGridPane();

		try {
			final var map =
					(new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			final var feePayerLabel = new Label(CommonMethods.nicknameOrNumber(feePayerAccountId, map));
			feePayerLabel.setWrapText(true);
			detailsGridPane.add(feePayerLabel, 1, 0);
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}


		final var text = new Text(new Hbar(transactionFee).toString().replace(" ", "\u00A0"));
		text.setFont(Font.font("Courier New", 17));
		text.setFill(Color.RED);
		detailsGridPane.add(text, 1, 1);

		if (!"".equals(memo)) {
			detailsGridPane.add(new Label("Memo: "), 0, 2);
			detailsGridPane.add(new Label(memo), 1, 2);
		}

		final var timeLabel = getTimeLabel(transactionValidStart, true);
		timeLabel.setWrapText(true);
		detailsGridPane.add(timeLabel, 1, 3);


		final var fileLink = new Hyperlink("Click for more details");
		fileLink.setOnAction(actionEvent -> {
			try {
				final var copyName =
						FilenameUtils.getBaseName(getContent().getName()) + "-copy." + CONTENT_EXTENSION;
				FileUtils.copyFile(getContent(), new File(copyName));
				final var r = Runtime.getRuntime();
				final var command = String.format("open -e %s", copyName);
				r.exec(command);
			} catch (final IOException e) {
				logger.error(e.getMessage());
			}
		});


		detailsGridPane.add(new Label("File contents"), 0, 4);
		detailsGridPane.add(fileLink, 1, 4);

		detailsGridPane.add(new Label("File Hash"), 0, 5);
		final var checksum = new Text(getChecksum());
		checksum.setFont(Font.font("Courier New", 16));
		detailsGridPane.add(checksum, 1, 5);

		detailsGridPane.add(new Label("File size"), 0, 6);
		final var formattedContentSize = String.format("%d bytes", FileUtils.sizeOf(getContent()));
		detailsGridPane.add(new Label(formattedContentSize), 1, 6);

		final var chunks = (int) FileUtils.sizeOf(getContent()) / getChunkSize() + ((FileUtils.sizeOf(
				getContent()) % getChunkSize() == 0) ? 0 : 1);

		if (chunks > 0) {
			detailsGridPane.add(new Label("Chunk size"), 0, 7);
			final var formattedChunkSize = String.format("%d bytes", getChunkSize());
			detailsGridPane.add(new Label(formattedChunkSize), 1, 7);

			detailsGridPane.add(new Label("Number of transactions"), 0, 8);
			final var formattedChunkNumber = String.format("%d", chunks);
			detailsGridPane.add(new Label(formattedChunkNumber), 1, 8);

			final var interval = new Label("Interval between transactions");
			interval.setWrapText(true);
			detailsGridPane.add(interval, 0, 9);
			final var formattedIntervalLength = String.format("%d nanoseconds", getValidIncrement());
			detailsGridPane.add(new Label(formattedIntervalLength), 1, 9);
		}

		return detailsGridPane;
	}

	@Override
	public boolean equals(final Object o) {
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
