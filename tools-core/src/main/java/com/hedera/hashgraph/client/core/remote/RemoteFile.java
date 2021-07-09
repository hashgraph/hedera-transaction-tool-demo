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

import com.google.protobuf.ByteString;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.MetadataAction;
import com.hedera.hashgraph.client.core.transactions.ToolSystemTransaction;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.AccountInfo;
import javafx.beans.binding.Bindings;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.KeyPair;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.COMMENT_FIELD_CHARACTER_LIMIT;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.HISTORY_BOX_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.KEYS_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.METADATA_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.REGULAR_BOX_STYLE;
import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_TYPE_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.enums.FileType.ACCOUNT_INFO;
import static com.hedera.hashgraph.client.core.enums.FileType.BATCH;
import static com.hedera.hashgraph.client.core.enums.FileType.COMMENT;
import static com.hedera.hashgraph.client.core.enums.FileType.LARGE_BINARY;
import static com.hedera.hashgraph.client.core.enums.FileType.METADATA;
import static com.hedera.hashgraph.client.core.enums.FileType.PUBLIC_KEY;
import static com.hedera.hashgraph.client.core.enums.FileType.SOFTWARE_UPDATE;
import static com.hedera.hashgraph.client.core.enums.FileType.TRANSACTION;

public class RemoteFile implements Comparable<RemoteFile>, GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(RemoteFile.class);


	private FileType type;

	private String name;
	private String parentPath;
	private long date = 0;

	private boolean history = false;
	private boolean valid = false;
	private boolean hasComments = false;
	private RemoteFile commentsFile;
	private long signDateInSecs = 0;
	private final TextArea commentArea = new TextArea();
	private final Set<File> signerSet = new HashSet<>();
	private final Set<File> extraSigners = new HashSet<>();

	// region CONSTRUCTORS
	public RemoteFile() {
		setValid(false);
	}

	public RemoteFile(String location) throws HederaClientException {
		var file = new File(location);
		if (!file.exists()) {
			if (location.endsWith(METADATA_EXTENSION)) {
				try {
					Files.createFile(file.toPath());
				} catch (IOException e) {
					throw new HederaClientException(e);
				}
			} else {
				throw new HederaClientException(ErrorMessages.FILE_DOES_NOT_EXIST_ERROR_MESSAGE);
			}
		}
		this.parentPath = FilenameUtils.getPath(location);
		this.name = FilenameUtils.getName(location);
		this.type = parseType(FilenameUtils.getExtension(location));

		var filePath = Paths.get(parentPath, name);
		if (!this.type.equals(METADATA)) {
			try {
				var attr =
						Files.readAttributes(filePath, BasicFileAttributes.class);
				this.date = attr.lastAccessTime().toMillis() / 1000;
			} catch (IOException e) {
				throw new HederaClientException(ErrorMessages.CANNOT_READ_FILE_ATTRIBUTES_ERROR_MESSAGE);
			}
		}

		var commentName = FilenameUtils.getBaseName(name) + ".txt";
		if (new File(parentPath + File.separator + commentName).exists()) {
			this.hasComments = true;
		}
		if (this.isHistory()) {
			loadSigningDateFromMetadata();
		}
		this.valid = true;
	}

	public RemoteFile(FileType type, String parentPath, long date) {
		this.type = type;
		this.parentPath = parentPath;
		this.date = date;
		this.name = FilenameUtils.getBaseName(parentPath);
		this.valid = true;
	}

	public RemoteFile(FileDetails file) {
		if (file == null) {
			this.valid = false;
			return;
		}

		if (file.getAttributes().isDirectory()) {
			logger.error("{} must be a file", file.getName());
			this.valid = false;
			return;
		}

		try {
			this.parentPath = new File(file.getPath()).getAbsolutePath();
			this.name = file.getName();
			this.type = parseType(FilenameUtils.getExtension(name));
			this.valid = true;
			this.date = file.getAttributes().lastModifiedTime().to(TimeUnit.SECONDS);
		} catch (HederaClientException e) {
			logger.error(e);
			logger.error(CANNOT_PARSE_TYPE_ERROR_MESSAGE);
			this.valid = false;
		}
		if (this.type != COMMENT) {
			final var commentFile = new File(file.getFullPath().replace(FilenameUtils.getExtension(file.getName()),
					Constants.TXT_EXTENSION));
			if (commentFile.exists()) {
				this.hasComments = true;
				try {
					var comment = FileDetails.parse(commentFile);
					this.commentsFile = new CommentFile(comment);
				} catch (IOException e) {
					logger.error(e);
				}
			}
		}
		final var historyFile = new File(DEFAULT_HISTORY + File.separator + getBaseName() + "." + METADATA_EXTENSION);
		if (historyFile.exists()) {
			loadSigningDateFromMetadata();
		}
	}
	// endregion

	// region GETTERS AND SETTERS
	public FileType getType() {
		return type;
	}

	public void setType(FileType type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public String getBaseName() {
		return FilenameUtils.getBaseName(name);
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPath() {
		return new File(parentPath, name).getPath();
	}

	public String getParentPath() {
		return parentPath;
	}

	public void setParentPath(String parentPath) {
		this.parentPath = parentPath;
	}

	public long getDate() {
		return date;
	}

	public boolean isHistory() {
		return history;
	}

	public void setHistory(boolean history) {
		this.history = history;
	}

	public boolean hasComments() {
		return hasComments;
	}

	public RemoteFile getCommentsFile() {
		return commentsFile;
	}

	public void setCommentsFile(RemoteFile commentsFile) {
		this.commentsFile = commentsFile;
	}

	public boolean isValid() {
		return valid;
	}

	public void setValid(boolean valid) {
		this.valid = valid;
	}

	public long getSignDateInSecs() {
		return signDateInSecs;
	}

	public void setSignDateInSecs(long signDateInSecs) {
		this.signDateInSecs = signDateInSecs;
	}

	public Set<File> getSignerSet() {
		return signerSet;
	}

	public void addToSignerSet(File newSigner) {
		signerSet.add(newSigner);
	}

	public Set<File> getExtraSigners() {
		return extraSigners;
	}

	public void addExtraSigners(List<File> files) {
		extraSigners.addAll(files);
		signerSet.addAll(files);
	}

	public Set<File> getOldSigners() {
		Set<File> oldKeys = new HashSet<>();

		final var historyFile = new File(DEFAULT_HISTORY + File.separator + getBaseName() + "." + METADATA_EXTENSION);
		if (!historyFile.exists()) {
			return oldKeys;
		}
		try {
			var d = new MetadataFile(getName());
			var actions = d.getMetadataActions();
			actions.forEach(action -> {
				final var key = new File(KEYS_FOLDER, action.getKeyName());
				if (Actions.ACCEPT.equals(action.getAction()) && key.exists()) {
					oldKeys.add(key);
				}
			});
		} catch (HederaClientException e) {
			logger.error(e);
		}
		signerSet.removeAll(oldKeys);
		return oldKeys;
	}

	public TextArea getCommentArea() {
		return commentArea;
	}

	public boolean isExpired() {
		return false;
	}

	public void setComments(boolean b) {
		this.hasComments = b;
	}

	public List<FileActions> getActions() {
		return new ArrayList<>();
	}

	// endregion

	/**
	 * Executes the required action on the file.
	 *
	 * @param pair
	 * 		A pair of a key and a name that will be used for signatures
	 * @param user
	 * 		The username. Needed to determine the storage location
	 * @param output
	 * 		The location where files will be stored
	 * @return The path to the produced files.
	 * @throws HederaClientException
	 */
	public String execute(Pair<String, KeyPair> pair, String user, String output) throws HederaClientException {
		return "";
	}

	/**
	 * Moves the current remote file to history
	 *
	 * @param action
	 * 		if the file was signed, or declined
	 * @param userComment
	 * 		Any comments the user might have left
	 * @param keyName
	 * 		The key used if the action was SIGN
	 * @throws HederaClientException
	 */
	public void moveToHistory(Actions action, String userComment, String keyName) throws HederaClientException {
		final var historyFile = new File(DEFAULT_HISTORY + File.separator + name);
		if (!historyFile.exists()) {
			try {
				FileUtils.copyFile(new File(getPath()), historyFile);
			} catch (IOException e) {
				throw new HederaClientException(e);
			}
		}

		if (hasComments()) {
			commentsFile.moveToHistory(action, "", "");
		}

		final var timestamp = new Timestamp();
		var d = new MetadataFile(getName());

		this.parentPath = DEFAULT_HISTORY;
		if (!type.equals(COMMENT)) {
			d.addAction(new MetadataAction(timestamp, action, userComment, keyName));
		}
		this.setHistory(true);
		setSignDateInSecs(timestamp.asDuration().getSeconds());

	}

	/**
	 * Bring back the file from the history to active. Used if more signatures are required
	 *
	 * @throws HederaClientException
	 */
	public void moveFromHistory() throws HederaClientException {
		setHistory(false);
		final var file = new File(DEFAULT_HISTORY, getName());
		if (file.exists()) {
			try {
				Files.delete(file.toPath());
			} catch (IOException e) {
				throw new HederaClientException(e);
			}
			logger.info("File deleted");
		}

	}

	/**
	 * Builds a grid pane with data common to all transaction files
	 *
	 * @return a grid pane with: Fee Payer Account ID, Maximum Transaction Fee, and Submission Time
	 */
	public GridPane buildGridPane() {
		var detailsGridPane = new GridPane();

		detailsGridPane.add(new Label("Fee Payer Account ID: "), 0, 0);

		final var txFeeLabel = new Label("Maximum Transaction Fee: ");
		txFeeLabel.setWrapText(true);
		detailsGridPane.add(txFeeLabel, 0, 1);

		var subLabel = new Label("To be submitted on: ");
		subLabel.setWrapText(true);
		detailsGridPane.add(subLabel, 0, 3);

		detailsGridPane.setHgap(20);
		detailsGridPane.setVgap(10);

		return detailsGridPane;

	}

	/**
	 * Builds the VBox "card" that will be displayed to the user.
	 *
	 * @return a VBOX with a view of the file for display on the home page
	 * @throws HederaClientException
	 */
	public VBox buildDetailsBox() throws HederaClientException {
		var fileVBox = buildFileVBox(isHistory());
		var titleLabel = (TRANSACTION.equals(type)) ? setupTitle(
				((TransactionFile) this).getTransactionType().toString()) : setupTitle(type.toKind());


		var commentsVBox = setupCommentsArea();

		var detailsGridPane = buildGridPane();
		detailsGridPane.setMinWidth(550);
		if (getType().equals(TRANSACTION) || getType().equals(BATCH) || getType().equals(LARGE_BINARY)) {
			addHistory(detailsGridPane);
		}

		var hBox = setupMainHBox(commentsVBox);

		hBox.getChildren().add(detailsGridPane);

		if (!SOFTWARE_UPDATE.equals(this.getType())) {
			hBox.getChildren().add(commentsVBox);
			detailsGridPane.maxWidthProperty().bind(fileVBox.widthProperty().divide(2));
		}

		fileVBox.getChildren().addAll(titleLabel, hBox);
		return fileVBox;
	}

	/**
	 * Finds out if any of the keys that are known by the app can be used to sign the transaction(s)
	 *
	 * @return a set of keys
	 */
	public Set<ByteString> getSigningPublicKeys() {
		Set<ByteString> keysSet = new HashSet<>();
		var accounts = getSigningAccounts();
		if (accounts == null) {
			return new HashSet<>();
		}
		for (var account : accounts) {
			var accountString =
					new Identifier(Objects.requireNonNull(account)).toReadableString();
			var accountFile = new File(ACCOUNTS_INFO_FOLDER, accountString + "." + INFO_EXTENSION);
			if (accountFile.exists()) {
				try {
					var accountInfo = AccountInfo.fromBytes(readBytes(accountFile.getAbsolutePath()));
					keysSet.addAll(EncryptionUtils.flatPubKeys(Collections.singletonList(accountInfo.key)));
				} catch (InvalidProtocolBufferException | HederaClientException e) {
					logger.error(e);
				}
			}
		}

		return keysSet;
	}

	/**
	 * Finds out if any of the accounts that are known by the app are signers for the transaction(s)
	 *
	 * @return a set of account IDs
	 */
	public Set<AccountId> getSigningAccounts() {
		return null;
	}

	/**
	 * Builds a label with special wording for public key and info files, in case they have been previously imported to
	 * the app
	 *
	 * @param entity
	 * 		the name of the key or file id
	 * @return a label with a message appropriate to the situation
	 * @throws HederaClientException
	 */
	public List<Label> getHistory(String entity) throws HederaClientException {
		List<Label> messages = new ArrayList<>();
		var metadataFileList = getSigningHistory();
		for (var m : metadataFileList) {
			var action = m.getAction().toString();
			var label = new Label(String.format("Information regarding %s %s was %s on %s.", entity, getName(),
					action, m.getTimestamp().asReadableLocalString()));
			label.setWrapText(true);
			label.minHeightProperty().bind(Constants.FONT_SIZE.multiply(3));
			VBox.setVgrow(label, Priority.ALWAYS);
			messages.add(label);
		}

		return messages;
	}

	/**
	 * Finds the associated metadata file (if it exists) and reads the action history of the file
	 *
	 * @return a list of the previous actions on the file
	 * @throws HederaClientException
	 */
	public List<MetadataAction> getSigningHistory() throws HederaClientException {
		List<MetadataAction> signingHistory = new ArrayList<>();
		var metadata = new File(DEFAULT_HISTORY, FilenameUtils.getBaseName(getName()) + "." + METADATA_EXTENSION);
		if (metadata.exists()) {
			signingHistory = new MetadataFile(
					FilenameUtils.getBaseName(getName()) + "." + METADATA_EXTENSION).getMetadataActions();
		}
		return signingHistory;
	}

	/**
	 * Finds out if the file has been previously accepted by the user
	 *
	 * @return true if the file has been accepted.
	 */
	public boolean accepted() {
		List<MetadataAction> signingHistory = new ArrayList<>();
		try {
			signingHistory = getSigningHistory();
		} catch (HederaClientException e) {
			logger.error(e);
		}
		for (var metadataAction : signingHistory) {
			if (metadataAction.getAction().equals(Actions.ACCEPT)) {
				return true;
			}
		}
		return false;
	}

	private VBox buildFileVBox(boolean isHistory) {
		var fileVBox = new VBox();
		fileVBox.setSpacing(10);
		fileVBox.setPadding(new Insets(10, 10, 10, 10));

		if (isHistory) {
			fileVBox.setStyle(HISTORY_BOX_STYLE);
			return fileVBox;
		}
		fileVBox.setStyle(REGULAR_BOX_STYLE);


		return fileVBox;
	}

	private Label setupTitle(String title) {
		if ("Content Transaction".equals(title)) {
			final var transaction = (ToolSystemTransaction) ((TransactionFile) this).getTransaction();
			title = ((transaction.isDelete()) ? "Remove " : "Restore ") + (transaction.isFile() ? "File" : "Contract");
		}
		var titleLabel = new Label(title);
		titleLabel.styleProperty().bind(
				Bindings.concat("-fx-font-size: ", Constants.FONT_SIZE.asString(), "; -fx-font-weight: bold;"));
		return titleLabel;
	}

	private void showCreatorComments(VBox commentsVBox) throws HederaClientException {
		if (hasComments()) {
			var txComments = buildTransactionCommentsArea(getCommentsFile());
			if (txComments.getText().length() > 0) {
				var region = new Region();
				region.setMaxHeight(10);
				region.setMinHeight(10);
				VBox.setVgrow(txComments, Priority.ALWAYS);
				commentsVBox.getChildren().addAll(txComments, region);
			}
		}
	}

	private VBox setupCommentsArea() throws HederaClientException {
		var commentsVBox = new VBox();
		showCreatorComments(commentsVBox);
		HBox.setHgrow(commentsVBox, Priority.ALWAYS);
		setupUserComments(commentsVBox);
		commentsVBox.managedProperty().bind(commentsVBox.visibleProperty());
		commentsVBox.setVisible(!(this instanceof SoftwareUpdateFile));
		return commentsVBox;
	}

	private TextArea buildTransactionCommentsArea(RemoteFile commentsFile) throws HederaClientException {
		var comments = readJsonObject(commentsFile.getPath());
		if (!comments.has("Contents")) {
			return new TextArea();
		}

		var authorMessage = "From: " + ((comments.has("Author")) ? comments.get("Author").getAsString() : "");
		var contentsMessage = comments.get("Contents").getAsString();

		var textArea = new TextArea(String.format("%s%n%s", authorMessage, contentsMessage));

		textArea.setWrapText(true);
		textArea.setEditable(false);
		textArea.setPrefRowCount(5);
		textArea.setFocusTraversable(false);
		textArea.setMouseTransparent(true);

		return textArea;
	}

	private void setupUserComments(VBox commentsVBox) {
		commentArea.setId("comment");
		commentArea.setWrapText(true);
		commentArea.setPrefRowCount(5);
		commentArea.setPromptText("Your comments to the coordinating administrator");
		VBox.setVgrow(commentArea, Priority.ALWAYS);

		var charsLeft = new Label(String.format("Characters left: %d", COMMENT_FIELD_CHARACTER_LIMIT));
		commentArea.lengthProperty().addListener((observable, oldValue, newValue) -> {
			if (newValue.intValue() > oldValue.intValue()) {
				if (commentArea.getText().length() >= COMMENT_FIELD_CHARACTER_LIMIT) {
					commentArea.setText(commentArea.getText().substring(0, COMMENT_FIELD_CHARACTER_LIMIT));
				}
			}
			charsLeft.setText(String.format("Characters left: %d",
					COMMENT_FIELD_CHARACTER_LIMIT - commentArea.getText().length()));
		});

		commentsVBox.setAlignment(Pos.BOTTOM_RIGHT);
		commentsVBox.getChildren().addAll(commentArea, charsLeft);
		commentsVBox.setMinHeight(Region.USE_COMPUTED_SIZE);
	}

	private HBox setupMainHBox(VBox commentsVBox) {
		var hBox = new HBox();

		HBox.setHgrow(commentsVBox, Priority.ALWAYS);
		hBox.setSpacing(30);
		hBox.setMaxHeight(Region.USE_PREF_SIZE);
		hBox.setMinHeight(Region.USE_PREF_SIZE);
		hBox.setPrefHeight(Region.USE_COMPUTED_SIZE);
		return hBox;
	}

	private void loadSigningDateFromMetadata() {
		final var historyFile = new File(DEFAULT_HISTORY + File.separator + getBaseName() + "." + METADATA_EXTENSION);
		if (!historyFile.exists()) {
			return;
		}

		try {
			var d = new MetadataFile(getName());
			var actions = d.getMetadataActions();

			actions.forEach(action -> {
				new File(KEYS_FOLDER, action.getKeyName());
				if (action.getTimestamp().getSeconds() > signDateInSecs) {
					signDateInSecs = action.getTimestamp().getSeconds();
				}
			});
		} catch (HederaClientException e) {
			logger.error(e);
		}
	}

	private void addHistory(GridPane detailsGridPane) throws HederaClientException {
		var rowCount = detailsGridPane.getRowCount();
		var signingHistory = getSigningHistory();
		if (signingHistory.size() == 0) {
			return;
		}
		var accepted = false;
		for (var metadataAction : signingHistory) {
			if (Actions.ACCEPT.equals(metadataAction.getAction())) {
				accepted = true;
				break;
			}
		}
		if (accepted) {
			detailsGridPane.add(new Label("Previously signed by:"), 0, rowCount++);
		}
		for (var metadataAction : signingHistory) {
			if (Actions.ACCEPT.equals(metadataAction.getAction())) {
				final var label = new Label(String.format("%s on: ", metadataAction.getKeyName()));
				label.setWrapText(true);
				detailsGridPane.add(label, 0, rowCount);
				detailsGridPane.add(new Label(metadataAction.getTimestamp().asReadableLocalString()), 1, rowCount++);
			}
		}
		for (var metadataAction : signingHistory) {
			if (Actions.DECLINE.equals(metadataAction.getAction())) {
				detailsGridPane.add(new Label("Declined on: "), 0, rowCount);
				detailsGridPane.add(new Label(metadataAction.getTimestamp().asReadableLocalString()), 1, rowCount++);
			}
		}

	}

	private static FileType parseType(String extension) throws HederaClientException {
		switch (extension) {
			case "txt":
				return COMMENT;
			case "tx":
				return TRANSACTION;
			case "info":
				return ACCOUNT_INFO;
			case "pub":
				return PUBLIC_KEY;
			case "pkg":
				return SOFTWARE_UPDATE;
			case "csv":
				return BATCH;
			case "zip":
				return LARGE_BINARY;
			case "meta":
				return METADATA;
			default:
				throw new HederaClientException(String.format("Unrecognized extension: %s", extension));
		}
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		var that = (RemoteFile) o;
		return name.equals(that.name) && date == that.getDate();
	}

	@Override
	public int compareTo(RemoteFile o) {
		// when both files are in the history, the ordering is backwards.
		if (isHistory() && o.isHistory()) {
			return -Long.compare(this.getSignDateInSecs(), o.getSignDateInSecs());
		}

		// Lastly the files are ordered according to the modification date (not expiration)
		return Long.compare(this.getDate(), o.getDate());
	}
}
