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
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolFreezeTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolSystemTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.PrivateKey;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TreeView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.zeroturnaround.zip.ZipUtil;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.security.KeyPair;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.CREDIT;
import static com.hedera.hashgraph.client.core.constants.Constants.DEBIT;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNATURE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MAX_TOKEN_ASSOCIATIONS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.getTimeLabel;

public class TransactionFile extends RemoteFile implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(TransactionFile.class);
	public static final String UNBREAKABLE_SPACE = "\u00A0";
	private static final Font COURIER_FONT = Font.font("Courier New", 17);

	private ToolTransaction transaction;
	private TransactionType transactionType;
	private Timestamp expiration;
	private TreeView<String> treeView = new TreeView<>();
	private TreeView<String> oldKey = new TreeView<>();

	private final List<FileActions> actions =
			Arrays.asList(FileActions.SIGN, FileActions.DECLINE, FileActions.ADD_MORE, FileActions.BROWSE);
	private JsonObject nicknames;
	private JsonObject oldInfo = null;
	private String network = "MAINNET";

	public TransactionFile() {
		super();
	}

	public TransactionFile(final FileDetails fileDetails) {
		super(fileDetails);
		if (!isValid() || !FileType.TRANSACTION.equals(getType())) {
			setValid(false);
			return;
		}
		try {
			final var toolTransaction = new ToolTransaction();
			transaction = toolTransaction.parseFile(new File(fileDetails.getFullPath()));
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
			setValid(false);
			return;
		}
		transactionType = transaction.getTransactionType();

		final var tvs = transaction.getTransactionValidStart();
		final var tvd = transaction.getTransactionValidDuration();

		expiration = new Timestamp(tvs.getEpochSecond() + tvd.getSeconds(), tvs.getNano() + tvd.getNano());
		setShowAdditionalBoxes();
	}

	public ToolTransaction getTransaction() {
		return transaction;
	}

	public void setTreeView(final TreeView<String> treeView) {
		this.treeView = treeView;
	}

	public void setOldKey(final TreeView<String> treeView) {
		this.oldKey = treeView;
	}

	public String getMemo() {
		return transaction.getMemo();
	}

	public Identifier getFeePayerAccountId() {
		return transaction.getFeePayerID();
	}

	public long getTransactionFee() {
		return transaction.getTransactionFee().toTinybars();
	}

	public Timestamp getTransactionValidStart() {
		return new Timestamp(transaction.getTransactionValidStart());
	}

	public TransactionType getTransactionType() {
		return transactionType;
	}

	public Timestamp getExpiration() {
		return expiration;
	}

	public void setNetwork(final String network) {
		this.network = network;
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public boolean isExpired() {
		final var now = new Timestamp();
		return now.getSeconds() > getExpiration().getSeconds();
	}

	@Override
	public GridPane buildGridPane() {
		final var detailsGridPane = super.buildGridPane();
		handleTransactionCommonFields(detailsGridPane);
		final var count = detailsGridPane.getRowCount() + 1;
		try {
			nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (final HederaClientException e) {
			logger.error(e);
		}
		switch (transaction.getTransactionType()) {
			case CRYPTO_TRANSFER:
				handleCryptoTransferFields(detailsGridPane, count);
				break;
			case CRYPTO_CREATE:
				handleCryptoCreateTransactionFields(detailsGridPane, count);
				break;
			case CRYPTO_UPDATE:
				handleCryptoUpdateTransactionField(detailsGridPane, count);
				break;
			case SYSTEM_DELETE_UNDELETE:
				handleSystemTransactionField(detailsGridPane, count);
				break;
			case FREEZE:
				handleFreezeTransactionFields(detailsGridPane, count);
				break;
			default:
				logger.error("Unrecognized transaction type {}", transaction.getTransactionType());
		}
		return detailsGridPane;
	}

	/**
	 * Add the common fields to the grid pane
	 *
	 * @param detailsGridPane
	 * 		the pane where the transaction details are entered
	 */
	private void handleTransactionCommonFields(final GridPane detailsGridPane) {
		try {
			nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (final HederaClientException e) {
			logger.error(e);
			nicknames = new JsonObject();
		}

		final var feePayerLabel = new Label(CommonMethods.nicknameOrNumber(transaction.getFeePayerID(), nicknames));
		feePayerLabel.setWrapText(true);
		detailsGridPane.add(feePayerLabel, 1, 0);

		final var text = new Text(transaction.getTransactionFee().toString().replace(" ", UNBREAKABLE_SPACE));
		text.setFont(COURIER_FONT);
		text.setFill(Color.RED);
		detailsGridPane.add(text, 1, 1);

		if (!"".equals(transaction.getMemo())) {
			detailsGridPane.add(new Label("Memo: "), 0, 2);
			detailsGridPane.add(new Label(transaction.getMemo()), 1, 2);
		}

		final var timeLabel = getTimeLabel(new Timestamp(transaction.getTransactionValidStart()), true);
		timeLabel.setWrapText(true);
		detailsGridPane.add(timeLabel, 1, 3);
		detailsGridPane.add(new Label("Node:"), 0, 4);
		detailsGridPane.add(new Label(transaction.getNodeID().toNicknameAndChecksum(nicknames)), 1, 4);
	}

	/**
	 * Add the CRYPTO TRANSFER exclusive fields to the grid pane
	 *
	 * @param detailsGridPane
	 * 		the pane where the transaction details are entered
	 * @param count
	 * 		the number of rows in the grid pane
	 */
	private void handleCryptoTransferFields(final GridPane detailsGridPane, int count) {

		final var accountAmountMap =
				((ToolTransferTransaction) transaction).getAccountAmountMap();
		final List<Pair<String, String>> senders = new ArrayList<>();
		final List<Pair<String, String>> receivers = new ArrayList<>();
		for (final var entry : accountAmountMap.entrySet()) {
			final var amount = entry.getValue();
			final var identifier = entry.getKey();
			if (amount.toTinybars() < 0) {
				senders.add(Pair.of(CommonMethods.nicknameOrNumber(identifier, nicknames),
						amount.toString().replace(" ", UNBREAKABLE_SPACE)));
			} else {
				receivers.add(Pair.of(CommonMethods.nicknameOrNumber(identifier, nicknames),
						amount.toString().replace(" ", UNBREAKABLE_SPACE)));
			}
		}

		detailsGridPane.add(new Label("Senders: "), 0, count++);
		count = setAccountAmounts(detailsGridPane, count, senders);

		detailsGridPane.add(new Label("Receivers: "), 0, count++);
		setAccountAmounts(detailsGridPane, count, receivers);
	}

	/**
	 * Add the CRYPTO CREATE exclusive fields to the grid pane
	 *
	 * @param detailsGridPane
	 * 		the pane where the transaction details are entered
	 * @param count
	 * 		the number of rows in the grid pane
	 */
	private void handleCryptoCreateTransactionFields(final GridPane detailsGridPane, int count) {
		final var keysLink = new Hyperlink("Click for more details");
		final var sigReqLabel = new Label("Receiver signature required: ");
		sigReqLabel.setWrapText(true);

		final var createTransaction = (ToolCryptoCreateTransaction) this.transaction;
		detailsGridPane.add(new Label("Key: "), 0, count);
		keysLink.setOnAction(actionEvent -> displayKey(treeView, new TreeView<>()));
		detailsGridPane.add(keysLink, 1, count++);

		detailsGridPane.add(new Label("Auto renew period: "), 0, count);
		detailsGridPane.add(
				new Label(String.format("%s seconds", createTransaction.getAutoRenewDuration().getSeconds())), 1,
				count++);

		detailsGridPane.add(new Label("Initial balance: "), 0, count);
		final var initialBalance = new Label(createTransaction.getInitialBalance().toString());
		initialBalance.setFont(COURIER_FONT);
		initialBalance.setStyle(DEBIT);
		detailsGridPane.add(initialBalance, 1, count++);

		final var titleLabel = new Label("Maximum automatic token associations: ");
		titleLabel.setWrapText(true);
		detailsGridPane.add(titleLabel, 0, count);
		final var maxTokens = new Label(String.valueOf(createTransaction.getMaxTokenAssociations()));
		detailsGridPane.add(maxTokens, 1, count++);


		detailsGridPane.add(new Label("Account memo: "), 0, count);
		final var accountMemo = new Label(createTransaction.getAccountMemo());
		detailsGridPane.add(accountMemo, 1, count++);


		detailsGridPane.add(sigReqLabel, 0, count);
		detailsGridPane.add(new Label(String.format("%s", createTransaction.isReceiverSignatureRequired())), 1,
				count);
	}

	/**
	 * Add the CRYPTO UPDATE exclusive fields to the grid pane
	 *
	 * @param detailsGridPane
	 * 		the pane where the transaction details are entered
	 * @param count
	 * 		the number of rows in the grid pane
	 */
	private void handleCryptoUpdateTransactionField(final GridPane detailsGridPane, int count) {

		final var updateTransaction = (ToolCryptoUpdateTransaction) this.transaction;

		detailsGridPane.add(new Label("Account to update"), 0, count);
		final var account = updateTransaction.getAccount();
		account.setNetworkName(network);
		detailsGridPane.add(new Label(account.toNicknameAndChecksum(nicknames)), 1, count++);

		final var sigReqLabel = new Label("Receiver signature required: ");
		sigReqLabel.setWrapText(true);

		final var hasOldInfo = oldInfo != null;
		final var keysLink = new Hyperlink("Click for more details");
		if (updateTransaction.getKey() != null) {
			detailsGridPane.add(new Label("Key: "), 0, count);
			keysLink.setOnAction(actionEvent -> displayKey(treeView, oldKey));
			detailsGridPane.add(keysLink, 1, count++);
		}

		if (updateTransaction.getAutoRenewDuration() != null) {
			count = handleAutoRenewDuration(detailsGridPane, count, updateTransaction, hasOldInfo);
		}

		if (updateTransaction.isReceiverSignatureRequired() != null) {
			count = handleIsReceiverSigRequired(detailsGridPane, count, updateTransaction, sigReqLabel, hasOldInfo);
		}

		if (updateTransaction.getMaxTokenAssociations() != null) {
			count = handleMaxTokenAssociations(detailsGridPane, count, updateTransaction, hasOldInfo);
		}

		if (updateTransaction.getAccountMemo() != null) {
			handleAccountMemo(detailsGridPane, count, updateTransaction, hasOldInfo);
		}
	}

	private int handleAutoRenewDuration(final GridPane detailsGridPane, int count,
			final ToolCryptoUpdateTransaction updateTransaction, final boolean hasOldInfo) {
		detailsGridPane.add(new Label("Auto renew period: "), 0, count);
		final var labelString = hasOldInfo ?
				String.format("%s seconds (updated from %s s)",
						updateTransaction.getAutoRenewDuration().getSeconds(),
						oldInfo.get("autoRenewPeriod").getAsJsonObject().get("seconds").getAsString()) :
				String.format("%s seconds", updateTransaction.getAutoRenewDuration().getSeconds());
		final var label = new Label(labelString);
		label.setWrapText(true);
		detailsGridPane.add(label, 1, count++);
		return count;
	}

	private int handleIsReceiverSigRequired(final GridPane detailsGridPane, int count,
			final ToolCryptoUpdateTransaction updateTransaction, final Label sigReqLabel, final boolean hasOldInfo) {
		detailsGridPane.add(sigReqLabel, 0, count);
		final var oldValue = (hasOldInfo && oldInfo.has("receiverSignatureRequired")) ?
				oldInfo.get("receiverSignatureRequired").getAsString() :
				"FALSE";
		final var newValue = updateTransaction.isReceiverSignatureRequired().toString().toUpperCase(Locale.ROOT);
		final var labelString = hasOldInfo ?
				String.format("%s (updated from %s)", newValue, oldValue) :
				String.format("%s", newValue);
		final var label = new Label(labelString);
		label.setWrapText(true);
		detailsGridPane.add(label, 1, count++);
		return count;
	}

	private int handleMaxTokenAssociations(final GridPane detailsGridPane, int count,
			final ToolCryptoUpdateTransaction updateTransaction, final boolean hasOldInfo) {
		final var labelMAT = new Label("Maximum automatic token associations");
		labelMAT.setWrapText(true);
		detailsGridPane.add(labelMAT, 0, count);
		final var newValue = updateTransaction.getMaxTokenAssociations();
		final var labelString = (hasOldInfo && oldInfo.has(MAX_TOKEN_ASSOCIATIONS_FIELD_NAME)) ?
				String.format("%d (updated from %d)", newValue,
						oldInfo.get(MAX_TOKEN_ASSOCIATIONS_FIELD_NAME).getAsInt()) :
				String.format("%d", newValue);
		final var label = new Label(labelString);
		label.setWrapText(true);
		detailsGridPane.add(label, 1, count++);
		return count;
	}

	private void handleAccountMemo(final GridPane detailsGridPane, final int count,
			final ToolCryptoUpdateTransaction updateTransaction, final boolean hasOldInfo) {
		detailsGridPane.add(new Label("Account memo"), 0, count);
		final var newValue = updateTransaction.getAccountMemo();
		final var labelString = (hasOldInfo && oldInfo.has(ACCOUNT_MEMO_FIELD_NAME)) ?
				String.format("%s (updated from %s)", newValue,
						oldInfo.get(ACCOUNT_MEMO_FIELD_NAME).getAsString()) :
				String.format("%s", newValue);
		final var label = new Label(labelString);
		label.setWrapText(true);
		detailsGridPane.add(label, 1, count);
	}

	/**
	 * Add the SYSTEM exclusive fields to the grid pane
	 *
	 * @param detailsGridPane
	 * 		the pane where the transaction details are entered
	 * @param count
	 * 		the number of rows in the grid pane
	 */
	private void handleSystemTransactionField(final GridPane detailsGridPane, int count) {
		final var toolSystemTransaction = (ToolSystemTransaction) transaction;
		final var isDelete = toolSystemTransaction.isDelete();
		final var isFile = toolSystemTransaction.isFile();

		detailsGridPane.add(new Label(isFile ? "File ID: " : "Contract ID: "), 0, count);
		detailsGridPane.add(new Label(toolSystemTransaction.getEntity().toReadableString()), 1,
				count++);

		if (isDelete) {
			final var subLabel = new Label((isFile ? "File" : "Contract") + " will expire on: ");
			subLabel.setWrapText(true);
			detailsGridPane.add(subLabel, 0, count);
			final var expirationTimeLabel = getTimeLabel(new Timestamp(toolSystemTransaction.getExpiration()),
					true);
			expirationTimeLabel.setWrapText(true);
			detailsGridPane.add(expirationTimeLabel, 1, count);
		}
	}

	/**
	 * Add the FREEZE exclusive fields to the grid pane
	 *
	 * @param detailsGridPane
	 * 		the pane where the transaction details are entered
	 * @param count
	 * 		the number of rows in the grid pane
	 */
	private void handleFreezeTransactionFields(final GridPane detailsGridPane, int count) {
		final var toolFreezeTransaction = (ToolFreezeTransaction) transaction;
		final var freezeType = toolFreezeTransaction.getFreezeType();
		final Label startTimeLabel;
		final Label fileIDLabel;
		final var fileHashLabel = new Text();
		fileHashLabel.setFont(Font.font("Courier New", 18));
		switch (freezeType) {
			case UNKNOWN_FREEZE_TYPE:
				logger.error("Cannot parse freeze type");
				break;
			case FREEZE_ONLY:
				// Freezes the network at the specified time. The start_time field must be provided and must
				// reference a future time. Any values specified for the update_file and file_hash fields will
				// be ignored. This transaction does not perform any network changes or upgrades and requires
				// manual intervention to restart the network.
				detailsGridPane.add(new Label("Freeze start:"), 0, count);
				startTimeLabel = getTimeLabel(toolFreezeTransaction.getStartTime(), true);
				startTimeLabel.setWrapText(true);
				detailsGridPane.add(startTimeLabel, 1, count);
				break;
			case PREPARE_UPGRADE:
				// A non-freezing operation that initiates network wide preparation in advance of a scheduled
				// freeze upgrade. The update_file and file_hash fields must be provided and valid. The
				// start_time field may be omitted and any value present will be ignored.
				fileIDLabel = new Label(toolFreezeTransaction.getFileID().toNicknameAndChecksum(new JsonObject()));
				detailsGridPane.add(new Label("Upgrade file:"), 0, count);
				detailsGridPane.add(fileIDLabel, 1, count++);
				fileHashLabel.setText(
						CommonMethods.splitStringDigest(CommonMethods.splitString(toolFreezeTransaction.getFileHash()),
								6));
				detailsGridPane.add(new Label("Upgrade file hash:"), 0, count);
				detailsGridPane.add(fileHashLabel, 1, count);
				break;
			case FREEZE_UPGRADE:
				// Freezes the network at the specified time and performs the previously prepared automatic
				// upgrade across the entire network.
			case TELEMETRY_UPGRADE:
				// Performs an immediate upgrade on auxilary services and containers providing
				// telemetry/metrics. Does not impact network operations.
				startTimeLabel = getTimeLabel(toolFreezeTransaction.getStartTime(), true);
				startTimeLabel.setWrapText(true);
				detailsGridPane.add(new Label("Upgrade start:"), 0, count);
				detailsGridPane.add(startTimeLabel, 1, count++);
				fileIDLabel = new Label(toolFreezeTransaction.getFileID().toNicknameAndChecksum(new JsonObject()));
				detailsGridPane.add(new Label("Upgrade file:"), 0, count);
				detailsGridPane.add(fileIDLabel, 1, count++);
				fileHashLabel.setText(
						CommonMethods.splitStringDigest(CommonMethods.splitString(toolFreezeTransaction.getFileHash()),
								6));
				detailsGridPane.add(new Label("Upgrade file hash:"), 0, count);
				detailsGridPane.add(fileHashLabel, 1, count);
				break;
			case FREEZE_ABORT:
				// Aborts a pending network freeze operation.
				break;
			default:
				throw new IllegalStateException("Unexpected value: " + freezeType);
		}
	}

	@Override
	public String execute(final Pair<String, KeyPair> pair, final String user,
			final String output) throws HederaClientException {
		try {
			moveToHistory(Actions.ACCEPT, getCommentArea().getText(), pair.getLeft());
			setHistory(true);
		} catch (final HederaClientException e) {
			logger.error(e);
		}

		final var keyName = FilenameUtils.getBaseName(pair.getLeft());
		final var tempStorage =
				new File(System.getProperty("java.io.tmpdir"),
						LocalDate.now().toString()).getAbsolutePath() + "/Transaction/" + keyName;
		final var finalZip = new File(new File(System.getProperty("java.io.tmpdir"), LocalDate.now().toString()),
				this.getBaseName() + "-" + keyName + ".zip");

		final var tempTxFile =
				tempStorage + File.separator + this.getBaseName() + "." + TRANSACTION_EXTENSION;
		final var signatureFile =
				tempStorage + File.separator + this.getBaseName() + "." + SIGNATURE_EXTENSION;

		try {
			final var value = pair.getValue();
			if (value == null || !isValid()) {
				return "";
			}

			if (new File(tempStorage).exists()) {
				FileUtils.cleanDirectory(new File(tempStorage));
			}

			if (new File(tempStorage).mkdirs()) {
				logger.info("Created temp folder {}", tempStorage);
			}

			// Store a copy of the transaction
			transaction.store(tempTxFile);

			// Sign the transaction;
			final var privateKey = PrivateKey.fromBytes(value.getPrivate().getEncoded());
			final var signaturePair = new SignaturePair(privateKey.getPublicKey(), transaction.sign(privateKey));
			signaturePair.write(signatureFile);

			final var toPack = new File[] { new File(tempTxFile), new File(signatureFile) };

			for (final var file : toPack) {
				assert file.exists();
				assert file.isFile();
			}

			final var packed = Arrays.toString(toPack);
			logger.info("Packing {} to {}.zip", packed, tempStorage);
			ZipUtil.packEntries(toPack, finalZip);

			for (final var file : toPack) {
				Files.deleteIfExists(file.toPath());
				logger.info("Delete {}", file.getName());
			}

			FileUtils.deleteDirectory(new File(tempStorage));

			final var outputFile = new File(output + File.separator + user, finalZip.getName());
			Files.deleteIfExists(outputFile.toPath());
			FileUtils.moveFile(finalZip, outputFile);
			return outputFile.getAbsolutePath();
		} catch (final IOException e) {
			logger.error(e);
			throw new HederaClientException(e);
		}

	}

	@Override
	public List<FileActions> getActions() {
		return actions;
	}

	@Override
	public Set<ByteString> getSigningPublicKeys() {
		try {
			return transaction.getSigningKeys(ACCOUNTS_INFO_FOLDER);
		} catch (final Exception e) {
			logger.error(e.getMessage());
		}
		return super.getSigningPublicKeys();
	}



	private int setAccountAmounts(final GridPane detailsGridPane, int count,
			final List<Pair<String, String>> receivers) {
		for (final var receiver : receivers) {
			final var accountText = new Label(receiver.getLeft());
			accountText.setWrapText(true);
			accountText.setPadding(new Insets(0, 0, 0, 5));
			detailsGridPane.add(accountText, 0, count);
			final var amountText = new Label(receiver.getRight());
			amountText.setFont(COURIER_FONT);
			amountText.setStyle(CREDIT);
			if (amountText.getText().contains("-")) {
				amountText.setStyle(DEBIT);
			}
			amountText.setPadding(new Insets(0, 50, 0, 0));
			detailsGridPane.add(amountText, 1, count++);
		}
		return count;
	}

	private void displayKey(final TreeView<String> keyTreeView, final TreeView<String> oldKeyTreeView) {
		final var window = new Stage();
		final var keys = new HBox();
		keys.setPrefHeight(Region.USE_COMPUTED_SIZE);
		keys.setPrefWidth(Region.USE_COMPUTED_SIZE);
		keys.setSpacing(15);

		final var keysPaneNew = getKeyVBox("New Key", keyTreeView);

		keys.getChildren().add(keysPaneNew);

		if (oldKeyTreeView.getRoot() != null && !isHistory()) {
			final var keyPaneOld = getKeyVBox("Old Key", oldKeyTreeView);
			keys.getChildren().add(keyPaneOld);
		}

		final var okButton = new Button("CLOSE");
		okButton.setStyle(WHITE_BUTTON_STYLE);
		okButton.setOnAction(event -> window.close());
		final var layout = new VBox();
		final var hBox = new HBox();
		hBox.getChildren().add(okButton);
		hBox.setAlignment(Pos.BASELINE_RIGHT);
		layout.getChildren().addAll(keys, hBox);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setSpacing(15);
		layout.setPrefHeight(400);
		layout.setPrefWidth(800);

		keyTreeView.prefHeightProperty().bind(layout.heightProperty().subtract(55));

		final var scene = new Scene(layout);
		window.initModality(Modality.APPLICATION_MODAL);
		window.sizeToScene();
		window.setScene(scene);
		window.showAndWait();
	}

	@NotNull
	private VBox getKeyVBox(final String title, final TreeView<String> keyTreeView) {
		final var keyBox = new VBox();
		keyBox.setPrefWidth(Region.USE_COMPUTED_SIZE);
		keyBox.setPrefHeight(Region.USE_COMPUTED_SIZE);
		keyBox.setSpacing(10);

		final var titleLabel = new Label(title);
		titleLabel.setStyle("-fx-font-size: 20");

		keyBox.getChildren().add(titleLabel);
		HBox.setHgrow(keyBox, Priority.ALWAYS);

		keyTreeView.setStyle("-fx-font-size: 16");
		final var keysPane = new ScrollPane();
		keysPane.setFitToWidth(true);
		keysPane.setFitToHeight(true);
		keysPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.NEVER);
		keysPane.setContent(keyTreeView);
		keyBox.getChildren().add(keysPane);

		return keyBox;
	}


	public void setOldInfo(final JsonObject oldInfo) {
		this.oldInfo = oldInfo;
	}
}
