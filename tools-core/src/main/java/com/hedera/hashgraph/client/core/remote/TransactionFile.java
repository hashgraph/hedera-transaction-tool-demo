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
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.ProgressPopup;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolFreezeTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolSystemTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.FXUtils;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.opencsv.CSVWriter;
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
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyPair;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.CREDIT;
import static com.hedera.hashgraph.client.core.constants.Constants.DEBIT;
import static com.hedera.hashgraph.client.core.constants.Constants.FILE_NAME_GROUP_SEPARATOR;
import static com.hedera.hashgraph.client.core.constants.Constants.FILE_NAME_INTERNAL_SEPARATOR;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNATURE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.ZIP_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_TO_UPDATE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MAX_TOKEN_ASSOCIATIONS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.getTimeLabel;

public class TransactionFile extends RemoteFile implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(TransactionFile.class);
	private static final String UNBREAKABLE_SPACE = "\u00A0";
	private static final Font COURIER_FONT = Font.font("Courier New", 17);
	private static final int NANO_INCREMENT = 10;

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

	public TransactionFile(final String location) throws HederaClientException {
		this(FileDetails.parse(new File(location)));
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

	@Override
	public Timestamp getExpiration() {
		return expiration;
	}

	public void setNetwork(final String network) {
		this.network = network;
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

		final HBox hbox = FXUtils.buildTransactionIDBox(detailsGridPane, transaction.getTransaction().getTransactionId().toString());
		detailsGridPane.add(hbox, RIGHT, 0);

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
		int count = 1;
		try {
			nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (final HederaClientException e) {
			logger.error(e);
			nicknames = new JsonObject();
		}

		final var feePayerLabel = new Label(CommonMethods.nicknameOrNumber(transaction.getFeePayerID(), nicknames));
		feePayerLabel.setWrapText(true);
		detailsGridPane.add(feePayerLabel, RIGHT, count);

		final var text = new Text(transaction.getTransactionFee().toString().replace(" ", UNBREAKABLE_SPACE));
		text.setFont(COURIER_FONT);
		text.setFill(Color.RED);
		detailsGridPane.add(text, RIGHT, ++count);

		final var timeLabel = getTimeLabel(new Timestamp(transaction.getTransactionValidStart()), true);
		timeLabel.setWrapText(true);
		detailsGridPane.add(timeLabel, RIGHT, ++count);

		if (!isHistory() && getTransactionCreationMetadata() != null
				&& !getTransactionCreationMetadata().getNodes().getList().isEmpty()) {
			final var nLabel = new Label("Transactions will be submitted to nodes: ");
			nLabel.setWrapText(true);
			detailsGridPane.add(nLabel, LEFT, ++count);

			final var nodesString = getTransactionCreationMetadata().getNodes().getList().stream()
					.map(n -> Identifier.parse(n, network).toNicknameAndChecksum(nicknames))
					.collect(Collectors.joining("\n"));

			final var scrollPane = new ScrollPane();
			scrollPane.setFitToWidth(true);
			final var label = new Label(nodesString);
			scrollPane.setMaxHeight(100);
			scrollPane.setContent(label);
			detailsGridPane.add(scrollPane, RIGHT, count);
		} else {
			detailsGridPane.add(new Label("Node:"), LEFT, ++count);
			detailsGridPane.add(new Label(transaction.getNodeID().toNicknameAndChecksum(nicknames)), RIGHT, count);
		}

		if (!"".equals(transaction.getMemo())) {
			detailsGridPane.add(new Label("Memo: "), LEFT, ++count);
			var memo = new Label(transaction.getMemo());
			memo.setWrapText(true);
			detailsGridPane.add(memo, RIGHT, count);
		}
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

		final var sigReqLabel = new Label("Receiver signature required: ");
		sigReqLabel.setWrapText(true);
		detailsGridPane.add(sigReqLabel, 0, count);
		detailsGridPane.add(new Label(String.format("%s", createTransaction.isReceiverSignatureRequired())), 1,
				count++);

		try {
			nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (final HederaClientException e) {
			logger.error(e);
			nicknames = new JsonObject();
		}

		var label = new Label("Staked account ID: ");
		label.setWrapText(true);
		detailsGridPane.add(label, 0, count);
		final var stakedAccountIdLabel = new Label(createTransaction.getStakedAccountId() == null ? "" :
				CommonMethods.nicknameOrNumber(createTransaction.getStakedAccountId(), nicknames));
		stakedAccountIdLabel.setWrapText(true);
		detailsGridPane.add(stakedAccountIdLabel, 1, count++);

		label = new Label("Staked Node ID: ");
		label.setWrapText(true);
		detailsGridPane.add(label, 0, count);
		detailsGridPane.add(new Label(createTransaction.getStakedNodeId() == null ?	"" :
				createTransaction.getStakedNodeId().toString()), 1, count++);

		label = new Label("Decline Staking Rewards: ");
		label.setWrapText(true);
		detailsGridPane.add(label, 0, count);
		detailsGridPane.add(new Label(String.format("%s", createTransaction.isDeclineStakingRewards())), 1,
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

		if (!isHistory() && getTransactionCreationMetadata() != null
				&& !getTransactionCreationMetadata().getAccounts().getList().isEmpty()) {
			final var nLabel = new Label("Accounts to update: ");
			detailsGridPane.add(nLabel, LEFT, count);

			try {
				nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			} catch (final HederaClientException e) {
				logger.error(e);
				nicknames = new JsonObject();
			}

			final var accountsString = getTransactionCreationMetadata().getAccounts().getList().stream()
					.map(n -> Identifier.parse(n, network).toNicknameAndChecksum(nicknames))
					.collect(Collectors.joining("\n"));

			final var scrollPane = new ScrollPane();
			scrollPane.setFitToWidth(true);
			final var label = new Label(accountsString);
			scrollPane.setMaxHeight(100);
			scrollPane.setContent(label);
			detailsGridPane.add(scrollPane, RIGHT, count++);
		} else {
			detailsGridPane.add(new Label("Account to update: "), LEFT, count);
			final var account = updateTransaction.getAccount();
			account.setNetworkName(network);
			detailsGridPane.add(new Label(account.toNicknameAndChecksum(nicknames)), RIGHT, count++);
		}

		final var sigReqLabel = new Label("Receiver signature required: ");
		sigReqLabel.setWrapText(true);

		final var hasOldInfo = oldInfo != null;
		final var keysLink = new Hyperlink("Click for more details");
		if (updateTransaction.getKey() != null) {
			detailsGridPane.add(new Label("Key: "), LEFT, count);
			keysLink.setOnAction(actionEvent -> displayKey(treeView, oldKey));
			detailsGridPane.add(keysLink, RIGHT, count++);
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
			count = handleAccountMemo(detailsGridPane, count, updateTransaction, hasOldInfo);
		}

		try {
			nicknames = new File(ACCOUNTS_MAP_FILE).exists() ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (final HederaClientException e) {
			logger.error(e);
			nicknames = new JsonObject();
		}

		if (updateTransaction.getStakedAccountId() != null) {
			var label = new Label("Staked account ID: ");
			label.setWrapText(true);
			detailsGridPane.add(label, LEFT, count);
			final var stakedAccountIdLabel = new Label(CommonMethods.nicknameOrNumber(updateTransaction.getStakedAccountId(), nicknames));
			stakedAccountIdLabel.setWrapText(true);
			detailsGridPane.add(stakedAccountIdLabel, RIGHT, count++);
		}

		if (updateTransaction.getStakedNodeId() != null) {
			var label = new Label("Staked Node ID: ");
			label.setWrapText(true);
			detailsGridPane.add(label, LEFT, count);
			detailsGridPane.add(new Label(updateTransaction.getStakedNodeId().toString()), RIGHT, count++);
		}

		if (updateTransaction.isDeclineStakingRewards() != null) {
			var label = new Label("Decline Staking Rewards: ");
			label.setWrapText(true);
			detailsGridPane.add(label, LEFT, count);
			detailsGridPane.add(new Label(String.format("%s", updateTransaction.isDeclineStakingRewards())), RIGHT,
					count);
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
		final var labelMAT = new Label("Maximum automatic token associations: ");
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

	private int handleAccountMemo(final GridPane detailsGridPane, int count,
			final ToolCryptoUpdateTransaction updateTransaction, final boolean hasOldInfo) {
		detailsGridPane.add(new Label("Account memo: "), 0, count);
		final var newValue = updateTransaction.getAccountMemo();
		final var labelString = (hasOldInfo && oldInfo.has(ACCOUNT_MEMO_FIELD_NAME)) ?
				String.format("%s (updated from %s)", newValue,
						oldInfo.get(ACCOUNT_MEMO_FIELD_NAME).getAsString()) :
				String.format("%s", newValue);
		final var label = new Label(labelString);
		label.setWrapText(true);
		detailsGridPane.add(label, 1, count++);
		return count;
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

		if (isDelete && toolSystemTransaction.getExpiration() != null) {
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

	/**
	 * Wrap a ToolTransaction in a simple TransactionFile in order to gain the UI functionality.
	 * @param transaction
	 * @return
	 */
	public static TransactionFile wrapToolTransaction(final ToolTransaction transaction) {
		var file = new TransactionFile();
		file.transaction = transaction;
		file.transactionType = transaction.getTransactionType();

		final var tvs = transaction.getTransactionValidStart();
		final var tvd = transaction.getTransactionValidDuration();

		file.expiration = new Timestamp(tvs.getEpochSecond() + tvd.getSeconds(), tvs.getNano() + tvd.getNano());
		file.setShowAdditionalBoxes();
		return file;
	}

	private class ExecuteGroupedTask extends Task<String> {
		static final String TRANSACTIONS_SUFFIX = FILE_NAME_GROUP_SEPARATOR + "transactions";
		static final String SIGNATURES_SUFFIX = FILE_NAME_GROUP_SEPARATOR + "signatures";
		final String keyName;
		final PrivateKey privateKey;
		final String tempStorage;
		final String user;
		final String output;
		final int max;
		long currentCount = 0L;

		public ExecuteGroupedTask(final String keyName, final PrivateKey privateKey,
								  final String user, final String output) {
			this.privateKey = privateKey;
			this.keyName = keyName;
			tempStorage =
					new File(System.getProperty("java.io.tmpdir"), keyName + File.separator +
							LocalDate.now()).getAbsolutePath() + File.separator + "Transaction";
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
			if (getTransactionCreationMetadata() == null) {
				this.max = 1;
			} else {
				final var nodeListSize = getTransactionCreationMetadata().getNodes() == null ? 1
						: getTransactionCreationMetadata().getNodes().getList().size();
				final var accountListSize = getTransactionCreationMetadata().getAccounts() == null ? 1
						: getTransactionCreationMetadata().getAccounts().getList().size();
				this.max = nodeListSize * accountListSize;
			}
		}

		@Override
		protected String call() throws Exception {
			var result = "";
			if (getTransactionCreationMetadata() == null) {
				result = processSingleTransaction();
			} else {
				final var transactionJson = transaction.asJson();
				var validStartTimestamp = new Timestamp(transactionJson.get(TRANSACTION_VALID_START_FIELD_NAME));
				if (getTransactionCreationMetadata().getNodes() != null) {
					for (final var nodeId : getTransactionCreationMetadata().getNodes().getList()) {
						transactionJson.add(NODE_ID_FIELD_NAME,
								Identifier.parse(nodeId, network).asJSON());
						var tempResult = processTransactionForNode(transactionJson, validStartTimestamp);
						if ("".equals(result)) {
							result = tempResult;
						}
					}
				} else {
					result = processTransactionForNode(transactionJson, validStartTimestamp);
				}
			}
			try {
				FileUtils.deleteDirectory(new File(tempStorage));
			} catch (IOException e) {
				logger.info(String.format("Directory could not be deleted: %s",tempStorage));
			}
			return result;
		}

		private String processTransactionForNode(final JsonObject jsonObject, final Timestamp validStartTimestamp)
				throws Exception {
			var result = "";
			long count = 0L;
			if (getTransactionCreationMetadata().getAccounts() != null) {
				var isUpdateAccountFeePayer = getTransactionCreationMetadata().isUpdateAccountFeePayer();
				// This is specific to account updates
				for (final var accountId : getTransactionCreationMetadata().getAccounts().getList()) {
					final var accountJson = Identifier.parse(accountId, network).asJSON();
					jsonObject.add(ACCOUNT_TO_UPDATE, accountJson);
					var incrementedTime = new Timestamp(validStartTimestamp.asDuration()
							.plusNanos( count++ * NANO_INCREMENT));
					jsonObject.add(TRANSACTION_VALID_START_FIELD_NAME, incrementedTime.asJSON());
					if (isUpdateAccountFeePayer) {
						jsonObject.add(FEE_PAYER_ACCOUNT_FIELD_NAME, accountJson);
					}
					var tempResult = processTransactionForAccount(jsonObject);
					// The first processed transaction will be the result
					if ("".equals(result)) {
						result = tempResult;
					}
				}
			} else {
				result = processTransactionForAccount(jsonObject);
			}
			pack(Identifier.parse(jsonObject.getAsJsonObject(NODE_ID_FIELD_NAME)).toReadableString());
			return result;
		}

		private String processTransactionForAccount(final JsonObject jsonObject)
				throws Exception {
			final var t = getTransaction(jsonObject);

			final var result = processTransaction(t);
			updateProgress(++currentCount, (long) max - 1);
			return result;
		}

		private String processTransaction(final ToolTransaction transaction) throws HederaClientException {
			final String tempTxFile = transaction.store(Path.of(tempStorage, getFileName(transaction)).toString());
			final String signatureFile = tempTxFile.replace(TRANSACTION_EXTENSION, SIGNATURE_EXTENSION);

			final var signaturePair = new SignaturePair(privateKey.getPublicKey(),
					transaction.createSignature(privateKey));
			signaturePair.write(signatureFile);

			return tempTxFile;
		}

		private String processSingleTransaction() throws HederaClientException, IOException {
			final var finalZip = new File(new File(System.getProperty("java.io.tmpdir"), LocalDate.now().toString()),
					transaction.buildFileName() + FILE_NAME_GROUP_SEPARATOR + keyName + "." + ZIP_EXTENSION);
			final String tempTxFile = transaction.store(tempStorage);
			final String signatureFile = tempTxFile.replace(TRANSACTION_EXTENSION, SIGNATURE_EXTENSION);

			final var signaturePair = new SignaturePair(privateKey.getPublicKey(),
					transaction.createSignature(privateKey));
			signaturePair.write(signatureFile);

			final var toPack = new File[] { new File(tempTxFile), new File(signatureFile) };

			Arrays.stream(toPack).filter(file -> !file.exists() || !file.isFile()).forEach(file -> {
				throw new HederaClientRuntimeException("Invalid file in file list");
			});

			final var packed = Arrays.toString(toPack);
			logger.info("Packing {} to {}.zip", packed, tempStorage);
			ZipUtil.packEntries(toPack, finalZip);

			for (final var file : toPack) {
				Files.deleteIfExists(file.toPath());
				logger.info("Delete {}", file.getName());
			}
			final var outputFile = Path.of(output, user, finalZip.getName());
			Files.deleteIfExists(outputFile);
			FileUtils.moveFile(finalZip, outputFile.toFile());

			return outputFile.toAbsolutePath().toString();
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
					dataList.add(file.getName());
					dataList.add(t.asJson().toString().replace(",", ";"));
					var data = new String[2];
					data = dataList.toArray(data);
					writer.writeNext(data);
				}
			} catch (final IOException e) {
				throw new HederaClientException(e);
			} catch (NoSuchMethodException | InvocationTargetException | InstantiationException |
					 IllegalAccessException e) {
				throw new HederaClientRuntimeException(e);
			}
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
					transaction.buildFileName(),
					keyName,
					String.format("%s%s%s", "Node", FILE_NAME_INTERNAL_SEPARATOR, nodeAccount));
		}

		private String getFileName(final ToolTransaction txn) {
			// transaction-id_account-to-update-id.[tx,sig]
			// or if no account, it will just be transaction-id.[tx,sig]
			if (txn.getTransactionType() == TransactionType.CRYPTO_UPDATE) {
				var suffixIdentifier = ((ToolCryptoUpdateTransaction)txn).getAccount().toReadableString();
				return String.format("%s%s%s.%s", txn.getTransactionId().toString(),
						FILE_NAME_GROUP_SEPARATOR, suffixIdentifier, TRANSACTION_EXTENSION);
			}
			return String.format("%s.%s", txn.getTransactionId().toString(), TRANSACTION_EXTENSION);
		}

		private ToolTransaction getTransaction(final Object obj) throws NoSuchMethodException,
				InvocationTargetException, InstantiationException, IllegalAccessException {
			final var c = transaction.getClass().getConstructor(obj.getClass());
			return c.newInstance(obj);
		}
	}
}
