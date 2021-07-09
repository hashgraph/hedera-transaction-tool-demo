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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.core.remote;

import com.google.gson.JsonObject;
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
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
import com.hedera.hashgraph.client.core.transactions.ToolSystemTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.Hbar;
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
import org.zeroturnaround.zip.ZipUtil;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.security.KeyPair;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_INFO_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.CREDIT;
import static com.hedera.hashgraph.client.core.constants.Constants.DEBIT;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.getTimeLabel;

public class TransactionFile extends RemoteFile implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(TransactionFile.class);
	public static final String UNBREAKABLE_SPACE = "\u00A0";
	private static final Font COURIER_FONT = Font.font("Courier", 17);

	private ToolTransaction transaction;
	private TransactionType transactionType;
	private Timestamp expiration;
	private TreeView<String> treeView = new TreeView<>();

	private final List<FileActions> actions =
			Arrays.asList(FileActions.SIGN, FileActions.DECLINE, FileActions.ADD_MORE, FileActions.BROWSE);


	public TransactionFile() {
		super();
	}

	public TransactionFile(FileDetails fileDetails) {
		super(fileDetails);
		if (!isValid() || !FileType.TRANSACTION.equals(getType())) {
			setValid(false);
			return;
		}
		try {
			var toolTransaction = new ToolTransaction();
			transaction = toolTransaction.parseFile(new File(fileDetails.getFullPath()));
		} catch (HederaClientException e) {
			logger.error(e.getMessage());
			setValid(false);
			return;
		}
		transactionType = transaction.getTransactionType();

		var tvs = transaction.getTransactionValidStart();
		var tvd = transaction.getTransactionValidDuration();

		expiration = new Timestamp(tvs.getEpochSecond() + tvd.getSeconds(), tvs.getNano() + tvd.getNano());
	}

	public ToolTransaction getTransaction() {
		return transaction;
	}

	public void setTreeView(TreeView<String> treeView) {
		this.treeView = treeView;
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

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		return super.equals(o);
	}

	@Override
	public boolean isExpired() {
		var now = new Timestamp();
		return now.getSeconds() > getExpiration().getSeconds();
	}

	@Override
	public GridPane buildGridPane() {
		var detailsGridPane = super.buildGridPane();
		var nicknames = new JsonObject();
		var keysLink = new Hyperlink("Click for more details");
		var sigReqLabel = new Label("Receiver signature required: ");
		sigReqLabel.setWrapText(true);

		try {
			var map =
					(new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();
			final var feePayerLabel = new Label(CommonMethods.nicknameOrNumber(transaction.getFeePayerID(), map));
			feePayerLabel.setWrapText(true);
			detailsGridPane.add(feePayerLabel, 1, 0);
		} catch (HederaClientException e) {
			logger.error(e);
		}

		var text = new Text(transaction.getTransactionFee().toString().replace(" ", UNBREAKABLE_SPACE));
		text.setFont(COURIER_FONT);
		text.setFill(Color.RED);
		detailsGridPane.add(text, 1, 1);

		if (!"".equals(transaction.getMemo())) {
			detailsGridPane.add(new Label("Memo: "), 0, 2);
			detailsGridPane.add(new Label(transaction.getMemo()), 1, 2);
		}

		var timeLabel = getTimeLabel(new Timestamp(transaction.getTransactionValidStart()), true);
		timeLabel.setWrapText(true);
		detailsGridPane.add(timeLabel, 1, 3);


		try {
			nicknames = (new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(
					Constants.ACCOUNTS_MAP_FILE) : new JsonObject();
		} catch (HederaClientException e) {
			logger.error(e);
		}
		var count = detailsGridPane.getRowCount() + 1;
		switch (transaction.getTransactionType()) {
			case CRYPTO_TRANSFER:
				var accountAmountMap =
						((ToolTransferTransaction) transaction).getAccountAmountMap();
				List<Pair<String, String>> senders = new ArrayList<>();
				List<Pair<String, String>> receivers = new ArrayList<>();
				for (Map.Entry<Identifier, Hbar> entry : accountAmountMap.entrySet()) {
					var amount = entry.getValue();
					var identifier = entry.getKey();
					if (amount.toTinybars() < 0) {
						senders.add(Pair.of(CommonMethods.nicknameOrNumber(identifier, nicknames),
								amount.toString().replace(" ", UNBREAKABLE_SPACE)));
					} else {
						receivers.add(Pair.of(CommonMethods.nicknameOrNumber(identifier, nicknames),
								amount.toString().replace(" ", UNBREAKABLE_SPACE)));
					}
				}

				detailsGridPane.add(new Label("Senders: "), 0, count++);
				setAccountAmounts(detailsGridPane, count++, senders);

				detailsGridPane.add(new Label("Receivers: "), 0, count++);
				setAccountAmounts(detailsGridPane, count++, receivers);
				break;
			case CRYPTO_CREATE:
				var createTransaction = (ToolCryptoCreateTransaction) this.transaction;
				detailsGridPane.add(new Label("Key: "), 0, count);
				keysLink.setOnAction(actionEvent -> displayKey(treeView));
				detailsGridPane.add(keysLink, 1, count++);

				detailsGridPane.add(new Label("Auto renew period: "), 0, count);
				detailsGridPane.add(
						new Label(String.format("%s seconds", createTransaction.getAutoRenewDuration().getSeconds()))
						, 1, count++);

				detailsGridPane.add(new Label("Initial balance: "), 0, count);
				var initialBalance = new Label(createTransaction.getInitialBalance().toString());
				initialBalance.setFont(COURIER_FONT);
				initialBalance.setStyle(DEBIT);
				detailsGridPane.add(initialBalance, 1, count++);

				detailsGridPane.add(sigReqLabel, 0, count);
				detailsGridPane.add(new Label(String.format("%s", createTransaction.isReceiverSignatureRequired())), 1,
						count);

				break;
			case CRYPTO_UPDATE:
				var updateTransaction = (ToolCryptoUpdateTransaction) this.transaction;

				if (updateTransaction.getKey() != null) {
					detailsGridPane.add(new Label("Key: "), 0, count);
					keysLink.setOnAction(actionEvent -> displayKey(treeView));
					detailsGridPane.add(keysLink, 1, count++);
				}

				if (updateTransaction.getAutoRenewDuration() != null) {
					detailsGridPane.add(new Label("Auto renew period: "), 0, count);
					detailsGridPane.add(
							new Label(String.format("%s seconds",
									updateTransaction.getAutoRenewDuration().getSeconds()))
							, 1, count++);
				}


				if (updateTransaction.isReceiverSignatureRequired() != null) {
					detailsGridPane.add(sigReqLabel, 0, count);
					detailsGridPane.add(new Label(String.format("%s", updateTransaction.isReceiverSignatureRequired())),
							1,
							count);
				}
				break;
			case SYSTEM_DELETE_UNDELETE:
				var toolSystemTransaction = (ToolSystemTransaction) transaction;
				var isDelete = toolSystemTransaction.isDelete();
				var isFile = toolSystemTransaction.isFile();

				detailsGridPane.add(new Label(isFile ? "File ID: " : "Contract ID: "), 0, count);
				detailsGridPane.add(new Label(toolSystemTransaction.getEntity().toReadableString()), 1,
						count++);

				if (isDelete) {
					var subLabel = new Label((isFile ? "File" : "Contract") + " will expire on: ");
					subLabel.setWrapText(true);
					detailsGridPane.add(subLabel, 0, count);
					var expirationTimeLabel = getTimeLabel(new Timestamp(toolSystemTransaction.getExpiration()),
							true);
					expirationTimeLabel.setWrapText(true);
					detailsGridPane.add(expirationTimeLabel, 1, count++);
				}
				break;

			default:
				logger.error("Unrecognized transaction type {}", transaction.getTransactionType());
		}

		return detailsGridPane;
	}

	@Override
	public String execute(Pair<String, KeyPair> pair, String user, String output) throws HederaClientException {
		try {
			moveToHistory(Actions.ACCEPT, getCommentArea().getText(), pair.getLeft());
			setHistory(true);
		} catch (HederaClientException e) {
			logger.error(e);
		}

		var keyName = FilenameUtils.getBaseName(pair.getLeft());
		var tempStorage =
				System.getProperty("java.io.tmpdir") + (LocalDate.now()).toString() + "/Transaction/" + keyName;
		var finalZip = new File(
				String.format("%s%s/%s-%s.zip", System.getProperty("java.io.tmpdir"),
						(LocalDate.now()).toString(), this.getBaseName(), keyName));

		final var tempTxFile =
				tempStorage + File.separator + this.getBaseName() + "." + Constants.TRANSACTION_EXTENSION;
		final var signatureFile =
				tempStorage + File.separator + this.getBaseName() + "." + Constants.SIGNATURE_EXTENSION;

		try {
			var value = pair.getValue();
			if (value == null || !isValid()) {
				return "";
			}

			if (new File(tempStorage).exists()) {
				FileUtils.cleanDirectory(new File(tempStorage));
			}

			if (new File(tempStorage).mkdirs()) {
				logger.info(String.format("Created temp folder %s", tempStorage));
			}

			// Store a copy of the transaction
			transaction.store(tempTxFile);

			// Sign the transaction;
			var privateKey = PrivateKey.fromBytes(value.getPrivate().getEncoded());
			var signaturePair = new SignaturePair(privateKey.getPublicKey(), transaction.sign(privateKey));
			signaturePair.write(signatureFile);

			var toPack = new File[] { new File(tempTxFile), new File(signatureFile) };

			for (var file : toPack) {
				assert file.exists();
				assert file.isFile();
			}

			logger.info("Packing " + Arrays.toString(toPack) + " to " + tempStorage + ".zip");
			ZipUtil.packEntries(toPack, finalZip);

			for (var file : toPack) {
				Files.deleteIfExists(file.toPath());
				logger.info("Delete " + file.getName());
			}

			FileUtils.deleteDirectory(new File(tempStorage));

			final var outputFile = new File(output + File.separator + user, finalZip.getName());
			if (outputFile.exists()) {
				outputFile.delete();
			}
			FileUtils.moveFile(finalZip, outputFile);
			return outputFile.getAbsolutePath();
		} catch (IOException e) {
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
		} catch (Exception e) {
			logger.error(e);
		}
		return super.getSigningPublicKeys();
	}

	private void setAccountAmounts(GridPane detailsGridPane, int count, List<Pair<String, String>> receivers) {
		for (var receiver : receivers) {
			var accountText = new Label(receiver.getLeft());
			accountText.setWrapText(true);
			accountText.setPadding(new Insets(0, 0, 0, 5));
			detailsGridPane.add(accountText, 0, count);
			var amountText = new Label(receiver.getRight());
			amountText.setFont(COURIER_FONT);
			amountText.setStyle(CREDIT);
			if (amountText.getText().contains("-")) {
				amountText.setStyle(DEBIT);
			}
			amountText.setPadding(new Insets(0, 50, 0, 0));
			detailsGridPane.add(amountText, 1, count++);
		}
	}

	private void displayKey(TreeView keyTreeView) {
		var window = new Stage();
		keyTreeView.setStyle("-fx-font-size: 16");
		var keysPane = new ScrollPane();
		keysPane.setFitToWidth(true);
		keysPane.setFitToHeight(true);
		keysPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.NEVER);
		keysPane.setContent(keyTreeView);
		var okButton = new Button("CLOSE");
		okButton.setStyle(Constants.WHITE_BUTTON_STYLE);
		okButton.setOnAction(event -> window.close());
		var layout = new VBox();
		var titleLabel = new Label("New Key");
		titleLabel.setStyle("-fx-font-size: 20");
		var hBox = new HBox();
		hBox.getChildren().add(okButton);
		hBox.setAlignment(Pos.BASELINE_RIGHT);
		layout.getChildren().addAll(titleLabel, keysPane, hBox);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setSpacing(15);
		layout.setPrefHeight(400);
		layout.setPrefWidth(800);

		keyTreeView.prefHeightProperty().bind(layout.heightProperty().subtract(55));

		var scene = new Scene(layout);
		window.initModality(Modality.APPLICATION_MODAL);
		window.sizeToScene();
		window.setScene(scene);
		window.showAndWait();
	}


}
