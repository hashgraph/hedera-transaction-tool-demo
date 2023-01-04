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

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransferTransaction;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.KeyStoreException;
import java.util.zip.ZipInputStream;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_KEYS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNATURE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Disabled("Temporarily disabling tests")
public class TransactionFileTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(TransactionFile.class);

	@BeforeEach
	public void setUp() throws Exception {

		// delete tools folders if it exists
		final var storage = new File(DEFAULT_STORAGE);
		if (storage.exists()) {
			FileUtils.deleteDirectory(storage);
		}

		if (storage.mkdirs()) {
			logger.info("Tools folder created");
		}

		if (new File(DEFAULT_KEYS).mkdirs()) {
			logger.info("Keys folder created");
		}
		final var files = new File("src/test/resources/PublicKeys").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(PUB_EXTENSION));
		assert files != null;
		for (final var file : files) {
			final var destFile = new File(DEFAULT_KEYS, file.getName());
			if (!destFile.exists()) {
				FileUtils.copyFile(file, destFile);
			}
		}
		if (new File(DEFAULT_ACCOUNTS).mkdirs()) {
			logger.info("Keys folder created");
		}
		final var accounts = new File("src/test/resources/AccountInfos").listFiles(
				(dir, name) -> FilenameUtils.getExtension(name).equals(INFO_EXTENSION));
		assert accounts != null;
		for (final var file : accounts) {
			final var destFile = new File(DEFAULT_ACCOUNTS, file.getName());
			if (!destFile.exists()) {
				FileUtils.copyFile(file, destFile);
			}
		}


	}

	@AfterEach
	public void tearDown() throws Exception {
		final var output = new File("src/test/resources/Files/output");
		if (output.exists()) {
			FileUtils.deleteDirectory(output);
		}
		final var storage = new File(DEFAULT_STORAGE);
		if (storage.exists()) {
			FileUtils.deleteDirectory(storage);
		}
	}

	@Test
	public void constructor_test() throws IOException, HederaClientException {
		final var emptyFile = new TransactionFile();
		assertFalse(emptyFile.isValid());

		var file = new File("src/test/resources/Files/TransactionFileTests/expired.tx");
		var info = FileDetails.parse(file);

		var transactionFile = new TransactionFile(info);
		assertNotNull(transactionFile);
		assertTrue(transactionFile.isValid());
		assertTrue(transactionFile.isExpired());

		file = new File(
				"src/test/resources/Files/TransactionFileTests/createAccount.tx");
		info = FileDetails.parse(file);
		transactionFile = new TransactionFile(info);
		assertFalse(transactionFile.isExpired());

		assertEquals(4, transactionFile.getActions().size());
		assertTrue(transactionFile.getActions().contains(FileActions.SIGN));
		assertTrue(transactionFile.getActions().contains(FileActions.DECLINE));
		assertTrue(transactionFile.getActions().contains(FileActions.ADD_MORE));
		assertTrue(transactionFile.getActions().contains(FileActions.BROWSE));

		file = new File(
				"src/test/resources/Files/RemoteFilesMapTests/TestCouncil1/InputFiles/hundredThousandBytes.lfu");
		info = FileDetails.parse(file);
		transactionFile = new TransactionFile(info);
		assertFalse(transactionFile.isValid());


		file = new File("src/test/resources/Files/TransactionFileTests/badTransactionFile.tx");
		info = FileDetails.parse(file);
		transactionFile = new TransactionFile(info);
		assertFalse(transactionFile.isValid());


	}

	@Test
	public void getters_test() throws HederaClientException {
		final var file = new File(
				"src/test/resources/Files/TransactionFileTests/createAccount.tx");
		final var info = FileDetails.parse(file);
		final var transactionFile = new TransactionFile(info);

		assertEquals("", transactionFile.getMemo());
		assertEquals(TransactionType.CRYPTO_CREATE, transactionFile.getTransactionType());
		assertEquals(new Identifier(0, 0, 94), transactionFile.getFeePayerAccountId());
		assertEquals(100000000, transactionFile.getTransactionFee());
		assertEquals(new Timestamp(1775282400, 0), transactionFile.getTransactionValidStart());

		final var transaction = transactionFile.getTransaction();
		assertTrue(transaction instanceof ToolCryptoCreateTransaction);

		assertEquals(16, transactionFile.getSigningPublicKeys().size());

	}

	@Test
	public void buildGridPaneCreate_test() throws HederaClientException {
		final var file = new File(
				"src/test/resources/Files/RemoteFilesMapTests/TestCouncil1/InputFiles/1743832800-0_0_94-58824159.tx");
		final var info = FileDetails.parse(file);

		final var createTransaction = new TransactionFile(info);
		final var gridPane = createTransaction.buildGridPane();
		assertEquals(2, gridPane.getColumnCount());
		assertEquals(28, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);

		var label = (Label) gridPane.getChildren().get(4);
		assertEquals("0.0.94-bbukb", label.getText());

		label = (Label) gridPane.getChildren().get(6);
		assertTrue(label.getText().contains("2025-04-05 06:00:00 UTC"));

		assertTrue(gridPane.getChildren().get(11) instanceof Hyperlink);

		label = (Label) gridPane.getChildren().get(13);
		assertEquals("7000000 seconds", label.getText());

		label = (Label) gridPane.getChildren().get(15);
		assertEquals("0 tℏ", label.getText());

		final var signers = createTransaction.getSigningPublicKeys();
		assertNotNull(signers);
		assertEquals(16, signers.size());
	}

	@Test
	public void buildGridPaneTransfer_test() throws HederaClientException {
		final var file = new File("src/test/resources/Files/TransactionFileTests/transferTransaction.tx");
		final var info = FileDetails.parse(file);

		final var transfer = new TransactionFile(info);
		final var gridPane = transfer.buildGridPane();
		assertEquals(2, gridPane.getColumnCount());
		assertEquals(16, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);

		var label = (Label) gridPane.getChildren().get(4);
		assertEquals("0.0.76-csasv", label.getText());

		label = (Label) gridPane.getChildren().get(6);
		assertTrue(label.getText().contains("2029-05-05 22:10:07 UTC"));

		label = (Label) gridPane.getChildren().get(8);
		assertEquals("0.0.3-tzfmz", label.getText());


		label = (Label) gridPane.getChildren().get(11);
		assertEquals("0.0.50-rlcsj", label.getText());
		label = (Label) gridPane.getChildren().get(12);
		assertEquals("-100 ℏ", label.getText());

		label = (Label) gridPane.getChildren().get(14);
		assertEquals("0.0.94-bbukb", label.getText());
		label = (Label) gridPane.getChildren().get(15);
		assertEquals("100 ℏ", label.getText());

		final var signers = transfer.getSigningPublicKeys();
		assertNotNull(signers);
		assertEquals(18, signers.size());
	}

	@Test
	public void buildGridPaneFileUpdate_test() throws HederaClientException {
		final var file = new File("src/test/resources/Files/TransactionFileTests/systemDelete.tx");
		final var info = FileDetails.parse(file);

		final var transfer = new TransactionFile(info);
		final var gridPane = transfer.buildGridPane();
		assertEquals(2, gridPane.getColumnCount());
		assertEquals(14, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);

		var label = (Label) gridPane.getChildren().get(4);
		assertEquals("0.0.2-lpifi", label.getText());

		label = (Label) gridPane.getChildren().get(6);
		assertTrue(label.getText().contains("2026-05-02 09:27:13 UTC"));

		label = (Label) gridPane.getChildren().get(8);
		assertEquals("0.0.3-tzfmz", label.getText());

		label = (Label) gridPane.getChildren().get(11);
		assertEquals("0.0.123", label.getText());

		label = (Label) gridPane.getChildren().get(13);
		assertTrue(label.getText().contains("2028-05-06 06:00:00 UTC"));

		final var signers = transfer.getSigningPublicKeys();
		assertNotNull(signers);
		assertEquals(28, signers.size());
	}

	@Test
	public void execute_test() throws IOException, HederaClientException, KeyStoreException {
		final var file = new File("src/test/resources/Files/TransactionFileTests/transferTransaction.tx");
		final var info = FileDetails.parse(file);

		final var transfer = new TransactionFile(info);
		final var password = Constants.TEST_PASSWORD.toCharArray();
		final var keyStore0 = new Ed25519KeyStore.Builder().withPassword(password).build();
		keyStore0.insertNewKeyPair();

		final var pair = Pair.of("testPem", keyStore0.get(0));
		final var execute = transfer.execute(pair, "test_user", "src/test/resources/Files/output");
		final var outputZip = new File(execute);
		assertTrue(outputZip.exists());

		unzip(outputZip);
		final var unzipped = new File(outputZip.getParent(), FilenameUtils.getBaseName(outputZip.getName()));
		assertTrue(unzipped.exists());

		final var files =
				unzipped.listFiles((dir, name) -> FilenameUtils.getExtension(name).equals(TRANSACTION_EXTENSION));
		assert files != null;
		assertEquals(1, files.length);
		final var bytes = readBytes(files[0]);
		final var updateTx = Transaction.fromBytes(bytes);
		assertTrue(updateTx instanceof TransferTransaction);
		final var sigs = updateTx.getSignatures();
		assertEquals(0, sigs.entrySet().size());

		final var sigFiles =
				unzipped.listFiles((dir, name) -> FilenameUtils.getExtension(name).equals(SIGNATURE_EXTENSION));
		assert sigFiles != null;
		assertEquals(1, sigFiles.length);
		final var signaturePair = new SignaturePair(sigFiles[0].getAbsolutePath());
		final var pubKey = signaturePair.getPublicKey();
		final var privateKey = PrivateKey.fromBytes(pair.getValue().getPrivate().getEncoded());
		assertEquals(pubKey, privateKey.getPublicKey());
	}

	private void unzip(final File zip) throws IOException {
		final var fileZip = zip.getAbsolutePath();
		final var destDir = new File(fileZip.replace(".zip", ""));
		if (destDir.mkdirs()) {
			logger.info("Destination directory created");
		}
		final var buffer = new byte[1024];
		final var zis = new ZipInputStream(new FileInputStream(fileZip));
		var zipEntry = zis.getNextEntry();
		while (zipEntry != null) {
			final var newFile = new File(destDir, zipEntry.getName());
			final var fos = new FileOutputStream(newFile);
			int len;
			while ((len = zis.read(buffer)) > 0) {
				fos.write(buffer, 0, len);
			}
			fos.close();
			zipEntry = zis.getNextEntry();
		}
		zis.closeEntry();
		zis.close();
	}
}
