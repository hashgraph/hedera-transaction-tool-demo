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
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.FileUpdateTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.text.Text;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.KeyStoreException;
import java.util.zip.ZipInputStream;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

public class LargeBinaryFileTest extends TestBase implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(LargeBinaryFileTest.class);

	@Before
	public void setUp() throws Exception {
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}

	}

	@Test
	public void constructor_test() throws IOException, HederaClientException {
		var emptyFile = new LargeBinaryFile();
		assertFalse(emptyFile.isValid());

		final var file = new File("src/test/resources/Files/largeBinaryTests/largeBinaryTest.zip");
		var info = FileDetails.parse(file);

		var largeBinary = new LargeBinaryFile(info);
		assertNotNull(largeBinary);
		assertTrue(largeBinary.isValid());

		assertEquals(4, largeBinary.getActions().size());
		assertTrue(largeBinary.getActions().contains(FileActions.SIGN));
		assertTrue(largeBinary.getActions().contains(FileActions.DECLINE));
		assertTrue(largeBinary.getActions().contains(FileActions.ADD_MORE));
		assertTrue(largeBinary.getActions().contains(FileActions.BROWSE));

		var badFile = new File("src/test/resources/Files/0.0.2.meta");
		var badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/2jsons.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/2bins.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/badBin.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/badFileID.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/badJson.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/badNode.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/badPayer.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/badStart.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/invalidFile.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/invalidNode.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/invalidPayer.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/noFileID.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/noNode.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/noPayer.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/largeBinaryTests/noStart.zip");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new LargeBinaryFile(badFileDetails).isValid());

	}

	@Test
	public void buildGridPane_test() throws IOException, HederaClientException {
		final var file = new File("src/test/resources/Files/largeBinaryTests/largeBinaryTest.zip");
		var info = FileDetails.parse(file);

		var largeBinary = new LargeBinaryFile(info);
		var gridPane = largeBinary.buildGridPane();
		assertEquals(2, gridPane.getColumnCount());
		assertEquals(20, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);

		var label = (Label) gridPane.getChildren().get(3);
		assertEquals("0.0.56-kqmmh", label.getText());

		var text = (Text) gridPane.getChildren().get(4);
		assertEquals("190000000 ℏ", text.getText());

		label = (Label) gridPane.getChildren().get(6);
		assertEquals("a memo included", label.getText());

		assertTrue(gridPane.getChildren().get(9) instanceof Hyperlink);

		label = (Label) gridPane.getChildren().get(7);
		assertTrue(label.getText().contains("2022-09-03 01:00:00 UTC"));

		text = (Text) gridPane.getChildren().get(11);
		assertTrue(text.getText().contains("b55f f40e 35c6 2da5 5d93 8e4f"));

		label = (Label) gridPane.getChildren().get(13);
		assertEquals("100320 bytes", label.getText());

		label = (Label) gridPane.getChildren().get(15);
		assertEquals("1023 bytes", label.getText());

		label = (Label) gridPane.getChildren().get(17);
		assertEquals("99", label.getText());

		label = (Label) gridPane.getChildren().get(19);
		assertEquals("100 nanoseconds", label.getText());

	}

	@Test
	public void getters_test() throws IOException, HederaClientException {

		final var file = new File("src/test/resources/Files/largeBinaryTests/largeBinaryTest.zip");
		var info = FileDetails.parse(file);
		var largeBinary = new LargeBinaryFile(info);

		assertEquals("hundredThousandBytes.bin", largeBinary.getFilename());
		assertEquals("b55ff40e35c62da55d938e4f72e1bf4276e62d571c494ea05cf672ad495f349536386bbdbdd089538d8e46b7e45542d9",
				largeBinary.getChecksum().replace("\n", "").replace("\u00a0", ""));
		assertEquals(1023, largeBinary.getChunkSize());
		assertEquals(120, largeBinary.getTransactionValidDuration());
		assertEquals(new Timestamp(1662166800, 0), largeBinary.getTransactionValidStart());
		assertEquals(100, largeBinary.getValidIncrement());
		assertEquals(new Identifier(0, 0, 3), largeBinary.getNodeID());
		assertEquals(190000000L, largeBinary.getTransactionFee());
		assertEquals("a memo included", largeBinary.getMemo());
		assertEquals(1, largeBinary.getSigningAccounts().size());
		assertTrue(largeBinary.getSigningAccounts().contains(new Identifier(0, 0, 56).asAccount()));
	}

	@Test
	public void execute_test() throws IOException, HederaClientException, KeyStoreException {
		final var file = new File("src/test/resources/Files/largeBinaryTests/largeBinaryTest.zip");
		var info = FileDetails.parse(file);

		var largeBinary = new LargeBinaryFile(info);

		var password = Constants.TEST_PASSWORD.toCharArray();
		var keyStore0 = new Ed25519KeyStore.Builder().withPassword(password).build();
		keyStore0.insertNewKeyPair();

		var pair = Pair.of("testPem", keyStore0.get(0));
		final var execute = largeBinary.execute(pair, "test_user", "src/test/resources/Files/output");
		final var outputZip = new File(execute);
		assertTrue(outputZip.exists());

		unzip(outputZip);
		var unzipped = new File(outputZip.getParent(), FilenameUtils.getBaseName(outputZip.getName()));
		assertTrue(unzipped.exists());

		var transactions = unzipped.listFiles();
		assert transactions != null;
		assertEquals(100, transactions.length);

		final var update = new File(unzipped.getAbsolutePath(), "hundredThousandBytes-00000.txsig");
		assertTrue(update.exists());
		var updateTx = Transaction.fromBytes(readBytes(update));
		assertTrue(updateTx instanceof FileUpdateTransaction);

		for (var i = 1; i < 50; i++) {
			var append = new File(unzipped.getAbsolutePath(), String.format("hundredThousandBytes-%05d.txsig", i));
			assertTrue(append.exists());
			var appendTx = Transaction.fromBytes(readBytes(append));
			assertTrue(appendTx instanceof FileAppendTransaction);
		}

	}

	@After
	public void tearDown() throws Exception {
		if (new File("src/test/resources/Files/output").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/Files/output"));
		}
	}

	private void unzip(File zip) throws IOException {
		var fileZip = zip.getAbsolutePath();
		var destDir = new File(fileZip.replace(".zip", ""));
		if (destDir.mkdirs()) {
			logger.info("Destination directory created");
		}
		var buffer = new byte[1024];
		var zis = new ZipInputStream(new FileInputStream(fileZip));
		var zipEntry = zis.getNextEntry();
		while (zipEntry != null) {
			var newFile = new File(destDir, zipEntry.getName());
			var fos = new FileOutputStream(newFile);
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

	@Test
	public void chunkTooLarge_test() throws IOException {
		final var file = new File("src/test/resources/Files/largeBinaryTests/largeBinaryChunkTooBig.zip");
		var info = FileDetails.parse(file);
		Exception exception = assertThrows(HederaClientException.class, () -> new LargeBinaryFile(info));
		assertEquals("Hedera Client: Maximum chunk size is 1024 for unsigned file update transactions.", exception.getMessage());
	}
}
