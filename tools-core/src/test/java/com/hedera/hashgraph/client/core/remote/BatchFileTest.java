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
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.time.LocalDate;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class BatchFileTest extends TestBase implements GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(BatchFileTest.class);

	@Before
	public void setUp() throws Exception {
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
	}

	@Test
	public void constructor_test() throws IOException {
		var emptyFile = new BatchFile();
		assertFalse(emptyFile.isValid());

		final var file = new File("src/test/resources/Files/batchFileTests/testCSV.csv");
		var info = FileDetails.parse(file);

		var batchFile = new BatchFile(info);
		assertNotNull(batchFile);
		assertTrue(batchFile.isValid());

		assertEquals(4, batchFile.getActions().size());
		assertTrue(batchFile.getActions().contains(FileActions.SIGN));
		assertTrue(batchFile.getActions().contains(FileActions.DECLINE));
		assertTrue(batchFile.getActions().contains(FileActions.ADD_MORE));
		assertTrue(batchFile.getActions().contains(FileActions.BROWSE));

		var badFile = new File("src/test/resources/Files/batchFileTests/notACSV.csv");
		var badFileDetails = FileDetails.parse(badFile);
		FileUtils.moveFile(badFile, new File("src/test/resources/Files/batchFileTests/notACSV2.csv"));
		assertFalse(new BatchFile(badFileDetails).isValid());
		FileUtils.moveFile(new File("src/test/resources/Files/batchFileTests/notACSV2.csv"), badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/noNodes.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/noSender.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/noSendingTime.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/noTransfers.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badHours.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badMinutes.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badNode.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badSender.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badTransferAmount.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badTransferDate.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badTransferId.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/tooManyNodes.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/expiredTransaction.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertTrue(new BatchFile(badFileDetails).isExpired());
	}

	@Test
	public void checksums_test() throws IOException {
		var badFile = new File("src/test/resources/Files/batchFileTests/badSender2.csv");
		var badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badNode2.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());

		badFile = new File("src/test/resources/Files/batchFileTests/badTransferId2.csv");
		badFileDetails = FileDetails.parse(badFile);
		assertFalse(new BatchFile(badFileDetails).isValid());
	}

	@Test
	public void getters_test() throws IOException {
		final var file = new File("src/test/resources/Files/batchFileTests/testCSV.csv");
		var info = FileDetails.parse(file);
		var batchFile = new BatchFile(info);

		assertEquals(100000000L, batchFile.getTransactionFee());
		batchFile.setTransactionFee(2500000L);
		assertEquals(2500000L, batchFile.getTransactionFee());
		batchFile.setTransactionFee(-12500000L);
		assertEquals(2500000L, batchFile.getTransactionFee());


		assertEquals(1, batchFile.getSigningAccounts().size());
		assertTrue(batchFile.getSigningAccounts().contains(new Identifier(0, 0, 94).asAccount()));

		assertEquals(new Identifier(0, 0, 94), batchFile.getSenderAccountID());
		assertEquals(2, batchFile.getNodeAccountID().size());
		assertTrue(batchFile.getNodeAccountID().contains(new Identifier(0, 0, 3)));
		assertTrue(batchFile.getNodeAccountID().contains(new Identifier(0, 0, 4)));
		assertEquals(19, batchFile.getHoursUTC());
		assertEquals(30, batchFile.getMinutesUTC());
		assertEquals(LocalDate.of(2029, 9, 17), batchFile.getFirstTransaction());
		assertEquals(new Timestamp(1884367800, 0), batchFile.getFirstTransactionTimeStamp());

		assertEquals(180, batchFile.getTxValidDuration());
		batchFile.setTxValidDuration(175);
		assertEquals(175, batchFile.getTxValidDuration());
		batchFile.setTxValidDuration(-175);
		assertEquals(180, batchFile.getTxValidDuration());
		batchFile.setTxValidDuration(285);
		assertEquals(180, batchFile.getTxValidDuration());


		var transfers = batchFile.getTransfers();
		assertEquals(22, transfers.size());
	}

	@Test
	public void buildGridPane_test() throws IOException {
		final var file = new File("src/test/resources/Files/batchFileTests/testCSV.csv");
		var info = FileDetails.parse(file);

		var batchFile = new BatchFile(info);
		var gridPane = batchFile.buildGridPane();
		assertEquals(2, gridPane.getColumnCount());
		assertEquals(10, gridPane.getChildren().size());
		assertTrue(gridPane.getChildren().get(0) instanceof Label);

		var label = (Label) gridPane.getChildren().get(1);
		assertEquals("0.0.94-bbukb", label.getText());

		label = (Label) gridPane.getChildren().get(3);
		assertTrue(label.getText().contains("2029-09-17 19:30:00 UTC"));

		label = (Label) gridPane.getChildren().get(5);
		assertTrue(label.getText().contains("19:30:00 UTC"));

		label = (Label) gridPane.getChildren().get(7);
		assertEquals("0.0.3 0.0.4 ", label.getText());

		assertTrue(gridPane.getChildren().get(9) instanceof Hyperlink);
	}

	@Test
	@Ignore("Execute method must be tested using the UI. See HomePaneTests class")
	public void execute_test() {
	}

	@After
	public void tearDown() throws Exception {
		if (new File("src/test/resources/Files/output").exists()) {
			FileUtils.deleteDirectory(new File("src/test/resources/Files/output"));
		}

	}

}
