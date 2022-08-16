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

package com.hedera.hashgraph.client.core.helpers;

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransactionReceiptQuery;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.time.Instant;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;

import static java.lang.Thread.sleep;

public class TransactionCallableWorker implements Callable<String>, GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(TransactionCallableWorker.class);

	private final Transaction<?> tx;
	private final int delay;
	private final String location;
	private final Client client;

	public final CountDownLatch doneSleeping = new CountDownLatch(1);

	public TransactionCallableWorker(final Transaction<?> tx, final int delay, final String location,
			final Client client) {
		this.tx = tx;
		this.delay = delay;
		this.location = location;
		this.client = client;
	}

	@Override
	public String call() throws Exception {
		try {
			return process();
		} finally {
			doneSleeping.countDown();
		}
	}

	public String process() throws Exception {
		if (tx == null) {
			throw new HederaClientRuntimeException("Null transaction");
		}
		if (tx.getTransactionValidDuration() == null) {
			throw new HederaClientRuntimeException("Invalid transaction valid duration");
		}
		if (Objects.requireNonNull(
				Objects.requireNonNull(Objects.requireNonNull(tx.getTransactionId()).validStart).plusSeconds(
						tx.getTransactionValidDuration().toSeconds())).isBefore(Instant.now())) {
			throw new HederaClientRuntimeException("Transaction happens in the past.");
		}

		sleepUntilNeeded();

		final var idString = Objects.requireNonNull(tx.getTransactionId()).toString();

		try {

			Status status = null;
			TransactionReceipt receipt = null;

			try {
				logger.info("Submitting transaction {} to sdk", idString);

				var response = tx.execute(client);
				if (response == null) {
					throw new HederaClientException("Response is null");
				}

				logger.info("Fetching receipt for transaction {} from sdk", idString);

				receipt = response.getReceipt(client);

				if (receipt != null && receipt.status != null) {
					status = receipt.status;
					logger.info("got receipt from sdk for {}, with status {}", idString, status);
				} else {
					logger.warn("sdk receipt was null or receipt status was null for {}", idString);
				}

			} catch (final Exception ex) {
				logger.warn("Error executing transaction " + idString + " and/or getting receipt from sdk.", ex);
			}

			long delay = 1000;
			boolean forceRetry = false;

			while (true) {

				if (forceRetry || receipt == null || (!Status.SUCCESS.equals(status))) {

					logger.info("Trying to manually fetch receipt for transaction {}", idString);

					try {
						var receiptQuery = new TransactionReceiptQuery()
								.setTransactionId(tx.getTransactionId());

						if (tx.getNodeAccountIds() != null) {
							receiptQuery.setNodeAccountIds(tx.getNodeAccountIds());
						}

						logger.info("Submitting manual receipt query for transaction {} to sdk", idString);

						var newReceipt = receiptQuery.execute(client);

						if (newReceipt != null && newReceipt.status != null) {
							receipt = newReceipt;
							status = receipt.status;
							logger.info("got receipt for {}, with status {}", idString, status);
						} else {
							logger.warn("receipt or receipt status was null for {}", idString);
						}

					} catch (final Exception ex) {
						logger.error("Failed to manually get the transaction receipt for " + idString, ex);
					}

				}

				forceRetry = true;

				if (Status.SUCCESS.equals(status)) {
					break;
				} else if (delay > 30000) {
					logger.warn("Giving up trying to get receipt for {}, current status is {}", idString, status);
					break;
				} else {
					logger.warn("Got status {}, will retry getting receipt for {} in {}ms", status, idString, delay);
					sleep(delay);
					delay *= 1.5;
				}
			}


			if (receipt != null) {

				logger.info("Worker: Transaction: {}, final status: {}", idString, status);

				final var storageLocation = storeResponse(receipt, idString);
				if (Status.SUCCESS.equals(status)) {
					return storageLocation;
				}
			} else {
				throw new HederaClientRuntimeException("could not get receipt for " + idString);
			}
		} catch (final Exception e) {
			logger.error("Worker: Transaction: " + idString + ", failed with error. ", e);
		}
		return "";
	}

	private void sleepUntilNeeded() throws InterruptedException {
		if (tx == null) {
			throw new HederaClientRuntimeException("Invalid transaction");
		}
		if (tx.getTransactionId() == null) {
			throw new HederaClientRuntimeException("Invalid transaction ID");
		}
		final var startTime = Objects.requireNonNull(tx.getTransactionId()).validStart;
		if (startTime == null) {
			throw new HederaClientRuntimeException("Invalid start time");
		}
		final var difference = Instant.now().getEpochSecond() - startTime.getEpochSecond() - delay;
		if (difference < 0) {
			logger.info("Transactions occur in the future. Sleeping for {} second(s)", -difference);
			sleep(-1000 * difference);
		}

		doneSleeping.countDown();
	}


	private String storeResponse(final TransactionReceipt receipt,
			final String idString) throws HederaClientException {
		final var filePath =
				location + File.separator + idString.replace(".", "_") + "." + Constants.RECEIPT_EXTENSION;
		writeBytes(filePath, receipt.toBytes());
		logger.info("Worker: TransactionID {} - Receipt stored to {}", idString, filePath);
		return filePath;
	}
}
