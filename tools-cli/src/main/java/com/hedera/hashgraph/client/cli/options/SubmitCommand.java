/*
 * Hedera Transaction Tool
 *
 * Copyright (C) 2018 - 2022 Hedera Hashgraph, LLC
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

package com.hedera.hashgraph.client.cli.options;

import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.cli.helpers.OrderedExecutor;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.helpers.TransactionCallableWorker;
import com.hedera.hashgraph.client.core.helpers.TransactionCallableWorker.TxnResult;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.FileUpdateTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.zeroturnaround.zip.ZipUtil;
import picocli.CommandLine;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.zip.ZipFile;

import static com.hedera.hashgraph.client.cli.options.SubmitCommand.TransactionIDFitness.getFitness;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;

@CommandLine.Command(name = "submit", aliases = { "sbm" }, description = "Submit transaction(s)")
public class SubmitCommand implements ToolCommand, GenericFileReadWriteAware {
	private static final Logger logger = LogManager.getLogger(SubmitCommand.class);

	@CommandLine.Option(names = {"-t", "--file-with-transaction"}, arity = "1..*", description = "The path(s) to " +
			"the transaction file(s) or directory that contains transaction files", required = true)
	private String[] transactionFiles;

	@SuppressWarnings("FieldMayBeFinal")
	@CommandLine.Option(names = {"-n", "--network"}, description = "The Hedera network the transaction will be " +
			"submitted to (one of MAINNET, PREVIEWNET, TESTNET, or the name of the custom network)")
	private String submissionClient = "mainnet";

	@CommandLine.Option(names = {"-o", "--output-directory"}, description = "The path to the folder where the " +
			"receipt(s) of the transaction(s) will be stored")
	private String out = System.getProperty("user.dir") + File.separator + "out" + File.separator;

	@CommandLine.Option(names = {"-d", "--delay"}, description = "Delay in seconds between the transaction valid " +
			"start and the actual submission time (Default 5 seconds)")
	private int delay = 5;

	@CommandLine.Option(names = {"-i", "--interval"}, description = "Wake up time. If transactions will be " +
			"submitted in the future, the app will wait until there are this amount of seconds left before waking up")
	private int readyTime = 1;

	@CommandLine.Option(names = {"-threads", "--number-of-threads"}, description =
			"The number of threads to use to submit transactions. Defaults to 500. This value can be large because the threads do not"
					+ " do much work other than wait on sockets. We need it to be large because many transactions can require submission"
					+ " at the same moment and a low number could cause transactions to expire before the tool has a chance to submit"
					+ " them. The main limit is that nodes limit the number of simultaneous connections.")
	private int numberOfThreads = 500;

	@Override
	public void execute() throws HederaClientException, InterruptedException {
		try (final var client = CommonMethods.getClient(submissionClient)) {
			// we don't want to backoff for more than a few seconds, we only have 120s to get all transactions through
			client.setMaxBackoff(Duration.ofSeconds(5));
			client.setNodeMaxBackoff(Duration.ofSeconds(10));

			// Load transactions
			final var files = getTransactionPaths();
			if (files.isEmpty()) {
				throw new HederaClientException("No valid transactions found");
			}

			// Setup threads
			final var transactionsFutureTasks = new ArrayList<FutureTask<TxnResult>>(files.size());
			final var txnId = new String[files.size()];
			final var executorServiceTransactions = Executors.newFixedThreadPool(this.numberOfThreads);
			final var orderedExecutor = new OrderedExecutor(executorServiceTransactions);

			// Load transactions into the priority queue
			final var transactions = setupPriorityQueue(files);
			// Submit the transactions
			var count = 0;

			while (!transactions.isEmpty()) {
				final var tx = transactions.poll();
				if (tx == null) {
					throw new HederaClientRuntimeException("Invalid transaction");
				}
				logger.info("Submitting transaction {} to thread pool", Objects.requireNonNull(
						tx.getTransactionId()));
				txnId[count] = tx.getTransactionId().toString();
				final TransactionCallableWorker worker = new TransactionCallableWorker(tx, delay, out, client);
				final var futureTask = new FutureTask<>(worker);
				transactionsFutureTasks.add(futureTask);
				// For FileUpdate and FileAppend, the fileId and nodeAccountIds will be used as the
				// key and group, respectively, for the orderedExecutor.
				if (tx instanceof FileUpdateTransaction) {
					final var fileId = ((FileUpdateTransaction) tx).getFileId();
					final var nodes = tx.getNodeAccountIds();
					orderedExecutor.submit(futureTask, fileId, nodes);
				} else if (tx instanceof FileAppendTransaction) {
					final var fileId = ((FileAppendTransaction) tx).getFileId();
					final var nodes = tx.getNodeAccountIds();
					orderedExecutor.submit(futureTask, fileId, nodes);
				} else {
					orderedExecutor.submit(futureTask);

					// We wait till workers are actually executing to proceed.
					// No need to execute more if they are just going to sleep.
					// This is left in, in the off chance that there are so many transactions as to
					// overwhelm any queue limit the executor service may have.
					worker.doneSleeping.await();
				}

				count++;
			}

			if (count == 0) {
				logger.error("No valid transactions found. Terminating process.");
				throw new HederaClientRuntimeException("No valid transactions found.");
			}

			final Set<TxnResult> transactionResponses = new HashSet<>();
			for (int x = 0; x < transactionsFutureTasks.size(); ++x) {
				TxnResult response;
				try {
					response = transactionsFutureTasks.get(x).get();
				} catch (final Exception e) {
					logger.error("Submit Thread Error Result", e);
					response = new TxnResult(txnId[x], false, txnId[x] + " - Threw Severe Exception");
				}
				transactionResponses.add(response);
			}
			executorServiceTransactions.shutdown();
			executorServiceTransactions.awaitTermination(1000000, TimeUnit.DAYS);

			transactionResponses.forEach(t -> {
				if (t.wasSuccessful()) {
					logger.debug(t.getMessage());
				} else {
					logger.warn(t.getMessage());
				}
			});
			logger.info("Transactions succeeded: {} of {}",
					transactionResponses.stream().filter(TxnResult::wasSuccessful).count(), transactionResponses.size());
		} catch (final TimeoutException e) {
			logger.error(e.getMessage());
			throw new HederaClientException(e);
		}
	}

	private PriorityQueue<Transaction<?>> setupPriorityQueue(
			final Set<String> files) throws HederaClientException {
		final var transactions = new PriorityQueue<Transaction<?>>(files.size(), (o1, o2) -> {
			if (o1.getTransactionId() == null || o2.getTransactionId() == null) {
				throw new HederaClientRuntimeException("Invalid transaction ID");
			}
			// Check if the transactionIds are the same AND the nodeAccountIds. If they are both the same
			// then an error must be thrown. If the nodeAccountIds are different, then assume this is due to
			// purposeful submission of a transaction to multiple nodes.
			if (Objects.equals(o1.getTransactionId(), o2.getTransactionId())
					&& Objects.equals(o1.getNodeAccountIds(), o2.getNodeAccountIds())) {
				throw new HederaClientRuntimeException("Duplicate transaction IDs being submitted to the same Node");
			}

			final var vs1 = Objects.requireNonNull(o1.getTransactionId()).validStart;
			final var vs2 = Objects.requireNonNull(o2.getTransactionId()).validStart;
			if (vs1 == null || vs2 == null) {
				throw new HederaClientRuntimeException("Invalid transaction valid start");
			}
			return vs1.compareTo(vs2);
		});

		for (final var file : files) {
			final var txBytes = readBytes(file);
			final Transaction<? extends Transaction<?>> tx;

			try {
				tx = Transaction.fromBytes(txBytes);
				if (tx == null) {
					throw new HederaClientRuntimeException("Invalid transaction");
				}
			} catch (final InvalidProtocolBufferException e) {
				logger.error(e.getMessage());
				throw new HederaClientException(e);
			}
			switch (getFitness(tx)) {
				case NULL_ID:
					logger.error("Invalid transaction {}: No valid id found", file);
					break;
				case NULL_VALID_START:
					logger.error("Invalid transaction {}: No valid start time found", file);
					break;
				case EXPIRED_START:
					logger.error("Transaction {} happens in the past. Skipped.", file);
					break;
				case ID_OK:
					logger.info("Transaction {} added to queue", file);
					transactions.add(tx);
					break;
				default:
					throw new IllegalStateException("Unexpected value: " + getFitness(tx));
			}
		}
		return transactions;
	}

	private Set<String> getTransactionPaths() {
		final Set<String> files = new HashSet<>();
		for (final var fileInput : transactionFiles) {
			// wildcards first
			if (fileInput.contains("*")) {
				handleWildCards(files, fileInput);
				continue;
			}
			final var file = new File(fileInput);
			try {
				files.addAll(buildFileList(file));
			} catch (IOException e) {
				logger.error(e);
			}
		}
		return files;
	}

	private Set<String> buildFileList(File file) throws IOException {
		Set<String> fileList = new HashSet<>();
		Files.walkFileTree(file.toPath(), new SimpleFileVisitor<Path>() {
			@Override
			public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) {
				var f = path.toFile();
				try (var zip = new ZipFile(f)) {
					final var temp = Files.createTempDirectory("TransactionToolSubmit").toFile();
					temp.deleteOnExit();
					ZipUtil.unpack(f, temp);
					fileList.addAll(buildFileList(temp));
				} catch (IOException e) {
					if (isTransaction(f) && f.exists()) {
						fileList.add(f.getAbsolutePath());
					}
				}
				return FileVisitResult.CONTINUE;
			}
		});
		return fileList;
	}

	private void handleWildCards(final Set<String> files, final String fileInput) {
		final var dir = fileInput.substring(0, fileInput.lastIndexOf("/"));
		final var currentDirectory = new File(dir);
		final var fileList = currentDirectory.list(
				new WildcardFileFilter(fileInput.substring(fileInput.lastIndexOf("/") + 1)));
		if (fileList == null) {
			throw new HederaClientRuntimeException("Invalid file list");
		}
		Arrays.stream(fileList).map(fileName -> new File(dir, fileName)).filter(file -> !file.isHidden()).forEach(
				file -> {
					try {
						files.addAll(buildFileList(file));
					} catch (IOException e) {
						logger.error(e);
					}
				});
	}

	private boolean isTransaction(final File file) {
		return SIGNED_TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(file.getName()));
	}

	public enum TransactionIDFitness {
		NULL_ID,
		NULL_VALID_START,
		EXPIRED_START,
		ID_OK;

		public static TransactionIDFitness getFitness(final Transaction<?> tx) {
			if (tx.getTransactionId() == null) {
				return NULL_ID;
			}
			if (tx.getTransactionId().validStart == null) {
				return NULL_VALID_START;
			}
			if (tx.getTransactionValidDuration() == null) {
				return NULL_ID;
			}
			if (tx.getTransactionId().validStart
					.plusSeconds(tx.getTransactionValidDuration().toSeconds())
					.isBefore(Instant.now())) {
				return EXPIRED_START;
			}
			return ID_OK;
		}
	}
}
