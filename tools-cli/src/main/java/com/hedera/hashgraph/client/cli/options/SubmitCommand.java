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

package com.hedera.hashgraph.client.cli.options;

import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.helpers.TransactionCallableWorker;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Transaction;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import picocli.CommandLine;

import java.io.File;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;

import static com.hedera.hashgraph.client.cli.options.SubmitCommand.TransactionIDFitness.getFitness;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;
import static java.lang.Thread.sleep;

@CommandLine.Command(name = "submit", aliases = { "sbm" }, description = "Submit transaction(s)")
public class SubmitCommand implements ToolCommand, GenericFileReadWriteAware {
	Logger logger = LogManager.getLogger(SubmitCommand.class);

	@CommandLine.Option(names = { "-t", "--file-with-transaction" }, arity = "1..*", description = "The path(s) to " +
			"the transaction file(s) or directory that contains transaction files", required = true)
	private String[] transactionFiles;

	@SuppressWarnings("FieldMayBeFinal")
	@CommandLine.Option(names = { "-n", "--network" }, description = "The Hedera network the transaction will be " +
			"submitted to (one of MAINNET, PREVIEWNET, or TESTNET)")
	private String submissionClient = "mainnet";

	@CommandLine.Option(names = { "-o", "--output-directory" }, description = "The path to the folder where the " +
			"receipt(s) of the transaction(s) will be stored")
	private String out = System.getProperty("user.dir") + File.separator + "out" + File.separator;

	@CommandLine.Option(names = { "-d", "--delay" }, description = "Delay in seconds between the transaction valid " +
			"start and the actual submission time (Default 5 seconds)")
	private int delay = 5;

	@CommandLine.Option(names = { "-i", "--interval" }, description = "Wake up time. If transactions will be " +
			"submitted in the future, the app will wait until there are this amount of seconds left before waking up")
	private int readyTime = 1;

	@Override
	public void execute() throws HederaClientException, InterruptedException {

		// Setup client
		final var network = NetworkEnum.valueOf(submissionClient.toUpperCase(Locale.ROOT));
		final Client client = CommonMethods.getClient(network);

		// Load transactions
		final var files = getTransactionPaths();
		if (files.isEmpty()) {
			throw new HederaClientException("No valid transactions found");
		}

		// Setup threads
		final var transactionsFutureTasks = new FutureTask[files.size()];
		final var executorServiceTransactions = Executors.newFixedThreadPool(Constants.NUMBER_OF_THREADS);


		// Load transactions into the priority queue
		final var transactions = setupPriorityQueue(files);

		// Submit the transactions
		var count = 0;
		while (!transactions.isEmpty()) {
			sleepUntilNeeded(transactions.peek(), readyTime);
			final var tx = transactions.poll();
			assert tx != null;
			logger.info("Submitting transaction {} to network", Objects.requireNonNull(
					tx.getTransactionId()));
			final TransactionCallableWorker worker = new TransactionCallableWorker(tx, delay, out, client);
			transactionsFutureTasks[count] = new FutureTask<>(worker);
			executorServiceTransactions.submit(transactionsFutureTasks[count]);
			count++;

		}

		if (count == 0) {
			logger.error("No valid transactions found. Terminating process.");
			throw new HederaClientRuntimeException("No valid transactions found.");
		}

		final List<String> transactionResponses = new ArrayList<>();
		for (final var future : transactionsFutureTasks) {
			String response;
			try {
				response = (String) future.get();
			} catch (final ExecutionException e) {
				logger.error(e.getMessage());
				response = "";
			}
			if (!"".equals(response)) {
				transactionResponses.add(response);
			}
		}
		executorServiceTransactions.shutdown();
		while (!executorServiceTransactions.isTerminated()) {
			// wait loop
		}

		logger.info("Transactions succeeded: {} of {}", transactionResponses.size(), count);

	}

	private void sleepUntilNeeded(final Transaction<?> transaction, final int readyTime) throws InterruptedException {
		assert transaction != null;
		final var startTime = Objects.requireNonNull(transaction.getTransactionId()).validStart;
		assert startTime != null;
		final var difference = Instant.now().getEpochSecond() - startTime.getEpochSecond();
		if (difference > readyTime) {
			logger.info("Transactions occur in the future. Sleeping for {} second", difference - readyTime);
			sleep(1000 * (difference - readyTime));

		}
	}

	private PriorityQueue<Transaction<?>> setupPriorityQueue(
			final Set<String> files) throws HederaClientException {
		final var transactions = new PriorityQueue<Transaction<?>>(files.size(), (o1, o2) -> {
			final var vs1 = Objects.requireNonNull(o1.getTransactionId()).validStart;
			final var vs2 = Objects.requireNonNull(o2.getTransactionId()).validStart;
			assert vs1 != null;
			assert vs2 != null;
			return vs1.compareTo(vs2);
		});

		for (final var file : files) {
			final var txBytes = readBytes(file);
			final Transaction<? extends Transaction<?>> tx;

			try {
				tx = Transaction.fromBytes(txBytes);
				assert tx.getTransactionValidDuration() != null;
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
		final Set<File> directories = new HashSet<>();
		for (final var fileInput : transactionFiles) {
			// wildcards first
			if (fileInput.contains("*")) {
				handleWildCards(files, directories, fileInput);
				continue;
			}
			final var file = new File(fileInput);
			if (file.isDirectory()) {
				directories.add(file);
			} else if (isTransaction(file) && file.exists()) {
				files.add(file.getAbsolutePath());
			}
		}
		if (!directories.isEmpty()) {
			handleDirectories(files, directories);
		}
		return files;
	}

	private void handleDirectories(final Set<String> files, final Set<File> directories) {
		for (final var directory : directories) {
			final var transactions = directory.listFiles(this::isTransaction);
			if (transactions == null) {
				continue;
			}
			Arrays.stream(transactions).map(File::getAbsolutePath).forEach(files::add);
		}
	}

	private void handleWildCards(final Set<String> files, final Set<File> directories, final String fileInput) {
		final var dir = fileInput.substring(0, fileInput.lastIndexOf("/"));
		final var currentDirectory = new File(dir);
		final var fileList = currentDirectory.list(
				new WildcardFileFilter(fileInput.substring(fileInput.lastIndexOf("/") + 1)));
		assert fileList != null;
		Arrays.stream(fileList).map(fileName -> new File(dir, fileName)).filter(file -> !file.isHidden()).forEach(
				file -> {
					if (file.isDirectory()) {
						directories.add(file);
						return;
					}
					if (isTransaction(file) && file.exists()) {
						files.add(file.getAbsolutePath());
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
