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

import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.ListeningExecutorService;
import com.google.common.util.concurrent.MoreExecutors;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.cli.helpers.CompletableExecutor;
import com.hedera.hashgraph.client.cli.helpers.CompletableTask;
import com.hedera.hashgraph.client.cli.helpers.DependentCompletableExecutor;
import com.hedera.hashgraph.client.cli.helpers.FileUpdateHelper;
import com.hedera.hashgraph.client.cli.helpers.IndependentCompletableExecutor;
import com.hedera.hashgraph.client.cli.helpers.OrderedCompletableExecutor;
import com.hedera.hashgraph.client.cli.helpers.OrderedExecutor;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.helpers.TransactionCallableWorker;
import com.hedera.hashgraph.client.core.helpers.TransactionCallableWorker.TxnResult;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.FileUpdateTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionResponse;
import javafx.css.CssParser;
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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.zip.ZipFile;

import static com.hedera.hashgraph.client.cli.options.SubmitCommand.TransactionIDFitness.getFitness;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;
import static java.lang.Thread.sleep;

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

	@CommandLine.Option(names = {"-s", "--system-file-update"}, description = "The flag to indicate that this is " +
			"a system file update. Only 1 system file can be updated a time. The file will only be sent to 1 node. " +
			"Any appends will be sent immediately, without waiting for a receipt. Once the file is uploaded, " +
			"verification of the resulting upload will occur, displaying the result.")
	private boolean isSystemFileUpdate = false;

	@CommandLine.Option(names = {"-r", "--restart-from-failure"}, description = "In the case of a " +
			"System File Update, if a failure during the uploading occurs, the file can be resubmitted using " +
			"this flag to indicate that the file uploaded should resume where the failure occurred, if possible. " +
			"If resuming is not possible, then the file in its entirety will be resubmitted.")
	private boolean shouldRestartFromFailure = false;


//	i can almost completely move over to completablefuture, where i use the exec to manage thread counts and stuff
//	then, if not fileid, just create completablefuture and let it go, if it has a fileid, it needs to get the
//			completablefuture from the map for the fileid/nodeid, thenapplyasyn(next append) and set that new completeablefuture
//	in the map. the only tricky part is in both types, I need to have the receipt be a separate completablefuture that runs
//	on its own (using hte exec still), that can stop the loop if needed. So how do I get a completablefuture to create
//			a new completablefuture on the side that will still have access to this class?
//maybe I can do a completablefuture().whencomplete(do receiptstuff), then the next one takes the original completeablefuture().thenapplyasync

	private IndependentCompletableExecutor independentExecutor;
	private DependentCompletableExecutor dependentExecutor;
	private OrderedCompletableExecutor orderedExecutor;

	@Override
	public void execute() throws HederaClientException, InterruptedException {
		try (final var client = CommonMethods.getClient(submissionClient)) {
			// we don't want to backoff for more than a few seconds, we only have 120s to get all transactions through
			client.setMaxBackoff(Duration.ofSeconds(5));
			client.setNodeMaxBackoff(Duration.ofSeconds(10));

			// determine if this is a system file update
			if (isSystemFileUpdate) {
				// for a system file update, only 1 file may be specified (of type zip)
				if (transactionFiles.length > 1) {
					logger.error("Only 1 system file may be submitted at a time. Please enter the path to the " +
							"system file to be submitted.");
					return;
				}
			}

			// Load transactions
			final var files = getTransactionPaths();
			if (files.isEmpty()) {
				throw new HederaClientException("No valid transactions found");
			}

			final var executor = Executors.newFixedThreadPool(this.numberOfThreads);

			// Load transactions into the priority queue
			final var transactions = setupPriorityQueue(files);
			// Submit the transactions
			var count = 0;

			// If it is a System File Update,
			// ensure that the first transaction is an update, and that it is a System File.
			if (isSystemFileUpdate) {
				final var tx = transactions.peek();
				if (tx instanceof FileUpdateTransaction) {
					final var fileId = ((FileUpdateTransaction)tx).getFileId();
					final var accountNumber = fileId.num;
					if (fileId.shard != 0) {
						logger.error("A System File Update must have a shard value of 0.");
						return;
					}
					if (fileId.realm != 0) {
						logger.error("A System File Update must have a realm value of 0.");
						return;
					}
					if (accountNumber < 150 && 159 < accountNumber) {
						logger.error("A System File Update must have an account number in the range of 150-159");
						return;
					}
				} else {
					logger.error("The first transaction to be submitted for a System File Update needs to be of " +
							"type FileUpdateTransaction.");
					return;
				}
			}

			while (!transactions.isEmpty()) {
				final var tx = transactions.poll();
				if (tx == null) {
					throw new HederaClientRuntimeException("Invalid transaction");
				}
				logger.info("Submitting transaction {} to thread pool", Objects.requireNonNull(
						tx.getTransactionId()));

//				determin which executor it needs, and lazy get/create that executor, create a completabletask and pass it in
//						up the count

				final var completableTask = new CompletableTask(tx, delay, out, client);

				if (isSystemFileUpdate) {
					getOrderedExecutor(executor).addTask(completableTask);
				} else if (tx instanceof FileUpdateTransaction || tx instanceof FileAppendTransaction) {
//					logger.info("size = " + tx.toBytes().length);
					getDependentExecutor(executor).addTask(completableTask);
				// If a FileUpdate/AppendTransaction, get the FileUpdateHelper from the
				// map and add a new CompletableFuture to the helper.
				// Otherwise, just create the CompletableFuture and let it go.
//				final var fileId = getFileId(tx);
//				if (fileId != null) {
//					final var nodes = tx.getNodeAccountIds();
//					var helper = fileUpdateHelpers.get(fileId);
//					if (helper == null) {
//						helper = new FileUpdateHelper(null, tx.getTransactionId());
//					} else if (Objects.equals(helper.getTransactionId(), tx.getTransactionId())) {
//						if this is where they get grouped, when will the last group get grouped? does it matter?
//						final var combinedFuture = helper.getCompletableFuture();
//						helper = new FileUpdateHelper(combinedFuture, tx.getTransactionId());
//						getReceipt(client, combinedFuture, executor);
//					}
//					helper.addCompletableFuture(submitTask);
				} else {
					getIndependentExecutor(executor).addTask(completableTask);
				}

				count++;
			}

			getOrderedExecutor(executor).getResults();
			getDependentExecutor(executor).getResults();
			getIndependentExecutor(executor).getResults();
			//TODO now go through and wait for each thing to finish
//			Thread.sleep(30*1000);
//
//			now go through the map and getreceipt(helper.getcompletablefuture())? that's
//		a bit ugly' - also this means the last future won't run until after the loop is done. but that's only half true
//		the combine won't finish, but all the asyns should run, right? no becuase they don't get attached to the parent?
//			actually, if I am creating a completable future (submit task) that will get run right away, where it needs to wait until after
//					so it should not create the future, but just get the function
//
//
//
//					also, is this correct? and can this approach not also work for any file? and can I somehow
//					allow for more than just one systemfile to be updated in one call, while also ensureing that the
//					systemfile is only being sent to one node?
//
//			and how can multi-node screw things up? the only thing I can think of is if a node disconnects and reconnects quickly
//					or maybe one node receives the transaction, starts teh consensus, meanwhile another node has already .....
//			i don't actually see the issue. because either both nodes will submit for consensus, and the next transaction will be submitted right
//					after for the
//
//					burst mode is a tihng, meaning non burst should wait for receipt, but burst should not
//					the difference being where the next completable gets attached. so for burst, it attaches to the first part
//					for non burst, it attaches to the second part
//
//					so make three types of tasks?
//			the first task will be 'independent' - it will create the submit and let it go, then attach a getreceipt to it
//			the second task will be 'ordered' - it will create teh submit, attach to a parent, also attach a receipt
//			the third task will be 'dependent' - it will be like ordered, but will attach to getreceipt
//
//					all of htem will need to have a 'whencomplete' thing from this class
//
//
//			// From here***************
//			// I need 2 objects (or methods, whatever).
//			// The first will be to run the submit.
//			// The second is get the receipt.
//			// There is a map for the FileUpdate/AppendTransactions.
//			// The loop will check if it is 'shutdown', if not proceed.
//			// The loop, if not a FileUpdate/AppendTransaction, will create a completableFuture to run the submit.
//			// Then it will thenApplyAsync() to add the receipt process.
//			// Then it will whenComplete() to enable the processing of the response (TxnResult type thing).
//			// The loop, if it is a FileUpdate/AppendTransaction, will get the current completableFuture for the
//			// given file and/or node (or create one if it doesn't exist) and will supplyAsync() to submit.
//			// Then it will put the completableFuture in the map.
//			// Then it will thenApplyAsync() to add the get receipt process.
//			// then it will whenComplete() to enable the processing of the response.
//			// whenComplete will be able to 'shutdown' the completableFuture chain. It also will catch any errors thrown in either of the two asyncs (for timeout or failed receipt responses).
//			// After the loop, get all completableFutures and 'get' the results to ensure all finish.
//			// Notes:
//			//   If a completableFuture fails, all whenCompletes down the chain (every branch of the chain) will catch it, make sure there is no redundancy.
//			//   Errors from the asyncs need to be descriptive and logged
//			//   whenComplete needs to track and display successful/unsuccessful counts.
//			//   whenComplete needs to know which FileUpdate/AppendTransaction was the last successful, in order restart where it left off, if necessary.
//			//   FileUpdate/AppendTransactions need to be grouped by level, then do anyOf() to get the first to pass?
//			//   A failure due to 'Duplicate' should NOT fail the whole chain
//			//   A failure in the receipt/whenComplete branches won't naturally trigger a shutdown on the whole chain, so certain failure types will need to manually fail the full completableFuture.
//
//			//   * maybe each node group could have a completableFuture that is the anyOf(), and everytime another transaction
//			//     is added to the node group, it replaces the anyOf() result. Or maybe get completableFuture for fileId, if exists, map.put(anyOf(future, new future))
//			//     and it can continually compound, until it gets to the next round down. If that's the case, it needs to know when it is starting a new level.
//			//     maybe the map<fileId, groupObject> and groupObject has a list of nodes and the future? Then I can tell if the node has been added to
//			//     the group yet or not. No, wait - fileId, nodeId, and transactionId are enough. fileId to separate initially,
//			//     then it.... no same thing, right?
//			//     map<fileId, map<transactionId, future>> the map based on fileId, then get the future based on transactionId but then I need to chain them together somehow
//			//     because they are ordered by time, I should be guaranteed that when time changes, it will be a new group
//			//     essentially, i get the future for the fileId, then I decide if I add to it, or group it together. In order to do that
//			//     I need to see if the transactionId of the transaction in the future matches hte new one. If it matches, group it up with anyof
//			//     if not, attach to it with thenapplyasync? no wait, this won't work because if thenapplyasync, then the next one comes and does
//			//     anyof, that anyof isn't attached to the previous group
//			//   * it will be an object that has 3 objects - the previous future (all futures of this group get added to the previous)
//			//     the transactionId of the current group, and the combined future (from chained anyof). when starting a new group
//			//     it will get the previous object.getfuture which returns teh combined future and that is the new groups 'previous' future
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//			// Setup threads
//			final var transactionsFutureTasks = new ArrayList<FutureTask<CompletableFuture<TxnResult>>>(files.size());
//			final var txnId = new String[files.size()];
//
//
//
//
//
////			or use these CompletableFuture completableFuture = new CompletableFuture();
////			completableFuture.whenComplete(new BiConsumer() {
////				@Override
////				public void accept(Object o, Object o2) {
////					//handle complete
////					in here, this is where i get the object (or error). i just need to test the receipt to see if it was
////							successful, if not, i can stop adding tasks and stuff, especially if submit() returns a boolean
////							where false means it is canceled, then just need to cancel all future tasks or pending tasks, etc.
////				}
////			});
//
//
//
////			i could subclass this, then override afterexecute then if any of the tasks fail (which I will have to override those as well
////			to make sure that a 'failure' includes no connection or refusal or whatever)
////			then i need to return a number for which failed for retry. if I store a map of worker to future, then I can just replace the future with the
////					getreceipt future, or count as the key? that map or what not will need to be in orderedexecutor. on failure, i need to get it from there
//			final var executorServiceTransactions = Executors.newFixedThreadPool(this.numberOfThreads);
//			final var orderedExecutor = new OrderedExecutor(executorServiceTransactions);
////			final var burstExecutor = this sill be the exec that will just keep on sending, it will need to
////					track receipt and stop all sending of future stuff if one fails
////					if it is hanging for at least a response (backoff stuff) can it hold the next one in line? basically
////					if it's sent, it's sent, if not don't send more'
////					can i keep pumping this thing full the whole time? or should I pass in the file path and let it do it itself?
////			as long as it has a threadpool, it will have to wait before another is queued, I think, right? either way, it should be fine
////					so a special exec that will send stuff, make sure it is sent, then keep sending, separately it will listen for a receipt
////					and ensure it was successful. if it finds a failure, it stops. at the very end, it needs to check file hash to ensure it was
////					actually successful.
////
////			or should i create a special submitcommand, separate from this, yes, that is likely best. or at least its own option and it will only ...
////			but the orderedexecutor can handle multiple file things, its just the restarting that could be an issue
////					well i can just do the option, and return the failure point just like yahcli
////
////
////					it does need to be an option, -s for systemfileupdate (with a -r or something for restart) that will check
////					that the file specified is a zip, then it will open the zip, put them in order somehow (memory issue?) then
////					ensure it is a special file type then it will go for it, wiht its own exec thing? meh, it can use hte same one
////					as long as it does the receipt stuff separately. but I need to ensure that a task waits until the previous one is submitted
////					which orderedexect can kinda do that
//
////			// Load transactions into the priority queue
////			final var transactions = setupPriorityQueue(files);
////			// Submit the transactions
////			var count = 0;
//
////			// If it is a System File Update,
////			// ensure that the first transaction is an update, and that it is a System File.
////			if (isSystemFileUpdate) {
////				final var tx = transactions.peek();
////				if (tx instanceof FileUpdateTransaction) {
////					final var fileId = ((FileUpdateTransaction)tx).getFileId();
////					final var accountNumber = fileId.num;
////					if (fileId.shard != 0) {
////						logger.error("A System File Update must have a shard value of 0.");
////						return;
////					}
////					if (fileId.realm != 0) {
////						logger.error("A System File Update must have a realm value of 0.");
////						return;
////					}
////					if (accountNumber < 150 && 159 < accountNumber) {
////						logger.error("A System File Update must have an account number in the range of 150-159");
////						return;
////					}
////				} else {
////					logger.error("The first transaction to be submitted for a System File Update needs to be of " +
////							"type FileUpdateTransaction.");
////					return;
////				}
////			}
//
//			while (!transactions.isEmpty()) {
//				final var tx = transactions.poll();
//				if (tx == null) {
//					throw new HederaClientRuntimeException("Invalid transaction");
//				}
//				logger.info("Submitting transaction {} to thread pool", Objects.requireNonNull(
//						tx.getTransactionId()));
//				txnId[count] = tx.getTransactionId().toString();
////				i need to create the right kind of worker? basically there are two types of file update flows
////						one that makes following things wait until it is submitted, and the other that waits until is is complete
////
////						the real issue here is, how does a failure in the submission or the following
////						receipt (once received) cancel this flow
////					I could have a flag that is checked before every submit, the workers need to be able to set this flag, though
////					and the receipt call backs, too.
////						so how does the receipt stuf fwork? should hte worker's future task be the receipt getting thing, ?'
////
////					but then just need to be sure the future thing isn't part of 'transactions future'? or should it?'
////						yes, that is waht should be added
////
////
////
////						i need exec.submit on the processing for each one
////						then when processing is done, it needs to resubmit the getreceipt to the same exec?
////						after all is sent to submit, it needs to wait on receipt of all the things
////						the submit needs to be done in order, the receipt will be by nature
////						i need a couple of things, i need the worker to be able to stop the exec when something fails
////						and i need the getreceipt to also stop exec when something fails
////						so I could just pass hte exec into the worker and it will schedule itself and can handle both things
////						so then how do I know to wait for the receipt?
////						if the worker class is a future task, when worker.get calls teh getreceipt futuretask.get()
////
////						except this worker class is in core, not cli
//
//
//				final var worker = new TransactionCallableWorker(executorServiceTransactions, tx, delay, out, client);
////				i need to do the re-submit to executor out here, so when this
////					first future gets finished, it resubmits
////
////						i don't know if that's possible
////						but if the worker is a completable, and we pass in the exec, no same issue.
////				I can perhaps subclass the ordered executor to workw ith workers
////				maybe callable instead of runnable? then don't wrap the future task?'
//
//				final var futureTask = new FutureTask<>(worker);
//				transactionsFutureTasks.add(futureTask);
//				// For FileUpdate and FileAppend, the fileId and nodeAccountIds will be used as the
//				// key and group, respectively, for the orderedExecutor.
//				if (tx instanceof FileUpdateTransaction) {
//					final var fileId = ((FileUpdateTransaction) tx).getFileId();
//					final var nodes = tx.getNodeAccountIds();
//					orderedExecutor.submit(worker, fileId, nodes);
//				} else if (tx instanceof FileAppendTransaction) {
//					final var fileId = ((FileAppendTransaction) tx).getFileId();
//					final var nodes = tx.getNodeAccountIds();
//					orderedExecutor.submit(worker, fileId, nodes);
//				} else {
//					orderedExecutor.submit(worker);
//
//					// We wait till workers are actually executing to proceed.
//					// No need to execute more if they are just going to sleep.
//					// This is left in, in the off chance that there are so many transactions as to
//					// overwhelm any queue limit the executor service may have.
//					worker.doneSleeping.await();
//				}
//
//				count++;
//			}
//
//			if (count == 0) {
//				logger.error("No valid transactions found. Terminating process.");
//				throw new HederaClientRuntimeException("No valid transactions found.");
//			}
//
//			final Set<TxnResult> transactionResponses = new HashSet<>();
//			for (int x = 0; x < transactionsFutureTasks.size(); ++x) {
//				TxnResult response;
//				try {
//					response = transactionsFutureTasks.get(x).get().get();
//				} catch (final Exception e) {
//					logger.error("Submit Thread Error Result", e);
//					response = new TxnResult(txnId[x], false, txnId[x] + " - Threw Severe Exception");
//				}
//				transactionResponses.add(response);
//			}
//			executorServiceTransactions.shutdown();
//			executorServiceTransactions.awaitTermination(1000000, TimeUnit.DAYS);
//
//			transactionResponses.forEach(t -> {
//				if (t.wasSuccessful()) {
//					logger.debug(t.getMessage());
//				} else {
//					logger.warn(t.getMessage());
//				}
//			});
//			logger.info("Transactions succeeded: {} of {}",
//					transactionResponses.stream().filter(TxnResult::wasSuccessful).count(), transactionResponses.size());
		} catch (final TimeoutException e) {
			logger.error(e.getMessage());
			throw new HederaClientException(e);
		}
	}

	private CompletableExecutor getIndependentExecutor(final Executor executor) {
		if (independentExecutor == null) {
			independentExecutor = new IndependentCompletableExecutor(executor);
		}
		return independentExecutor;
	}

	private CompletableExecutor getDependentExecutor(final Executor executor) {
		if (dependentExecutor == null) {
			dependentExecutor = new DependentCompletableExecutor(executor);
		}
		return dependentExecutor;
	}

	private CompletableExecutor getOrderedExecutor(final Executor executor) {
		if (orderedExecutor == null) {
			orderedExecutor = new OrderedCompletableExecutor(executor);
		}
		return orderedExecutor;
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

	private CompletableFuture<TransactionResponse> createSubmitTask(final Client client,
																	final Transaction<? extends Transaction<?>> tx,
																	final ExecutorService executor) {
		return new CompletableFuture<>().supplyAsync(() -> {
			if (tx == null) {
				throw new HederaClientRuntimeException("Null transaction");
			}
			if (tx.getTransactionId() == null) {
				throw new HederaClientRuntimeException("Invalid transaction ID");
			}
			if (tx.getTransactionValidDuration() == null) {
				throw new HederaClientRuntimeException("Invalid transaction valid duration");
			}
			final var startTime = Objects.requireNonNull(tx.getTransactionId()).validStart;
			if (startTime == null) {
				throw new HederaClientRuntimeException("Invalid start time");
			}
			if (Objects.requireNonNull(startTime.plusSeconds(
							tx.getTransactionValidDuration().toSeconds())).isBefore(Instant.now())) {
				throw new HederaClientRuntimeException("Transaction happens in the past.");
			}

			final var idString = tx.getTransactionId().toString();

			final var difference = Instant.now().getEpochSecond() - startTime.getEpochSecond() - delay;
			if (difference < 0) {
				logger.info("Transactions occur in the future. Sleeping for {} second(s)", -difference);
				try {
					Thread.sleep(-1000 * difference);
				} catch (InterruptedException ex) {
					logger.warn("Transaction " + idString + " interrupted while waiting for valid start time.");
					throw new CompletionException(ex);
				}
			}

			logger.info("Submitting transaction {} to sdk", idString);

			TransactionResponse response;
			try {
				response = tx.execute(client);
				if (response == null) {
					throw new HederaClientException("Response is null");
				}
			} catch (Exception ex) {
				logger.warn("Error executing transaction "
						+ idString + " and/or getting receipt from sdk. Exception -", ex);
				throw new CompletionException(ex);
			}
			return response;
		}, executor);
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
