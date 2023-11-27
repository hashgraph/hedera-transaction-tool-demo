package com.hedera.hashgraph.client.cli.helpers;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.FileId;
import com.hedera.hashgraph.sdk.FileUpdateTransaction;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransactionResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executor;

// This class is designed to group each level within a chain, enabling the entirety of the submission
// to process faster. I think it an unnecessary burden, for little advantage.
public class GroupedOrderedCompletableExecutor extends CompletableExecutor {
    private static final Logger LOGGER = LogManager.getLogger(GroupedOrderedCompletableExecutor.class);

    private final Map<FileId, OrderedTask> taskMap = new HashMap<>();

    public GroupedOrderedCompletableExecutor(final Executor executor) {
        super(executor);
    }

//    we want to shutdown this executor thing when:
//    transactions are identified as out of order (receipt.timestamp?)
    @Override
    public void addTask(CompletableTask task) {
//        if (delegate.isShutdown()) return;
        final var transaction = task.getTransaction();
        final var fileId = getFileId(transaction);
        // Whether null or not, get the OrderedTask from the map
        var orderedTask = taskMap.get(fileId);
        if (orderedTask == null) {
            final var previousTask = new CompletableFuture<>();
            previousTask.complete(null);
            orderedTask = new OrderedTask(previousTask,
                    task.getTransaction().getTransactionId(), executor);
            orderedTask.addCompletableTask(task);
            taskMap.put(fileId, orderedTask);
        } else {
            if (Objects.equals(task.getTransaction().getTransactionId(), orderedTask.getTransactionId())) {
                orderedTask.addCompletableTask(task);
            } else {
                final var previousTask = orderedTask.getCompletableFuture();
//                previousTask.thenApplyAsync(task::getResult, executor)
//                        .whenCompleteAsync((result, t) -> {
//                            if (t != null) {
//                                LOGGER.error("Submit Thread Error Result", t);
//                                results.add(new TransactionResult(task.getIdString(), false,
//                                        task.getIdString() + " - Threw Severe Exception"));
//                            } else {
//                                results.add(result);
//                            }
//                        }, executor);
//                orderedTask.getCompletedReceipt().whenCompleteAsync((result, t) -> {
//                    if (t != null || receipt == null) {
//                            LOGGER.error("Submit Thread Error Result", t);
//                            results.add(new TransactionResult(task.getIdString(), false,
//                                    task.getIdString() + " - Threw Severe Exception"));
//                        } else {
//                            // Only store the response if the receipt status is SUCCESS (should only be
//                            // one SUCCESS per transactionId)
//                            if (Status.SUCCESS.equals(receipt.status)) {
//                                try {
//                                    task.storeResponse(receipt);
//                                } catch (HederaClientException ex) {
//                                    LOGGER.error(String.format("Could not save receipt for transaction {}",
//                                            task.getIdString()), ex);
//                                    return;
//                                }
//                            }
//                            results.add(task.getResult(receipt));
//                        }
//                });
                orderedTask = new OrderedTask(previousTask, task.getTransaction().getTransactionId(), executor);
                orderedTask.addCompletableTask(task);
                taskMap.put(fileId, orderedTask);
            }
        }
    }

    @Override
    public List<TransactionResult> getResults() {
//        taskMap.values().forEach(orderedTask ->
//                orderedTask.getCompletedReceipt().whenComplete((result, t) -> {
//                    add to results
//                }));

        return super.getResults();
    }

    private FileId getFileId(final Transaction<?> transaction) {
        if (transaction instanceof FileUpdateTransaction) {
            return ((FileUpdateTransaction) transaction).getFileId();
        } else if (transaction instanceof FileAppendTransaction) {
            return ((FileAppendTransaction) transaction).getFileId();
        } else {
            return null;
        }
    }

    private static class OrderedTask {
        private final CompletableFuture<Object> previousFuture;
        private final TransactionId transactionId;
        private final Executor executor;
        private final List<CompletableFuture<TransactionResponse>> completableFutures;
        private final List<CompletableFuture<TransactionReceipt>> receipts;

        public OrderedTask(final CompletableFuture<Object> previousFuture,
                                final TransactionId transactionId,
                                final Executor executor) {
            this.previousFuture = previousFuture;
            this.transactionId = transactionId;
            this.executor = executor;
            completableFutures = new ArrayList<>();
            receipts = new ArrayList<>();
        }

        public TransactionId getTransactionId() {
            return transactionId;
        }

        public CompletableFuture<Object> getCompletableFuture() {
            final var combinedFuture = CompletableFuture.anyOf(
                    completableFutures.toArray(CompletableFuture[]::new));
            final var newFuture = previousFuture.thenComposeAsync(
                    response -> combinedFuture, executor);
            return newFuture;
        }

        public CompletableFuture<Object> getCompletedReceipt() {
            final var combinedFuture = CompletableFuture.anyOf(
                    receipts.toArray(CompletableFuture[]::new));
            return combinedFuture;
        }

        public void addCompletableTask(final CompletableTask task) {
//            final var newCompletableFuture = previousFuture.thenApplyAsync(response -> {
//                try {
//                    return task.submitTransaction();
//                } catch (Exception ex) {
//                    LOGGER.error(ex);
//                    return null;
//                }
//            }, executor);
//            completableFutures.add(newCompletableFuture);
//            final var receiptCompletableFuture =
//                    newCompletableFuture.thenApplyAsync(response -> {
//                        try {
//                            return task.getReceipt(response);
//                        } catch (Exception ex) {
//                            throw new CompletionException(ex);
//                        }
//                    }, executor);
//            receipts.add(receiptCompletableFuture);
        }
    }
}
