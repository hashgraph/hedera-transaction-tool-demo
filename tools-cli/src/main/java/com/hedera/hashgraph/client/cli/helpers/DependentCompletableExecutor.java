package com.hedera.hashgraph.client.cli.helpers;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.FileId;
import com.hedera.hashgraph.sdk.FileUpdateTransaction;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransactionResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executor;

public class DependentCompletableExecutor extends CompletableExecutor {

    private static final Logger LOGGER = LogManager.getLogger(DependentCompletableExecutor.class);

    private final Map<FileId, DependentTask> taskMap = new HashMap<>();
    private CompletableFuture<TransactionReceipt> lastTask;

    public DependentCompletableExecutor(final Executor executor) {
        super(executor);
    }

    //    we want to shut down this executor thing when:
//    transactions are identified as out of order (receipt.timestamp?)
    @Override
    public void addTask(CompletableTask task) {
//        if (delegate.isShutdown()) return;
        final var transaction = task.getTransaction();
        final var fileId = getFileId(transaction);
        // Whether fileId is null or not, get the OrderedTask from the map
        var dependentTask = taskMap.get(fileId);

        if (dependentTask == null) {
            final var previousFuture = new CompletableFuture<TransactionResponse>();
            previousFuture.complete(null);
            dependentTask = new DependentTask(executor);
            taskMap.put(fileId, dependentTask);
        }

        final var future = dependentTask.addCompletableTask(task);
        //TODO
//        is this right? i need to check for failures from the whole group? ugh
//                also, if out of order, etc. if a failure, could put it into a failure list, or something
//                so that when the list shows all nodes failed, it fails,
//                then if a failed receipt followed by non failed, or out of order, fail everything for this fileid
        future.whenCompleteAsync((receipt, t) -> {
            if (t != null || receipt == null) {
                LOGGER.error("Submit Thread Error Result", t);
                results.add(new TransactionResult(task.getIdString(), false,
                        task.getIdString() + " - Threw Severe Exception"));
            } else {
                // Only store the response if the receipt status is SUCCESS (should only be
                // one SUCCESS per transactionId) - unless receipt uses the transactionId, and not the
                // receipt to this specific submit...
                if (Status.SUCCESS.equals(receipt.status)) {
                    try {
                        task.storeResponse(receipt);
                    } catch (HederaClientException ex) {
                        LOGGER.error(String.format("Could not save receipt for transaction {}",
                                task.getIdString()), ex);
                        return;
                    }
                }
                results.add(task.getResult(receipt));
            }
        }, executor);

        //TODO just for testing
        lastTask = future;
    }

    @Override
    public List<TransactionResult> getResults() {
        try {
            if (lastTask != null) {
                lastTask.get();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
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

    private static class DependentTask {
        private final Map<Object, CompletableFuture<TransactionReceipt>> nodeMap;
        private final Executor executor;

        public DependentTask(final Executor executor) {
            this.executor = executor;
            nodeMap = new HashMap<>();
        }

        public CompletableFuture<TransactionReceipt> addCompletableTask(final CompletableTask task) {
            //TODO
            // This returns a list of nodes, but it really should be a list of 1.
            final var node = task.getTransaction().getNodeAccountIds();
            var previousFuture = nodeMap.get(node);
            if (previousFuture == null) {
                // Create a new future
                previousFuture = CompletableFuture.completedFuture(null);
            }
            final var newFuture = previousFuture.thenApplyAsync(receipt -> {
                try {
                    return task.submitTransaction();
                } catch (Exception ex) {
                    LOGGER.error(String.format("Failed to submit transaction %s with error - ",
                            task.getIdString()), ex);
                    throw new CompletionException(ex);
                }
            }).thenApplyAsync(response -> {
                try {
                    return task.getReceipt(response);
                } catch (InterruptedException ex) {
                    LOGGER.error(String.format("Failed to retrieve the receipt for transaction %s with error - ",
                            task.getIdString()), ex);
                    throw new CompletionException(ex);
                }
            }, executor);
            nodeMap.put(node, newFuture);

            return newFuture;
        }
    }
}
