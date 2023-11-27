package com.hedera.hashgraph.client.cli.helpers;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.TransactionResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executor;

public class IndependentCompletableExecutor extends CompletableExecutor {
    private static final Logger LOGGER = LogManager.getLogger(IndependentCompletableExecutor.class);

    public IndependentCompletableExecutor(final Executor executor) {
        super(executor);
    }

    @Override
    public void addTask(final CompletableTask task) {
        var future = CompletableFuture.supplyAsync(() -> {
                    try {
                        return task.submitTransaction();
                    } catch (Exception ex) {
                        LOGGER.error(ex);
                        throw new CompletionException(ex);
                    }
                }, executor);
        future.thenApplyAsync(response -> {
                        try {
                            return task.getReceipt(response);
                        } catch (Exception ex) {
                            throw new CompletionException(ex);
                        }
                    }, executor)
                .whenCompleteAsync((receipt, t) -> {
                    if (t != null || receipt == null) {
                        LOGGER.error("Submit Thread Error Result", t);
                        results.add(new TransactionResult(task.getIdString(), false,
                                task.getIdString() + " - Threw Severe Exception"));
                    } else {
                        // Only store the response if the receipt status is SUCCESS (should only be
                        // one SUCCESS per transactionId)
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
    }
}
