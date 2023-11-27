package com.hedera.hashgraph.client.cli.helpers;

import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionResponse;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.function.Function;

//I don't really need a type, do i? if I do it would be the txnresult, but no
//        this doesn't need a type, nothing is returned, but in the bit that is added for receipt (should that also have to do the multinode thing?)
//that should probably return txnresult - so how do i add the receipt bit? i can just do 1 receipt, or all receipts, well, if the receipt is attached to the
//first future (the one passed in) then if that future fails, the receipt will fail, and if the future passes, the receipt will pass). so should work fine.
//the whencomplete portion will just need to somehow talk to eachother so that it will not report a bunch of fails, but will report at least one?
public class FileUpdateHelper {
    private final CompletableFuture<TransactionResponse> previousFuture;
    private final TransactionId transactionId;
    private final Executor executor;

    private List<CompletableFuture<TransactionResponse>> completableFutures;

    public FileUpdateHelper(final CompletableFuture<TransactionResponse> previousFuture,
                            final TransactionId transactionId,
                            final Executor executor) {
        this.previousFuture = previousFuture;
        this.transactionId = transactionId;
        this.executor = executor;
        completableFutures = new ArrayList<>();
    }

    public TransactionId getTransactionId() {
        return transactionId;
    }

    public CompletableFuture<TransactionResponse> getCompletableFuture() {
        final var combinedFuture = CompletableFuture.anyOf(
                completableFutures.toArray(CompletableFuture[]::new));
//        return (CompletableFuture<TransactionResponse>)previousFuture.thenComposeAsync(
//                txnResponse -> combinedFuture, executor);
        return null;
    }

//    these should be attached to the parent immediately, but then combined in anyof as well?
//    so completableFutures.add(parent.thenapplyasync(stuff, exec));
//    then getcompletablefuture still returns teh combined stuff
//    that way, these run as soon as possible, then there is no issue with the delay of the last round
    public void addCompletableFuture(final Function function) {
        completableFutures.add(previousFuture.thenApplyAsync(function, executor));
    }
}
