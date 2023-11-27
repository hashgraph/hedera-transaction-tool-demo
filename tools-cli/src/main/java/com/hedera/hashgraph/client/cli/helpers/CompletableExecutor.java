package com.hedera.hashgraph.client.cli.helpers;

import com.hedera.hashgraph.sdk.Transaction;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executor;

public abstract class CompletableExecutor {
    protected final Executor executor;
    protected final List<TransactionResult> results;

    public CompletableExecutor(final Executor executor) {
        this.executor = executor;
        results = new ArrayList<>();
    }

    public List<TransactionResult> getResults() {
        return results;
    }

    // Add a task. Each implementation will do this differently (as to if/when to link tasks, etc.)
    public abstract void addTask(final CompletableTask task);
}
