package com.hedera.hashgraph.client.ui.utilities;

import com.hedera.hashgraph.client.core.transactions.ToolTransaction;

public abstract class SubmitTask<T> extends ProgressTask<T> {
    private final ToolTransaction transaction;
    private boolean successful = false;
    private Object result;

    protected SubmitTask(final ToolTransaction transaction) {
        this.transaction = transaction;
    }

    @Override
    public String getDescription() {
        return transaction.getTransaction().getTransactionId().toString();
    }

    public ToolTransaction getTransaction() {
        return transaction;
    }

    public boolean isSuccessful() {
        return successful;
    }

    public void setSuccessful(final boolean successful) {
        this.successful = successful;
    }

    public Object getResult() {
        return result;
    }

    public void setResult(final Object result) {
        this.result = result;
    }
}
