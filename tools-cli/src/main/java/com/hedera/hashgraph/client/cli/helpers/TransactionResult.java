package com.hedera.hashgraph.client.cli.helpers;

import java.util.Objects;

public class TransactionResult {
    private final String transactionId;
    private final boolean success;
    private final String message;

    public TransactionResult(String transactionId, boolean success, String message) {
        this.transactionId = transactionId;
        this.success = success;
        this.message = message;
    }

    public String getTransactionId() {
        return transactionId;
    }

    public boolean wasSuccessful() {
        return success;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof TransactionResult
                && Objects.equals(((TransactionResult)obj).getTransactionId(), getTransactionId());
    }

    @Override
    public int hashCode() {
        return getTransactionId().hashCode();
    }
}
