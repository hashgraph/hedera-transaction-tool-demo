package com.hedera.hashgraph.client.cli.helpers;

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.Status;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionReceipt;
import com.hedera.hashgraph.sdk.TransactionReceiptQuery;
import com.hedera.hashgraph.sdk.TransactionResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.time.Instant;
import java.util.EnumSet;
import java.util.Objects;

import static java.lang.Thread.sleep;

public class CompletableTask implements GenericFileReadWriteAware {
    private static final Logger logger = LogManager.getLogger(CompletableTask.class);

    private final Transaction<?> transaction;
    private final int delay;
    private final String location;
    private final Client client;

    public CompletableTask(final Transaction<?> transaction,
                                     final int delay, final String location,
                                     final Client client) {
        this.transaction = transaction;
        this.delay = delay;
        this.location = location;
        this.client = client;
    }

    public Transaction getTransaction() {
        return transaction;
    }

    public String getIdString() {
        return Objects.requireNonNull(transaction.getTransactionId()).toString();
    }

    public TransactionResponse submitTransaction() throws Exception {
        if (transaction == null) {
            throw new HederaClientRuntimeException("Null transaction");
        }
        if (transaction.getTransactionId() == null) {
            throw new HederaClientRuntimeException("Invalid transaction ID");
        }
        final var startTime = transaction.getTransactionId().validStart;
        if (startTime == null) {
            throw new HederaClientRuntimeException("Invalid start time");
        }
        if (transaction.getTransactionValidDuration() == null) {
            throw new HederaClientRuntimeException("Invalid transaction valid duration");
        }
        if (startTime.plusSeconds(transaction.getTransactionValidDuration().toSeconds()).isBefore(Instant.now())) {
            throw new HederaClientRuntimeException("Transaction happens in the past.");
        }

        sleepUntilNeeded(startTime);

        final var idString = getIdString();

        logger.info("Submitting transaction {} to sdk", idString);

        final var response = transaction.execute(client);
        if (response == null) {
            throw new HederaClientRuntimeException("Response is null");
        }
        return response;
    }

    public TransactionReceipt getReceipt(final TransactionResponse response) throws InterruptedException {
        final var idString = getIdString();

        logger.info("Fetching receipt for transaction {} from sdk", idString);

        Status status = null;
        TransactionReceipt receipt = null;

        try {
            receipt = response.getReceiptQuery().setMaxAttempts(2).execute(client);

            if (receipt != null && receipt.status != null) {
                status = receipt.status;
                logger.info("got receipt from sdk for {}, with status {}", idString, status);
            } else {
                logger.warn("sdk receipt was null or receipt status was null for {}", idString);
            }
        } catch (final Exception ex) {
            logger.warn("Error executing transaction " + idString + " and/or getting receipt from sdk. Exception -", ex);
        }

        final var retryStatuses = EnumSet.of(Status.UNKNOWN, Status.OK,
                Status.BUSY, Status.FAIL_INVALID);

        long retryDelay = 1000;
        boolean forceRetry = false;

        while (true) {
            if (forceRetry || receipt == null || status == null || retryStatuses.contains(status)) {
                logger.info("Trying to manually fetch receipt for transaction {}", idString);

                try {
                    var receiptQuery = new TransactionReceiptQuery()
                            .setTransactionId(transaction.getTransactionId())
                            .setMaxAttempts(2);

                    if (transaction.getNodeAccountIds() != null) {
                        receiptQuery.setNodeAccountIds(transaction.getNodeAccountIds());
                    }

                    logger.info("Submitting manual receipt query for transaction {} to sdk", idString);

                    var newReceipt = receiptQuery.execute(client);

                    if (newReceipt != null && newReceipt.status != null) {
                        receipt = newReceipt;
                        status = receipt.status;
                        logger.info("got receipt for {}, with status {}", idString, status);
                    } else {
                        logger.warn("receipt or receipt status was null for {}", idString);
                    }

                } catch (final Exception ex) {
                    logger.error("Failed to manually get the transaction receipt for " + idString, ex);
                }

            }

            forceRetry = true;

            if (receipt != null && status != null && (!retryStatuses.contains(status))) {
                if (!Status.SUCCESS.equals(status)) {
                    throw new HederaClientRuntimeException(
                            String.format("Failed to retrieve the receipt for {} with status {}",
                                    idString, status));
                }
                break;
            } else if (retryDelay > 30000) {
                logger.warn("Giving up trying to get receipt for {}, current status is {}", idString, status);
                throw new HederaClientRuntimeException(
                        String.format("Failed to retrieve the receipt for {} within reasonable time.",
                                idString));
            } else {
                logger.warn("Got status {}, will retry getting receipt for {} in {}ms", status, idString, retryDelay);
                sleep(retryDelay);
                retryDelay *= 1.5;
            }
        }

        return receipt;
    }

    public TransactionResult getResult(final TransactionReceipt receipt) {
        final var idString = getIdString();
        final var status = receipt.status;

        logger.info("Fetching receipt for transaction {} from sdk", idString);

        if (receipt != null) {
            logger.info("Worker: Transaction: {}, final status: {}", idString, status);

            return new TransactionResult(idString, Status.SUCCESS.equals(status),
                    "Final status for " + idString + " - " + status);
        } else {
            throw new HederaClientRuntimeException("could not get receipt for " + idString);
        }
    }

    public String storeResponse(final TransactionReceipt receipt) throws HederaClientException {
        final var idString = getIdString();
        final var filePath =
                location + File.separator + idString + "." + Constants.RECEIPT_EXTENSION;
        writeBytes(filePath, receipt.toBytes());
        logger.info("Worker: TransactionID {} - Receipt stored to {}", idString, filePath);
        return filePath;
    }

    private void sleepUntilNeeded(final Instant startTime) throws InterruptedException {
        final var difference = Instant.now().getEpochSecond() - startTime.getEpochSecond() - delay;
        if (difference < 0) {
            logger.info("Transactions occur in the future. Sleeping for {} second(s)", -difference);
            sleep(-1000 * difference);
        }
    }
}
