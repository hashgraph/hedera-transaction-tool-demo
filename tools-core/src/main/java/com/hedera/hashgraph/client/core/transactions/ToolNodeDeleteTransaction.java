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

package com.hedera.hashgraph.client.core.transactions;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.sdk.NodeDeleteTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Collections;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DAB_NODE_ID_FIELD_NAME;

public class ToolNodeDeleteTransaction  extends ToolTransaction {
    private static final Logger logger = LogManager.getLogger(ToolNodeDeleteTransaction.class);

    // This is different from the nodeID in ToolTransaction, as nodeId is a property of
    // NodeUpdateTransaction
    private long dabNodeId;

    public ToolNodeDeleteTransaction(final JsonObject input) throws HederaClientException {
        super(input);
        this.transactionType = TransactionType.NODE_DELETE;
    }

    public ToolNodeDeleteTransaction(final File inputFile) throws HederaClientException {
        super(inputFile);
        this.dabNodeId = ((NodeDeleteTransaction) transaction).getNodeId();
        setTransactionType(TransactionType.NODE_DELETE);
    }

    public long getDabNodeId() {
        return dabNodeId;
    }

    @Override
    public boolean checkInput(final JsonObject input) {
        var answer = super.checkInput(input);

        try {
            this.dabNodeId = input.get(DAB_NODE_ID_FIELD_NAME).getAsLong();
        } catch (final Exception e) {
            logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, DAB_NODE_ID_FIELD_NAME);
            answer = false;
        }

        return answer;
    }

    @Override
    public Transaction<?> build() throws HederaClientRuntimeException {
        final var transactionId =
                new TransactionId(feePayerID.asAccount(), transactionValidStart);

        final var nodeDeleteTransaction = new NodeDeleteTransaction();

        nodeDeleteTransaction
                .setTransactionId(transactionId)
                .setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
                .setMaxTransactionFee(transactionFee)
                .setTransactionMemo(memo)
                .setTransactionValidDuration(transactionValidDuration)
                .setNodeId(dabNodeId);

        return nodeDeleteTransaction.freeze();
    }

    @Override
    public boolean equals(final Object obj) {
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public JsonObject asJson() {
        final var asJson = super.asJson();
        asJson.addProperty(DAB_NODE_ID_FIELD_NAME, dabNodeId);

        return asJson;
    }
}
