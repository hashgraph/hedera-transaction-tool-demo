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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.enums.TransactionType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.NodeCreateTransaction;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static com.hedera.hashgraph.client.core.constants.ErrorMessages.CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ADMIN_KEY_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DECLINE_REWARD_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.GOSSIP_CA_CERTIFICATE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.GOSSIP_ENDPOINTS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.GRPC_CERTIFICATE_HASH_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DAB_NODE_ACCOUNT_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.DAB_NODE_DESCRIPTION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.SERVICE_ENDPOINTS_FIELD_NAME;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.bytesToHex;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.convertCertificateBytesToString;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.convertCertificateStringToBytes;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.hexToBytes;
import static com.hedera.hashgraph.client.core.utils.CommonMethods.verifyFieldExist;

public class ToolNodeCreateTransaction  extends ToolTransaction {
    private static final Logger logger = LogManager.getLogger(ToolNodeCreateTransaction.class);

    private Identifier dabNodeAccountId;
    private String nodeDescription;
    private Key adminKey;
    private String gossipCACertificate;
    private String grpcCertificateHash;
    private List<ToolEndpoint> gossipEndpointList;
    private List<ToolEndpoint> serviceEndpointList;
    private Boolean declineReward;

    public ToolNodeCreateTransaction(final JsonObject input) throws HederaClientException {
        super(input);
        this.transactionType = TransactionType.NODE_CREATE;
    }

    public ToolNodeCreateTransaction(final File inputFile) throws HederaClientException {
        super(inputFile);
        this.dabNodeAccountId = new Identifier(((NodeCreateTransaction) transaction).getAccountId());
        this.nodeDescription = ((NodeCreateTransaction) transaction).getDescription();
        this.adminKey = ((NodeCreateTransaction) transaction).getAdminKey();
        byte[] bytes = ((NodeCreateTransaction) transaction).getGossipCaCertificate();
        if (bytes != null && bytes.length > 0) {
            this.gossipCACertificate = convertCertificateBytesToString(bytes);
        }
        bytes = ((NodeCreateTransaction) transaction).getGrpcCertificateHash();
        if (bytes != null && bytes.length > 0) {
            this.grpcCertificateHash = bytesToHex(bytes);
        }
        this.gossipEndpointList = ((NodeCreateTransaction) transaction).getGossipEndpoints().stream()
                .map(ToolEndpoint::fromSdk).collect(Collectors.toList());
        this.serviceEndpointList = ((NodeCreateTransaction) transaction).getServiceEndpoints().stream()
                .map(ToolEndpoint::fromSdk).collect(Collectors.toList());
        this.declineReward = ((NodeCreateTransaction) transaction).getDeclineReward();
        setTransactionType(TransactionType.NODE_CREATE);
    }

    public Identifier getDabNodeAccountId() {
        return dabNodeAccountId;
    }

    public String getNodeDescription() {
        return nodeDescription;
    }

    public Key getAdminKey() {
        return adminKey;
    }

    public String getGossipCACertificate() {
        return gossipCACertificate;
    }

    public String getGrpcCertificateHash() {
        return grpcCertificateHash;
    }

    public List<ToolEndpoint> getGossipEndpointList() {
        return gossipEndpointList;
    }

    public List<ToolEndpoint> getServiceEndpointList() { return serviceEndpointList; }

    public Boolean getDeclineReward() { return declineReward; }

    @Override
    public boolean checkInput(final JsonObject input) {
        var answer = super.checkInput(input);

        if (!verifyFieldExist(input, DAB_NODE_ACCOUNT_ID_FIELD_NAME, ADMIN_KEY_FIELD_NAME,
                GOSSIP_CA_CERTIFICATE_FIELD_NAME, GOSSIP_ENDPOINTS_FIELD_NAME,
                SERVICE_ENDPOINTS_FIELD_NAME)) {
            return false;
        }

        try {
            this.dabNodeAccountId = Identifier.parse(input.getAsJsonObject(DAB_NODE_ACCOUNT_ID_FIELD_NAME));
        } catch (final Exception e) {
            logger.error(CANNOT_PARSE_IDENTIFIER_ERROR_MESSAGE, DAB_NODE_ACCOUNT_ID_FIELD_NAME);
            answer = false;
        }

        try {
            var description = input.get(DAB_NODE_DESCRIPTION_FIELD_NAME);
            if (description != null) {
                this.nodeDescription = description.getAsString();
            }
        } catch (final Exception e) {
            logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, DAB_NODE_DESCRIPTION_FIELD_NAME);
            answer = false;
        }

        try {
            this.adminKey = EncryptionUtils.jsonToKey(input.getAsJsonObject(ADMIN_KEY_FIELD_NAME));
        } catch (final Exception e) {
            logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, ADMIN_KEY_FIELD_NAME);
            answer = false;
        }

        try {
            this.gossipCACertificate = input.get(GOSSIP_CA_CERTIFICATE_FIELD_NAME).getAsString();
        } catch (final Exception e) {
            logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, GOSSIP_CA_CERTIFICATE_FIELD_NAME);
            answer = false;
        }

        try {
            var hash = input.get(GRPC_CERTIFICATE_HASH_FIELD_NAME);
            if (hash != null) {
                this.grpcCertificateHash = hash.getAsString();
            }
        } catch (final Exception e) {
            logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, GRPC_CERTIFICATE_HASH_FIELD_NAME);
            answer = false;
        }

        gossipEndpointList = new ArrayList<>();
        if (!loadEndpoints(input, GOSSIP_ENDPOINTS_FIELD_NAME, gossipEndpointList)) {
            answer = false;
        }

        serviceEndpointList = new ArrayList<>();
        if (!loadEndpoints(input, SERVICE_ENDPOINTS_FIELD_NAME, serviceEndpointList)) {
            answer = false;
        }

        try {
            if (input.has(DECLINE_REWARD_FIELD_NAME)) {
                this.declineReward = input.get(DECLINE_REWARD_FIELD_NAME).getAsBoolean();
            }
        } catch (final Exception e) {
            logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, DECLINE_REWARD_FIELD_NAME);
            answer = false;
        }

        return answer;
    }

    private boolean loadEndpoints(final JsonObject input, final String fieldName,
                                  final List<ToolEndpoint> endpointList) {
        final var endpoints = input.getAsJsonArray(fieldName);
        for (final var endpoint : endpoints) {
            final var jsonObject = endpoint.getAsJsonObject();
            try {
                var toolEndpoint = ToolEndpoint.fromJson(jsonObject);
                endpointList.add(toolEndpoint);
            } catch (final Exception e) {
                logger.error(ErrorMessages.CANNOT_PARSE_ERROR_MESSAGE, fieldName);
                return false;
            }
        }
        return true;
    }

    @Override
    public Transaction<?> build() throws HederaClientRuntimeException {
        final var transactionId =
                new TransactionId(feePayerID.asAccount(), transactionValidStart);

        final var nodeCreateTransaction = new NodeCreateTransaction();

        nodeCreateTransaction
                .setTransactionId(transactionId)
                .setNodeAccountIds(Collections.singletonList(nodeID.asAccount()))
                .setMaxTransactionFee(transactionFee)
                .setTransactionMemo(memo)
                .setTransactionValidDuration(transactionValidDuration)
                .setAccountId(dabNodeAccountId.asAccount())
                .setAdminKey(adminKey)
                .setGossipCaCertificate(convertCertificateStringToBytes(gossipCACertificate))
                .setDeclineReward(declineReward);

        if (nodeDescription != null) {
            nodeCreateTransaction.setDescription(nodeDescription);
        }

        if (grpcCertificateHash != null) {
            nodeCreateTransaction.setGrpcCertificateHash(hexToBytes(grpcCertificateHash));
        }

        for (final ToolEndpoint endpoint : gossipEndpointList) {
            nodeCreateTransaction.addGossipEndpoint(endpoint.toSdk());
        }

        for (final ToolEndpoint endpoint : serviceEndpointList) {
            nodeCreateTransaction.addServiceEndpoint(endpoint.toSdk());
        }

        return nodeCreateTransaction.freeze();
    }

    @Override
    public Set<ByteString> getSigningKeys(final String accountsInfoFolder) {
        final var keysSet = super.getSigningKeys(accountsInfoFolder);
        final var keyFromTransaction = ((NodeCreateTransaction) transaction).getAdminKey();
        if (keyFromTransaction != null) {
            keysSet.addAll(EncryptionUtils.flatPubKeys(Collections.singletonList(keyFromTransaction)));
        }
        return keysSet;
    }

    @Override
    public KeyList getSigningAccountKeys(KeyList accountKeyList) throws HederaClientRuntimeException {
        final var requiredKeyList = new KeyList();
        requiredKeyList.add(accountKeyList);
        final var keyFromTransaction = ((NodeCreateTransaction) transaction).getAdminKey();
        if (keyFromTransaction != null) {
            requiredKeyList.add(keyFromTransaction);
        }
        return requiredKeyList;
    }

    @Override
    public boolean equals(final Object obj) {
        if (!super.equals(obj) || !(obj instanceof ToolNodeCreateTransaction)) {
            return false;
        }

        final var other = (ToolNodeCreateTransaction) obj;
        return Objects.equals(this.dabNodeAccountId, other.dabNodeAccountId) &&
                Objects.equals(this.nodeDescription, other.nodeDescription) &&
                Objects.equals(this.adminKey, other.adminKey) &&
                Objects.equals(this.gossipCACertificate, other.gossipCACertificate) &&
                Objects.equals(this.grpcCertificateHash, other.grpcCertificateHash) &&
                Objects.equals(this.gossipEndpointList, other.gossipEndpointList) &&
                Objects.equals(this.serviceEndpointList, other.serviceEndpointList) &&
                Objects.equals(this.declineReward, other.declineReward);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public JsonObject asJson() {
        final var asJson = super.asJson();
        asJson.add(DAB_NODE_ACCOUNT_ID_FIELD_NAME, dabNodeAccountId.asJSON());
        asJson.add(ADMIN_KEY_FIELD_NAME, EncryptionUtils.keyToJson(adminKey));
        asJson.addProperty(GOSSIP_CA_CERTIFICATE_FIELD_NAME, gossipCACertificate);
        if (nodeDescription != null) {
            asJson.addProperty(DAB_NODE_DESCRIPTION_FIELD_NAME, nodeDescription);
        }
        if (grpcCertificateHash != null) {
            asJson.addProperty(GRPC_CERTIFICATE_HASH_FIELD_NAME, grpcCertificateHash);
        }

        var array = new JsonArray();
        for (final ToolEndpoint endpoint : gossipEndpointList) {
            array.add(endpoint.asJson());
        }
        asJson.add(GOSSIP_ENDPOINTS_FIELD_NAME, array);

        array = new JsonArray();
        for (final ToolEndpoint endpoint : serviceEndpointList) {
            array.add(endpoint.asJson());
        }
        asJson.add(SERVICE_ENDPOINTS_FIELD_NAME, array);

        if (declineReward != null) {
            asJson.addProperty(DECLINE_REWARD_FIELD_NAME, declineReward);
        }

        return asJson;
    }
}
