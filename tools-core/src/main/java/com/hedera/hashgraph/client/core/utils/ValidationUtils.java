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

package com.hedera.hashgraph.client.core.utils;

import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.proto.TransactionBody;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class ValidationUtils {

    public static boolean validateSignature(Transaction<?> tx, PublicKey publicKey) throws IllegalAccessException {
        for (Map<PublicKey, byte[]> innerMap : tx.getSignatures().values()) {
            if (!innerMap.containsKey(publicKey)) {
                continue;
            }
            return validateSignature(tx, publicKey, innerMap.get(publicKey));
        }
        return false;
    }

    public static boolean validateSignature(Transaction<?> tx, PublicKey publicKey, byte[] signatureBytes) throws IllegalAccessException {
        byte[] data = Objects.requireNonNull(getInnerTransactions(tx)).toByteArray();
        return publicKey.verify(data, signatureBytes);
    }

    // Helper method to extract the body bytes. This si
    public static TransactionBody getInnerTransactions(Transaction<?> tx) throws IllegalAccessException {
        Field field = getDeclaredFieldFromHierarchy(tx.getClass(), "sourceTransactionBody");
        if (field != null) {
            return (TransactionBody) field.get(tx);
        } else {
            System.err.println("Field 'sourceTransactionBody' not found in the class hierarchy.");
        }
        return null;
    }

    public static Field getDeclaredFieldFromHierarchy(Class<?> clazz, String fieldName) {
        Class<?> current = clazz;
        while (current != null) {
            try {
                Field field = current.getDeclaredField(fieldName);
                field.setAccessible(true);
                return field;
            } catch (NoSuchFieldException e) {
                current = current.getSuperclass();
            }
        }
        return null;
    }

    public static List<PublicKey> validateSignatures(Transaction<?> tx) {
        List<Transaction.SignableNodeTransactionBodyBytes> nodeBodyList = tx.getSignableNodeBodyBytesList();
        Map<AccountId, Map<PublicKey, byte[]>> signatures = tx.getSignatures();

        if (nodeBodyList.isEmpty()) {
            throw new IllegalArgumentException("Transaction has no signable node bodies");
        }
        if (signatures.isEmpty()) {
            throw new IllegalArgumentException("Transaction has no signatures");
        }

        Iterator<Map<PublicKey, byte[]>> it = signatures.values().iterator();
        Set<PublicKey> referenceKeys = it.next().keySet();
        while (it.hasNext()) {
            if (!it.next().keySet().equals(referenceKeys)) {
                throw new IllegalStateException("Signature maps have inconsistent keys across nodes");
            }
        }

        final var failedSignatures = new ArrayList<PublicKey>();
        for (PublicKey publicKey : referenceKeys) {
            if (!hasValidSignaturesForKey(nodeBodyList, signatures, publicKey)) {
                failedSignatures.add(publicKey);
            }
        }

        return failedSignatures;
    }

    public static boolean hasValidSignaturesForKey(Transaction<?> tx, PublicKey publicKey) {
        List<Transaction.SignableNodeTransactionBodyBytes> nodeBodyList = tx.getSignableNodeBodyBytesList();
        Map<AccountId, Map<PublicKey, byte[]>> signatures = tx.getSignatures();

        if (nodeBodyList.isEmpty() || signatures.isEmpty()) {
            return false;
        }

        return hasValidSignaturesForKey(nodeBodyList, signatures, publicKey);
    }

    private static boolean hasValidSignaturesForKey(
            List<Transaction.SignableNodeTransactionBodyBytes> nodeBodyList,
            Map<AccountId, Map<PublicKey, byte[]>> signatures,
            PublicKey publicKey
    ) {
        for (Transaction.SignableNodeTransactionBodyBytes entry : nodeBodyList) {
            Map<PublicKey, byte[]> signaturesForNode = signatures.get(entry.getNodeID());
            if (signaturesForNode == null) {
                return false;
            }
            byte[] sigBytes = signaturesForNode.get(publicKey);
            if (sigBytes == null || !publicKey.verify(entry.getBody(), sigBytes)) {
                return false;
            }
        }
        return true;
    }
}
