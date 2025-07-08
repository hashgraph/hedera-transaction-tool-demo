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

import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.proto.TransactionBody;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ValidationUtils {
    /**
     * Verifies the signatures of a transaction.
     * @param tx the transaction to verify
     * @return a list of public keys that failed verification
     * @throws IllegalAccessException
     */
    public static List<PublicKey> validateSignatures(Transaction<?> tx) throws IllegalAccessException {
        final var failedSignatures = new ArrayList<PublicKey>();
        for (Map<PublicKey, byte[]> innerMap : tx.getSignatures().values()) {
            for (Map.Entry<PublicKey, byte[]> entry : innerMap.entrySet()) {
                if (!validateSignature(tx, entry.getKey(), entry.getValue())) {
                    failedSignatures.add(entry.getKey());
                }
            }
        }
        return failedSignatures;
    }

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
}
