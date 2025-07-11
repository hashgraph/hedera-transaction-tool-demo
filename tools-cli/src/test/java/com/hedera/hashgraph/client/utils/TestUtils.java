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

package com.hedera.hashgraph.client.utils;

import com.hedera.hashgraph.sdk.*;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TestUtils {
    public static final String NODE_CREATE_TX = "src/test/resources/transactions/1751303100_0.0.2_-1623455011.tx";

    public static Key copyKeyStructureAndCollectPrivateKeys(Key key, Map<PublicKey, PrivateKey> keyMap) {
        if (key instanceof KeyList) {
            var newKeyList = KeyList.withThreshold(((KeyList) key).threshold);
            for (Key child : (KeyList) key) {
                newKeyList.add(copyKeyStructureAndCollectPrivateKeys(child, keyMap));
            }
            return newKeyList;
        } else {
            PrivateKey privateKey = PrivateKey.generateED25519();
            PublicKey pubKey = privateKey.getPublicKey();
            keyMap.put(pubKey, privateKey);
            return pubKey;
        }
    }

    public static List<PrivateKey> getMinimumKeysRequired(Key key, Map<PublicKey, PrivateKey> keyMap) {
        List<PrivateKey> signingKeys = new ArrayList<>();
        int count = 0;

        if (key instanceof KeyList) {
            KeyList keyList = (KeyList) key;
            int threshold = keyList.getThreshold();
            for (Key child : keyList) {
                if (count >= threshold) break;
                if (child instanceof PublicKey) {
                    PrivateKey privateKey = keyMap.get(child);
                    if (privateKey != null) {
                        signingKeys.add(privateKey);
                        count++;
                    }
                }
            }
            for (Key child : keyList) {
                if (count >= threshold) break;
                if (child instanceof KeyList) {
                    signingKeys.addAll(getMinimumKeysRequired(child, keyMap));
                    count++;
                }
            }
        } else if (key instanceof PublicKey) {
            PrivateKey privateKey = keyMap.get(key);
            if (privateKey != null) {
                signingKeys.add(privateKey);
            }
        } else {
            throw new IllegalArgumentException("Unsupported key type: " + key.getClass().getName());
        }
        return signingKeys;
    }

    public static Transaction<?> loadTransactionFromFile(String txFileLocation) throws Exception {
        // Load transaction bytes from resources
        File txFile = new File(txFileLocation);
        byte[] txBytes = FileUtils.readFileToByteArray(txFile);

        // Parse transaction
        return Transaction.fromBytes(txBytes);
    }

    public static Map<PublicKey, byte[]> createSignaturesWithKeys(Transaction<?> tx, List<PrivateKey> signingKeys) {
        Map<PublicKey, byte[]> signatures = new HashMap<>();
        final var txBytes = tx.toBytes();
        signingKeys.forEach(key -> {
            signatures.put(key.getPublicKey(), key.sign(txBytes));
        });
        return signatures;
    }

    public static void signTxFileWithKeysAndSave(Transaction<?> tx, List<PrivateKey> signingKeys, String outputTxsigPath) throws Exception {
        tx.freeze();

        // Sign with each key
        for (PrivateKey key : signingKeys) {
            tx.sign(key);
        }

        // Write signed transaction to output
        FileUtils.writeByteArrayToFile(new File(outputTxsigPath), tx.toBytes());
    }

    public static void addSignaturesToTransactionAndSave(Transaction<?> tx, Map<PublicKey, byte[]> signatures, String outputTxsigPath) throws Exception {
        // Sign with each key
        for (var sig : signatures.entrySet()) {
            tx.addSignature(sig.getKey(), sig.getValue());
        }

        // Write signed transaction to output
        FileUtils.writeByteArrayToFile(new File(outputTxsigPath), tx.toBytes());
    }
}