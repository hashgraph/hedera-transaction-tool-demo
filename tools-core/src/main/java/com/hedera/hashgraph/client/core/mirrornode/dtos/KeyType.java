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

package com.hedera.hashgraph.client.core.mirrornode.dtos;

import com.fasterxml.jackson.annotation.JsonCreator;

public enum KeyType {
    ECDSA_SECP256K1,
    ED25519,
    PROTOBUF_ENCODED;

    @JsonCreator
    public static KeyType fromString(String value) {
        if ("ED25519".equals(value)) {
            return ED25519;
        } else if ("ECDSASecp256k1".equals(value) || "ECDSA_SECP256K1".equals(value)) {
            return ECDSA_SECP256K1;
        } else if ("ProtobufEncoded".equals(value)) {
            return PROTOBUF_ENCODED;
        } else {
            throw new IllegalArgumentException("Unknown key type: " + value);
        }
    }
}