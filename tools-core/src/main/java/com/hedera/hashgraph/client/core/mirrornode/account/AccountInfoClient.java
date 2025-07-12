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

package com.hedera.hashgraph.client.core.mirrornode.account;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.mirrornode.dtos.AccountInfoDTO;
import com.hedera.hashgraph.client.core.mirrornode.dtos.KeyDTO;
import com.hedera.hashgraph.client.core.mirrornode.dtos.KeyType;
import com.hedera.hashgraph.client.core.mirrornode.utils.MirrorNodeREST;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.PublicKey;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import java.io.IOException;

public class AccountInfoClient {
    private static final ObjectMapper objectMapper = new ObjectMapper();

    public static AccountInfoDTO getAccountInfo(String network, String accountId) throws IOException {
        String url = MirrorNodeREST.fromBaseURL(network) + "/accounts/" + accountId;
        try (CloseableHttpClient client = HttpClients.createDefault()) {
            HttpGet request = new HttpGet(url);
            try (CloseableHttpResponse response = client.execute(request)) {
                String json = EntityUtils.toString(response.getEntity());
                return objectMapper.readValue(json, AccountInfoDTO.class);
            }
        }
    }

    public static Key getAccountKey(String network, AccountId accountId) throws IllegalArgumentException, IOException {
        return getAccountKey(network, accountId.toString());
    }

    public static Key getAccountKey(String network, String accountId) throws IllegalArgumentException, IOException {
        AccountInfoDTO accountInfo = getAccountInfo(network, accountId);
        if (accountInfo == null || accountInfo.getKey() == null) {
            throw new IllegalArgumentException("Account info or key is null for account: " + accountId);
        }
        return getKeyFromDTO(accountInfo.getKey());
    }

    private static Key getKeyFromDTO(KeyDTO dto) throws InvalidProtocolBufferException {
        // Convert KeyDTO to Key
        if (dto.getType() == KeyType.PROTOBUF_ENCODED) {
            return Key.fromBytes(CommonMethods.hexToBytes(dto.getKey()));
        }
        return PublicKey.fromString(dto.getKey());
    }
}