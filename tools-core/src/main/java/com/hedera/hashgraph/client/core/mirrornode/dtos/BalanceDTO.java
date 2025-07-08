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

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;

public class BalanceDTO {
    @JsonProperty("timestamp")
    private String timestamp;

    @JsonProperty("balance")
    private Long balance;

    @JsonProperty("tokens")
    private List<TokenBalanceDTO> tokens;

    public String getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(String timestamp) {
        this.timestamp = timestamp;
    }

    public Long getBalance() {
        return balance;
    }

    public void setBalance(Long balance) {
        this.balance = balance;
    }

    public List<TokenBalanceDTO> getTokens() {
        return tokens;
    }

    public void setTokens(List<TokenBalanceDTO> tokens) {
        this.tokens = tokens;
    }
}