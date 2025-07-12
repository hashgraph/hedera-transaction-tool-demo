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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.hedera.hashgraph.client.core.mirrornode.utils.EpochSecondDurationDeserializer;
import com.hedera.hashgraph.client.core.mirrornode.utils.EpochSecondInstantDeserializer;

import java.time.Duration;
import java.time.Instant;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AccountInfoDTO {
    @JsonProperty("account")
    private String account;

    @JsonProperty("auto_renew_period")
    @JsonDeserialize(using = EpochSecondDurationDeserializer.class)
    private Duration autoRenewPeriod;

    @JsonProperty("balance")
    private BalanceDTO balance;

    @JsonProperty("created_timestamp")
    private String createdTimestamp;

    @JsonProperty("deleted")
    private Boolean deleted;

    @JsonProperty("expiry_timestamp")
    @JsonDeserialize(using = EpochSecondInstantDeserializer.class)
    private Instant expiryTimestamp;

    @JsonProperty("key")
    private KeyDTO key;

    @JsonProperty("max_automatic_token_associations")
    private Integer maxAutomaticTokenAssociations;

    @JsonProperty("memo")
    private String memo;

    @JsonProperty("receiver_sig_required")
    private Boolean receiverSigRequired;

    @JsonProperty("alias")
    private String alias;

    @JsonProperty("ethereum_nonce")
    private Long ethereumNonce;

    @JsonProperty("evm_address")
    private String evmAddress;

    @JsonProperty("decline_reward")
    private Boolean declineReward;

    @JsonProperty("staked_account_id")
    private String stakedAccountId;

    @JsonProperty("staked_node_id")
    private Long stakedNodeId;

    @JsonProperty("stake_period_start")
    @JsonDeserialize(using = EpochSecondInstantDeserializer.class)
    private Instant stakePeriodStart;

    @JsonProperty("pending_reward")
    private Long pendingReward;

    public String getAccount() {
        return account;
    }

    public void setAccount(String account) {
        this.account = account;
    }

    public Duration getAutoRenewPeriod() {
        return autoRenewPeriod;
    }

    public void setAutoRenewPeriod(Duration autoRenewPeriod) {
        this.autoRenewPeriod = autoRenewPeriod;
    }

    public BalanceDTO getBalance() {
        return balance;
    }

    public void setBalance(BalanceDTO balance) {
        this.balance = balance;
    }

    public String getCreatedTimestamp() {
        return createdTimestamp;
    }

    public void setCreatedTimestamp(String createdTimestamp) {
        this.createdTimestamp = createdTimestamp;
    }

    public Boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(Boolean deleted) {
        this.deleted = deleted;
    }

    public Instant getExpiryTimestamp() {
        return expiryTimestamp;
    }

    public void setExpiryTimestamp(Instant expiryTimestamp) {
        this.expiryTimestamp = expiryTimestamp;
    }

    public KeyDTO getKey() {
        return key;
    }

    public void setKey(KeyDTO key) {
        this.key = key;
    }

    public Integer getMaxAutomaticTokenAssociations() {
        return maxAutomaticTokenAssociations;
    }

    public void setMaxAutomaticTokenAssociations(Integer maxAutomaticTokenAssociations) {
        this.maxAutomaticTokenAssociations = maxAutomaticTokenAssociations;
    }

    public String getMemo() {
        return memo;
    }

    public void setMemo(String memo) {
        this.memo = memo;
    }

    public Boolean getReceiverSigRequired() {
        return receiverSigRequired;
    }

    public void setReceiverSigRequired(Boolean receiverSigRequired) {
        this.receiverSigRequired = receiverSigRequired;
    }

    public String getAlias() {
        return alias;
    }

    public void setAlias(String alias) {
        this.alias = alias;
    }

    public Long getEthereumNonce() {
        return ethereumNonce;
    }

    public void setEthereumNonce(Long ethereumNonce) {
        this.ethereumNonce = ethereumNonce;
    }

    public String getEvmAddress() {
        return evmAddress;
    }

    public void setEvmAddress(String evmAddress) {
        this.evmAddress = evmAddress;
    }

    public Boolean getDeclineReward() {
        return declineReward;
    }

    public void setDeclineReward(Boolean declineReward) {
        this.declineReward = declineReward;
    }

    public String getStakedAccountId() {
        return stakedAccountId;
    }

    public void setStakedAccountId(String stakedAccountId) {
        this.stakedAccountId = stakedAccountId;
    }

    public Long getStakedNodeId() {
        return stakedNodeId;
    }

    public void setStakedNodeId(Long stakedNodeId) {
        this.stakedNodeId = stakedNodeId;
    }

    public Instant getStakePeriodStart() {
        return stakePeriodStart;
    }

    public void setStakePeriodStart(Instant stakePeriodStart) {
        this.stakePeriodStart = stakePeriodStart;
    }

    public Long getPendingReward() {
        return pendingReward;
    }

    public void setPendingReward(Long pendingReward) {
        this.pendingReward = pendingReward;
    }
}