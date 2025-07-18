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

package com.hedera.hashgraph.client.core.mirrornode.utils;

public class MirrorNodeREST {
    private static final String VERSION = "/api/v1";
    public static final String MAINNET = "https://mainnet-public.mirrornode.hedera.com";
    public static final String TESTNET = "https://testnet.mirrornode.hedera.com";
    public static final String PREVIEWNET = "https://previewnet.mirrornode.hedera.com";
    public static final String LOCAL_NODE = "http://host.docker.internal:5551";

    public static String fromBaseURL(String mirrorNetwork) {
        switch (mirrorNetwork.toUpperCase()) {
            case "MAINNET":
                return MAINNET + VERSION;
            case "TESTNET":
                return TESTNET + VERSION;
            case "PREVIEWNET":
                return PREVIEWNET + VERSION;
            case "LOCAL_NODE":
                return LOCAL_NODE + VERSION;
            default:
                return "https://" + mirrorNetwork + VERSION;
        }
    }
}