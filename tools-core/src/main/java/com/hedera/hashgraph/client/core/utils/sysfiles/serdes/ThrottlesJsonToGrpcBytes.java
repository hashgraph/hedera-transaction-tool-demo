/*
 * Copyright (C) 2021-2023 Hedera Hashgraph, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hedera.hashgraph.client.core.utils.sysfiles.serdes;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.utils.sysfiles.ThrottleDefinitionsPojo;
import com.hedera.hashgraph.sdk.proto.ThrottleDefinitions;

public class ThrottlesJsonToGrpcBytes implements SysFileSerde<String> {
    private final ObjectMapper mapper = new ObjectMapper();

    @Override
    public String fromRawFile(byte[] bytes) {
        try {
            var defs = ThrottleDefinitions.parseFrom(bytes);
            var pojo = ThrottleDefinitionsPojo.fromProto(defs);
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(pojo);
        } catch (InvalidProtocolBufferException | JsonProcessingException e) {
            throw new IllegalArgumentException("Unusable raw throttle definitions!", e);
        }
    }

    @Override
    public String preferredFileName() {
        return "throttles.json";
    }
}
