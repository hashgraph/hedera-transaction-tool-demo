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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.sdk.Endpoint;

import java.net.InetAddress;
import java.net.UnknownHostException;

import static com.hedera.hashgraph.client.core.constants.JsonConstants.HOST_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.PORT_FIELD_NAME;

public class ToolEndpoint {
    private String host;
    private int port;

    public ToolEndpoint(String host, int port) {
        setHost(host);
        setPort(port);
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        if (host != null && !host.isEmpty()) {
            try {
                InetAddress.getByName(host);
            } catch (UnknownHostException e) {
                throw new InvalidHostException("Invalid Host.");
            }
        }
        this.host = host;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        if (port > 0 && port <= 65535) {
            this.port = port;
        } else {
            throw new InvalidPortException("Invalid port number.");
        }
    }

    public JsonObject asJson() {
        final var object = new JsonObject();
        object.addProperty(HOST_FIELD_NAME, host);
        object.addProperty(PORT_FIELD_NAME, port);
        return object;
    }

    public static ToolEndpoint fromJson(JsonObject jsonObject) {
        var host = jsonObject.get(HOST_FIELD_NAME).getAsString();
        var port = jsonObject.get(PORT_FIELD_NAME).getAsInt();
        return new ToolEndpoint(host, port);
    }

    @Override
    public String toString() {
        return getHost() + ":" + getPort();
    }

    public Endpoint toSdk() {
        try {
            final Endpoint endpoint = new Endpoint();
            endpoint.setPort(port);
            InetAddress inetAddress = InetAddress.getByName(host);
            String address = inetAddress.getHostAddress();
            if (address.equals(host)) {
                // Host is an IP address
                endpoint.setAddress(inetAddress.getAddress());
            } else {
                // Host is a domain name
                endpoint.setDomainName(host);
            }
            return endpoint;
        } catch (UnknownHostException e) {
            throw new RuntimeException("Invalid host: " + host, e);
        }
    }

    public static ToolEndpoint fromSdk(Endpoint endpoint) {
        try {
            String host;
            var address = endpoint.getAddress();
            if (address != null && address.length > 0) {
                host = InetAddress.getByAddress(address).getHostAddress();
            } else {
                host = endpoint.getDomainName();
            }
            return new ToolEndpoint(host, endpoint.getPort());
        } catch (UnknownHostException e) {
            throw new RuntimeException("Invalid host: " + endpoint.getDomainName(), e);
        }
    }

    public static class InvalidPortException extends IllegalArgumentException {
        public InvalidPortException(String message) {
            super(message);
        }
    }

    public static class InvalidHostException extends IllegalArgumentException {
        public InvalidHostException(String message) {
            super(message);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof ToolEndpoint)) return false;
        ToolEndpoint other = (ToolEndpoint) obj;
        return port == other.port &&
                java.util.Objects.equals(host, other.host);
    }

    @Override
    public int hashCode() {
        return java.util.Objects.hash(host, port);
    }
}