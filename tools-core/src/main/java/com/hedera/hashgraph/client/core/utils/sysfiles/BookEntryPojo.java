/*
 * Copyright (C) 2020-2023 Hedera Hashgraph, LLC
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

package com.hedera.hashgraph.client.core.utils.sysfiles;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.sdk.proto.AccountID;
import com.hedera.hashgraph.sdk.proto.NodeAddress;
import com.hedera.hashgraph.sdk.proto.ServiceEndpoint;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class BookEntryPojo {
    private static final String MISSING_CERT_HASH = "<N/A>";

    /** lower characters for hex conversion */
    private static final char[] DIGITS_LOWER = {
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
    };

    public static class EndpointPojo {
        private String ipAddressV4;
        private Integer port;

        @SuppressWarnings("java:S5960")
        private static String asReadableIp(final ByteString octets) {
            final byte[] raw = octets.toByteArray();
            final var sb = new StringBuilder();
            for (int i = 0; i < 4; i++) {
                sb.append("" + (0xff & raw[i]));
                if (i != 3) {
                    sb.append(".");
                }
            }
            return sb.toString();
        }

        public String getIpAddressV4() {
            return ipAddressV4;
        }

        public void setIpAddressV4(final String ipAddressV4) {
            this.ipAddressV4 = ipAddressV4;
        }

        public Integer getPort() {
            return port;
        }

        public void setPort(final Integer port) {
            this.port = port;
        }

        @SuppressWarnings("java:S5960")
        static EndpointPojo fromProto(final ServiceEndpoint proto) {
            final var pojo = new EndpointPojo();
            pojo.setIpAddressV4(asReadableIp(proto.getIpAddressV4()));
            pojo.setPort(proto.getPort());
            return pojo;
        }
    }

    private String deprecatedIp;
    private String deprecatedMemo;
    private Integer deprecatedPortNo;

    private Long stake;
    private Long nodeId;
    private String certHash = MISSING_CERT_HASH;
    private String rsaPubKey;
    private String nodeAccount;
    private String description;
    private List<EndpointPojo> endpoints;

    @SuppressWarnings("java:S1874")
    static BookEntryPojo fromProto(final NodeAddress address) {
        final var entry = new BookEntryPojo();

        entry.deprecatedIp =
                address.getIpAddress().isEmpty() ? null : address.getIpAddress().toStringUtf8();
        entry.deprecatedPortNo = address.getPortno();
        if (entry.deprecatedPortNo == 0) {
            entry.deprecatedPortNo = null;
        }
        entry.deprecatedMemo =
                address.getMemo().isEmpty() ? null : address.getMemo().toStringUtf8();

        entry.rsaPubKey = address.getRSAPubKey().isEmpty() ? null : address.getRSAPubKey();
        entry.nodeId = address.getNodeId();
        if (address.hasNodeAccountId()) {
            final var account = address.getNodeAccountId();
            entry.nodeAccount = String.format("%d.%d.%d",
                    account.getShardNum(), account.getRealmNum(), account.getAccountNum());
        } else {
            try {
                final var memo = address.getMemo().toStringUtf8();
                // Try and create an AccountID to verify it is in the proper format
                final var parts = memo.split("[.]");
                final var partsAsLongs = Stream.of(parts).mapToLong(Long::valueOf).toArray();
                AccountID.newBuilder()
                        .setShardNum(partsAsLongs[0])
                        .setRealmNum(partsAsLongs[1])
                        .setAccountNum(partsAsLongs[2])
                        .build();

                entry.nodeAccount = memo;
            } catch (final Exception ignore) {
                entry.nodeAccount = null;
            }
        }
        entry.certHash = address.getNodeCertHash().isEmpty()
                ? MISSING_CERT_HASH
                : address.getNodeCertHash().toStringUtf8();
        mapEndpoints(address, entry);

        entry.description = address.getDescription().isEmpty() ? null : address.getDescription();
        entry.stake = address.getStake();
        if (entry.stake == 0) {
            entry.stake = null;
        }

        return entry;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    private static void mapEndpoints(final NodeAddress from, final BookEntryPojo to) {
        to.endpoints = from.getServiceEndpointList().stream()
                .map(EndpointPojo::fromProto)
                .collect(Collectors.toList());
    }

    public Long getStake() {
        return stake;
    }

    public void setStake(final Long stake) {
        this.stake = stake;
    }

    public String getDeprecatedIp() {
        return deprecatedIp;
    }

    public void setDeprecatedIp(final String deprecatedIp) {
        this.deprecatedIp = deprecatedIp;
    }

    public String getDeprecatedMemo() {
        return deprecatedMemo;
    }

    public void setDeprecatedMemo(final String deprecatedMemo) {
        this.deprecatedMemo = deprecatedMemo;
    }

    public String getNodeAccount() {
        return nodeAccount;
    }

    public void setNodeAccount(final String nodeAccount) {
        this.nodeAccount = nodeAccount;
    }

    public String getRsaPubKey() {
        return rsaPubKey;
    }

    public void setRsaPubKey(final String rsaPubKey) {
        this.rsaPubKey = rsaPubKey;
    }

    public String getCertHash() {
        return certHash;
    }

    public void setCertHash(final String certHash) {
        this.certHash = certHash;
    }

    public Integer getDeprecatedPortNo() {
        return deprecatedPortNo;
    }

    public void setDeprecatedPortNo(final Integer deprecatedPortNo) {
        this.deprecatedPortNo = deprecatedPortNo;
    }

    public Long getNodeId() {
        return nodeId;
    }

    public void setNodeId(final Long nodeId) {
        this.nodeId = nodeId;
    }

    public List<EndpointPojo> getEndpoints() {
        return endpoints;
    }

    public void setEndpoints(final List<EndpointPojo> endpoints) {
        this.endpoints = endpoints;
    }

    static String asHexEncodedSha384HashFor(final String baseDir, final long nodeId) {
        try {
            final var crtBytes = Files.readAllBytes(Paths.get(baseDir, String.format("node%d.crt", nodeId)));
            final var crtHash = MessageDigest.getInstance("SHA-384").digest(crtBytes);
            return hex(crtHash);
        } catch (final Exception e) {
            throw new IllegalStateException(e);
        }
    }

    static String asHexEncodedDerPubKey(final String baseDir, final long nodeId) {
        try {
            final var pubKeyBytes = Files.readAllBytes(Paths.get(baseDir, String.format("node%d.der", nodeId)));
            return hex(pubKeyBytes);
        } catch (final Exception e) {
            throw new IllegalStateException(e);
        }
    }

    public static ByteString asOctets(final String ipAddressV4) {
        final byte[] octets = new byte[4];
        final String[] literals = ipAddressV4.split("[.]");
        for (int i = 0; i < 4; i++) {
            octets[i] = (byte) Integer.parseInt(literals[i]);
        }
        return ByteString.copyFrom(octets);
    }

    /**
     * Converts an array of bytes to a lowercase hexadecimal string.
     *
     * @param bytes  the array of bytes to hexadecimal
     * @param length the length of the array to convert to hex
     * @return a {@link String} containing the lowercase hexadecimal representation of the byte array
     */
    @SuppressWarnings("java:S127")
    public static String hex(final byte[] bytes, final int length) {
        if (bytes == null) {
            return "null";
        }

        if (length < 0 || length > bytes.length) {
            throw new IllegalArgumentException(String.format(
                    "The argument '%s' should have a value between %d and %d! Value provided is %d",
                    "length", 0, bytes.length, length));
        }

        final char[] out = new char[length << 1];
        for (int i = 0, j = 0; i < length; i++) {
            out[j++] = DIGITS_LOWER[(0xF0 & bytes[i]) >>> 4];
            out[j++] = DIGITS_LOWER[0x0F & bytes[i]];
        }

        return new String(out);
    }

    /**
     * Equivalent to calling {@link #hex(byte[], int)} with length set to bytes.length
     *
     * @param bytes an array of bytes
     * @return a {@link String} containing the lowercase hexadecimal representation of the byte array
     */
    public static String hex(final byte[] bytes) {
        return hex(bytes, bytes == null ? 0 : bytes.length);
    }
}
