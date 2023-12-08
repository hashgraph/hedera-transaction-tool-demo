package com.hedera.hashgraph.client.core.utils.sysfiles;

import com.hedera.hashgraph.sdk.proto.ThrottleDefinitions;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ThrottleDefinitionsPojo {
    List<ThrottleBucketPojo> buckets = new ArrayList<>();

    public List<ThrottleBucketPojo> getBuckets() {
        return buckets;
    }

    public void setBuckets(List<ThrottleBucketPojo> buckets) {
        this.buckets = buckets;
    }

    public static ThrottleDefinitionsPojo fromProto(ThrottleDefinitions defs) {
        var pojo = new ThrottleDefinitionsPojo();
        pojo.setBuckets(defs.getThrottleBucketsList().stream()
                .map(ThrottleBucketPojo::fromProto)
                .collect(Collectors.toList()));
        return pojo;
    }
}
