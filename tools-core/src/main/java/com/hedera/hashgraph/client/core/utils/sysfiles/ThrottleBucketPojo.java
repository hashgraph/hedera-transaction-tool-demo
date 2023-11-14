package com.hedera.hashgraph.client.core.utils.sysfiles;

import com.hedera.hashgraph.sdk.proto.ThrottleBucket;

import java.util.List;
import java.util.stream.Collectors;

public class ThrottleBucketPojo {
    private long burstPeriod;
    private long burstPeriodMs;
    private String name;
    private List<ThrottleGroupPojo> throttleGroups;

    public long getBurstPeriod() {
        return burstPeriod;
    }

    public void setBurstPeriod(long burstPeriod) {
        this.burstPeriod = burstPeriod;
    }

    public long getBurstPeriodMs() {
        return burstPeriodMs;
    }

    public void setBurstPeriodMs(long burstPeriodMs) {
        this.burstPeriodMs = burstPeriodMs;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<ThrottleGroupPojo> getThrottleGroups() {
        return throttleGroups;
    }

    public void setThrottleGroups(List<ThrottleGroupPojo> throttleGroups) {
        this.throttleGroups = throttleGroups;
    }

    public static ThrottleBucketPojo fromProto(ThrottleBucket bucket) {
        final var pojo = new ThrottleBucketPojo();
        pojo.setBurstPeriod(0);
        pojo.setBurstPeriodMs(bucket.getBurstPeriodMs());
        pojo.setName(bucket.getName());
        pojo.setThrottleGroups(bucket.getThrottleGroupsList().stream()
                .map(ThrottleGroupPojo::fromProto)
                .collect(Collectors.toList()));
        return pojo;
    }
}
