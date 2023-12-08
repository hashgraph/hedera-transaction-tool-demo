package com.hedera.hashgraph.client.core.utils.sysfiles;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.hedera.hashgraph.sdk.proto.HederaFunctionality;
import com.hedera.hashgraph.sdk.proto.ThrottleGroup;

import java.util.List;
import java.util.stream.Collectors;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class ThrottleGroupPojo {
    private long milliOpsPerSec;
    private List<HederaFunctionality> operations;

    public long getMilliOpsPerSec() {
        return milliOpsPerSec;
    }

    public void setMilliOpsPerSec(long milliOpsPerSec) {
        this.milliOpsPerSec = milliOpsPerSec;
    }

    public List<HederaFunctionality> getOperations() {
        return operations;
    }

    public void setOperations(List<HederaFunctionality> operations) {
        this.operations = operations;
    }

    public static ThrottleGroupPojo fromProto(ThrottleGroup group) {
        final var pojo = new ThrottleGroupPojo();
        pojo.setMilliOpsPerSec(group.getMilliOpsPerSec());
        pojo.setOperations(group.getOperationsValueList().stream()
                .map(HederaFunctionality::forNumber)
                .collect(Collectors.toList()));
        return pojo;
    }
}
