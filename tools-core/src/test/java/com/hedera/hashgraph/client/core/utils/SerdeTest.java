package com.hedera.hashgraph.client.core.utils;

import com.hedera.hashgraph.client.core.utils.sysfiles.serdes.StandardSerdes;
import org.junit.Assert;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

class SerdeTest {

    @ParameterizedTest
    @MethodSource("getDeserializeFileInput")
    void deserializeFile_test(long fileId, String path, String fileName, String baseName) throws IOException {
        final var bytes = Files.readAllBytes(Path.of(path, fileName));
        final var text = StandardSerdes.SYS_FILE_SERDES.get(fileId).fromRawFile(bytes);
        final var base = Files.readString(Path.of(path, baseName));
        Assert.assertEquals(base, text);
    }

    private static Stream<Arguments> getDeserializeFileInput() {
        return Stream.of(
                Arguments.of(101L, "src/test/resources/Serde/", "file-101.bin", "file-101.json"),
                Arguments.of(102L, "src/test/resources/Serde/", "file-102.bin", "file-102.json"),
                //111
                Arguments.of(111L, "src/test/resources/Serde/", "feeSchedules.bin", "feeSchedules.json"),
                //112
                Arguments.of(112L, "src/test/resources/Serde/", "exchangeRates.bin", "exchangeRates.json"),
                //121
                Arguments.of(121L, "src/test/resources/Serde/", "application.bin",
                        "application.properties"),
                //122
                Arguments.of(122L, "src/test/resources/Serde/", "api-permission.bin",
                        "api-permission.properties"),
                //123
                Arguments.of(123L, "src/test/resources/Serde/", "throttles.bin", "throttles.json")
        );
    }
}