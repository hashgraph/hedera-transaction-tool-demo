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

package com.hedera.hashgraph.client.ui.components;

import javafx.beans.value.ObservableValue;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.File;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SigningKeyCheckBoxListenerTest {
    static SigningKeyCheckBoxListener listener;
    static Set<File> signersSet;
    @TempDir
    static Path tempDir;
    static File tempFile;

    @BeforeAll
    static void setup() {
        signersSet = new HashSet<>();
        try {
            tempFile = tempDir.resolve("tempFile.tx").toFile();
            listener = new SigningKeyCheckBoxListener(signersSet, tempFile, "testBaseName");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    // Currently, only newValue is used, so the other two values will only be tested using nulls
    @ParameterizedTest
    @MethodSource("getArguments")
    void changed(ObservableValue observable, Object oldValue, Object newValue, boolean expectedResult) {
        listener.changed(observable, oldValue, newValue);
        assertEquals(signersSet.contains(tempFile), expectedResult);
    }

    @NotNull
    private static Stream<Arguments> getArguments() {
        return Stream.of(
                Arguments.of(null, null, null, false),
                Arguments.of(null, null, true, true),
                Arguments.of(null, null, false, false),
                Arguments.of(null, null, Boolean.TRUE, true),
                Arguments.of(null, null, Boolean.FALSE, false),
                Arguments.of(null, null, 1, false),
                Arguments.of(null, null, 0, false),
                Arguments.of(null, null, "true", false),
                Arguments.of(null, null, "false", false),
                Arguments.of(null, null, "test", false),
                Arguments.of(null, null, new Object(), false)
        );
    }
}