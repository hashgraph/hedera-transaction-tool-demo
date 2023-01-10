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

import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.HbarUnit;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.math.BigDecimal;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class HbarCurrencyFormatTest {

    static HbarCurrencyFormat hbarCurrencyFormat = new HbarCurrencyFormat();

    @ParameterizedTest
    @MethodSource("getLongArguments")
    void formatLongTest(final long bars, String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getHbarAndTinybarArguments")
    void formatHbarAndTinybarTest(final long hbars, final long tinybars, String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(hbars, tinybars));
    }

    @ParameterizedTest
    @MethodSource("getLongAndHbarUnitArguments")
    void formatLongWithHbarUnitTest(final long hbars, final HbarUnit unit, String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(hbars, unit));
    }

    @Test
    void formatLongWithHbarUnitExceptionTest() {
        assertThrows(NullPointerException.class, () -> hbarCurrencyFormat.format(1L, null));
    }

    @ParameterizedTest
    @MethodSource("getDoubleArguments")
    void formatDoubleTest(final double bars, String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getBigDecimalArguments")
    void formatBigDecimalTest(final BigDecimal bars, String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @Test
    void formatBigDecimalExceptionTest() {
        assertThrows(NullPointerException.class, () -> hbarCurrencyFormat.format((BigDecimal)null));
    }

    @ParameterizedTest
    @MethodSource("getStringArguments")
    void formatStringTest(final String bars, String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getStringExceptionArguments")
    void formatStringExceptionTest(final String bars) {
        assertThrows(NumberFormatException.class, () -> hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getHbarArguments")
    void formatHbarTest(final Hbar bars, String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @NotNull
    private static Stream<Arguments> getLongArguments() {
        return Stream.of(
                Arguments.of(0L, "0 ℏ"),
                Arguments.of(-0L, "0 ℏ"),
                Arguments.of(1L, "1 ℏ"),
                Arguments.of(-1L, "-1 ℏ"),
                Arguments.of(123_456_789L, "123 456 789 ℏ"),
                Arguments.of(-123_456_789L, "-123 456 789 ℏ"),
                Arguments.of(50_000_000_001L, "50 000 000 000 ℏ"),
                Arguments.of(-50_000_000_001L, "-50 000 000 000 ℏ")
        );
    }

    @NotNull
    private static Stream<Arguments> getHbarAndTinybarArguments() {
        return Stream.of(
                Arguments.of(0L, 0L, "0 ℏ"),
                Arguments.of(-0L, 0L, "0 ℏ"),
                Arguments.of(1L, 0L, "1 ℏ"),
                Arguments.of(-1L, 0L, "-1 ℏ"),
                Arguments.of(1L, 1L, "1.00 000 001 ℏ"),
                Arguments.of(-1L, -1L, "-1.00 000 001 ℏ"),
                Arguments.of(1L, -1L, "0.99 999 999 ℏ"),
                Arguments.of(-1L, 1L, "-0.99 999 999 ℏ"),
//                should i allow tiny and hbar to be different signs?
                Arguments.of(1L, 100_000_000L, "2 ℏ"),
                Arguments.of(-1L, -100_000_000L, "-2 ℏ"),
                Arguments.of(50_000_000_001L, 1L, "50 000 000 000 ℏ"),
                Arguments.of(-50_000_000_001L, -1L, "-50 000 000 000 ℏ")
        );
    }

    @NotNull
    private static Stream<Arguments> getLongAndHbarUnitArguments() {
        return Stream.of(
                Arguments.of(0L, HbarUnit.TINYBAR, "0 ℏ"),
                Arguments.of(0L, HbarUnit.HBAR, "0 ℏ"),
                Arguments.of(-0L, HbarUnit.TINYBAR, "0 ℏ"),
                Arguments.of(-0L, HbarUnit.HBAR, "0 ℏ"),
                Arguments.of(1L, HbarUnit.TINYBAR, "0.00 000 001 ℏ"),
                Arguments.of(1L, HbarUnit.MICROBAR, "0.00 000 100 ℏ"),
                Arguments.of(1L, HbarUnit.MILLIBAR, "0.00 100 000 ℏ"),
                Arguments.of(1L, HbarUnit.HBAR, "1 ℏ"),
                Arguments.of(1L, HbarUnit.KILOBAR, "1 000 ℏ"),
                Arguments.of(1L, HbarUnit.MEGABAR, "1 000 000 ℏ"),
                Arguments.of(1L, HbarUnit.GIGABAR, "1 000 000 000 ℏ"),
                Arguments.of(123_456_789L, HbarUnit.TINYBAR, "1.23 456 789 ℏ"),
                Arguments.of(-123_456_789L, HbarUnit.TINYBAR, "-1.23 456 789 ℏ"),
                Arguments.of(50_000_000_001L, HbarUnit.HBAR, "50 000 000 000 ℏ"),
                Arguments.of(-50_000_000_001L, HbarUnit.HBAR, "-50 000 000 000 ℏ"),
                Arguments.of(51L, HbarUnit.GIGABAR, "50 000 000 000 ℏ"),
                Arguments.of(-51L, HbarUnit.GIGABAR, "-50 000 000 000 ℏ")
        );
    }

    @NotNull
    private static Stream<Arguments> getDoubleArguments() {
        return Stream.of(
                Arguments.of(0D, "0 ℏ"),
                Arguments.of(-0D, "0 ℏ"),
                Arguments.of(1D, "1 ℏ"),
                Arguments.of(-1D, "-1 ℏ"),
                Arguments.of(12_345.6789, "12 345.67 890 000 ℏ"),
                Arguments.of(-12_345.6789, "-12 345.67 890 000 ℏ"),
                Arguments.of(50_000_000_000.1, "50 000 000 000 ℏ"),
                Arguments.of(-50_000_000_000.1, "-50 000 000 000 ℏ")
        );
    }

    @NotNull
    private static Stream<Arguments> getBigDecimalArguments() {
        return Stream.of(
                Arguments.of(new BigDecimal("0"), "0 ℏ"),
                Arguments.of(new BigDecimal("-0"), "0 ℏ"),
                Arguments.of(new BigDecimal("1"), "1 ℏ"),
                Arguments.of(new BigDecimal("-1"), "-1 ℏ"),
                Arguments.of(new BigDecimal("12345.6789"), "12 345.67 890 000 ℏ"),
                Arguments.of(new BigDecimal("-12345.6789"), "-12 345.67 890 000 ℏ"),
                Arguments.of(new BigDecimal("50000000000.1"), "50 000 000 000 ℏ"),
                Arguments.of(new BigDecimal("-50000000000.1"), "-50 000 000 000 ℏ")
        );
    }

    @NotNull
    private static Stream<Arguments> getStringArguments() {
        return Stream.of(
                Arguments.of("0", "0 ℏ"),
                Arguments.of("-0", "0 ℏ"),
                Arguments.of("1", "1 ℏ"),
                Arguments.of("-1", "-1 ℏ"),
                Arguments.of("12345.6789", "12 345.67 890 000 ℏ"),
                Arguments.of("-12345.6789", "-12 345.67 890 000 ℏ"),
                Arguments.of("50000000000.1", "50 000 000 000 ℏ"),
                Arguments.of("-50000000000.1", "-50 000 000 000 ℏ")
        );
    }

    @NotNull
    private static Stream<Arguments> getStringExceptionArguments() {
        return Stream.of(
//                Arguments.of(null),
                Arguments.of(""),
                Arguments.of(" "),
                Arguments.of("test"),
                Arguments.of("123test")
        );
    }

    @NotNull
    private static Stream<Arguments> getHbarArguments() {
        return Stream.of(
                Arguments.of(Hbar.from(0L), "0 ℏ"),
                Arguments.of(Hbar.from(-0L), "0 ℏ"),
                Arguments.of(Hbar.fromTinybars(0L), "0 ℏ"),
                Arguments.of(Hbar.fromTinybars(-0L), "0 ℏ"),
                Arguments.of(Hbar.fromTinybars(1L), "0.00 000 001 ℏ"),
                Arguments.of(Hbar.from(1L, HbarUnit.MICROBAR), "0.00 000 100 ℏ"),
                Arguments.of(Hbar.from(1L, HbarUnit.MILLIBAR), "0.00 100 000 ℏ"),
                Arguments.of(Hbar.from(1L), "1 ℏ"),
                Arguments.of(Hbar.from(1L, HbarUnit.KILOBAR), "1 000 ℏ"),
                Arguments.of(Hbar.from(1L, HbarUnit.MEGABAR), "1 000 000 ℏ"),
                Arguments.of(Hbar.from(1L, HbarUnit.GIGABAR), "1 000 000 000 ℏ"),
                Arguments.of(Hbar.from(123_456_789L, HbarUnit.TINYBAR), "1.23 456 789 ℏ"),
                Arguments.of(Hbar.from(-123_456_789L, HbarUnit.TINYBAR), "-1.23 456 789 ℏ"),
                Arguments.of(Hbar.from(50_000_000_001L, HbarUnit.HBAR), "50 000 000 000 ℏ"),
                Arguments.of(Hbar.from(-50_000_000_001L, HbarUnit.HBAR), "-50 000 000 000 ℏ"),
                Arguments.of(Hbar.from(51L, HbarUnit.GIGABAR), "50 000 000 000 ℏ"),
                Arguments.of(Hbar.from(-51L, HbarUnit.GIGABAR), "-50 000 000 000 ℏ")
        );
    }
}