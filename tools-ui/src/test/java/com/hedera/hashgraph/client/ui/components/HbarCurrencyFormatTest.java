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
    @MethodSource("getFormatLongArguments")
    void formatLongTest(final long bars, final String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getFormatHbarAndTinybarArguments")
    void formatHbarAndTinybarTest(final long hbars, final long tinybars, final String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(hbars, tinybars));
    }

    @ParameterizedTest
    @MethodSource("getFormatLongAndHbarUnitArguments")
    void formatLongWithHbarUnitTest(final long hbars, final HbarUnit unit, final String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(hbars, unit));
    }

    @Test
    void formatLongWithHbarUnitExceptionTest() {
        assertThrows(NullPointerException.class, () -> hbarCurrencyFormat.format(1L, null));
    }

    @ParameterizedTest
    @MethodSource("getFormatDoubleArguments")
    void formatDoubleTest(final double bars, final String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getFormatBigDecimalArguments")
    void formatBigDecimalTest(final BigDecimal bars, final String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @Test
    void formatBigDecimalExceptionTest() {
        assertThrows(NullPointerException.class, () -> hbarCurrencyFormat.format((BigDecimal)null));
    }

    @ParameterizedTest
    @MethodSource("getFormatStringArguments")
    void formatStringTest(final String bars, final String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getFormatStringExceptionArguments")
    void formatStringExceptionTest(final String bars) {
        assertThrows(NumberFormatException.class, () -> hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getFormatHbarArguments")
    void formatHbarTest(final Hbar bars, final String expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.format(bars));
    }

    @ParameterizedTest
    @MethodSource("getParseStringArguments")
    void parseStringTest(final String text, final Hbar expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.parse(text));
    }
    @ParameterizedTest
    @MethodSource("getParseStringForUnitArguments")
    void parseStringTest(final String text, final HbarUnit unit, final Hbar expectedResult) {
        assertEquals(expectedResult, hbarCurrencyFormat.parse(text, unit));
    }

    @NotNull
    private static Stream<Arguments> getFormatLongArguments() {
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
    private static Stream<Arguments> getFormatHbarAndTinybarArguments() {
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
    private static Stream<Arguments> getFormatLongAndHbarUnitArguments() {
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
    private static Stream<Arguments> getFormatDoubleArguments() {
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
    private static Stream<Arguments> getFormatBigDecimalArguments() {
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
    private static Stream<Arguments> getFormatStringArguments() {
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
    private static Stream<Arguments> getFormatStringExceptionArguments() {
        return Stream.of(
//                Arguments.of(null),
                Arguments.of(""),
                Arguments.of(" "),
                Arguments.of("test"),
                Arguments.of("123test")
        );
    }

    @NotNull
    private static Stream<Arguments> getFormatHbarArguments() {
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

    @NotNull
    private static Stream<Arguments> getParseStringArguments() {
        return Stream.of(
                Arguments.of("", Hbar.ZERO),
                Arguments.of("0", Hbar.ZERO),
                Arguments.of(".0", Hbar.ZERO),
                Arguments.of("1", Hbar.from(1L)),
                Arguments.of("+1", Hbar.from(1L)),
                Arguments.of("-1", Hbar.from(-1L)),
                Arguments.of("1.0", Hbar.from(1L)),
                Arguments.of("1.1", Hbar.from(new BigDecimal("1.1"))),
                Arguments.of("+1.1", Hbar.from(new BigDecimal("1.1"))),
                Arguments.of("-1.1", Hbar.from(new BigDecimal("-1.1"))),
                Arguments.of("1111", Hbar.from(new BigDecimal("1111"))),
                Arguments.of("+1111", Hbar.from(new BigDecimal("1111"))),
                Arguments.of("-1111", Hbar.from(new BigDecimal("-1111"))),
                Arguments.of("1 111", Hbar.from(new BigDecimal("1111"))),
                Arguments.of("+1 111", Hbar.from(new BigDecimal("1111"))),
                Arguments.of("-1 111", Hbar.from(new BigDecimal("-1111"))),
                Arguments.of("1111.1", Hbar.from(new BigDecimal("1111.1"))),
                Arguments.of("+1111.1", Hbar.from(new BigDecimal("1111.1"))),
                Arguments.of("-1111.1", Hbar.from(new BigDecimal("-1111.1"))),
                Arguments.of("1 111.11 11", Hbar.from(new BigDecimal("1111.1111"))),
                Arguments.of("+1 111.11 11", Hbar.from(new BigDecimal("1111.1111"))),
                Arguments.of("-1 111.11 11", Hbar.from(new BigDecimal("-1111.1111")))
        );
    }

    @NotNull
    private static Stream<Arguments> getParseStringForUnitArguments() {
        return Stream.of(
                Arguments.of("", HbarUnit.HBAR, Hbar.ZERO),
                Arguments.of("0", HbarUnit.TINYBAR, Hbar.ZERO),
                Arguments.of(".0", HbarUnit.GIGABAR, Hbar.ZERO),
                Arguments.of("1", HbarUnit.KILOBAR, Hbar.from(1_000L)),
                Arguments.of("+1", HbarUnit.MICROBAR, Hbar.fromTinybars(100L)),
                Arguments.of("-1", HbarUnit.MILLIBAR, Hbar.fromTinybars(-100_000L)),
                Arguments.of("1.0", HbarUnit.MEGABAR, Hbar.from(1_000_000L)),
                Arguments.of("1.1", HbarUnit.HBAR, Hbar.from(new BigDecimal("1.1"))),
                Arguments.of("+1.1", HbarUnit.KILOBAR, Hbar.from(1_100L)),
                Arguments.of("-1.1", HbarUnit.MILLIBAR, Hbar.fromTinybars(-110_000L))
        );
    }
}