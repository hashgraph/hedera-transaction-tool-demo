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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class HbarCurrencyFormat {
    private static final String HBAR_CURRENCY_CHARACTERS_STRING = "(tℏ|μℏ|mℏ|ℏ|kℏ|Mℏ|Gℏ)*";

    private final DecimalFormat hbarFormat;
    private final DecimalFormat tinybarFormat;

    public HbarCurrencyFormat() {
        final var symbols = new DecimalFormatSymbols();
        symbols.setGroupingSeparator(' ');
        hbarFormat = new DecimalFormat("###,###,###,###,###,###", symbols);
        tinybarFormat = new DecimalFormat("00,000,000", symbols);
    }

    /**
     * Format the given amount as an Hbar currency. The value is assumed to be in Hbars.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(final long bars) {
        // Convert into an Hbar, for consolidated validation
        return format(bars, true);
    }

    /**
     * Format the given amount as an Hbar currency. The value is assumed to be in Hbars.
     *
     * @param bars
     * @param showSymbol
     * @return
     */
    @NotNull
    public final String format(final long bars, boolean showSymbol) {
        // Convert into an Hbar, for consolidated validation
        return format(Hbar.from(bars), showSymbol);
    }

    /**
     * Format the given hbar and tinybar amounts as an Hbar currency.
     *
     * @param hbar
     * @param tinybar
     * @return
     */
    @NotNull
    public final String format(final long hbar, final long tinybar) {
        // Convert hbar and tinybar into 1 Hbar object, for consolidated validation
        return format(hbar, tinybar, true);
    }

    /**
     * Format the given hbar and tinybar amounts as an Hbar currency.
     *
     * @param hbar
     * @param tinybar
     * @return
     */
    @NotNull
    public final String format(final long hbar, final long tinybar, boolean showSymbol) {
        // Convert hbar and tinybar into 1 Hbar object, for consolidated validation
        return format(Hbar.fromTinybars(hbar*100_000_000+tinybar), showSymbol);
    }

    /**
     * Format the given amount as an Hbar currency, using the {@link HbarUnit} to convert into the proper format.
     *
     * @param bars
     * @param unit
     * @return
     */
    @NotNull
    public final String format(final long bars, @NotNull final HbarUnit unit) {
        return format(bars, unit, true);
    }

    /**
     * Format the given amount as an Hbar currency, using the {@link HbarUnit} to convert into the proper format.
     *
     * @param bars
     * @param unit
     * @return
     */
    @NotNull
    public final String format(final long bars, @NotNull final HbarUnit unit, boolean showSymbol) {
        Objects.requireNonNull(unit, "Cannot format using a null unit");
        return format(Hbar.from(bars, unit), showSymbol);
    }

    /**
     * Format the given amount as an Hbar currency. The value is assumed to be in Hbars.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(final double bars) {
        return format(bars, true);
    }

    /**
     * Format the given amount as an Hbar currency. The value is assumed to be in Hbars.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(final double bars, boolean showSymbol) {
        return format(Hbar.from(BigDecimal.valueOf(bars)), showSymbol);
    }

    /**
     * Format the given amount as an Hbar currency. The value is assumed to be in Hbars.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(@NotNull final BigDecimal bars) {
        return format(bars, true);
    }

    /**
     * Format the given amount as an Hbar currency. The value is assumed to be in Hbars.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(@NotNull final BigDecimal bars, boolean showSymbol) {
        Objects.requireNonNull(bars, "Cannot format a null value");
        return format(Hbar.from(bars), showSymbol);
    }

    /**
     * Format the given string as an Hbar currency.
     *
     * @param bars
     * @return
     * @throws NumberFormatException
     */
    @NotNull
    public final String format(@NotNull final String bars) throws NumberFormatException {
        return format(bars, true);
    }

    /**
     * Format the given string as an Hbar currency.
     *
     * @param bars
     * @return
     * @throws NumberFormatException
     */
    @NotNull
    public final String format(@NotNull final String bars, boolean showSymbol) throws NumberFormatException {
        Objects.requireNonNull(bars, "Cannot format a null value");
        return format(Hbar.from(new BigDecimal(bars)), showSymbol);
    }

    /**
     * Format the given amount of {@link Hbar} as an Hbar currency.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(@NotNull final Hbar bars) {
        return format(bars, true);
    }

    /**
     * Format the given amount of {@link Hbar} as an Hbar currency.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(@NotNull final Hbar bars, boolean showSymbol) {
        Objects.requireNonNull(bars, "Cannot format a null value");

        // Make sure the Hbar amount isn't greater than or less than the Max/Min
        var totalHbars = bars;
        if (totalHbars.compareTo(Hbar.MAX) == 1) {
            totalHbars = Hbar.MAX;
        } else if (totalHbars.compareTo(Hbar.MIN) == -1) {
            totalHbars = Hbar.MIN;
        }

        // Convert to tiny bars and separate into Hbars and Tinybars
        final var totalTinybars = totalHbars.toTinybars();
        final var tinybars = (totalTinybars % 100_000_000);
        final var hbars = (totalTinybars - tinybars) / 100_000_000L;

        // Determine the sign to display, if applicable
        final var sign = totalTinybars < 0 ? "-" : "";

        // Start formatting with the Hbar amount
        var formattedValue = sign + hbarFormat.format(Math.abs(hbars));

        // If there are Tinybars, format them and add them to the string
        if (tinybars != 0) {
            formattedValue += "." + tinybarFormat.format(Math.abs(tinybars));
        }

        // Show the symbol if requested
        if (showSymbol) {
            formattedValue += " " + HbarUnit.HBAR.getSymbol();
        }

        return formattedValue;
    }

    // Because Hbar doesn't allow for a leading '.', parsing or adjusting the string must be done here.
    /**
     * Parses the given String into an {@link Hbar} object.
     *
     * @param text
     * @return
     * @throws NumberFormatException
     */
    @NotNull
    public final Hbar parse(@NotNull final String text) throws NumberFormatException {
        Objects.requireNonNull(text, "Cannot parse a null string");

        // Find the currency symbol, if applicable
        var pattern = Pattern.compile(HBAR_CURRENCY_CHARACTERS_STRING);
        Matcher matcher = pattern.matcher(text);

        // If found, get the HbarUnit and parse the text
        if (matcher.find()) {
            return parse(text.replaceAll(HBAR_CURRENCY_CHARACTERS_STRING, ""),
                Stream.of(HbarUnit.values()).filter(unit -> Objects.equals(unit.getSymbol(), matcher.group(1)))
                        .findFirst().orElse(HbarUnit.HBAR));
        }

        // If no symbol was found, assume the HbarUnit type is Hbar
        return parse(text, HbarUnit.HBAR);
    }

    /**
     * Parses the given String into an {@link Hbar} object for the given {@link HbarUnit} type.
     *
     * @param text
     * @return
     * @throws NumberFormatException
     */
    @NotNull
    public final Hbar parse(@NotNull final String text, @NotNull final HbarUnit unit) throws NumberFormatException {
        Objects.requireNonNull(text, "Cannot parse a null string");
        Objects.requireNonNull(unit, "Cannot format using a null unit");

        // Strip spaces
        var strippedValue = text.replaceAll("\\s", "");

        // If blank, return Hbar(0)
        if (strippedValue.isBlank()) {
            return Hbar.ZERO;
        }

        // Convert the value to a BigDecimal, rounding past the 8th decimal point
        var value = new BigDecimal(strippedValue).setScale(8, RoundingMode.HALF_UP);

        // Convert the BigDecimal to Hbar
        return Hbar.from(value, unit);
    }
}
