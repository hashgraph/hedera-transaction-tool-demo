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
    private final DecimalFormat hbarFormat;
    private final DecimalFormat tinybarFormat;
    private boolean showSymbol = true;

    public static final String HBAR_CURRENCY_CHARACTERS_STRING = "(tℏ|μℏ|mℏ|ℏ|kℏ|Mℏ|Gℏ)*";

    public HbarCurrencyFormat() {
        final var symbols = new DecimalFormatSymbols();
        symbols.setGroupingSeparator(' ');
        hbarFormat = new DecimalFormat("###,###,###,###,###,###", symbols);
        tinybarFormat = new DecimalFormat("00,000,000", symbols);
    }

//    i think id rather overload the format methods, or i need to do the protected constructor and 'getInstance(withSymbol) or something
    public HbarCurrencyFormat(boolean showSymbol) {
        this();
        this.showSymbol = showSymbol;
    }

    public final boolean isShowSymbol() {
        return showSymbol;
    }

    public final void setShowSymbol(final boolean showSymbol) {
        this.showSymbol = showSymbol;
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
        return format(Hbar.from(bars));
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
        return format(Hbar.fromTinybars(hbar*100_000_000+tinybar));
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
        Objects.requireNonNull(unit, "Cannot format using a null unit");
        return format(Hbar.from(bars, unit));
    }

    /**
     * Format the given amount as an Hbar currency. The value is assumed to be in Hbars.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(final double bars) {
        return format(Hbar.from(BigDecimal.valueOf(bars)));
    }

    /**
     * Format the given amount as an Hbar currency. The value is assumed to be in Hbars.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(@NotNull final BigDecimal bars) {
        Objects.requireNonNull(bars, "Cannot format a null value");
        return format(Hbar.from(bars));
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
        Objects.requireNonNull(bars, "Cannot format a null value");
        return format(Hbar.from(new BigDecimal(bars)));
    }

    /**
     * Format the given amount of {@link Hbar} as an Hbar currency.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(@NotNull final Hbar bars) {
        Objects.requireNonNull(bars, "Cannot format a null value");
        var totalHbars = bars;
        if (totalHbars.compareTo(Hbar.MAX) == 1) {
            totalHbars = Hbar.MAX;
        } else if (totalHbars.compareTo(Hbar.MIN) == -1) {
            totalHbars = Hbar.MIN;
        }

        final var totalTinybars = totalHbars.toTinybars();
        final var tinybars = (totalTinybars % 100_000_000);
        final var hbars = (totalTinybars - tinybars) / 100_000_000L;
        if (tinybars != 0) {
            // If there are tinybars, an extra step is needed
            // if hbars < 0, then the sign is '-'
            // otherwise, the sign depends on tinybars
            var sign = hbars < 0 || tinybars < 0 ? "-" : "";
            // The sign is being handled separately, so both hbars and tinybars need to be adjusted
            return sign + hbarFormat.format(Math.abs(hbars)) + "."
                    + tinybarFormat.format(Math.abs(tinybars)) + getCurrencySymbol();
        }
        return hbarFormat.format(hbars) + getCurrencySymbol();
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
        var pattern = Pattern.compile(HBAR_CURRENCY_CHARACTERS_STRING);
        Matcher matcher = pattern.matcher(text);
        if (matcher.find()) {
            return parse(text.replaceAll(HBAR_CURRENCY_CHARACTERS_STRING, ""),
                Stream.of(HbarUnit.values()).filter(unit -> Objects.equals(unit.getSymbol(), matcher.group(1)))
                        .findFirst().orElse(HbarUnit.HBAR));
        }
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
        // strip spaces and currency symbol
        var strippedValue = text.replaceAll("\\s", "");
        if (strippedValue.isBlank()) {
            return Hbar.ZERO;
        }
        var value = new BigDecimal(strippedValue).setScale(8, RoundingMode.HALF_UP);
        return Hbar.from(value, unit);
    }

    /**
     * Get the currency symbol (and added space).
     *
     * @return
     */
    private String getCurrencySymbol() {
        if (!isShowSymbol()) {
            return "";
        }
        return " " + HbarUnit.HBAR.getSymbol();
    }

}
