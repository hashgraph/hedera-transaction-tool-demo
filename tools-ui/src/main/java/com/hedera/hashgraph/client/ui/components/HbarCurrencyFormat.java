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
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Objects;

public class HbarCurrencyFormat {
    private final DecimalFormat hbarFormat;
    private final DecimalFormat tinybarFormat;
    private boolean showSymbol = true;

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
     * Format the given amount as an Hbar currency, using the unit to convert into the proper format.
     *
     * @param bars
     * @param unit
     * @return
     */
    @NotNull
    public final String format(final long bars, @NotNull final HbarUnit unit) {
        Objects.requireNonNull(unit);
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
        Objects.requireNonNull(bars);
        return format(Hbar.from(bars));
    }

    /**
     * Format the given string as an Hbar currency.
     * @param bars
     * @return
     * @throws NumberFormatException
     */
    @NotNull
    public final String format(@NotNull final String bars) throws NumberFormatException {
        Objects.requireNonNull(bars);
        return format(Hbar.from(new BigDecimal(bars)));
    }

    /**
     * Format the given amount of Hbar as an Hbar currency.
     *
     * @param bars
     * @return
     */
    @NotNull
    public final String format(@NotNull final Hbar bars) {
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

    public final Hbar parse(@NotNull final String formattedValue) {
        // ensure the string is formatted as an HbarCurrencyFormat
        // strip spaces and currency symbol
        var strippedValue = formattedValue.replace(HbarUnit.HBAR.getSymbol(), "")
                .replace(" ", "");
        try {
            var value = new BigDecimal(strippedValue);
            return new Hbar(value);
        } catch (Exception ex) {
            // display proper message for exception
        }

        return Hbar.ZERO;
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
