package com.hedera.hashgraph.client.core.remote.helpers;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class AccountList {
    public static final String INPUT_STRING = "input";
    public static final String LIST_STRING = "list";

    private final String input;
    private final List<Identifier> list;

    public AccountList(@NotNull String input, @NotNull List<Identifier> list) {
        this.input = input;
        this.list = list;
    }

    public AccountList(@NotNull JsonObject accountList) {
        // While these should never be null, check anyway
        if (accountList.has(INPUT_STRING)) {
            input = accountList.get(INPUT_STRING).getAsString();
        } else {
            input = "";
        }
        if (accountList.has(LIST_STRING)) {
            list = ((JsonArray) accountList.get(LIST_STRING)).asList().stream()
                    .map(element -> {
                        try {
                            return Identifier.parse(element.getAsJsonObject());
                        } catch (HederaClientException ex) {
                            return null;
                        }
                    }).filter(Objects::nonNull).collect(Collectors.toList());
        } else {
            list = new ArrayList<>();
        }
    }

    public String getInput() {
        return input;
    }

    public List<Identifier> getList() {
        return list;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this, ToStringStyle.JSON_STYLE)
                .append(INPUT_STRING, input)
                .append(LIST_STRING, list)
                .toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AccountList) {
            return Objects.equals(((AccountList) obj).list, list)
                    && Objects.equals(((AccountList) obj).input, input);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return input.hashCode() + list.hashCode();
    }
}