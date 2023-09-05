package com.hedera.hashgraph.client.core.remote.helpers;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class AccountList {
    public static final String INPUT_STRING = "input";
    public static final String LIST_STRING = "list";

    private final String input;
    private final List<String> list;

    public AccountList(String input, List<String> list) {
        this.input = input;
        this.list = list;
    }
    public AccountList(JsonObject accountList) {
        input = accountList.get(INPUT_STRING).getAsString();
        list = ((JsonArray)accountList.get(LIST_STRING)).asList().stream()
                .map(JsonElement::getAsString).collect(Collectors.toList());
    }

    public String getInput() {
        return input;
    }

    public List<String> getList() {
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