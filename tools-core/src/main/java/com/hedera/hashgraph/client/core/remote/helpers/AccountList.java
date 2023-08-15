package com.hedera.hashgraph.client.core.remote.helpers;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.List;
import java.util.stream.Collectors;

public class AccountList {
    private static final String INPUT_STRING = "input";
    private static final String LIST_STRING = "list";

    @JsonProperty()
    private String input;
    @JsonProperty()
    private List<String> list;

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
}