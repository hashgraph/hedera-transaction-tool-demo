package com.hedera.hashgraph.client.ui.utilities;

import javafx.concurrent.Task;

public abstract class ProgressTask<T> extends Task<T> {
    public abstract String getDescription();
}