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

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Set;

/**
 * Class name is temporary
 *
 * A simple listener to log changes, and add/remove keyfiles from a set of accepted signers.
 */
public class SigningKeyCheckBoxListener implements ChangeListener {
    private static final Logger LOG = LogManager.getLogger(SigningKeyCheckBoxListener.class);
    private final Set<File> signersSet;
    private final File keyFile;
    private final String baseName;

    public SigningKeyCheckBoxListener(final Set<File> signersSet, final File keyFile, final String baseName) {
        this.signersSet = signersSet;
        this.keyFile = keyFile;
        this.baseName = baseName;
    }

    @Override
    public void changed(ObservableValue observable, Object oldValue, Object newValue) {
        if (newValue instanceof Boolean flag) {
            if (flag) {
                LOG.info("Added {} to list of signing keys", baseName);
                signersSet.add(keyFile);
            } else {
                LOG.info("Removed {} from list of signing keys", baseName);
                signersSet.remove(keyFile);
            }
        }
    }
}
