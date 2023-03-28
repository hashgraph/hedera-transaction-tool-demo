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

package com.hedera.hashgraph.client.ui.utilities;

import javafx.application.Platform;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;

public class ReloadFilesWatcher implements Runnable {
    private final WatchService watcher;
    private final WatchKey key;
    private final Runnable runnable;


    /**
     * Creates a WatchService and registers the given directory
     */
    public ReloadFilesWatcher(Path dir, Runnable method) throws IOException {
        this.watcher = FileSystems.getDefault().newWatchService();
        this.key = dir.register(watcher,
                StandardWatchEventKinds.ENTRY_CREATE,
                StandardWatchEventKinds.ENTRY_DELETE,
                StandardWatchEventKinds.ENTRY_MODIFY);
        this.runnable = method;

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            try {
                this.watcher.close();
            } catch (IOException e) {
                // Not concerned with an exception at this point.
            }
        }));
    }

    public void run() {
        try {
            var keepTrying = true;
            while (keepTrying) {
                // Wait for key to be signalled
                final var watchKey = watcher.take();

                if (this.key != watchKey) {
                    continue;
                }

                // If take happened, this should always be true
                if (!watchKey.pollEvents().isEmpty()) {
                    // Initialize the pane
                    Platform.runLater(runnable);
                }

                // Reset key, if it fails, break from the loop
                keepTrying = watchKey.reset();
            }
        } catch (InterruptedException e) {
            // Do nothing except interrupt the current thread (which is likely why this was interrupted anyway)
            Thread.currentThread().interrupt();
        }
    }
}
