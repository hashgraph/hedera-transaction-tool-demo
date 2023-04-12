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

import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import javafx.application.Platform;
import org.apache.commons.io.FilenameUtils;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.List;
import java.util.function.BiConsumer;

public class ReloadFilesWatcher implements Runnable {
    private final WatchService watcher;
    private final WatchKey key;
    private final BiConsumer<String, WatcherType> method;

    public enum WatcherType { ADD, REMOVE, MODIFIED}

    /**
     * Creates a WatchService and registers the given directory
     */
    public ReloadFilesWatcher(Path dir, BiConsumer<String, WatcherType> method) throws IOException {
        this.watcher = FileSystems.getDefault().newWatchService();
        this.key = dir.register(watcher,
                StandardWatchEventKinds.ENTRY_CREATE,
                StandardWatchEventKinds.ENTRY_DELETE,
                StandardWatchEventKinds.ENTRY_MODIFY);
        this.method = method;

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
                //TODO watcher was null when I closed the app?
                final var watchKey = watcher.take();

                if (this.key != watchKey) {
                    continue;
                }

//                if instead of this, it sends teh poll events into another thread,
//                that thread would go through each and build a list of paths
//                for (WatchEvent<?> event : wk.pollEvents()) {
//                    //we only register "ENTRY_MODIFY" so the context is always a Path.
//                    final Path changed = (Path) event.context();
//                    then send that list to the runnable, which can then just be a function instead of a runnable

                // If 'take' happened, this should always be true
                final var events = watchKey.pollEvents();
                if (!events.isEmpty()) {
                    // Initialize the pane
//                    Platform.runLater(runnable);
                    Platform.runLater(() -> processEvents(watchKey, events));
                }

                // Reset key, if it fails, break from the loop
                keepTrying = watchKey.reset();
            }
        } catch (InterruptedException e) {
            // Do nothing except interrupt the current thread (which is likely why this was interrupted anyway)
            Thread.currentThread().interrupt();
        }
    }

    public void processEvents(final WatchKey key, final List<WatchEvent<?>> events) {
        final var dir = (Path)key.watchable();
        events.forEach(e -> {
            final var path = dir.resolve((Path)e.context()).toAbsolutePath();
            // When a directory is created inside the watched directory, it doesn't think it is a directory
            // at this point, so it will pass through
            if (!Files.isDirectory(path)) {
                final var pathString = path.toString();
                try {
                    // If the fileType is UNKNOWN (or if getting the fileType fails due to not being supported)
                    // this will short circuit (or fail through)
                    if (FileType.UNKNOWN == FileType.getType(FilenameUtils.getExtension(pathString))) {
                        return;
                    }
                    var watchType = WatcherType.ADD;
                    final var kind = e.kind().name();
                    if (StandardWatchEventKinds.ENTRY_MODIFY.name().equals(kind)) {
                        watchType = WatcherType.MODIFIED;
                    } else if (StandardWatchEventKinds.ENTRY_DELETE.name().equals(kind)) {
                        watchType = WatcherType.REMOVE;
                    }
                    method.accept(pathString, watchType);
                } catch (HederaClientException ex) {
                    // No need to handle the error, the path is just not a file that should be dealt with
                }
            }
        });
    }
}
