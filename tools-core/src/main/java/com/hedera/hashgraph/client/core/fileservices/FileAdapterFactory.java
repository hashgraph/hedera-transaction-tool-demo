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

package com.hedera.hashgraph.client.core.fileservices;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.interfaces.FileService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.Objects;

public class FileAdapterFactory {

	private static final Logger logger = LogManager.getLogger(FileAdapterFactory.class);

	private FileAdapterFactory() {
	}

	public static FileService getAdapter(final String path) throws HederaClientException {
		if (path != null && !Objects.requireNonNull(path).isEmpty()) {
			try {
				if (path.contains("USB")) {
					logger.debug("USB path {} has been detected", path);
					final var usbPath = findVolumes();
					if ("".equals(usbPath)) {
						return null;
					}
					return new LocalFileServiceAdapter(usbPath);
				} else {
					Paths.get(path);
					logger.debug("Local path {} has been detected", path);
					return new LocalFileServiceAdapter(path);
				}
			} catch (final InvalidPathException e) {
				throw new HederaClientException("No FileService is created.");
			}
		} else {
			throw new HederaClientException("Path cannot be null");
		}
	}

	private static String findVolumes() {
		final var volume = new File(File.separator + "Volumes");
		if (volume.exists()) {
			final var roots = volume.listFiles();
			if (roots == null) {
				throw new HederaClientRuntimeException("Volume list is null");
			}
			logger.debug("Found {} volumes", roots.length);
			//Scan all volumes (MAC only)
			if (roots.length > 0) {
				for (final var volumes : roots) {
					// The volume containing the string "HD" is the default name for the laptop's hard drive
					// Added MACOS to the ignored volumes list, as rebuilt laptops might use it as the name for the
					// hard drive (08/03/2021)
					if (volumes.getName().contains("HD") || volumes.getName().equalsIgnoreCase("MACOS")) {
						continue;
					}
					return volumes.getAbsolutePath();
				}
			}
		}
		return "";
	}
}
