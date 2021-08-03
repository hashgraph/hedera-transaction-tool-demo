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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.core.fileservices;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
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

	public static FileService getAdapter(String path) throws HederaClientException {
		if (path != null && !Objects.requireNonNull(path).isEmpty()) {
			try {
				if (path.contains("USB")) {
					logger.debug("USB path {} has been detected", path);
					var usbPath = findVolumes();
					if ("".equals(usbPath)) {
						return null;
					}
					return new LocalFileServiceAdapter(usbPath);
				} else {
					Paths.get(path);
					logger.debug("Local path {} has been detected", path);
					return new LocalFileServiceAdapter(path);
				}
			} catch (InvalidPathException e) {
				throw new HederaClientException("No FileService is created.");
			}
		} else {
			throw new HederaClientException("Path cannot be null");
		}
	}

	private static String findVolumes() {
		final var volume = new File(File.separator + "Volumes");
		if (volume.exists()) {
			var roots = volume.listFiles();
			assert roots != null;
			logger.debug("Found {} volumes", roots.length);
			if (roots.length > 0) {
				for (var volumes : roots) {
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
