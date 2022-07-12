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


package com.hedera.hashgraph.client.core.remote.helpers;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;

public class FileDetails {
	private static final Logger logger = LogManager.getLogger(FileDetails.class);

	private final BasicFileAttributes attributes;
	private final String name;
	private final String path;
	private final String extension;

	public FileDetails(final BasicFileAttributes attributes, final String name, final String path,
			final String extension) {
		this.attributes = attributes;
		this.name = name;
		this.path = path;
		this.extension = extension;

	}

	public BasicFileAttributes getAttributes() {
		return attributes;
	}

	public String getName() {
		return name;
	}

	public String getBaseName() {
		return FilenameUtils.getBaseName(name);
	}

	public String getPath() {
		return path;
	}

	public String getFullPath() {
		return path + "/" + name;
	}

	public String getExtension() {
		return extension;
	}

	/**
	 * Reads the file details from a file
	 *
	 * @param file
	 * 		the file
	 * @return a File details object
	 */
	public static FileDetails parse(final File file) throws HederaClientException {
		try {
			final var attr = Files.readAttributes(file.toPath(), BasicFileAttributes.class);
			final var n = file.getName();
			final var p = file.getParent();
			final var e = FilenameUtils.getExtension(n);
			return new FileDetails(attr, n, p, e);
		} catch (final IOException | IllegalArgumentException ex) {
			logger.error(ex.getMessage());
			throw new HederaClientException(ex);
		}
	}
}
