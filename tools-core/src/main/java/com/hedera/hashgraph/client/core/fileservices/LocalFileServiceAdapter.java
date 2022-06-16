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
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

public class LocalFileServiceAdapter implements FileService {

	private final String path;
	private static final Logger logger = LogManager.getLogger(LocalFileServiceAdapter.class);

	public String getPath() {
		return path;
	}

	public LocalFileServiceAdapter(final String path) {
		this.path = path;
	}

	@Override
	public File download(final String name, final String localDestination) throws HederaClientException {
		final var remote = new File(path, name);
		if (!remote.exists()) {
			throw new HederaClientException("File does not exist");
		}
		if (remote.isDirectory()) {
			throw new HederaClientException("File is a directory");
		}
		final var local = new File(localDestination);
		try {
			Files.deleteIfExists(local.toPath());
			FileUtils.copyFile(remote, local);
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}
		return local;
	}

	@Override
	public void upload(final String fileName, final String remoteDestination) throws HederaClientException {
		logger.info("Exporting to output");
		final var remote = new File(path, remoteDestination);
		final var file = new File(fileName);

		if (!file.exists()) {
			return;
		}

		if (remote.mkdirs()) {
			logger.info("Output directory created");
		}

		if (!remote.isDirectory()) {
			throw new HederaClientException("Destination must be a directory");
		}
		try {
			logger.info("Exporting {} to {}", file.getAbsolutePath(), remote);
			FileUtils.copyFile(file, new File(remote.getAbsolutePath(), file.getName()));
			final var remotes = remote.listFiles();
			if (remotes == null) {
				throw new HederaClientRuntimeException("Remote file list cannot be read");
			}
			logger.info("{} files in folder {}", remotes.length, remote.getAbsolutePath());
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}
	}

	@Override
	public List<FileDetails> listFiles() throws HederaClientException {
		return listFiles("");
	}

	@Override
	public List<FileDetails> listFiles(final String dir) throws HederaClientException {
		final List<FileDetails> directory = new ArrayList<>();
		final var folder = new File(path + File.separator + dir);
		if (!folder.exists()) {
			throw new HederaClientException(String.format("Directory %s does not exist", dir));
		}
		if (!folder.isDirectory()) {
			throw new HederaClientException("Path must point to a folder");
		}
		final var files = folder.listFiles(pathname -> !pathname.isHidden());
		if (files == null) {
			logger.error("Unable to read file list in listFiles() {}", folder);
			return directory;
		}

		for (final var f : files) {
			directory.add(FileDetails.parse(f));
		}
		return directory;
	}

	@Override
	public List<String> listFilePaths() throws HederaClientException {
		final var files = listFiles("");
		final List<String> paths = new ArrayList<>();
		for (final var file : files) {
			paths.add(file.getFullPath());
		}
		return paths;
	}

	@Override
	public FileDetails find(final String name) throws HederaClientException {

		final var files = listFiles(FilenameUtils.getPath(name));
		FileDetails details = null;
		for (final var d : files) {
			if (FilenameUtils.getName(name).equals(d.getName())) {
				details = d;
			}
		}
		return details;
	}

	@Override
	public boolean exists(final String name) {
		return new File(path, name).exists();
	}

	@Override
	public void rename(final String oldName, final String newName) throws HederaClientException {
		try {
			final var oldFile = new File(path, oldName);
			if (oldFile.exists() && oldFile.renameTo(new File(path, newName))) {
				logger.info("Renamed file {} to {}", oldName, newName);
			}
		} catch (final Exception e) {
			throw new HederaClientException(e);
		}
	}

	@Override
	public boolean exists() {
		return new File(path).exists();
	}

	@Override
	public String getName() {
		final var pathName = Paths.get(path);
		return pathName.getParent().getFileName().toString();
	}

	@Override
	public long lastModified() throws HederaClientException {
		final var remote = new File(path).listFiles();
		if (remote == null) {
			logger.error("Unable to read file list in lastModified() {}", path);
			return -1;
		}
		long lastMod = 0;

		for (final File file : remote) {
			final BasicFileAttributes attr;
			try {
				attr = Files.readAttributes(file.toPath(), BasicFileAttributes.class);
			} catch (final IOException e) {
				throw new HederaClientException(e);
			}
			final var last = attr.lastModifiedTime().toMillis();
			if (last > lastMod) {
				lastMod = last;
			}
		}
		return lastMod / 1000;
	}
}
