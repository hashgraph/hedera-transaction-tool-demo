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

package com.hedera.hashgraph.client.core.fileServices;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
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

	public LocalFileServiceAdapter(String path) {
		this.path = path;
	}

	@Override
	public File download(String name, String localDestination) throws HederaClientException {
		var remote = new File(path, name);
		if (!remote.exists()) {
			throw new HederaClientException("File does not exist");
		}
		if (remote.isDirectory()) {
			throw new HederaClientException("File is a directory");
		}
		var local = new File(localDestination);
		if (local.exists()) {
			local.delete();
		}
		try {
			FileUtils.copyFile(remote, local);
		} catch (IOException e) {
			throw new HederaClientException(e);
		}
		return local;
	}

	@Override
	public void upload(String fileName, String remoteDestination) throws HederaClientException {
		logger.info("Exporting to output");
		var remote = new File(path, remoteDestination);
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
			var remotes = remote.listFiles();
			assert remotes != null;
			logger.info("{} files in folder {}", remotes.length, remote.getAbsolutePath());
		} catch (IOException e) {
			throw new HederaClientException(e);
		}
	}

	@Override
	public List<FileDetails> listFiles() throws HederaClientException {
		return listFiles("");
	}

	@Override
	public List<FileDetails> listFiles(String dir) throws HederaClientException {
		List<FileDetails> directory = new ArrayList<>();
		final var folder = new File(path + File.separator + dir);
		if (!folder.exists()) {
			throw new HederaClientException(String.format("Directory %s does not exist", dir));
		}
		if (!folder.isDirectory()) {
			throw new HederaClientException("Path must point to a folder");
		}
		var files = folder.listFiles(pathname -> !pathname.isHidden());

		assert files != null;
		for (var f : files) {
			try {
				directory.add(FileDetails.parse(f));
			} catch (IOException e) {
				logger.error(e);
			}
		}
		return directory;
	}

	@Override
	public List<String> listFilePaths() throws HederaClientException {
		var files = listFiles("");
		List<String> paths = new ArrayList<>();
		for (var file : files) {
			paths.add(file.getFullPath());
		}
		return paths;
	}

	@Override
	public FileDetails find(String name) throws HederaClientException {

		var files = listFiles(FilenameUtils.getPath(name));
		FileDetails details = null;
		for (var d : files) {
			if (FilenameUtils.getName(name).equals(d.getName())) {
				details = d;
			}
		}
		return details;
	}

	@Override
	public boolean exists(String name) {
		return new File(path, name).exists();
	}

	@Override
	public void rename(String oldName, String newName) throws HederaClientException {
		try {
			var oldFile = new File(path, oldName);
			if (oldFile.exists() && oldFile.renameTo(new File(path, newName))) {
				logger.info("Renamed file {} to {}", oldName, newName);
			}
		} catch (Exception e) {
			throw new HederaClientException(e);
		}
	}

	@Override
	public boolean exists() {
		return new File(path).exists();
	}

	@Override
	public String getName() {
		var pathName = Paths.get(path);
		return pathName.getParent().getFileName().toString();
	}

	@Override
	public long lastModified() throws HederaClientException {
		var remote = new File(path).listFiles();
		long lastMod = 0;
		assert remote != null;
		for (File file : remote) {
			BasicFileAttributes attr;
			try {
				attr = Files.readAttributes(file.toPath(), BasicFileAttributes.class);
			} catch (IOException e) {
				throw new HederaClientException(e);
			}
			final var last = attr.lastModifiedTime().toMillis();
			if (last > lastMod) {
				lastMod = last;
			}
		}
		return lastMod/1000;
	}
}
