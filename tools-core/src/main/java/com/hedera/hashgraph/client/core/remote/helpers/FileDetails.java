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

package com.hedera.hashgraph.client.core.remote.helpers;

import org.apache.commons.io.FilenameUtils;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;

public class FileDetails {
	private final BasicFileAttributes attributes;
	private final String name;
	private final String path;
	private final String extension;

	private FileDetails(BasicFileAttributes attributes, String name, String path, String extension) {
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
	 * @throws IOException
	 */
	public static FileDetails parse(File file) throws IOException {
		var attr = Files.readAttributes(file.toPath(), BasicFileAttributes.class);
		var n = file.getName();
		var p = file.getParent();
		var e = FilenameUtils.getExtension(n);
		return new FileDetails(attr, n, p, e);
	}
}
