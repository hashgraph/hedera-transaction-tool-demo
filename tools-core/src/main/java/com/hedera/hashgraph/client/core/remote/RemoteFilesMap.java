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

package com.hedera.hashgraph.client.core.remote;

import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.interfaces.FileService;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RemoteFilesMap {

	private static final Logger logger = LogManager.getLogger(RemoteFilesMap.class);
	private static final int TO_MS = 1000;

	private Map<String, RemoteFile> files;
	// versionDateString is of the form XX.XX.XXXX dateTimeString

	/**
	 * Constructor
	 *
	 * @param fileList
	 * 		a list of remote files
	 */
	public RemoteFilesMap(final List<RemoteFile> fileList) {
		assert (fileList != null);
		files = new HashMap<>();
		for (var remoteFile : fileList) {
			files.put(remoteFile.getName(), remoteFile);
		}
	}

	public RemoteFilesMap() {
		files = new HashMap<>();
	}

	/**
	 * Creates a list of the files in the map
	 *
	 * @return an ArrayList
	 */
	public List<RemoteFile> getFiles() {
		List<RemoteFile> remoteFileList = new ArrayList<>();
		for (var entry : files.entrySet()) {
			remoteFileList.add(entry.getValue());
		}
		return remoteFileList;
	}

	public RemoteFile get(String key) {
		return files.getOrDefault(key, null);
	}

	/**
	 * Returns a list of all remote files with a particular File Type
	 *
	 * @param fileType
	 * 		the type of file that we want to filter by
	 * @return a list of RemoteFile of type fileType
	 */
	public List<RemoteFile> getFiles(FileType fileType) {
		List<RemoteFile> remoteFileList = new ArrayList<>();
		for (var entry : files.entrySet()) {
			final var value = entry.getValue();
			if (value.getType().equals(fileType)) {
				remoteFileList.add(value);
			}
		}
		return remoteFileList;
	}


	public RemoteFilesMap fromFile(FileService fileService, String version) {
		String location = fileService.getName().equals("Volumes") || fileService.getName().equals(
				"TransactionTools") ? "" : "InputFiles";
		List<FileDetails> files;
		try {
			files = fileService.listFiles(location);
		} catch (HederaClientException e) {
			logger.info(String.format("Files folder not found in FileService %s", fileService.getName()));
			return new RemoteFilesMap();
		}

		List<RemoteFile> remoteFiles = new ArrayList<>();
		for (var f : files) {
			try {
				if (validFile(f)) {
					var type = getType(FilenameUtils.getExtension(f.getName()));
					RemoteFile remoteFile;
					switch (type) {
						case TRANSACTION:
							remoteFile = new TransactionFile(f);
							break;
						case LARGE_BINARY:
							remoteFile = new LargeBinaryFile(f);
							break;
						case BATCH:
							remoteFile = new BatchFile(f);
							break;
						case COMMENT:
							remoteFile = new CommentFile(f);
							break;
						case ACCOUNT_INFO:
							remoteFile = new InfoFile(f);
							break;
						case PUBLIC_KEY:
							remoteFile = new PublicKeyFile(f);
							break;
						case SOFTWARE_UPDATE:
						case CONFIG:
							remoteFile = new SoftwareUpdateFile(f);

							var splitVersion = (version).split(" ");
							var formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
							Date dateTime;
							try {
								dateTime = formatter.parse(splitVersion[3].replace(",", ""));
							} catch (ParseException e) {
								logger.error(e);
								throw new HederaClientException(e);
							}

							((SoftwareUpdateFile) remoteFile).setOldVersion(splitVersion[1].replace(",", ""));
							((SoftwareUpdateFile) remoteFile).setOldStamp(dateTime.getTime() / TO_MS);
							break;
						case METADATA:
							remoteFile = new MetadataFile(f);
							break;
						default:
							throw new HederaClientException(String.format("Unrecognized type %s", type));
					}

					if (remoteFile.isValid()) {
						final var remoteLocation = new File(remoteFile.getParentPath() + "/History/" +
								FilenameUtils.removeExtension(remoteFile.getName()).concat(".meta"));
						if (remoteLocation.exists()) {
							long lastDate = 0;
							try {
								var metadataActions =
										new MetadataFile(remoteFile.getName()).getMetadataActions();

								remoteFile.setParentPath(
										FilenameUtils.removeExtension(remoteLocation.getPath()).concat(".").concat(
												type.getExtension()));

								for (var metadataAction : metadataActions) {
									final var seconds = metadataAction.getTimestamp().asDuration().getSeconds();
									if (seconds > lastDate) {
										lastDate = seconds;
									}
								}
							} catch (HederaClientException e) {
								logger.error(e);
							}
							remoteFile.setSignDateInSecs(lastDate);
						}
						remoteFiles.add(remoteFile);
					}
				}
			} catch (Exception exception) {
				logger.error("Could not load remote file from {} due to error {}", f.getName(), exception.getMessage());
				logger.error(exception);
			}
		}

		for (var rf :
				remoteFiles) {
			if (!rf.getType().equals(FileType.COMMENT)) {
				var commentName = FilenameUtils.getBaseName(rf.getName()) + ".txt";
				var linkedComments = findFile(remoteFiles, commentName);
				rf.setComments((linkedComments != null));
				rf.setCommentsFile(rf.hasComments() ? linkedComments : null);
				if (rf.getType().equals(FileType.SOFTWARE_UPDATE) && rf.hasComments()) {
					var jsonObject = ((CommentFile) rf.getCommentsFile()).getContents();
					if (jsonObject.has("timeStamp")) {
						((SoftwareUpdateFile) rf).setNewStamp(jsonObject.get("timeStamp").getAsLong());
					} else if (jsonObject.has("dateStamp")) {
						var dateString = jsonObject.get("dateStamp").getAsString();
						try {
							((SoftwareUpdateFile) rf).setNewStamp(
									new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").parse(dateString).getTime() / TO_MS);
						} catch (ParseException e) {
							logger.error("Bad time format in text file");
						}
					}
				}
			}
		}
		return new RemoteFilesMap(remoteFiles);
	}

	private static FileType getType(String extension) throws HederaClientException {
		for (var type :
				FileType.values()) {
			if (type.getExtension().equals(extension)) {
				return type;
			}
		}
		throw new HederaClientException(String.format("Unrecognized extension %s", extension));
	}

	/**
	 * Filters out the expired transactions from the files map
	 *
	 * @return a list of remote files that are not expired
	 */
	public List<RemoteFile> getFilesNotExpired() {
		List<RemoteFile> remoteFileList = new ArrayList<>();
		for (var entry : files.entrySet()) {
			final var value = entry.getValue();
			if (value.isExpired()) {
				logger.info("Transaction {} is expired", value.getBaseName());
				continue;
			}
			remoteFileList.add(value);
		}
		return remoteFileList;
	}


	/**
	 * Returns the number of files in the map (expired and non-expired)
	 *
	 * @return an int
	 */
	public int size() {
		return files.size();
	}

	/**
	 * Checks if a particular file exists in the map
	 *
	 * @param name
	 * 		the filename
	 * @return true if the file can be found in the map
	 */
	public boolean exists(String name) {
		return (files.containsKey(name));
	}

	/**
	 * Removes a file from the map
	 *
	 * @param name
	 * 		the name of the file
	 * @return true if the file has been successfully removed
	 */
	public boolean remove(String name) {
		if (exists(name)) {
			var removedFile = files.remove(name);
			logger.info("File {} removed from map", removedFile.getName());
			return true;
		}
		return false;
	}

	/**
	 * Adds a file to the map. It does not replace files that already exist
	 *
	 * @param remoteFile
	 * 		a remote file
	 */
	public void add(RemoteFile remoteFile) {
		/* Since the whole system is based on filenames, we will not allow repeated names across remote folders */
		if (!this.files.containsKey(remoteFile.getName())) {
			this.files.put(remoteFile.getName(), remoteFile);
		} else {
			logger.info("Duplicated file: Ignored");
		}
	}


	/**
	 * Adds all the members of a second map to the first
	 *
	 * @param remoteFilesMap
	 * 		a RemoteFilesMap
	 */
	public void addAll(RemoteFilesMap remoteFilesMap) {
		for (var rf :
				remoteFilesMap.getFiles()) {
			add(rf);
		}
	}

	/**
	 * Add all the members of the second map that are not expired to the first map
	 *
	 * @param remoteFilesMap
	 * 		a RemoteFilesMap
	 */
	public void addAllNotExpired(RemoteFilesMap remoteFilesMap) {
		// First treat the software updates (Only the latest should be shown)
		var s = new SoftwareUpdateFile();
		for (var file : remoteFilesMap.getFiles(FileType.SOFTWARE_UPDATE)) {
			if (file.compareTo(s) > 0) {
				s = (SoftwareUpdateFile) file;
			}
		}
		if (!s.isExpired()) {
			add(s);
		}

		// Filter our all software updates
		for (var rf : remoteFilesMap.getFilesNotExpired()) {
			if (!(rf instanceof SoftwareUpdateFile)) {
				add(rf);
			}
		}

	}


	public int countType(FileType type) {
		var counter = 0;
		for (var entry : files.entrySet()) {
			if (entry.getValue().getType().equals(type)) {
				counter++;
			}
		}
		return counter;
	}

	public void clearMap() {
		files = new HashMap<>();
	}

	private static boolean validFile(FileDetails file) {
		var extension = file.getExtension();

		for (var type : FileType.values()) {
			if (type.getExtension().equals(extension)) {
				return true;
			}
		}
		return false;
	}

	private static RemoteFile findFile(List<RemoteFile> remoteFiles, String commentName) {
		for (var rf :
				remoteFiles) {
			if (commentName.equals(rf.getName())) {
				return rf;
			}
		}
		return null;
	}
}