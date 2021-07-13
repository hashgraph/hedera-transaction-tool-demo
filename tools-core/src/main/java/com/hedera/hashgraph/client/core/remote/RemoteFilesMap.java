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

package com.hedera.hashgraph.client.core.remote;

import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.interfaces.FileService;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
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

import static com.hedera.hashgraph.client.core.constants.Constants.METADATA_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getExtension;
import static org.apache.commons.io.FilenameUtils.removeExtension;

public class RemoteFilesMap {

	private static final Logger logger = LogManager.getLogger(RemoteFilesMap.class);
	private static final int TO_MS = 1000;
	private static String version;

	private Map<String, RemoteFile> files;

	/**
	 * Constructor
	 *
	 * @param fileList
	 * 		a list of remote files
	 */
	public RemoteFilesMap(final List<RemoteFile> fileList) {
		if (fileList == null) {
			return;
		}
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

	/**
	 * Load remote files from a FileService
	 *
	 * @param fileService
	 * 		the FileService
	 * @param currentVersion
	 * 		the current app version
	 * @return a RemoteFilesMap
	 */
	public RemoteFilesMap fromFile(FileService fileService, String currentVersion) {
		version = currentVersion;
		String location = fileService.getName().equals("Volumes") || fileService.getName().equals(
				"TransactionTools") ? "" : "InputFiles";
		try {
			List<FileDetails> fileDetails = fileService.listFiles(location);
			return new RemoteFilesMap(getRemoteFiles(fileDetails));
		} catch (HederaClientException | ParseException e) {
			logger.info("Files folder not found in FileService {}", fileService.getName());
			return new RemoteFilesMap();
		}
	}

	/**
	 * Load all files into a list of remote files
	 *
	 * @param fileDetails
	 * 		a list of files
	 * @return a list of RemoteFile
	 */
	private List<RemoteFile> getRemoteFiles(List<FileDetails> fileDetails) throws ParseException {
		List<RemoteFile> remoteFiles = getRemoteFileList(fileDetails);

		for (RemoteFile rf : remoteFiles) {
			if (rf.getType().equals(FileType.COMMENT)) {
				continue;
			}

			var linkedComments = findFile(remoteFiles, getBaseName(rf.getName()) + "." + TXT_EXTENSION);
			rf.setComments((linkedComments != null));
			rf.setCommentsFile(rf.hasComments() ? linkedComments : null);

			// Handle the software update
			if (rf.getType().equals(FileType.SOFTWARE_UPDATE) && rf.hasComments()) {
				handleSoftwareUpdate((SoftwareUpdateFile) rf);
			}
		}
		return remoteFiles;
	}

	/**
	 * Software updates are handled differently, since only the latest can be displayed at the time
	 *
	 * @param remoteFile
	 * 		the software update file
	 */
	private void handleSoftwareUpdate(SoftwareUpdateFile remoteFile) throws ParseException {
		var jsonObject = ((CommentFile) remoteFile.getCommentsFile()).getContents();

		if (jsonObject.has("timeStamp")) {
			final var timeStamp = jsonObject.get("timeStamp").getAsLong();
			remoteFile.setNewStamp(timeStamp);
			return;
		}

		if (jsonObject.has("dateStamp")) {
			var dateString = jsonObject.get("dateStamp").getAsString();
			final var newStamp = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").parse(dateString).getTime() / TO_MS;
			remoteFile.setNewStamp(newStamp);
		}
	}

	/**
	 * Given list of files, load all the remote files
	 *
	 * @param fileDetails
	 * 		the list of prospective remote files
	 * @return a list of RemoteFiles
	 */
	private List<RemoteFile> getRemoteFileList(List<FileDetails> fileDetails) {
		List<RemoteFile> remoteFiles = new ArrayList<>();
		for (var f : fileDetails) {
			try {
				if (validFile(f)) {
					RemoteFile remoteFile = getSingleRemoteFile(f);
					if (remoteFile.isValid()) {
						final var remoteLocation = new File(remoteFile.getParentPath() + File.separator + "History",
								getBaseName(remoteFile.getName()) + "." + METADATA_EXTENSION);
						validRemoteAction(remoteFile.getType(), remoteFile, remoteLocation);
						remoteFiles.add(remoteFile);
					}
				}
			} catch (Exception exception) {
				logger.error("Could not load remote file from {} due to error {}", f.getName(), exception.getMessage());
				logger.error(exception);
			}
		}
		return remoteFiles;
	}

	/**
	 * Load a file into a RemoteFile
	 *
	 * @param fileDetails
	 * 		the remote file
	 * @return a RemoteFile
	 */
	private RemoteFile getSingleRemoteFile(FileDetails fileDetails) throws HederaClientException {
		RemoteFile remoteFile;
		var type = getType(getExtension(fileDetails.getName()));
		switch (type) {
			case TRANSACTION:
				remoteFile = new TransactionFile(fileDetails);
				break;
			case LARGE_BINARY:
				remoteFile = new LargeBinaryFile(fileDetails);
				break;
			case BATCH:
				remoteFile = new BatchFile(fileDetails);
				break;
			case COMMENT:
				remoteFile = new CommentFile(fileDetails);
				break;
			case ACCOUNT_INFO:
				remoteFile = new InfoFile(fileDetails);
				break;
			case PUBLIC_KEY:
				remoteFile = new PublicKeyFile(fileDetails);
				break;
			case SOFTWARE_UPDATE:
			case CONFIG:
				remoteFile = getSoftwareUpdateFile(version, fileDetails);
				break;
			case METADATA:
				remoteFile = new MetadataFile(fileDetails);
				break;
			default:
				throw new HederaClientException(String.format("Unrecognized type %s", type));
		}
		return remoteFile;
	}

	private void validRemoteAction(FileType type, RemoteFile remoteFile, File remoteLocation) {
		if (remoteLocation.exists()) {
			long lastDate = 0;
			try {
				var metadataActions =
						new MetadataFile(remoteFile.getName()).getMetadataActions();

				remoteFile.setParentPath(
						removeExtension(remoteLocation.getPath()).concat(".").concat(
								type.getExtension()));

				for (var metadataAction : metadataActions) {
					final var seconds = metadataAction.getTimeStamp().asDuration().getSeconds();
					if (seconds > lastDate) {
						lastDate = seconds;
					}
				}
			} catch (HederaClientException e) {
				logger.error(e);
			}
			remoteFile.setSignDateInSecs(lastDate);
		}
	}

	private RemoteFile getSoftwareUpdateFile(String version, FileDetails f) throws HederaClientException {
		SoftwareUpdateFile remoteFile = new SoftwareUpdateFile(f);

		var splitVersion = version.split(" ");
		var formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
		Date dateTime;
		try {
			dateTime = formatter.parse(splitVersion[3].replace(",", ""));
		} catch (ParseException e) {
			logger.error(e);
			throw new HederaClientException(e);
		}

		remoteFile.setOldVersion(splitVersion[1].replace(",", ""));
		remoteFile.setOldStamp(dateTime.getTime() / TO_MS);
		return remoteFile;
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