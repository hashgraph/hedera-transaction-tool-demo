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
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static com.hedera.hashgraph.client.core.constants.Constants.INPUT_FILES;
import static com.hedera.hashgraph.client.core.constants.Constants.METADATA_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static com.hedera.hashgraph.client.core.remote.SoftwareUpdateFile.getBuildDateSecondsFromVersionStr;
import static com.hedera.hashgraph.client.core.remote.SoftwareUpdateFile.getSoftwareVersionFromVersionStr;
import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getExtension;
import static org.apache.commons.io.FilenameUtils.removeExtension;

public class RemoteFilesMap {

	private static final Logger LOG = LogManager.getLogger(RemoteFilesMap.class);

	public static final String UNKNOWN_VERSION = "UNKNOWN";
	private final String version;
	private final Map<String, RemoteFile> files = new HashMap<>();
	/**
	 * Constructor
	 *
	 * @param fileList
	 * 		a list of remote files
	 */
	public RemoteFilesMap(final List<RemoteFile> fileList) {
		this(UNKNOWN_VERSION);
		Optional.ofNullable(fileList).ifPresent(list -> {
			for (final var remoteFile : list) {
				files.put(remoteFile.getName(), remoteFile);
			}
		});
	}

	public RemoteFilesMap() {
		this(UNKNOWN_VERSION);
	}

	public RemoteFilesMap(final String version) {
		if (version == null || version.isBlank()) {
			this.version = UNKNOWN_VERSION;
		} else {
			this.version = version;
		}
	}

	/**
	 * Creates a list of the files in the map
	 *
	 * @return an ArrayList
	 */
	public List<RemoteFile> getFiles() {
		final List<RemoteFile> remoteFileList = new ArrayList<>();
		for (final var entry : files.entrySet()) {
			remoteFileList.add(entry.getValue());
		}
		return remoteFileList;
	}

	public RemoteFile get(final String key) {
		return files.getOrDefault(key, null);
	}

	/**
	 * Returns a list of all remote files with a particular File Type
	 *
	 * @param fileType
	 * 		the type of file that we want to filter by
	 * @return a list of RemoteFile of type fileType
	 */
	public List<RemoteFile> getFiles(final FileType fileType) {
		final List<RemoteFile> remoteFileList = new ArrayList<>();
		for (final var entry : files.entrySet()) {
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
	 * @return a RemoteFilesMap
	 */
	public RemoteFilesMap fromFile(final FileService fileService) {
		final String location = fileService.getName().equals("Volumes") || fileService.getName().equals(
				"TransactionTools") ? "" : INPUT_FILES;
		try {
			final List<FileDetails> fileDetails = fileService.listFiles(location);
			return new RemoteFilesMap(getRemoteFiles(fileDetails));
		} catch (final HederaClientException | ParseException e) {
			LOG.info("Files folder not found in FileService {}", fileService.getName());
			return new RemoteFilesMap(version);
		}
	}

	/**
	 * Load all files into a list of remote files
	 *
	 * @param fileDetails
	 * 		a list of files
	 * @return a list of RemoteFile
	 */
	private List<RemoteFile> getRemoteFiles(final List<FileDetails> fileDetails) throws ParseException {
		final List<RemoteFile> remoteFiles = getRemoteFileList(fileDetails);

		for (final RemoteFile rf : remoteFiles) {
			if (rf.getType().equals(FileType.COMMENT)) {
				continue;
			}

			final var linkedComments = findFile(remoteFiles, getBaseName(rf.getName()) + "." + TXT_EXTENSION);
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
	private void handleSoftwareUpdate(final SoftwareUpdateFile remoteFile) throws ParseException {
		final var jsonObject = ((CommentFile) remoteFile.getCommentsFile()).getContents();

		if (jsonObject.has("timeStamp")) {
			final var timeStamp = jsonObject.get("timeStamp").getAsLong();
			remoteFile.setNewStamp(timeStamp);
			return;
		}

		if (jsonObject.has("dateStamp")) {
			final var dateString = jsonObject.get("dateStamp").getAsString();
			final var newStamp = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").parse(dateString).getTime() / 1000;
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
	private List<RemoteFile> getRemoteFileList(final List<FileDetails> fileDetails) {
		final List<RemoteFile> remoteFiles = new ArrayList<>();
		for (final var f : fileDetails) {
			try {
				if (validFile(f)) {
					final RemoteFile remoteFile = getSingleRemoteFile(f);
					if (remoteFile.isValid()) {
						final var remoteLocation = new File(remoteFile.getParentPath() + File.separator + "History",
								getBaseName(remoteFile.getName()) + "." + METADATA_EXTENSION);
						validRemoteAction(remoteFile.getType(), remoteFile, remoteLocation);
						remoteFiles.add(remoteFile);
					}
				} else {
					LOG.info("File will be ignored: " + f.getFullPath());
				}
			} catch (final Exception exception) {
				LOG.error("Could not load remote file from '" + f.getPath() + "/" + f.getName() + "' due to error ", exception);
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
	private RemoteFile getSingleRemoteFile(final FileDetails fileDetails) throws HederaClientException {
		final RemoteFile remoteFile;
		final var type = getType(getExtension(fileDetails.getName()));
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
			case BUNDLE:
				remoteFile = new BundleFile(fileDetails);
				break;
			case SOFTWARE_UPDATE:
			case CONFIG:
				remoteFile = getSoftwareUpdateFile(fileDetails);
				break;
			case METADATA:
				remoteFile = new MetadataFile(fileDetails);
				break;
			default:
				throw new HederaClientException(
						String.format("Unrecognized type '%s' for file '%s'", type,
								fileDetails.getPath() + "/" + fileDetails.getName()));
		}
		return remoteFile;
	}

	private void validRemoteAction(final FileType type, final RemoteFile remoteFile, final File remoteLocation) {
		if (remoteLocation.exists()) {
			long lastDate = 0;
			try {
				final var metadataActions =
						new MetadataFile(remoteFile.getName()).getMetadataActions();

				remoteFile.setParentPath(
						removeExtension(remoteLocation.getPath()).concat(".").concat(
								type.getExtension()));

				for (final var metadataAction : metadataActions) {
					final var seconds = metadataAction.getTimeStamp().asDuration().getSeconds();
					if (seconds > lastDate) {
						lastDate = seconds;
					}
				}
			} catch (final HederaClientException e) {
				LOG.error(e);
			}
			remoteFile.setSignDateInSecs(lastDate);
		}
	}
	private RemoteFile getSoftwareUpdateFile(final FileDetails f) throws HederaClientException {
		if (Objects.equals(UNKNOWN_VERSION, version)) {
			throw new IllegalStateException("Can not create SoftwareUpdateFile since current version is unknown");
		}
		final SoftwareUpdateFile remoteFile = new SoftwareUpdateFile(f);

		remoteFile.setOldVersion(getSoftwareVersionFromVersionStr(version));
		remoteFile.setOldStamp(getBuildDateSecondsFromVersionStr(version));
		return remoteFile;
	}

	private static FileType getType(final String extension) throws HederaClientException {
		for (final var type :
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
		final List<RemoteFile> remoteFileList = new ArrayList<>();
		for (final var entry : files.entrySet()) {
			final var value = entry.getValue();
			if (value.isExpired()) {
				LOG.info("Transaction {} is expired", value.getBaseName());
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
	public boolean exists(final String name) {
		return (files.containsKey(name));
	}

	/**
	 * Removes a file from the map
	 *
	 * @param name
	 * 		the name of the file
	 * @return true if the file has been successfully removed
	 */
	public boolean remove(final String name) {
		if (exists(name)) {
			final var removedFile = files.remove(name);
			LOG.info("File {} removed from map", removedFile.getName());
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
	public void add(final RemoteFile remoteFile) {
		/* Since the whole system is based on filenames, we will not allow repeated names across remote folders */
		if (!this.files.containsKey(remoteFile.getName())) {
			this.files.put(remoteFile.getName(), remoteFile);
		} else {
			LOG.info("Duplicated file: Ignored");
		}
	}


	/**
	 * Adds all the members of a second map to the first
	 *
	 * @param remoteFilesMap
	 * 		a RemoteFilesMap
	 */
	public void addAll(final RemoteFilesMap remoteFilesMap) {
		for (final var rf :
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
	public void addAllNotExpired(final RemoteFilesMap remoteFilesMap) {
		// First treat the software updates (Only the latest should be shown)
		var s = new SoftwareUpdateFile();
		for (final var file : remoteFilesMap.getFiles(FileType.SOFTWARE_UPDATE)) {
			if (file.compareTo(s) > 0) {
				s = (SoftwareUpdateFile) file;
			}
		}
		if (!s.isExpired() && s.getTimestamp() != null) {
			add(s);
		}

		// Filter our all software updates
		for (final var rf : remoteFilesMap.getFilesNotExpired()) {
			if (!(rf instanceof SoftwareUpdateFile)) {
				add(rf);
			}
		}

	}

	public int countType(final FileType type) {
		var counter = 0;
		for (final var entry : files.entrySet()) {
			if (entry.getValue().getType().equals(type)) {
				counter++;
			}
		}
		return counter;
	}

	public void clearMap() {
		files.clear();
	}

	private static boolean validFile(final FileDetails file) {
		if (Paths.get(file.getFullPath()).toFile().isDirectory()) {
			return false;
		}
		final var extension = file.getExtension();

		try {
			return FileType.getType(extension) != FileType.UNKNOWN;
		} catch (HederaClientException e) {
			// No need to log or deal with this exception, the extension is invalid, return false
		}
		return false;
	}

	private static RemoteFile findFile(final List<RemoteFile> remoteFiles, final String commentName) {
		for (final var rf :
				remoteFiles) {
			if (commentName.equals(rf.getName())) {
				return rf;
			}
		}
		return null;
	}
}