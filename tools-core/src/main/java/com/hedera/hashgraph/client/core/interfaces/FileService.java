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

package com.hedera.hashgraph.client.core.interfaces;

import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;

import java.io.File;
import java.util.List;

public interface FileService {

	/**
	 * Downloads a file from a remote location to a local one.
	 *
	 * @param path
	 * 		Remote location of the file (could be cloud or USB)
	 * @param localDestination
	 * 		Location where we will store the file locally
	 */
	File download(String path, String localDestination) throws HederaClientException;

	/**
	 * Uploads a file to the remote root of the file service.
	 *
	 * @param file
	 * 		Path to the file to be uploaded
	 */
	void upload(String file, String remoteDestination) throws HederaClientException;

	/**
	 * List all files in the root path
	 *
	 */
	List<FileDetails> listFiles() throws HederaClientException;

	/***
	 * List all files in the directory pointed on by path+folder
	 * @param path inner directory where we are searching for files
	 */
	List<FileDetails> listFiles(String path) throws HederaClientException;

	/**
	 * List all files in the root path
	 *
	 */
	List<String> listFilePaths() throws HederaClientException;

	/**
	 * Returns the details of a file if it exists in the remote location.
	 *
	 * @param name
	 * 		Name of the file that we are searching for.
	 */
	FileDetails find(String name) throws HederaClientException;

	/**
	 * Returns true if a file exists in the remote location. False otherwise
	 *
	 * @param name
	 * 		Name of the file we are searching for
	 */
	boolean exists(String name) throws HederaClientException;

	/**
	 * Rename a file in the remote location
	 *
	 * @param oldName
	 * 		Original name of the file
	 * @param newName
	 * 		New name of the file
	 */
	void rename(String oldName, String newName) throws HederaClientException;

	/**
	 * Returns true if the remote service is up (in the case of local files, if the directory exists or the USB is
	 * plugged in)
	 *
	 */
	boolean exists() throws HederaClientException;

	/**
	 * Returns a name that represents the file service
	 *
	 */
	String getName();

	String getPath();

	/**
	 * Returns the time in milliseconds of the last modified file
	 *
	 * @return a long
	 */
	long lastModified() throws HederaClientException;
}
