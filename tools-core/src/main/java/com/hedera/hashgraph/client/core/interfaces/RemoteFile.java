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
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;

/**
 * Remote files are classified as executable, when they can be signed, and informational.
 * This interface defines the actions that can be taken by the user on remote files.
 */
public interface RemoteFile {

	/**
	 * Executes the action prescribed by the file. For example:
	 * Transaction Files: transactions are signed and results are stored;
	 * CSV files: transactions are created, signed and results are stored;
	 * File Updates: Files are partitioned, transactions are created, signed and results are stored;
	 * After execution, files are marked as history and moved to the corresponding folder. Metadata files are created
	 * detailing the action.
	 *
	 * @return true if the execution has succeeded.
	 * @throws HederaClientRuntimeException
	 */
	boolean execute() throws HederaClientRuntimeException;

	/**
	 * Declines to execute the action. Files are marked as history and moved to the corresponding folder. Metadata files
	 * are created detailing the action.
	 *
	 * @return true if the file has been moved to the history folder successfully.
	 * @throws HederaClientRuntimeException
	 */
	boolean decline() throws HederaClientRuntimeException;

	/**
	 * Check if a remote file is expired.
	 *
	 * @return true if the file has expired. For transactions, this will mean that the transaction valid start is in the
	 * 		past. For info files, this would mean that either the file is identical to one already imported, or the file
	 * 		modification date is older than the file already imported.
	 */
	boolean isExpired();

	/**
	 * Checks if the current object is equal to another
	 * @param o an object to be compared
	 * @return true if the objects are the same.
	 */
	boolean equalTo(Object o) throws HederaClientException;

	/**
	 * Compares to another object
	 * @param o another object
	 * @return -1 if less, 0 if equal 1 if more
	 */
	int compareTo(Object o) throws HederaClientException;
}
