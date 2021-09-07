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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.transactions.SignaturePair;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import com.hedera.hashgraph.sdk.Transaction;
import com.hedera.hashgraph.sdk.TransactionId;
import com.hedera.hashgraph.sdk.TransactionReceipt;

import java.util.Map;
import java.util.Set;

/**
 * Main interface between the tools and the SDK.
 */
public interface SDKInterface {


	/**
	 * Signs the transaction with the provided key
	 *
	 * @param key
	 * 		a key to sign the transaction. Can be a single public key or a complex key.
	 * @return a signature for the
	 */
	byte[] sign(PrivateKey key) throws HederaClientRuntimeException;

	/**
	 * Adds all signatures to a transaction
	 *
	 * @param signatures
	 * 		list of signatures of the transaction
	 * @return a signed transaction
	 */
	Transaction<?> collate(Map<PublicKey, byte[]> signatures) throws HederaClientRuntimeException;


	/**
	 * Adds all signatures to a transaction
	 *
	 * @param signaturePairs
	 * 		a set of signature pairs
	 * @return a signed transaction
	 */
	Transaction<?> collate(Set<SignaturePair> signaturePairs) throws HederaClientRuntimeException;

	/**
	 * Collates two transactions.
	 *
	 * @param otherTransaction
	 * 		the other transaction
	 * @return a transaction that has all the signatures of both transactions
	 */
	Transaction<?> collate(Transaction<?> otherTransaction) throws HederaClientRuntimeException;

	/**
	 * Verifies the transaction has been signed with the key provided
	 *
	 * @param publicKey
	 * 		a public key
	 * @return true if the key has signed the transaction
	 */
	boolean verify(PublicKey publicKey) throws HederaClientRuntimeException;

	/**
	 * Verifies the transaction has been signed byt the account provided
	 *
	 * @param info
	 * 		account info of the account to test
	 * @return true if the account has signed
	 */
	boolean verify(AccountInfo info) throws HederaClientException;

	/**
	 * Submits a transaction to the network.
	 *
	 * @return the transaction receipt from the network
	 */
	TransactionReceipt submit() throws HederaClientRuntimeException, InterruptedException;

	/**
	 * Checks the input provided before attempting to build a transaction. Provides feedback to the user regarding
	 * missing fields
	 *
	 * @param input
	 * 		a Json object containing the fields required to build a transaction
	 * @return true if all the required fields are present
	 */
	boolean checkInput(JsonObject input) throws HederaClientException;

	/**
	 * Gets the transaction id from the main transaction
	 *
	 * @return a TransactionId corresponding to the created object
	 */
	TransactionId getTransactionId();


	/**
	 * Stores the transaction to disk
	 *
	 * @param location
	 * 		String path to the storage location
	 * @return true if the operation succeeded
	 */
	String store(String location) throws HederaClientException;

	/**
	 * Reads the transaction from disk
	 *
	 * @param location
	 * 		String path to the storage location
	 * @return true if the operation succeeded
	 */
	boolean read(String location) throws HederaClientException;

	/**
	 * Converts a transaction to a json file for ease of interpretation
	 *
	 * @return a json object with the relevant fields of the transaction
	 */
	JsonObject asJson();

	/**
	 * Converts the transaction into bytes
	 *
	 * @return a byte array
	 */
	byte[] toBytes();
}
