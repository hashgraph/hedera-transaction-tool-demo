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

package com.hedera.hashgraph.client.core.transactions;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.sdk.PublicKey;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Arrays;

import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNATURE_EXTENSION;

public class SignaturePair implements GenericFileReadWriteAware, Serializable {
	private static final Logger logger = LogManager.getLogger(SignaturePair.class);

	@JsonProperty("publicKey")
	private final byte[] publicKey;
	private final byte[] signature;


	public SignaturePair() {
		publicKey = new byte[64];
		signature = new byte[64];
	}

	public SignaturePair(final PublicKey publicKey, final byte[] signature) {
		this.publicKey = publicKey.toBytes();
		this.signature = signature;
	}

	public SignaturePair(final String location) {
		final var signaturePair = read(location);
		assert signaturePair != null;
		this.publicKey = signaturePair.getPublicKey().toBytes();
		this.signature = signaturePair.getSignature();
	}

	@JsonIgnore
	public PublicKey getPublicKey() {
		return PublicKey.fromBytes(publicKey);
	}

	public byte[] getSignature() {
		return signature;
	}

	public void write(final String filePath) {
		try {

			try (final var fileOut = new FileOutputStream(filePath);
				 final var objectOut = new ObjectOutputStream(fileOut)) {
				objectOut.writeObject(this);
				logger.info("The Object was successfully written to a file");
			}

			// Write the JSON representation - for importing into TTv2
			final String jsonFilePath = filePath.replaceFirst(SIGNATURE_EXTENSION + "$", JSON_EXTENSION);
			final var mapper = new ObjectMapper();
			try (final var jsonOut = new FileOutputStream(jsonFilePath)) {
				mapper.writeValue(jsonOut, this);
				logger.info("The JSON was successfully written to a file");
			}
		} catch (final Exception ex) {
			logger.error(ex);
		}
	}

	private SignaturePair read(final String filePath) {
		var signaturePair = new SignaturePair();

		try (
				final InputStream file = new FileInputStream(filePath);
				final InputStream buffer = new BufferedInputStream(file);
				final ObjectInput input = new ObjectInputStream(buffer)
		) {
			signaturePair = (SignaturePair) input.readObject();
		} catch (final ClassNotFoundException | IOException ex) {
			logger.error("Cannot perform input. Class not found.", ex);
		}
		return signaturePair;
	}

	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof SignaturePair)) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		final var o = (SignaturePair) obj;
		return (Arrays.equals(this.publicKey, o.publicKey)) &&
				(Arrays.equals(this.signature, o.signature));

	}

	@Override
	public int hashCode() {
		return Arrays.hashCode(signature);
	}
}
