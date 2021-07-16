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

package com.hedera.hashgraph.client.core.security;

import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.sdk.Mnemonic;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.edec.EdECObjectIdentifiers;
import org.bouncycastle.asn1.pkcs.PrivateKeyInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.crypto.digests.SHA512Digest;
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator;
import org.bouncycastle.crypto.macs.HMac;
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters;
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters;
import org.bouncycastle.crypto.params.KeyParameter;
import org.bouncycastle.math.ec.rfc8032.Ed25519;
import org.bouncycastle.util.encoders.Hex;

import javax.annotation.Nullable;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.text.Normalizer;

/**
 * An ed25519 private key.
 *
 * <p>To obtain an instance, see {@link #generate()}}.
 */
@SuppressWarnings("Duplicates")
public final class Ed25519PrivateKey {
	final Ed25519PrivateKeyParameters privateKeyParams;

	// computed from private key and memoized
	@Nullable
	private Ed25519PublicKey publicKey;

	@Nullable
	private final KeyParameter chainCode;

	private Ed25519PrivateKey(Ed25519PrivateKeyParameters privateKeyParameters) {
		this.privateKeyParams = privateKeyParameters;
		chainCode = null;
	}

	private Ed25519PrivateKey(
			Ed25519PrivateKeyParameters privateKeyParameters,
			@Nullable Ed25519PublicKeyParameters publicKeyParameters) {
		this.privateKeyParams = privateKeyParameters;

		if (publicKeyParameters != null) {
			this.publicKey = new Ed25519PublicKey(publicKeyParameters);
		}
		chainCode = null;
	}

	private Ed25519PrivateKey(Ed25519PrivateKeyParameters privateKeyParameters, @Nullable KeyParameter chainCode) {
		this.privateKeyParams = privateKeyParameters;
		this.publicKey = new Ed25519PublicKey(privateKeyParameters.generatePublicKey());
		this.chainCode = chainCode;
	}

	public static Ed25519PrivateKey fromBytes(byte[] keyBytes) {
		Ed25519PrivateKeyParameters privateKeyParams;
		Ed25519PublicKeyParameters pubKeyParams = null;

		if (keyBytes.length == Ed25519.SECRET_KEY_SIZE) {
			// if the decoded bytes matches the length of a private key, try that
			privateKeyParams = new Ed25519PrivateKeyParameters(keyBytes, 0);
		} else if (keyBytes.length == Ed25519.SECRET_KEY_SIZE + Ed25519.PUBLIC_KEY_SIZE) {
			// some legacy code delivers private and public key pairs concatenated together
			try {
				// this is how we read only the first 32 bytes
				privateKeyParams = new Ed25519PrivateKeyParameters(
						new ByteArrayInputStream(keyBytes, 0, Ed25519.SECRET_KEY_SIZE));
				// read the remaining 32 bytes as the public key
				pubKeyParams = new Ed25519PublicKeyParameters(keyBytes, Ed25519.SECRET_KEY_SIZE);

				return new Ed25519PrivateKey(privateKeyParams, pubKeyParams);
			} catch (IOException e) {
				throw new HederaClientRuntimeException(e);
			}
		} else {
			// decode a properly DER-encoded private key descriptor
			var privateKeyInfo = PrivateKeyInfo.getInstance(keyBytes);

			try {
				var privateKey = privateKeyInfo.parsePrivateKey();
				privateKeyParams = new Ed25519PrivateKeyParameters(((ASN1OctetString) privateKey).getOctets(), 0);

				var pubKeyData = privateKeyInfo.getPublicKeyData();

				if (pubKeyData != null) {
					pubKeyParams = new Ed25519PublicKeyParameters(pubKeyData.getOctets(), 0);
				}

			} catch (IOException e) {
				throw new HederaClientRuntimeException(e);
			}
		}

		return new Ed25519PrivateKey(privateKeyParams, pubKeyParams);
	}

	/** @return a new private key using {@link SecureRandom} */
	public static Ed25519PrivateKey generate() {
		return generate(new SecureRandom());
	}

	/** @return a new private key using the given {@link SecureRandom} */
	public static Ed25519PrivateKey generate(SecureRandom secureRandom) {
		return new Ed25519PrivateKey(new Ed25519PrivateKeyParameters(secureRandom));
	}

	/** @return the public key counterpart of this private key to share with the hashgraph */
	public Ed25519PublicKey getPublicKey() {
		if (publicKey == null) {
			publicKey = new Ed25519PublicKey(privateKeyParams.generatePublicKey());
		}
		return publicKey;
	}

	@Override
	public String toString() {
		PrivateKeyInfo privateKeyInfo;

		try {
			privateKeyInfo = new PrivateKeyInfo(
					new AlgorithmIdentifier(EdECObjectIdentifiers.id_Ed25519),
					new DEROctetString(privateKeyParams.getEncoded()));
		} catch (IOException e) {
			throw new HederaClientRuntimeException(e);
		}

		byte[] encoded;

		try {
			encoded = privateKeyInfo.getEncoded("DER");
		} catch (IOException e) {
			throw new HederaClientRuntimeException(e);
		}

		return Hex.toHexString(encoded);
	}

	/**
	 * Recover a private key from a generated mnemonic phrase and a passphrase.
	 *
	 * This is not compatible with the phrases generated by the Android and iOS wallets;
	 * use the no-passphrase version instead.
	 *
	 * @param mnemonic
	 * 		the mnemonic phrase which should be a 24 byte list of words.
	 * @param passphrase
	 * 		the passphrase used to protect the mnemonic (not used in the
	 * 		mobile wallets, use {@link #fromMnemonic(Mnemonic)} instead.)
	 * @return the recovered key; use {@link #derive(int)} to get a key for an account index (0
	 * 		for default account)
	 */
	public static Ed25519PrivateKey fromMnemonic(Mnemonic mnemonic, String passphrase) {
		// BIP-39 seed generation
		var salt = "mnemonic" + Normalizer.normalize(passphrase, Normalizer.Form.NFKD);
		var pbkdf2 = new PKCS5S2ParametersGenerator(new SHA512Digest());
		pbkdf2.init(
				Normalizer.normalize(mnemonic.toString(), Normalizer.Form.NFKD).getBytes(StandardCharsets.UTF_8),
				salt.getBytes(StandardCharsets.UTF_8),
				2048);

		var key = (KeyParameter) pbkdf2.generateDerivedParameters(512);
		final var seed = key.getKey();

		final var hmacSha512 = new HMac(new SHA512Digest());
		hmacSha512.init(new KeyParameter("ed25519 seed".getBytes(StandardCharsets.UTF_8)));
		hmacSha512.update(seed, 0, seed.length);

		final var derivedState = new byte[hmacSha512.getMacSize()];
		hmacSha512.doFinal(derivedState, 0);

		var derivedKey = derivableKey(derivedState);

		// BIP-44 path with the Hedera Hbar coin-type (omitting key index)
		// we pre-derive most of the path as the mobile wallets don't expose more than the index
		// https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki
		// https://github.com/satoshilabs/slips/blob/master/slip-0044.md
		for (var index : new int[] { 44, 3030, 0, 0 }) {
			derivedKey = derivedKey.derive(index);
		}

		return derivedKey;
	}

	private static Ed25519PrivateKey derivableKey(byte[] deriveData) {
		final var privateKeyParameters = new Ed25519PrivateKeyParameters(deriveData, 0);
		final var chainCode = new KeyParameter(deriveData, 32, 32);
		return new Ed25519PrivateKey(privateKeyParameters, chainCode);
	}

	/**
	 * Recover a private key from a mnemonic phrase compatible with the iOS and Android wallets.
	 * <p>
	 * An overload of {@link #fromMnemonic(Mnemonic, String)} which uses an empty string for the
	 * passphrase.
	 *
	 * @param mnemonic
	 * 		the mnemonic phrase which should be a 24 byte list of words.
	 * @return the recovered key; use {@link #derive(int)} to get a key for an account index (0
	 * 		for default account)
	 */
	public static Ed25519PrivateKey fromMnemonic(Mnemonic mnemonic) {
		return fromMnemonic(mnemonic, "");
	}

	/**
	 * Recover a private key from a mnemonic phrase compatible with the iOS and Android wallets.
	 * <p>
	 * An overload of {@link #fromMnemonic(Mnemonic, String)} which uses an empty string for the
	 * passphrase.
	 *
	 * @param mnemonic
	 * 		the mnemonic phrase which should be a 24 byte list of words.
	 * @param index
	 * 		the index of the requested key. index = 0 should return the wallet key
	 * @return the recovered key;
	 */
	public static Ed25519PrivateKey fromMnemonic(Mnemonic mnemonic, int index) {
		return fromMnemonic(mnemonic, "").derive(index);
	}

	/**
	 * Check if this private key supports derivation.
	 * <p>
	 * This is currently only the case if this private key was created from a mnemonic.
	 */
	public boolean supportsDerivation() {
		return this.chainCode != null;
	}

	/**
	 * Given a wallet/account index, derive a child key compatible with the iOS and Android wallets.
	 * <p>
	 * Use index 0 for the default account.
	 *
	 * @param index
	 * 		the wallet/account index of the account, 0 for the default account.
	 * @return the derived key
	 * @throws IllegalStateException
	 * 		if this key does not support derivation.
	 * @see #supportsDerivation()
	 */
	public Ed25519PrivateKey derive(int index) {
		if (!supportsDerivation()) {
			throw new IllegalStateException("this private key does not support derivation");
		}

		// SLIP-10 child key derivation
		// https://github.com/satoshilabs/slips/blob/master/slip-0010.md#master-key-generation
		final var hmacSha512 = new HMac(new SHA512Digest());

		hmacSha512.init(chainCode);
		hmacSha512.update((byte) 0);

		hmacSha512.update(privateKeyParams.getEncoded(), 0, Ed25519.SECRET_KEY_SIZE);

		// write the index in big-endian order, setting the 31st bit to mark it "hardened"
		final var indexBytes = new byte[4];
		ByteBuffer.wrap(indexBytes).order(ByteOrder.BIG_ENDIAN).putInt(index);
		indexBytes[0] |= (byte) 0b10000000;

		hmacSha512.update(indexBytes, 0, indexBytes.length);

		var output = new byte[64];
		hmacSha512.doFinal(output, 0);

		final var childKeyParams = new Ed25519PrivateKeyParameters(output, 0);
		final var childChainCode = new KeyParameter(output, 32, 32);

		return new Ed25519PrivateKey(childKeyParams, childChainCode);
	}


}
