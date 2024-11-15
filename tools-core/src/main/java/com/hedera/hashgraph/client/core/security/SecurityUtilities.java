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

package com.hedera.hashgraph.client.core.security;

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.BadMnemonicException;
import com.hedera.hashgraph.sdk.Mnemonic;
import net.i2p.crypto.eddsa.EdDSAPublicKey;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.bouncycastle.crypto.generators.Argon2BytesGenerator;
import org.bouncycastle.crypto.params.Argon2Parameters;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.openpgp.PGPCompressedData;
import org.bouncycastle.openpgp.PGPException;
import org.bouncycastle.openpgp.PGPPublicKeyRingCollection;
import org.bouncycastle.openpgp.PGPSignature;
import org.bouncycastle.openpgp.PGPSignatureList;
import org.bouncycastle.openpgp.jcajce.JcaPGPObjectFactory;
import org.bouncycastle.openpgp.operator.jcajce.JcaKeyFingerprintCalculator;
import org.bouncycastle.openpgp.operator.jcajce.JcaPGPContentVerifierBuilderProvider;
import org.bouncycastle.util.encoders.Hex;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.Security;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.GPG_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUBLIC_KEY_LOCATION;
import static java.nio.charset.StandardCharsets.UTF_8;
import static org.bouncycastle.openpgp.PGPUtil.getDecoderStream;

public class SecurityUtilities {
	private static final Logger logger = LogManager.getLogger(SecurityUtilities.class);

	private SecurityUtilities() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Stores the mnemonic in a password encrypted file
	 *
	 * @param key
	 * 		byte array that contains the key
	 * @param path
	 * 		path to the file where the mnemonic will be stored
	 * @param mnemonic
	 * 		the mnemonic phrase to be encrypted
	 */
	public static void toEncryptedFile(final byte[] key, final String path,
			final String mnemonic) throws HederaClientException {
		final byte[] cipherText;
		try {
			final var iv = generateRandomBytes(Constants.GCM_IV_LENGTH);
			// Get Cipher Instance
			final var cipher = Cipher.getInstance("AES/GCM/NoPadding");

			// Create SecretKeySpec
			final var keySpec = new SecretKeySpec(key, "AES");

			// Create GCMParameterSpec
			final var gcmParameterSpec = new GCMParameterSpec(Constants.GCM_TAG_LENGTH * 8, iv);

			// Initialize Cipher for ENCRYPT_MODE
			cipher.init(Cipher.ENCRYPT_MODE, keySpec, gcmParameterSpec);

			// Perform Encryption
			cipherText = ArrayUtils.addAll(iv, cipher.doFinal(mnemonic.getBytes(UTF_8)));
		} catch (final NoSuchAlgorithmException | NoSuchPaddingException | InvalidKeyException | InvalidAlgorithmParameterException | IllegalBlockSizeException | BadPaddingException e) {
			throw new HederaClientException(e);
		}

		try (final var outputStream = new FileOutputStream(path)) {
			outputStream.write("AES|256|CBC|PKCS5Padding|".getBytes(UTF_8));
			outputStream.write(cipherText);
			ownerReadWritePermissions(path);
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}
	}


	/**
	 * Loads a mnemonic from a password-protected file
	 *
	 * @param passwordChar
	 * 		char array containing the password
	 * @param salt
	 * 		the salt used to generate the key bytes
	 * @param path
	 * 		location of the mnemonic
	 * @return a Mnemonic object
	 */
	public static Mnemonic fromEncryptedFile(final char[] passwordChar, final byte[] salt,
			final String path) throws HederaClientException {

		return (Arrays.equals(new byte[Constants.SALT_LENGTH], salt)) ?
				fromEncryptedFile(keyFromPasswordLegacy(passwordChar), path) :
				fromEncryptedFile(keyFromPassword(passwordChar, salt), path);
	}

	/***
	 * Loads a mnemonic from a password-protected file
	 * @param key byte array where the key is stored
	 * @param path path of the file where the mnemonic is stored
	 * @throws HederaClientException in place of all exceptions
	 */
	public static Mnemonic fromEncryptedFile(final byte[] key, final String path) throws HederaClientException {
		final byte[] data;
		try {
			data = Files.readAllBytes(Paths.get(path));
		} catch (final IOException e) {
			throw new HederaClientException(e);
		}

		final var header = "AES|256|CBC|PKCS5Padding|".getBytes(UTF_8);
		if (data.length < header.length || (!Arrays.equals(header, Arrays.copyOfRange(data, 0, header.length)))) {
			return legacyEncryption(key, data);
		}

		final var cipherText = Arrays.copyOfRange(data, header.length + Constants.GCM_IV_LENGTH, data.length);
		final var iv = Arrays.copyOfRange(data, header.length, header.length + Constants.GCM_IV_LENGTH);
		try {
			// Get Cipher Instance
			final var cipher = Cipher.getInstance("AES/GCM/NoPadding");

			// Create SecretKeySpec
			final var keySpec = new SecretKeySpec(key, "AES");

			// Create GCMParameterSpec
			final var gcmParameterSpec = new GCMParameterSpec(Constants.GCM_TAG_LENGTH * 8, iv);

			// Initialize Cipher for DECRYPT_MODE
			cipher.init(Cipher.DECRYPT_MODE, keySpec, gcmParameterSpec);

			// Perform Decryption
			final var decryptedText = cipher.doFinal(cipherText);
			final var decryptedWords = new String(decryptedText);

			return Mnemonic.fromString(decryptedWords);

		} catch (final NoSuchAlgorithmException | NoSuchPaddingException | InvalidKeyException | InvalidAlgorithmParameterException | IllegalBlockSizeException | BadPaddingException | BadMnemonicException e) {
			throw new HederaClientException(e);
		}
	}

	/**
	 * Given a char[] password calculates the password hash and a random salt
	 *
	 * @param password
	 * 		a char[]
	 * @return a byte[] of size ARGON_SALT_SIZE + 32;
	 */
	public static byte[] keyFromPassword(final char[] password, final byte[] salt) {
		return generateArgon2id(password, salt);
	}

	/**
	 * Creates and stores a private/public key given a mnemonic, an index and a password
	 *
	 * @param keyName
	 * 		location where the pem file will be stored
	 * @param application
	 * 		application that requested the key. Can be "Hedera Transaction Tool" or "Hedera CLI Tool"
	 * @param mnemonic
	 * 		the recovery phrase
	 * @param index
	 * 		the index of the created key
	 * @param password
	 * 		password that will be used to store the encrypted pem
	 * @return true if the key creation succeeded.
	 */
	public static boolean generateAndStoreKey(final String keyName, final String application, final Mnemonic mnemonic,
			final int index,
			final char[] password) {
		try {
			final var keyStore = new Ed25519KeyStore.Builder().withPassword(password).build();
			final var privateKey = Ed25519PrivateKey.fromMnemonic(mnemonic).derive(index);
			final var keyPair = keyStore.insertNewKeyPair(privateKey);
			keyStore.write(keyName, application, index, "0.1", mnemonic.words.hashCode());

			final var pubFile = keyName.replace(Constants.PK_EXTENSION, Constants.PUB_EXTENSION);
			try (final var fos = new FileOutputStream(pubFile)) {
				fos.write(Hex.encode(((EdDSAPublicKey) keyPair.getPublic()).getAbyte()));
			}
		} catch (final KeyStoreException | IOException e) {
			logger.error(e);
			return false;
		}
		return true;
	}

	/**
	 * Generates an array of random bytes
	 *
	 * @param size
	 * 		the size of the array
	 * @return a byte[]
	 */
	public static byte[] generateRandomBytes(final int size) {
		final var secureRandom = new SecureRandom();
		final var salt = new byte[size];
		secureRandom.nextBytes(salt);
		return salt;
	}

	/**
	 * Generates a byte array based on a password and a salt
	 *
	 * @param password
	 * 		the password to be hashed
	 * @param salt
	 * 		an array of random bytes
	 * @return a 32 byte array
	 */
	public static byte[] generateArgon2id(final char[] password, final byte[] salt) {
		final var opsLimit = 3;
		final var memLimit = 262144;
		final var outputLength = 32;
		final var parallelism = 1;
		final var builder = new Argon2Parameters.Builder(Argon2Parameters.ARGON2_id)
				.withVersion(Argon2Parameters.ARGON2_VERSION_13) // 19
				.withIterations(opsLimit)
				.withMemoryAsKB(memLimit)
				.withParallelism(parallelism)
				.withSalt(salt);
		final var gen = new Argon2BytesGenerator();
		gen.init(builder.build());
		final var result = new byte[outputLength];
		gen.generateBytes(new String(password).getBytes(StandardCharsets.UTF_8), result, 0, result.length);
		return result;
	}


	/**
	 * Allows the user to read mnemonic files that have been encrypted by a previous version of the app
	 *
	 * @param key
	 * 		byte array that contains the key
	 * @param data
	 * 		byte array containing the encrypted mnemonic
	 * @return a mnemonic
	 */
	private static Mnemonic legacyEncryption(final byte[] key, final byte[] data) throws HederaClientException {
		try {
			final var c = Cipher.getInstance("AES");
			final var k = new SecretKeySpec(key, "AES");
			c.init(Cipher.DECRYPT_MODE, k);
			final var decryptedWords = new String(c.doFinal(data));
			return Mnemonic.fromString(decryptedWords);
		} catch (final NoSuchAlgorithmException | NoSuchPaddingException | InvalidKeyException | IllegalBlockSizeException | BadPaddingException | BadMnemonicException e) {
			throw new HederaClientException(e);
		}

	}

	/***
	 * Converts a char array to a byte array with 32 elements
	 * @param password char array
	 * @return a byte array
	 */
	public static byte[] keyFromPasswordLegacy(final char[] password) throws HederaClientException {
		/* Derive the key, given password and salt. */
		final var iterationCount = 65536;
		final var keyLength = 256;
		final var salt = new byte[] { 1, 2, 3, 4, 5, 6, 7, 8 };
		try {
			final var factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
			final KeySpec spec = new PBEKeySpec(password, salt, iterationCount, keyLength);
			final var tmp = factory.generateSecret(spec);
			final SecretKey secret = new SecretKeySpec(tmp.getEncoded(), "AES");
			return secret.getEncoded();
		} catch (final NoSuchAlgorithmException | InvalidKeySpecException e) {
			throw new HederaClientException(e);
		}
	}

	/**
	 * Given a file, a signature, and a key, verify that the file has been signed
	 *
	 * @param filePath
	 * 		path to the source file
	 * @param pathToSignature
	 * 		path to the signature file
	 * @param pathToPublicKey
	 * 		path to the gpg public key
	 * @return true if the key has signed the file
	 */
	public static boolean verifyFile(final String filePath, final String pathToSignature,
			final String pathToPublicKey) throws IOException, PGPException, HederaClientException {

		CommonMethods.checkFiles(filePath, pathToPublicKey, pathToSignature);

		InputStream in = new BufferedInputStream((new FileInputStream(pathToSignature)));
		Security.addProvider(new BouncyCastleProvider());
		final boolean verify;
		try (final InputStream keyIn = new BufferedInputStream(new FileInputStream(pathToPublicKey))) {
			in = getDecoderStream(in);
			var pgpFactory = new JcaPGPObjectFactory(in);
			final PGPSignatureList p3;

			final var object = pgpFactory.nextObject();
			if (object instanceof PGPCompressedData) {
				final var c1 = (PGPCompressedData) object;
				pgpFactory = new JcaPGPObjectFactory(c1.getDataStream());
				p3 = (PGPSignatureList) pgpFactory.nextObject();
			} else {
				p3 = (PGPSignatureList) object;
			}

			final var pgpPubRingCollection =
					new PGPPublicKeyRingCollection(getDecoderStream(keyIn), new JcaKeyFingerprintCalculator());

			final PGPSignature sig;
			try (final InputStream dIn = new BufferedInputStream(new FileInputStream(filePath))) {

				sig = p3.get(0);
				final var key = pgpPubRingCollection.getPublicKey(sig.getKeyID());

				sig.init(new JcaPGPContentVerifierBuilderProvider().setProvider("BC"), key);

				int ch;
				while ((ch = dIn.read()) >= 0) {
					sig.update((byte) ch);
				}
			}

			verify = sig.verify();

			if (verify) {
				logger.debug("signature verified.");
			} else {
				logger.warn("signature verification failed.");
			}
			in.close();
		}
		return verify;
	}

	/**
	 * Set owner read/write permissions only
	 *
	 * @param path
	 * 		the location of the file
	 * @throws IOException
	 * 		if there are issues changing permissions
	 */
	public static void ownerReadWritePermissions(final String path) throws IOException {
		final Set<PosixFilePermission> perms = new HashSet<>();
		perms.add(PosixFilePermission.OWNER_READ);
		perms.add(PosixFilePermission.OWNER_WRITE);
		Files.setPosixFilePermissions(Path.of(path), perms);
	}

	public static boolean verifySignature(final String filePath) {
		try {
			final var signaturePath = filePath + "." + GPG_EXTENSION;
			if (!new File(signaturePath).exists()) {
				logger.warn("Cannot find signature file");
				return false;
			}
			if (!new File(DEFAULT_STORAGE + PUBLIC_KEY_LOCATION).exists()) {
				logger.error("Cannot find gpg public key file");
				return false;
			}
			return SecurityUtilities.verifyFile(filePath, signaturePath, DEFAULT_STORAGE + PUBLIC_KEY_LOCATION);
		} catch (final Exception e) {
			logger.error(e);
			return false;
		}
	}
}
