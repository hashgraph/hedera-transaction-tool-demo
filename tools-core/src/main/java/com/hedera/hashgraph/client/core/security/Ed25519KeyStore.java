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

import com.hedera.hashgraph.client.core.constants.ErrorMessages;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import net.i2p.crypto.eddsa.EdDSAPrivateKey;
import net.i2p.crypto.eddsa.EdDSAPublicKey;
import net.i2p.crypto.eddsa.EdDSASecurityProvider;
import net.i2p.crypto.eddsa.spec.EdDSANamedCurveTable;
import net.i2p.crypto.eddsa.spec.EdDSAPublicKeySpec;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.openssl.PEMEncryptedKeyPair;
import org.bouncycastle.openssl.PEMKeyPair;
import org.bouncycastle.openssl.PEMParser;
import org.bouncycastle.openssl.PKCS8Generator;
import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter;
import org.bouncycastle.openssl.jcajce.JcaPEMWriter;
import org.bouncycastle.openssl.jcajce.JcaPKCS8Generator;
import org.bouncycastle.openssl.jcajce.JceOpenSSLPKCS8DecryptorProviderBuilder;
import org.bouncycastle.openssl.jcajce.JceOpenSSLPKCS8EncryptorBuilder;
import org.bouncycastle.openssl.jcajce.JcePEMDecryptorProviderBuilder;
import org.bouncycastle.operator.OperatorCreationException;
import org.bouncycastle.operator.OutputEncryptor;
import org.bouncycastle.pkcs.PKCS8EncryptedPrivateKeyInfo;
import org.bouncycastle.pkcs.PKCSException;
import org.bouncycastle.util.io.pem.PemGenerationException;
import org.bouncycastle.util.io.pem.PemObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.DrbgParameters;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;

import static net.i2p.crypto.eddsa.EdDSAPrivateKey.KEY_ALGORITHM;


public class Ed25519KeyStore extends ArrayList<KeyPair> implements KeyStore {

	private static final String RECOVERY_PHRASE_HASH_MESSAGE = "Recovery Phrase Hash:";
	private static final String APPLICATION_NAME = "Application: %s\n";
	private static final String VERSION = "Version: %s\n";
	private static final String INDEX = "Index: %d\n";
	private static final String HASH = "Recovery Phrase Hash: %s\n";
	private static final int ITERATION_COUNT = 65536;

	private final SecureRandom random;
	private final JcaPEMKeyConverter converter;
	private final char[] password;

	public static final class Builder {

		public Builder withPassword(final char[] password) {
			this.password = password;
			return this;
		}

		public Builder withConverter(final JcaPEMKeyConverter converter) {
			this.converter = converter;
			return this;
		}

		public Builder withSecureRandom(final SecureRandom secureRandom) {
			this.random = secureRandom;
			return this;
		}

		public Ed25519KeyStore build() throws KeyStoreException {
			if (password == null) {
				password = new char[0];
			}
			if (converter == null) {
				this.converter = new JcaPEMKeyConverter().setProvider(ED_PROVIDER);
			}
			if (random == null) {
				try {
					this.random = SecureRandom.getInstance("DRBG",
							DrbgParameters.instantiation(256, DrbgParameters.Capability.RESEED_ONLY, null));
				} catch (NoSuchAlgorithmException ex) {
					throw new KeyStoreException(ex);
				}
			}
			return new Ed25519KeyStore(password, converter, random);
		}

		private SecureRandom random;
		private JcaPEMKeyConverter converter;
		private char[] password;

	}

	private static final Provider BC_PROVIDER = new BouncyCastleProvider();
	private static final Provider ED_PROVIDER = new EdDSASecurityProvider();

	/**
	 * Issue #139, load from a pem file without a password.
	 */
	public static Ed25519KeyStore read(final File source) throws KeyStoreException {
		final var keyStore = new Builder().build();
		keyStore.loadFile(source);
		return keyStore;
	}

	public static Ed25519KeyStore read(final char[] password, final File source) throws KeyStoreException {
		final var keyStore = new Builder().withPassword(password).build();
		keyStore.loadFile(source);
		return keyStore;
	}

	public static Ed25519KeyStore read(final char[] password, final String sourceFile) throws KeyStoreException {
		return read(password, new File(sourceFile));
	}

	public static Ed25519KeyStore read(final char[] password, final URL sourceUrl) throws KeyStoreException {
		try {
			return read(password, new File(sourceUrl.toURI()));
		} catch (URISyntaxException ex) {
			throw new KeyStoreException(ex);
		}
	}

	public static Ed25519KeyStore read(final char[] password, final InputStream source) throws KeyStoreException {
		final var keyStore = new Builder().withPassword(password).build();
		keyStore.loadKeyPairs(source);
		return keyStore;
	}

	public static KeyPair createKeyPair() throws NoSuchAlgorithmException {
		final var generator = KeyPairGenerator.getInstance(KEY_ALGORITHM, ED_PROVIDER);
		return generator.generateKeyPair();
	}

	public KeyPair insertNewKeyPair() throws KeyStoreException {
		try {
			final var kp = createKeyPair();
			this.add(kp);
			return kp;
		} catch (NoSuchAlgorithmException ex) {
			throw new KeyStoreException(ex);
		}
	}

	public KeyPair insertNewKeyPair(Ed25519PrivateKey privateKey) throws KeyStoreException {
		try {
			final var kp = EncryptionUtils.buildKeyPairFromMainNetPrivateKey(privateKey);
			this.add(kp);
			return kp;
		} catch (Exception e) {
			throw new KeyStoreException(e);
		}
	}

	public void write(final String destFile) throws KeyStoreException {
		write(new File(destFile));
	}

	public void write(final String destFile, final String app, long index, String version,
			int hash) throws KeyStoreException {
		try (var fos = new FileOutputStream(destFile)) {
			write(fos, app, index, version, hash);
			SecurityUtilities.ownerReadWritePermissions(destFile);
		} catch (IOException ex) {
			throw new KeyStoreException(ex);
		}
	}

	public void write(final File dest) throws KeyStoreException {
		//make parent directory if it doesn't exist
		if (dest.getParentFile() != null) {
			dest.getParentFile().mkdirs();
		}
		try (var fos = new FileOutputStream(dest)) {
			write(fos);
		} catch (IOException ex) {
			throw new KeyStoreException(ex);
		}
	}

	public void write(final OutputStream ostream) throws KeyStoreException {
		try {
			if (isEmpty()) {
				throw new KeyStoreException(ErrorMessages.EMPTY_KEYSTORE_ERROR_MESSAGE);
			}

			final var encryptor = (new JceOpenSSLPKCS8EncryptorBuilder(PKCS8Generator.AES_256_CBC))
					.setPRF(PKCS8Generator.PRF_HMACSHA384)
					.setIterationCount(ITERATION_COUNT)
					.setRandom(random)
					.setPasssword(password)
					.setProvider(BC_PROVIDER)
					.build();

			try (final var pemWriter = new JcaPEMWriter(new OutputStreamWriter(ostream))) {
				for (var kp : this) {
					pemWriter.writeObject(encodeKeyPair(kp, encryptor));
				}
				pemWriter.flush();
			}
		} catch (IOException | OperatorCreationException ex) {
			throw new KeyStoreException(ex);
		}
	}

	public void write(final OutputStream ostream, final String app, final long index, final String version,
			final int hash) throws KeyStoreException {
		try {
			if (isEmpty()) {
				throw new KeyStoreException(ErrorMessages.EMPTY_KEYSTORE_ERROR_MESSAGE);
			}

			final var encryptor = (new JceOpenSSLPKCS8EncryptorBuilder(PKCS8Generator.AES_256_CBC))
					.setPRF(PKCS8Generator.PRF_HMACSHA384)
					.setIterationCount(ITERATION_COUNT)
					.setRandom(random)
					.setPasssword(password)
					.setProvider(BC_PROVIDER)
					.build();

			try (final var pemWriter = new JcaPEMWriter(new OutputStreamWriter(ostream))) {
				pemWriter.write(String.format(APPLICATION_NAME, app));
				pemWriter.write(String.format(VERSION, version));
				pemWriter.write(String.format(INDEX, index));
				pemWriter.write(String.format(HASH, hash));

				for (var kp : this) {
					pemWriter.writeObject(encodeKeyPair(kp, encryptor));
				}
				pemWriter.flush();
			}
		} catch (IOException | OperatorCreationException ex) {
			throw new KeyStoreException(ex);
		}
	}

	private PemObject encodeKeyPair(KeyPair keyPair, OutputEncryptor encryptor) throws KeyStoreException {
		try {
			return new JcaPKCS8Generator(keyPair.getPrivate(), encryptor).generate();
		} catch (PemGenerationException ex) {
			throw new KeyStoreException(ex);
		}
	}


	private KeyPair decodeKeyPair(Object rawObject) throws KeyStoreException {
		try {
			KeyPair kp;

			if (rawObject instanceof PEMEncryptedKeyPair) {
				final var ekp = (PEMEncryptedKeyPair) rawObject;
				final var decryptor = new JcePEMDecryptorProviderBuilder().setProvider(
						BC_PROVIDER).build(password);
				kp = converter.getKeyPair(ekp.decryptKeyPair(decryptor));
			} else if (rawObject instanceof PKCS8EncryptedPrivateKeyInfo) {
				final var ekpi = (PKCS8EncryptedPrivateKeyInfo) rawObject;
				final var decryptor = new JceOpenSSLPKCS8DecryptorProviderBuilder()
						.setProvider(BC_PROVIDER)
						.build(password);

				final var pki = ekpi.decryptPrivateKeyInfo(decryptor);
				final var sk = (EdDSAPrivateKey) converter.getPrivateKey(pki);
				final var pk = new EdDSAPublicKey(
						new EdDSAPublicKeySpec(sk.getA(), EdDSANamedCurveTable.ED_25519_CURVE_SPEC));
				kp = new KeyPair(pk, sk);
			} else {
				final var ukp = (PEMKeyPair) rawObject;
				kp = converter.getKeyPair(ukp);
			}

			return kp;
		} catch (IOException | OperatorCreationException | PKCSException ex) {
			throw new KeyStoreException(ex);
		}
	}

	private void loadKeyPairs(final InputStream istream) throws KeyStoreException {
		clear();

		try (final var parser = new PEMParser(new InputStreamReader(istream))) {

			Object rawObject;
			while ((rawObject = parser.readObject()) != null) {
				add(decodeKeyPair(rawObject));
			}
		} catch (IOException ex) {
			throw new KeyStoreException(ex);
		}
	}

	private void loadFile(final File source) throws KeyStoreException {
		try (final var fis = new FileInputStream(source)) {
			loadKeyPairs(fis);
		} catch (IOException ex) {
			throw new KeyStoreException(ex);
		}
	}

	/**
	 * Returns the index used to create the keystore. If the file was created without a mnemonic phrase, it returns -1
	 *
	 * @param sourceFile
	 * 		the location of the PEM file
	 * @return a String
	 */
	public static int getIndex(final String sourceFile) throws KeyStoreException {
		try (var reader = new BufferedReader(new FileReader(sourceFile))) {
			var line = reader.readLine();
			while (line != null) {
				if (line.contains("Index:")) {
					var parsedLine = line.split(":");
					return Integer.parseInt(parsedLine[1].replace(" ", ""));
				}
				line = reader.readLine();
			}
		} catch (IOException e) {
			throw new KeyStoreException(e);
		}
		return -1;
	}

	/**
	 * Returns the version of the app used to generate the key. If the key was created with an older version of the app,
	 * it returns an empty string.
	 *
	 * @param sourceFile
	 * 		the location of the PEM file
	 * @return a String
	 */
	public static String getVersion(final String sourceFile) throws KeyStoreException {
		try (var reader = new BufferedReader(new FileReader(sourceFile))) {
			var line = reader.readLine();
			while (line != null) {
				if (line.contains("Version:")) {
					var parsedLine = line.split(":");
					return parsedLine[1].replace(" ", "");
				}
				line = reader.readLine();
			}
		} catch (IOException e) {
			throw new KeyStoreException(e);
		}
		return "";
	}

	/**
	 * Returns the hash code of the mnemonic phrase used to create the keystore. If the hashcode is missing, it returns
	 * null
	 *
	 * @param sourceFile
	 * 		the location of the PEM file
	 * @return a String
	 */
	public static Integer getMnemonicHashCode(final String sourceFile) throws KeyStoreException {

		try (var reader = new BufferedReader(new FileReader(sourceFile))) {
			var line = reader.readLine();
			while (line != null) {
				if (line.contains(RECOVERY_PHRASE_HASH_MESSAGE)) {
					var parsedLine = line.split(":");
					return Integer.parseInt(parsedLine[1].replace(" ", ""));
				}
				line = reader.readLine();
			}
		} catch (IOException e) {
			throw new KeyStoreException(e);
		}
		return null;
	}

	private Ed25519KeyStore(final char[] password, final JcaPEMKeyConverter converter, final SecureRandom random) {
		this.password = password;
		this.converter = converter;
		this.random = random;
	}

	@Override
	public boolean equals(Object o) {
		return super.equals(o);
	}

	@Override
	public int hashCode() {
		return Arrays.hashCode(password) + converter.hashCode() + random.hashCode();
	}
}
