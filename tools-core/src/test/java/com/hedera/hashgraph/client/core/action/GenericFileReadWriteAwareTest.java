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

package com.hedera.hashgraph.client.core.action;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.protobuf.ByteString;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Random;
import java.util.function.BooleanSupplier;

import static java.lang.Boolean.parseBoolean;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Disabled("Temporarily disabling tests")
class GenericFileReadWriteAwareTest implements GenericFileReadWriteAware {

	private final static Logger logger = LogManager.getLogger(GenericFileReadWriteAwareTest.class);
	public static BooleanSupplier isInCircleCi = () ->
			parseBoolean(Optional.ofNullable(System.getenv("IN_CIRCLE_CI")).orElse("false"));
	public static final String RESOURCES_DIRECTORY =
			((isInCircleCi.getAsBoolean()) ? "/repo/tools-core/" : "") + "src/test/resources";

	@BeforeEach
	void setUp() {
		if (new File(RESOURCES_DIRECTORY).mkdirs()) {
			logger.info("Resources file created");
		}
	}

	@AfterEach
	void tearDown() throws IOException {
		if (new File(RESOURCES_DIRECTORY + "aDirectory").exists()) {
			FileUtils.deleteDirectory(new File(RESOURCES_DIRECTORY + "aDirectory"));
		}
	}

	@Test
	void writeBytes_readBytes_test() throws HederaClientException {
		final byte[] contents = new byte[20];
		new Random().nextBytes(contents);

		final ByteString contentsBS = ByteString.copyFrom(contents);
		writeBytes(RESOURCES_DIRECTORY + "aDirectory/testByteString.txt", contentsBS.toByteArray());
		final ByteString recoveredContents =
				ByteString.copyFrom(readBytes(RESOURCES_DIRECTORY + "aDirectory/testByteString.txt"));
		assertEquals(contentsBS, recoveredContents);

		assertTrue(new File(RESOURCES_DIRECTORY + "aDirectory").exists());
		assertTrue(new File(RESOURCES_DIRECTORY + "aDirectory").isDirectory());

		new File(RESOURCES_DIRECTORY + "aDirectory/testByteString.txt").deleteOnExit();

		final Exception exception1 = assertThrows(NullPointerException.class, () -> readBytes((File) null));
		assertNull(exception1.getMessage());
		final var bogusFile = new File(RESOURCES_DIRECTORY + "aDirectory/bogus.txt");
		final Exception exception2 = assertThrows(HederaClientRuntimeException.class, () -> readBytes(bogusFile));
		assertEquals(
				"Hedera Client Runtime: Unable to get input stream from empty source: " +
						"src/test/resourcesaDirectory/bogus.txt.",
				exception2.getMessage());


	}

	@Test
	void writeJsonObject_readJsonObject_test() throws HederaClientException {
		final JsonObject testJson = new JsonObject();
		testJson.addProperty("property1", 0);
		testJson.addProperty("property2", true);
		testJson.addProperty("property3", "a string");
		testJson.addProperty("property4", 'c');

		writeJsonObject(RESOURCES_DIRECTORY + "testJson.json", testJson);
		final JsonObject recoveredJsonFromString = readJsonObject(RESOURCES_DIRECTORY + "testJson.json");

		assertNotNull(recoveredJsonFromString);
		assertEquals(testJson, recoveredJsonFromString);

		final var bogusFile = new File(RESOURCES_DIRECTORY + "bogus.json");
		final Exception exception2 = assertThrows(HederaClientRuntimeException.class,
				() -> readJsonObject(bogusFile));
		assertEquals(
				"Hedera Client Runtime: Unable to get input stream from empty source: " + RESOURCES_DIRECTORY +
						"bogus.json.", exception2.getMessage());

		final JsonObject recoveredJsonFromFile = readJsonObject(new File(RESOURCES_DIRECTORY + "testJson.json"));
		assertNotNull(recoveredJsonFromFile);
		assertEquals(testJson, recoveredJsonFromFile);

		final JsonArray testJsonArray = new JsonArray();
		testJsonArray.add("string 0");
		testJsonArray.add("string 1");
		testJsonArray.add("string 2");
		testJsonArray.add("string 3");

		writeJsonObject(RESOURCES_DIRECTORY + "testJsonArray.json", testJsonArray);
		final JsonArray recoveredArray = readJsonArray(RESOURCES_DIRECTORY + "testJsonArray.json");

		assertNotNull(recoveredArray);
		assertEquals(testJsonArray, recoveredArray);

		final Exception exception3 = assertThrows(HederaClientRuntimeException.class, () -> readJsonArray(null));
		assertEquals("Hedera Client Runtime: Unable to get input stream from empty source: null.",
				exception3.getMessage());

		final Exception exception4 = assertThrows(HederaClientRuntimeException.class,
				() -> readJsonArray(RESOURCES_DIRECTORY + "bogus.json"));
		assertEquals(
				"Hedera Client Runtime: Unable to get input stream from empty source: " + RESOURCES_DIRECTORY +
						"bogus.json.", exception4.getMessage());

		new File(RESOURCES_DIRECTORY + "testJson.json").deleteOnExit();
		new File(RESOURCES_DIRECTORY + "testJsonArray.json").deleteOnExit();
	}

	@Test
	void nonExistentFile() {

		final String expected = "Hedera Client Runtime: Unable to get input stream from empty source: bogusFile.txt.";

		final Exception exception1 = assertThrows(HederaClientRuntimeException.class, () -> readBytes("bogusFile.txt"));
		assertEquals(expected, exception1.getMessage());

		final Exception exception2 =
				assertThrows(HederaClientRuntimeException.class, () -> readJsonObject("bogusFile.txt"));
		assertEquals(expected, exception2.getMessage());

		final Exception exception3 =
				assertThrows(HederaClientRuntimeException.class, () -> readJsonArray("bogusFile.txt"));
		assertEquals(expected, exception3.getMessage());
	}

	@Test
	void exceptionTests() {

		final String expected1 = "Hedera Client Runtime: Unable to write input stream to empty destination.";

		final byte[] contents = new byte[20];
		new Random().nextBytes(contents);

		final Exception exception1 = assertThrows(HederaClientRuntimeException.class, () -> writeBytes(null, contents));
		assertEquals(expected1, exception1.getMessage());

		final JsonObject testJson = new JsonObject();
		testJson.addProperty("property1", 0);
		testJson.addProperty("property2", true);
		testJson.addProperty("property3", "a string");
		testJson.addProperty("property4", 'c');

		final Exception exception2 =
				assertThrows(HederaClientRuntimeException.class, () -> writeJsonObject(null, testJson));
		assertEquals(expected1, exception2.getMessage());

		final String expected3 = "Hedera Client Runtime: Unable to write empty content object.";
		final Exception exception3 = assertThrows(HederaClientRuntimeException.class,
				() -> writeBytes(RESOURCES_DIRECTORY + "emptyBytes.txt", null));
		assertEquals(expected3, exception3.getMessage());

		final String expected4 = "Hedera Client Runtime: Unable to write empty json object: null";
		final Exception exception4 = assertThrows(HederaClientRuntimeException.class,
				() -> writeJsonObject(RESOURCES_DIRECTORY + "emptyJson.json", null));
		assertEquals(expected4, exception4.getMessage());

		final String expected5 = "Hedera Client Runtime: Unable to write object of class class java.lang.Integer";
		final Exception exception5 = assertThrows(HederaClientRuntimeException.class,
				() -> writeJsonObject(RESOURCES_DIRECTORY + "emptyJson.json", 123654));
		assertEquals(expected5, exception5.getMessage());

		final String expected6 =
				String.format("Hedera Client: Unable to store bytes to location: %s", RESOURCES_DIRECTORY);
		final Exception exception6 =
				assertThrows(HederaClientException.class, () -> writeBytes(RESOURCES_DIRECTORY, contents));
		assertEquals(expected6, exception6.getMessage());

		final String expected7 =
				String.format("Hedera Client: Unable to write Json object to file: %s", RESOURCES_DIRECTORY);
		final Exception exception7 =
				assertThrows(HederaClientException.class, () -> writeJsonObject(RESOURCES_DIRECTORY, testJson));
		assertEquals(expected7, exception7.getMessage());

		final String expected8 = String.format("Hedera Client: Unable to read a file: %s.", RESOURCES_DIRECTORY);
		final Exception exception8 = assertThrows(HederaClientException.class, () -> readBytes(RESOURCES_DIRECTORY));
		assertEquals(expected8, exception8.getMessage());
	}

	@Test
	void nonParsableJson_test() throws IOException {
		final FileWriter writer = new FileWriter(RESOURCES_DIRECTORY + "filename.txt");
		writer.write("\"key\": \"value\"}");
		writer.close();

		final String expected1 = String.format("Hedera Client: Unable to read Json object from file: %s",
				RESOURCES_DIRECTORY + "filename.txt");
		final Exception exception1 =
				assertThrows(HederaClientException.class, () -> readJsonObject(RESOURCES_DIRECTORY + "filename.txt"));
		assertEquals(expected1, exception1.getMessage());

		final String expected2 = String.format("Hedera Client: Unable to read Json array from file: %s",
				RESOURCES_DIRECTORY + "filename.txt");
		final Exception exception2 =
				assertThrows(HederaClientException.class, () -> readJsonArray(RESOURCES_DIRECTORY + "filename.txt"));
		assertEquals(expected2, exception2.getMessage());

		new File(RESOURCES_DIRECTORY + "filename.txt").deleteOnExit();
	}


	@Test
	void zipFolder_test() {
		final String source = "src/test/resources/Files/TransactionFileTests";
		final File zip = zipFolder(source);
		assertTrue(zip.exists());
		zip.deleteOnExit();
	}

	@Test
	void zipDir_test() throws IOException {
		final Path source = Path.of("src/test/resources/Files");
		zipDir(source);
		final File zip = new File("src/test/resources/Files.zip");
		assertTrue(zip.exists());
		zip.deleteOnExit();
	}

	@Test
	void findFileName_test() {
		final Path source = Path.of("src/test/resources/Keys");
		var filePath = findFileName(source, "genesis", Constants.PUB_EXTENSION);
		assertFalse(filePath.toFile().exists());
		assertTrue(FilenameUtils.getBaseName(filePath.toString()).startsWith("genesis"));

		filePath = findFileName(source, "generic", Constants.PUB_EXTENSION);
		assertFalse(filePath.toFile().exists());
		assertEquals("generic", FilenameUtils.getBaseName(filePath.toString()));
	}

	@Test
	void writeCSV_test() throws HederaClientException {
		final Map<String, List<String>> testMap = new HashMap<>();
		final List<String> strings = new ArrayList<>();
		strings.add("alpha");
		testMap.put("acct1", new ArrayList<>(strings));
		strings.add("beta");
		testMap.put("acct2", new ArrayList<>(strings));
		strings.add("gamma");
		testMap.put("acct3", new ArrayList<>(strings));
		strings.add("delta");
		testMap.put("acct4", new ArrayList<>(strings));
		strings.add("epsilon");
		testMap.put("acct5", new ArrayList<>(strings));
		final var csvFile = new File("src/test/resources/testCSV.csv");
		writeCSV(csvFile.getPath(), testMap);
		assertTrue(csvFile.exists());

		final var records = readCSV(csvFile.getAbsolutePath());
		assertEquals(5, records.size());
		for (final List<String> record : records) {
			final List<String> list = new LinkedList<>(record);
			final String key = record.get(0);
			list.remove(key);
			final var value = new HashSet<>(list);
			assertTrue(testMap.containsKey(key));
			assertEquals(new HashSet<>(testMap.get(key)), value);
		}
		csvFile.deleteOnExit();
	}

	@Test
	void testZipFiles() throws IOException, HederaClientException {
		// Prepare test files
		final var tempFiles = new File("src/test/resources/temp");
		if (tempFiles.mkdirs()) {
			logger.info("Temp folder created");
		}
		final File[] files = new File("src/test/resources/AccountInfos").listFiles();
		assert files != null;
		for (final File file : files) {
			FileUtils.copyFile(file, new File("src/test/resources/temp", file.getName()));
		}

		final File[] toPack = tempFiles.listFiles();

		final var packed = zipFiles(toPack, "src/test/resources/temp.zip");
		assertTrue(packed.exists());
		unZip("src/test/resources/temp.zip", "src/test/resources/newTemp");
		final var newTemp = new File("src/test/resources/newTemp").listFiles();
		assert newTemp != null;
		assertEquals(files.length, Objects.requireNonNull(newTemp).length);

		final var packed2 = zipFiles(newTemp, "src/test/resources/temp.zip");
		assertTrue(packed2.exists());
		assertEquals("temp.zip", packed2.getName());
		assertTrue(new File("src/test/resources/temp.zip").exists());
		assertTrue(new File("src/test/resources/temp_0.zip").exists());

		Files.deleteIfExists(Path.of("src/test/resources/temp.zip"));
		Files.deleteIfExists(Path.of("src/test/resources/temp_0.zip"));
		FileUtils.deleteDirectory(new File("src/test/resources/temp"));
		FileUtils.deleteDirectory(new File("src/test/resources/newTemp"));
	}
}
