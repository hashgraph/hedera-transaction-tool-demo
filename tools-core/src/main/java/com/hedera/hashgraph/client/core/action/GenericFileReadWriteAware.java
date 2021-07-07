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

import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonIOException;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import org.zeroturnaround.zip.ZipUtil;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static com.hedera.hashgraph.client.core.constants.Constants.ZIP_EXTENSION;
import static org.zeroturnaround.zip.ZipUtil.unpack;

public interface GenericFileReadWriteAware {
	/**
	 * Read a file content into a byte array.
	 *
	 * @param filePath path to the file to be read
	 * @return a byte array
	 */
	default byte[] readBytes(String filePath) throws HederaClientException {
		return readBytes(new File(filePath));
	}

	/**
	 * Read a file content into a byte array.
	 *
	 * @param filePath
	 * 		the file containing the array
	 * @return a byte array
	 */
	default byte[] readBytes(File filePath) throws HederaClientException {
		if (filePath == null || !filePath.exists()) {
			throw new HederaClientRuntimeException(
					String.format("Unable to get input stream from empty source: %s.",
							Objects.requireNonNull(filePath).getPath()));
		}
		try (final var fis = new FileInputStream(filePath)) {
			return fis.readAllBytes();
		} catch (IOException cause) {
			throw new HederaClientException(String.format("Unable to read a file: %s.", filePath.getPath()),
					cause);
		}

	}


	/**
	 * Read a file into a json object
	 *
	 * @param filePath The path to the file where the json object is stored
	 * @return a json object
	 */
	default JsonObject readJsonObject(final String filePath) throws HederaClientException {
		if (filePath == null || !(new File(filePath)).exists()) {
			throw new HederaClientRuntimeException(
					String.format("Unable to get input stream from empty source: %s.", filePath));
		}
		// Read file into object
		try {
			final var file = new FileReader(filePath);
			return JsonParser.parseReader(file).getAsJsonObject();
		} catch (JsonIOException | JsonSyntaxException | ClassCastException | FileNotFoundException cause) {
			throw new HederaClientException(String.format("Unable to read Json object from file: %s", filePath), cause);
		}
	}

	/**
	 * Read a file into a json object
	 *
	 * @param file
	 * 		a file containing a json object
	 * @return a json object
	 */
	default JsonObject readJsonObject(final File file) throws HederaClientException {
		if (!file.exists()) {
			throw new HederaClientRuntimeException(
					String.format("Unable to get input stream from empty source: %s.", file.getPath()));
		}
		return readJsonObject(file.getAbsolutePath());
	}


	/**
	 * Read a file into a json array
	 *
	 * @param filePath the path to the file that contains the json array
	 * @return a json array
	 */
	default JsonArray readJsonArray(final String filePath) throws HederaClientException {
		if (filePath == null || !(new File(filePath)).exists()) {
			throw new HederaClientRuntimeException(
					String.format("Unable to get input stream from empty source: %s.", filePath));
		}

		// Read file into object
		try (var file = new FileReader(filePath)) {
			return JsonParser.parseReader(file).getAsJsonArray();
		} catch (JsonIOException | JsonSyntaxException | IOException cause) {
			throw new HederaClientException(String.format("Unable to read Json array from file: %s", filePath), cause);
		}
	}

	/**
	 * Store a json array to a file path
	 *
	 * @param filePath the path to the file where the json array will be stored
	 * @param jsonObject the json object to be stored
	 */
	default void writeJsonObject(final String filePath, final Object jsonObject) throws HederaClientException {
		if (filePath == null) {
			throw new HederaClientRuntimeException(
					String.format("Unable to write input stream to empty destination: %s", filePath));
		}

		if (jsonObject == null) {
			throw new HederaClientRuntimeException(String.format("Unable to write empty json object: %s",
					(Object) null));
		}

		if (!(jsonObject instanceof JsonObject) && !(jsonObject instanceof JsonArray)) {
			throw new HederaClientRuntimeException(
					String.format("Unable to write object of class %s", jsonObject.getClass().toString()));
		}

		try (var file = new FileWriter(filePath)) {
			var gson = new GsonBuilder().setPrettyPrinting().create();
			file.write(gson.toJson(jsonObject));
			file.flush();
		} catch (IOException cause) {
			throw new HederaClientException(String.format("Unable to write Json object to file: %s",
					filePath), cause);
		}
	}

	/**
	 * Write an array of bytes to a file
	 * @param filePath the path to the file where the bytes will be stored
	 * @param contents the byte array
	 */
	default void writeBytes(final String filePath, final byte[] contents) throws HederaClientException {
		if (filePath == null) {
			throw new HederaClientRuntimeException(
					String.format("Unable to write input stream to empty destination: %s", (Object) null));
		}
		if (contents == null) {
			throw new HederaClientRuntimeException(
					String.format("Unable to write empty content object: %s", (Object) null));
		}
		final var file = new File(filePath);
		//make parent directory if it doesn't exist
		if (file.getParentFile() != null && !file.getParentFile().exists()) {
			file.getParentFile().mkdirs();
		}
		try (var fos = new FileOutputStream(filePath)) {
			fos.write(contents);
		} catch (IOException cause) {
			throw new HederaClientException(String.format("Unable to store bytes to location: %s", filePath));
		}
	}


	/**
	 * Unzips a compressed file to a destination
	 *
	 * @param source
	 * 		Path to the file. File does not exist the application throws an exception
	 * @param destination
	 * 		Path to the destination
	 * @return the destination directory
	 */
	default File unZip(String source, String destination) throws HederaClientException {
		var destinationFolder = new File(destination);
		try {
			if (new File(source).exists()) {
				unpack(new File(source), destinationFolder);
			} else {
				throw new HederaClientException(String.format("The file %s does not exist", source));
			}
		} catch (Throwable cause) {
			throw new HederaClientException(
					String.format("Unzip error, %s, with source at location %s", cause.getCause(), source));

		}
		return destinationFolder;
	}

	/**
	 * Compresses all the files in the directory source
	 *
	 * @param source
	 * 		directory where all files to be compressed are located
	 * @return a file pointing to the zip created.
	 */
	default File zipFolder(String source) {
		final var destination = new File(source + "." + ZIP_EXTENSION);
		var toPack = new File(source).listFiles();
		assert toPack != null;
		ZipUtil.packEntries(toPack, destination);

		return destination;
	}


	/**
	 * Create an unique name for a file. If a file with the same name exists, increment a subscript by one until the
	 * name
	 * is unique;
	 *
	 * @param dir
	 * 		the directory where the new file will be created
	 * @param baseName
	 * 		the base name of the new file
	 * @param extension
	 * 		the extension of the new file
	 * @return the path to a new file whose filename starts with <code>baseName</code> and uses extension
	 * 		<code>extension</code>
	 */
	default Path findFileName(final Path dir, final String baseName, final String extension) {
		var ret = Paths.get(String.valueOf(dir), String.format("%s.%s", baseName, extension));
		if (!Files.exists(ret)) {
			return ret;
		}

		for (var i = 0; i < Integer.MAX_VALUE; i++) {
			ret = Paths.get(String.valueOf(dir), String.format("%s_%d.%s", baseName, i, extension));
			if (!Files.exists(ret)) {
				return ret;
			}
		}
		throw new IllegalStateException("Cannot build path");
	}


	default void zipDir(final Path path) throws IOException {
		if (!Files.isDirectory(path)) {
			throw new IllegalArgumentException("Path must be a directory.");
		}

		try (var bos = new BufferedOutputStream(new FileOutputStream(path + "." + ZIP_EXTENSION))) {
			try (var out = new ZipOutputStream(bos)) {
				addZipDir(out, path.getFileName(), path);
			}
		}
	}

	private void addZipDir(final ZipOutputStream out, final Path root, final Path dir) throws IOException {
		try (var stream = Files.newDirectoryStream(dir)) {
			for (var child : stream) {
				var entry = buildPath(root, child.getFileName());
				if (Files.isDirectory(child)) {
					addZipDir(out, entry, child);
				} else {
					out.putNextEntry(new ZipEntry(entry.toString()));
					Files.copy(child, out);
					out.closeEntry();
				}
			}
		}
	}

	private Path buildPath(final Path root, final Path child) {
		if (root == null) {
			return child;
		} else {
			return Paths.get(root.toString(), child.toString());
		}
	}

	/**
	 * Read a csv file into a list of lists
	 *
	 * @param csvFile
	 * 		the location of the csv
	 * @return a List of Lists of Strings
	 */
	default List<List<String>> readCSV(String csvFile) {
		List<List<String>> records = new ArrayList<>();
		try (var br = new BufferedReader(new FileReader(csvFile))) {
			String line;
			while ((line = br.readLine()) != null) {
				var values = line.split(Constants.COMMA_DELIMITER);
				records.add(Arrays.asList(values));
			}
		} catch (IOException e) {
			throw new HederaClientRuntimeException(e);
		}
		return records;
	}

	/**
	 * Writes a map to a csv
	 *
	 * @param filePath
	 * 		the location of the csv
	 * @param listOfStrings
	 * 		a Map<String, List<String>>
	 */
	default void writeCSV(final String filePath,
			final Map<String, List<String>> listOfStrings) throws HederaClientException {
		var eol = System.getProperty("line.separator");
		final var separator = ",";
		try (Writer writer = new FileWriter(filePath)) {
			for (var entry : listOfStrings.entrySet()) {
				writer.append(entry.getKey());
				for (var s : entry.getValue()) {
					writer.append(separator).append(s);
				}
				writer.append(eol);
			}
		} catch (IOException ex) {
			throw new HederaClientException(ex);
		}
	}

}
