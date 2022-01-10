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

package com.hedera.hashgraph.client.core.utils;

import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.JFileChooser;
import java.io.File;
import java.util.List;

public class BrowserUtilities {

	private static final Logger logger = LogManager.getLogger(BrowserUtilities.class);

	private BrowserUtilities() {
		throw new IllegalStateException("Utility class");
	}

	/***
	 *
	 * Brings up a file window to allow the user to select a directory (Not used for files)
	 *
	 * @param initialPath    The base folder where the popup will start.
	 * @param localPane        The pane from which the browse window will appear.
	 * @return The selected directory.
	 */
	public static String browseDirectories(final String initialPath, final Pane localPane) {

		final var directoryChooser = new DirectoryChooser();
		if ((initialPath != null && !initialPath.equals("")) && new File(initialPath).exists()) {
			directoryChooser.setInitialDirectory(new File(initialPath));
		}

		final var directory = directoryChooser.showDialog(localPane.getScene().getWindow());

		return (directory != null) ? directory.getPath() : "";
	}

	/***
	 *
	 * Brings up a file window to allow the user to select a file (Not used for directories)
	 *
	 * @param initialPath    The base folder where the popup will start.
	 * @param localPane        The pane from which the browse window will appear.
	 * @return The selected file.
	 */
	public static File browseFiles(String initialPath, final Pane localPane) {

		if (initialPath == null || !(new File(initialPath)).exists()) {
			initialPath =
					new JFileChooser().getFileSystemView().getDefaultDirectory().toString();
		}

		final var fileChooser = new FileChooser();
		fileChooser.setInitialDirectory(new File((initialPath)));

		return fileChooser.showOpenDialog(localPane.getScene().getWindow());
	}

	/***
	 * Open a browser popup to a single file
	 * @param initialPath the path where the browsing starts
	 * @param localPane the pane from which the browser will appear
	 * @param type the type of files to browse
	 * @param ext the allowed extensions
	 * @return a file
	 */
	public static File browseFiles(final String initialPath, final Pane localPane, final String type,
			final String... ext) {
		return browseFiles(initialPath, localPane.getScene(), type, ext);
	}

	/***
	 * Open a browser popup to a single file
	 * @param initialPath the path where the browsing starts
	 * @param scene the scene from which the browser will appear
	 * @param type the type of files to browse
	 * @param ext the allowed extensions
	 * @return a file
	 */
	public static File browseFiles(String initialPath, final Scene scene, final String type, final String... ext) {

		if (initialPath == null || !(new File(initialPath)).exists()) {
			initialPath =
					new JFileChooser().getFileSystemView().getDefaultDirectory().toString();
		}

		final var fileChooser = new FileChooser();


		for (final var e : ext) {
			fileChooser.getExtensionFilters().add(
					new FileChooser.ExtensionFilter(String.format("%s [*.%s]", type, e), String.format("*.%s", e)));
		}

		fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All Files", "*.*"));

		final var initialPathFile = new File(initialPath);
		if (!initialPathFile.exists() && initialPathFile.mkdirs()) {
			logger.info("{} does not exist. Directory has been created", initialPath);
		}

		fileChooser.setInitialDirectory(new File((initialPath)));

		return fileChooser.showOpenDialog(scene.getWindow());
	}

	/***
	 *Open a browser popup to a multiple files
	 * @param initialPath the path where the browsing starts
	 * @param localPane the pane from which the browser will appear
	 * @param type the type of files to browse
	 * @param ext the allowed extensions
	 * @return a list of files
	 */
	public static List<File> browseMultiFiles(String initialPath, final Pane localPane, final String type,
			final String... ext) {
		initialPath = (initialPath != null) ?
				initialPath :
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString();
		final var fileChooser = new FileChooser();
		for (final var e : ext) {
			fileChooser.getExtensionFilters().add(
					new FileChooser.ExtensionFilter(type + "-" + e, String.format("*.%s", e)));
		}
		fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All Files", "*.*"));

		fileChooser.setInitialDirectory(new File(initialPath));
		return fileChooser.showOpenMultipleDialog(localPane.getScene().getWindow());
	}

}
