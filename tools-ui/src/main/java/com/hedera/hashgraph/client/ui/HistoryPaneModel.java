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

package com.hedera.hashgraph.client.ui;

import com.google.gson.JsonArray;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.enums.Actions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.RemoteFile;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.ui.utilities.HistoryData;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.IntStream;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_SYSTEM_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.HISTORY_MAP;
import static com.hedera.hashgraph.client.core.enums.FileType.COMMENT;
import static com.hedera.hashgraph.client.core.enums.FileType.METADATA;
import static com.hedera.hashgraph.client.core.enums.FileType.UNKNOWN;
import static com.hedera.hashgraph.client.core.enums.FileType.getType;
import static javafx.beans.binding.Bindings.createObjectBinding;

public class HistoryPaneModel implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(HistoryPaneModel.class);

	private final Map<Integer, Boolean> historyMap = new HashMap<>();
	private final ObservableList<FileType> typeFilter = FXCollections.observableArrayList();
	private final ObservableList<Actions> actionsFilter = FXCollections.observableArrayList();
	private final ObservableList<HistoryData> tableList = FXCollections.observableArrayList();
	private final FilteredList<HistoryData> filteredList = new FilteredList<>(tableList, p -> true);

	private final ObservableList<Predicate<HistoryData>> filters = FXCollections.observableArrayList();


	private boolean noise = true;
	private LocalDate start = LocalDate.now();
	private LocalDate end = LocalDate.now();
	private Predicate<HistoryData> feePayerPredicate;
	private Predicate<HistoryData> typePredicate;
	private Predicate<HistoryData> actionDatePredicate;
	private Predicate<HistoryData> actionTypePredicate;
	private Predicate<HistoryData> expirationDatePredicate;

	public HistoryPaneModel() {
		tableList.addListener((ListChangeListener<HistoryData>) change -> {
			if (isNoise()) {
				return;
			}
			while (change.next()) {
				if (change.wasRemoved() || change.wasAdded()) {
					storeHistory();
				}
			}
		});
		filteredList.predicateProperty().bind(
				createObjectBinding(() -> filters.stream().reduce(x -> true, Predicate::and), filters));
	}

	public int getRow(final HistoryData historyData) {
		return IntStream.range(0, tableList.size()).filter(
				i -> tableList.get(i).equals(historyData)).findFirst().orElse(-1);
	}

	public void resetFeeFilter() {
		filters.remove(feePayerPredicate);
	}

	/**
	 * Cleans all maps and tables
	 */
	public void cleanHistory() {
		noise = false;
		historyMap.clear();
		tableList.clear();
		filteredList.clear();
	}

	public void loadHistory() {
		cleanHistory();
		try {
			if (!new File(HISTORY_MAP).exists()) {
				logger.info("Map not found. Parsing history folder");
				parseHistoryFolder();
				FXCollections.sort(tableList, Comparator.reverseOrder());
				return;
			}
			final var array = readJsonArray(HISTORY_MAP);
			for (final var element : array) {
				final var jsonObject = element.getAsJsonObject();
				final var message = jsonObject.get("filename").toString();
				logger.info("Loading element {}", message);
				final var historyData = new HistoryData(jsonObject);
				tableList.add(historyData);
				historyMap.put(historyData.getCode(), historyData.isHistory());
			}
			FXCollections.sort(tableList, Comparator.reverseOrder());
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * Counts the number of files with a certain type
	 *
	 * @param type
	 * 		the type needed
	 * @return an integer
	 */
	public int countType(final FileType type) {
		var count = 0;
		for (final var value : tableList) {
			if (type.equals(value.getType())) {
				count++;
			}
		}
		return count;
	}

	/**
	 * Adds the fee filter to the list
	 */
	public void feePayerFilterAccept() {
		filters.add(feePayerPredicate);
	}

	/**
	 * Action when the declined checkbox is selected
	 *
	 * @param observableValue
	 * 		observable
	 * @param aBoolean
	 * 		boolean
	 * @param t1
	 * 		new value
	 */
	public void declinedCheckBoxOnChanged(final ObservableValue<? extends Boolean> observableValue,
			final Boolean aBoolean,
			final Boolean t1) {
		if (Boolean.TRUE.equals(t1)) {
			actionsFilter.add(Actions.DECLINE);
		} else {
			actionsFilter.remove(Actions.DECLINE);
		}
	}


	/**
	 * Action when the accepted checkbox has changed
	 *
	 * @param observableValue
	 * 		observable
	 * @param aBoolean
	 * 		boolean
	 * @param t1
	 * 		new value
	 */
	public void acceptedCheckBoxOnChanged(final ObservableValue<? extends Boolean> observableValue,
			final Boolean aBoolean,
			final Boolean t1) {
		if (Boolean.TRUE.equals(t1)) {
			actionsFilter.add(Actions.ACCEPT);
		} else {
			actionsFilter.remove(Actions.ACCEPT);
		}
	}


	/**
	 * For backwards compatibility: parse the history folder and creates the flat history file
	 */
	private void parseHistoryFolder() {
		final var files = getRemoteFiles();
		final var jsonArray = new JsonArray();
		noise = true;
		for (final var file : files) {
			if (METADATA.equals(file.getType()) || COMMENT.equals(file.getType())) {
				continue;
			}
			logger.info("Parsing file {}", file.getName());
			final var data = new HistoryData(file);
			data.setHistory(true);
			jsonArray.add(data.asJson());
			historyMap.put(data.getCode(), data.isHistory());
			tableList.add(data);
		}
		storeHistory();
		noise = false;
	}

	/**
	 * Stores the map to a file
	 */
	public void storeHistory() {
		if (new File(DEFAULT_SYSTEM_FOLDER).mkdirs()) {
			logger.info("Creating system folder");
		}
		logger.info("Storing map to {}", HISTORY_MAP);
		final var array = new JsonArray();
		for (final var entry : tableList) {
			array.add(entry.asJson());
		}

		try {
			writeJsonObject(HISTORY_MAP, array);
		} catch (final HederaClientException e) {
			logger.error(e.getMessage());
		}
	}

	/**
	 * Only allow files that are allowed by the app
	 *
	 * @param name
	 * 		the name of the file
	 * @return true if the file type is allowed
	 */
	private boolean isAllowedFile(final String name) {
		try {
			return !getType(FilenameUtils.getExtension(name)).equals(UNKNOWN);
		} catch (final HederaClientException e) {
			return false;
		}
	}

	/**
	 * Loads all the remote files from the history folder.
	 *
	 * @return an array of remote files
	 */
	private ArrayList<RemoteFile> getRemoteFiles() {
		final var filenames = new File(DEFAULT_HISTORY).listFiles((dir, name) -> isAllowedFile(name));
		final var remoteFiles = new ArrayList<RemoteFile>();
		for (final var filename : filenames) {
			FileDetails details = null;
			try {
				details = FileDetails.parse(filename);
			} catch (final HederaClientException e) {
				logger.error(e.getMessage());
			}

			try {
				remoteFiles.add(new RemoteFile().getSingleRemoteFile(details));
			} catch (final HederaClientException e) {
				logger.error(e.getMessage());
			}
		}
		return remoteFiles;
	}

	public void addToHistory(final RemoteFile remoteFile) {
		setNoise(false);
		final var addition = new HistoryData(remoteFile);
		tableList.remove(addition);
		addition.setHistory(true);
		historyMap.put(addition.getCode(), true);
		tableList.add(0, addition);
	}

	public void removeFromHistory(final RemoteFile remoteFile) {
		setNoise(false);
		final var remove = new HistoryData(remoteFile);
		tableList.remove(remove);
		historyMap.remove(remoteFile.hashCode());
	}

	/**
	 * Check if the file is in history
	 *
	 * @param code
	 * 		the hashcode of the file
	 * @return If the code is in the map, and it is marked as history, return true
	 */
	public boolean isHistory(final int code) {
		return historyMap.containsKey(code) && (Boolean.TRUE.equals(historyMap.get(code)));
	}


	public Map<Integer, Boolean> getHistoryMap() {
		return historyMap;
	}

	public ObservableList<FileType> getTypeFilter() {
		return typeFilter;
	}

	public ObservableList<Actions> getActionsFilter() {
		return actionsFilter;
	}

	public ObservableList<HistoryData> getTableList() {
		return tableList;
	}

	public FilteredList<HistoryData> getFilteredList() {
		return filteredList;
	}

	public boolean isNoise() {
		return noise;
	}

	public void setNoise(final boolean noise) {
		this.noise = noise;
	}
}
