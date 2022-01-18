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


package com.hedera.hashgraph.client.core.props;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.Hbar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nullable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;


public class UserProperties {

	private final Logger logger = LogManager.getLogger(UserProperties.class);
	private Properties prop = new Properties();
	private final String storage;
	private final String description;

	/***
	 * Initialized the user properties class
	 * @param storage location where the properties will be stored
	 * @param description short description of the property file
	 */
	public UserProperties(final String storage, @Nullable final String description) {
		this.storage = storage;
		this.description = description;
		loadProperties();
	}

	/***
	 * Load properties from storage
	 */
	private void loadProperties() {
		if (!new File(storage).exists()) {
			storeProperties();
		}
		try (final InputStream inputStream = new FileInputStream(storage)) {
			prop.load(inputStream);
		} catch (final IOException e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/***
	 * Store the current properties to file
	 */
	private void storeProperties() {
		if (new File(new File(storage).getParent()).mkdirs()) {
			logger.info("Folder structure created");
		}
		try (final OutputStream outputStream = new FileOutputStream(storage)) {
			prop.store(outputStream, description);
		} catch (final IOException e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/***
	 * Set a string property and store to the local file
	 * @param key String
	 * @param value String
	 */
	public void setProperty(final String key, final String value) {
		loadProperties();
		prop.setProperty(key, value);
		storeProperties();
		logger.info("Property {} set to {}", key, value);
	}

	/***
	 * Set an int property and store to the local file
	 * @param key String
	 * @param value int
	 */
	public void setProperty(final String key, final int value) {
		loadProperties();
		prop.setProperty(key, Integer.toString(value));
		storeProperties();
	}

	/***
	 * Set a long property and store to the local file
	 * @param key String
	 * @param value long
	 */
	public void setProperty(final String key, final long value) {
		loadProperties();
		prop.setProperty(key, Long.toString(value));
		storeProperties();
	}

	/**
	 * Set an account Identifier property and store to the local file
	 *
	 * @param key
	 * 		string used as a key
	 * @param account
	 * 		the account
	 */
	public void setProperty(final String key, final Identifier account) {
		loadProperties();
		prop.setProperty(key, account.toReadableString());
		storeProperties();
	}

	/**
	 * Set a map property and store to the local file
	 *
	 * @param key
	 * 		String
	 * @param map
	 * 		Map<String, String>
	 */
	public void setProperty(final String key, final Map<String, String> map) {
		final var serializedMap = serializeMap(map);
		setProperty(key, serializedMap);
	}

	/**
	 * Set a json property and store it to the local file
	 *
	 * @param key
	 * 		String
	 * @param jsonObject
	 * 		a json object to be stored
	 */
	public void setProperty(final String key, final JsonObject jsonObject) {
		final var json = jsonObject.toString();
		setProperty(key, json);
	}


	/**
	 * Set a Hbar property and store it to the local file
	 *
	 * @param key
	 * 		String
	 * @param hBars
	 * 		a hbar amount to be stored
	 */
	public void setProperty(final String key, final Hbar hBars) {
		setProperty(key, hBars.toString());
	}

	/**
	 * Set a boolean property and store it to file
	 *
	 * @param key
	 * 		String
	 * @param flag
	 * 		boolean
	 */
	public void setProperty(final String key, final boolean flag) {
		setProperty(key, String.valueOf(flag));
	}

	/***
	 * Load the properties map and return the property value
	 * @param key String
	 * @param defaultValue String default value returned if the key is not found
	 * @return String
	 */
	public String getProperty(final String key, final String defaultValue) {
		loadProperties();
		return prop.getProperty(key, defaultValue);
	}

	/**
	 * Load the properties map and return the property value as a Hbar
	 *
	 * @param key
	 * 		String
	 * @param defaultValue
	 * 		hbar default value to be returned if the key is not found
	 * @return Hbar
	 */
	public Hbar getHBarProperty(final String key, final Hbar defaultValue) {
		final var hBarProperty = getProperty(key, defaultValue.toString()).replaceAll("[^\\d.]", "");
		try {
			return Hbar.fromString(hBarProperty);
		} catch (final Exception e) {
			throw new HederaClientRuntimeException(e);
		}
	}


	/***
	 * Load the properties map and return the property value as an int
	 * @param key String
	 * @param defaultInt integer default value returned if the key is not found
	 * @return int
	 */
	public int getIntProperty(final String key, final int defaultInt) {
		final var intProp = getProperty(key, String.valueOf(defaultInt));
		try {
			return Integer.parseInt(intProp);
		} catch (final NumberFormatException e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/***
	 * Load the properties map and return the property value as an int
	 * @param key String
	 * @param defaultLong long default value returned if the key is not found
	 * @return long
	 */
	public long getLongProperty(final String key, final long defaultLong) {
		final var longProp = getProperty(key, String.valueOf(defaultLong));
		try {
			return Long.parseLong(longProp);
		} catch (final NumberFormatException e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/**
	 * Load the properties map and return the property value as an identifier
	 *
	 * @param key
	 * 		String
	 * @param defaultIdentifier
	 * 		An identifier value that is returned if the key is not found
	 * @return an Identifier
	 */
	public Identifier getIdentifierProperty(final String key, final Identifier defaultIdentifier) {
		final var identifierProperty = getProperty(key, defaultIdentifier.toReadableString());
		try {
			return Identifier.parse(identifierProperty);
		} catch (final Exception e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/**
	 * Load the properties map and return the property value as an identifier
	 *
	 * @param key
	 * 		String
	 * @param defaultJson
	 * 		A json object that is returned if the key is not found
	 * @return a JsonObject
	 */
	public JsonObject getJsonProperty(final String key, final JsonObject defaultJson) {
		final var jsonProperty = getProperty(key, defaultJson.toString());
		try {
			final var g = new Gson();
			return g.fromJson(jsonProperty, JsonObject.class);
		} catch (final Exception e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/**
	 * Load the properties map and return the property value as a map
	 *
	 * @param key
	 * 		the key used for the property
	 * @param defaultMap
	 * 		Map<String, String>
	 * @return Map<String, String>
	 */
	public Map<String, String> getMapProperty(final String key, final Map<String, String> defaultMap) {
		final var serializedMap = getProperty(key, "");
		if ("".equalsIgnoreCase(serializedMap)) {
			return defaultMap;
		} else {
			return deserializeMap(serializedMap);
		}
	}

	public Set<String> getSetProperty(final String key, final Set<String> defaultSet) {
		final var serializedSet = getProperty(key, "");
		if ("".equalsIgnoreCase(serializedSet)) {
			return defaultSet;
		}
		return deserializeSet(serializedSet);
	}

	public void setSetProperty(final String key, final Set<String> set) {
		setProperty(key, serializeSet(set));
	}

	public boolean removeProperty(final String key) {
		if (prop.contains(key)) {
			prop.remove(key);
			storeProperties();
			return true;
		}
		return false;
	}

	/**
	 * Load the properties map and return the property value as a boolean
	 *
	 * @param key
	 * 		String
	 * @param defaultBoolean
	 * 		boolean
	 * @return boolean
	 */
	public boolean getBooleanProperty(final String key, final boolean defaultBoolean) {
		final var b = getProperty(key, String.valueOf(defaultBoolean));
		return Boolean.parseBoolean(b);
	}

	/**
	 * Removes all key value pairs from the properties
	 */
	public void cleanProperties() {
		prop = new Properties();
		storeProperties();
	}

	/**
	 * Converts the properties into a Json object that can be exported
	 *
	 * @return the properties in json format
	 */
	public JsonObject propertiesToJson() {
		loadProperties();
		final var jsonObject = new JsonObject();
		final var keys = prop.keySet();
		for (final var key :
				keys) {
			jsonObject.addProperty((String) key, prop.getProperty((String) key));
		}
		return jsonObject;
	}

	/**
	 * Converts the properties into a Map
	 *
	 * @return Map<String, String>
	 */
	public Map<String, String> propertiesToMap() {
		loadProperties();
		final Map<String, String> propertiesMap = new HashMap<>();
		for (final var key :
				prop.keySet()) {
			propertiesMap.put((String) key, prop.getProperty((String) key));
		}
		return propertiesMap;
	}

	/**
	 * Converts a map into properties
	 *
	 * @param map
	 * 		Map<String, String>
	 */
	public void mapToProperties(final Map<String, String> map) {
		cleanProperties();
		for (final var entry : map.entrySet()) {
			prop.setProperty(entry.getKey(), entry.getValue());
		}
	}

	/**
	 * Converts a json object into properties
	 *
	 * @param jsonObject
	 * 		JsonObject
	 */
	public void jsonToProperties(final JsonObject jsonObject) {
		final var serialized = jsonObject.toString();
		final var deserialized = deserializeMap(serialized);
		mapToProperties(deserialized);
		storeProperties();
	}

	/**
	 * Serializes a map into a string
	 *
	 * @param map
	 * 		Map<String, String>
	 * @return String
	 */
	private String serializeMap(final Map<String, String> map) {
		var mapAsString = "";
		final var objectMapper = new ObjectMapper();
		try {
			mapAsString = objectMapper.writeValueAsString(map);
		} catch (final JsonProcessingException e) {
			throw new HederaClientRuntimeException(e);
		}
		return mapAsString;
	}

	private Map<String, String> deserializeMap(final String serializedMap) {
		final var objectMapper = new ObjectMapper();
		Map<String, String> map = new HashMap<>();
		try {
			map = objectMapper.readValue(serializedMap, new TypeReference<>() {
			});
		} catch (final Exception e) {
			setProperty("credentials", map);
			logger.error(e);
			logger.error("Credentials cannot be read from the map.");
		}
		return map;
	}

	private String serializeSet(final Set<String> set) {
		return String.join(" ", set);
	}

	private Set<String> deserializeSet(final String serializedSet) {
		return new HashSet<>(List.of(serializedSet.split(" ")));
	}

}
