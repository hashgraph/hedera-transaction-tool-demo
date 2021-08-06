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
import java.util.Map;
import java.util.Properties;


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
	public UserProperties(String storage, @Nullable String description) {
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
		try (InputStream inputStream = new FileInputStream(storage)) {
			prop.load(inputStream);
		} catch (IOException e) {
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
		try (OutputStream outputStream = new FileOutputStream(storage)) {
			prop.store(outputStream, description);
		} catch (IOException e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/***
	 * Set a string property and store to the local file
	 * @param key String
	 * @param value String
	 */
	public void setProperty(String key, String value) {
		loadProperties();
		prop.setProperty(key, value);
		storeProperties();
	}

	/***
	 * Set a int property and store to the local file
	 * @param key String
	 * @param value int
	 */
	public void setProperty(String key, int value) {
		loadProperties();
		prop.setProperty(key, Integer.toString(value));
		storeProperties();
	}

	/***
	 * Set a long property and store to the local file
	 * @param key String
	 * @param value long
	 */
	public void setProperty(String key, long value) {
		loadProperties();
		prop.setProperty(key, Long.toString(value));
		storeProperties();
	}

	/**
	 * Set an account Identifier property and store to the local file
	 *
	 * @param key
	 * @param account
	 */
	public void setProperty(String key, Identifier account) {
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
	public void setProperty(String key, Map<String, String> map) {
		var serializedMap = serializeMap(map);
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
	public void setProperty(String key, JsonObject jsonObject) {
		var json = jsonObject.toString();
		setProperty(key, json);
	}


	/**
	 * Set a Hbar property and store it to the local file
	 *
	 * @param key
	 * 		String
	 * @param hBars
	 * 		an hbar amount to be stored
	 */
	public void setProperty(String key, Hbar hBars) {
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
	public void setProperty(String key, boolean flag) {
		setProperty(key, String.valueOf(flag));
	}

	/***
	 * Load the properties map and return the property value
	 * @param key String
	 * @param defaultValue String default value returned if the key is not found
	 * @return String
	 */
	public String getProperty(String key, String defaultValue) {
		loadProperties();
		return prop.getProperty(key, defaultValue);
	}

	/**
	 * Load the properties map and return the property value as an Hbar
	 *
	 * @param key
	 * 		String
	 * @param defaultValue
	 * 		hbar default value to be returned if the key is not found
	 * @return Hbar
	 */
	public Hbar getHBarProperty(String key, Hbar defaultValue) {
		var hBarProperty = getProperty(key, defaultValue.toString()).replaceAll("[^\\d.]", "");
		try {
			return Hbar.fromString(hBarProperty);
		} catch (Exception e) {
			throw new HederaClientRuntimeException(e);
		}
	}


	/***
	 * Load the properties map and return the property value as an int
	 * @param key String
	 * @param defaultInt integer default value returned if the key is not found
	 * @return int
	 */
	public int getIntProperty(String key, int defaultInt) {
		var intProp = getProperty(key, String.valueOf(defaultInt));
		try {
			return Integer.parseInt(intProp);
		} catch (NumberFormatException e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/***
	 * Load the properties map and return the property value as an int
	 * @param key String
	 * @param defaultLong long default value returned if the key is not found
	 * @return long
	 */
	public long getLongProperty(String key, long defaultLong) {
		var longProp = getProperty(key, String.valueOf(defaultLong));
		try {
			return Long.parseLong(longProp);
		} catch (NumberFormatException e) {
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
	public Identifier getIdentifierProperty(String key, Identifier defaultIdentifier) {
		var identifierProperty = getProperty(key, defaultIdentifier.toReadableString());
		try {
			return Identifier.parse(identifierProperty);
		} catch (Exception e) {
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
	public JsonObject getJsonProperty(String key, JsonObject defaultJson) {
		var jsonProperty = getProperty(key, defaultJson.toString());
		try {
			var g = new Gson();
			return g.fromJson(jsonProperty, JsonObject.class);
		} catch (Exception e) {
			throw new HederaClientRuntimeException(e);
		}
	}

	/**
	 * Load the properties map and return the property value as a map
	 *
	 * @param key
	 * @param defaultMap
	 * 		Map<String, String>
	 * @return Map<String, String>
	 */
	public Map<String, String> getMapProperty(String key, Map<String, String> defaultMap) {
		var serializedMap = getProperty(key, "");
		if ("".equalsIgnoreCase(serializedMap)) {
			return defaultMap;
		} else {
			return deserializeMap(serializedMap);
		}
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
	public boolean getBooleanProperty(String key, boolean defaultBoolean) {
		var b = getProperty(key, String.valueOf(defaultBoolean));
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
	 * @return
	 */
	public JsonObject propertiesToJson() {
		loadProperties();
		var jsonObject = new JsonObject();
		var keys = prop.keySet();
		for (var key :
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
		Map<String, String> propertiesMap = new HashMap<>();
		for (var key :
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
	public void mapToProperties(Map<String, String> map) {
		cleanProperties();
		for (var entry : map.entrySet()) {
			prop.setProperty(entry.getKey(), entry.getValue());
		}
	}

	/**
	 * Converts a json object into properties
	 *
	 * @param jsonObject
	 * 		JsonObject
	 */
	public void jsonToProperties(JsonObject jsonObject) {
		var serialized = jsonObject.toString();
		var deserialized = deserializeMap(serialized);
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
	private String serializeMap(Map<String, String> map) {
		var mapAsString = "";
		var objectMapper = new ObjectMapper();
		try {
			mapAsString = objectMapper.writeValueAsString(map);
		} catch (JsonProcessingException e) {
			throw new HederaClientRuntimeException(e);
		}
		return mapAsString;
	}

	private Map<String, String> deserializeMap(String serializedMap) {
		var objectMapper = new ObjectMapper();
		Map<String, String> map = new HashMap<>();
		try {
			map = objectMapper.readValue(serializedMap, new TypeReference<>() {
			});
		} catch (Exception e) {
			setProperty("credentials", map);
			logger.error(e);
			logger.error("Credentials cannot be read from the map.");
		}
		return map;
	}

}
