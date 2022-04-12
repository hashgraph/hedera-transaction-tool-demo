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

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.Hbar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class UserPropertiesTest {
	private static final Logger logger = LogManager.getLogger(UserPropertiesTest.class);
	UserProperties properties;

	@BeforeEach
	void setUp() {
		if (new File("src/test/resources/test.properties").delete()) {
			logger.info("Test properties deleted");
		}
		properties = new UserProperties("src/test/resources/test.properties", "test file");
	}

	@AfterEach
	void tearDown() {
		new File("src/test/resources/test.properties").deleteOnExit();
	}

	@Test
	void testSetStringProperty() {
		final String testString = "a test string";
		properties.setProperty("testString", testString);
		assertEquals(testString, properties.getProperty("testString", "badString"));
	}

	@Test
	void testSetIntProperty() {
		final int testInt = 1234;
		properties.setProperty("testInt", testInt);
		assertEquals(testInt, properties.getIntProperty("testInt", 0));
	}

	@Test
	void testSetLongProperty() {
		final long testLong = 1234L;
		properties.setProperty("testLong", testLong);
		assertEquals(testLong, properties.getLongProperty("testLong", 0));
	}


	@Test
	void testHBarProperty() {
		final Hbar testAmount = Hbar.fromTinybars(10000);
		properties.setProperty("testCurrency", testAmount);
		assertEquals(testAmount, properties.getHBarProperty("testCurrency", Hbar.ZERO));
	}

	@Test
	void testSetIdentifierProperty() {
		final Identifier testIdentifier = new Identifier(3, 5, 6);
		properties.setProperty("testIdentifier", testIdentifier);
		assertEquals(testIdentifier, properties.getIdentifierProperty("testIdentifier", new Identifier(0, 0, 0)));
	}

	@Test
	void testSetMapProperty() {
		final Map<String, String> testMap = new HashMap<>();
		testMap.put("test1", "test string 1");
		testMap.put("test2", "test string 2");
		testMap.put("test3", "test string 3");
		testMap.put("test4", "test string 4");
		properties.setProperty("testMap", testMap);
		final Map<String, String> recoveredMap = properties.getMapProperty("testMap", new HashMap<>());
		assertEquals("test string 1", recoveredMap.get("test1"));
		assertEquals("test string 2", recoveredMap.get("test2"));
		assertEquals("test string 3", recoveredMap.get("test3"));
		assertEquals("test string 4", recoveredMap.get("test4"));
		assertEquals(4, recoveredMap.size());
	}

	@Test
	void testSetBooleanProperty() {
		properties.setProperty("testBoolean", true);
		assertTrue(properties.getBooleanProperty("testBoolean", false));
	}

	@Test
	void cleanProperties() {
		final String testString = "a test string";
		properties.setProperty("testString", testString);
		final int testInt = 1234;
		properties.setProperty("testInt", testInt);
		final long testLong = 1234L;
		properties.setProperty("testLong", testLong);
		final Identifier testIdentifier = new Identifier(3, 5, 6);
		properties.setProperty("testIdentifier", testIdentifier);
		final Map<String, String> testMap = new HashMap<>();
		testMap.put("test1", "test string 1");
		testMap.put("test2", "test string 2");
		testMap.put("test3", "test string 3");
		testMap.put("test4", "test string 4");
		properties.setProperty("testMap", testMap);
		properties.cleanProperties();
		assertEquals("badString", properties.getProperty("testString", "badString"));
		assertEquals(0, properties.getIntProperty("testInt", 0));
		assertEquals(0L, properties.getLongProperty("testLong", 0));
		assertEquals(new Identifier(0, 0, 0, Identifier.MAINNET_NAME_STRING),
				properties.getIdentifierProperty("testIdentifier", new Identifier(0, 0, 0)));
		assertEquals(new HashMap<>(), properties.getMapProperty("testMap", new HashMap<>()));
	}

	@Test
	void propertiesToJson() {
		final String testString = "a test string";
		properties.setProperty("testString", testString);
		final int testInt = 1234;
		properties.setProperty("testInt", testInt);
		final long testLong = 1234L;
		properties.setProperty("testLong", testLong);
		final Identifier testIdentifier = new Identifier(3, 5, 6);
		properties.setProperty("testIdentifier", testIdentifier);

		final JsonObject jsonProperties = properties.propertiesToJson();
		assertEquals("a test string", jsonProperties.get("testString").getAsString());
		assertEquals(1234, jsonProperties.get("testInt").getAsInt());
		assertEquals(1234L, jsonProperties.get("testLong").getAsLong());
		assertEquals(new Identifier(3, 5, 6).toReadableString(), jsonProperties.get("testIdentifier").getAsString());

		properties.cleanProperties();
		assertEquals("badString", properties.getProperty("testString", "badString"));
		assertEquals(0, properties.getIntProperty("testInt", 0));
		assertEquals(0L, properties.getLongProperty("testLong", 0));
		assertEquals(new Identifier(0, 0, 0, Identifier.MAINNET_NAME_STRING),
				properties.getIdentifierProperty("testIdentifier", new Identifier(0, 0, 0)));
		assertEquals(new HashMap<>(), properties.getMapProperty("testMap", new HashMap<>()));

		properties.jsonToProperties(jsonProperties);
		assertEquals("a test string", properties.getProperty("testString", "badString"));
		assertEquals(1234, properties.getIntProperty("testInt", 0));
		assertEquals(1234L, properties.getLongProperty("testLong", 0));
		assertEquals(new Identifier(3, 5, 6),
				properties.getIdentifierProperty("testIdentifier", new Identifier(0, 0, 0)));
	}

	@Test
	void propertiesToMap() {
		final String testString = "a test string";
		properties.setProperty("testString", testString);
		final int testInt = 1234;
		properties.setProperty("testInt", testInt);
		final long testLong = 1234L;
		properties.setProperty("testLong", testLong);
		final Identifier testIdentifier = new Identifier(3, 5, 6);
		properties.setProperty("testIdentifier", testIdentifier);

		final Map<String, String> mapProperties = properties.propertiesToMap();
		assertEquals("a test string", mapProperties.get("testString"));
		assertEquals("1234", mapProperties.get("testInt"));
		assertEquals("1234", mapProperties.get("testLong"));
		assertEquals(new Identifier(3, 5, 6).toReadableString(), mapProperties.get("testIdentifier"));

		properties.cleanProperties();
		assertEquals("badString", properties.getProperty("testString", "badString"));
		assertEquals(0, properties.getIntProperty("testInt", 0));
		assertEquals(0L, properties.getLongProperty("testLong", 0));
		assertEquals(new Identifier(0, 0, 0, Identifier.MAINNET_NAME_STRING),
				properties.getIdentifierProperty("testIdentifier", new Identifier(0, 0, 0)));
		assertEquals(new HashMap<>(), properties.getMapProperty("testMap", new HashMap<>()));

		properties.mapToProperties(mapProperties);
		assertEquals("a test string", properties.getProperty("testString", "badString"));
		assertEquals(1234, properties.getIntProperty("testInt", 0));
		assertEquals(1234L, properties.getLongProperty("testLong", 0));
		assertEquals(new Identifier(3, 5, 6),
				properties.getIdentifierProperty("testIdentifier", new Identifier(0, 0, 0)));

	}
}