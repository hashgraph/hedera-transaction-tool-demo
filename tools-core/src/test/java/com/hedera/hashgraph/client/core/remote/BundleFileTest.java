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

package com.hedera.hashgraph.client.core.remote;

import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import javafx.scene.control.CheckBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

public class BundleFileTest extends TestBase {
	public static final Logger logger = LogManager.getLogger(BundleFileTest.class);

	@Before
	public void setUp() throws Exception {
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
	}

	@Test
	public void constructor_test() throws IOException {
		var file = new File("src/test/resources/Files/bundleFileTests/mixed.zip");
		var details = FileDetails.parse(file);

		BundleFile bundleFile = new BundleFile(details);
		assertEquals(6, bundleFile.getPublicKeyMap().size());
		assertEquals(5, bundleFile.getAccountInfoMap().size());

		file = new File("src/test/resources/Files/bundleFileTests/infoBundle.zip");
		details = FileDetails.parse(file);

		bundleFile = new BundleFile(details);
		assertEquals(0, bundleFile.getPublicKeyMap().size());
		assertEquals(5, bundleFile.getAccountInfoMap().size());

		file = new File("src/test/resources/Files/bundleFileTests/publicKeyBundle.zip");
		details = FileDetails.parse(file);

		bundleFile = new BundleFile(details);
		assertEquals(6, bundleFile.getPublicKeyMap().size());
		assertEquals(0, bundleFile.getAccountInfoMap().size());

		file = new File("src/test/resources/Files/bundleFileTests/testBundle.zip");
		details = FileDetails.parse(file);

		bundleFile = new BundleFile(details);
		assertEquals(6, bundleFile.getPublicKeyMap().size());
		assertEquals(5, bundleFile.getAccountInfoMap().size());
	}

	@Test
	public void buildGridpane_test() throws IOException {
		var file = new File("src/test/resources/Files/bundleFileTests/mixed.zip");
		var details = FileDetails.parse(file);

		var children = new BundleFile(details).buildGridPane().getChildren();

		assertFalse(children.isEmpty());
		assertEquals(5, children.size());
		assertTrue(children.get(4) instanceof CheckBox);
		assertTrue(((CheckBox) children.get(4)).isSelected());

		file = new File("src/test/resources/Files/bundleFileTests/infoBundle.zip");
		details = FileDetails.parse(file);

		children = new BundleFile(details).buildGridPane().getChildren();
		assertFalse(children.isEmpty());
		assertEquals(3, children.size());
		assertTrue(children.get(2) instanceof CheckBox);
		assertTrue(((CheckBox) children.get(2)).isSelected());

		file = new File("src/test/resources/Files/bundleFileTests/publicKeyBundle.zip");
		details = FileDetails.parse(file);

		children = new BundleFile(details).buildGridPane().getChildren();
		assertFalse(children.isEmpty());
		assertEquals(2, children.size());

		file = new File("src/test/resources/Files/bundleFileTests/testBundle.zip");
		details = FileDetails.parse(file);

		children = new BundleFile(details).buildGridPane().getChildren();
		assertFalse(children.isEmpty());
		assertEquals(5, children.size());
		assertTrue(children.get(4) instanceof CheckBox);
		assertTrue(((CheckBox) children.get(4)).isSelected());
	}

	@Test
	public void equals_test() throws IOException {
		final var mixed = new File("src/test/resources/Files/bundleFileTests/mixed.zip");
		final var mixedDetails = FileDetails.parse(mixed);

		final BundleFile mixedBundle = new BundleFile(mixedDetails);

		final var test = new File("src/test/resources/Files/bundleFileTests/testBundle.zip");
		final var testDetails = FileDetails.parse(test);

		final BundleFile testBundle = new BundleFile(testDetails);

		assertEquals(mixedBundle, testBundle);
		assertEquals(mixedBundle.hashCode(), testBundle.hashCode());

		final var other = new File("src/test/resources/Files/bundleFileTests/publicKeyBundle.zip");
		final var otherDetails = FileDetails.parse(other);
		final BundleFile otherBundle = new BundleFile(otherDetails);
		assertNotEquals(mixedBundle, otherBundle);
		assertNotEquals(mixedBundle.hashCode(), otherBundle.hashCode());

		assertEquals(-1592848019, mixedBundle.hashCode());
		assertEquals(75034152, otherBundle.hashCode());
	}

	@Test
	public void bug221_test() throws IOException, HederaClientException {
		final var infoBug = new File("src/test/resources/Files/bundleFileTests/info.zip");
		final var fileDetails = FileDetails.parse(infoBug);

		final BundleFile bundleFile = new BundleFile(fileDetails);
		assertEquals(9, bundleFile.getInfos().size());

	}
}
