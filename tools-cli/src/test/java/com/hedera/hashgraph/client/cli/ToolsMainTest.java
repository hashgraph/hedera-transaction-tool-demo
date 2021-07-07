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

package com.hedera.hashgraph.client.cli;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.util.Optional;
import java.util.function.BooleanSupplier;

import static java.lang.Boolean.parseBoolean;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolsMainTest {
	private static BooleanSupplier isInCircleCi = () ->
			parseBoolean(Optional.ofNullable(System.getenv("IN_CIRCLE_CI")).orElse("false"));
	private static final String RESOURCES_DIRECTORY =
			((isInCircleCi.getAsBoolean()) ? "/repo/tools-cli/" : "") + "src/test/resources/";


	@Test
	void missingParameter_test() throws Exception {
		final ByteArrayOutputStream myOut = new ByteArrayOutputStream();
		System.setOut(new PrintStream(myOut));
		final String[] args =
				{ "collate", "-a", RESOURCES_DIRECTORY + File.separator + "infos" };
		ToolsMain.main(args);
		final String standardOutput = myOut.toString();
		assertTrue(standardOutput.contains("Usage:"));
	}
}