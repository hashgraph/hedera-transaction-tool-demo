
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

package com.hedera.hashgraph.client.cli.options;


import picocli.CommandLine;
import picocli.CommandLine.Command;

@Command
public class LoggingOptions {

	@CommandLine.Option(names = { "-lfn", "--log-file-name" }, description = "The base name of all log files (default = 'app').")
	private String logFileName;

	public String getLogFileName() {
		return logFileName;
	}
}