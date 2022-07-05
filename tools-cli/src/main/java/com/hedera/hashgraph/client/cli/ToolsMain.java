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

import com.hedera.hashgraph.client.cli.options.LoggingOptions;
import com.hedera.hashgraph.client.cli.options.ToolCommand;
import com.hedera.hashgraph.client.cli.options.ToolOptions;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import picocli.CommandLine;
import picocli.CommandLine.ParameterException;

public class ToolsMain {

	private static Logger _logger = null;

	public static void main(final String[] args) throws Exception {

		ToolCommand commandCopy = null;
		try {
			if (args != null) {
				CommandLine logOptsCmdLine = new CommandLine(LoggingOptions.class);
				logOptsCmdLine.getCommandSpec().parser().collectErrors(true);
				logOptsCmdLine.parseArgs(args);
				LoggingOptions loggingOptions = logOptsCmdLine.getCommand();

				if (loggingOptions.getLogFileName() != null) {
					System.setProperty("toolsCliLogFileName", loggingOptions.getLogFileName());
				}
			} else {
				CommandLine.usage(ToolOptions.class, System.out);
				return;
			}

			CommandLine commandLine = new CommandLine(ToolOptions.class);
			final CommandLine.ParseResult parsed;

			try {
				parsed = commandLine.parseArgs(args);
			} catch (final ParameterException ex) {
				CommandLine.usage(ex.getCommandLine().getCommand(), System.out);
				return;
			}

			var commandList = parsed.asCommandLineList();

			if (commandList.size() != 2) {
				CommandLine.usage(commandLine.getCommand(), System.out);
				return;
			}

			final var commandName = commandList.get(1).getCommandName();
			final var joinedCommand = String.join(" ", parsed.subcommand().originalArgs());
			getLogger().info("Executing command [{}] with options [{}]", commandName, joinedCommand);

			if (CommandLine.printHelpIfRequested(parsed)) {
				return;
			}

			final var toolCommand = (ToolCommand) commandList.get(1).getCommand();
			commandCopy = toolCommand;
			toolCommand.execute();
		} catch (final Exception cause) {
			getLogger().error("Error in executing command: " +
					(commandCopy != null ? commandCopy.getClass() : null), cause);
			throw cause;
		}
	}

	private static Logger getLogger() {
		if (_logger == null) {
			_logger = LogManager.getLogger(ToolsMain.class);
		}
		return _logger;
	}

}

