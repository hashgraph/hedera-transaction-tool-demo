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

import com.hedera.hashgraph.client.cli.options.ToolCommand;
import com.hedera.hashgraph.client.cli.options.ToolOptions;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import picocli.CommandLine;

import java.util.Arrays;

public class ToolsMain {

	private static final Logger logger = LogManager.getLogger(ToolsMain.class);

	public static void main(final String[] args) throws Exception {

		ToolCommand commandCopy = null;
		try {
			final var commandLine = new CommandLine(new ToolOptions());
			final CommandLine.ParseResult parsed;

			try {
				parsed = commandLine.parseArgs(args);
			} catch (CommandLine.MissingParameterException | CommandLine.UnmatchedArgumentException ex) {
				CommandLine.usage(ex.getCommandLine().getCommand(), System.out);
				return;
			}
			verifyOrPrintHelp(args);

			logger.info("Executing command [{}] with options [{}]",
					parsed.asCommandLineList().get(1).getCommandName(),
					String.join(" ", parsed.subcommand().originalArgs()));
			if (CommandLine.printHelpIfRequested(parsed)) {
				return;
			}

			var toolCommand = (ToolCommand) parsed.asCommandLineList().get(1).getCommand();
			commandCopy = toolCommand;
			toolCommand.execute();
		} catch (Exception cause) {
			logger.error(String.format("Error in executing command: %s, message: %s",
					commandCopy != null ? commandCopy.getClass() : null,
					cause.getMessage()));
			throw cause;
		}
	}

	private static void verifyOrPrintHelp(String... args) {
		if (args == null || args.length < 1) {
			final var arguments = Arrays.toString(args);
			logger.info("invalid command argument. {}", arguments);
			CommandLine.usage(new ToolOptions(), System.out);
			System.exit(1);
		}
	}

}

