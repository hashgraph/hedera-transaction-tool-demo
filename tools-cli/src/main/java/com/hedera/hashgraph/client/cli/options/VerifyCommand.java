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

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.mirrornode.account.AccountInfoClient;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.client.core.utils.ValidationUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PublicKey;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.zeroturnaround.zip.ZipUtil;
import picocli.CommandLine;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.zip.ZipFile;

import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;

@CommandLine.Command(name = "verify", aliases = { "ver" }, description = "Verify a transaction has the required number of valid signatures.")
public class VerifyCommand implements ToolCommand, GenericFileReadWriteAware {

    private static final Logger logger = LogManager.getLogger(VerifyCommand.class);

    @SuppressWarnings("FieldMayBeFinal")
    @CommandLine.Option(names = {"-n", "--network"}, description = "The Hedera network the transaction will be " +
            "submitted to (one of MAINNET, PREVIEWNET, TESTNET, or the name of the custom network)")
    private String submissionClient = "mainnet";

    @CommandLine.Option(names = {"-t", "--file-with-transaction"}, arity = "1..*", description = "The path(s) to " +
            "the transaction file(s) or directory that contains signed transaction files", required = true)
    private String[] transactionFiles;

    @CommandLine.Option(names = { "-k", "--public-key" }, description = "The path to the public key files that " +
            "correspond with the transaction's required signatures", split = ",")
    private String[] keyFiles = new String[]{};

    private final Map<PublicKey, String> publicKeys = new HashMap<>();

    @Override
    public void execute() throws HederaClientException, IOException {
        // Capture original System.out
        PrintStream originalOut = System.out;
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        PrintStream captureStream = new PrintStream(buffer);
        System.setOut(captureStream);

        try {
            // Load transactions
            final var files = getTransactionPaths();
            if (files.isEmpty()) {
                throw new HederaClientException("No valid transactions found");
            }

            // Parse public key files from inputs
            loadVerificationFiles(Constants.PUB_EXTENSION, keyFiles);

            // Verify transactions
            int passedCount = verifyTransactions(files);

            System.out.println("Verification complete: " + passedCount + " of " + files.size() + " passed");
        } finally {
            // Restore System.out
            System.setOut(originalOut);
            captureStream.flush();
            String captured = buffer.toString();

            // Dump to logger
            logger.info("{}", captured);}
    }

    private Set<String> getTransactionPaths() {
        final Set<String> files = new HashSet<>();
        for (final var fileInput : transactionFiles) {
            // wildcards first
            if (fileInput.contains("*")) {
                handleWildCards(files, fileInput);
                continue;
            }
            final var file = new File(fileInput);
            try {
                files.addAll(buildFileList(file));
            } catch (IOException e) {
                logger.error(e);
            }
        }
        return files;
    }

    private Set<String> buildFileList(File file) throws IOException {
        Set<String> fileList = new HashSet<>();
        Files.walkFileTree(file.toPath(), new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) {
                var f = path.toFile();
                try (var zip = new ZipFile(f)) {
                    final var temp = Files.createTempDirectory("TransactionToolSubmit").toFile();
                    temp.deleteOnExit();
                    ZipUtil.unpack(f, temp);
                    fileList.addAll(buildFileList(temp));
                } catch (IOException e) {
                    if (isTransaction(f) && f.exists()) {
                        fileList.add(f.getAbsolutePath());
                    }
                }
                return FileVisitResult.CONTINUE;
            }
        });
        return fileList;
    }

    private void handleWildCards(final Set<String> files, final String fileInput) {
        final var dir = fileInput.substring(0, fileInput.lastIndexOf("/"));
        final var currentDirectory = new File(dir);
        final var fileList = currentDirectory.list(
                new WildcardFileFilter(fileInput.substring(fileInput.lastIndexOf("/") + 1)));
        if (fileList == null) {
            throw new HederaClientRuntimeException("Invalid file list");
        }
        Arrays.stream(fileList).map(fileName -> new File(dir, fileName)).filter(file -> !file.isHidden()).forEach(
                file -> {
                    try {
                        files.addAll(buildFileList(file));
                    } catch (IOException e) {
                        logger.error(e);
                    }
                });
    }

    private boolean isTransaction(final File file) {
        return SIGNED_TRANSACTION_EXTENSION.equals(FilenameUtils.getExtension(file.getName()));
    }

    /**
     * Loads the files used for verification
     *
     * @param files
     * 		An array of file paths
     * @param extension
     * 		the extension of the files to load.
     * @throws HederaClientException
     * 		if an incorrect extension is found or the app encounters an IOException
     */
    private void loadVerificationFiles(final String extension, final String... files) throws HederaClientException {
        if (files != null && files.length > 0) {
            final var infoArray = Arrays.stream(files)
                    .filter(Objects::nonNull)
                    .map(File::new)
                    .toArray(File[]::new);
            parseFiles(infoArray, extension);
        }
    }

    private int verifyTransactions(final Set<String> files) throws HederaClientException {
        int passedCount = 0;
        for (final var file : files) {
            String filename = getBaseName(file);
            System.out.println("Verify transaction " + filename);
            final var tx = new ToolTransaction().parseFile(new File(file));

            try {
                if (tx == null) {
                    throw new HederaClientRuntimeException("Invalid transaction");
                }
                System.out.print("validating signatures...");
                final var failedPublicKeys = ValidationUtils.validateSignatures(tx.getTransaction());
                if (failedPublicKeys.isEmpty()) {
                    System.out.println("...done");
                } else {
                    System.out.println("...invalid signatures found, they will not be included in verification");
                    for (PublicKey failedPublicKey : failedPublicKeys) {
                        System.out.println("  - " + decodedKey(failedPublicKey));
                    }
                    System.out.println();
                }

                System.out.println("verifying keys...");
                KeyList accountKeyList = new KeyList();
                for (AccountId accountId : tx.getSigningAccountIds()) {
                    Key key = AccountInfoClient.getAccountKey(submissionClient, accountId);
                    accountKeyList.add(key);
                }
                KeyList requiredKeylist = tx.getSigningAccountKeys(accountKeyList);

                if (!verifyKeyList(tx, requiredKeylist, 1)) {
                    System.out.println("Transaction " + filename + " failed verification");
                } else {
                    System.out.println("Transaction " + filename + " passed verification");
                    passedCount++;
                }
                System.out.println();
            } catch (IllegalAccessException e) {
                logger.error("Unable to parse transaction body bytes for transaction ({}): {}", filename, e.getMessage());
            } catch (Exception e) {
                logger.error("An error occurred while verifying transaction ({}): {}", filename, e.getMessage());
            }
        }
        return passedCount;
    }

    private String getBaseName(String path) {
        File file = new File(path);
        String name = file.getName();
        int dotIndex = name.lastIndexOf('.');
        if (dotIndex > 0) {
            return name.substring(0, dotIndex);
        }
        return name;
    }
    /**
     * Verifies the KeyList and prints the results.
     *
     * @param tx          The transaction to verify against.
     * @param keyList     The KeyList to verify.
     * @param indentLevel The current indentation level for printing.
     * @return true if the KeyList is satisfied, false otherwise.
     * @throws IllegalAccessException if there is an issue accessing the transaction body bytes.
     */
    private boolean verifyKeyList(ToolTransaction tx, KeyList keyList, int indentLevel) throws IllegalAccessException {
        String indent = "  ".repeat(indentLevel);
        int threshold = (keyList.getThreshold() != null ? keyList.getThreshold() : keyList.size());

        int satisfied = 0;
        List<String> childOutput = new ArrayList<>();

        for (Key key : keyList) {
            if (key instanceof PublicKey) {
                PublicKey pk = (PublicKey) key;
                // As signatures cannot be removed from a transaction,
                // verify the public key is valid before verifying against required
                boolean found = ValidationUtils.validateSignature(tx.getTransaction(), pk) && tx.verify(pk);
                if (found) satisfied++;
                String line = indent + "  " + (found ? "✔ " : "✘ ") + decodedKey(pk);
                childOutput.add(line);
            } else if (key instanceof KeyList) {
                // Capture nested output in-memory using a separate buffer
                ByteArrayOutputStream buffer = new ByteArrayOutputStream();
                PrintStream originalOut = System.out;
                System.setOut(new PrintStream(buffer));

                boolean subSatisfied = verifyKeyList(tx, (KeyList) key, indentLevel + 1);

                System.setOut(originalOut);
                if (subSatisfied) satisfied++;

                // Capture sub-output and add to childOutput
                childOutput.add(buffer.toString().stripTrailing());
            }
        }

        boolean thisLevelSatisfied = satisfied >= threshold;
        String status = thisLevelSatisfied ? "✔" : "✘";
        System.out.println(indent + status + " threshold: " + threshold + " of " + keyList.size());

        // ✅ Then print child output
        for (String line : childOutput) {
            System.out.println(line);
        }

        return thisLevelSatisfied;
    }

    private String decodedKey(PublicKey pk) {
        if (publicKeys.containsKey(pk)) {
            return publicKeys.get(pk);
        }
        var keyBytes = pk.toBytesRaw();
        return CommonMethods.bytesToHex(keyBytes);
    }

    private void parseFiles(final File[] files, final String extension) throws HederaClientException {
        for (final var file : files) {
            if (!file.exists()) {
                throw new HederaClientException(String.format("The file %s does not exist", file.getName()));
            }
            if (file.isDirectory()) {
                final var inner = file.listFiles((dir, name) -> name.endsWith(extension));
                if (inner == null) {
                    throw new HederaClientRuntimeException("Unable to read files from directory");
                }
                parseFiles(inner, extension);
                return;
            }
            if (Constants.PUB_EXTENSION.equals(extension)) {
                publicKeys.put(EncryptionUtils.publicKeyFromFile(file.getAbsolutePath()),
                        FilenameUtils.getBaseName(file.getName()));
            } else {
                throw new HederaClientException("Not implemented");
            }
        }
    }
}
