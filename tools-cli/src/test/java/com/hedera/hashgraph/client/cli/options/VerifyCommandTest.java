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

import static org.mockito.Mockito.*;

import com.hedera.hashgraph.sdk.TransactionId;
import org.apache.commons.lang3.StringUtils;
import org.mockito.MockedStatic;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.mirrornode.account.AccountInfoClient;
import com.hedera.hashgraph.client.utils.TestUtils;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.PrivateKey;
import com.hedera.hashgraph.sdk.PublicKey;
import org.apache.commons.io.FileUtils;
import org.bouncycastle.util.encoders.Hex;
import org.junit.jupiter.api.*;
import picocli.CommandLine;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.*;

class VerifyCommandTest {
    private static final String RESOURCES_DIRECTORY = "src/test/resources/verify_test/";
    private static final String TRANSACTIONS_DIR = RESOURCES_DIRECTORY + "transactions/";
    private static final String VALID_TX = TRANSACTIONS_DIR + "valid_signed_transaction.txsig";
    private static final String PUB_KEY_DIR = RESOURCES_DIRECTORY + "pubkeys/";

    private static final Map<PublicKey, PrivateKey> keyMap = new HashMap<>();
    private static Key testKey;

    @BeforeAll
    static void setupAll() throws IOException {
        new File(TRANSACTIONS_DIR).mkdirs();

        var currentKey = AccountInfoClient.getAccountKey("mainnet", AccountId.fromString("0.0.2"));
        testKey = TestUtils.copyKeyStructureAndCollectPrivateKeys(currentKey, keyMap);
        keyMap.keySet().stream().forEach(key -> {
            try {
                String hexPubKey = Hex.toHexString(key.toBytes());
                FileUtils.writeStringToFile(new File(PUB_KEY_DIR + key.toString() + ".pub"), hexPubKey, "UTF-8");
            } catch (IOException e) {
                throw new RuntimeException("Failed to save public key to file", e);
            }
        });
    }

    @BeforeEach
    void setup() throws IOException {
        // Ensure the test directory is clean before each test
        FileUtils.cleanDirectory(new File(TRANSACTIONS_DIR));
        // Copy a valid transaction file to the test directory, renaming to be a .txsig file
        FileUtils.copyFile(new File(TestUtils.NODE_CREATE_TX), new File(VALID_TX));
    }

    @AfterAll
    static void cleanupAll() throws IOException {
        FileUtils.deleteDirectory(new File(RESOURCES_DIRECTORY));
    }

    /**
     * Tests the command will process a transaction (.txsig) file.
     */
    @Test
    void testFoundTransaction() {
        String[] args = { "-t", VALID_TX };
        VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
        assertDoesNotThrow(cmd::execute);
    }

    /**
     * Tests the command will throw an exception if the transaction file is not found.
     */
    @Test
    void testMissingTransactionFile() {
        String[] args = { "-t", "nonexistent.txsig" };
        VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
        assertThrows(HederaClientException.class, cmd::execute);
    }

    /**
     * Tests the command will process properly if the public key file is found.
     */
    @Test
    void testFoundKeyFile() {
        String publicKeyFile = PUB_KEY_DIR + keyMap.keySet().iterator().next().toString() + ".pub";
        String[] args = { "-t", VALID_TX, "-k", publicKeyFile };
        VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
        assertDoesNotThrow(cmd::execute);
    }

    /**
     * Tests the command will throw an exception if the public key file is not found.
     */
    @Test
    void testMissingKeyFile() {
        String[] args = { "-t", VALID_TX, "-k", "nonexistent.pub" };
        VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
        assertThrows(HederaClientException.class, cmd::execute);
    }

    /**
     * Tests the command will accept a directory as input for transactions and public keys.
     */
    @Test
    void testDirectoryInputs() {
        String[] args = { "-t", TRANSACTIONS_DIR, "-k", PUB_KEY_DIR };
        VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
        assertDoesNotThrow(cmd::execute);
    }

    /**
     * Tests the command will accept wild cards for the transaction name.
     */
    @Test
    void testWildcardInput() {
        String[] args = { "-t", TRANSACTIONS_DIR + "*.txsig", "-k", PUB_KEY_DIR };
        VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
        assertDoesNotThrow(cmd::execute);
    }

    /**
     * Tests the command will show a valid transaction if the signatures collated pass the minimum thresholds required.
     */
    @Test
    void testMinimumThreshold() {
        PrintStream originalOut = System.out;
        String output = StringUtils.EMPTY;
        try (MockedStatic<AccountInfoClient> mocked = mockStatic(AccountInfoClient.class)) {
            mocked.when(() -> AccountInfoClient.getAccountKey(anyString(), any(AccountId.class)))
                    .thenReturn(testKey);
            final var tx = TestUtils.loadTransactionFromFile(VALID_TX);
            final var keys = TestUtils.getMinimumKeysRequired(testKey, keyMap);

            TestUtils.signTxFileWithKeysAndSave(tx, keys, VALID_TX);

            ByteArrayOutputStream outContent = new ByteArrayOutputStream();
            System.setOut(new PrintStream(outContent));

            String[] args = { "-t", TRANSACTIONS_DIR + "*.txsig", "-k", PUB_KEY_DIR };
            VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
            assertDoesNotThrow(cmd::execute);

            output = outContent.toString();
            assertEquals(keys.size(), countSymbols(output, "✔(?! threshold:)"));
            assertEquals(keyMap.size() - keys.size(), countSymbols(output, "✘(?! threshold:)"));
            assertTrue(output.contains("validating signatures......done"));
            assertTrue(output.contains("passed verification"));
        } catch (Exception e) {
            fail("Test failed with exception: " + e.getMessage());
        } finally {
            System.setOut(originalOut); // Restore original System.out
            System.out.print(output);
        }
    }

    /**
     * Tests the command will show a valid transaction if the signatures collated pass
     * more than just the minimum thresholds required.
     */
    @Test
    void testMoreThanThreshold() {
        PrintStream originalOut = System.out;
        String output = StringUtils.EMPTY;
        try (MockedStatic<AccountInfoClient> mocked = mockStatic(AccountInfoClient.class)) {
            mocked.when(() -> AccountInfoClient.getAccountKey(anyString(), any(AccountId.class)))
                    .thenReturn(testKey);
            final var tx = TestUtils.loadTransactionFromFile(VALID_TX);
            final var keys = TestUtils.getMinimumKeysRequired(testKey, keyMap);
            var count = 0;
            // Add 10 more keys to the list to ensure we exceed the minimum threshold
            for (PrivateKey pk : keyMap.values()) {
                if (count >= 10) break;
                if (!keys.contains(pk)) {
                    keys.add(pk);
                    count++;
                }
            }

            TestUtils.signTxFileWithKeysAndSave(tx, keys, VALID_TX);

            ByteArrayOutputStream outContent = new ByteArrayOutputStream();
            System.setOut(new PrintStream(outContent));

            String[] args = { "-t", TRANSACTIONS_DIR + "*.txsig", "-k", PUB_KEY_DIR };
            VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
            assertDoesNotThrow(cmd::execute);

            output = outContent.toString();
            assertEquals(keys.size(), countSymbols(output, "✔(?! threshold:)"));
            assertEquals(keyMap.size() - keys.size(), countSymbols(output, "✘(?! threshold:)"));
            assertTrue(output.contains("validating signatures......done"));
            assertTrue(output.contains("passed verification"));
        } catch (Exception e) {
            fail("Test failed with exception: " + e.getMessage());
        } finally {
            System.setOut(originalOut); // Restore original System.out
            System.out.print(output);
        }
    }

    /**
     * Tests the command will show a failed transaction if the signatures collated are
     * less than the minimum thresholds required.
     */
    @Test
    void testLessThanThreshold() {
        PrintStream originalOut = System.out;
        String output = StringUtils.EMPTY;
        try (MockedStatic<AccountInfoClient> mocked = mockStatic(AccountInfoClient.class)) {
            mocked.when(() -> AccountInfoClient.getAccountKey(anyString(), any(AccountId.class)))
                    .thenReturn(testKey);
            final var tx = TestUtils.loadTransactionFromFile(VALID_TX);
            final var keys = TestUtils.getMinimumKeysRequired(testKey, keyMap);
            // Remove 5 keys from the list to ensure we do not meet the minimum threshold
            for (int i = 0; i <= 5; i++) {
                keys.remove(keys.size() - 1);
            }

            TestUtils.signTxFileWithKeysAndSave(tx, keys, VALID_TX);

            ByteArrayOutputStream outContent = new ByteArrayOutputStream();
            System.setOut(new PrintStream(outContent));

            String[] args = { "-t", TRANSACTIONS_DIR + "*.txsig", "-k", PUB_KEY_DIR };
            VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
            assertDoesNotThrow(cmd::execute);

            output = outContent.toString();
            assertEquals(keys.size(), countSymbols(output, "✔(?! threshold:)"));
            assertEquals(keyMap.size() - keys.size(), countSymbols(output, "✘(?! threshold:)"));
            assertTrue(output.contains("validating signatures......done"));
            assertTrue(output.contains("failed verification"));
        } catch (Exception e) {
            fail("Test failed with exception: " + e.getMessage());
        } finally {
            System.setOut(originalOut); // Restore original System.out
            System.out.print(output);
        }
    }

    @Test
    void testInvalidSignature() {
        PrintStream originalOut = System.out;
        String output = StringUtils.EMPTY;
        try (MockedStatic<AccountInfoClient> mocked = mockStatic(AccountInfoClient.class)) {
            mocked.when(() -> AccountInfoClient.getAccountKey(anyString(), any(AccountId.class)))
                    .thenReturn(testKey);
            final var keys = TestUtils.getMinimumKeysRequired(testKey, keyMap);
            var tx = TestUtils.loadTransactionFromFile(VALID_TX);
            tx.setTransactionId(TransactionId.generate(AccountId.fromString("0.0.123")));

            var signatures = TestUtils.createSignaturesWithKeys(tx, keys);
            // Now reload the transaction, and add signatures to it
            tx = TestUtils.loadTransactionFromFile(VALID_TX);
            TestUtils.addSignaturesToTransactionAndSave(tx, signatures, VALID_TX);

            ByteArrayOutputStream outContent = new ByteArrayOutputStream();
            System.setOut(new PrintStream(outContent));

            String[] args = { "-t", TRANSACTIONS_DIR + "*.txsig", "-k", PUB_KEY_DIR };
            VerifyCommand cmd = CommandLine.populateCommand(new VerifyCommand(), args);
            assertDoesNotThrow(cmd::execute);

            output = outContent.toString();
            // None of the signatures are counted, as they are all invalid
            assertEquals(0, countSymbols(output, "✔(?! threshold:)"));
            assertEquals(keyMap.size(), countSymbols(output, "✘(?! threshold:)"));
            assertTrue(output.contains("validating signatures......invalid signatures found, they will not be included in verification"));
            assertEquals(keys.size(), countSymbols(output, "- 302a300506032b6570032100"));
            assertTrue(output.contains("failed verification"));
        } catch (Exception e) {
            fail("Test failed with exception: " + e.getMessage());
        } finally {
            System.setOut(originalOut); // Restore original System.out
            System.out.print(output);
        }
    }

    int countSymbols(String str, String symbol) {
        Pattern pattern = Pattern.compile(symbol);
        Matcher matcher = pattern.matcher(str);
        int count = 0;
        while (matcher.find()) {
            count++;
        }
        return count;
    }
}