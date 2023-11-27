package com.hedera.hashgraph.client.core.remote;

import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.sdk.Transaction;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class TransactionCreationMetadataFileTest extends TestBase {
    static TransactionCreationMetadataFile emptyTest;
    static TransactionCreationMetadataFile oneNodeTest;
    static TransactionCreationMetadataFile oneAccountTest;
    static TransactionCreationMetadataFile feePayerFalseTest;
    static TransactionCreationMetadataFile feePayerTrueTest;
    static TransactionCreationMetadataFile oneNodeOneAccountTest;
    static TransactionCreationMetadataFile oneNodeOneAccountFeePayerTest;
    static TransactionCreationMetadataFile manyNodesTest;
    static TransactionCreationMetadataFile manyAccountsTest;
    static TransactionCreationMetadataFile fullTest;

    static final String NODE_3 = "0.0.3";
    static final String NODE_LIST_INPUT = "3-5";
    static final Identifier NODE_3_WITH_CHECKSUM =
            new Identifier(0,0,3, "testnet");
    static final Identifier NODE_4_WITH_CHECKSUM =
            new Identifier(0,0,4, "testnet");
    static final Identifier NODE_5_WITH_CHECKSUM =
            new Identifier(0,0,5, "testnet");
    static final String ACCOUNT_6 = "0.0.6";
    static final String ACCOUNT_LIST_INPUT = "6-8";
    static final Identifier ACCOUNT_6_WITH_CHECKSUM =
            new Identifier(0,0,6, "testnet");
    static final Identifier ACCOUNT_7_WITH_CHECKSUM =
            new Identifier(0,0,7, "testnet");
    static final Identifier ACCOUNT_8_WITH_CHECKSUM =
            new Identifier(0,0,8, "testnet");


    @BeforeAll
    static void setUp() {
        emptyTest = new TransactionCreationMetadataFile.Builder().build();
        oneNodeTest = new TransactionCreationMetadataFile.Builder()
                .withNodes(NODE_3, Arrays.asList(NODE_3_WITH_CHECKSUM))
                .build();
        oneAccountTest = new TransactionCreationMetadataFile.Builder()
                .withAccounts(ACCOUNT_6, Arrays.asList(ACCOUNT_6_WITH_CHECKSUM))
                .build();
        feePayerFalseTest = new TransactionCreationMetadataFile.Builder()
                .withIsUpdateAccountFeePayer(false)
                .build();
        feePayerTrueTest = new TransactionCreationMetadataFile.Builder()
                .withIsUpdateAccountFeePayer(true)
                .build();
        oneNodeOneAccountTest = new TransactionCreationMetadataFile.Builder()
                .withNodes(NODE_3, Arrays.asList(NODE_3_WITH_CHECKSUM))
                .withAccounts(ACCOUNT_6, Arrays.asList(ACCOUNT_6_WITH_CHECKSUM))
                .build();
        oneNodeOneAccountFeePayerTest = new TransactionCreationMetadataFile.Builder()
                .withNodes(NODE_3, Arrays.asList(NODE_3_WITH_CHECKSUM))
                .withAccounts(ACCOUNT_6, Arrays.asList(ACCOUNT_6_WITH_CHECKSUM))
                .withIsUpdateAccountFeePayer(true)
                .build();
        manyNodesTest = new TransactionCreationMetadataFile.Builder()
                .withNodes(NODE_LIST_INPUT,
                        Arrays.asList(NODE_3_WITH_CHECKSUM,NODE_4_WITH_CHECKSUM,NODE_5_WITH_CHECKSUM))
                .build();
        manyAccountsTest = new TransactionCreationMetadataFile.Builder()
                .withAccounts(ACCOUNT_LIST_INPUT,
                        Arrays.asList(ACCOUNT_6_WITH_CHECKSUM,ACCOUNT_7_WITH_CHECKSUM,ACCOUNT_8_WITH_CHECKSUM))
                .build();
        fullTest = new TransactionCreationMetadataFile.Builder()
                .withNodes(NODE_LIST_INPUT,
                        Arrays.asList(NODE_3_WITH_CHECKSUM,NODE_4_WITH_CHECKSUM,NODE_5_WITH_CHECKSUM))
                .withAccounts(ACCOUNT_LIST_INPUT,
                        Arrays.asList(ACCOUNT_6_WITH_CHECKSUM,ACCOUNT_7_WITH_CHECKSUM,ACCOUNT_8_WITH_CHECKSUM))
                .withIsUpdateAccountFeePayer(true)
                .build();

    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void getNodes() {
        assertNull(emptyTest);
        assertNull(oneNodeTest);
        assertNull(oneAccountTest);
        assertNull(feePayerFalseTest);
        assertNull(feePayerTrueTest.getNodes());
        assertNull(oneNodeOneAccountTest);
        assertNull(oneNodeOneAccountFeePayerTest.getNodes());
        assertNull(oneNodeOneAccountFeePayerTest.getNodes());
        assertEquals(manyNodesTest.getNodes().getInput(), NODE_LIST_INPUT);
        assertEquals(manyNodesTest.getNodes().getList(),
                Arrays.asList(NODE_3_WITH_CHECKSUM,NODE_4_WITH_CHECKSUM,NODE_5_WITH_CHECKSUM));
        assertNull(manyAccountsTest.getNodes());
        assertEquals(fullTest.getNodes().getInput(), NODE_LIST_INPUT);
        assertEquals(fullTest.getNodes().getList(),
                Arrays.asList(NODE_3_WITH_CHECKSUM,NODE_4_WITH_CHECKSUM,NODE_5_WITH_CHECKSUM));
    }

    @Test
    void getAccounts() {
        assertNull(emptyTest);
        assertNull(oneNodeTest);
        assertNull(oneAccountTest);
        assertNull(feePayerFalseTest);
        assertNull(feePayerTrueTest.getAccounts());
        assertNull(oneNodeOneAccountTest);
        assertNull(oneNodeOneAccountFeePayerTest.getNodes());
        assertNull(oneNodeOneAccountFeePayerTest.getNodes());
        assertEquals(manyNodesTest.getNodes().getInput(), NODE_LIST_INPUT);
        assertEquals(manyNodesTest.getNodes().getList(),
                Arrays.asList(NODE_3_WITH_CHECKSUM,NODE_4_WITH_CHECKSUM,NODE_5_WITH_CHECKSUM));
        assertNull(manyAccountsTest.getNodes());
        assertEquals(fullTest.getNodes().getInput(), NODE_LIST_INPUT);
        assertEquals(fullTest.getNodes().getList(),
                Arrays.asList(NODE_3_WITH_CHECKSUM,NODE_4_WITH_CHECKSUM,NODE_5_WITH_CHECKSUM));
    }

    @Test
    void isUpdateAccountFeePayer() {
    }

    @Test
    void toFile() {
    }

    @Test
    void testToString() {
    }

    @Test
    void testEquals() {
    }
}