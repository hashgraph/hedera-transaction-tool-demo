package com.hedera.hashgraph.client.cli.options;

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.security.Ed25519PrivateKey;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Client;
import com.hedera.hashgraph.sdk.FileAppendTransaction;
import com.hedera.hashgraph.sdk.FileContentsQuery;
import com.hedera.hashgraph.sdk.FileId;
import com.hedera.hashgraph.sdk.PrecheckStatusException;
import com.hedera.hashgraph.sdk.PrivateKey;
import io.github.cdimascio.dotenv.Dotenv;
import org.apache.commons.io.FileUtils;
import org.bouncycastle.util.encoders.Hex;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.nio.channels.Channels;
import java.security.KeyStoreException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import static com.hedera.hashgraph.client.core.constants.Constants.CUSTOM_NETWORK_FOLDER;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;

class GetFileUpdateTest {

    private Client client;

    @BeforeEach
    public void setUp() throws Exception {
        setupClient();
    }

    private void setupClient() throws KeyStoreException {
        final var dotenv = Dotenv.configure().directory("../").ignoreIfMissing().load();
        final var myAccountId = AccountId.fromString(dotenv.get("CUSTOM_ACCOUNT_ID"));
        final var privateKey = dotenv.get("CUSTOM_PRIVATE_KEY");
        final var myPrivateKey = PrivateKey.fromString(privateKey);
        final var keyStore = new Ed25519KeyStore.Builder()
                .withPassword(Constants.TEST_PASSWORD.toCharArray()).build();
        keyStore.insertNewKeyPair(Ed25519PrivateKey.fromBytes(Hex.decode(privateKey.startsWith("0x") ?
                privateKey.substring(2) : privateKey)));

//        client = CommonMethods.getClient(NetworkEnum.TESTNET);
        client = CommonMethods.getClient("localnet");
        client.setOperator(myAccountId, myPrivateKey);
    }

    @ParameterizedTest
    @MethodSource("getTestFileUpdateInput")
    void testFileUpdate(long fileId, String output) throws PrecheckStatusException, TimeoutException {
        var fileID = new FileId(fileId);

        var fileInfoOriginal = new FileContentsQuery()
                .setFileId(fileID)
                .execute(client);
        try {
            var c = Channels.newChannel(new FileOutputStream(output));
            c.write(fileInfoOriginal.asReadOnlyByteBuffer());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static Stream<Arguments> getTestFileUpdateInput() {
        return Stream.of(
                Arguments.of(111, "/Users/johnbair/Downloads/file-111-4.bin")
        );
    }
}
