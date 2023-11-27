package com.hedera.hashgraph.client.core.utils;


import com.hedera.hashgraph.client.core.utils.sysfiles.serdes.StandardSerdes;
import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

class SerdeTests {

    @Test
    void testingCompletableFuture() {
//        This works! so i can create the 'task' which is a completablefuture. take an a .whencompelteasync(getreceipt) which will do hte work and check
//                receipt, stoping the loop if necessary
//                then for file id, get the completablefuture out of hte map, thenapplyasync(next task) -> put htat in the map, then that new completablefuture.whencompleteasync(getrecipt)
//            so what object should this 'task' or completablefuture be? i need to make sure I grab the errors and display them, need to also display 'successful/unsuccessful' count
//            and the completablefuture AND the getreceipt stuff need to both be able to stop the loop if necessary (for fileid stuff)
//            and i need to be able to keep track of which failed so I can restart
//            and I need to know when they are all done (should just get all completeables and do some sort of .allcomplete().join, or something?)
//
//        using this approach, can I group all the same fileId sthings going to different nodes, so that it creates each append(#3) and puts them together to form a
//                .anyof() or whatever? so first one to respond successfully wins? if failed response, returns an error.
//
//        I could have 2 different objects, one for doing the submit, one for the receipt.
//        and instead of receipt being in a whencomplete, it should just be in another thenapplyasync. why? because the two objects should return values
//                independent of the submitcommand stuff, then submitcommand can put its own touch on it with the whencomplete which can cancel
//                the loop


        final var exec = Executors.newFixedThreadPool(10);
        System.out.println(Thread.currentThread().getName());
        final var superbaseFuture = CompletableFuture.completedFuture(1);
        final var baseFuture = superbaseFuture.thenApplyAsync(i -> {
            System.out.println(Thread.currentThread().getName());
            return 1;
        }, exec);
        baseFuture.whenCompleteAsync((result, t) -> {
            try {
                Thread.sleep(1000 * 5);
            } catch (Exception ex) {

            }
            System.out.println(Thread.currentThread().getName());
            System.out.println("baseFuture complete = " + result);
        }, exec);
        final var future2 = baseFuture.thenApplyAsync(i -> {
            System.out.println(Thread.currentThread().getName());
            return i+1;
        }, exec);
        future2.whenCompleteAsync((result, t) -> {
            System.out.println(Thread.currentThread().getName());
            System.out.println("future2 complete = " + result);
        }, exec);
        final var future3 = future2.thenApplyAsync(i -> {
            System.out.println(Thread.currentThread().getName());
            return i+1;
        }, exec);
        future3.whenCompleteAsync((result, t) -> {
            System.out.println(Thread.currentThread().getName());
            System.out.println("future3 complete = " + result);
        }, exec);

        final var future4 = baseFuture.thenApplyAsync(i -> {
            System.out.println(Thread.currentThread().getName());
            return i+1;
        }, exec);
        future4.whenCompleteAsync((result, t) -> {
            System.out.println(Thread.currentThread().getName());
            System.out.println("future4 complete = " + result);
        }, exec);
        var acceptedFuture = baseFuture.thenAccept(i -> System.out.println("Accepting value = " + i));
        //i is of type Void, so this doesn't work.
//        acceptedFuture.thenApplyAsync(i -> i+1);
//        this didn't stop future stuff, can i stop it somehow?'
//        cancel apparently does nothing for completableFuture
//        System.out.println("baseFuture is canceled = " + baseFuture.cancel(true));
//        if (!baseFuture.isCancelled()) {
//            baseFuture.whenCompleteAsync((result, t) -> {
//                try {
//                    Thread.sleep(1000 * 5);
//                } catch (Exception ex) {
//
//                }
//                System.out.println(Thread.currentThread().getName());
//                System.out.println("baseFuture after cancel; result = " + result + " error = " + t);
//            }, exec);
//        }
        try {
            Thread.sleep(1000 * 5);
        } catch (Exception ex) {

        }
    }

    @ParameterizedTest
    @MethodSource("getTestFileDerInput")
    void testFileDer(long fileId, String path, String fileName, String baseName) throws IOException {
        final var bytes = Files.readAllBytes(Path.of(path, fileName));
        final var text = StandardSerdes.SYS_FILE_SERDES.get(fileId).fromRawFile(bytes);
        final var base = Files.readString(Path.of(path, baseName));
        Assert.assertEquals(base, text);
    }

    private static Stream<Arguments> getTestFileDerInput() {
        return Stream.of(
                Arguments.of(101L, "src/test/resources/Serde/", "file-101.bin", "file-101.json"),
                Arguments.of(102L, "src/test/resources/Serde/", "file-102.bin", "file-102.json"),
                //FeeSchedules
                Arguments.of(111L, "src/test/resources/Serde/", "file-111.bin", "file-111.json"),
                //ExchangeRates
                Arguments.of(112L, "src/test/resources/Serde/", "file-112.bin", "file-112.json"),
                //Application
                Arguments.of(121L, "src/test/resources/Serde/", "file-121.bin", "file-121.properties"),
                //Api-permission
                Arguments.of(122L, "src/test/resources/Serde/", "file-122.bin", "file-122.properties"),
                //Throttles
                Arguments.of(123L, "src/test/resources/Serde/", "file-123.bin", "file-123.json")
        );
    }
}
