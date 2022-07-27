package com.hedera.hashgraph.client.core.updater;

import java.io.File;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javafx.application.Platform;

/**
 * Service that auto downloads updates from github.
 */
public class GithubUpdater {

	private static final Logger logger = LogManager.getLogger(GithubUpdater.class);

    private final ExecutorService executor = Executors.newCachedThreadPool(new ThreadFactory() {
        private final AtomicLong counter = new AtomicLong(1);
        private final ThreadFactory tf = Executors.defaultThreadFactory();
        @Override
        public Thread newThread(@NotNull Runnable r) {
            Thread t = tf.newThread(r);
            t.setDaemon(true);
            t.setName("GithubUpdaterProcessorThread_" + counter.getAndIncrement());
            return t;
        }
    });

    private final Object sync = new Object();
    private final Object wait = new Object();

    private AtomicBoolean keepRunning = new AtomicBoolean(true);
    private boolean isRunning = false;
    private boolean isShutdown = false;
    private boolean hasStarted = false;

    private AtomicReference<Instant> nextRun = new AtomicReference<>(Instant.MAX);

    private final String curVersion;
    private final File outputDir;
    private final Runnable refreshHomePage;

    public GithubUpdater(final String curVersion, final File outputDir, final Runnable refreshHomePage) {
        this.curVersion = curVersion;
        this.outputDir = outputDir;
        this.refreshHomePage = refreshHomePage;
    }

    public void start(boolean runNow) {
        if (isShutdown || isRunning) {
            return;
        }
        isRunning = true;

        if (runNow || (!hasStarted)) {
            nextRun.set(Instant.now().plus(2, ChronoUnit.SECONDS));
        }
        hasStarted = true;

        executor.submit(new GithubUpdaterProcessorCaller(keepRunning));
    }

    public void stop() {
        keepRunning.set(false);
        keepRunning = new AtomicBoolean(true);
        isRunning = false;

        // Try to boot waiting threads out, not guaranteed to work all the time, but that's fine. Worse case, a thread waits
        // for a long time and then quits without doing anything.
        synchronized (wait) {
            wait.notifyAll();
        }
    }

    public void shutdown() {
        isShutdown = true;
        stop();
        executor.shutdown();
    }

    private class GithubUpdaterProcessorCaller implements Callable<Void> {

        private final AtomicBoolean myKeepRunning;

        private GithubUpdaterProcessorCaller(final AtomicBoolean keepRunning) {
            this.myKeepRunning = keepRunning;
        }


        @Override
        public Void call() {
            try {
                logger.debug("start {}", Thread.currentThread().getName());

                var curNextRun = nextRun.get();

                var waitStart = Instant.now();

                while (true) {
                    var delay = Duration.between(Instant.now(), curNextRun).toMillis();
                    var waitedTime = Duration.between(waitStart, Instant.now()).toMillis();

                    if ((delay < 2000) && (waitedTime < 2000)) {
                        logger.debug("forcing delay to 2000, delay={}, waitedTime={} - {}", delay, waitedTime,
                                Thread.currentThread().getName());
                        delay = 2000;
                    }

                    if (!myKeepRunning.get()) {
                        logger.debug("abort 1 {}", Thread.currentThread().getName());
                        return null;
                    } else if (delay > 0) {
                        logger.debug("sleep {} {}", delay, Thread.currentThread().getName());
                        synchronized (wait) {
                            wait.wait(delay);
                        }
                    } else {
                        break;
                    }
                }

                synchronized (sync) {
                    if (!myKeepRunning.get()) {
                        logger.debug("abort 2 {}", Thread.currentThread().getName());
                        return null;
                    }

                    logger.debug("run {}", Thread.currentThread().getName());

                    var processor = new GithubUpdateProcessor(myKeepRunning, curVersion, outputDir);

                    processor.doCheck();

                    if (processor.wasOutputModified) {
                        Platform.runLater(refreshHomePage);
                    }

                    if (myKeepRunning.get()) {
                        nextRun.compareAndSet(curNextRun, Instant.now().plus(30, ChronoUnit.MINUTES));
                        executor.submit(new GithubUpdaterProcessorCaller(myKeepRunning));
                    }
                }

                return null;
            } catch (Throwable e) {
                logger.error("GithubUpdaterProcessorCaller failed", e);
                return null;
            }
        }
    }
}
