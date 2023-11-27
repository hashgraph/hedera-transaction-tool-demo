package com.hedera.hashgraph.client.cli.helpers;

import com.hedera.hashgraph.client.core.helpers.TransactionCallableWorker;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.FutureTask;

/**
 * An Executor that can be used to keep groups of tasks in order, running each task in a group
 * sequentially.
 */
public class OrderedExecutor {

    private final ExecutorService delegate;
    private final Map<Object, Map<Object, Queue<Callable>>> keyedTasks = new HashMap<>();

    public OrderedExecutor(ExecutorService delegate){
        this.delegate = delegate;
    }

    public void submit(Callable task) {
        if (delegate.isShutdown()) return;
        // task without key can be executed immediately
        delegate.submit(task);
    }

    public void submit(Callable task, Object key, Object group) {
        if (delegate.isShutdown()) return;
        if (key == null){ // if key is null, execute without ordering
            submit(task);
            return;
        }

        boolean first = false;
        Callable wrappedTask;
        synchronized (keyedTasks){
            var groupedTasks = keyedTasks.get(key);
            if (groupedTasks == null) {
                groupedTasks = new HashMap<>();
                keyedTasks.put(key, groupedTasks);
            }
            var dependencyQueue = groupedTasks.get(group);
            if (dependencyQueue == null) {
                dependencyQueue = new LinkedList<>();
                groupedTasks.put(group, dependencyQueue);
                first = true;
            }
            wrappedTask = wrap(task, dependencyQueue, key, group);
            dependencyQueue.add(wrappedTask);
        }

        // If this is the current top-of-the-list task, submit it immediately
        if (first) {
            submit(wrappedTask);
        }
    }

    public void shutdown() {
        delegate.shutdown();
    }

    public List<Runnable> shutdownNow() {
        return delegate.shutdownNow();
    }

    private Callable wrap(final Callable task,
                          final Queue<Callable> dependencyQueue,
                          final Object key, final Object group) {
        return new OrderedTask(task, dependencyQueue, key, group);
    }

    class OrderedTask<T> implements Callable<T> {

        private final Queue<Callable> dependencyQueue;
        private final Callable<T> task;
        private final Object key;
        private final Object group;

        public OrderedTask(final Callable<T> task,
                           final Queue<Callable> dependencyQueue,
                           final Object key, final Object group) {
            this.task = task;
            this.dependencyQueue = dependencyQueue;
            this.key = key;
            this.group = group;
        }

        @Override
        public T call() {
            try{
                var result = task.call();

                // Less than ideal, deal with it later
                if (result instanceof FutureTask) {
                    final var completableFuture =
                            ((FutureTask<CompletableFuture<TransactionCallableWorker.TxnResult>>) task).get();
                    completableFuture.whenComplete((txnResult, t) -> {
                        if (t != null || !txnResult.wasSuccessful()) {
                            shutdownNow();
                        }
                        else {
//                            find out what number this was and save it for the retry, if needed
//                                    also, shutdownnow might stop a successful thing, so should just shutdown, and get the last successful one?
//                                    but is that right?
                        }
                    });
                }
                return result;
            } catch (Exception ex) {
                shutdownNow();
                return null;
            } finally {
                callNext();
            }
        }

        private void callNext() {
            Callable nextTask = null;
            synchronized (keyedTasks) {
                dependencyQueue.remove(this);
                if (dependencyQueue.isEmpty()) {
                    final var groupedTasks = keyedTasks.get(group);
                    groupedTasks.remove(group);
                    if (groupedTasks.isEmpty()) {
                        keyedTasks.remove(key);
                    }
                } else {
                    nextTask = dependencyQueue.peek();
                }
            }
            if (nextTask != null) {
                submit(nextTask);
            }
        }
    }
}