package com.hedera.hashgraph.client.cli.helpers;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ExecutorService;

/**
 * This Executor warrants task ordering for tasks with same key (key have to implement hashCode and equal methods correctly).
 */
public class OrderedExecutor {

    private final ExecutorService delegate;
    private final Map<Object, Map<Object, Queue<Runnable>>> keyedTasks = new HashMap<>();

    public OrderedExecutor(ExecutorService delegate){
        this.delegate = delegate;
    }

    public void submit(Runnable task) {
        // task without key can be executed immediately
        delegate.submit(task);
    }

    public void submit(Runnable task, Object key, Object group) {
        if (key == null){ // if key is null, execute without ordering
            submit(task);
            return;
        }

        boolean first = false;
        Runnable wrappedTask;
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

        if (first) {
            submit(wrappedTask);
        }
    }

    private Runnable wrap(final Runnable task,
                          final Queue<Runnable> dependencyQueue,
                          final Object key, final Object group) {
        return new OrderedTask(task, dependencyQueue, key, group);
    }

    class OrderedTask implements Runnable{

        private final Queue<Runnable> dependencyQueue;
        private final Runnable task;
        private final Object key;
        private final Object group;

        public OrderedTask(final Runnable task,
                           final Queue<Runnable> dependencyQueue,
                           final Object key, final Object group) {
            this.task = task;
            this.dependencyQueue = dependencyQueue;
            this.key = key;
            this.group = group;
        }

        @Override
        public void run() {
            try{
                task.run();
            } finally {
                Runnable nextTask = null;
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
}