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

package com.hedera.hashgraph.client.ui.popups;

import com.hedera.hashgraph.client.ui.AccountsPaneController;
import com.hedera.hashgraph.client.ui.utilities.ProgressTask;
import javafx.application.Platform;
import javafx.beans.binding.DoubleBinding;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;

public class ProgressPopup extends Stage {
	private static final Logger LOG = LogManager.getLogger(ProgressPopup.class);
	private static final Object LOCK = new Object();
	private final ProgressBar bar;
	private final Button cancelButton;

	private ProgressPopup(final ProgressBar progressBar, final Button cancelButton) {
		this.bar = progressBar;
		this.cancelButton = cancelButton;
	}

	private static ProgressPopup createProgressPopup(
			final ProgressBar bar, final Button cancelButton, final String title, final String message,
			final long size) {
		final var window = new ProgressPopup(bar, cancelButton);
		final var layout = new VBox();
		layout.setAlignment(Pos.CENTER);
		layout.setSpacing(10);
		layout.setPadding(new Insets(20, 20, 20, 20));
		layout.setMaxWidth(400);

		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle(title);
		window.sizeToScene();
		window.setWidth(450);

		final var titleLabel = new Label();
		titleLabel.setText(title);
		titleLabel.setStyle("-fx-font-size: 20");

		final var messageLabel = new Label(message);
		messageLabel.setWrapText(true);
		messageLabel.setStyle("-fx-font-size: 16");

		cancelButton.setStyle(
				"-fx-background-color: white; -fx-border-color: #0b9dfd; -fx-text-fill: #0b9dfd; " +
						"-fx-border-radius: 10; -fx-background-radius: 10;");
		cancelButton.setMinWidth(200);

		// Cancel Button is currently disabled due to issues with the popup not closing properly at times.
		cancelButton.setDisable(true);
		cancelButton.setTooltip(new Tooltip("Cannot be canceled until currently running tasks are complete"));

		bar.setPrefWidth(375);

		final var text = new Text();
		text.textProperty().bind(bar.progressProperty().map(progress -> {
			final var v = (progress.doubleValue() < 0 ? 0 : Math.round(progress.doubleValue() * size)) + 1;
			return String.format("Submitting %d of %d", v, size);
		}));

		layout.getChildren().addAll(titleLabel, messageLabel, bar, text, cancelButton);

		final var scene = new Scene(layout);

		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		return window;
	}

	public static ProgressPopup setupProgressPopup(
			final ProgressBar bar, final Button cancelButton, final String title, final String message,
			final long size) {
		final var window = createProgressPopup(bar, cancelButton, title, message, size);
		window.show();
		return window;
	}

	public static Object showProgressPopup(final List<ProgressTask> tasks, final String title,
										  final String message) {
		final var progressBar = new ProgressBar();
		final var cancelButton = new Button(AccountsPaneController.CANCEL_LABEL);
		final var taskCount = tasks.size();
		final var window = createProgressPopup(progressBar, cancelButton, title, message, taskCount);
		window.setupTasks(tasks);
		cancelButton.setOnAction(event -> {
//			window.cancelPopup(tasks);
		});
		window.setOnCloseRequest(event -> {
			event.consume();
//			window.cancelPopup(tasks);
		});
		window.show();
		return Platform.enterNestedEventLoop(LOCK);
	}

	private void cancelPopup(final List<ProgressTask> tasks) {
		boolean canClose = true;
		for (var task : tasks) {
			// If the task is running, or can't cancel, then
			// the task will exit the loop when done processing.
			if (task.isRunning() || !task.cancel(false)) {
				canClose = false;
			}
		}
		if (canClose) {
			close();
			Platform.exitNestedEventLoop(LOCK, null);
		} else {
			cancelButton.setDisable(true);
			cancelButton.setTooltip(new Tooltip("Cannot be canceled until currently running tasks are complete"));
		}
	}

	private void setupTasks(final List<ProgressTask> tasks) {
		final var taskCount = tasks.size();

		final DoubleBinding progressBinding = new DoubleBinding() {
			@Override
			protected double computeValue() {
				return 0;
			}
		};

		TaskStatusListener.reset();

		for (final var task : tasks) {
			task.stateProperty().addListener(new TaskStatusListener(task, this, taskCount));
			progressBinding.add(task.progressProperty().divide(taskCount*2));
			new Thread(task).start();
		}

		bar.progressProperty().bind(progressBinding);
	}

	private static class TaskStatusListener implements ChangeListener<Worker.State> {
		private static int doneCount = 0;
		private static List<Object> results = new ArrayList<>();
		private ProgressTask task;
		private ProgressPopup window;
		private int taskCount;

		public TaskStatusListener(final ProgressTask task, final ProgressPopup window, final int taskCount) {
			this.task = task;
			this.window = window;
			this.taskCount = taskCount;
		}
		@Override
		public void changed(ObservableValue<? extends Worker.State> observableValue,
							Worker.State state, Worker.State t1) {
			try {
				switch (t1) {
					case CANCELLED, FAILED, SUCCEEDED:
						incrementDoneCount();
						addResult(task.get());
				}
			} catch (InterruptedException ex) {
				LOG.error(ex.getMessage());
				task.setResult(ex);
				addResult(task);
				Thread.currentThread().interrupt();
			} catch (ExecutionException ex) {
				LOG.error(ex.getMessage());
				task.setResult(ex);
				addResult(task);
			}

			if (doneCount == taskCount) {
				window.close();
				Platform.exitNestedEventLoop(LOCK, results);
			}
		}

		private static synchronized void incrementDoneCount() {
			doneCount++;
		}

		private static synchronized void addResult(final Object result) {
			results.add(result);
		}

		private static synchronized void reset() {
			doneCount = 0;
			results = new ArrayList<>();
		}
	}
}
