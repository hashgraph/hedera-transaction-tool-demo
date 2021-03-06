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


package com.hedera.hashgraph.client.ui.pages;

import com.hedera.hashgraph.client.ui.TestBase;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeItem;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.Window;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.table.TableRowExpanderColumn;
import org.jetbrains.annotations.NotNull;
import org.testfx.api.FxRobot;

import javax.swing.JFileChooser;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_DELETED_ACCOUNTS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_HISTORY;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_KEYS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_LOGS;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_SYSTEM_FOLDER;
import static com.hedera.hashgraph.client.ui.JavaFXIDs.ACCOUNTS_SCROLL_PANE;

@SuppressWarnings("rawtypes")
public class TestUtil {

	private static final FxRobot robot = new FxRobot();
	private static final Logger logger = LogManager.getLogger(TestUtil.class);


	public static PasswordField findPasswordInPopup(final ObservableList<Node> popupNodes) {
		for (final Node popupNode : popupNodes) {
			if (popupNode instanceof PasswordField) {
				return (PasswordField) popupNode;
			}
			if (popupNode instanceof HBox) {
				return findPasswordInPopup(((HBox) popupNode).getChildren());
			}
			if (popupNode instanceof VBox) {
				return findPasswordInPopup(((VBox) popupNode).getChildren());
			}
		}
		return null;
	}

	public static ObservableList<Node> getPopupNodes() {
		final var actualAlertDialog = findModalWindow();
		if (actualAlertDialog != null) {
			final Node dialogPane = actualAlertDialog.getScene().getRoot();
			if (dialogPane != null) {
				if (dialogPane.getClass().isAssignableFrom(VBox.class)) {
					return ((VBox) dialogPane).getChildren();
				}
				if (dialogPane.getClass().isAssignableFrom(HBox.class)) {
					return ((HBox) dialogPane).getChildren();
				}
			}
		}
		return null;
	}

	public static Stage findModalWindow() {
		// Get a list of windows but ordered from top[0] to bottom[n] ones.
		// It is needed to get the first found modal window.
		final List<Window> allWindows = new ArrayList<>(robot.robotContext().getWindowFinder().listWindows());
		Collections.reverse(allWindows);

		return (Stage) allWindows
				.stream()
				.filter(window -> window instanceof Stage)
				.filter(window -> ((Stage) window).getModality() == Modality.APPLICATION_MODAL)
				.findFirst()
				.orElse(null);
	}

	public static String getModalWindowTitle() {
		final List<Window> allWindows = new ArrayList<>(robot.robotContext().getWindowFinder().listWindows());
		Collections.reverse(allWindows);

		final var s = (Stage) allWindows
				.stream()
				.filter(window -> window instanceof Stage)
				.filter(window -> ((Stage) window).getModality() == Modality.APPLICATION_MODAL)
				.findFirst()
				.orElse(null);
		if (s != null) {
			return s.getTitle();
		} else {
			return "";
		}
	}

	public static List<TreeItem> getChildren(final TreeItem treeItem) {
		final List<TreeItem> leaves = new ArrayList<>();

		if (treeItem.getChildren().isEmpty()) {
			leaves.add(treeItem);
		} else {
			final List<TreeItem> children = treeItem.getChildren();
			for (final var child : children) {
				leaves.addAll(getChildren(child));
			}
		}
		return leaves;
	}

	/**
	 * Given a list of nodes that originate in a popup, find the button whose text is equal to the provided legend
	 *
	 * @param popupNodes
	 * 		a list of nodes
	 * @param legend
	 * 		the text in the button
	 * @return a button
	 */
	public static Button findButtonInPopup(final ObservableList<Node> popupNodes, final String legend) {
		if (popupNodes==null) return null;
		for (final var popupNode : popupNodes) {
			if (popupNode instanceof Button && legend.equalsIgnoreCase(((Button) popupNode).getText())) {
				return (Button) popupNode;
			} else if (popupNode instanceof ButtonBar) {
				final var f = findButtonInPopup(((ButtonBar) popupNode).getButtons(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof VBox) {
				final var f = findButtonInPopup(((VBox) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof HBox) {
				final var f = findButtonInPopup(((HBox) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof GridPane) {
				final var f = findButtonInPopup(((GridPane) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			}
		}
		return null;
	}

	/**
	 * Returns a list of textfields contained in the Popup
	 *
	 * @param popupNodes
	 * 		List of nodes in the popup
	 * @return a list of TextFields
	 */
	public static List<TextField> findTextFieldsInPopup(final ObservableList<Node> popupNodes) {
		final List<TextField> textFields = new ArrayList<>();
		for (final var popupNode : popupNodes) {
			if (popupNode instanceof TextField) {
				textFields.add((TextField) popupNode);
			}
			if (popupNode instanceof HBox) {
				textFields.addAll(
						Objects.requireNonNull(findTextFieldsInPopup(((HBox) popupNode).getChildren())));
			}
			if (popupNode instanceof VBox) {
				textFields.addAll(
						Objects.requireNonNull(findTextFieldsInPopup(((VBox) popupNode).getChildren())));
			}
			if (popupNode instanceof GridPane) {
				textFields.addAll(
						Objects.requireNonNull(findTextFieldsInPopup(((GridPane) popupNode).getChildren())));
			}
			if (popupNode instanceof TitledPane) {
				textFields.addAll(Objects.requireNonNull(findTextFieldsInPopup(
						FXCollections.singletonObservableList(((TitledPane) popupNode).getContent()))));
			}
		}
		return textFields;
	}


	/**
	 * Returns a list of Labels contained in the Popup
	 *
	 * @param popupNodes
	 * 		List of nodes in the popup
	 * @return a list of Labels
	 */
	public static List<Label> findLabelsInPopup(final ObservableList<Node> popupNodes) {
		final List<Label> labels = new ArrayList<>();
		for (final var popupNode : popupNodes) {
			if (popupNode instanceof Label) {
				labels.add((Label) popupNode);
			}
			if (popupNode instanceof HBox) {
				labels.addAll(
						Objects.requireNonNull(findLabelsInPopup(((HBox) popupNode).getChildren())));
			}
			if (popupNode instanceof VBox) {
				labels.addAll(
						Objects.requireNonNull(findLabelsInPopup(((VBox) popupNode).getChildren())));
			}
			if (popupNode instanceof GridPane) {
				labels.addAll(
						Objects.requireNonNull(findLabelsInPopup(((GridPane) popupNode).getChildren())));
			}
		}
		return labels;
	}

	/**
	 * Returns a list of CheckBoxes contained in the Popup
	 * @param popupNodes list of nodes in the popup
	 * @return a list of checkboxes
	 */
	public static List<CheckBox> findCheckBoxesInPopup(final ObservableList<Node> popupNodes){
		final List<CheckBox> checkBoxes = new ArrayList<>();
		for (final var popupNode : popupNodes) {
			if (popupNode instanceof CheckBox) {
				checkBoxes.add((CheckBox) popupNode);
			}
			if (popupNode instanceof HBox) {
				checkBoxes.addAll(
						Objects.requireNonNull(findCheckBoxesInPopup(((HBox) popupNode).getChildren())));
			}
			if (popupNode instanceof VBox) {
				checkBoxes.addAll(
						Objects.requireNonNull(findCheckBoxesInPopup(((VBox) popupNode).getChildren())));
			}
			if (popupNode instanceof GridPane) {
				checkBoxes.addAll(
						Objects.requireNonNull(findCheckBoxesInPopup(((GridPane) popupNode).getChildren())));
			}
			if (popupNode instanceof TitledPane) {
				checkBoxes.addAll(Objects.requireNonNull(findCheckBoxesInPopup(
						FXCollections.singletonObservableList(((TitledPane) popupNode).getContent()))));
			}
		}
		return checkBoxes;


	}

	/**
	 * Initialize keys folder with test keys.
	 */
	public static void copyCreatePaneKeys() {
		final var createPaneFolderSuffix = "CreatePaneTest";
		final var testResourceFolder = "/src/test/resources";
		final var createPaneFolder =
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents" + File.separator + createPaneFolderSuffix;
		final var testDirectory = new File(createPaneFolder);
		if (!(testDirectory).exists() && testDirectory.mkdirs()) {
			logger.info("Test folder created");
		}
		final var keysDirectory = new File(testDirectory, "Keys");
		if (!(keysDirectory).exists() && keysDirectory.mkdirs()) {
			logger.info("Keys folder created");
		}

		final var sourceCreatePaneTestDirectory = Paths.get(
				"").toAbsolutePath() + testResourceFolder + File.separator + createPaneFolderSuffix + File.separator +
				"Keys";
		logger.info("Test keys directory : {}", sourceCreatePaneTestDirectory);
	}

	/**
	 * Count the nodes in a tree
	 *
	 * @param node
	 * 		root of the tree
	 * @return the number of nodes.
	 */
	public static int countTreeNodes(final TreeItem<?> node) {
		var count = 1;
		for (final TreeItem t : node.getChildren()) {
			count += countTreeNodes(t);
		}
		return count;
	}


	public static List<Hyperlink> findHyperlinksInPopup() {
		final var popupNodes = getPopupNodes();
		if (popupNodes == null) {
			return new ArrayList<>();
		}
		return findHyperlinksInPopup(popupNodes);
	}

	private static List<Hyperlink> findHyperlinksInPopup(final ObservableList<Node> popupNodes) {
		final List<Hyperlink> nodes = new ArrayList<>();
		for (final Node popupNode : popupNodes) {
			if (popupNode instanceof Hyperlink) {
				nodes.add((Hyperlink) popupNode);
			} else if (popupNode instanceof HBox) {
				nodes.addAll(findHyperlinksInPopup(((HBox) popupNode).getChildren()));
			} else if (popupNode instanceof VBox) {
				nodes.addAll(findHyperlinksInPopup(((VBox) popupNode).getChildren()));
			} else if (popupNode instanceof GridPane) {
				nodes.addAll(findHyperlinksInPopup(((GridPane) popupNode).getChildren()));
			}
		}

		return nodes;
	}

	public static List<GridPane> findGridpanesInPopup() {
		final var popupNodes = getPopupNodes();
		if (popupNodes == null) {
			return new ArrayList<>();
		}
		return findGridPanesInPopup(popupNodes);
	}

	private static List<GridPane> findGridPanesInPopup(final ObservableList<Node> popupNodes) {
		final List<GridPane> nodes = new ArrayList<>();
		for (final Node popupNode : popupNodes) {
			if (popupNode instanceof GridPane) {
				nodes.add((GridPane) popupNode);
			} else if (popupNode instanceof HBox) {
				nodes.addAll(findGridPanesInPopup(((HBox) popupNode).getChildren()));
			} else if (popupNode instanceof VBox) {
				nodes.addAll(findGridPanesInPopup(((VBox) popupNode).getChildren()));
			}
		}

		return nodes;
	}

	public static List<ChoiceBox> findChoiceBoxes(final ObservableList<Node> children) {
		final List<ChoiceBox> nodes = new ArrayList<>();
		children.forEach(popupNode -> {
			if (popupNode instanceof ChoiceBox) {
				nodes.add((ChoiceBox) popupNode);
			}
			if (popupNode instanceof GridPane) {
				nodes.addAll(findChoiceBoxes(((GridPane) popupNode).getChildren()));
			} else if (popupNode instanceof HBox) {
				nodes.addAll(findChoiceBoxes(((HBox) popupNode).getChildren()));
			} else if (popupNode instanceof VBox) {
				nodes.addAll(findChoiceBoxes(((VBox) popupNode).getChildren()));
			}
		});

		return nodes;
	}

	@NotNull
	public static List<ChoiceBox> getChoiceBoxesFromExpander(final TestBase driver) {
		final ScrollPane scrollPane = driver.find(ACCOUNTS_SCROLL_PANE);
		final var tableView = (TableView) scrollPane.getContent();
		final var tableRowExpanderColumn = (TableRowExpanderColumn) tableView.getColumns().get(0);
		final var vBox = (VBox) tableRowExpanderColumn.getExpandedNode(tableView.getItems().get(0));
		return findChoiceBoxes(vBox.getChildren());
	}

	public static void buildFolders() throws IOException {
		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		if (new File(DEFAULT_ACCOUNTS).mkdirs()) {
			logger.info("Accounts folder created");
		}
		if (new File(DEFAULT_DELETED_ACCOUNTS).mkdirs()) {
			logger.info("Deleted accounts folder created");
		}
		if (new File(DEFAULT_SYSTEM_FOLDER).mkdirs()) {
			logger.info("System folder created");
		}
		if (new File(DEFAULT_HISTORY).mkdirs()) {
			logger.info("History folder created");
		}
		if (new File(DEFAULT_KEYS).mkdirs()) {
			logger.info("Keys folder created");
		}
		if (new File(DEFAULT_LOGS).mkdirs()) {
			logger.info("Logs folder created");
		}

	}

	public static List<String> getLabels(final ObservableList<Node> children) {
		final List<String> labels = new ArrayList<>();
		children.forEach(child -> {
			if (child instanceof Label) {
				labels.add(((Label) child).getText().toLowerCase(Locale.ROOT));
			}
			if (child instanceof HBox) {
				labels.addAll(getLabels(((HBox) child).getChildren()));
			}
			if (child instanceof VBox) {
				labels.addAll(getLabels(((VBox) child).getChildren()));
			}
			if (child instanceof GridPane) {
				labels.addAll(getLabels(((GridPane) child).getChildren()));
			}
		});
		return labels;
	}
}
