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
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.Window;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.testfx.api.FxRobot;

import javax.swing.JFileChooser;
import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public class TestUtil {

	private static final TestBase driver = new TestBase();
	private static final FxRobot robot = new FxRobot();
	private static final Logger logger = LogManager.getLogger(TestUtil.class);

	public static void applyPath(final String filePath) {
		driver.press(KeyCode.COMMAND);
		driver.press(KeyCode.SHIFT);
		driver.press(KeyCode.G);
		driver.release(KeyCode.G);
		driver.release(KeyCode.SHIFT);
		driver.release(KeyCode.COMMAND);

		final var charPath = filePath.toCharArray();
		for (final var c : charPath) {
			pressButton(c);
		}

		driver.press(KeyCode.ENTER);
		driver.release(KeyCode.ENTER);

		driver.push(KeyCode.ENTER);

		WaitForAsyncUtils.waitForFxEvents(1);
	}

	private static void pressButton(final char c) {
		if (Character.isLetter(c)) {
			if (Character.isUpperCase(c)) {
				driver.press(KeyCode.SHIFT);
				driver.press(KeyCode.getKeyCode(String.valueOf(c)));
				driver.release(KeyCode.getKeyCode(String.valueOf(c)));
				driver.release(KeyCode.SHIFT);
			} else {
				driver.press(KeyCode.getKeyCode(String.valueOf(c).toUpperCase()));
				driver.release(KeyCode.getKeyCode(String.valueOf(c).toUpperCase()));
			}
		} else if (Character.isDigit(c)) {
			driver.press(KeyCode.getKeyCode(String.valueOf(c).toUpperCase()));
			driver.release(KeyCode.getKeyCode(String.valueOf(c).toUpperCase()));
		} else if (c == '/') {
			driver.press(KeyCode.SLASH);
			driver.release(KeyCode.SLASH);
		} else if (c == '.') {
			driver.press(KeyCode.PERIOD);
			driver.release(KeyCode.PERIOD);
		} else if (c == '-') {
			driver.press(KeyCode.MINUS);
			driver.release(KeyCode.MINUS);
		} else if (c == '_') {
			driver.press(KeyCode.SHIFT);
			driver.press(KeyCode.MINUS);
			driver.release(KeyCode.MINUS);
			driver.release(KeyCode.SHIFT);
		}

	}

	public static Button findButton(final String buttonName, final String buttonMessage) {
		final Button button = driver.find(buttonName);
		if (button.getText().equals(buttonMessage)) {
			return button;
		}
		return null;
	}

	public static Button findButtonFromPopup(final ObservableList<Node> popupNodes,
			final String buttonName) {
		for (final var node : getPopupNodes()) {
			if (node.getClass().isAssignableFrom(Button.class) || node.toString().equalsIgnoreCase(buttonName)) {
				return (Button) node;
			}
		}
		return null;
	}


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

	public static ObservableList<Node> getPopupNodesReset() {
		final var actualAlertDialog = findModalWindow();
		final var dialogPane = (DialogPane) actualAlertDialog.getScene().getRoot();
		return dialogPane.getChildren();
	}

	public static ObservableList<Node> getPopupNodeCopyKeys() {
		final var actualAlertDialog = findModalWindow();
		final var dialogPane = (HBox) actualAlertDialog.getScene().getRoot();
		final var vbox = (VBox) dialogPane.getChildren().get(0);
		final var children = vbox.getChildren();
		final var label = ((Label) children.get(0)).getText();
		Assert.assertTrue(label.contains("signPaneKey.pub,signPaneKey1.pub"));
		final var hbox = (HBox) children.get(1);
		return ((HBox) hbox.getChildren().get(1)).getChildren();
	}

	public static ObservableList<Node> getSavedTransactionPopupNodes() {
		final var actualAlertDialog = findModalWindow();
		final var dialogPane = (ScrollPane) actualAlertDialog.getScene().getRoot();
		final var vbox = (VBox) dialogPane.getContent();
		return vbox.getChildren();
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

	public static void selectFromComboBox(final String item, final String boxName) {
		driver.clickOn(boxName);
		driver.clickOn(item);

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

	public static <T> long countChildren(final TreeItem<T> treeItem) {
		long count = 0;

		if (treeItem != null) {
			final var children = treeItem.getChildren();

			if (children != null) {
				count += children.size();

				for (final var child : children) {
					count += countChildren(child);
				}
			}
		}

		return count;
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
		}
		return textFields;
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
				"").toAbsolutePath().toString() + testResourceFolder + File.separator + createPaneFolderSuffix + File.separator + "Keys";
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


	/**
	 * Count the leaves in a tree
	 *
	 * @param node
	 * 		root of the tree
	 * @return the number of leaves.
	 */
	public static int countTreeLeaves(final TreeItem<?> node) {
		var count = 1;
		for (final TreeItem t : node.getChildren()) {
			count += (t.isLeaf()) ? 1 : countTreeNodes(t);
		}
		return count;
	}

	public static void applyPath2(final String filePath) {
		final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		final StringSelection stringSelection = new StringSelection(filePath);
		clipboard.setContents(stringSelection, stringSelection);
		driver.press(KeyCode.BACK_SPACE);
		driver.press(KeyCode.COMMAND).press(KeyCode.SHIFT).press(KeyCode.G);
		driver.release(KeyCode.G).release(KeyCode.SHIFT).release(KeyCode.COMMAND);

		driver.press(KeyCode.CONTROL).press(KeyCode.V).release(KeyCode.V).release(KeyCode.CONTROL);
		driver.push(KeyCode.ENTER);
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
}
