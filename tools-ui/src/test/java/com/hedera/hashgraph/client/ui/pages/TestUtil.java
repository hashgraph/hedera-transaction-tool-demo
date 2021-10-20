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
import javafx.scene.control.DialogPane;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.Window;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Assert;
import org.testfx.api.FxRobot;
import org.testfx.util.WaitForAsyncUtils;

import javax.swing.JFileChooser;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static org.junit.Assert.assertNotNull;

public class TestUtil {

	private static final TestBase driver = new TestBase();
	private static FxRobot robot = new FxRobot();
	private static final Logger logger = LogManager.getLogger(TestUtil.class);

	public static void applyPath(String filePath) {
		driver.press(KeyCode.COMMAND);
		driver.press(KeyCode.SHIFT);
		driver.press(KeyCode.G);
		driver.release(KeyCode.G);
		driver.release(KeyCode.SHIFT);
		driver.release(KeyCode.COMMAND);

		var charPath = filePath.toCharArray();
		for (var c : charPath) {
			pressButton(c);
		}

		driver.press(KeyCode.ENTER);
		driver.release(KeyCode.ENTER);

		driver.push(KeyCode.ENTER);

		WaitForAsyncUtils.waitForFxEvents(1);
	}

	private static void pressButton(char c) {
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

	public static Button findButton(String buttonName, String buttonMessage) {
		Button button = driver.find(buttonName);
		if (button.getText().equals(buttonMessage)) {
			return button;
		}
		return null;
	}

	public static Button findButtonFromPopup(ObservableList<Node> popupNodes,
			String buttonName) {
		for (var node : getPopupNodes()) {
			if (node.getClass().isAssignableFrom(Button.class) || node.toString().equalsIgnoreCase(buttonName)) {
				return (Button) node;
			}
		}
		return null;
	}


	public static PasswordField findPasswordInPopup(ObservableList<Node> popupNodes) {
		for (Node popupNode : popupNodes) {
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
		var actualAlertDialog = findModalWindow();
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
		var actualAlertDialog = findModalWindow();
		final var dialogPane = (DialogPane) actualAlertDialog.getScene().getRoot();
		return dialogPane.getChildren();
	}

	public static ObservableList<Node> getPopupNodeCopyKeys() {
		var actualAlertDialog = findModalWindow();
		final var dialogPane = (HBox) actualAlertDialog.getScene().getRoot();
		var vbox = (VBox) dialogPane.getChildren().get(0);
		var children = vbox.getChildren();
		var label = ((Label) children.get(0)).getText();
		Assert.assertTrue(label.contains("signPaneKey.pub,signPaneKey1.pub"));
		var hbox = (HBox) children.get(1);
		return ((HBox) hbox.getChildren().get(1)).getChildren();
	}

	public static ObservableList<Node> getSavedTransactionPopupNodes() {
		var actualAlertDialog = findModalWindow();
		final var dialogPane = (ScrollPane) actualAlertDialog.getScene().getRoot();
		var vbox = (VBox) dialogPane.getContent();
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

	public static void selectFromComboBox(String item, String boxName) {
		driver.clickOn(boxName);
		driver.clickOn(item);

	}

	public static String getModalWindowTitle() {
		final List<Window> allWindows = new ArrayList<>(robot.robotContext().getWindowFinder().listWindows());
		Collections.reverse(allWindows);

		var s = (Stage) allWindows
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

	public static <T> long countChildren(TreeItem<T> treeItem) {
		long count = 0;

		if (treeItem != null) {
			var children = treeItem.getChildren();

			if (children != null) {
				count += children.size();

				for (var child : children) {
					count += countChildren(child);
				}
			}
		}

		return count;
	}

	public static List<TreeItem> getChildren(TreeItem treeItem) {
		List<TreeItem> leaves = new ArrayList<>();

		if (treeItem.getChildren().isEmpty()) {
			leaves.add(treeItem);
		} else {
			List<TreeItem> children = treeItem.getChildren();
			for (var child : children) {
				leaves.addAll(getChildren(child));
			}
		}
		return leaves;
	}

	public static String[] testPopupWindow() {
		final var actualAlertDialog = findModalWindow();
		assertNotNull(actualAlertDialog);

		final var dialogPane = (DialogPane) actualAlertDialog.getScene().getRoot();
		return new String[] { dialogPane.getHeaderText(), dialogPane.getContentText() };
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
	public static Button findButtonInPopup(ObservableList<Node> popupNodes, String legend) {
		for (var popupNode : popupNodes) {
			if (popupNode instanceof Button && legend.equalsIgnoreCase(((Button) popupNode).getText())) {
				return (Button) popupNode;
			} else if (popupNode instanceof ButtonBar) {
				var f = findButtonInPopup(((ButtonBar) popupNode).getButtons(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof VBox) {
				var f = findButtonInPopup(((VBox) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof HBox) {
				var f = findButtonInPopup(((HBox) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof GridPane) {
				var f = findButtonInPopup(((GridPane) popupNode).getChildren(), legend);
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
	public static List<TextField> findTextFieldsInPopup(ObservableList<Node> popupNodes) {
		List<TextField> textFields = new ArrayList<>();
		for (var popupNode : popupNodes) {
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
		var createPaneFolder =
				new JFileChooser().getFileSystemView().getDefaultDirectory().toString() + "/Documents" + File.separator + createPaneFolderSuffix;
		var testDirectory = new File(createPaneFolder);
		if (!(testDirectory).exists() && testDirectory.mkdirs()) {
			logger.info("Test folder created");
		}
		var keysDirectory = new File(testDirectory, "Keys");
		if (!(keysDirectory).exists() && keysDirectory.mkdirs()) {
			logger.info("Keys folder created");
		}

		var sourceCreatePaneTestDirectory = Paths.get(
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
	public static int countTreeNodes(TreeItem<?> node) {
		var count = 1;
		for (TreeItem t : node.getChildren()) {
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
	public static int countTreeLeaves(TreeItem<?> node) {
		var count = 1;
		for (TreeItem t : node.getChildren()) {
			count += (t.isLeaf()) ? 1 : countTreeNodes(t);
		}
		return count;
	}

	public static void applyPath2(String filePath) {
		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		StringSelection stringSelection = new StringSelection(filePath);
		clipboard.setContents(stringSelection, stringSelection);
		driver.press(KeyCode.BACK_SPACE);
		driver.press(KeyCode.COMMAND).press(KeyCode.SHIFT).press(KeyCode.G);
		driver.release(KeyCode.G).release(KeyCode.SHIFT).release(KeyCode.COMMAND);

		driver.press(KeyCode.CONTROL).press(KeyCode.V).release(KeyCode.V).release(KeyCode.CONTROL);
		driver.push(KeyCode.ENTER);
	}
}
