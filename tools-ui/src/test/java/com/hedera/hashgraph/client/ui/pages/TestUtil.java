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

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
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
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeItem;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.Window;
import javax.annotation.Nullable;
import javax.swing.JFileChooser;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.testfx.api.FxRobot;

/**
 * Utility class used to recursively build lists or find controllers.
 */
@SuppressWarnings("rawtypes")
public class TestUtil {

	/**
	 * Recursively search and return the first {@link PasswordField} located in the {@link ObservableList} of {@link Node} objects.
	 *
	 * @param popupNodes
	 * 		an ObservableList of Nodes
	 * @return a PasswordField
	 */
	@Nullable
	public static PasswordField findPasswordInPopup(@NotNull final ObservableList<Node> popupNodes) {
		for (final Node popupNode : popupNodes) {
			if (popupNode instanceof PasswordField field) {
				return field;
			} else if (popupNode instanceof HBox hBox) {
				return findPasswordInPopup(hBox.getChildren());
			} else if (popupNode instanceof VBox vBox) {
				return findPasswordInPopup(vBox.getChildren());
			}
		}
		return null;
	}

	/**
	 * Given a {@link ObservableList} of {@link Node} objects that originate in a popup,
	 * find the {@link Button} whose text is equal to the provided legend.
	 *
	 * @param popupNodes
	 * 		a list of nodes
	 * @param legend
	 * 		the text in the button
	 * @return a button
	 */
	@Nullable
	public static Button findButtonInPopup(@NotNull final ObservableList<Node> popupNodes, final String legend) {
		for (final var popupNode : popupNodes) {
			if (popupNode instanceof Button button && legend.equalsIgnoreCase(button.getText())) {
				return button;
			} else if (popupNode instanceof ButtonBar buttonBar) {
				final var f = findButtonInPopup(buttonBar.getButtons(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof VBox vBox) {
				final var f = findButtonInPopup(vBox.getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof HBox hBox) {
				final var f = findButtonInPopup(hBox.getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof GridPane gridPane) {
				final var f = findButtonInPopup(gridPane.getChildren(), legend);
				if (f != null) {
					return f;
				}
			}
		}
		return null;
	}

	/**
	 * Recursively build a {@link List} of {@link TextField} controllers contained in the {@link ObservableList} of {@link Node} objects.
	 *
	 * @param popupNodes
	 * 		List of nodes in the popup
	 * @return a list of TextFields
	 */
	@NotNull
	public static List<TextField> findTextFieldsInPopup(@NotNull final ObservableList<Node> popupNodes) {
		final List<TextField> textFields = new ArrayList<>();
		popupNodes.forEach(popupNode -> {
			if (popupNode instanceof TextField textField) {
				textFields.add(textField);
			} else if (popupNode instanceof HBox hBox) {
				textFields.addAll(findTextFieldsInPopup(hBox.getChildren()));
			} else if (popupNode instanceof VBox vBox) {
				textFields.addAll(findTextFieldsInPopup(vBox.getChildren()));
			} else if (popupNode instanceof GridPane gridPane) {
				textFields.addAll(findTextFieldsInPopup(gridPane.getChildren()));
			} else if (popupNode instanceof TitledPane titledPane) {
				textFields.addAll(findTextFieldsInPopup(FXCollections.singletonObservableList(titledPane.getContent())));
			}
		});
		return textFields;
	}

	/**
	 * Recursively build a {@link List} of {@link Label} controllers contained in the {@link ObservableList} of {@link Node} objects.
	 *
	 * @param popupNodes
	 * 		List of nodes in the popup
	 * @return a list of Labels
	 */
	@NotNull
	public static List<Label> findLabelsInPopup(@NotNull final ObservableList<Node> popupNodes) {
		final List<Label> labels = new ArrayList<>();
		popupNodes.forEach(popupNode -> {
			if (popupNode instanceof Label label) {
				labels.add(label);
			} else if (popupNode instanceof HBox hBox) {
				labels.addAll(findLabelsInPopup(hBox.getChildren()));
			} else if (popupNode instanceof VBox vBox) {
				labels.addAll(findLabelsInPopup(vBox.getChildren()));
			} else if (popupNode instanceof GridPane gridPane) {
				labels.addAll(findLabelsInPopup(gridPane.getChildren()));
			}
		});
		return labels;
	}

	/**
	 * Recursively build a {@link List} of {@link CheckBox} controllers contained in the {@link ObservableList} of {@link Node} objects.
	 *
	 * @param popupNodes
	 * 		list of nodes in the popup
	 * @return a list
	 */
	@NotNull
	public static List<CheckBox> findCheckBoxesInPopup(@NotNull final ObservableList<Node> popupNodes){
		final List<CheckBox> checkBoxes = new ArrayList<>();
		popupNodes.forEach(popupNode -> {
			if (popupNode instanceof CheckBox checkBox) {
				checkBoxes.add(checkBox);
			} else if (popupNode instanceof HBox hBox) {
				checkBoxes.addAll(findCheckBoxesInPopup(hBox.getChildren()));
			} else if (popupNode instanceof VBox vBox) {
				checkBoxes.addAll(findCheckBoxesInPopup(vBox.getChildren()));
			} else if (popupNode instanceof GridPane gridPane) {
				checkBoxes.addAll(findCheckBoxesInPopup(gridPane.getChildren()));
			} else if (popupNode instanceof TitledPane titledPane) {
				checkBoxes.addAll(findCheckBoxesInPopup(FXCollections.singletonObservableList(titledPane.getContent())));
			}
		});
		return checkBoxes;
	}

	/**
	 * Recursively build a {@link List} of {@link Hyperlink} controllers contained in the {@link ObservableList} of {@link Node} objects.
	 *
	 * @param popupNodes
	 * 		list of nodes in the popup
	 * @return a list
	 */
	@NotNull
	public static List<Hyperlink> findHyperlinksInPopup(@NotNull final ObservableList<Node> popupNodes) {
		final List<Hyperlink> hyperlinks = new ArrayList<>();
		popupNodes.forEach(popupNode -> {
			if (popupNode instanceof Hyperlink hyperlink) {
				hyperlinks.add(hyperlink);
			} else if (popupNode instanceof HBox hBox) {
				hyperlinks.addAll(findHyperlinksInPopup(hBox.getChildren()));
			} else if (popupNode instanceof VBox vBox) {
				hyperlinks.addAll(findHyperlinksInPopup(vBox.getChildren()));
			} else if (popupNode instanceof GridPane gridPane) {
				hyperlinks.addAll(findHyperlinksInPopup(gridPane.getChildren()));
			}
		});
		return hyperlinks;
	}

	/**
	 * Recursively build a {@link List} of {@link GridPane} controllers contained in the {@link ObservableList} of {@link Node} objects.
	 *
	 * @param popupNodes
	 * 		list of nodes in the popup
	 * @return a list
	 */
	@NotNull
	public static List<GridPane> findGridPanesInPopup(@NotNull final ObservableList<Node> popupNodes) {
		final List<GridPane> nodes = new ArrayList<>();
		popupNodes.forEach(popupNode -> {
			if (popupNode instanceof GridPane gridPane) {
				nodes.add(gridPane);
			} else if (popupNode instanceof HBox hBox) {
				nodes.addAll(findGridPanesInPopup(hBox.getChildren()));
			} else if (popupNode instanceof VBox vBox) {
				nodes.addAll(findGridPanesInPopup(vBox.getChildren()));
			}
		});
		return nodes;
	}

	/**
	 * Recursively builds a {@link List} of the leaves of the given {@link TreeItem}.
	 *
	 * @param treeItem
	 * 		a treeItem
	 * @return a list
	 */
	@NotNull
	public static List<TreeItem> getChildren(@NotNull final TreeItem<?> treeItem) {
		final List<TreeItem> leaves = new ArrayList<>();

		if (treeItem.getChildren().isEmpty()) {
			leaves.add(treeItem);
		} else {
			final ObservableList<? extends TreeItem<?>> children = treeItem.getChildren();
			for (final var child : children) {
				leaves.addAll(Objects.requireNonNull(getChildren(child)));
			}
		}
		return leaves;
	}

	/**
	 * Count the nodes in a {@link TreeItem}
	 *
	 * @param node
	 * 		root of the tree
	 * @return the number of nodes.
	 */
	public static int countTreeNodes(@NotNull final TreeItem<?> node) {
		var count = 1;
		for (final TreeItem t : node.getChildren()) {
			count += countTreeNodes(t);
		}
		return count;
	}

	/**
	 * Recursively build a {@link List} of {@link ChoiceBox} controllers
	 * contained in the {@link ObservableList} of {@link Node} objects.
	 *
	 * @param children
	 *		a list of nodes
	 * @return a list
	 */
	@NotNull
	public static List<ChoiceBox> findChoiceBoxes(@NotNull final ObservableList<Node> children) {
		final List<ChoiceBox> nodes = new ArrayList<>();
		children.forEach(child -> {
			if (child instanceof ChoiceBox choiceBox) {
				nodes.add(choiceBox);
			}
			if (child instanceof GridPane gridPane) {
				nodes.addAll(findChoiceBoxes(gridPane.getChildren()));
			} else if (child instanceof HBox hBox) {
				nodes.addAll(findChoiceBoxes(hBox.getChildren()));
			} else if (child instanceof VBox vBox) {
				nodes.addAll(findChoiceBoxes(vBox.getChildren()));
			}
		});
		return nodes;
	}

	/**
	 * Recursively build a {@link List} of the text of {@link Label} controllers, in lowercase,
	 * contained in the {@link ObservableList} of {@link Node} objects.
	 *
	 * @param children
	 *		a list of nodes
	 * @return a list
	 */
	@NotNull
	public static List<String> getLabels(@NotNull final ObservableList<Node> children) {
		final List<String> labels = new ArrayList<>();
		children.forEach(child -> {
			if (child instanceof Label label) {
				labels.add(label.getText().toLowerCase(Locale.ROOT));
			}
			if (child instanceof HBox hBox) {
				labels.addAll(getLabels(hBox.getChildren()));
			}
			if (child instanceof VBox vBox) {
				labels.addAll(getLabels(vBox.getChildren()));
			}
			if (child instanceof GridPane gridPane) {
				labels.addAll(getLabels(gridPane.getChildren()));
			}
		});
		return labels;
	}
}
