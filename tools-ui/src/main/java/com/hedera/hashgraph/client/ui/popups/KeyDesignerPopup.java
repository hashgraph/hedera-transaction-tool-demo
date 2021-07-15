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

import com.google.gson.JsonObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.ToolTipMessages;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoCreateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolCryptoUpdateTransaction;
import com.hedera.hashgraph.client.core.transactions.ToolTransaction;
import com.hedera.hashgraph.client.core.utils.BrowserUtilities;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import com.hedera.hashgraph.sdk.AccountInfo;
import com.hedera.hashgraph.sdk.Key;
import com.hedera.hashgraph.sdk.KeyList;
import com.hedera.hashgraph.sdk.PublicKey;
import com.sun.javafx.scene.control.LabeledText;
import javafx.beans.binding.Bindings;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Accordion;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Separator;
import javafx.scene.control.TitledPane;
import javafx.scene.control.Tooltip;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DragEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.util.Callback;
import javafx.util.Duration;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.hedera.hashgraph.client.core.constants.Constants.ACCOUNTS_MAP_FILE;
import static com.hedera.hashgraph.client.core.constants.Constants.BLUE_BUTTON_STYLE;
import static com.hedera.hashgraph.client.core.constants.Constants.DEFAULT_STORAGE;
import static com.hedera.hashgraph.client.core.constants.Constants.INFO_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.JSON_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.PUB_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SIGNED_TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TRANSACTION_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.USER_PROPERTIES;
import static com.hedera.hashgraph.client.core.constants.Constants.WHITE_BUTTON_STYLE;
import static com.hedera.hashgraph.client.core.constants.Messages.CONTINUE_MESSAGE;
import static com.hedera.hashgraph.client.core.constants.Messages.FILE_HAS_BEEN_DELETED_MESSAGE;
import static com.hedera.hashgraph.client.core.utils.EncryptionUtils.trimTo64;

public class KeyDesignerPopup implements GenericFileReadWriteAware {

	private static final Logger logger = LogManager.getLogger(KeyDesignerPopup.class);
	public static final String FX_BORDER_COLOR_BLACK = "-fx-border-color: black";
	public static final String THRESHOLD_KEY_X_OF_X = "Threshold key (x of x)";
	public static final String THRESHOLD_KEY_1_OF_1 = "Threshold key (1 of 1)";
	private static final String KEYS_STRING = "/Keys/";
	public static final String THRESHOLD_STRING = "Threshold";
	public static final String THRESHOLD_MARKER = " key (";
	public static final String THRESHOLDS_NOT_SET_ERROR =
			"The following threshold key values have not been set. Please enter a valid threshold value and try again" +
					".\n";
	public static final String DESIGNER_TITLE = "Key";

	private final Map<String, PublicKey> publicKeys;
	private final Map<String, PublicKey> orphanKeys = new HashMap<>();
	private final Map<String, PublicKey> extraKeys = new HashMap<>();
	private final Map<String, String> keyAddresses = new HashMap<>();
	private final Map<String, Key> accountsAddresses = new HashMap<>();
	private final UserAccessibleProperties properties =
			new UserAccessibleProperties(DEFAULT_STORAGE + File.separator + USER_PROPERTIES, "");

	private VBox design = new VBox();
	private final VBox publicKeysBox = new VBox();
	private final VBox accountsBox = new VBox();
	private final VBox buttonsAccounts = new VBox();
	private final VBox buttonsKeys = new VBox();
	private Key key;
	private JsonObject jsonKey = new JsonObject();
	private TreeView<String> treeView;
	private ListView<String> appPubKeyList = new ListView<>();
	private ListView<String> extraPubKeyList = new ListView<>();
	private final ListView<String> accountKeyList = new ListView<>();
	private final Set<String> missingThresholds = new HashSet<>();

	Stage window = new Stage();
	private VBox layout;

	/**
	 * Constructor. Primarily used when a key is build from scratch (i.e: account create)
	 *
	 * @param publicKeys
	 * 		the map of public keys
	 */
	public KeyDesignerPopup(Map<String, PublicKey> publicKeys) {
		this.publicKeys = publicKeys;
		this.key = null;
	}

	/**
	 * Constructor. Primarily used when a key is going to be modified (i.e: account update)
	 *
	 * @param publicKeys
	 * 		the list of public keys known by the app
	 * @param key
	 * 		if there is an initial key to be updated. Otherwise an empty key.
	 */
	public KeyDesignerPopup(Map<String, PublicKey> publicKeys, Key key) {
		this.publicKeys = publicKeys;
		this.key = key;
	}

	/**
	 * Main entry point for the class. Displays the Key Designer popup.
	 *
	 * @return a json object representing the created key. If nothing is created the method returns an empty json.
	 */
	public JsonObject display() {
		window.initModality(Modality.APPLICATION_MODAL);
		window.setTitle(DESIGNER_TITLE);
		window.sizeToScene();

		var menuBar = new MenuBar();
		layout = new VBox(menuBar);
		layout.setStyle("-fx-font-size: 16");
		layout.setAlignment(Pos.CENTER);
		layout.setMinWidth(800);
		layout.setPrefHeight(Region.USE_COMPUTED_SIZE);

		var fileMenu = buildFileMenu();
		menuBar.getMenus().addAll(fileMenu);

		var workBox = new HBox();
		workBox.setSpacing(10);
		workBox.setAlignment(Pos.CENTER);
		VBox.setVgrow(workBox, Priority.ALWAYS);

		final var buttons = new VBox();
		setupCenterButtonsBox(buttons);
		setupAccountsBox();
		setupKeysBox();
		setupDesignBox();
		var accordion = setupAccordionPane();

		var designWithTitle = new TitledPane("Key", design);
		designWithTitle.setMaxHeight(Double.MAX_VALUE);
		designWithTitle.setCollapsible(false);
		HBox.setHgrow(designWithTitle, Priority.ALWAYS);
		HBox.setHgrow(accordion, Priority.ALWAYS);

		workBox.getChildren().addAll(accordion, buttons, designWithTitle);
		VBox.setVgrow(designWithTitle, Priority.ALWAYS);
		publicKeysBox.setPrefHeight(Region.USE_COMPUTED_SIZE);

		var saveBox = new HBox();
		var region = new Region();
		region.setPrefHeight(Region.USE_COMPUTED_SIZE);
		region.setPrefWidth(Region.USE_COMPUTED_SIZE);
		HBox.setHgrow(region, Priority.ALWAYS);

		var saveButton = new Button("SAVE");
		saveButton.setStyle(BLUE_BUTTON_STYLE);
		saveButton.setPrefWidth(150);
		saveButton.setOnAction(actionEvent -> {
			missingThresholds.clear();
			var keyList = treeToKeyList(treeView.getRoot());
			if (keyList != null) {
				try {
					jsonKey = EncryptionUtils.keyToJson(keyList);
					window.close();
				} catch (IOException e) {
					logger.error(e);
				}
			} else {
				PopupMessage.display("Thresholds not set", getErrorMessage(), CONTINUE_MESSAGE);
			}
		});

		var cancelButton = new Button("CANCEL");
		cancelButton.setStyle(WHITE_BUTTON_STYLE);
		cancelButton.setPrefWidth(150);
		cancelButton.setOnAction(actionEvent -> {
			jsonKey = new JsonObject();
			window.close();
		});
		saveBox.setSpacing(5);
		saveBox.getChildren().addAll(region, saveButton, cancelButton);
		saveBox.setAlignment(Pos.CENTER);
		saveBox.setPrefHeight(Region.USE_COMPUTED_SIZE);

		var vBox = new VBox();
		vBox.setSpacing(20);
		vBox.setPadding(new Insets(10));

		vBox.getChildren().addAll(workBox, saveBox);

		layout.getChildren().add(vBox);
		VBox.setVgrow(vBox, Priority.ALWAYS);

		var scene = new Scene(layout);
		scene.getStylesheets().add("tools.css");
		window.setScene(scene);
		window.showAndWait();
		return jsonKey;
	}

	/**
	 * Setups the accordion pane that will contain the accounts and the public keys
	 *
	 * @return an accordion Pane
	 */
	private Accordion setupAccordionPane() {
		var accountsTitledPane = new TitledPane("Accounts", accountsBox);
		accountsTitledPane.setGraphic(getImageViewWithTooltip(ToolTipMessages.ACCOUNTS_BOX_DESIGNER_TOOLTIP_TEXT));
		accountsTitledPane.setContentDisplay(ContentDisplay.RIGHT);
		accountsTitledPane.setAnimated(false);


		var keysTitledPane = new TitledPane("Public keys", publicKeysBox);
		keysTitledPane.setGraphic(getImageViewWithTooltip(ToolTipMessages.PUBLIC_KEYS_BOX_DESIGNER_TOOLTIP_TEXT));
		keysTitledPane.setContentDisplay(ContentDisplay.RIGHT);
		keysTitledPane.setAnimated(false);

		buttonsKeys.visibleProperty().bind(keysTitledPane.expandedProperty());
		buttonsAccounts.visibleProperty().bind(accountsTitledPane.expandedProperty());

		var accordion = new Accordion();
		accordion.getPanes().addAll(keysTitledPane, accountsTitledPane);
		accordion.setExpandedPane(keysTitledPane);
		return accordion;
	}

	/**
	 * Builds an image with a tooltip
	 *
	 * @param message
	 * 		the tooltip
	 * @return an image
	 */
	private ImageView getImageViewWithTooltip(String message) {
		var img = new ImageView(new Image("icons/helpIcon.png"));

		img.setPreserveRatio(true);
		img.setFitHeight(20);
		var tooltip = new Tooltip(message);
		tooltip.setShowDelay(new Duration(200));
		tooltip.setStyle("-fx-background-color: white; -fx-text-fill: black;");
		tooltip.setMaxWidth(300);
		tooltip.setWrapText(true);
		Tooltip.install(img, tooltip);
		return img;
	}

	/**
	 * Sets the menu to be shown at the top of the window (File menu)
	 *
	 * @return a menu bar
	 */
	private Menu buildFileMenu() {
		var fileMenu = new Menu("File");
		var load = new Menu("Load key...");
		var load1 = new MenuItem("from transaction (TX or TXSIG)");
		load1.setOnAction(actionEvent -> {
			browseTransactions();
			refreshTree();
		});

		var load2 = new MenuItem("from account or entity (INFO)");
		load2.setOnAction(actionEvent -> {
			browseInfos();
			refreshTree();
		});

		var load3 = new MenuItem("from json file (JSON)");
		load3.setOnAction(actionEvent -> {
			browseJsons();
			refreshTree();
		});

		load.getItems().addAll(load1, load2, load3);

		var importPub = new MenuItem("Import public key (PUB)");
		importPub.setOnAction(actionEvent -> {
			importPublicKey();
			setupKeysBox();
		});

		var save = new MenuItem("Save as json file");
		save.setOnAction(actionEvent -> saveAsJson());
		fileMenu.getItems().addAll(load, importPub, save);

		return fileMenu;
	}

	/**
	 * Sets the accounts box: Displays the accounts currently known by the app. If the user double clicks on any
	 * account, its key replaces whatever is currently in the Key Design box
	 */
	private void setupAccountsBox() {
		JsonObject nicknameMap;
		try {
			var accountInfoMap = properties.getAccountInfoMap();
			nicknameMap =
					(new File(ACCOUNTS_MAP_FILE).exists()) ? readJsonObject(ACCOUNTS_MAP_FILE) : new JsonObject();

			for (var entry : accountInfoMap.entrySet()) {
				var nn = CommonMethods.nicknameOrNumber(Identifier.parse(entry.getKey()), nicknameMap);
				if (nicknameMap.has(entry.getKey())) {
					accountsAddresses.put(nn, AccountInfo.fromBytes(readBytes(entry.getValue())).key);
				}
			}

		} catch (HederaClientException | InvalidProtocolBufferException e) {
			logger.error(e);
		}


		final List<String> sortedList = new ArrayList<>(accountsAddresses.keySet());
		Collections.sort(sortedList);
		for (var keyName : sortedList) {
			accountKeyList.getItems().add(keyName);
		}

		accountKeyList.setOnMouseClicked(event -> {
			if (event.getButton() == MouseButton.PRIMARY && event.getClickCount() == 2 &&
					event.getTarget() instanceof LabeledText) {
				var accountNickname = ((LabeledText) event.getTarget()).getText();
				key = accountsAddresses.get(accountNickname);
				refreshTree();
			}

		});

		accountKeyList.setStyle(FX_BORDER_COLOR_BLACK);
		VBox.setVgrow(accountKeyList, Priority.ALWAYS);

		accountsBox.getChildren().add(accountKeyList);
		accountsBox.setSpacing(10);
		VBox.setVgrow(accountsBox, Priority.ALWAYS);
		HBox.setHgrow(accountsBox, Priority.ALWAYS);
	}

	/**
	 * Sets the public keys box: Displays the public keys currently known to the app, as well as the public keys
	 * imported by the user (imported keys are not imported to the app).
	 * The user can drag any public key to the design. The user can double click a key to display its information.
	 */
	private void setupKeysBox() {
		publicKeysBox.getChildren().clear();
		publicKeysBox.setMinHeight(450);
		HBox.setHgrow(publicKeysBox, Priority.ALWAYS);
		appPubKeyList = getPublicKeyListView(publicKeys.keySet());

		extraPubKeyList = getPublicKeyListView(extraKeys.keySet());
		extraPubKeyList.minHeightProperty().bind(Bindings.size(extraPubKeyList.getItems()).multiply(34).add(15));
		extraPubKeyList.maxHeightProperty().bind(Bindings.size(extraPubKeyList.getItems()).multiply(34.5).add(15));


		var keys = new VBox();
		keys.getChildren().add(appPubKeyList);
		if (!extraPubKeyList.getItems().isEmpty()) {
			keys.getChildren().add(new Separator());
			keys.getChildren().add(extraPubKeyList);
		}
		VBox.setVgrow(keys, Priority.ALWAYS);
		VBox.setVgrow(appPubKeyList, Priority.ALWAYS);
		publicKeysBox.getChildren().addAll(keys);
		publicKeysBox.setSpacing(10);
	}

	/**
	 * Sets the center buttons box. The two buttons in the box are "Add" (right-arrow) to add whatever public key is
	 * highlighted to the current design; and "Remove" (cross) that erases the portion of the designed key that is
	 * highlighted.
	 */
	private void setupCenterButtonsBox(VBox buttons) {

		var add = new Button();
		var arrowImage = new Image("icons/outline_east_black_48pt_3x.png");
		var arrowView = new ImageView(arrowImage);
		arrowView.setFitHeight(20);
		arrowView.setPreserveRatio(true);
		add.setGraphic(arrowView);
		add.setMinWidth(50);
		add.setStyle("-fx-background-color: transparent; -fx-border-color: darkgray; -fx-border-radius: 5");
		add.setOnAction(event -> {
			if (appPubKeyList.getSelectionModel().getSelectedItem() == null && extraPubKeyList.getSelectionModel().getSelectedItem() == null) {
				logger.info("No source selected");
				return;
			}
			var source =
					(appPubKeyList.getSelectionModel().getSelectedItem() != null) ?
							appPubKeyList.getSelectionModel().getSelectedItem() :
							extraPubKeyList.getSelectionModel().getSelectedItem();

			var destination =
					(treeView.getSelectionModel().getSelectedItem() != null) ?
							treeView.getSelectionModel().getSelectedItem() : treeView.getRoot();
			logger.info(String.format("Copying %s to %s", source, destination.getValue()));
			final var value = destination.getValue();
			if (!value.contains(" key")) {
				destination.setValue(THRESHOLD_KEY_X_OF_X);
				destination.getChildren().add(new TreeItem<>(value));
			}
			destination.getChildren().add(new TreeItem<>(source));
			destination.setExpanded(true);
			setSizes(treeView.getRoot());
		});

		var remove = new Button("\u2718");
		remove.setOnAction(event -> removeFromTree(treeView.getSelectionModel().getSelectedItem()));
		remove.setStyle(
				"-fx-background-color: transparent; -fx-text-fill: red;-fx-border-color: indianred; " +
						"-fx-border-radius: 5");
		remove.setMinWidth(50);

		buttonsKeys.getChildren().addAll(add, remove);
		buttonsKeys.setAlignment(Pos.CENTER);
		buttonsKeys.setSpacing(10);

		buttonsKeys.managedProperty().bind(buttonsKeys.visibleProperty());
		buttons.getChildren().add(buttonsKeys);

		var addAccount = new Button();
		var arrowView2 = new ImageView(arrowImage);
		arrowView2.setFitHeight(20);
		arrowView2.setPreserveRatio(true);
		addAccount.setGraphic(arrowView2);
		addAccount.setMinWidth(50);
		addAccount.setStyle("-fx-background-color: transparent; -fx-border-color: darkgray; -fx-border-radius: 5");
		addAccount.setOnAction(event -> {
			var accountNickname = accountKeyList.getSelectionModel().getSelectedItem();
			key = accountsAddresses.get(accountNickname);
			refreshTree();
		});
		buttonsAccounts.managedProperty().bind(buttonsAccounts.visibleProperty());
		buttonsAccounts.setAlignment(Pos.CENTER);
		buttonsAccounts.getChildren().add(addAccount);

		buttons.setAlignment(Pos.CENTER);
		buttons.getChildren().add(buttonsAccounts);
	}

	/**
	 * Set the design box. This box contains a TreeView that will be used to design the new key.
	 */
	private void setupDesignBox() {
		design = new VBox();
		design.setStyle("-fx-border-color: transparent");

		//TreeView
		var buildKeysTreeView = buildKeysTreeView();
		buildKeysTreeView.setStyle(FX_BORDER_COLOR_BLACK);
		VBox.setVgrow(buildKeysTreeView, Priority.ALWAYS);
		design.getChildren().clear();
		design.getChildren().add(buildKeysTreeView);
		design.setSpacing(10);
		HBox.setHgrow(design, Priority.ALWAYS);
	}

	// region MENU ITEMS SETUP

	/**
	 * Saves the current design as a json file
	 */
	private void saveAsJson() {
		logger.info("saving progress");
		missingThresholds.clear();
		var keyList = treeToKeyList(treeView.getRoot());
		if (keyList == null) {
			PopupMessage.display("Thresholds not set", getErrorMessage(), CONTINUE_MESSAGE);
			return;
		}

		var fileChooser = new FileChooser();
		fileChooser.setTitle("Save key as json");
		fileChooser.setInitialDirectory(new File(properties.getLastBrowsedDirectory()));
		var file = fileChooser.showSaveDialog(window);
		if (file != null) {
			try {
				if (file.exists()) {
					Files.delete(file.toPath());
					logger.info(FILE_HAS_BEEN_DELETED_MESSAGE);
				}
				writeJsonObject(file.getAbsolutePath(), EncryptionUtils.keyToJson(keyList));
				properties.setLastBrowsedDirectory(file);
			} catch (IOException | HederaClientException ex) {
				logger.error(ex);
			}
		}

	}

	/**
	 * If there are error messages, it concatenates them into a single string that will be displayed in a popup
	 *
	 * @return an error message string
	 */
	private String getErrorMessage() {
		var builder = new StringBuilder(THRESHOLDS_NOT_SET_ERROR);
		var separator = "\t - ";
		for (var missingThreshold : missingThresholds) {
			builder.append(separator).append(missingThreshold);
			separator = "\n\t - ";
		}
		return builder.toString();
	}

	/**
	 * Imports a single public key. This key will be displayed in a separate ListView
	 */
	private void importPublicKey() {
		logger.info("import public key");
		var file = BrowserUtilities.browseFiles(properties.getLastBrowsedDirectory(), layout,
				"Json Key", PUB_EXTENSION);
		if (file == null) {
			return;
		}
		properties.setLastBrowsedDirectory(file);
		final var absolutePath = file.getAbsolutePath();

		var truncated =
				absolutePath.substring(0, 16) + "\u2026" + absolutePath.substring(absolutePath.lastIndexOf("/"));
		keyAddresses.put(truncated, absolutePath);
		extraKeys.put(truncated, EncryptionUtils.publicKeyFromFile(absolutePath));
	}

	/**
	 * Browse json files for keys. If the resulting JsonObject can be transformed to a KeyList, it will be set in the
	 * design box
	 */
	private void browseJsons() {
		logger.info("browsing jsons");
		var file = BrowserUtilities.browseFiles(properties.getLastBrowsedDirectory(), layout,
				"Json Key", JSON_EXTENSION);
		if (file == null) {
			return;
		}
		properties.setLastBrowsedDirectory(file);
		try {
			key = EncryptionUtils.jsonToKey(readJsonObject(file));
		} catch (HederaClientException e) {
			logger.error("Cannot parse json object into a key");
		}
	}

	/**
	 * Browse info files. The key associated with the account will be set in the design box
	 */
	private void browseInfos() {
		logger.info("browsing infos");
		var file = BrowserUtilities.browseFiles(properties.getLastBrowsedDirectory(), layout,
				"Transaction", INFO_EXTENSION);
		if (file == null) {
			return;
		}
		properties.setLastBrowsedDirectory(file);
		try {
			key = AccountInfo.fromBytes(readBytes(file.getAbsolutePath())).key;
		} catch (InvalidProtocolBufferException | HederaClientException e) {
			logger.error(e);
			key = null;
		}
	}

	/**
	 * Browse transaction files. If the transaction is an Account Creation or Update, the key will be set in the design
	 * box
	 */
	private void browseTransactions() {
		logger.info("browsing transactions");

		Key newKey = null;
		var file = BrowserUtilities.browseFiles(properties.getLastBrowsedDirectory(), layout,
				"Transaction", TRANSACTION_EXTENSION, SIGNED_TRANSACTION_EXTENSION);
		if (file == null) {
			return;
		}
		properties.setLastBrowsedDirectory(file);


		try {
			var transaction = new ToolTransaction().parseFile(file);
			logger.info("Transaction loaded");
			if (transaction instanceof ToolCryptoCreateTransaction) {
				newKey = ((ToolCryptoCreateTransaction) transaction).getKey();
			} else if (transaction instanceof ToolCryptoUpdateTransaction) {
				newKey = ((ToolCryptoUpdateTransaction) transaction).getKey();
			} else {
				PopupMessage.display("Key not found", "The transaction selected does not contain a key",
						CONTINUE_MESSAGE);
			}
			key = newKey;
		} catch (HederaClientException e) {
			logger.error("Cannot read transaction");
		}
	}

	/**
	 * Reloads the tree. Used when a new key is loaded from a file
	 */
	private void refreshTree() {
		buildKeysTreeView();
		VBox.setVgrow(treeView, Priority.ALWAYS);
		design.getChildren().clear();
		design.getChildren().add(treeView);
	}
	// endregion

	// region TREE SETUP

	/**
	 * Creates the TreeView that will contain the user's design. Double-clicking an inner node edits the threshold.
	 * Right-clicking any node shows a "delete" context menu, that will remove the node and any children from the tree
	 *
	 * @return a TreeView
	 */
	private TreeView<String> buildKeysTreeView() {

		var root = (key == null) ? new TreeItem<>(THRESHOLD_KEY_X_OF_X) : keyToTreeView(key);
		if (!root.getValue().contains(THRESHOLD_STRING)) {
			var newRoot = new TreeItem<>(THRESHOLD_KEY_1_OF_1);
			newRoot.getChildren().add(root);
			root = newRoot;
		}
		root.setExpanded(true);

		treeView = new TreeView<>(root);
		treeView.setEditable(true);

		treeView.setCellFactory(getTreeCallback());

		return treeView;

	}

	/**
	 * Callback for the tree view drag and drop
	 *
	 * @return a callback
	 */
	private Callback<TreeView<String>, TreeCell<String>> getTreeCallback() {
		return new Callback<>() {
			@Override
			public TreeCell<String> call(TreeView<String> stringTreeView) {
				TreeCell<String> treeCell = new TreeCell<>() {
					@Override
					protected void updateItem(String item, boolean empty) {
						super.updateItem(item, empty);
						updateAction(item);
					}

					private void updateAction(String item) {
						if (item != null) {
							setText(item);
							final var contextMenu = new ContextMenu();
							var deleteMenu = new MenuItem("delete");
							deleteMenu.setOnAction(
									actionEvent -> removeFromTree(treeView.getSelectionModel().getSelectedItem()));
							contextMenu.getItems().add(deleteMenu);

							handleThresholds(item, contextMenu);
							setContextMenu(contextMenu);
							return;
						}
						setText(null);
						setGraphic(null);
					}

					private void handleThresholds(String item, ContextMenu contextMenu) {
						if (item.contains(THRESHOLD_MARKER)) {
							var addThresholdMenu = new MenuItem("add threshold key");
							addThresholdMenu.setOnAction(actionEvent -> addThresholdMenuEvent());
							contextMenu.getItems().add(addThresholdMenu);
						}
					}

					private void addThresholdMenuEvent() {
						var newThreshold = new TreeItem<>(THRESHOLD_KEY_X_OF_X);
						getTreeItem().getChildren().add(newThreshold);
						logger.info("Added threshold to tree");
					}
				};

				treeCell.setOnDragDetected((MouseEvent event) -> treeCellDragAction(treeCell, event));

				treeCell.setOnDragDone(event -> treeCellDragDoneEvent(treeCell, event));

				treeCell.setOnDragEntered((DragEvent event) -> treeCell.setStyle("-fx-background-color: lightblue;"));

				treeCell.setOnDragExited((DragEvent event) -> treeCell.setStyle(""));

				treeCell.setOnDragOver((DragEvent event) -> treeCellDragOverEvent(event));

				treeCell.setOnDragDropped((DragEvent event) -> treeCellDragDroppedEvent(treeCell, event));

				treeCell.setOnMouseClicked(mouseEvent -> treeCellMouseClickedEvent(treeCell, mouseEvent));
				return treeCell;
			}
		};
	}

	private void treeCellMouseClickedEvent(TreeCell<String> treeCell, MouseEvent mouseEvent) {
		if (treeCell.getTreeItem() == null) {
			return;
		}
		if (!mouseEvent.getButton().equals(MouseButton.PRIMARY) || mouseEvent.getClickCount() != 2) {
			return;
		}
		if (treeCell.getTreeItem().isLeaf()) {
			return;
		}

		var item = treeCell.getTreeItem();
		try {
			var thresholdString = GenericPopup.display(THRESHOLD_STRING, "ACCEPT", "CANCEL", true, true,
					"Enter the threshold");
			if (thresholdString == null) {
				return;
			}
			var threshold = Integer.parseInt(thresholdString);
			final var formattedItem = String.format("%s(%d of %d)",
					item.getValue().substring(0, item.getValue().lastIndexOf("(")), threshold,
					item.getChildren().size());
			item.setValue(formattedItem);
		} catch (HederaClientException e) {
			logger.error(e);
		}
	}

	private void treeCellDragDroppedEvent(TreeCell<String> treeCell, DragEvent event) {
		logger.info("treeCell.setOnDragDropped");
		var db = event.getDragboard();
		var success = false;
		if (treeCell.getTreeItem() != null && db.hasString()) {
			final var value = treeCell.getTreeItem().getValue();
			if (!value.contains(" key")) {
				treeCell.getTreeItem().setValue(THRESHOLD_KEY_X_OF_X);
				treeCell.getTreeItem().getChildren().add(new TreeItem<>(value));
			}
			treeCell.getTreeItem().getChildren().add(new TreeItem<>(db.getString()));
			treeCell.getTreeItem().setExpanded(true);
			success = true;
		}
		setSizes(treeView.getRoot());
		event.setDropCompleted(success);
		event.consume();
	}

	private void treeCellDragOverEvent(DragEvent event) {
		var db = event.getDragboard();
		if (db.hasString()) {
			event.acceptTransferModes(TransferMode.COPY_OR_MOVE);
		}
		event.consume();
	}

	private void treeCellDragDoneEvent(TreeCell<String> treeCell, DragEvent event) {
		if (event.getTransferMode().equals(TransferMode.MOVE)) {
			removeFromTree(treeCell.getTreeItem());
		}
		event.consume();
	}

	private void treeCellDragAction(TreeCell<String> treeCell, MouseEvent event) {
		if (treeCell.getTreeItem().isLeaf()) {
			var db = treeCell.startDragAndDrop(TransferMode.MOVE);
			var content = new ClipboardContent();
			content.putString(treeCell.getItem());
			db.setContent(content);
			event.consume();
		}
	}

	/**
	 * Converts an SDK Key to a TreeView
	 *
	 * @param key
	 * 		the key to be displayed
	 * @return the root of a TreeView
	 */
	private TreeItem<String> keyToTreeView(Key key) {
		if (key instanceof KeyList && ((KeyList) key).size() == 1) {
			var object = ((KeyList) key).toArray()[0];
			assert object instanceof Key;
			return keyToTreeView((Key) object);
		}
		if (key instanceof PublicKey) {
			return new TreeItem<>(getKeyName((PublicKey) key));
		} else {
			assert key instanceof KeyList;
			return getTreeItem((KeyList) key);
		}
	}

	/**
	 * Converts a TreeView to a KeyList
	 *
	 * @param root
	 * 		the root of the tree
	 * @return the KeyList represented by the Tree structure
	 */
	private KeyList treeToKeyList(TreeItem<String> root) {
		if (root.isLeaf()) {
			return null;
		}
		checkTree(root);
		if (missingThresholds.isEmpty()) {
			return thresholdKey(root);
		}
		return null;
	}

	/**
	 * Traverses the tree collecting information regarding the thresholds. If any threshold is not set, it adds its name
	 * to the missingThresholds list
	 *
	 * @param root
	 * 		initial node
	 */
	private void checkTree(TreeItem<String> root) {
		if (root.isLeaf()) {
			return;
		}
		if (getThresholdFromString(root.getValue(), root.getChildren().size()) == -1) {
			missingThresholds.add(root.getValue());
		}
		for (var child : root.getChildren()) {
			checkTree(child);
		}
	}

	/**
	 * Converts a node in the TreeView to a Threshold key (All internal nodes in the tree correspond to threshold keys,
	 * all leaves correspond to public keys)
	 *
	 * @param thresholdTree
	 * 		an inner node in the TreeView
	 * @return The threshold key represented by the structure
	 */
	private KeyList thresholdKey(TreeItem<String> thresholdTree) {
		var thresholdKey = new KeyList();
		var threshold = getThresholdFromString(thresholdTree.getValue(), thresholdTree.getChildren().size());
		if (threshold == -1) {
			return null;
		}

		thresholdKey.setThreshold(threshold);
		for (var child : thresholdTree.getChildren()) {
			final var childValue = child.getValue();
			if (!child.isLeaf()) {
				final var keyList = thresholdKey(child);
				if (keyList == null) {
					return null;
				}
				thresholdKey.add(keyList);
				continue;
			}
			var publicKey = getPublicKeyFromMaps(childValue);

			final var addOK = thresholdKey.add(publicKey);
			if (addOK) {
				logger.info("{} has been added", childValue);
			}
		}

		return thresholdKey;
	}

	/**
	 * Finds the public key name in the maps
	 *
	 * @param keyName
	 * 		the name of the public key
	 * @return a public key if it is found. null otherwise
	 */
	private PublicKey getPublicKeyFromMaps(String keyName) {
		if (publicKeys.containsKey(keyName)) {
			return publicKeys.get(keyName);
		}
		if (orphanKeys.containsKey(keyName)) {
			return orphanKeys.get(keyName);
		}
		return extraKeys.getOrDefault(keyName, null);
	}

	/**
	 * Names the threshold key according to the names of the public keys. If all the children in the node are public
	 * keys (leaves), we calculate the longest common prefix of the names of all keys and use it as the name for the
	 * threshold (Any special characters at the end of the prefix are removed). If no common prefix is found, the
	 * generic "Threshold" name will be set
	 *
	 * @param keyList
	 * 		a KeyList
	 * @return the root of a threshold key with the name set accordingly
	 */
	private TreeItem<String> getTreeItem(KeyList keyList) {
		var size = keyList.size();
		var threshold = size;
		if (keyList.getThreshold() != null) {
			threshold = keyList.getThreshold();
		}

		var root = new TreeItem<>("placeholder");
		for (var keyMember : keyList) {
			if (keyMember instanceof PublicKey) {
				root.getChildren().add(new TreeItem<>(getKeyName((PublicKey) keyMember)));
				continue;
			}
			root.getChildren().add(keyToTreeView(keyMember));
		}
		var lcp = CommonMethods.longestCommonPrefix(getNamesList(root));
		var commonName = getCommonName(lcp);
		root.setValue(String.format("%s key (%d of %d)", commonName, threshold, size));
		return root;
	}


	/**
	 * Gets the threshold that the user previously set for the Threshold key (All inner nodes must have a threshold set)
	 *
	 * @param value
	 * 		the node's String value
	 * @param size
	 * 		the size of the KeyList (for checking)
	 * @return an integer representing the set threshold. -1 if the threshold has not been set.
	 */
	private int getThresholdFromString(String value, int size) {
		var split = value.split("[ ()]+");
		var thresholdString = split[2];
		var expectedSizeString = split[4];
		if (thresholdString.equals("x") || expectedSizeString.equals("x")) {
			return -1;
		}

		try {
			final var expectedSize = Integer.parseInt(expectedSizeString);
			final var threshold = Integer.parseInt(thresholdString);
			if (expectedSize != size || threshold > size) {
				return -1;
			}
			return threshold;
		} catch (NumberFormatException e) {
			return -1;
		}

	}

	/**
	 * Updates the inner node sizes when a new key is added or removed
	 *
	 * @param root
	 * 		an inner node of the tree view
	 */
	private void setSizes(TreeItem<String> root) {
		final var value = root.getValue();

		if (!value.contains(THRESHOLD_MARKER)) {
			return;
		}

		var lcp = CommonMethods.longestCommonPrefix(getNamesList(root));
		var newValue = getCommonName(lcp);
		root.setValue(String.format("%s key (x of %d)", newValue, root.getChildren().size()));

		for (var child : root.getChildren()) {
			setSizes(child);
		}
	}

	/**
	 * @param root
	 * 		an inner node of the tree structure
	 * @return a list of the names of the root's children
	 */
	private List<String> getNamesList(TreeItem<String> root) {
		List<String> keys = new ArrayList<>();
		for (var child : root.getChildren()) {
			var name = child.getValue().startsWith("/") ? child.getValue().substring(
					child.getValue().lastIndexOf("/") + 1) : child.getValue();
			keys.add(name);
		}
		return keys;
	}

	/**
	 * Builds the name for the threshold. Discards any special characters at the end of the string. If the input is
	 * empty or it already contains the "Threshold" keyword it returns "Threshold"
	 *
	 * @param lcp
	 * 		the longest common prefix of a series of strings
	 * @return the name for the threshold key
	 */
	private String getCommonName(String lcp) {
		return lcp.equals("") || lcp.contains(THRESHOLD_MARKER) ? THRESHOLD_STRING : StringUtils.stripEnd(lcp,
				"!@#$%^&*())_-+=?");
	}

	/**
	 * Removes a node from the Design tree
	 *
	 * @param treeItem
	 * 		the item to be removed
	 */
	private void removeFromTree(TreeItem<String> treeItem) {
		var p = treeItem.getParent();
		if (p == null) {
			// We are at the root, reset tree
			final var children = treeItem.getChildren();
			while (!children.isEmpty()) {
				removeFromTree(children.get(0));
			}
			setSizes(treeItem);
			return;
		}
		p.getChildren().remove(treeItem);
		if (!p.isLeaf()) {
			setSizes(p);
		}
		if (p.getChildren().isEmpty()) {
			removeFromTree(p);
		}
	}

	// endregion

	// region KEY LIST SETUP

	/**
	 * Converts a set of strings to a ListView. Double-clicking on any key will show the "Complete Keys" popup, that
	 * displays information regarding the key
	 *
	 * @param items
	 * 		a set of strings
	 * @return a ListView
	 */
	private ListView<String> getPublicKeyListView(Set<String> items) {
		var stringListView = new ListView<String>();

		final List<String> sortedList = new ArrayList<>(items);
		Collections.sort(sortedList);
		for (var keyName : sortedList) {
			stringListView.getItems().add(keyName);
		}

		stringListView.setOnMouseClicked(event -> {
			if (event.getButton() == MouseButton.PRIMARY && event.getClickCount() == 2 &&
					event.getTarget() instanceof LabeledText) {
				var textKey = ((LabeledText) event.getTarget()).getText();
				displayKeyPopup(textKey);
			}

		});

		stringListView.setCellFactory(new Callback<>() {
			@Override
			public ListCell<String> call(ListView<String> param) {
				ListCell<String> listCell = new ListCell<>() {
					@Override
					protected void updateItem(String item, boolean empty) {
						super.updateItem(item, empty);
						setText(item);
					}
				};

				listCell.setOnDragDetected((MouseEvent event) -> {
					var db = listCell.startDragAndDrop(TransferMode.COPY);
					var content = new ClipboardContent();
					content.putString(listCell.getItem());
					db.setContent(content);
					event.consume();
				});

				return listCell;
			}
		});
		stringListView.setStyle(FX_BORDER_COLOR_BLACK);
		return stringListView;
	}

	/**
	 * Gives a user readable string to be used as the key name
	 *
	 * @param key
	 * 		a public key
	 * @return the name of the public key. If no name is found, the method returns a string containing the hex code of
	 * 		the key
	 */
	private String getKeyName(PublicKey key) {
		var name = trimTo64(key);
		for (var entry : publicKeys.entrySet()) {
			if (entry.getValue().equals(key)) {
				return entry.getKey();
			}
		}
		orphanKeys.put(name, key);
		return name;
	}

	/**
	 * Displays the "Complete Keys" popup window.
	 *
	 * @param key
	 * 		the name of a public key
	 */
	private void displayKeyPopup(String key) {
		var pubKeyFiles = new File(properties.getPreferredStorageDirectory() + KEYS_STRING).listFiles(
				(dir, name) -> name.endsWith(key + "." + PUB_EXTENSION));
		assert pubKeyFiles != null;
		assert pubKeyFiles.length < 2;
		var address = (pubKeyFiles.length == 0) ? keyAddresses.get(key) : pubKeyFiles[0].getAbsolutePath();
		CompleteKeysPopup.display(address, false);
	}
	// endregion

}
