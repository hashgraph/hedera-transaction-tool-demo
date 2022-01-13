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

package com.hedera.hashgraph.client.integration;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.NetworkEnum;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Identifier;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.security.Ed25519KeyStore;
import com.hedera.hashgraph.client.core.transactions.ToolTransferTransaction;
import com.hedera.hashgraph.sdk.AccountId;
import com.hedera.hashgraph.sdk.Hbar;
import com.hedera.hashgraph.sdk.PrivateKey;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.CheckBox;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.Window;
import org.testfx.api.FxRobot;

import java.io.File;
import java.security.KeyStoreException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.hedera.hashgraph.client.core.constants.Constants.TEST_PASSWORD;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.ACCOUNT_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.AMOUNT;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_ACCOUNT_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.FEE_PAYER_KEY_LOCATION;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.H_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.MEMO_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NETWORK_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.NODE_ID_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.REALM_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.SHARD_NUMBER;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TINY_BARS;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_FEE_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_DURATION_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSACTION_VALID_START_FIELD_NAME;
import static com.hedera.hashgraph.client.core.constants.JsonConstants.TRANSFERS;

public class TestUtil {

	private static final TestBase driver = new TestBase();
	private static final FxRobot robot = new FxRobot();
	private static final Logger logger = LogManager.getLogger(TestUtil.class);
	private static final String RESOURCES_DIRECTORY = "src/test/resources";

	/**
	 * Create a json input for testing single transfers
	 *
	 * @param tinyBars
	 * 		amount to be transferred between accounts
	 * @param fromAccount
	 * 		sender
	 * @param toAccount
	 * 		receiver
	 * @return a json object
	 */
	public static JsonObject getJsonInputCT(final long tinyBars, final long fromAccount, final long toAccount) {
		final JsonObject testJson = new JsonObject();
		final File key = new File(RESOURCES_DIRECTORY + "/Keys/genesis.pem");

		final JsonObject feeJson = new JsonObject();
		feeJson.addProperty(H_BARS, 0);
		feeJson.addProperty(TINY_BARS, 100000000);

		final JsonObject feePayerAccount = new JsonObject();
		feePayerAccount.addProperty(REALM_NUMBER, 0);
		feePayerAccount.addProperty(SHARD_NUMBER, 0);
		feePayerAccount.addProperty(ACCOUNT_NUMBER, fromAccount);

		final JsonObject node = new JsonObject();
		node.addProperty(REALM_NUMBER, 0);
		node.addProperty(SHARD_NUMBER, 0);
		node.addProperty(ACCOUNT_NUMBER, 3);

		testJson.addProperty(FEE_PAYER_KEY_LOCATION, key.getAbsolutePath());
		testJson.add(FEE_PAYER_ACCOUNT_FIELD_NAME, feePayerAccount);
		testJson.add(TRANSACTION_FEE_FIELD_NAME, feeJson);

		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(10).asRFCString());

		testJson.add(NODE_ID_FIELD_NAME, node);
		testJson.addProperty(NETWORK_FIELD_NAME, NetworkEnum.INTEGRATION.toString());

		final JsonArray jsonArray = new JsonArray();
		final JsonObject from = new JsonObject();
		from.add(ACCOUNT, new Identifier(0, 0, fromAccount).asJSON());
		from.addProperty(AMOUNT, -tinyBars);

		final JsonObject to = new JsonObject();
		to.add(ACCOUNT, new Identifier(0, 0, toAccount).asJSON());
		to.addProperty(AMOUNT, tinyBars);
		jsonArray.add(from);
		jsonArray.add(to);

		testJson.add(TRANSFERS, jsonArray);

		final var startInstant = Instant.now().plusMillis(500);
		testJson.addProperty(TRANSACTION_VALID_START_FIELD_NAME, new Timestamp(startInstant).asRFCString());

		testJson.addProperty(TRANSACTION_VALID_DURATION_FIELD_NAME, 120);

		testJson.addProperty(MEMO_FIELD_NAME, "a memo to go with the transaction");

		return testJson;

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
		final var windowFinder = robot.robotContext().getWindowFinder();
		if (windowFinder == null) {
			return null;
		}
		final List<Window> allWindows = new ArrayList<>(windowFinder.listWindows());
		if (allWindows.isEmpty()) {
			return null;
		}

		Collections.reverse(allWindows);

		return (Stage) allWindows
				.stream()
				.filter(window -> window instanceof Stage)
				.filter(window -> ((Stage) window).getModality() == Modality.APPLICATION_MODAL)
				.findFirst()
				.orElse(null);
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
	 * Given a list of nodes that originate in a popup, find the checkbox whose text is equal to the provided legend
	 *
	 * @param popupNodes
	 * 		a list of nodes
	 * @param legend
	 * 		the text in the button
	 * @return a button
	 */
	public static CheckBox findCheckBoxInPopup(final ObservableList<Node> popupNodes, final String legend) {
		for (final var popupNode : popupNodes) {
			if (popupNode instanceof CheckBox && legend.equalsIgnoreCase(((CheckBox) popupNode).getText())) {
				return (CheckBox) popupNode;
			} else if (popupNode instanceof VBox) {
				final var f = findCheckBoxInPopup(((VBox) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof HBox) {
				final var f = findCheckBoxInPopup(((HBox) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
			} else if (popupNode instanceof GridPane) {
				final var f = findCheckBoxInPopup(((GridPane) popupNode).getChildren(), legend);
				if (f != null) {
					return f;
				}
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


	public static TextField findTextFieldInPopup(final ObservableList<Node> popupNodes) {
		for (final Node popupNode : popupNodes) {
			if (popupNode instanceof TextField) {
				return (TextField) popupNode;
			} else if (popupNode instanceof HBox) {
				return findPasswordInPopup(((HBox) popupNode).getChildren());
			} else if (popupNode instanceof VBox) {
				return findPasswordInPopup(((VBox) popupNode).getChildren());
			}
		}
		return null;
	}

	/**
	 * Transfers tinibars from one account to another
	 *
	 * @param payer
	 * 		payer account (key is genesis)
	 * @param receiver
	 * 		receover account
	 * @param amount
	 * 		the amount to be transfered
	 */
	public static void transfer(final AccountId payer, final AccountId receiver,
			final Hbar amount) throws HederaClientException,
			InterruptedException, KeyStoreException {
		final var keyStore =
				Ed25519KeyStore.read(TEST_PASSWORD.toCharArray(), "src/test/resources/KeyFiles/genesis.pem");
		final var genesisKey = PrivateKey.fromBytes(keyStore.get(0).getPrivate().getEncoded());
		final JsonObject testJson = getJsonInputCT(amount.toTinybars(), payer.num, receiver.num);
		final ToolTransferTransaction transaction = new ToolTransferTransaction(testJson);
		transaction.sign(genesisKey);
		transaction.submit();

	}


}
