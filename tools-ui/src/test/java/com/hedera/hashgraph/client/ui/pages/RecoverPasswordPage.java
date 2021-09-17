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
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import java.util.List;
import java.util.Objects;

import static com.hedera.hashgraph.client.ui.pages.TestUtil.findButtonInPopup;
import static com.hedera.hashgraph.client.ui.pages.TestUtil.getPopupNodes;
import static org.junit.Assert.assertNotNull;

public class RecoverPasswordPage {
	private final TestBase driver;

	public RecoverPasswordPage(TestBase driver) {
		this.driver = driver;
	}

	public RecoverPasswordPage setWords(List<String> words) {

		assertNotNull(words);

		for (String w : words) {
			setNextWord(w);
		}
		return this;
	}

	public RecoverPasswordPage acceptWords() {
		driver.clickOn("#recoverPhraseButton");
		return this;
	}

	public RecoverPasswordPage setNextWord(String word) {

		assertNotNull(word);
		VBox vBox = driver.find("#recoverPhraseBox");
		GridPane fullGrid = (GridPane) vBox.getChildren().get(0);
		ObservableList<Node> full = fullGrid.getChildren();
		for (Node n :
				full) {
			assert n instanceof TextField;
			if (((TextField) n).getText().isEmpty()) {
				driver.clickOn(n);
				driver.write(word);
				break;
			}
		}
		return this;
	}

	public RecoverPasswordPage clearWords() {
		VBox vBox = driver.find("#recoverPhraseBox");
		GridPane fullGrid = (GridPane) vBox.getChildren().get(0);
		ObservableList<Node> full = fullGrid.getChildren();
		for (Node n :
				full) {
			assert n instanceof TextField;
			((TextField) n).clear();
		}
		return this;

	}

	public RecoverPasswordPage enterNewPassword(String password) {
		assertNotNull(password);
		driver.clickOn("#recoverAppPasswordField");
		driver.write(password);
		driver.type(KeyCode.ENTER);
		return this;
	}

	public RecoverPasswordPage cleanPasswordOne() {
		Node n = driver.find("#recoverAppPasswordField");
		if (n instanceof PasswordField) {
			((PasswordField) n).clear();
		}
		return this;
	}

	public RecoverPasswordPage reEnterNewPassword(String password) {
		assertNotNull(password);

		driver.clickOn("#recoverReEnterPasswordField");
		driver.write(password);
		return this;
	}

	public RecoverPasswordPage cleanPasswordTwo() {
		Node n = driver.find("#recoverReEnterPasswordField");
		if (n instanceof PasswordField) {
			((PasswordField) n).clear();
		}
		return this;
	}


	public RecoverPasswordPage acceptPasswordEnter() {
		driver.clickOn("#recoverReEnterPasswordField");
		driver.type(KeyCode.ENTER);
		return this;

	}

	public RecoverPasswordPage acceptPasswordButton() {
		var node = driver.find("#recoverChangePasswordButton");
		driver.clickOn(node);
		return this;
	}

	public RecoverPasswordPage pressFinishButton() {
		driver.clickOn("#recoverFinishButton");
		return this;
	}

	public RecoverPasswordPage clickOnPopupButton(String buttonName) {
		var button = findButtonInPopup(Objects.requireNonNull(getPopupNodes()), buttonName);
		driver.clickOn(button);
		return this;
	}

	public RecoverPasswordPage clickContinue() {
		ObservableList<Node> popupNodes =
				((VBox) Objects.requireNonNull(TestUtil.getPopupNodes()).get(0)).getChildren();

		Button button = new Button();

		HBox hBox = new HBox();
		for (Node n :
				popupNodes) {
			if (n instanceof HBox) {
				hBox = (HBox) n;
			}
		}

		HBox buttons = new HBox();
		for (Node n :
				hBox.getChildren()) {
			if (n instanceof HBox) {
				buttons = (HBox) n;
			}
		}

		for (Node n :
				buttons.getChildren()) {
			if (n instanceof Button && ((Button) n).getText().equals("CONTINUE")) {
				button = (Button) n;
			}
		}

		driver.clickOn(button);
		return this;
	}

	public RecoverPasswordPage clickContinueTwo() {
		ObservableList<Node> popupNodes = TestUtil.getPopupNodes();

		Button button = new Button();

		HBox hBox = new HBox();

		assertNotNull(popupNodes);
		for (Node n :
				popupNodes) {
			if (n instanceof HBox) {
				hBox = (HBox) n;
			}
		}

		for (Node n :
				hBox.getChildren()) {
			if (n instanceof Button && "CONTINUE".equals(((Button) n).getText())) {
				button = (Button) n;
			}
		}

		driver.clickOn(button);

		return this;
	}

	public void finish(String text) {
		ObservableList<Node> popupNodes = TestUtil.getPopupNodes();
		assert popupNodes != null;
		assert popupNodes.size() == 1;
		assert popupNodes.get(0) instanceof VBox;
		VBox vBox = (VBox) popupNodes.get(0);
		HBox hBox = new HBox();
		for (Node n : vBox.getChildren()) {
			if (n instanceof HBox) {
				hBox = (HBox) n;
				break;
			}
		}
		for (Node child : hBox.getChildren()) {
			if (child instanceof Button && text.equalsIgnoreCase(((Button) child).getText())) {
				driver.clickOn(child);
				break;
			}
		}
	}

	public RecoverPasswordPage clickContinueThree() {
		ObservableList<Node> popupNodes =
				((VBox) Objects.requireNonNull(TestUtil.getPopupNodes()).get(0)).getChildren();

		Button button = new Button();

		HBox hBox = new HBox();

		for (Node n :
				popupNodes) {
			if (n instanceof HBox) {
				hBox = (HBox) n;
			}
		}

		for (Node n :
				hBox.getChildren()) {
			if (n instanceof Button && "CONTINUE".equals(((Button) n).getText())) {
				button = (Button) n;
				break;
			}
		}

		driver.clickOn(button);

		return this;
	}
}
