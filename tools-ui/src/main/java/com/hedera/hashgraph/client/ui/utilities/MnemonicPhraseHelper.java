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

package com.hedera.hashgraph.client.ui.utilities;

import com.hedera.hashgraph.client.core.action.GenericFileReadWriteAware;
import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.Dictionary;
import com.hedera.hashgraph.client.core.security.SecurityUtilities;
import com.hedera.hashgraph.client.ui.popups.MnemonicBox;
import com.hedera.hashgraph.client.ui.popups.NewPasswordPopup;
import com.hedera.hashgraph.client.ui.popups.PopupMessage;
import com.hedera.hashgraph.sdk.BadMnemonicException;
import com.hedera.hashgraph.sdk.Mnemonic;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.Clipboard;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.RowConstraints;
import javafx.scene.layout.VBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;

import static com.hedera.hashgraph.client.core.constants.Constants.KEY_LENGTH;
import static com.hedera.hashgraph.client.core.constants.Constants.SALT_LENGTH;
import static javafx.scene.control.PopupControl.USE_COMPUTED_SIZE;

public class MnemonicPhraseHelper implements GenericFileReadWriteAware {

	public static final String STYLE =
			"-fx-background-color: white;-fx-border-color: silver;-fx-background-radius: 10;" +
					"-fx-border-radius: 10;";
	private static final Logger logger = LogManager.getLogger(MnemonicPhraseHelper.class);
	private static final Dictionary dictionary = new Dictionary();

	private Mnemonic mnemonic;
	private final Label mnemonicErrorMessage;
	private final String storageDirectory;
	private final VBox phraseBox;
	private final Button generateKeys;
	private final VBox finishBox;
	GridPane mnemonicGridPane;

	public MnemonicPhraseHelper(final Label mnemonicErrorMessage, final String storageDirectory, final VBox phraseBox,
			final Button generateKeys, final VBox finishBox) {
		this.mnemonicErrorMessage = mnemonicErrorMessage;
		this.storageDirectory = storageDirectory;
		this.phraseBox = phraseBox;
		this.generateKeys = generateKeys;
		this.finishBox = finishBox;
	}

	public List<CharSequence> getWordsFromGridPane() {
		mnemonicGridPane = (GridPane) phraseBox.getChildren().get(0);
		final var nodes = mnemonicGridPane.getChildren();
		final List<CharSequence> words = new ArrayList<>();
		for (final var n : nodes) {
			final var text = (((TextField) n).getText().replace(" ", ""));
			if (!"".equals(text)) {
				words.add(text);
			}
		}

		return words;
	}

	public Mnemonic getMnemonic() {
		return mnemonic;
	}

	public boolean setWordsInGridPane(final String[] words) {
		var flag = true;
		if (words.length > Constants.MNEMONIC_SIZE) {
			throw new HederaClientRuntimeException("Incorrect number of words in mnemonic");
		}
		final var gridPane = (GridPane) phraseBox.getChildren().get(0);
		final var nodes = gridPane.getChildren();
		var counter = 0;

		if (nodes.size() != Constants.MNEMONIC_SIZE) {
			logger.error("Mnemonic pane does not have the correct number of spots");
			return false;
		}
		for (final var node : nodes) {
			assert node instanceof TextField;
			final var word = words[counter];

			final var style = "-fx-background-color: white;-fx-border-color: silver;-fx-background-radius: 10;" +
					"-fx-border-radius: 10;";
			if (spellCheck(word)) {
				node.setStyle(style + "-fx-text-fill: black");
			} else {
				node.setStyle(style + "-fx-text-fill: red");
				flag = false;
			}
			((TextField) node).setText(word);
			counter++;
		}
		return flag;
	}

	public void setupMnemonicGridPane(final GridPane mnemonicWordsGridPane, final Mnemonic mnemonic) {
		var i = 0;
		for (final var c : mnemonic.words) {
			final var t = styleTextField(c.toString().toUpperCase());
			mnemonicWordsGridPane.add(t, i % 4, i / 4);
			i++;
		}
	}

	public void generatePassphraseEvent(final boolean showPopup) {
		var words = getWordsFromGridPane();

		final var properties = new UserAccessibleProperties(storageDirectory + "/Files/user.properties", "");


		if (words.isEmpty()) {
			mnemonic = Mnemonic.generate24();
			words = mnemonic.words;
		}

		if (words.size() < Constants.MNEMONIC_SIZE) {
			mnemonicErrorMessage.setVisible(true);
			return;
		}

		try {
			mnemonic = Mnemonic.fromWords(words);
			mnemonicErrorMessage.setVisible(false);
			properties.setMnemonicHashCode(mnemonic.words.hashCode());
		} catch (final BadMnemonicException e) {
			logger.error(e);
			mnemonicErrorMessage.setVisible(true);
		}


		try {
			final var gridPane = new GridPane();
			setupFullMnemonicBoxGridPane(mnemonic, gridPane);
			phraseBox.getChildren().clear();
			phraseBox.getChildren().add(gridPane);
		} catch (final Exception e) {
			logger.error(e.getMessage());
			logger.error("Error resetting grid pane");
		}

		if (mnemonic == null) {
			logger.error("Mnemonic not generated. See log for details");
			return;
		}

		if (showPopup) {
			MnemonicBox.display("Recovery Phrase Generated",
					"A recovery phrase has been generated. Please make a copy of this phrase and store it in a safe " +
							"place.", mnemonic.toString().split(" "),
					"OK");
		}

		final var nodes = mnemonicGridPane.getChildren();
		for (final var n : nodes) {
			assert n instanceof TextField;
			n.setStyle(
					"-fx-background-color: white;-fx-background-radius: 10;-fx-border-radius: 10;-fx-border-color: " +
							"transparent");
			((TextField) n).setEditable(false);

		}

		final var password = NewPasswordPopup.display("Recovery phrase password",
				"Enter your recovery phrase password. Frequently used passwords will not be accepted.");
		try {
			if (password != null) {
				properties.setHash(password);
				final var token = properties.getHash();
				final var decoder = Base64.getDecoder();

				final var tokenBytes = decoder.decode(token);
				if (tokenBytes.length < SALT_LENGTH + KEY_LENGTH / 8) {
					logger.error("Token size check failed");
				}
				storeMnemonic(password, Arrays.copyOfRange(tokenBytes, 0, SALT_LENGTH));

				Arrays.fill(password, 'x');
			}
		} catch (final Exception e) {
			logger.error(e);
			mnemonic = null;
		}
		generateKeys.setVisible(false);
		finishBox.setVisible(true);
	}

	public void storeMnemonic(final char[] password, final byte[] salt) throws HederaClientException {
		final var passwordBytes = SecurityUtilities.keyFromPassword(password, salt);
		SecurityUtilities.toEncryptedFile(passwordBytes, storageDirectory + File.separator + Constants.MNEMONIC_PATH,
				mnemonic.toString());
	}

	private void setupFullMnemonicBoxGridPane(final Mnemonic mnemonic, final GridPane mnemonicWordsGridPane) {
		setupMnemonicGridPane(mnemonicWordsGridPane, mnemonic);
		setupMnemonicBoxConstraints(mnemonicWordsGridPane);
	}

	public void setupEmptyMnemonicBox(final GridPane mnemonicWordsGridPane) {
		for (var i = 0; i < Constants.MNEMONIC_SIZE; i++) {
			final var t = new AutoCompleteTextField() {
				@Override
				public void paste() {
					final String[] words = getWords();
					if (words.length == 1) {
						setText(words[0]);
					} else {
						pastePhraseFromClipBoard();
					}
				}
			};
			t.setMinWidth(150);
			t.setMaxWidth(150);
			t.setPrefHeight(USE_COMPUTED_SIZE);
			t.setStyle(STYLE);
			t.setStyleString(STYLE);

			t.textProperty().addListener((observableValue, s, t1) -> {
				final var words = getWordsFromGridPane();

				var valid = words.size() == Constants.MNEMONIC_SIZE;
				try {
					Mnemonic.fromWords(words);
				} catch (final BadMnemonicException e) {
					valid = false;
				}
				generateKeys.setVisible(valid);
				generateKeys.setDisable(!(valid));
				if (valid) {
					generateKeys.setText("RECOVER");
					generateKeys.requestFocus();
				} else {
					generateKeys.setText("GENERATE");
				}
			});
			mnemonicWordsGridPane.add(t, i % 4, i / 4);
		}
		setupMnemonicBoxConstraints(mnemonicWordsGridPane);
	}

	public void setupMnemonicBoxConstraints(final GridPane mnemonicWordsGridPane) {
		final var rowConstraints = new RowConstraints();
		rowConstraints.setPrefHeight(USE_COMPUTED_SIZE);
		final var columnConstraints = new ColumnConstraints();
		columnConstraints.setPrefWidth(USE_COMPUTED_SIZE);
		mnemonicWordsGridPane.getColumnConstraints().addAll(columnConstraints, columnConstraints, columnConstraints,
				columnConstraints);
		mnemonicWordsGridPane.getRowConstraints().addAll(rowConstraints, rowConstraints, rowConstraints, rowConstraints,
				rowConstraints, rowConstraints);
		mnemonicWordsGridPane.setHgap(10);
		mnemonicWordsGridPane.setVgap(10);
		mnemonicWordsGridPane.setAlignment(Pos.CENTER);
	}

	private TextField styleTextField(final CharSequence c) {
		final var t = new TextField(c.toString().toUpperCase());
		t.setMinWidth(150);
		t.setMaxWidth(150);
		t.setPrefHeight(USE_COMPUTED_SIZE);
		t.setEditable(false);
		t.setFocusTraversable(false);
		t.setStyle(
				"-fx-background-color: white;-fx-border-color: white;-fx-background-radius: 10;" +
						"-fx-border-radius: 10;-fx-font-size: 18");
		return t;
	}

	public void copyPhraseToClipBoard() {
		Toolkit.getDefaultToolkit()
				.getSystemClipboard()
				.setContents(new StringSelection(mnemonic.words.toString()),
						null);
		PopupMessage.display("Recovery phrase", "The recovery phrase has been copied to the clipboard.", "OK");
	}

	public void pastePhraseFromClipBoard() {
		mnemonicErrorMessage.setVisible(false);
		final String[] words = getWords();
		if (words.length != Constants.MNEMONIC_SIZE) {
			logger.error("Incorrect size of recovery phrase");
			mnemonicErrorMessage.setText("The recovery phase pasted does not have the right number of words.");
			mnemonicErrorMessage.setVisible(true);
			return;
		}
		if (setWordsInGridPane(words)) {
			logger.info("Words set in text boxes");
		}
	}

	@NotNull
	private String[] getWords() {
		String[] words = new String[0];
		final var clipboard = Clipboard.getSystemClipboard();
		if (clipboard.hasString()) {
			final var content =
					clipboard.getString().toLowerCase().replaceAll("[^A-Za-z0-9 ]", "").replaceAll(" +", " ");
			words = content.split(" ");
		}
		return words;
	}

	private boolean spellCheck(final String word) {
		return dictionary.valid(word);
	}

	public static final class Builder {
		private Label mnemonicErrorMessage;
		private String storageDirectory;
		private VBox phraseBox;
		private Button generateKeys;
		private VBox finishBox;

		private Builder() {
		}

		public static Builder aMnemonicPhraseHelper() {
			return new Builder();
		}

		public Builder withMnemonicErrorMessage(final Label mnemonicErrorMessage) {
			this.mnemonicErrorMessage = mnemonicErrorMessage;
			return this;
		}

		public Builder withStorageDirectory(final String storageDirectory) {
			this.storageDirectory = storageDirectory;
			return this;
		}

		public Builder withPhraseBox(final VBox phraseBox) {
			this.phraseBox = phraseBox;
			return this;
		}

		public Builder withGenerateKeys(final Button generateKeys) {
			this.generateKeys = generateKeys;
			return this;
		}

		public Builder withFinishBox(final VBox finishBox) {
			this.finishBox = finishBox;
			return this;
		}

		public MnemonicPhraseHelper build() {
			return new MnemonicPhraseHelper(mnemonicErrorMessage, storageDirectory, phraseBox, generateKeys,
					finishBox);
		}
	}
}
