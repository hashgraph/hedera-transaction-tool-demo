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

package com.hedera.hashgraph.client.ui;

import com.hedera.hashgraph.client.core.constants.Constants;
import com.hedera.hashgraph.client.core.enums.SetupPhase;
import com.hedera.hashgraph.client.core.props.UserAccessibleProperties;
import com.hedera.hashgraph.client.core.security.PasswordAuthenticator;
import com.hedera.hashgraph.client.ui.pages.MainWindowPage;
import com.hedera.hashgraph.client.ui.pages.RecoverPasswordPage;
import com.hedera.hashgraph.client.ui.pages.TestUtil;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.testfx.api.FxRobotException;
import org.testfx.api.FxToolkit;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeoutException;

import static com.hedera.hashgraph.client.ui.InitialStartupPaneControllerTest.PASSWORD;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

public class RecoverPasswordPaneControllerTest extends TestBase {

	private RecoverPasswordPage recoverPasswordPage;
	private MainWindowPage mainWindowPage;
	private UserAccessibleProperties properties;
	private final Logger logger = LogManager.getLogger(RecoverPasswordPaneControllerTest.class);

	private static final String DEFAULT_STORAGE = System.getProperty(
			"user.home") + File.separator + "Documents" + File.separator + "TransactionTools" + File.separator;

	private static final List<String> testWords =
			Arrays.asList("dignity", "domain", "involve", "report",
					"sail", "middle", "rhythm", "husband",
					"usage", "pretty", "rate", "town",
					"account", "side", "extra", "outer",
					"eagle", "eight", "design", "page",
					"regular", "bird", "race", "answer");

	@Before
	public void setUp() throws Exception {
		logger.info("Setting up for password recovery");

		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}

		if (new File(DEFAULT_STORAGE).mkdirs()) {
			logger.info("Transaction tools folder created");
		}

		FileUtils.copyFile(new File("src/test/resources/Keys/testKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/testKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/Keys/testKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/testKey.pub"));
		FileUtils.copyDirectory(new File("src/test/resources/TransactionTools-Original/Accounts"),
				new File(DEFAULT_STORAGE + "/Accounts/"));

		properties = new UserAccessibleProperties(DEFAULT_STORAGE + "/Files/user.properties", "");
		if (new File(DEFAULT_STORAGE, "Files/.System/").mkdirs()) {
			logger.info("System folder created");
		}


		properties.resetProperties();
		properties.setSetupPhase(SetupPhase.PASSWORD_RECOVERY_PHASE);
		properties.setPreferredStorageDirectory(DEFAULT_STORAGE);

		Controller controller = new Controller();
		var version = controller.getVersion();
		properties.setVersionString(version);

		File recovery = new File(DEFAULT_STORAGE, Constants.MNEMONIC_PATH);
		if (recovery.exists()) {
			if (recovery.delete()) {
				logger.info("Recovery words file deleted");
			}
		}

	}

	@Test(expected = FxRobotException.class)
	public void clickOnBogusItem_Test() throws TimeoutException {
		setStage();
		clickOn("#exterminate");
	}

	@Test
	public void enterWords_Test() throws TimeoutException {
		setStage();
		Node phraseButton = find("#recoverPhraseButton");
		assert phraseButton instanceof Button;

		assertTrue(phraseButton.isDisabled());
		assertTrue(phraseButton.isVisible());

		recoverPasswordPage.clearWords()
				.setNextWord("aaaaaaaaa");

		Node n = find("aaaaaaaaa");
		assertTrue(n.getStyle().contains("red"));

		recoverPasswordPage.clearWords();
		for (int i = 0; i < 24; i++) {
			recoverPasswordPage.setNextWord("test");
		}

		assertTrue(phraseButton.isDisabled());
		assertFalse(phraseButton.isVisible());

		recoverPasswordPage.clearWords()
				.setWords(testWords);

		assertFalse(phraseButton.isDisabled());
		assertTrue(phraseButton.isVisible());

		recoverPasswordPage.acceptWords();


		assertTrue(find("#recoverPasswordVBox").isVisible());

		assertTrue(find("#recoverAppPasswordField").isFocused());

		assertTrue(find("#recoverAppPasswordField").isVisible());
		assertFalse(find("#recoverAppPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverAppPasswordField")).getText().isEmpty());

		assertTrue(find("#recoverReEnterPasswordField").isVisible());
		assertTrue(find("#recoverReEnterPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverReEnterPasswordField")).getText().isEmpty());

		assertFalse(find("#recoverCheckPassword").isVisible());
		assertFalse(find("#recoverReCheckPassword").isVisible());
		assertFalse(find("#recoverChangePasswordButton").isVisible());

	}

	@Test
	public void enterWordsNoKeys_Test() throws TimeoutException {

		if (new File(DEFAULT_STORAGE + "/Keys/testKey.pem").delete()) {
			logger.info("Key deleted");
		}

		setStage();

		recoverPasswordPage.clearWords()
				.setWords(testWords)
				.acceptWords();


		assertTrue(find("#recoverPasswordVBox").isVisible());

		assertTrue(find("#recoverAppPasswordField").isFocused());

		assertTrue(find("#recoverAppPasswordField").isVisible());
		assertFalse(find("#recoverAppPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverAppPasswordField")).getText().isEmpty());

		assertTrue(find("#recoverReEnterPasswordField").isVisible());
		assertTrue(find("#recoverReEnterPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverReEnterPasswordField")).getText().isEmpty());

		assertFalse(find("#recoverCheckPassword").isVisible());
		assertFalse(find("#recoverReCheckPassword").isVisible());
		assertFalse(find("#recoverChangePasswordButton").isVisible());

	}

	@Test
	public void enterWordsDupKeys_Test() throws TimeoutException, IOException {

		FileUtils.copyFile(new File("src/test/resources/Keys/testKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/duplicatedIndexKey.pem"));

		setStage();

		recoverPasswordPage.clearWords()
				.setWords(testWords)
				.acceptWords();


		assertTrue(find("#recoverPasswordVBox").isVisible());

		assertTrue(find("#recoverAppPasswordField").isFocused());

		assertTrue(find("#recoverAppPasswordField").isVisible());
		assertFalse(find("#recoverAppPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverAppPasswordField")).getText().isEmpty());

		assertTrue(find("#recoverReEnterPasswordField").isVisible());
		assertTrue(find("#recoverReEnterPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverReEnterPasswordField")).getText().isEmpty());

		assertFalse(find("#recoverCheckPassword").isVisible());
		assertFalse(find("#recoverReCheckPassword").isVisible());
		assertFalse(find("#recoverChangePasswordButton").isVisible());

		recoverPasswordPage.enterNewPassword(PASSWORD)
				.reEnterNewPassword(PASSWORD)
				.acceptPasswordEnter();

		assertTrue(find("#recoverFinishVBox").isVisible());
	}

	@Test
	public void enterNewPassword_Test() throws TimeoutException {
		setStage();
		recoverPasswordPage.clearWords()
				.setWords(testWords)
				.acceptWords()
				.enterNewPassword("test");

		assertFalse(find("#recoverCheckPassword").isVisible());
		assertTrue(find("#recoverReEnterPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverReEnterPasswordField")).getText().isEmpty());

		recoverPasswordPage.cleanPasswordOne()
				.enterNewPassword(PASSWORD);

		assertTrue(find("#recoverCheckPassword").isVisible());
		assertFalse(find("#recoverReEnterPasswordField").isDisabled());

		recoverPasswordPage.reEnterNewPassword("987654321");

		assertFalse(find("#recoverReCheckPassword").isVisible());
		assertFalse(find("#recoverChangePasswordButton").isVisible());

		recoverPasswordPage.cleanPasswordTwo()
				.reEnterNewPassword(PASSWORD);

		assertTrue(find("#recoverReCheckPassword").isVisible());
		assertTrue(find("#recoverChangePasswordButton").isVisible());

		recoverPasswordPage.acceptPasswordEnter();

		assertTrue(find("#recoverFinishVBox").isVisible());
	}

	@Test
	public void passwordBehavior_test() throws TimeoutException {
		setStage();

		recoverPasswordPage.clearWords()
				.setWords(testWords)
				.acceptWords()
				.enterNewPassword("tempurasoju");

		assertTrue(find("#recoverCheckPassword").isVisible());
		assertFalse(find("#recoverReEnterPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverReEnterPasswordField")).getText().isEmpty());

		recoverPasswordPage.reEnterNewPassword("tempurasoju");
		assertTrue(find("#recoverReCheckPassword").isVisible());
		assertTrue(find("#recoverChangePasswordButton").isVisible());

		clickOn("#recoverAppPasswordField");
		type(KeyCode.BACK_SPACE);
		type(KeyCode.BACK_SPACE);
		type(KeyCode.BACK_SPACE);
		type(KeyCode.BACK_SPACE);

		assertFalse(find("#recoverCheckPassword").isVisible());
		assertTrue(find("#recoverReEnterPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverReEnterPasswordField")).getText().isEmpty());

		type(KeyCode.S);
		type(KeyCode.O);
		type(KeyCode.J);
		type(KeyCode.U);

		assertTrue(find("#recoverCheckPassword").isVisible());
		assertFalse(find("#recoverReEnterPasswordField").isDisabled());
		assertTrue(((TextField) find("#recoverReEnterPasswordField")).getText().isEmpty());
	}

	@Test
	@Ignore("weird behavior")
	public void enterNewPasswordUseButton_Test() throws TimeoutException {

		setStage();

		recoverPasswordPage.clearWords()
				.setWords(testWords)
				.acceptWords()
				.enterNewPassword(PASSWORD)
				.reEnterNewPassword(PASSWORD)
				.acceptPasswordButton();

		recoverPasswordPage.clickContinue()
				.clickContinueTwo();

		assertTrue(find("#recoverFinishVBox").isVisible());
		assertFalse(find("#recoverChangePasswordButton").isVisible());

	}

	@Test
	public void finishSetup_Test() throws TimeoutException {

		setStage();
		String oldHash = properties.getHash();

		recoverPasswordPage.clearWords()
				.setWords(testWords)
				.acceptWords()
				.enterNewPassword(PASSWORD)
				.reEnterNewPassword(PASSWORD)
				.acceptPasswordEnter()
				.pressFinishButton();

		String newHash = properties.getHash();

		assertNotEquals(oldHash, newHash);

		PasswordAuthenticator passwordAuthenticator = new PasswordAuthenticator();

		assertTrue(passwordAuthenticator.authenticate(PASSWORD.toCharArray(), newHash));

		recoverPasswordPage.finish("CONTINUE");
	}

	@Test
	public void finishSetupBadKey_Test() throws TimeoutException, IOException {


		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pem"),
				new File(DEFAULT_STORAGE + "/Keys/testKey.pem"));
		FileUtils.copyFile(new File("src/test/resources/principalTestingKey.pub"),
				new File(DEFAULT_STORAGE + "/Keys/testKey.pub"));
		setStage();
		String oldHash = properties.getHash();

		recoverPasswordPage.clearWords()
				.setWords(testWords)
				.acceptWords()
				.enterNewPassword(PASSWORD)
				.reEnterNewPassword(PASSWORD)
				.acceptPasswordEnter()
				.pressFinishButton();

		String newHash = properties.getHash();

		assertNotEquals(oldHash, newHash);

		PasswordAuthenticator passwordAuthenticator = new PasswordAuthenticator();

		assertTrue(passwordAuthenticator.authenticate(PASSWORD.toCharArray(), newHash));

		ObservableList<Node> nodes = TestUtil.getPopupNodes();
		assert nodes != null;
		assert nodes.size() == 1;
		assert nodes.get(0) instanceof VBox;
		VBox vBox = (VBox) nodes.get(0);
		Label message = new Label();
		for (Node n : vBox.getChildren()) {
			if (n instanceof Label) {
				message = (Label) n;
				break;
			}
		}

		assertEquals(
				"The application will now close. Please restart the application and log in using your new password.",
				message.getText());

		recoverPasswordPage.finish("CONTINUE");

		closeUI();

	}

	private void closeUI() throws TimeoutException {
		FxToolkit.hideStage();
		FxToolkit.cleanupStages();
	}

	private void setStage() throws TimeoutException {
		logger.info("Setting up stage");
		FxToolkit.registerPrimaryStage();
		FxToolkit.setupApplication(StartUI.class);

		// Now that the UI has started, we can set the phase to TEST
		properties.setSetupPhase(SetupPhase.TEST_PHASE);
		recoverPasswordPage = new RecoverPasswordPage(this);
	}

	@After
	public void tearDown() throws Exception {
		if (new File(DEFAULT_STORAGE).exists()) {
			FileUtils.deleteDirectory(new File(DEFAULT_STORAGE));
		}
	}
}
