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

/*
 * (c) 2016-2020 Swirlds, Inc.
 *
 * This software is the confidential and proprietary information of
 * Swirlds, Inc. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Swirlds.
 *
 * SWIRLDS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. SWIRLDS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

package com.hedera.hashgraph.client.core.remote;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.json.Timestamp;
import com.hedera.hashgraph.client.core.remote.helpers.FileDetails;
import com.hedera.hashgraph.client.core.remote.helpers.MetadataAction;
import com.hedera.hashgraph.client.core.utils.CommonMethods;
import com.hedera.hashgraph.client.core.utils.EncryptionUtils;
import javafx.geometry.Pos;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

public class SoftwareUpdateFile extends RemoteFile {

	private static final Logger logger = LogManager.getLogger(SoftwareUpdateFile.class);


	private Timestamp timestamp;
	private String version;
	private String digest;
	private final List<FileActions> actions = List.of(FileActions.UPDATE);


	private String oldVersion = "0.0.0";
	private long oldStamp = 0;
	private long newStamp = 0;

	public SoftwareUpdateFile(FileDetails file) {
		super(file);
		var split = FilenameUtils.getBaseName(file.getName()).split("-");
		assert split.length > 1;
		this.timestamp = new Timestamp(file.getAttributes().creationTime().toInstant());
		this.version = fixVersion(split[1]);
		this.digest = calculateDigest();
		if (this.digest.equals("")) {
			this.setValid(false);
		}
	}

	public SoftwareUpdateFile() {
		super();
		this.version = "1.0.0";
		this.setType(FileType.SOFTWARE_UPDATE);
		this.digest = "";
	}

	public void setVersion(String version) {
		this.version = fixVersion(version);
	}

	public void setOldVersion(String oldVersion) {
		this.oldVersion = fixVersion(oldVersion);
	}

	public Timestamp getTimestamp() {
		return timestamp;
	}

	public void setOldStamp(long oldStamp) {
		this.oldStamp = oldStamp;
	}

	public void setNewStamp(long newStamp) {
		this.newStamp = newStamp;
	}

	public String getDigest() {
		return digest;
	}

	public List<FileActions> getActions() {
		return actions;
	}

	public String getVersion() {
		return version;
	}

	@Override
	public boolean isExpired() {
		if (oldVersion.equals(version)) {
			return newStamp <= oldStamp;
		} else {
			var oldV = oldVersion.split("\\.");
			var newV = version.split("\\.");
			if (!oldV[0].equals(newV[0])) {
				return isOlder(oldV[0], newV[0]);
			}
			if (!oldV[1].equals(newV[1])) {
				return isOlder(oldV[1], newV[1]);
			}
			return isOlder(oldV[2], newV[2]);
		}
	}

	@Override
	public GridPane buildGridPane() {
		var detailsGridPane = new GridPane();
		List<Label> messages = new ArrayList<>();
		var count = 0;

		var subTitleText =
				(isHistory()) ? "The application was updated to " : "This will upgrade your application to";

		var formattedText = String.format("%s Version %s", subTitleText, getVersion());

		if (isHistory()) {
			formattedText = handleHistory(formattedText);

		}
		var subTitleLabel = new Label(formattedText);
		subTitleLabel.setWrapText(true);
		messages.add(subTitleLabel);

		//comment files will have the notes highlights
		JsonObject notesJson = handleNotes(messages);

		for (var message : messages) {
			detailsGridPane.add(message, 0, count++);
		}

		if (notesJson.has("link")) {
			HBox hbox = handleHyperLink(notesJson);
			detailsGridPane.add(hbox, 0, count++);
		}

		var shaDigest = new Text(digest);
		shaDigest.setFont(Font.font("Courier", 18));


		var shaDigestHBox = new HBox();
		shaDigestHBox.setAlignment(Pos.TOP_LEFT);
		final var shaLabel = new Label("File Hash:  ");

		shaDigestHBox.getChildren().addAll(shaLabel, shaDigest);
		shaDigestHBox.setPrefWidth(Region.USE_COMPUTED_SIZE);
		shaDigestHBox.setPrefHeight(Region.USE_COMPUTED_SIZE);

		detailsGridPane.add(shaDigestHBox, 0, count);
		detailsGridPane.setVgap(10);
		return detailsGridPane;
	}

	/**
	 * If the card is in history show its metadata (date of update)
	 *
	 * @param formattedText
	 * 		The string that will be displayed to the user
	 * @return The string that will be displayed to the user
	 */
	private String handleHistory(String formattedText) {
		String onDate;
		try {
			var metadataFile = new MetadataFile(getName());
			var action = getLastAction(metadataFile.getMetadataActions());

			onDate = String.format(" on %s", action.getTimeStamp().asReadableLocalString());
			formattedText += onDate;
			digest = action.getUserComments();
			if (digest.length() == 0) {
				fixDigest(metadataFile, action);
			}
		} catch (HederaClientException e) {
			logger.error(e);
		}
		return formattedText;
	}


	/**
	 * If the notes contain a Hyperlink, show it to the user
	 *
	 * @param notesJson
	 * 		the update notes
	 * @return a hyperlink to be included in the grid pane
	 */
	private HBox handleHyperLink(JsonObject notesJson) {
		var hyperlink = new Hyperlink(notesJson.get("link").getAsString());
		var title = new Label("Find more details at:");
		var hbox = new HBox();
		hbox.setSpacing(20);
		hbox.setAlignment(Pos.CENTER_LEFT);
		hbox.getChildren().addAll(title, hyperlink);

		hyperlink.setOnAction(event -> {
			try {
				if (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
					Desktop.getDesktop().browse(new URI(hyperlink.getText()));
				}
			} catch (IOException | URISyntaxException e) {
				logger.error("Cannot open web browser");
				logger.error(e);
			}
		});
		return hbox;
	}

	/**
	 * If there are any release notes, display them to the user
	 *
	 * @param messages
	 * 		the notes
	 * @return an object with notes
	 */
	private JsonObject handleNotes(List<Label> messages) {
		var notesJson =
				(hasComments()) ? ((CommentFile) getCommentsFile()).getContents() : new JsonObject();

		if (notesJson.has("notes")) {
			messages.add(new Label("Highlights:"));
			var notesArray = notesJson.getAsJsonArray("notes");
			for (var je : notesArray) {
				var jsonObject = je.getAsJsonObject();
				var titleString = (jsonObject.has("title")) ? jsonObject.get("title").getAsString() : "";
				var contentsString = (jsonObject.has("contents")) ? jsonObject.get("contents").getAsString() : "";
				messages.add(
						new Label(String.format("\t\u2022\t%s:\t%s", titleString, contentsString)));
			}
		}
		return notesJson;
	}

	/**
	 * Get the last action in the list
	 *
	 * @param metadataActions
	 * 		a list of metadata actions
	 * @return an action
	 */
	private MetadataAction getLastAction(List<MetadataAction> metadataActions) {
		var action = metadataActions.get(0);
		var stamp = metadataActions.get(0).getTimeStamp();

		for (var metadataAction : metadataActions) {

			if (metadataAction.getTimeStamp().asCalendar().after(stamp.asCalendar())) {
				stamp = metadataAction.getTimeStamp();
				action = metadataAction;
			}
		}
		return action;
	}

	/**
	 * Add the file digest to the metadata
	 *
	 * @param metadataFile
	 * 		the file update metadata
	 * @param action
	 * 		the action to be updated
	 */
	private void fixDigest(MetadataFile metadataFile, MetadataAction action) {
		this.digest = calculateDigest();
		action.setUserComments(digest);
		action.setTimeStamp(action.getTimeStamp().plusNanos(1));
		metadataFile.addAction(action);
	}

	@Override
	public int compareTo(RemoteFile otherFile) {
		if (otherFile instanceof SoftwareUpdateFile) {
			var oldV = ((SoftwareUpdateFile) otherFile).getVersion().split("\\.");
			var newV = version.split("\\.");
			if (!oldV[0].equals(newV[0])) {
				return compareToAsIntegers(oldV[0], newV[0]);
			}
			if (!oldV[1].equals(newV[1])) {
				return compareToAsIntegers(oldV[1], newV[1]);
			}
			return compareToAsIntegers(oldV[2], newV[2]);
		} else {
			return super.compareTo(otherFile);
		}
	}

	private int compareToAsIntegers(String s, String s1) {
		Integer n = Integer.parseInt(s1);
		Integer o = Integer.parseInt(s);
		return n.compareTo(o);
	}

	private String calculateDigest() {
		var digestString = EncryptionUtils.getFileDigest(new File(getParentPath() + File.separator + getName()));
		if ("".equals(digestString)) {
			return "";
		}
		return CommonMethods.splitStringDigest(digestString, 12);
	}

	private boolean isOlder(String oldV, String newV) {
		var oldInt = Integer.parseInt(oldV);
		var newInt = Integer.parseInt(newV);
		return newInt < oldInt;
	}

	private String fixVersion(String version) {
		var versionArray = version.split("\\.");
		final var fixedVersion = new String[] { "0", "0", "0" };

		for (var i = 0; i < Math.min(3, versionArray.length); i++) {
			if (!isNumeric(versionArray[i])) {
				versionArray[i] = "0";
			}
			fixedVersion[i] = versionArray[i];
		}
		return String.format("%s.%s.%s", fixedVersion[0], fixedVersion[1], fixedVersion[2]);
	}

	private boolean isNumeric(String strNum) {
		if (strNum == null || "".equals(strNum)) {
			return false;
		}
		try {
			Integer.parseInt(strNum);
		} catch (NumberFormatException nfe) {
			return false;
		}
		return true;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof SoftwareUpdateFile)) {
			return false;
		}

		if (!super.equals(o)) {
			return false;
		}

		var other = (SoftwareUpdateFile) o;
		if (!this.timestamp.equals(other.timestamp)) {
			return false;
		}
		if (!this.version.equals(other.version)) {
			return false;
		}
		if (!this.digest.equals(other.digest)) {
			return false;
		}
		if (this.actions.size() != other.actions.size()) {
			return false;
		}
		for (FileActions action : actions) {
			if (!other.actions.contains(action)) {
				return false;
			}
		}
		return true;
	}

	@Override
	public int hashCode() {
		return super.hashCode() + timestamp.hashCode() + version.hashCode() + digest.hashCode();
	}
}
