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


package com.hedera.hashgraph.client.core.remote;

import com.google.gson.JsonObject;
import com.hedera.hashgraph.client.core.enums.FileActions;
import com.hedera.hashgraph.client.core.enums.FileType;
import com.hedera.hashgraph.client.core.exceptions.HederaClientException;
import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
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
import org.apache.maven.artifact.versioning.ComparableVersion;
import org.jetbrains.annotations.NotNull;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
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

	public SoftwareUpdateFile(final FileDetails file) {
		super(file);
		this.version = getVersionFromFileName(file.getName());
		if (this.version == null) {
			throw new HederaClientRuntimeException("Cannot determine version");
		}
		this.timestamp = new Timestamp(file.getAttributes().creationTime().toInstant());
		this.digest = calculateDigest();
		if (this.digest.equals("")) {
			this.setValid(false);
		}
	}

	public SoftwareUpdateFile() {
		super();
		this.version = "0.0.1";
		this.setType(FileType.SOFTWARE_UPDATE);
		this.digest = "";
	}

	public void setOldVersion(final String oldVersion) {
		this.oldVersion = fixVersion(oldVersion);
	}

	public Timestamp getTimestamp() {
		return timestamp;
	}

	public void setOldStamp(final long oldStamp) {
		this.oldStamp = oldStamp;
	}

	public void setNewStamp(final long newStamp) {
		this.newStamp = newStamp;
	}

	public String getDigest() {
		return digest;
	}

	@Override
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
			return new ComparableVersion(version).compareTo(new ComparableVersion(oldVersion)) < 0;
		}
	}

	@Override
	public GridPane buildGridPane() {
		final var detailsGridPane = new GridPane();
		final List<Label> messages = new ArrayList<>();
		var count = 0;

		final var subTitleText =
				(isHistory()) ? "The application was updated to " : "This will upgrade your application to";

		var formattedText = String.format("%s Version %s", subTitleText, getVersion());

		if (isHistory()) {
			formattedText = handleHistory(formattedText);

		}
		final var subTitleLabel = new Label(formattedText);
		subTitleLabel.setWrapText(true);
		messages.add(subTitleLabel);

		//comment files will have the notes highlights
		final var notesJson = handleNotes(messages);

		for (final var message : messages) {
			detailsGridPane.add(message, 0, count++);
		}

		if (notesJson.has("link")) {
			final var hbox = handleHyperLink(notesJson);
			detailsGridPane.add(hbox, 0, count++);
		}

		final var shaDigest = new Text(digest);
		shaDigest.setFont(Font.font("Courier New", 18));


		final var shaDigestHBox = new HBox();
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
		final String onDate;
		try {
			final var metadataFile = new MetadataFile(getName());
			final var action = getLastAction(metadataFile.getMetadataActions());

			onDate = String.format(" on %s", action.getTimeStamp().asReadableLocalString());
			formattedText += onDate;
			digest = action.getUserComments();
			if (digest.length() == 0) {
				fixDigest(metadataFile, action);
			}
		} catch (final HederaClientException e) {
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
	private HBox handleHyperLink(final JsonObject notesJson) {
		final var hyperlink = new Hyperlink(notesJson.get("link").getAsString());
		final var title = new Label("Find more details at:");
		final var hbox = new HBox();
		hbox.setSpacing(20);
		hbox.setAlignment(Pos.CENTER_LEFT);
		hbox.getChildren().addAll(title, hyperlink);

		hyperlink.setOnAction(event -> {
			try {
				if (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
					Desktop.getDesktop().browse(new URI(hyperlink.getText()));
				}
			} catch (final IOException | URISyntaxException e) {
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
	private JsonObject handleNotes(final List<Label> messages) {
		final var notesJson =
				(hasComments()) ? ((CommentFile) getCommentsFile()).getContents() : new JsonObject();

		if (notesJson.has("markdownNote")) {
			final var jsonObject = notesJson.getAsJsonObject("markdownNote");
			final var titleString = (jsonObject.has("title")) ? jsonObject.get("title").getAsString() : "";
			final var contentsString = (jsonObject.has("contents")) ? jsonObject.get("contents").getAsString() : "";
			final var titleLabel = new Label(titleString + ":");
			titleLabel.setWrapText(true);
			messages.add(titleLabel);
			final var contentLabel = new Label(contentsString);
			contentLabel.setWrapText(true);
			messages.add(contentLabel);
		} else if (notesJson.has("notes")) {
			messages.add(new Label("Highlights:"));
			final var notesArray = notesJson.getAsJsonArray("notes");
			for (final var je : notesArray) {
				final var jsonObject = je.getAsJsonObject();
				final var titleString = (jsonObject.has("title")) ? jsonObject.get("title").getAsString() : "";
				final var contentsString = (jsonObject.has("contents")) ? jsonObject.get("contents").getAsString() : "";
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
	private MetadataAction getLastAction(final List<MetadataAction> metadataActions) {
		var action = metadataActions.get(0);
		var stamp = metadataActions.get(0).getTimeStamp();

		for (final var metadataAction : metadataActions) {

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
	private void fixDigest(final MetadataFile metadataFile, final MetadataAction action) {
		this.digest = calculateDigest();
		action.setUserComments(digest);
		action.setTimeStamp(action.getTimeStamp().plusNanos(1));
		metadataFile.addAction(action);
	}

	@Override
	public int compareTo(@NotNull final RemoteFile otherFile) {
		if (otherFile instanceof SoftwareUpdateFile) {
			return new ComparableVersion(version).compareTo(new ComparableVersion(((SoftwareUpdateFile) otherFile).getVersion()));
		} else {
			return super.compareTo(otherFile);
		}
	}

	private String calculateDigest() {
		final var digestString = EncryptionUtils.getFileDigest(new File(getParentPath() + File.separator + getName()));
		if ("".equals(digestString)) {
			return "";
		}
		return CommonMethods.splitStringDigest(digestString, 12);
	}

	public static String getVersionFromFileName(String fileName) {
		final var split = FilenameUtils.getBaseName(fileName).split("-(?!(alpha|beta|rc))");
		if (split.length < 2) {
			return null;
		}
		return fixVersion(split[1]);
	}

	public static String fixVersion(final String version) {
		var fixed = version.replaceAll("(beta|alpha|rc)(\\d+)", "$1.$2");
		fixed = fixed.replace(".alpha", "-alpha");
		fixed = fixed.replace(".beta", "-beta");
		fixed = fixed.replace(".rc", "-rc");
		return new ComparableVersion(fixed).getCanonical();
	}

	public static String getSoftwareVersionFromVersionStr(final String version) {
		return fixVersion(version.split(", ")[0].split(" ")[1]);
	}

	public static long getBuildDateSecondsFromVersionStr(final String version) throws HederaClientException {

		final var splitVersion = version.split(" ");
		final var formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
		final Date dateTime;
		try {
			dateTime = formatter.parse(splitVersion[3].replace(",", ""));
		} catch (final ParseException e) {
			logger.error(e);
			throw new HederaClientException(e);
		}

		return dateTime.getTime() / 1000;
	}

	@Override
	public boolean equals(final Object o) {
		if (!(o instanceof SoftwareUpdateFile)) {
			return false;
		}

		if (!super.equals(o)) {
			return false;
		}

		final var other = (SoftwareUpdateFile) o;
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
		for (final var action : actions) {
			if (!other.actions.contains(action)) {
				return false;
			}
		}
		return true;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public JsonObject toJson() {
		final var toJson = super.toJson();
		toJson.add("timestamp", timestamp.asJSON());
		toJson.addProperty("version", version);
		toJson.addProperty("digest", digest);
		return toJson;
	}

	public int compareVersion(final String otherVersion) {
		final var thisVersion = new ComparableVersion(version);
		final var other = new ComparableVersion(otherVersion);
		return thisVersion.compareTo(other);
	}

}
