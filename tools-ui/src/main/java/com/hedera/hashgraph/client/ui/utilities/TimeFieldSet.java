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

import com.hedera.hashgraph.client.core.json.Timestamp;
import javafx.scene.control.DateCell;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.TimeZone;

public class TimeFieldSet {
	private static final Logger logger = LogManager.getLogger(TimeFieldSet.class);
	private static final String REGEX = "[^\\d]";

	DatePicker datePicker;
	TextField hours;
	TextField minutes;
	TextField seconds;
	TextField nanos;

	HBox timeZoneBox;
	TimeZone timeZone;

	Label utcLabel;
	Label errorLabel;

	public TimeFieldSet(DatePicker datePicker, TextField hours, TextField minutes, TextField seconds,
			TextField nanos, HBox timeZoneBox, Label utcLabel, Label errorLabel) {
		this.datePicker = datePicker;
		this.hours = hours;
		this.minutes = minutes;
		this.seconds = seconds;
		this.nanos = nanos;
		this.utcLabel = utcLabel;
		this.errorLabel = errorLabel;
		this.timeZoneBox = timeZoneBox;
		if (timeZoneBox.getChildren().isEmpty()) {
			configureTimeZoneChooser();
		}

		this.timeZone = getZoneFromBox(timeZoneBox);
	}

	// region GETTERS AND SETTERS
	public DatePicker getDatePicker() {
		return datePicker;
	}

	public void setDatePicker(DatePicker datePicker) {
		this.datePicker = datePicker;
	}

	public TextField getHours() {
		return hours;
	}

	public void setHours(TextField hours) {
		this.hours = hours;
	}

	public TextField getMinutes() {
		return minutes;
	}

	public void setMinutes(TextField minutes) {
		this.minutes = minutes;
	}

	public TextField getSeconds() {
		return seconds;
	}

	public void setSeconds(TextField seconds) {
		this.seconds = seconds;
	}

	public TextField getNanos() {
		return nanos;
	}

	public void setNanos(TextField nanos) {
		this.nanos = nanos;
	}

	public TimeZone getTimeZone() {
		return timeZone;
	}

	public void setTimeZone(TimeZone timeZone) {
		this.timeZone = timeZone;
	}

	public Label getUtcLabel() {
		return utcLabel;
	}

	public void setUtcLabel(Label utcLabel) {
		this.utcLabel = utcLabel;
	}

	public Label getErrorLabel() {
		return errorLabel;
	}

	public void setErrorLabel(Label errorLabel) {
		this.errorLabel = errorLabel;
	}

	public HBox getTimeZoneBox() {
		return timeZoneBox;
	}

	public void setTimeZoneBox(HBox timeZoneBox) {
		this.timeZoneBox = timeZoneBox;
	}
// endregion

	// region METHODS

	/**
	 * Checks if there is a valid date represented by the fields
	 *
	 * @return true if a date can be built using the given information
	 */
	public boolean isDateValid() {
		var flag = true;

		final var date = datePicker.getValue();
		if (date == null) {
			logger.error("Date not set");
			flag = false;
		}

		if (!checkTimeField(hours.getText(), 24)) {
			logger.error("Invalid hours field");
			flag = false;
		}
		if (!checkTimeField(minutes.getText(), 60)) {
			logger.error("Invalid minutes field");
			flag = false;
		}
		if (!checkTimeField(seconds.getText(), 60)) {
			logger.error("Invalid seconds field");
			flag = false;
		}

		if (!checkTimeField(nanos.getText(), 1000000000)) {
			logger.error("Invalid nanos field");
			flag = false;
		}

//		final var transactionValidStart = getDate().asDate();
//
//		if (transactionValidStart.before(new Date())) {
//			logger.error("Transaction valid start in the past");
//			flag = false;
//		}
		return flag;
	}

	/**
	 * Configures the fields events.
	 *
	 * @param start
	 * 		a LocalDateTime. Dates before start will be disabled.
	 */
	public void configureDateTime(LocalDateTime start) {
		setupNumberField(hours, 24);
		setupNumberField(minutes, 60);
		setupNumberField(seconds, 60);
		setupNumberField(nanos, 1000000000);

		datePicker.setDayCellFactory(picker -> disablePastDates(start));

		datePicker.valueProperty().addListener(
				(observable, oldDate, newDate) -> setLocalDateString());

		// region FOCUS EVENTS
		removeFocusOnEnter(hours);
		removeFocusOnEnter(minutes);
		removeFocusOnEnter(seconds);
		removeFocusOnEnter(nanos);

		hours.focusedProperty().addListener(
				(arg0, oldPropertyValue, newPropertyValue) -> setChangeListener(newPropertyValue, "Hours",
						hours.getText()));
		minutes.focusedProperty().addListener(
				(arg0, oldPropertyValue, newPropertyValue) -> setChangeListener(
						newPropertyValue, "Minutes", minutes.getText()));
		seconds.focusedProperty().addListener(
				(arg0, oldPropertyValue, newPropertyValue) -> setChangeListener(
						newPropertyValue, "Seconds", seconds.getText()));
		nanos.focusedProperty().addListener(
				(arg0, oldPropertyValue, newPropertyValue) -> setChangeListener(
						newPropertyValue, "Nanos", nanos.getText()));
		datePicker.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
			if (datePicker.getValue() != null && Boolean.FALSE.equals(newPropertyValue)) {
				logger.info("Date changed to: {}", datePicker.getValue());
				setLocalDateString();
			}
		});
		datePicker.setOnKeyReleased(event -> {
			if (event.getCode().equals(KeyCode.ENTER)) {
				datePicker.getParent().requestFocus();
			}
		});
	}

	/**
	 * Checks that the textfields represent a valid date/time and refreshes the label with the UTC time
	 */
	public void setLocalDateString() {
		if (!isDateValid()) {
			return;
		}

		final var transactionValidStart = getDate().asDate();

		final var tvsInstant = transactionValidStart.toInstant();
		final var nowInstant = Instant.now();

		final var beforeNow = tvsInstant.isBefore(nowInstant);

		utcLabel.setStyle("-fx-text-fill: " + (beforeNow ? "red" : "black"));
		errorLabel.setVisible(beforeNow);

		var dateTimeFormatter =
				DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.of("UTC"));

		utcLabel.setText(dateTimeFormatter.format(tvsInstant) + " Coordinated Universal Time");
		utcLabel.setVisible(true);
	}

	/**
	 * Calculates the date represented by the fields
	 *
	 * @return a Timestamp for the date if the fields can be parsed. A zero timestamp it they cannot.
	 */
	public Timestamp getDate() {
		try {
			var hour = Integer.parseInt(hours.getText());
			var minute = Integer.parseInt(minutes.getText());
			var second = Integer.parseInt(seconds.getText());
			var nano = Integer.parseInt(nanos.getText());
			var localDateTime = LocalDateTime.of(datePicker.getValue() != null ? datePicker.getValue() :
					LocalDate.now(), LocalTime.of(hour, minute, second));
			return new Timestamp(localDateTime.atZone(timeZone.toZoneId()).toInstant()).plusNanos(nano);
		} catch (Exception e) {
			logger.error("Cannot parse local date time");
			return new Timestamp(0, 0);
		}
	}

	public LocalDateTime getLocalDateTime() {
		if (datePicker.getValue() != null) {
			var localTime =
					LocalTime.of(Integer.parseInt(hours.getText()),
							Integer.parseInt(minutes.getText()),
							Integer.parseInt(seconds.getText()));

			return LocalDateTime.of(datePicker.getValue(), localTime);
		}
		return LocalDateTime.now();
	}

	public void setDate(Instant instant) {
		final var zonedDateTime = instant.atZone(ZoneId.of(timeZone.getID()));
		hours.setText(String.format("%02d", zonedDateTime.getHour()));
		minutes.setText(String.format("%02d", zonedDateTime.getMinute()));
		seconds.setText(String.format("%02d", zonedDateTime.getSecond()));
		nanos.setText(String.format("%09d", zonedDateTime.getNano()));
		datePicker.setValue(zonedDateTime.toLocalDate());
		setLocalDateString();
	}

	public void reset(int defaultHours, int defaultMinutes, int defaultSeconds) {
		datePicker.setValue(null);
		hours.setText(String.format("%02d", defaultHours));
		minutes.setText(String.format("%02d", defaultMinutes));
		seconds.setText(String.format("%02d", defaultSeconds));
		nanos.setText("000000000");
		utcLabel.setVisible(false);
		errorLabel.setVisible(false);
		logger.info("Dare fields reset");
	}
	// endregion

	// region UTILITY
	private void configureTimeZoneChooser() {
		var chooser = new AutoCompleteNickname(ZoneId.getAvailableZoneIds());
		chooser.setDefault(TimeZone.getDefault().getID());
		timeZoneBox.getChildren().clear(); // need to clear the hox before entering the new Field.
		timeZoneBox.getChildren().add(chooser);
		chooser.textProperty().addListener((observableValue, aBoolean, t1) -> {
			var timeZones = ZoneId.getAvailableZoneIds();
			if (!timeZones.contains(chooser.getText())) {
				return;
			}
			if (datePicker.getValue() != null) {
				var instant = getDate().asInstant();
				var ldt = LocalDateTime.ofInstant(instant, ZoneId.of(chooser.getText()));
				datePicker.setValue(ldt.toLocalDate());
				hours.setText(String.valueOf(ldt.getHour()));
				minutes.setText(String.format("%02d", ldt.getMinute()));
				seconds.setText(String.format("%02d", ldt.getSecond()));
				nanos.setText(String.format("%09d", ldt.getNano()));
				setLocalDateString();
			}
			timeZone.setID(chooser.getText());
			logger.info("Timezone changed to: {}", timeZone.getID());
			setLocalDateString();
		});
	}

	private boolean checkTimeField(String value, int limit) {
		if (value.equals("")) {
			return false;
		}

		try {
			int intValue = Integer.parseInt(value);
			return intValue >= 0 && intValue < limit;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	private TimeZone getZoneFromBox(HBox timeZoneBox) {
		var zone = timeZoneBox.getChildren().get(0);
		assert zone instanceof AutoCompleteNickname;
		var zoneString = ((AutoCompleteNickname) zone).getText();
		if (!ZoneId.getAvailableZoneIds().contains(zoneString)) {
			logger.error("Zone is not available");
			return TimeZone.getDefault();
		}
		return TimeZone.getTimeZone(zoneString);
	}

	private void fixTextField(TextField hour, String newValue, String s, String regex) {
		if (!newValue.matches(s)) {
			hour.setText(newValue.replaceAll(regex, ""));
		}
	}

	private void setupNumberField(TextField timeField, int limit) {
		timeField.textProperty().addListener((observable, oldValue, newValue) -> {
			fixTextField(hours, newValue, "\\d*", REGEX);
			timeField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (Boolean.FALSE.equals(newPropertyValue)) {
					try {
						final var corrected = Math.min(Math.abs(Integer.parseInt(timeField.getText())), limit - 1);
						timeField.setText(
								limit > 100 ? String.format("%09d", corrected) : String.format("%02d", corrected));
					} catch (NumberFormatException e) {
						logger.error("Cannot parse field");
					}
				}
			});
			setLocalDateString();
		});
	}

	private DateCell disablePastDates(LocalDateTime start) {
		return new DateCell() {
			@Override
			public void updateItem(LocalDate date, boolean empty) {
				super.updateItem(date, empty);
				var localTime =
						LocalTime.of(Integer.parseInt(hours.getText()), Integer.parseInt(minutes.getText()),
								Integer.parseInt(seconds.getText()));
				var dateTime = LocalDateTime.of(date, localTime);

				setDisable(empty || dateTime.compareTo(start) <= 0);
			}
		};
	}

	private void removeFocusOnEnter(TextField field) {
		field.setOnKeyPressed(event -> {
			if (KeyCode.ENTER.equals(event.getCode())) {
				field.getParent().requestFocus();
			}
		});
	}

	private void setChangeListener(Boolean newPropertyValue, String field, String text) {
		if (datePicker.getValue() != null && Boolean.FALSE.equals(newPropertyValue)) {
			logger.info("{} text field changed to: {}", field, text);
			setLocalDateString();
		}
	}

	// endregion
}
