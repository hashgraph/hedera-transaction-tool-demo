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

import com.hedera.hashgraph.client.core.exceptions.HederaClientRuntimeException;
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

	private final DatePicker datePicker;
	private final TextField hours;
	private final TextField minutes;
	private final TextField seconds;
	private final TextField nanos;

	private final HBox timeZoneBox;
	private final TimeZone timeZone;

	private final Label utcLabel;
	private final Label errorLabel;

	public TimeFieldSet(final DatePicker datePicker, final TextField hours, final TextField minutes,
			final TextField seconds,
			final TextField nanos, final HBox timeZoneBox, final Label utcLabel, final Label errorLabel) {
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
		this.timeZone = getZoneFromBox();

		// Set managed for labels
		utcLabel.managedProperty().bind(utcLabel.visibleProperty());
		errorLabel.managedProperty().bind(errorLabel.visibleProperty());
	}

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
			utcLabel.setVisible(false);
			logger.error("Date not set");
			flag = false;
		}

		if (checkTimeField(hours.getText(), 24)) {
			logger.error("Invalid hours field");
			flag = false;
		}
		if (checkTimeField(minutes.getText(), 60)) {
			logger.error("Invalid minutes field");
			flag = false;
		}
		if (checkTimeField(seconds.getText(), 60)) {
			logger.error("Invalid seconds field");
			flag = false;
		}

		if (checkTimeField(nanos.getText(), 1000000000)) {
			logger.error("Invalid nanos field");
			flag = false;
		}

		return flag;
	}

	/**
	 * Configures the fields events.
	 *
	 * @param start
	 * 		a LocalDateTime. Dates before start will be disabled.
	 */
	public void configureDateTime(final LocalDateTime start) {
		setupNumberField(hours, 24);
		setupNumberField(minutes, 60);
		setupNumberField(seconds, 60);
		setupNumberField(nanos, 1000000000);

		datePicker.setDayCellFactory(picker -> disablePastDates(start));

		datePicker.valueProperty().addListener(
				(observable, oldDate, newDate) -> setUTCDateString());

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
				setUTCDateString();
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
	public void setUTCDateString() {
		if (!isDateValid()) {
			return;
		}

		final var transactionValidStart = getDate().asDate();

		final var tvsInstant = transactionValidStart.toInstant();
		final var nowInstant = Instant.now();

		final var beforeNow = tvsInstant.isBefore(nowInstant);

		utcLabel.setStyle("-fx-text-fill: " + (beforeNow ? "red" : "black"));
		errorLabel.setVisible(beforeNow);

		final var dateTimeFormatter =
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
			final var textH = hours.getText();
			final var hour = "".equals(textH) ? 0 : Integer.parseInt(textH);
			final var textM = minutes.getText();
			final var minute = "".equals(textM) ? 0 : Integer.parseInt(textM);
			final var textS = seconds.getText();
			final var second = "".equals(textS) ? 0 : Integer.parseInt(textS);
			final var textN = nanos.getText();
			final var nano = "".equals(textN) ? 0 : Integer.parseInt(textN);
			final var localDateTime = LocalDateTime.of(datePicker.getValue() != null ? datePicker.getValue() :
					LocalDate.now(), LocalTime.of(hour, minute, second));
			return new Timestamp(localDateTime.atZone(timeZone.toZoneId()).toInstant()).plusNanos(nano);
		} catch (final Exception e) {
			logger.error("Cannot parse local date time");
			return new Timestamp(0, 0);
		}
	}

	public LocalDateTime getLocalDateTime() {
		if (datePicker.getValue() != null) {
			final var localTime =
					LocalTime.of(Integer.parseInt(hours.getText()),
							Integer.parseInt(minutes.getText()),
							Integer.parseInt(seconds.getText()));

			return LocalDateTime.of(datePicker.getValue(), localTime);
		}
		return LocalDateTime.now();
	}

	public void setDate(final Instant instant) {
		final var zonedDateTime = instant.atZone(ZoneId.of(timeZone.getID()));
		hours.setText(String.format("%02d", zonedDateTime.getHour()));
		minutes.setText(String.format("%02d", zonedDateTime.getMinute()));
		seconds.setText(String.format("%02d", zonedDateTime.getSecond()));
		nanos.setText(String.format("%09d", zonedDateTime.getNano()));
		datePicker.setValue(zonedDateTime.toLocalDate());
		setUTCDateString();
	}

	public void reset(final int defaultHours, final int defaultMinutes, final int defaultSeconds) {
		datePicker.setValue(null);
		hours.setText(String.format("%02d", defaultHours));
		minutes.setText(String.format("%02d", defaultMinutes));
		seconds.setText(String.format("%02d", defaultSeconds));
		nanos.setText("000000000");
		utcLabel.setVisible(false);
		errorLabel.setVisible(false);
		logger.info("Date fields reset");
	}
	// endregion

	// region UTILITY

	/**
	 * Adds a zone chooser textfield to the hbox. The chooser is an AutoCompleteNickname textfield, where allowed texts
	 * are in the list of Java's available time zones
	 */
	private void configureTimeZoneChooser() {
		final var chooser = new AutoCompleteNickname(ZoneId.getAvailableZoneIds());
		chooser.setDefault(TimeZone.getDefault().getID());
		timeZoneBox.getChildren().clear(); // need to clear the hox before entering the new Field.
		timeZoneBox.getChildren().add(chooser);
		chooser.textProperty().addListener((observableValue, aBoolean, t1) -> {
			final var timeZones = ZoneId.getAvailableZoneIds();
			if (!timeZones.contains(chooser.getText())) {
				return;
			}
			if (datePicker.getValue() != null) {
				final var instant = getDate().asInstant();
				final var ldt = LocalDateTime.ofInstant(instant, ZoneId.of(chooser.getText()));
				datePicker.setValue(ldt.toLocalDate());
				hours.setText(String.valueOf(ldt.getHour()));
				minutes.setText(String.format("%02d", ldt.getMinute()));
				seconds.setText(String.format("%02d", ldt.getSecond()));
				nanos.setText(String.format("%09d", ldt.getNano()));
				setUTCDateString();
			}
			timeZone.setID(chooser.getText());
			logger.info("Timezone changed to: {}", timeZone.getID());
			setUTCDateString();
		});
	}

	/**
	 * Checks the value in a textfield against the limit
	 *
	 * @param value
	 * 		the value in the textfield.
	 * @param limit
	 * 		the maximum allowed value.
	 * @return true is the value is between 0 and the limit.
	 */
	private boolean checkTimeField(final String value, final int limit) {
		if (value.equals("")) {
			return true;
		}
		try {
			final int intValue = Integer.parseInt(value);
			return intValue < 0 || intValue >= limit;
		} catch (final NumberFormatException e) {
			return true;
		}
	}

	/**
	 * Extracts the timezone from the HBox.
	 *
	 * @return a timezone
	 */
	private TimeZone getZoneFromBox() {
		final var zone = timeZoneBox.getChildren().get(0);
		if (!(zone instanceof AutoCompleteNickname)) {
			throw new HederaClientRuntimeException("Unrecongized node");
		}
		final var zoneString = ((AutoCompleteNickname) zone).getText();
		if (!ZoneId.getAvailableZoneIds().contains(zoneString)) {
			logger.error("Zone is not available");
			return TimeZone.getDefault();
		}
		return TimeZone.getTimeZone(zoneString);
	}

	/**
	 * Setups a timefield. Disallows all characters other than numerics, limits the contents to positive numbers smaller
	 * than the limit.
	 *
	 * @param timeField
	 * 		the TextField that is being formatted
	 * @param limit
	 * 		the maximum allowed value for the field
	 */
	private void setupNumberField(final TextField timeField, final int limit) {
		timeField.textProperty().addListener((observable, oldValue, newValue) -> {
			if (!newValue.matches("\\d*")) {
				timeField.setText(newValue.replaceAll("[^\\d]", ""));
			}
			timeField.focusedProperty().addListener((arg0, oldPropertyValue, newPropertyValue) -> {
				if (Boolean.FALSE.equals(newPropertyValue)) {
					try {
						final var corrected = Math.min(Math.abs(Integer.parseInt(timeField.getText())), limit - 1);
						timeField.setText(
								limit > 100 ? String.format("%09d", corrected) : String.format("%02d", corrected));
					} catch (final NumberFormatException e) {
						logger.error("Cannot parse field");
					}
				}
			});
			setUTCDateString();
		});
	}

	/**
	 * Setups the dates allowed in the calendar chooser. Dates before {@code start} will show as disabled in the
	 * calendar.
	 *
	 * @param start
	 * 		the earlies possible datetime for the parameter
	 * @return a DateCell that will be disabled if before {@code start}
	 */
	private DateCell disablePastDates(final LocalDateTime start) {
		return new DateCell() {
			@Override
			public void updateItem(final LocalDate date, final boolean empty) {
				super.updateItem(date, empty);
				final var localTime =
						LocalTime.of(Integer.parseInt(hours.getText()), Integer.parseInt(minutes.getText()),
								Integer.parseInt(seconds.getText()));
				final var dateTime = LocalDateTime.of(date, localTime);

				setDisable(empty || dateTime.compareTo(start) <= 0);
			}
		};
	}

	/**
	 * Pressing the {@code ENTER} key will remove focus from the textfield
	 *
	 * @param field
	 * 		a TextField
	 */
	private void removeFocusOnEnter(final TextField field) {
		field.setOnKeyPressed(event -> {
			if (KeyCode.ENTER.equals(event.getCode())) {
				field.getParent().requestFocus();
			}
		});
	}

	/**
	 * Changes the label with the utc time when the user changes the input.
	 *
	 * @param newPropertyValue
	 * 		true if changes were made
	 * @param field
	 * 		the name of the changed field
	 * @param text
	 * 		the new value of the field
	 */
	private void setChangeListener(final Boolean newPropertyValue, final String field, final String text) {
		if (datePicker.getValue() != null && Boolean.FALSE.equals(newPropertyValue)) {
			logger.info("{} text field changed to: {}", field, text);
			setUTCDateString();
		}
	}

	// endregion
}
