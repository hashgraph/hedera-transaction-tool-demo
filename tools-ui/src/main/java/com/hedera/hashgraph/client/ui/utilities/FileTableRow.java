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

import javafx.beans.property.SimpleStringProperty;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.text.TextFlow;

public class FileTableRow extends AccountsAndTransfersTableRow {
	private SimpleStringProperty modifiedFile;

	public FileTableRow(CheckBox cb, TextFlow fileType, String description, String modifiedFile, String paidBy,
			Button sign) {
		super(cb, fileType, description, paidBy, sign);
		this.modifiedFile = new SimpleStringProperty(modifiedFile);
	}

	public String getModifiedFile() {
		return modifiedFile.get();
	}

	public void setModifiedFile(String modifiedFile) {
		this.modifiedFile.set(modifiedFile);
	}
}

