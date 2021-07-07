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

package com.hedera.hashgraph.client.ui.utilities;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Collections.sort;

public enum CreateTransactionType {
	/**
	 * Enum that contains all possible transaction creation type
	 */
	SELECT("SELECT"),
	CREATE("Account Creation"),
	UPDATE("Account Update"),
	TRANSFER("Transfer"),
	SYSTEM("Admin Modify Content"),
	FILE_UPDATE("File Contents Update"),
	UNKNOWN("");

	private final String typeString;

	CreateTransactionType(String type) {
		this.typeString = type;
	}

	public String getTypeString() {
		return typeString;
	}

	//****** Reverse Lookup Implementation************//

	//Lookup table
	private static final Map<String, CreateTransactionType> lookup = new HashMap<>();

	//Populate the lookup table on loading time
	static {
		for (var env : CreateTransactionType.values()) {
			lookup.put(env.getTypeString(), env);
		}
	}

	//This method can be used for reverse lookup purpose
	public static CreateTransactionType get(String type) {
		if (type != null && lookup.containsKey(type)) {
			return lookup.get(type);
		}
		return lookup.get("");
	}

	public static ObservableList<String> names() {
		List<String> names = new ArrayList<>();

		var keySet = lookup.keySet();
		for (var key : keySet) {
			if (key.toUpperCase().equals(key) || "".equals(key)) {
				continue;
			}
			names.add(key);
		}
		sort(names);
		names.add(0, "SELECT");
		return FXCollections.observableArrayList(names);
	}

}
