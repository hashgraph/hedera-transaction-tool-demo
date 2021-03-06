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

public class ResponseTuple {
	private ResponseEnum responseEnum;
	private String nickname;

	public ResponseTuple() {
		responseEnum = ResponseEnum.UNKNOWN;
		nickname = "";
	}

	public ResponseTuple(final ResponseEnum responseEnum, final String nickname) {
		this.responseEnum = responseEnum;
		this.nickname = nickname;
	}

	public ResponseEnum getResponseEnum() {
		return responseEnum;
	}

	public void setResponseEnum(final ResponseEnum responseEnum) {
		this.responseEnum = responseEnum;
	}

	public String getNickname() {
		return nickname;
	}

	public void setNickname(final String nickname) {
		this.nickname = nickname;
	}
}
