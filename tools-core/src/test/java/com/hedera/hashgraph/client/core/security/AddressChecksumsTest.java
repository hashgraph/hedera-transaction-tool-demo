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

package com.hedera.hashgraph.client.core.security;

import com.hedera.hashgraph.client.core.security.AddressChecksums.parseStatus;
import org.junit.jupiter.api.Test;

import static com.hedera.hashgraph.client.core.security.AddressChecksums.parseAddress;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AddressChecksumsTest {

	@Test
	void checksum() {
		assertEquals("vfmkw", AddressChecksums.checksum("0.0.123"));
		assertTrue(parseAddress("===0.0.000123   (VFMkw)===").isValid);
		assertEquals(0, parseAddress("===0.0.000123   (VFMkw)===").num1);
		assertEquals(parseStatus.GOOD_WITH_CHECKSUM,
				parseAddress("===0.0.000123   (VFMkw)===").status);
		assertEquals(0, parseAddress("===0.0.000123   (VFMkw)===").num2);
		assertEquals(123, parseAddress("===0.0.000123   (VFMkw)===").num3);
		assertEquals("vfmkw", parseAddress("===0.0.000123   (VFMkw)===").correctChecksum);
		assertEquals("VFMkw", parseAddress("===0.0.000123   (VFMkw)===").givenChecksum);
		assertEquals("0.0.123", parseAddress("===0.0.000123   (VFMkw)===").noChecksumFormat);
		assertEquals("0.0.123(vfmkw)", parseAddress("===0.0.000123   (VFMkw)===").withChecksumFormat);

		assertEquals(
				"[isValid: true, status: GOOD_WITH_CHECKSUM, num1: 0, num2: 0, num3: 123, correctChecksum: vfmkw, " +
						"givenChecksum: VFMkw, noChecksumFormat: 0.0.123, withChecksumFormat: 0.0.123(vfmkw)]",
				parseAddress("===0.0.000123   (VFMkw)===").toString());

		assertEquals(parseStatus.GOOD_NO_CHECKSUM, parseAddress("===0.0.000123   ()===").status);
		assertEquals(parseStatus.BAD_CHECKSUM, parseAddress("0.0.123(la)").status);
		assertEquals(parseStatus.BAD_CHECKSUM, parseAddress("0.0.123(vfmkwxxx)").status);
		assertEquals(parseStatus.BAD_CHECKSUM, parseAddress("0.0.123(abcde)").status);
		assertEquals(parseStatus.BAD_FORMAT, parseAddress("0.0.123.").status);
	}

	@Test
	void getStatus_test() {
		assertEquals(parseStatus.GOOD_NO_CHECKSUM, parseAddress("===0.0.000123   ()===").getStatus());
		assertEquals(parseStatus.BAD_CHECKSUM, parseAddress("0.0.123(la)").getStatus());
		assertEquals(parseStatus.BAD_CHECKSUM, parseAddress("0.0.123(vfmkwxxx)").getStatus());
		assertEquals(parseStatus.BAD_CHECKSUM, parseAddress("0.0.123(abcde)").getStatus());
		assertEquals(parseStatus.BAD_FORMAT, parseAddress("0.0.123.").getStatus());
	}
}