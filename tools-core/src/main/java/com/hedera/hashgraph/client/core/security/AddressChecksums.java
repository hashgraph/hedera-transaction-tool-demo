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

import java.util.regex.Pattern;

/**
 * Static methods and classes useful for dealing with Hedera address checksums, as defined in
 * <a href="https://github.com/hashgraph/hedera-improvement-proposal/issues/35>HIP-1</a>
 */
public class AddressChecksums {
	/** regex accepting both no-checksum and with-checksum formats, with 4 capture groups: 3 numbers and a checksum */
	private static final Pattern ADDRESS_INPUT_FORMAT = Pattern.compile(
			"^[^a-zA-Z0-9.]*(\\d+)\\.(\\d+)\\.(\\d+)[^a-zA-Z0-9.]*([a-zA-Z]*)[^a-zA-Z0-9.]*$");

	/** the status of an address parsed by parseAddress */
	public enum parseStatus {
		BAD_FORMAT,         //incorrectly formatted
		BAD_CHECKSUM,      //checksum was present, but it was incorrect
		GOOD_NO_CHECKSUM,  //good no-checksum format address (no checksum was given)
		GOOD_WITH_CHECKSUM //good with-checksum format address (a correct checksum was given)
	}

	/**
	 * the result returned by {@link }#parseAddress(addr)}, including all 4 components of addr, and correct checksum
	 * .
	 */
	public static class ParsedAddress {
		/** is this a valid address? (If it's valid, then it either has a correct checksum, or no checksum) */
		boolean isValid;
		/** the status of the parsed address */
		parseStatus status;
		/** the first number in the addrss (10 in 10.20.30) */
		int num1;
		/** the second number in the addrss (20 in 10.20.30) */
		int num2;
		/** the third number in the addrss (30 in 10.20.30) */
		int num3;
		/** the checksum in the address that was parsed */
		String givenChecksum;
		/** the correct checksum */
		String correctChecksum;
		/** the address in no-checksum format */
		String noChecksumFormat;
		/** the address in with-checksum format */
		String withChecksumFormat;

		public String toString() {
			return String.format(
					"[isValid: %s, status: %s, num1: %s, num2: %s, num3: %s, correctChecksum: %s, " +
							"givenChecksum: %s, noChecksumFormat: %s, withChecksumFormat: %s]",
					isValid, status, num1, num2, num3, correctChecksum,
					givenChecksum, noChecksumFormat, withChecksumFormat);
		}
	}


	/**
	 * Given an address in either no-checksum or with-checksum format, return the components of the address, the correct
	 * checksum, and the canonical form of the address in no-checksum and with-checksum format.
	 *
	 * @return the address components, checksum, and forms
	 */
	public static ParsedAddress parseAddress(String addr) {
		var results = new ParsedAddress();
		var match = ADDRESS_INPUT_FORMAT.matcher(addr);
		if (!match.matches()) {
			results.isValid = false;
			results.status = parseStatus.BAD_FORMAT; //when status==BAD_FORMAT, the rest of the fields should be ignored
			return results;
		}
		results.num1 = Integer.parseInt(match.group(1));
		results.num2 = Integer.parseInt(match.group(2));
		results.num3 = Integer.parseInt(match.group(3));
		var ad = results.num1 + "." + results.num2 + "." + results.num3;
		var c = checksum(ad);
		final var matchGroup = match.group(4).toLowerCase();
		if ("".equals(matchGroup)) {
			results.status = parseStatus.GOOD_NO_CHECKSUM;
		} else if (c.equals(matchGroup)) {
			results.status = parseStatus.GOOD_WITH_CHECKSUM;
		} else {
			results.status = parseStatus.BAD_CHECKSUM;
		}
		results.isValid = (results.status != parseStatus.BAD_CHECKSUM);
		results.correctChecksum = c;
		results.givenChecksum = match.group(4);
		results.noChecksumFormat = ad;
		results.withChecksumFormat = ad + "(" + c + ")";
		return results;
	}

	/**
	 * Given an address like "0.0.123", return a checksum like "vfmkw" . The address must be in no-checksum format, with
	 * no extra characters (so not "0.0.00123" or "==0.0.123==" or "0.0.123-vfmkw"). The algorithm is defined by the
	 * HIP-15 standard to be:
	 *
	 * <pre>{@code
	 * a = a valid no-checksum address string, such as 0.0.123
	 * d = int array for the digits of a (using 10 to represent "."), so 0.0.123 is [0,10,0,10,1,2,3]
	 * h = unsigned byte array containing the ledger ID followed by 6 zero bytes
	 * p3 = 26 * 26 * 26
	 * p5 = 26 * 26 * 26 * 26 * 26
	 * sd0 = (d[0] + d[2] + d[4] + d[6] + ...) mod 11
	 * sd1 = (d[1] + d[3] + d[5] + d[7] + ...) mod 11
	 * sd = (...((((d[0] * 31) + d[1]) * 31) + d[2]) * 31 + ... ) * 31 + d[d.length-1]) mod p3
	 * sh = (...(((h[0] * 31) + h[1]) * 31) + h[2]) * 31 + ... ) * 31 + h[h.length-1]) mod p5
	 * c = (((d.length mod 5) * 11 + sd0) * 11 + sd1) * p3 + sd + sh ) mod p5
	 * cp = (c * 1000003) mod p5
	 * checksum = cp, written as 5 digits in base 26, using a-z
	 * }</pre>
	 *
	 * @param ledgerId
	 * 		the ledger ID for the ledger this address is on
	 * @param addr
	 * 		no-checksum address string without leading zeros or extra characters (so ==00.00.00123== becomes 0.0.123)
	 * @return the checksum
	 */
	public static String checksum(byte[] ledgerId, String addr) {
		var a = addr;      //address, such as "0.0.123"
		var d = new int[addr.length()]; //digits of address, with 10 for '.', such as [0,10,0,10,1,2,3]
		var h = ledgerId;  //ledger ID as an array of unsigned bytes
		var sd0 = 0;           //sum of even positions (mod 11)
		var sd1 = 0;           //sum of odd positions (mod 11)
		var sd = 0;            //weighted sum of all positions (mod p3)
		var sh = 0;           //hash of the ledger ID
		long c = 0;           //the checksum, before the final permutation
		long cp = 0;           //the checksum, as a single number (it's a long, to prevent overflow)
		var checksum = ""; //the answer to return
		final var p3 = 26 * 26 * 26;             //3 digits base 26
		final var p5 = 26 * 26 * 26 * 26 * 26;   //5 digits base 26
		final int ascii_0 = '0';                 //48
		final int ascii_a = 'a';                 //97
		final var m = 1_000_003;                 //min prime greater than a million. Used for the final permutation.
		final var w = 31;     //sum of digit values weights them by powers of w. Should be coprime to p5.
		for (var i = 0; i < a.length(); i++) {
			d[i] = (a.charAt(i) == '.' ? 10 : (a.charAt(i) - ascii_0));
		}
		for (var i = 0; i < d.length; i++) {
			sd = (w * sd + d[i]) % p3;
			if (i % 2 == 0) {
				sd0 = (sd0 + d[i]) % 11;
			} else {
				sd1 = (sd1 + d[i]) % 11;
			}
		}
		for (var sb : h) {
			sh = (w * sh + (sb & 0xff)) % p5; //convert signed byte to unsigned before adding
		}
		for (var i = 0; i < 6; i++) { //process 6 zeros as if they were appended to the ledger ID
			sh = (w * sh + 0) % p5;
		}
		c = ((((a.length() % 5) * 11 + sd0) * 11 + sd1) * p3 + sd + sh) % p5;
		cp = (c * m) % p5;
		for (var i = 0; i < 5; i++) {
			checksum = Character.toString(ascii_a + (int) (cp % 26)) + checksum;
			cp /= 26;
		}
		return checksum;
	}

	//Given an address like "0.0.123", return a checksum like "laujm" .
	//The address must be in no-checksum format, with no extra characters (so not "0.0.00123" or "==0.0.123==")
	public static String checksum(String addr) {
		return checksum(new byte[1], addr);
	}

}
