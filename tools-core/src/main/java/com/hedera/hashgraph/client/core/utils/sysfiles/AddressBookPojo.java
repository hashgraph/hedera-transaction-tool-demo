/*
 * Copyright (C) 2020-2023 Hedera Hashgraph, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hedera.hashgraph.client.core.utils.sysfiles;

import com.hedera.hashgraph.sdk.proto.NodeAddress;
import com.hedera.hashgraph.sdk.proto.NodeAddressBook;

import java.util.List;
import java.util.function.Function;

import static java.util.stream.Collectors.toList;

public class AddressBookPojo {
    private List<BookEntryPojo> entries;

    public List<BookEntryPojo> getEntries() {
        return entries;
    }

    public void setEntries(List<BookEntryPojo> entries) {
        this.entries = entries;
    }

    public static AddressBookPojo addressBookFromProto(NodeAddressBook book) {
        return from(book, BookEntryPojo::fromProto);
    }

    public static AddressBookPojo nodeDetailsFromProto(NodeAddressBook book) {
        return from(book, BookEntryPojo::fromProto);
    }

    private static AddressBookPojo from(NodeAddressBook book, Function<NodeAddress, BookEntryPojo> converter) {
        var pojo = new AddressBookPojo();
        pojo.setEntries(book.getNodeAddressList().stream().map(converter).collect(toList()));
        return pojo;
    }
}
