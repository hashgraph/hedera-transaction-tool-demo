
The line below generates 7 key stores at Keys/
./launch.sh generate-key -n 7 -f Keys/

The line below creates 3 accounts with information from the json file 
./launch.sh create-account -f JSON_files/createAccount.json -a 3

The line below creates an account with a key structure defined in the json file
./launch.sh create-account-with-key -f JSON_files/createAccountWithKey_ThresholdKeyOfSimpleKey.json

The line below updates an account's simple key
./launch.sh update-account -f JSON_files/updateAccountKey.json

The line below transfers hbars
./launch.sh transfer-hbars -f JSON_files/createTransfer.json

The line below updates genesis accounts in batch
./launch.sh update-keys-in-batch -f JSON_files/batchAccountKeyUpdate.json

The line below gets account info
./launch.sh get-account-info -f JSON_files/getAccountInfo.json

The line below creates an unsigned transfer transaction and saves it as a file
./launch.sh generate-unsigned-transfer-transaction -f JSON_files/generateUnsignedTransfer.json

The line below creates an unsigned update key transaction and saves it as a file
./launch.sh generate-unsigned-update-key-transaction -f JSON_files/generateUnsignedUpdateKeyTransaction_thresholdKey.json

The line below generates a signature for an unsigned transfer transaction and saves the signature as a file
./launch.sh sign-transaction-with-key -f JSON_files/generateSignatureFile_1019.json

The line below builds a multi-sig for an unsigned transfer transaction and saves the signature as a file
./launch.sh build-multisig -f JSON_files/buildMultiSigFile_ThresholdSigOfSimpleSig.json

The line below signs an transaction with signature files and generates a signed transaction file
./launch.sh sign-transaction-with-sigs -f JSON_files/signTransactionWithFile.json

The line below submits a signed transaction file to the network
./launch.sh submit -f Transactions/updateAccountTransaction.txsig

The line below shows content of a transaction file
./launch.sh show-transaction -f Transactions/updateAccountTransaction.txsig

The line below generates, signs and submits a FreezeTransaction 
./launch.sh freeze -f JSON_files/freeze.json

The line below submit (round-robin) Queries for getting Account's balance to all nodes in the network
./launch.sh proxy-test-get-balance -p <accountNum of the fee payer, e.g., 0.0.2 or 2> -f <accountNum of the first Account to get balance> -l <accountNum of the last Account to get balance> -k <the .pem file to sign the transaction> -x <password> 

-------------Update System File----------------

The line below gets a File's content
./launch.sh get-file-contents -f JSON_files/getFileContent.json -o <path to output file>

The line below converts a AddressBook json file to a protobuf message file 
java -cp tools-cli.jar:lib/* com.hedera.client.tools.utils.AddressBookToProto <path to JSON file>  <path to output proto binary file>

The line below converts a FeeSchedule json file to a protobuf message file 
./launch.sh exchange-rate-json-to-proto -i <path to JSON file> -o <path to output proto binary file>

The line below converts a ExchangeRate json file to a protobuf message file 
./launch.sh fee-json-to-proto -i <path to JSON file> -o <path to output proto binary file>

The line below parameters description creates, signs, and submits FileCreate or FileUpdate(& FileAppend) Transactions for updating file. 
Parameters Description: 
-m	Operational mode (CREATE, OVERWRITE, or APPEND)
-i	The network fileNum to be updated, e.g., 0.0.101 or 101
-p	The accountNum of the fee payer, e.g., 0.0.2 or 2
-n	The nodeNum to which the transaction is to be submitted,  e.g., 0.0.3 or 3
-k	The .pem file to sign the transaction
-f	The file to be uploaded
-a	The maximum fee payment to be sent with the transaction(s)
-t	Transaction valid start time (in UTC seconds)
-c	The memo to be included with the transactions 
-x	The password used to decrypt the keystore

./launch.sh upload-file -m <mode: CREATE, OVERWRITE, or APPEND> -i <fileID, e.g., 0.0.101 or 101> -p <accountNum of the fee payer, e.g., 0.0.2 or 2> -n <nodeAccountID, 0.0.3 or 3> -k <the .pem file to sign the transaction> -f <the file to be uploaded> -a <the fee payment to be sent> -t <transaction valid start> -c <memo> -x <password>

------Update System File with Multisig------

1. Generate unsigned transaction

The line below parameters description creates unsigned FileCreate or FileUpdate(& FileAppend) Transactions for updating file. 
Parameters Description: 
-u	Only Generate unsigned Transaction
-m	Operational mode (CREATE, OVERWRITE, or APPEND)
-i	The network fileNum to be updated, e.g., 0.0.101 or 101
-p	The accountNum of the fee payer, e.g., 0.0.2 or 2
-n	The nodeNum to which the transaction is to be submitted,  e.g., 0.0.3 or 3
-f	The file to be uploaded
-a	The maximum fee payment to be sent with the transaction(s)
-t	Transaction valid start time (in UTC seconds)
-c	The memo to be included with the transactions 
-d	Save unsigned Transaction to this directory

./launch.sh upload-file -u -m <mode: CREATE, OVERWRITE, or APPEND> -i <fileID, e.g., 0.0.101 or 101> -p <accountNum of the fee payer, e.g., 0.0.2 or 2> -n <nodeAccountID, 0.0.3 or 3> -f <the file to be uploaded> -a <the fee payment to be sent> -t <transaction valid start> -c <memo> -d <directory>

2. Generate signatures

./launch.sh generate-signature -t <transaction file path> -n <signature file path> -k <keystore file path>

3. Sign the transaction with sigpairs
./launch.sh sign-transaction-with-sigs -t <transaction file path> -d <directory with sig files> 

4. Submit signed transaction
./launch.sh submit -f <signed transaction file path>
