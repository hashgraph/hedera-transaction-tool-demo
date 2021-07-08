[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=com.hedera.hashgraph%3Ahedera-transaction-tool&metric=alert_status&token=028c36aa276e50cba3e8f765a6e709ae2336443b)](https://sonarcloud.io/dashboard?id=com.hedera.hashgraph%3Ahedera-transaction-tool)

# Hedera Transaction Tool Demo
The Hedera Transaction Tool application is a demo application that allows a user to generate keys, create transactions, 
sign transactions, and submit transactions to a Hedera network. This software is designed for use solely by the Hedera 
Council and staff. The software is being released as open source as example code only, and is not intended or suitable 
for use in its current form by anyone other than members of the Hedera Council and Hedera personnel. If you are not a 
Hedera Council member or staff member, use of this application or of the code in its current form is not recommended 
and is at your own risk.

# Install
## Requirements
* Java 14+
* MacOS (currently does not support other operating systems)

You can download the packages for the Hedera Transaction Tool UI and CLI by downloading the assets in each tag.

To run the Hedera Transaction Tool UI locally, complete the following steps:

* Clone the repository
```
git clone https://github.com/hashgraph/hedera-transaction-tool.git
```
* Open the project with your favorite IDE
* Enter the following command within the root directory of the project
```
mvn clean install
```
* Navigate to `hedera-transaction-tool/tools-ui/src/java` and run the Main class 

## Documentation
For instructions on the application installation and user guide, please visit [Hedera Transaction Tools Documentation](https://docs.hedera.com/hedera-transaction-tool-demo/)

## Contributing
Contributions are welcome. Please see the contributing guide to see how you can get involved.

## Code of Conduct
This project is governed by the Contributor Covenant Code of Conduct. By participating, you are expected to uphold this code of conduct. Please report unacceptable behavior to oss@hedera.com

## License
Apache License 2.0