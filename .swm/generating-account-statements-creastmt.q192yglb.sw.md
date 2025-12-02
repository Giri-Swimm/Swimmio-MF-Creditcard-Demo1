---
title: Generating Account Statements (CREASTMT)
---
This document explains the CREASTMT job which generates credit card account statements. It processes transaction data by resetting datasets, sorting and loading transactions, cleaning old reports, and producing new statements in text and HTML formats. The job ensures statements accurately reflect recent transactions and account details.

# Dependencies

```mermaid
graph TD
  
  z5v2y("CREASTMT"):::currentEntity --> q0qzq("Printing Account Statements (CBSTM03A)")
click q0qzq openCode "app/cbl/CBSTM03A.CBL:1"
  q0qzq("Printing Account Statements (CBSTM03A)") --> 1hjqj("CBSTM03B")
click 1hjqj openCode "app/cbl/CBSTM03B.CBL:1"
  
  
q0qzq("Printing Account Statements (CBSTM03A)") --> 0m3fo("CEE3ABD")
  
  
  
  
click z5v2y openCode "app/jcl/CREASTMT.JCL:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   z5v2y("CREASTMT"):::currentEntity --> q0qzq("Printing Account Statements (CBSTM03A)")
%% click q0qzq openCode "<SwmPath>[app/cbl/CBSTM03A.CBL](app/cbl/CBSTM03A.CBL)</SwmPath>:1"
%%   q0qzq("Printing Account Statements (CBSTM03A)") --> 1hjqj("CBSTM03B")
%% click 1hjqj openCode "<SwmPath>[app/cbl/CBSTM03B.CBL](app/cbl/CBSTM03B.CBL)</SwmPath>:1"
%%   
%%   
%% q0qzq("Printing Account Statements (CBSTM03A)") --> 0m3fo("CEE3ABD")
%%   
%%   
%%   
%%   
%% click z5v2y openCode "<SwmPath>[app/jcl/CREASTMT.JCL](app/jcl/CREASTMT.JCL)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

Here is a high level diagram of the file:

```mermaid
graph TD
a01823b6b("Reset Transaction Flow Dataset")
  click a01823b6b goToHeading "Reset Transaction Flow Dataset"
  hd3gu("IDCAMS")
        a01823b6b -.-> hd3gu
        click hd3gu openCode "app/jcl/INTRDRJ2.JCL:1"

a9b9430cc("Prepare Transaction Data for Statements")
  click a9b9430cc goToHeading "Prepare Transaction Data for Statements"
  

a9efb970d("Load Data into Transaction Flow Dataset")
  click a9efb970d goToHeading "Load Data into Transaction Flow Dataset"
  zbmh3("IDCAMS")
        a9efb970d -.-> zbmh3
        click zbmh3 openCode "app/jcl/INTRDRJ2.JCL:1"

ada68bcb4("Clean Up Old Statements")
  click ada68bcb4 goToHeading "Clean Up Old Statements"
  

a7eccf98b("Generate Account Statements")
  click a7eccf98b goToHeading "Generate Account Statements"
  pastg("CBSTM03A")
        a7eccf98b -.-> pastg
        click pastg openCode "app/cbl/CBSTM03A.CBL:1"




a01823b6b --> a9b9430cc
a9b9430cc --> a9efb970d
a9efb970d --> ada68bcb4
ada68bcb4 --> a7eccf98b
style a01823b6b color:#000000,fill:#7CB9F4
style a9b9430cc color:#000000,fill:#7CB9F4
style a9efb970d color:#000000,fill:#7CB9F4
style ada68bcb4 color:#000000,fill:#7CB9F4
style a7eccf98b color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%% a01823b6b("Reset Transaction Flow Dataset")
%%   click a01823b6b goToHeading "Reset Transaction Flow Dataset"
%%   hd3gu("IDCAMS")
%%         a01823b6b -.-> hd3gu
%%         click hd3gu openCode "<SwmPath>[app/jcl/INTRDRJ2.JCL](app/jcl/INTRDRJ2.JCL)</SwmPath>:1"
%% 
%% a9b9430cc("Prepare Transaction Data for Statements")
%%   click a9b9430cc goToHeading "Prepare Transaction Data for Statements"
%%   
%% 
%% a9efb970d("Load Data into Transaction Flow Dataset")
%%   click a9efb970d goToHeading "Load Data into Transaction Flow Dataset"
%%   zbmh3("IDCAMS")
%%         a9efb970d -.-> zbmh3
%%         click zbmh3 openCode "<SwmPath>[app/jcl/INTRDRJ2.JCL](app/jcl/INTRDRJ2.JCL)</SwmPath>:1"
%% 
%% ada68bcb4("Clean Up Old Statements")
%%   click ada68bcb4 goToHeading "Clean Up Old Statements"
%%   
%% 
%% a7eccf98b("Generate Account Statements")
%%   click a7eccf98b goToHeading "Generate Account Statements"
%%   pastg("CBSTM03A")
%%         a7eccf98b -.-> pastg
%%         click pastg openCode "<SwmPath>[app/cbl/CBSTM03A.CBL](app/cbl/CBSTM03A.CBL)</SwmPath>:1"
%% 
%% 
%% 
%% 
%% a01823b6b --> a9b9430cc
%% a9b9430cc --> a9efb970d
%% a9efb970d --> ada68bcb4
%% ada68bcb4 --> a7eccf98b
%% style a01823b6b color:#000000,fill:#7CB9F4
%% style a9b9430cc color:#000000,fill:#7CB9F4
%% style a9efb970d color:#000000,fill:#7CB9F4
%% style ada68bcb4 color:#000000,fill:#7CB9F4
%% style a7eccf98b color:#000000,fill:#7CB9F4
```

## Reset Transaction Flow Dataset

Step in this section: `DELDEF01`.

Prepares a clean transactional data source by deleting any old transaction flow files and defining a new dataset, which ensures the batch job can accurately process and generate current account statements.

1. The process deletes any previous transaction flow sequential and VSAM datasets that may contain residual data from earlier runs, ensuring removal of outdated records.
2. A new VSAM KSDS cluster is then defined, along with its data and index components, to create an empty, clean structure for upcoming transactional data.
3. As a result, at the end of this section, the transaction flow data is reset, and the system is ready to load current transactions for accurate downstream batch processing and statement generation.

### Input

**AWS.M2.CARDDEMO.TRXFL.SEQ**

Previous sequential transaction flow dataset that may contain outdated or incomplete records.

**AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS**

Previous VSAM transaction flow dataset cluster that holds keyed transaction records from previous cycles.

### Output

**AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS**

Newly defined VSAM KSDS cluster for transaction flow data, now empty and ready for new records for the current processing cycle.

**AWS.M2.CARDDEMO.TRXFL.DATA**

New data component for the transaction flow VSAM dataset ready to store transactional information.

**AWS.M2.CARDDEMO.TRXFL.INDEX**

New index component for the transaction flow VSAM dataset to enable keyed access to transaction records.

## Prepare Transaction Data for Statements

Step in this section: `STEP010`.

This section takes raw transaction data and sorts it by card number and transaction ID, creating a new dataset prepared for credit card statement production.

The section reads unsorted credit card transaction records from the input dataset. It then sorts these transactions first by card number and then by transaction ID, ensuring transactions for each card are grouped and ordered by their identifiers. The sorted and reorganized data is written out as a new sequential dataset, making it ready for downstream batch statement generation.

### Input

**AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS**

Input transaction data file containing unsorted transaction records.

### Output

**AWS.M2.CARDDEMO.TRXFL.SEQ**

Output sequential dataset of transactions sorted and reorganized by card number and transaction ID, ready for use in statement production.

## Load Data into Transaction Flow Dataset

Step in this section: `STEP020`.

This section copies prepared and sorted transaction data into the main transaction flow dataset so these records can be used for batch credit card statement production.

## Clean Up Old Statements

Step in this section: `STEP030`.

This section ensures that outdated statement report files are deleted so only the results of the current statement production are available for users and downstream processes.

## Generate Account Statements

Step in this section: `STEP040`.

This section uses up-to-date transaction, account, and customer data to produce new credit card statements for each account, available in text and HTML formats for reporting and analysis.

- For each card/account, the process:
  1. Reads all relevant transaction records from the transaction flow file, matching them to accounts via the cross-reference file.
  2. Pulls account and customer data from their respective datasets for statement details and personalization.
  3. Combines transaction, account, and customer information into a statement object.
  4. Formats each statement in text and HTML; writes the text data to the text statement dataset and the HTML data to the HTML statement dataset.

### Input

**AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS (VSAM Transaction Flow File)**

Contains sorted transaction records by card number, serving as the source for statement generation.

**AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS (Card Cross-Reference File)**

Links card numbers to account numbers and facilitates correct data matching for statements.

**AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS (Account Data File)**

Includes credit card account details used to populate account statement headers and summary.

**AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS (Customer Data File)**

Provides customer information needed for personalization and identification on statements.

### Output

[**AWS.M2.CARDDEMO.STATEMNT.PS**](http://AWS.M2.CARDDEMO.STATEMNT.PS)\*\* (Text Statement Report Dataset)\*\*

Holds newly generated account statements in a structured text format for batch reporting and archival.

**AWS.M2.CARDDEMO.STATEMNT.HTML (HTML Statement Report Dataset)**

Contains the same statements as STMTFILE, formatted as HTML documents for online delivery and integration.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
