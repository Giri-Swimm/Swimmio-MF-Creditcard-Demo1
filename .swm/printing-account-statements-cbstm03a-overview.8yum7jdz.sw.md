---
title: Printing Account Statements (CBSTM03A) - Overview
---
# Overview

This document describes the flow for generating credit card account statements. The process aggregates transaction records by card, retrieves customer and account information, and produces formatted statements in both text and HTML formats.

```mermaid
flowchart TD
    node1["Dispatching File Operations Based on Input Type"]:::HeadingStyle --> node2["Grouping Transactions by Card and Preparing for Next File"]:::HeadingStyle
    click node1 goToHeading "Dispatching File Operations Based on Input Type"
    click node2 goToHeading "Grouping Transactions by Card and Preparing for Next File"
    node2 --> node3{"Iterating Over Accounts and Generating Statements
(Iterating Over Accounts and Generating Statements)"}:::HeadingStyle
    click node3 goToHeading "Iterating Over Accounts and Generating Statements"
    node3 -->|"Valid account and customer data"| node4["Formatting and Writing Account Statement Output"]:::HeadingStyle
    click node4 goToHeading "Formatting and Writing Account Statement Output"
    node3 -->|"Missing data"| node5["Skip statement generation for this account"]
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- CBSTM03A (<SwmPath>[app/cbl/CBSTM03A.CBL](app/cbl/CBSTM03A.CBL)</SwmPath>)
- CBSTM03B (<SwmPath>[app/cbl/CBSTM03B.CBL](app/cbl/CBSTM03B.CBL)</SwmPath>)
- CEE3ABD

### Copybooks

- COSTM01 (<SwmPath>[app/cpy/COSTM01.CPY](app/cpy/COSTM01.CPY)</SwmPath>)
- CVACT03Y (<SwmPath>[app/cpy/CVACT03Y.cpy](app/cpy/CVACT03Y.cpy)</SwmPath>)
- CUSTREC (<SwmPath>[app/cpy/CUSTREC.cpy](app/cpy/CUSTREC.cpy)</SwmPath>)
- CVACT01Y (<SwmPath>[app/cpy/CVACT01Y.cpy](app/cpy/CVACT01Y.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  uhf1b("Creating Credit Card Account Statements (CREASTMT)") --> 7lujt("Printing Account Statements (CBSTM03A)"):::currentEntity
click uhf1b openCode "app/jcl/CREASTMT.JCL:1"
  
  
click 7lujt openCode "app/cbl/CBSTM03A.CBL:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   uhf1b("Creating Credit Card Account Statements (CREASTMT)") --> 7lujt("Printing Account Statements (CBSTM03A)"):::currentEntity
%% click uhf1b openCode "<SwmPath>[app/jcl/CREASTMT.JCL](app/jcl/CREASTMT.JCL)</SwmPath>:1"
%%   
%%   
%% click 7lujt openCode "<SwmPath>[app/cbl/CBSTM03A.CBL](app/cbl/CBSTM03A.CBL)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
