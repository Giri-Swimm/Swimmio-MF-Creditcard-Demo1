---
title: Interest Calculation Batch Program (CBACT04C) - Overview
---
# Overview

This document describes the flow for calculating and posting monthly interest to credit card accounts. The process reads transaction balances, determines account changes, fetches account and rate data, calculates interest, updates balances, and records transactions for each account.

```mermaid
flowchart TD
    node1["Main Processing Loop"]:::HeadingStyle --> node2["Read Next Transaction Category Balance"]:::HeadingStyle
    click node1 goToHeading "Main Processing Loop"
    click node2 goToHeading "Read Next Transaction Category Balance"
    node2 --> node3{"Account Update and Interest Calculation
Is this a new account?
(Account Update and Interest Calculation)"}:::HeadingStyle
    click node3 goToHeading "Account Update and Interest Calculation"
    node3 -->|"Yes"| node4["Fetch Account Data"]:::HeadingStyle
    click node4 goToHeading "Fetch Account Data"
    click node4 goToHeading "Fetch Cross-Reference Data"
    click node4 goToHeading "Fetch Interest Rate Data"
    node4 --> node5{"Is interest rate available?"}
    node5 -->|"No"| node6["Default Interest Rate Retrieval"]:::HeadingStyle
    click node6 goToHeading "Default Interest Rate Retrieval"
    node5 -->|"Yes"| node7["Monthly Interest Calculation and Posting"]:::HeadingStyle
    click node7 goToHeading "Monthly Interest Calculation and Posting"
    node6 -->|"Use default rate"| node7
    node7 --> node8["Interest Transaction Record Creation"]:::HeadingStyle
    click node8 goToHeading "Interest Transaction Record Creation"
    node8 --> node9["Post Interest to Account"]:::HeadingStyle
    click node9 goToHeading "Post Interest to Account"
    node9 --> node10["Account Update After Interest Posting"]:::HeadingStyle
    click node10 goToHeading "Account Update After Interest Posting"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- CBACT04C (<SwmPath>[app/cbl/CBACT04C.cbl](app/cbl/CBACT04C.cbl)</SwmPath>)
- CEE3ABD

### Copybooks

- CVTRA01Y (<SwmPath>[app/cpy/CVTRA01Y.cpy](app/cpy/CVTRA01Y.cpy)</SwmPath>)
- CVACT03Y (<SwmPath>[app/cpy/CVACT03Y.cpy](app/cpy/CVACT03Y.cpy)</SwmPath>)
- CVTRA02Y (<SwmPath>[app/cpy/CVTRA02Y.cpy](app/cpy/CVTRA02Y.cpy)</SwmPath>)
- CVACT01Y (<SwmPath>[app/cpy/CVACT01Y.cpy](app/cpy/CVACT01Y.cpy)</SwmPath>)
- CVTRA05Y (<SwmPath>[app/cpy/CVTRA05Y.cpy](app/cpy/CVTRA05Y.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  ljv40("INTCALC") --> d3uqy("Interest Calculation Batch Program (CBACT04C)"):::currentEntity
click ljv40 openCode "app/jcl/INTCALC.jcl:1"
  
  
click d3uqy openCode "app/cbl/CBACT04C.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   ljv40("INTCALC") --> d3uqy("Interest Calculation Batch Program (CBACT04C)"):::currentEntity
%% click ljv40 openCode "<SwmPath>[app/jcl/INTCALC.jcl](app/jcl/INTCALC.jcl)</SwmPath>:1"
%%   
%%   
%% click d3uqy openCode "<SwmPath>[app/cbl/CBACT04C.cbl](app/cbl/CBACT04C.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
