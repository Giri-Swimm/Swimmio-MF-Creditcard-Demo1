---
title: Printing Transaction Detail Report (CBTRN03C) - Overview
---
# Overview

This document explains the flow of generating a transaction detail report for credit card accounts. The process involves reading transaction records, filtering by reporting period, organizing transactions by account, type, and category, and producing a formatted report with totals and headers for business analysis.

```mermaid
flowchart TD
    node1["Reading Date Parameters"]:::HeadingStyle --> node2["Outputting Report Headers"]:::HeadingStyle
    click node1 goToHeading "Reading Date Parameters"
    click node2 goToHeading "Outputting Report Headers"
    node2 --> node3["Main Transaction Processing Loop"]:::HeadingStyle
    click node3 goToHeading "Main Transaction Processing Loop"
    node3 --> node4["Filtering and Processing Transactions
(Filtering and Processing Transactions)"]:::HeadingStyle
    click node4 goToHeading "Filtering and Processing Transactions"
    node4 --> node5{"New card/account?
(Filtering and Processing Transactions)"}:::HeadingStyle
    click node5 goToHeading "Filtering and Processing Transactions"
    node5 -->|"Yes"|node6["Writing Account Totals to Report"]:::HeadingStyle
    click node6 goToHeading "Writing Account Totals to Report"
    node5 --> node7["Card and Transaction Type Lookups"]:::HeadingStyle
    click node7 goToHeading "Card and Transaction Type Lookups"
    node7 --> node8["Writing Transaction Report Entry"]:::HeadingStyle
    click node8 goToHeading "Writing Transaction Report Entry"
    click node8 goToHeading "Writing Transaction Detail Line"
    node8 --> node9{"Page size reached?
(Handling Page Breaks and Totals)"}:::HeadingStyle
    click node9 goToHeading "Handling Page Breaks and Totals"
    node9 -->|"Yes"|node10["Writing Page Totals"]:::HeadingStyle
    click node10 goToHeading "Writing Page Totals"
    node9 -->|"No"|node3
    node10 --> node3
    node3 --> node11{"End of transactions?
(Final Totals and Cleanup)"}:::HeadingStyle
    click node11 goToHeading "Final Totals and Cleanup"
    node11 -->|"Yes"|node12["Final Totals and Cleanup
(Final Totals and Cleanup)"]:::HeadingStyle
    click node12 goToHeading "Final Totals and Cleanup"
    node11 -->|"No"|node3

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- CBTRN03C (<SwmPath>[app/cbl/CBTRN03C.cbl](app/cbl/CBTRN03C.cbl)</SwmPath>)
- CEE3ABD

### Copybooks

- CVTRA05Y (<SwmPath>[app/cpy/CVTRA05Y.cpy](app/cpy/CVTRA05Y.cpy)</SwmPath>)
- CVACT03Y (<SwmPath>[app/cpy/CVACT03Y.cpy](app/cpy/CVACT03Y.cpy)</SwmPath>)
- CVTRA03Y (<SwmPath>[app/cpy/CVTRA03Y.cpy](app/cpy/CVTRA03Y.cpy)</SwmPath>)
- CVTRA04Y (<SwmPath>[app/cpy/CVTRA04Y.cpy](app/cpy/CVTRA04Y.cpy)</SwmPath>)
- CVTRA07Y (<SwmPath>[app/cpy/CVTRA07Y.cpy](app/cpy/CVTRA07Y.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  e51ad("TRANREPT") --> el7h6("Printing Transaction Detail Report (CBTRN03C)"):::currentEntity
click e51ad openCode "app/proc/TRANREPT.prc:1"
54s9q("TRANREPT") --> el7h6("Printing Transaction Detail Report (CBTRN03C)"):::currentEntity
click 54s9q openCode "app/jcl/TRANREPT.jcl:1"
  
  
click el7h6 openCode "app/cbl/CBTRN03C.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   e51ad("TRANREPT") --> el7h6("Printing Transaction Detail Report (CBTRN03C)"):::currentEntity
%% click e51ad openCode "<SwmPath>[app/proc/TRANREPT.prc](app/proc/TRANREPT.prc)</SwmPath>:1"
%% 54s9q("TRANREPT") --> el7h6("Printing Transaction Detail Report (CBTRN03C)"):::currentEntity
%% click 54s9q openCode "<SwmPath>[app/jcl/TRANREPT.jcl](app/jcl/TRANREPT.jcl)</SwmPath>:1"
%%   
%%   
%% click el7h6 openCode "<SwmPath>[app/cbl/CBTRN03C.cbl](app/cbl/CBTRN03C.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
