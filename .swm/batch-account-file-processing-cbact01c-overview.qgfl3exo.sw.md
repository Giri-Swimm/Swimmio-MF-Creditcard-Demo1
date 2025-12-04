---
title: Batch Account File Processing (CBACT01C) - Overview
---
# Overview

This document describes the flow for processing credit card account records. Account data is read from an input file, transformed according to business rules, and written to multiple output files in different formats for downstream systems.

```mermaid
flowchart TD
    node1["Account File Open and Status Handling"]:::HeadingStyle
    click node1 goToHeading "Account File Open and Status Handling"
    node1 --> node2{"All files opened successfully?"}
    node2 -->|"Yes"| node3["Main Account Record Processing Loop"]:::HeadingStyle
    click node3 goToHeading "Main Account Record Processing Loop"
    node2 -->|"No"| node4["Account File Close and Status Reporting"]:::HeadingStyle
    click node4 goToHeading "Account File Close and Status Reporting"
    node3 --> node5{"More records to process?"}
    node5 -->|"Yes"| node3
    node5 -->|"No"| node4
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- CBACT01C (<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>)
- COBDATFT
- CEE3ABD

### Copybooks

- CVACT01Y (<SwmPath>[app/cpy/CVACT01Y.cpy](app/cpy/CVACT01Y.cpy)</SwmPath>)
- CODATECN (<SwmPath>[app/cpy/CODATECN.cpy](app/cpy/CODATECN.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  pb0no("READACCT") --> fjh1d("Batch Account File Processing (CBACT01C)"):::currentEntity
click pb0no openCode "app/jcl/READACCT.jcl:1"
  
  
click fjh1d openCode "app/cbl/CBACT01C.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   pb0no("READACCT") --> fjh1d("Batch Account File Processing (CBACT01C)"):::currentEntity
%% click pb0no openCode "<SwmPath>[app/jcl/READACCT.jcl](app/jcl/READACCT.jcl)</SwmPath>:1"
%%   
%%   
%% click fjh1d openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
