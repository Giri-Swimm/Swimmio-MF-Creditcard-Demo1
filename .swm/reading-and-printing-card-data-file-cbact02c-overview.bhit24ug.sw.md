---
title: Reading and Printing Card Data File (CBACT02C) - Overview
---
# Overview

This document describes the flow for reading and printing credit card records from a mainframe file. The process accesses the card data file, iterates through each record to display card information, and handles end-of-file and error conditions.

```mermaid
flowchart TD
    node1["Opening Card Data File and Handling Errors
(Opening Card Data File and Handling Errors)"]:::HeadingStyle --> node2{"Was file opened successfully?
(Opening Card Data File and Handling Errors)"}:::HeadingStyle
    click node1 goToHeading "Opening Card Data File and Handling Errors"
    click node2 goToHeading "Opening Card Data File and Handling Errors"
    node2 -->|"Yes"| node3["Main Card Data Read Loop"]:::HeadingStyle
    node2 -->|"No"| node4["Terminate process due to file open error
(Opening Card Data File and Handling Errors)"]:::HeadingStyle
    click node3 goToHeading "Main Card Data Read Loop"
    click node4 goToHeading "Opening Card Data File and Handling Errors"
    node3 --> node5{"Was record read successfully?
(Reading Next Card Record and Handling End-of-File)"}:::HeadingStyle
    click node5 goToHeading "Reading Next Card Record and Handling End-of-File"
    node5 -->|"Yes"| node3
    node5 -->|"End-of-file"| node6["Stop reading records
(Reading Next Card Record and Handling End-of-File)"]:::HeadingStyle
    node5 -->|"Error"| node7["Terminate process due to read error
(Reading Next Card Record and Handling End-of-File)"]:::HeadingStyle
    click node6 goToHeading "Reading Next Card Record and Handling End-of-File"
    click node7 goToHeading "Reading Next Card Record and Handling End-of-File"

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- CBACT02C (<SwmPath>[app/cbl/CBACT02C.cbl](app/cbl/CBACT02C.cbl)</SwmPath>)
- CEE3ABD

### Copybook

- CVACT02Y (<SwmPath>[app/cpy/CVACT02Y.cpy](app/cpy/CVACT02Y.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  9tk2j("READCARD") --> sl8jg("Reading and Printing Card Data File (CBACT02C)"):::currentEntity
click 9tk2j openCode "app/jcl/READCARD.jcl:1"
  
  
click sl8jg openCode "app/cbl/CBACT02C.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   9tk2j("READCARD") --> sl8jg("Reading and Printing Card Data File (CBACT02C)"):::currentEntity
%% click 9tk2j openCode "<SwmPath>[app/jcl/READCARD.jcl](app/jcl/READCARD.jcl)</SwmPath>:1"
%%   
%%   
%% click sl8jg openCode "<SwmPath>[app/cbl/CBACT02C.cbl](app/cbl/CBACT02C.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
