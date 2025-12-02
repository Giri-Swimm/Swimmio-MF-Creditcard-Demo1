---
title: Marking Authorization Message Fraud (COPAUS2C) - Overview
---
# Overview

This document explains the flow of marking credit card authorization messages as fraudulent by capturing current timestamps and managing fraud records in the database with detailed transaction and merchant information.

```mermaid
flowchart TD
    node1["Starting the Fraud Marking Process
(Starting the Fraud Marking Process)"]:::HeadingStyle --> node2["Insert new fraud record into database
(Starting the Fraud Marking Process)"]:::HeadingStyle
    node2 --> node3{"Was insertion successful?
(Starting the Fraud Marking Process)"}:::HeadingStyle
    node3 -->|"Yes"| node4["Mark fraud addition success
(Starting the Fraud Marking Process)"]:::HeadingStyle
    node3 -->|"No - Duplicate Key"| node5["Updating Existing Fraud Records
(Updating Existing Fraud Records)"]:::HeadingStyle
    node5 --> node6{"Was update successful?
(Updating Existing Fraud Records)"}:::HeadingStyle
    node6 -->|"Yes"| node7["Mark fraud update success
(Updating Existing Fraud Records)"]:::HeadingStyle
    node6 -->|"No"| node8["Mark fraud update failure
(Updating Existing Fraud Records)"]:::HeadingStyle

    click node1 goToHeading "Starting the Fraud Marking Process"
    click node2 goToHeading "Starting the Fraud Marking Process"
    click node3 goToHeading "Starting the Fraud Marking Process"
    click node4 goToHeading "Starting the Fraud Marking Process"
    click node5 goToHeading "Updating Existing Fraud Records"
    click node6 goToHeading "Updating Existing Fraud Records"
    click node7 goToHeading "Updating Existing Fraud Records"
    click node8 goToHeading "Updating Existing Fraud Records"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- COPAUS2C (<SwmPath>[app/…/cbl/COPAUS2C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl)</SwmPath>)

### Copybooks

- SQLCA
- AUTHFRDS (<SwmPath>[app/…/dcl/AUTHFRDS.dcl](app/app-authorization-ims-db2-mq/dcl/AUTHFRDS.dcl)</SwmPath>)
- CIPAUDTY (<SwmPath>[app/…/cpy/CIPAUDTY.cpy](app/app-authorization-ims-db2-mq/cpy/CIPAUDTY.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  v53oq("Authorization Message Detail View (COPAUS1C)") --> qvnn6("Marking Authorization Message Fraud (COPAUS2C)"):::currentEntity
click v53oq openCode "app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:1"
  
  
click qvnn6 openCode "app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   v53oq("Authorization Message Detail View (COPAUS1C)") --> qvnn6("Marking Authorization Message Fraud (COPAUS2C)"):::currentEntity
%% click v53oq openCode "<SwmPath>[app/…/cbl/COPAUS1C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl)</SwmPath>:1"
%%   
%%   
%% click qvnn6 openCode "<SwmPath>[app/…/cbl/COPAUS2C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
