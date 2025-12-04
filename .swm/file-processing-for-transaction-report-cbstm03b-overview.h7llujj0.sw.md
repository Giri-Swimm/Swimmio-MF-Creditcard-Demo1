---
title: File Processing for Transaction Report (CBSTM03B) - Overview
---
# Overview

This document explains the flow of dispatching and handling file operations for transaction reporting. The system processes requests for transaction, cross-reference, customer, and account files, performing the requested operation and updating the status accordingly.

## Dependencies

### Program

- CBSTM03B (<SwmPath>[app/cbl/CBSTM03B.CBL](app/cbl/CBSTM03B.CBL)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  yrdzf("Printing Account Statements (CBSTM03A)") --> qiqz9("File Processing for Transaction Report (CBSTM03B)"):::currentEntity
click yrdzf openCode "app/cbl/CBSTM03A.CBL:1"
  
  
click qiqz9 openCode "app/cbl/CBSTM03B.CBL:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   yrdzf("Printing Account Statements (CBSTM03A)") --> qiqz9("File Processing for Transaction Report (CBSTM03B)"):::currentEntity
%% click yrdzf openCode "<SwmPath>[app/cbl/CBSTM03A.CBL](app/cbl/CBSTM03A.CBL)</SwmPath>:1"
%%   
%%   
%% click qiqz9 openCode "<SwmPath>[app/cbl/CBSTM03B.CBL](app/cbl/CBSTM03B.CBL)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
