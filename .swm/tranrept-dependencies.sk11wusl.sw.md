---
title: TRANREPT - Dependencies
---
# Dependencies

```mermaid
graph TD
  
  ossoy("TRANREPT"):::currentEntity --> hlayc("REPROC")
click hlayc openCode "app/proc/REPROC.prc:1"
  
  
ossoy("TRANREPT"):::currentEntity --> vtalk("Printing Transaction Detail Report (CBTRN03C)")
click vtalk openCode "app/cbl/CBTRN03C.cbl:1"
  vtalk("Printing Transaction Detail Report (CBTRN03C)") --> v3tza("CEE3ABD")
  
  
  
  
click ossoy openCode "app/jcl/TRANREPT.jcl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   
%%   ossoy("TRANREPT"):::currentEntity --> hlayc("REPROC")
%% click hlayc openCode "<SwmPath>[app/proc/REPROC.prc](app/proc/REPROC.prc)</SwmPath>:1"
%%   
%%   
%% ossoy("TRANREPT"):::currentEntity --> vtalk("Printing Transaction Detail Report (CBTRN03C)")
%% click vtalk openCode "<SwmPath>[app/cbl/CBTRN03C.cbl](app/cbl/CBTRN03C.cbl)</SwmPath>:1"
%%   vtalk("Printing Transaction Detail Report (CBTRN03C)") --> v3tza("CEE3ABD")
%%   
%%   
%%   
%%   
%% click ossoy openCode "<SwmPath>[app/jcl/TRANREPT.jcl](app/jcl/TRANREPT.jcl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Paths

<SwmPath>[app/proc/REPROC.prc](app/proc/REPROC.prc)</SwmPath>

<SwmPath>[app/cbl/CBTRN03C.cbl](app/cbl/CBTRN03C.cbl)</SwmPath>

<SwmPath>[app/cpy/CVTRA05Y.cpy](app/cpy/CVTRA05Y.cpy)</SwmPath>

<SwmPath>[app/cpy/CVACT03Y.cpy](app/cpy/CVACT03Y.cpy)</SwmPath>

<SwmPath>[app/cpy/CVTRA03Y.cpy](app/cpy/CVTRA03Y.cpy)</SwmPath>

<SwmPath>[app/cpy/CVTRA04Y.cpy](app/cpy/CVTRA04Y.cpy)</SwmPath>

<SwmPath>[app/cpy/CVTRA07Y.cpy](app/cpy/CVTRA07Y.cpy)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
