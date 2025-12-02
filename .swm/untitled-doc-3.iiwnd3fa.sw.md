---
title: Untitled doc (3)
---
<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="241">

---

&nbsp;

```cobol
      *---------------------------------------------------------------*
       1350-WRITE-ACCT-RECORD.
           WRITE OUT-ACCT-REC.

           IF OUTFILE-STATUS NOT = '00' AND OUTFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'  OUTFILE-STATUS
              MOVE OUTFILE-STATUS  TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
      *---------------------------------------------------------------*
       1400-POPUL-ARRAY-RECORD.
           MOVE   ACCT-ID         TO   ARR-ACCT-ID.
           MOVE   ACCT-CURR-BAL   TO   ARR-ACCT-CURR-BAL(1).
           MOVE   1005.00         TO   ARR-ACCT-CURR-CYC-DEBIT(1).
           MOVE   ACCT-CURR-BAL   TO   ARR-ACCT-CURR-BAL(2).
           MOVE   1525.00         TO   ARR-ACCT-CURR-CYC-DEBIT(2).
           MOVE   -1025.00        TO   ARR-ACCT-CURR-BAL(3).
           MOVE   -2500.00        TO   ARR-ACCT-CURR-CYC-DEBIT(3).
           EXIT.
```

---

</SwmSnippet>

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
