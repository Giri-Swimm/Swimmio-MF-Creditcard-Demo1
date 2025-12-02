---
title: Pending Authorization Screen (COPAU0A)
---
The Pending Authorization screen (COPAU0A) provides a summary view of credit card authorization requests for a given account, allowing users to search by account ID, view customer and account details, and select specific authorizations for further review.

## Screen Preview

```
Tran: CPVS           AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COPAUS0C                            CardDemo                   Time: hh:mm:ss

                              View Authorizations

   Search Acct Id: ____________

   Name: ___________________________   Customer Id: _________
   ___________________________        Acct Status: _
   ___________________________
   PH: _____________   Approval # : ___   Decline #: ___

   Credit Lim: ____________   Cash Lim: _________   Appr Amt: __________
   Credit Bal: ____________   Cash Bal: _________   Decl Amt: __________

Sel Transaction ID     Date     Time     Type A/D STS    Amount   
--- ---------------- -------- -------- ---- --- --- ------------
_   ________________ ________ ________ ____ _   _   ____________
_   ________________ ________ ________ ____ _   _   ____________
_   ________________ ________ ________ ____ _   _   ____________
_   ________________ ________ ________ ____ _   _   ____________
_   ________________ ________ ________ ____ _   _   ____________

           Type 'S' to View Authorization details from the list

[Error/Status Message Area]

ENTER=Continue  F3=Back  F7=Backward  F8=Forward
```

## Fields

### Transaction ID (TRNNAME)

- Displayed at top left as 'Tran: CPVS'.
- Fixed value from WS-CICS-TRANID ('CPVS').
- Read-only, blue color.

### Program Name (PGMNAME)

- Displayed at top left as 'Prog: COPAUS0C'.
- Fixed value from WS-PGM-AUTH-SMRY ('COPAUS0C').
- Read-only, blue color.

### Date (CURDATE)

- Displayed at top right as 'Date: mm/dd/yy'.
- Populated from system date (WS-CURDATE-MM-DD-YY).
- Read-only, blue color.

### Time (CURTIME)

- Displayed at top right as 'Time: hh:mm:ss'.
- Populated from system time (WS-CURTIME-HH-MM-SS).
- Read-only, blue color.

### Title (TITLE01, TITLE02)

- TITLE01: 'AWS Mainframe Modernization' (fixed, yellow).
- TITLE02: 'CardDemo' (fixed, yellow).
- Read-only.

### Search Account ID (ACCTID)

- Input field, 11 characters, underlined, green color.
- Required for search; must be numeric.
- Validation: If empty or non-numeric, error message displayed.

### Name (CNAME)

- Display field, up to 25 characters, blue color.
- Populated from customer record (first, middle initial, last name).
- Read-only.

### Customer ID (CUSTID)

- Display field, 9 characters, blue color.
- Populated from customer record.
- Read-only.

### Address Line 1 (ADDR001)

- Display field, 25 characters, blue color.
- Populated from customer address line 1 and 2.
- Read-only.

### Address Line 2 (ADDR002)

- Display field, 25 characters, blue color.
- Populated from customer address line 3, state, zip.
- Read-only.

### Account Status (ACCSTAT)

- Display field, 1 character, blue color.
- Populated from account record (active status).
- Read-only.

### Phone Number (PHONE1)

- Display field, 13 characters, blue color.
- Populated from customer record (phone number 1).
- Read-only.

### Approval Count (APPRCNT)

- Display field, 3 characters, blue color.
- Populated from pending authorization summary (approved count).
- Read-only.

### Decline Count (DECLCNT)

- Display field, 3 characters, blue color.
- Populated from pending authorization summary (declined count).
- Read-only.

### Credit Limit (CREDLIM)

- Display field, 12 characters, blue color.
- Populated from account record (credit limit).
- Read-only.

### Cash Limit (CASHLIM)

- Display field, 9 characters, blue color.
- Populated from account record (cash credit limit).
- Read-only.

### Approved Amount (APPRAMT)

- Display field, 10 characters, blue color.
- Populated from pending authorization summary (approved amount).
- Read-only.

### Credit Balance (CREDBAL)

- Display field, 12 characters, blue color.
- Populated from pending authorization summary (credit balance).
- Read-only.

### Cash Balance (CASHBAL)

- Display field, 9 characters, blue color.
- Populated from pending authorization summary (cash balance).
- Read-only.

### Declined Amount (DECLAMT)

- Display field, 10 characters, blue color.
- Populated from pending authorization summary (declined amount).
- Read-only.

### Authorization List Selection (SEL0001-SEL0005)

- Input field, 1 character, underlined, green color.
- User can type 'S' to select a row for details.
- Validation: Only 'S' or 's' accepted; otherwise, error message displayed.

### Transaction List Fields (TRNID01-TRNID05, PDATE01-05, PTIME01-05, PTYPE01-05, PAPRV01-05, PSTAT01-05, PAMT001-005)

- Display fields for each authorization row (up to 5 per page):
  - Transaction ID: 16 chars
  - Date: 8 chars
  - Time: 8 chars
  - Type: 4 chars
  - Approval Status: 1 char ('A' for approved, 'D' for declined)
  - Match Status: 1 char ('P', 'D', 'E', 'M')
  - Amount: 12 chars
- All are read-only except selection field.

### Error/Status Message (ERRMSG)

- Display area, 78 characters, red color.
- Shows validation errors, system errors, or status messages.
- Populated by program logic.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
