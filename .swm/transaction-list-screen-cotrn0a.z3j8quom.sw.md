---
title: Transaction List Screen (COTRN0A)
---
The Transaction List screen (COTRN0A) provides users with a paginated view of their credit card transactions, allowing them to search by Transaction ID, navigate through pages, and select a transaction for detailed viewing.

## Screen Preview

```
Tran: CT00                AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COTRN00C                      CardDemo                   Time: hh:mm:ss

                              List Transactions                Page: ________

     Search Tran ID:________________

  Sel  Transaction ID     Date     Description               Amount
  ---  ----------------  --------  --------------------------  ------------
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________
  _    ________________  ________  __________________________  ____________

           Type 'S' to View Transaction details from the list

[Error/Status Message Area]

ENTER=Continue  F3=Back  F7=Backward  F8=Forward
```

## Fields

### Transaction ID (TRNNAME)

- Displayed at the top left as Tran: CT00
- Fixed value from WS-TRANID ('CT00')
- Not editable by user

### Program Name (PGMNAME)

- Displayed at the top left as Prog: COTRN00C
- Fixed value from WS-PGMNAME ('COTRN00C')
- Not editable by user

### Title (TITLE01, TITLE02)

- TITLE01: 'AWS Mainframe Modernization' (fixed)
- TITLE02: 'CardDemo' (fixed)
- Not editable by user

### Date (CURDATE)

- Format: mm/dd/yy
- Populated from system date at runtime
- Not editable by user

### Time (CURTIME)

- Format: hh:mm:ss
- Populated from system time at runtime
- Not editable by user

### Page Number (PAGENUM)

- 8 characters, numeric
- Indicates current page of transactions
- Not editable by user

### Search Tran ID (TRNIDIN)

- 16 characters, underlined input field
- User can enter a Transaction ID to search
- Validation: Must be numeric (see COBOL code)
- Error message shown if not numeric

### Transaction List (SEL0001-SEL0010, TRNID01-TRNID10, TDATE01-TDATE10, TDESC01-TDESC10, TAMT001-TAMT010)

- 10 rows displayed
- SELxxxx: 1 character, underlined input field for selection (type 'S' to select)
- TRNIDxx: 16 characters, Transaction ID (populated from file)
- TDATExx: 8 characters, Date (mm/dd/yy, populated from file)
- TDESCxx: 26 characters, Description (populated from file)
- TAMTxxx: 12 characters, Amount (populated from file)
- All fields except SELxxxx are read-only
- Only one SELxxxx should be set to 'S' to select a transaction
- Validation: If SELxxxx is not blank, must be 'S' or 's'; otherwise error message

### Error/Status Message (ERRMSG)

- 78 characters
- Displays error or status messages (e.g., invalid selection, invalid key, navigation status)
- Populated by program logic

### Function Key Instructions

- Displayed at bottom of screen
- ENTER=Continue, F3=Back, F7=Backward, F8=Forward
- Not editable by user

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
