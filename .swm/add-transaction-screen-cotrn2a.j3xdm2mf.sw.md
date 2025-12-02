---
title: Add Transaction Screen (COTRN2A)
---
The Add Transaction screen (COTRN2A) allows users to input all details required to add a new transaction to the credit card system, including account/card info, transaction codes, amounts, dates, merchant details, and confirmation. It validates all fields and guides users through the process, ensuring data integrity before submission.

## Screen Preview

```
Tran: CT02           AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COTRN02C                  CardDemo                   Time: hh:mm:ss

                              Add Transaction

      Enter Acct #: ___________    (or) Card #: ________________

      ---------------------------------------------------------------

      Type CD: __  Category CD: ____  Source: ____________

      Description: ____________________________________________________________

      Amount: ____________   Orig Date: __________   Proc Date: __________
                 (-99999999.99)         (YYYY-MM-DD)      (YYYY-MM-DD)

      Merchant ID: _________   Merchant Name: ________________________________

      Merchant City: ___________________________   Merchant Zip: __________

      You are about to add this transaction. Please confirm : _ (Y/N)

[Error/Status Message Area]

ENTER=Continue  F3=Back  F4=Clear  F5=Copy Last Tran.
```

## Fields

### Transaction ID (Tran: CT02)

- Fixed value 'CT02' for this transaction, as set in the COBOL program.
- Display only, not editable by user.

### Program Name (Prog: COTRN02C)

- Fixed value 'COTRN02C', the program handling this screen.
- Display only, not editable by user.

### Date (Date: mm/dd/yy)

- Populated dynamically from system date in MM/DD/YY format.
- Display only, not editable by user.

### Time (Time: hh:mm:ss)

- Populated dynamically from system time in HH:MM:SS format.
- Display only, not editable by user.

### Title Lines (AWS Mainframe Modernization / CardDemo)

- Fixed titles for branding and context.
- Display only, not editable by user.

### Add Transaction Banner

- Fixed label 'Add Transaction'.
- Display only, not editable by user.

### Account Number (Enter Acct #: ACTIDIN)

- Input field, 11 digits, underlined.
- Must be numeric.
- If entered, Card Number is auto-populated from cross-reference file.
- Required: Either Account Number or Card Number must be entered.
- Validation: If not numeric, error message shown.

### Card Number (Card #: CARDNIN)

- Input field, 16 digits, underlined.
- Must be numeric.
- If entered, Account Number is auto-populated from cross-reference file.
- Required: Either Card Number or Account Number must be entered.
- Validation: If not numeric, error message shown.

### Type Code (Type CD: TTYPCD)

- Input field, 2 digits, underlined.
- Must be numeric.
- Required field.
- Validation: Cannot be empty or non-numeric.

### Category Code (Category CD: TCATCD)

- Input field, 4 digits, underlined.
- Must be numeric.
- Required field.
- Validation: Cannot be empty or non-numeric.

### Source (Source: TRNSRC)

- Input field, 10 characters, underlined.
- Required field.
- Validation: Cannot be empty.

### Description (Description: TDESC)

- Input field, 60 characters, underlined.
- Required field.
- Validation: Cannot be empty.

### Amount (Amount: TRNAMT)

- Input field, 12 characters, underlined.
- Required field.
- Format: Must be in form -99999999.99 (signed, two decimals).
- Validation: Cannot be empty; must match format; error if not.

### Original Date (Orig Date: TORIGDT)

- Input field, 10 characters, underlined.
- Required field.
- Format: YYYY-MM-DD.
- Validation: Cannot be empty; must match format; validated by date utility.

### Processing Date (Proc Date: TPROCDT)

- Input field, 10 characters, underlined.
- Required field.
- Format: YYYY-MM-DD.
- Validation: Cannot be empty; must match format; validated by date utility.

### Merchant ID (Merchant ID: MID)

- Input field, 9 digits, underlined.
- Required field.
- Must be numeric.
- Validation: Cannot be empty or non-numeric.

### Merchant Name (Merchant Name: MNAME)

- Input field, 30 characters, underlined.
- Required field.
- Validation: Cannot be empty.

### Merchant City (Merchant City: MCITY)

- Input field, 25 characters, underlined.
- Required field.
- Validation: Cannot be empty.

### Merchant Zip (Merchant Zip: MZIP)

- Input field, 10 characters, underlined.
- Required field.
- Validation: Cannot be empty.

### Confirmation (Confirm: CONFIRM)

- Input field, 1 character, underlined.
- Must be 'Y' or 'N' (case-insensitive).
- Required to proceed.
- Validation: If empty or invalid, error message shown.

### Error/Status Message (ERRMSG)

- 78 characters, displayed at bottom.
- Populated with error or status messages from validation or processing.
- Display only, not editable by user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
