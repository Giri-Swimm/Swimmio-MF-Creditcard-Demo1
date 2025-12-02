---
title: Transaction Reports Screen (CORPT0A)
---
The Transaction Reports screen (CORPT0A) allows users to select and submit requests for printing transaction reports, either for the current month, year, or a custom date range. It guides users through report selection, date entry, and confirmation, ensuring correct input and providing feedback for errors.

## Screen Preview

```
Tran: CR00           AWS Mainframe Modernization          Date: mm/dd/yy
Prog: CORPT00C                 CardDemo                   Time: hh:mm:ss

                              Transaction Reports

     _ Monthly (Current Month)

     _ Yearly (Current Year)

     _ Custom (Date Range)

          Start Date : __/__/____   (MM/DD/YYYY)
          End Date   : __/__/____   (MM/DD/YYYY)

The Report will be submitted for printing. Please confirm: _ (Y/N)

[Error/Status Message Area]

ENTER=Continue  F3=Back
```

## Fields

### Transaction ID (Tran:)

- Fixed value: 'CR00' (from WS-TRANID)
- Display only, not editable by user
- Used for transaction routing in CICS

### Program Name (Prog:)

- Fixed value: 'CORPT00C' (from WS-PGMNAME)
- Display only, not editable by user
- Identifies the running program

### Date (Date:)

- Format: mm/dd/yy
- Populated from current system date
- Display only, not editable by user

### Time (Time:)

- Format: hh:mm:ss
- Populated from current system time
- Display only, not editable by user

### Title (AWS Mainframe Modernization / CardDemo)

- Fixed text from CCDA-TITLE01 and CCDA-TITLE02
- Display only, not editable by user

### Monthly Report Selection (Monthly)

- Single character input field (green, underlined)
- User marks with any non-space character to select
- Only one report type can be selected at a time
- Validation: If selected, other types should not be selected

### Yearly Report Selection (Yearly)

- Single character input field (green, underlined)
- User marks with any non-space character to select
- Only one report type can be selected at a time
- Validation: If selected, other types should not be selected

### Custom Report Selection (Custom)

- Single character input field (green, underlined)
- User marks with any non-space character to select
- Only one report type can be selected at a time
- Validation: If selected, other types should not be selected
- If selected, Start Date and End Date fields must be filled

### Start Date (Start Date :)

- Three input fields: MM (2 digits), DD (2 digits), YYYY (4 digits)
- All fields required if Custom is selected
- Each field must be numeric
- MM: 01-12, DD: 01-31, YYYY: 4 digits
- Validated for correct date (calls CSUTLDTC for date validation)
- Error messages shown if invalid or empty

### End Date (End Date :)

- Three input fields: MM (2 digits), DD (2 digits), YYYY (4 digits)
- All fields required if Custom is selected
- Each field must be numeric
- MM: 01-12, DD: 01-31, YYYY: 4 digits
- Validated for correct date (calls CSUTLDTC for date validation)
- Error messages shown if invalid or empty

### Confirmation (Please confirm: \_ (Y/N))

- Single character input field
- Required to submit report for printing
- Accepts 'Y'/'y' or 'N'/'n' (case-insensitive)
- Validation: Only Y/N accepted, error shown for other values

### Error/Status Message Area

- Up to 78 characters
- Displays error or status messages
- Populated by WS-MESSAGE
- Read-only, not editable by user

### Function Key Instructions

- Fixed text: 'ENTER=Continue  F3=Back'
- Display only, not editable by user
- Guides user for navigation

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
