---
title: Login Screen (COSGN0A)
---
The Login Screen (COSGN0A) is the entry point for users to authenticate into the CardDemo application. It collects User ID and Password, validates credentials, and provides access to mainframe modernization features upon successful sign-on.

## Screen Preview

```
Tran : CC00           AWS Mainframe Modernization           Date : mm/dd/yy
Prog : COSGN00C                 CardDemo                   Time : hh:mm:ss
AppID: ________                                             SysID: ________

      This is a Credit Card Demo Application for Mainframe Modernization

                     +========================================+
                     |%%%%%%%  NATIONAL RESERVE NOTE  %%%%%%%%|
                     |%(1)  THE UNITED STATES OF KICSLAND (1)%|
                     |%$$              ___       ********  $$%|
                     |%$    {x}       (o o)                 $%|
                     |%$     ******  (  V  )      O N E     $%|
                     |%(1)          ---m-m---             (1)%|
                     |%%~~~~~~~~~~~ ONE DOLLAR ~~~~~~~~~~~~~%%|
                     +========================================+

         Type your User ID and Password, then press ENTER:

                           User ID     : ________ (8 Char)
                           Password    : ________ (8 Char)


[Error/Status Message Area]

ENTER=Sign-on  F3=Exit
```

## Fields

### Transaction ID (Tran : CC00)

- Fixed value: 'CC00' (from WS-TRANID)
- Displayed for reference, not editable
- No validation required

### Program Name (Prog : COSGN00C)

- Fixed value: 'COSGN00C' (from WS-PGMNAME)
- Displayed for reference, not editable
- No validation required

### Date (Date : mm/dd/yy)

- Populated dynamically from system date
- Format: MM/DD/YY
- Not editable by user
- No validation required

### Time (Time : hh:mm:ss)

- Populated dynamically from system time
- Format: HH:MM:SS
- Not editable by user
- No validation required

### Application ID (AppID)

- Populated via EXEC CICS ASSIGN APPLID
- 8 characters, system assigned
- Not editable by user
- No validation required

### System ID (SysID)

- Populated via EXEC CICS ASSIGN SYSID
- 8 characters, system assigned
- Not editable by user
- No validation required

### User ID (USERID)

- Input field, 8 characters max
- Must not be blank or spaces
- Validation: Required field, error if empty
- Converted to uppercase before processing
- Used to look up user in security file
- Color: Green, underlined
- Error message shown if not found

### Password (PASSWD)

- Input field, 8 characters max
- Must not be blank or spaces
- Validation: Required field, error if empty
- Converted to uppercase before processing
- Compared against stored password for user
- Color: Green, underlined, dark attribute
- Error message shown if incorrect

### Error/Status Message Area (ERRMSG)

- Output field, up to 78 characters
- Displays validation errors or status messages
- Color: Red, bright attribute
- Populated by WS-MESSAGE in COBOL code
- Not editable by user

### Function Key Instructions

- ENTER=Sign-on: Submits credentials for validation
- F3=Exit: Exits the application and shows thank you message
- No other function keys supported; invalid keys show error

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
