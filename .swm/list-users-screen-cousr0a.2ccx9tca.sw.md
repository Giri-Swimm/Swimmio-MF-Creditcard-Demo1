---
title: List Users Screen (COUSR0A)
---
The List Users screen (COUSR0A) provides administrators with a paginated view of user records, allowing them to search for users, and select individual users for update or deletion. It is a central interface for user management in the CardDemo application.

## Screen Preview

```
Tran: CU00           AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COUSR00C                     CardDemo                   Time: hh:mm:ss

                                   List Users                Page: ________

     Search User ID: ________

     Sel  User ID       First Name             Last Name             Type
     ---  --------   --------------------     --------------------   ----
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _
     _    ________   ____________________     ____________________   _

            Type 'U' to Update or 'D' to Delete a User from the list

[Error/Status Message Area]

ENTER=Continue  F3=Back  F7=Backward  F8=Forward
```

## Fields

### Transaction ID (TRNNAME)

- Fixed value: 'CU00' (from WS-TRANID)
- Displayed at the top left
- Not editable by the user

### Program Name (PGMNAME)

- Fixed value: 'COUSR00C' (from WS-PGMNAME)
- Displayed below Transaction ID
- Not editable by the user

### Date (CURDATE)

- Format: mm/dd/yy
- Populated from system date at runtime
- Not editable by the user

### Time (CURTIME)

- Format: hh:mm:ss
- Populated from system time at runtime
- Not editable by the user

### Title (TITLE01, TITLE02)

- TITLE01: 'AWS Mainframe Modernization' (fixed)
- TITLE02: 'CardDemo' (fixed)
- Not editable by the user

### Page Number (PAGENUM)

- 8 characters, numeric
- Indicates the current page of user list
- Populated by program logic
- Not editable by the user

### Search User ID (USRIDIN)

- 8 characters, alphanumeric
- Editable by the user
- Used to filter/search for a specific User ID
- Underlined, green color
- No explicit validation in provided code, but empty value resets search

### Selection Field (SEL0001 - SEL0010)

- 1 character, editable by the user
- Underlined, green color
- Accepts 'U'/'u' for Update, 'D'/'d' for Delete
- Any other value triggers error message: 'Invalid selection. Valid values are U and D'
- Only one selection per page is processed

### User ID (USRID01 - USRID10)

- 8 characters, alphanumeric
- Populated from user records
- Not editable by the user

### First Name (FNAME01 - FNAME10)

- 20 characters, alphanumeric
- Populated from user records
- Not editable by the user

### Last Name (LNAME01 - LNAME10)

- 20 characters, alphanumeric
- Populated from user records
- Not editable by the user

### User Type (UTYPE01 - UTYPE10)

- 1 character, values: 'A' (Admin), 'U' (User)
- Populated from user records
- Not editable by the user

### Error/Status Message Area (ERRMSG)

- 78 characters
- Populated by program logic for errors, status, or instructions
- Not editable by the user
- Examples: 'Invalid key pressed. Please see below...', 'You are already at the top of the page...'

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
