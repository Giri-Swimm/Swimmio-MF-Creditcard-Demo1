---
title: Update User Screen (COUSR2A)
---
The Update User screen (COUSR2A) allows administrators to fetch and update user details in the CardDemo application. It provides fields for entering and modifying user information, with validation and clear instructions for saving or cancelling changes.

## Screen Preview

```
Tran: CU02           AWS Mainframe Modernization                Date: mm/dd/yy
Prog: COUSR02C                        CardDemo                   Time: hh:mm:ss

                                   Update User

      Enter User ID: ________

      **********************************************************************

      First Name: ____________________   Last Name: ____________________

      Password: ________   (8 Char)

      User Type: _   (A=Admin, U=User)



[Error/Status Message Area]

ENTER=Fetch  F3=Save&Exit  F4=Clear  F5=Save  F12=Cancel
```

## Fields

### Transaction ID (Tran: CU02)

- Fixed value: 'CU02' (from WS-TRANID)
- Displayed at top left
- No user input or validation

### Program Name (Prog: COUSR02C)

- Fixed value: 'COUSR02C' (from WS-PGMNAME)
- Displayed at top left, second line
- No user input or validation

### Screen Title (AWS Mainframe Modernization / CardDemo)

- Fixed values: 'AWS Mainframe Modernization' and 'CardDemo' (from CCDA-TITLE01 and CCDA-TITLE02)
- No user input or validation

### Date (mm/dd/yy)

- Populated dynamically from system date (WS-CURDATE-MM-DD-YY)
- Format: mm/dd/yy
- No user input

### Time (hh:mm:ss)

- Populated dynamically from system time (WS-CURTIME-HH-MM-SS)
- Format: hh:mm:ss
- No user input

### Update User Section Title

- Fixed label: 'Update User'
- No user input

### User ID Input Field (Enter User ID)

- Length: 8 characters
- Required field
- Underlined, green color
- Validation: Cannot be empty; error message 'User ID can NOT be empty...'
- Used to fetch user details from USRSEC file

### First Name Input Field

- Length: 20 characters
- Required field
- Underlined, green color
- Validation: Cannot be empty; error message 'First Name can NOT be empty...'
- Populated from USRSEC file after User ID fetch

### Last Name Input Field

- Length: 20 characters
- Required field
- Underlined, green color
- Validation: Cannot be empty; error message 'Last Name can NOT be empty...'
- Populated from USRSEC file after User ID fetch

### Password Input Field

- Length: 8 characters
- Required field
- Underlined, green color, dark attribute
- Validation: Cannot be empty; error message 'Password can NOT be empty...'
- Populated from USRSEC file after User ID fetch
- Display hint: '(8 Char)'

### User Type Input Field

- Length: 1 character
- Required field
- Underlined, green color
- Validation: Cannot be empty; error message 'User Type can NOT be empty...'
- Accepts 'A' for Admin, 'U' for User
- Populated from USRSEC file after User ID fetch
- Display hint: '(A=Admin, U=User)'

### Error/Status Message Area

- Length: 78 characters
- Color: Red for errors, neutral/green for status
- Displays validation errors, status, or instructions
- Populated by WS-MESSAGE

### Function Key Instructions

- Fixed text: 'ENTER=Fetch  F3=Save&Exit  F4=Clear  F5=Save  F12=Cancel'
- No user input
- Guides user on available actions

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
