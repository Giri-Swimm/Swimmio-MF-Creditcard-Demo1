---
title: Delete User Screen (COUSR3A)
---
The Delete User screen (COUSR3A) allows administrators to securely remove a user from the CardDemo system. By entering a User ID, the screen displays the user's details and provides options to fetch, clear, or delete the user record.

## Screen Preview

```
Tran: CU03           AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COUSR03C                     CardDemo                   Time: hh:mm:ss

                                   Delete User

     Enter User ID: ________

     **********************************************************************

     First Name: ____________________

     Last Name:  ____________________

     User Type: _ (A=Admin, U=User)


[Error/Status Message Area]

ENTER=Fetch  F3=Back  F4=Clear  F5=Delete
```

## Fields

### Transaction ID (Tran: CU03)

- Fixed value: 'CU03' (from WS-TRANID)
- Display only, not editable by user
- Used to identify the transaction for CICS routing

### Program Name (Prog: COUSR03C)

- Fixed value: 'COUSR03C' (from WS-PGMNAME)
- Display only, not editable by user
- Indicates the backend program handling this screen

### Date (Date: mm/dd/yy)

- Format: mm/dd/yy
- Populated from system date at runtime
- Display only, not editable by user

### Time (Time: hh:mm:ss)

- Format: hh:mm:ss
- Populated from system time at runtime
- Display only, not editable by user

### Screen Title (Delete User)

- Fixed text, not editable
- Indicates the purpose of the screen

### User ID Input (Enter User ID)

- Length: 8 characters
- Required field (cannot be empty)
- Underlined, green color
- Validation: If empty, error message 'User ID can NOT be empty...'
- Used to fetch user details for deletion

### First Name Display (First Name)

- Length: 20 characters
- Populated after valid User ID is entered and found in USRSEC file
- Display only, not editable
- Blank until User ID is fetched

### Last Name Display (Last Name)

- Length: 20 characters
- Populated after valid User ID is entered and found in USRSEC file
- Display only, not editable
- Blank until User ID is fetched

### User Type Display (User Type)

- Length: 1 character
- Populated after valid User ID is entered and found in USRSEC file
- Display only, not editable
- Values: 'A' for Admin, 'U' for User
- Blank until User ID is fetched

### Error/Status Message Area

- Length: 78 characters
- Used to display error or status messages
- Populated by program logic (e.g., 'User ID can NOT be empty...', 'User ID NOT found...', 'Press PF5 key to delete this user ...', 'User <id> has been deleted ...')
- Color: Red for errors, Green for success, Neutral for prompts

### Function Key Instructions

- Fixed text, not editable
- ENTER=Fetch: Fetch user details
- F3=Back: Return to previous screen
- F4=Clear: Clear all fields
- F5=Delete: Delete the user

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
