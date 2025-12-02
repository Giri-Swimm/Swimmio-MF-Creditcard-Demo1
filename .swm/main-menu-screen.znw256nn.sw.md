---
title: Main Menu Screen
---
The Main Menu screen is the central navigation point for regular users in the CardDemo application, allowing them to select from a range of credit card and account management functions.

## Screen Preview

```
Tran: CM00              AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COMEN01C                        CardDemo                   Time: hh:mm:ss

                                   Main Menu

                    1. Account View
                    2. Account Update
                    3. Credit Card List
                    4. Credit Card View
                    5. Credit Card Update
                    6. Transaction List
                    7. Transaction View
                    8. Transaction Add
                    9. Transaction Reports
                   10. Bill Payment
                   11. Pending Authorization View

               Please select an option : __



[Error/Status Message Area]

ENTER=Continue  F3=Exit
```

## Fields

### Transaction ID (Tran:)

- Fixed value: 'CM00' (from WS-TRANID)
- Display only, not editable by the user.

### Program Name (Prog:)

- Fixed value: 'COMEN01C' (from WS-PGMNAME)
- Display only, not editable by the user.

### Title Line 1

- Fixed value: 'AWS Mainframe Modernization' (from CCDA-TITLE01)
- Display only, not editable by the user.

### Title Line 2

- Fixed value: 'CardDemo' (from CCDA-TITLE02)
- Display only, not editable by the user.

### Date (Date:)

- Format: mm/dd/yy (populated from current date)
- Display only, not editable by the user.

### Time (Time:)

- Format: hh:mm:ss (populated from current time)
- Display only, not editable by the user.

### Main Menu Options (1-11)

- Each line displays a menu option number and description, e.g., '1. Account View'.
- Options 1-11 are shown; there is space for a 12th option, but only 11 are defined for regular users.
- Display only, not editable by the user.

### Option Input Field

- Input field for selecting a menu option (2 characters, right-justified, zero-filled if needed).
- Only numeric values 1-11 are valid (must be numeric, >0, <=11).
- Field is underlined and accepts only numbers.
- Validation: If not numeric, out of range, or zero, an error message is shown.
- If the option is restricted (admin only), an error message is shown.

### Error/Status Message Area

- Up to 78 characters.
- Used to display error messages, such as invalid key or access denied.
- Display only; content is set by the program logic.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1NRi1DcmVkaXRjYXJkLURlbW8xJTNBJTNBR2lyaS1Td2ltbQ==" repo-name="Swimmio-MF-Creditcard-Demo1"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
