       IDENTIFICATION DIVISION.
       PROGRAM-ID. GUEST-LIST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GUEST-FILE ASSIGN TO "guest_list.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD GUEST-FILE.
       01 GUEST-RECORD.
           05 GUEST-NAME         PIC X(30).
           05 GUEST-CONTACT      PIC X(15).
           05 GUEST-TYPE         PIC X(10).
           05 GUEST-RSVP-STATUS  PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-GUEST.
           05 WS-GUEST-NAME      PIC X(30).
           05 WS-GUEST-CONTACT   PIC X(15).
           05 WS-GUEST-TYPE      PIC X(10).
           05 WS-GUEST-RSVP      PIC X(10).

       01 WS-FILE-STATUS         PIC X(2).
       01 WS-CHOICE              PIC 9.
       01 WS-GUEST-COUNT         PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-GUEST-MENU.
           PERFORM CLEAR-SCREEN
           DISPLAY "--------------------------------------------------"
           DISPLAY "                 Guest List Management"
           DISPLAY "--------------------------------------------------"
           DISPLAY "1. View Guest List"
           DISPLAY "2. Add Guest"
           DISPLAY "3. Edit Guest Information"
           DISPLAY "4. Remove Guest"
           DISPLAY "5. Back to Main Menu"
           DISPLAY "--------------------------------------------------"
           DISPLAY "Please select an option: "
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1 PERFORM VIEW-GUEST-LIST
               WHEN 2 PERFORM ADD-GUEST
               WHEN 3 PERFORM EDIT-GUEST
               WHEN 4 PERFORM REMOVE-GUEST
               WHEN 5 EXIT PROGRAM
               WHEN OTHER 
                   DISPLAY "Invalid option. Please try again."
                   PERFORM MAIN-GUEST-MENU
           END-EVALUATE.

       VIEW-GUEST-LIST.
           OPEN INPUT GUEST-FILE
           IF WS-FILE-STATUS = "35"
               DISPLAY "No guests have been added yet."
               DISPLAY "Press Enter to continue..."
               ACCEPT WS-CHOICE
           ELSE
               PERFORM READ-GUESTS
           END-IF
           CLOSE GUEST-FILE
           PERFORM MAIN-GUEST-MENU.

       READ-GUESTS.
           MOVE 0 TO WS-GUEST-COUNT
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ GUEST-FILE
                   AT END 
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       ADD 1 TO WS-GUEST-COUNT
                       DISPLAY "Guest " WS-GUEST-COUNT ": " 
                               GUEST-NAME " | Contact: " GUEST-CONTACT
               END-READ
           END-PERFORM
           
           IF WS-GUEST-COUNT = 0
               DISPLAY "No guests have been added yet."
           END-IF
           
           DISPLAY "Press Enter to continue..."
           ACCEPT WS-CHOICE.

       ADD-GUEST.
           OPEN EXTEND GUEST-FILE
           
           DISPLAY "Enter Guest Name: "
           ACCEPT WS-GUEST-NAME
           
           DISPLAY "Enter Contact Information: "
           ACCEPT WS-GUEST-CONTACT
           
           DISPLAY "Enter Guest Type (Family/Friend/Other): "
           ACCEPT WS-GUEST-TYPE
           
           DISPLAY "RSVP Status (Confirmed/Pending/Declined): "
           ACCEPT WS-GUEST-RSVP
           
           MOVE WS-GUEST-NAME TO GUEST-NAME
           MOVE WS-GUEST-CONTACT TO GUEST-CONTACT
           MOVE WS-GUEST-TYPE TO GUEST-TYPE
           MOVE WS-GUEST-RSVP TO GUEST-RSVP-STATUS
           
           WRITE GUEST-RECORD
           
           CLOSE GUEST-FILE
           
           DISPLAY "Guest added successfully!"
           DISPLAY "Press Enter to continue..."
           ACCEPT WS-CHOICE
           
           PERFORM MAIN-GUEST-MENU.

       EDIT-GUEST.
           DISPLAY "Edit Guest functionality not implemented yet."
           DISPLAY "Press Enter to continue..."
           ACCEPT WS-CHOICE
           PERFORM MAIN-GUEST-MENU.

       REMOVE-GUEST.
           DISPLAY "Remove Guest functionality not implemented yet."
           DISPLAY "Press Enter to continue..."
           ACCEPT WS-CHOICE
           PERFORM MAIN-GUEST-MENU.

       CLEAR-SCREEN.
           DISPLAY " " UPON CRT ERASE SCREEN.

       END PROGRAM GUEST-LIST.



       