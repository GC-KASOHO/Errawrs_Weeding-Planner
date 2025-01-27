       IDENTIFICATION DIVISION.
       PROGRAM-ID. GUEST-LIST-MANAGEMENT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GUEST-FILE ASSIGN TO "guests.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD GUEST-FILE.
       01 GUEST-RECORD.
           05 GUEST-NAME      PIC X(30).
           05 GUEST-CONTACT   PIC X(20).
           05 GUEST-STATUS    PIC X(10).
       
       FD TEMP-FILE.
       01 TEMP-RECORD.
           05 TEMP-NAME      PIC X(30).
           05 TEMP-CONTACT   PIC X(20).
           05 TEMP-STATUS    PIC X(10).
       
       WORKING-STORAGE SECTION.
       01 WS-GUEST-RECORD.
           05 WS-GUEST-NAME      PIC X(30).
           05 WS-GUEST-CONTACT   PIC X(20).
           05 WS-GUEST-STATUS    PIC X(10).
       01 WS-EDIT-NAME         PIC X(30).
       01 WS-MENU-CHOICE       PIC 9.
       01 WS-EOF               PIC X VALUE 'N'.
       01 WS-FOUND             PIC X VALUE 'N'.
       01 WS-PAUSE             PIC X.
       
       PROCEDURE DIVISION.
       CLEAR-SCREEN.
           CALL "SYSTEM" USING "cls".
       
       MAIN-MENU.
           PERFORM CLEAR-SCREEN.
           DISPLAY "--------------------------------------------------"
           DISPLAY "             GUEST LIST MANAGEMENT".
           DISPLAY "--------------------------------------------------"
           DISPLAY "1. View Guest List".
           DISPLAY "2. Add Guest".
           DISPLAY "3. Edit Guest Information".
           DISPLAY "4. Remove Guest".
           DISPLAY "5. Save Guest Information".
           DISPLAY "6. Back to Main Menu".
           DISPLAY "--------------------------------------------------"
           DISPLAY "Enter your choice: ".
           ACCEPT WS-MENU-CHOICE.
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM VIEW-GUEST-LIST
               WHEN 2 PERFORM ADD-GUEST
               WHEN 3 PERFORM EDIT-GUEST
               WHEN 4 PERFORM REMOVE-GUEST
               WHEN 5 PERFORM SAVE-GUEST-INFO
               WHEN 6 GO TO RETURN-TO-MAIN-MENU
               WHEN OTHER 
                   DISPLAY "Invalid choice. Try again."
                   PERFORM PAUSE-SCREEN
                   GO TO MAIN-MENU
           END-EVALUATE.
           
       VIEW-GUEST-LIST.
           PERFORM CLEAR-SCREEN.
           OPEN INPUT GUEST-FILE.
           MOVE 'N' TO WS-EOF.
           DISPLAY "GUEST LIST:".
           PERFORM READ-GUESTS UNTIL WS-EOF = 'Y'.
           CLOSE GUEST-FILE.
           PERFORM PAUSE-SCREEN.
           GO TO MAIN-MENU.
           
       READ-GUESTS.
           READ GUEST-FILE
               AT END 
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   DISPLAY "Name: " GUEST-NAME
                   DISPLAY "Contact: " GUEST-CONTACT
                   DISPLAY "Status: " GUEST-STATUS
                   DISPLAY "------------------------"
           END-READ.
           
       ADD-GUEST.
           PERFORM CLEAR-SCREEN.
           OPEN EXTEND GUEST-FILE.
           DISPLAY "Enter Guest Name: ".
           ACCEPT WS-GUEST-NAME.
           DISPLAY "Enter Guest Contact: ".
           ACCEPT WS-GUEST-CONTACT.
           DISPLAY "Enter Guest Status (Confirmed/Pending): ".
           ACCEPT WS-GUEST-STATUS.
           
           MOVE WS-GUEST-NAME TO GUEST-NAME.
           MOVE WS-GUEST-CONTACT TO GUEST-CONTACT.
           MOVE WS-GUEST-STATUS TO GUEST-STATUS.
           
           WRITE GUEST-RECORD.
           
           CLOSE GUEST-FILE.
           DISPLAY "Guest added successfully!".
           PERFORM PAUSE-SCREEN.
           GO TO MAIN-MENU.
           
       EDIT-GUEST.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Enter the name of the guest to edit: ".
           ACCEPT WS-EDIT-NAME.
           
           OPEN INPUT GUEST-FILE.
           OPEN OUTPUT TEMP-FILE.
           MOVE 'N' TO WS-EOF.
           MOVE 'N' TO WS-FOUND.
           
           PERFORM EDIT-GUEST-PROCESS UNTIL WS-EOF = 'Y'.
           
           CLOSE GUEST-FILE.
           CLOSE TEMP-FILE.
           
           CALL "SYSTEM" USING "del guests.txt".
           CALL "SYSTEM" USING "ren temp.txt guests.txt".
           
           IF WS-FOUND = 'N'
               DISPLAY "Guest not found."
           ELSE
               DISPLAY "Guest information updated successfully."
           END-IF.
           
           PERFORM PAUSE-SCREEN.
           GO TO MAIN-MENU.
           
       EDIT-GUEST-PROCESS.
           READ GUEST-FILE
               AT END 
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF GUEST-NAME = WS-EDIT-NAME
                       MOVE 'Y' TO WS-FOUND
                       DISPLAY "Current Name: " GUEST-NAME
                       DISPLAY "Current Contact: " GUEST-CONTACT
                       DISPLAY "Current Status: " GUEST-STATUS
                       
                       DISPLAY "Enter new Name :  "
                       ACCEPT WS-GUEST-NAME
                       IF WS-GUEST-NAME = SPACES
                           MOVE GUEST-NAME TO WS-GUEST-NAME
                       END-IF
                       
                       DISPLAY "Enter new Contact : "
                       ACCEPT WS-GUEST-CONTACT
                       IF WS-GUEST-CONTACT = SPACES
                           MOVE GUEST-CONTACT TO WS-GUEST-CONTACT
                       END-IF
                       
                       DISPLAY "Enter new Status  : "
                       ACCEPT WS-GUEST-STATUS
                       IF WS-GUEST-STATUS = SPACES
                           MOVE GUEST-STATUS TO WS-GUEST-STATUS
                       END-IF
                       
                       MOVE WS-GUEST-NAME TO TEMP-NAME
                       MOVE WS-GUEST-CONTACT TO TEMP-CONTACT
                       MOVE WS-GUEST-STATUS TO TEMP-STATUS
                       WRITE TEMP-RECORD
                   ELSE
                       MOVE GUEST-NAME TO TEMP-NAME
                       MOVE GUEST-CONTACT TO TEMP-CONTACT
                       MOVE GUEST-STATUS TO TEMP-STATUS
                       WRITE TEMP-RECORD
                   END-IF
           END-READ.
           
       REMOVE-GUEST.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Enter the name of the guest to remove: ".
           ACCEPT WS-EDIT-NAME.
           
           OPEN INPUT GUEST-FILE.
           OPEN OUTPUT TEMP-FILE.
           MOVE 'N' TO WS-EOF.
           MOVE 'N' TO WS-FOUND.
           
           PERFORM REMOVE-GUEST-PROCESS UNTIL WS-EOF = 'Y'.
           
           CLOSE GUEST-FILE.
           CLOSE TEMP-FILE.
           
           CALL "SYSTEM" USING "del guests.txt".
           CALL "SYSTEM" USING "ren temp.txt guests.txt".
           
           IF WS-FOUND = 'N'
               DISPLAY "Guest not found."
           ELSE
               DISPLAY "Guest removed successfully."
           END-IF.
           
           PERFORM PAUSE-SCREEN.
           GO TO MAIN-MENU.
           
       REMOVE-GUEST-PROCESS.
           READ GUEST-FILE
               AT END 
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF GUEST-NAME = WS-EDIT-NAME
                       MOVE 'Y' TO WS-FOUND
                   ELSE
                       MOVE GUEST-NAME TO TEMP-NAME
                       MOVE GUEST-CONTACT TO TEMP-CONTACT
                       MOVE GUEST-STATUS TO TEMP-STATUS
                       WRITE TEMP-RECORD
                   END-IF
           END-READ.
           
       SAVE-GUEST-INFO.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Saving guest information to guest.txt...".
           PERFORM PAUSE-SCREEN.
           GO TO MAIN-MENU.
           
       PAUSE-SCREEN.
           DISPLAY "Press Enter to continue..."
           ACCEPT WS-PAUSE.
           
       RETURN-TO-MAIN-MENU.
           EXIT PROGRAM.
       
       END PROGRAM GUEST-LIST-MANAGEMENT.


       