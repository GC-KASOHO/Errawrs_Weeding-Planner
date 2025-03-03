       IDENTIFICATION DIVISION.
       PROGRAM-ID. WeddingPlanner.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-NUMBER-FILE 
               ASSIGN TO "file_number.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TEMP-FILE 
               ASSIGN TO "temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FILE-NUMBER-FILE.
       01 FILE-NUMBER-RECORD    PIC X(3).

       FD TEMP-FILE.
       01 TEMP-FILE-RECORD      PIC X(255).

       WORKING-STORAGE SECTION.
       01 WS-CONTROL.
           05 user-choice       PIC 9.
           05 os-cmd        PIC X(100).
           05 current-folder    PIC X(50).
           05 temp-folder       PIC X(255).
           
       01 WS-FILE-CONTROL.
           05 CURRENT-YEAR      PIC X(4).
           05 FILE-NUMBER       PIC X(3).
           05 NUM-FILE-NO       PIC 999.
           05 FOLDER-NAME       PIC X(50).
           
       01 WS-INPUT.
           05 WEDDING-NAME      PIC X(50).
           05 account-num       PIC X(3).
           
       01 WS-FLAGS.
           05 folder-status     PIC 9.
           05 found-flag        PIC 9 VALUE 0.
           
       01 PAUSE-KEY            PIC X.
       01 ERROR-CHECK          PIC X(15).

       PROCEDURE DIVISION.
       MAIN-MENU.
           PERFORM CLEAR-SCREEN
           DISPLAY "Wedding Planner System"
           DISPLAY "1. Create New Wedding Plan"
           DISPLAY "2. Edit Existing Wedding Plan"
           DISPLAY "3. Exit"
           DISPLAY "Select option: "
           ACCEPT user-choice

           EVALUATE user-choice
               WHEN 1
                   PERFORM CREATE-NEW-WEDDING-PLAN
               WHEN 2
                   PERFORM EDIT-EXISTING-PLAN
               WHEN 3
                   STOP RUN
               WHEN OTHER
                   PERFORM MAIN-MENU
           END-EVALUATE.

       CREATE-NEW-WEDDING-PLAN.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter wedding plan name: "
           ACCEPT WEDDING-NAME

           PERFORM CREATE-WEDDING-FOLDER
           
           STRING "Archives\" FOLDER-NAME DELIMITED BY SIZE 
               INTO current-folder
           
           DISPLAY "Wedding plan created! Folder: " FOLDER-NAME
           DISPLAY "Account number: " FILE-NUMBER
           PERFORM PAUSE-SCREEN
           PERFORM WEDDING-PLAN-MANAGEMENT.

       CREATE-WEDDING-FOLDER.
           MOVE FUNCTION CURRENT-DATE(1:4) TO CURRENT-YEAR
           PERFORM READ-AND-INCREMENT-FILE-NUMBER
           
           STRING CURRENT-YEAR "-" FILE-NUMBER "-" WEDDING-NAME 
               DELIMITED BY SIZE INTO FOLDER-NAME
           
           STRING "mkdir Archives\" FOLDER-NAME 
               DELIMITED BY SIZE INTO os-cmd
           
           CALL "SYSTEM" USING os-cmd.

       READ-AND-INCREMENT-FILE-NUMBER.
           OPEN I-O FILE-NUMBER-FILE
           READ FILE-NUMBER-FILE
               AT END
                   MOVE "000" TO FILE-NUMBER
               NOT AT END
                   MOVE FILE-NUMBER-RECORD TO FILE-NUMBER
           END-READ
           
           COMPUTE NUM-FILE-NO = FUNCTION NUMVAL(FILE-NUMBER) + 1
           MOVE NUM-FILE-NO TO FILE-NUMBER
           
           REWRITE FILE-NUMBER-RECORD FROM FILE-NUMBER
           CLOSE FILE-NUMBER-FILE.

       EDIT-EXISTING-PLAN.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter account number (3 digits only): "
           ACCEPT account-num
           
           MOVE FUNCTION CURRENT-DATE(1:4) TO CURRENT-YEAR
           
           STRING "dir /b Archives\" CURRENT-YEAR "-" 
               account-num "-* > temp.txt 2>&1"
               DELIMITED BY SIZE INTO os-cmd
           CALL "SYSTEM" USING os-cmd
           
           OPEN INPUT TEMP-FILE
           READ TEMP-FILE INTO temp-folder
               AT END
                   MOVE SPACES TO temp-folder
           END-READ
           
           MOVE temp-folder(1:15) TO ERROR-CHECK
           
           IF ERROR-CHECK = "File Not Found "
               DISPLAY "Account not found."
               PERFORM PAUSE-SCREEN
               CLOSE TEMP-FILE
               STRING "del temp.txt" DELIMITED BY SIZE INTO os-cmd
               CALL "SYSTEM" USING os-cmd
               PERFORM MAIN-MENU
           ELSE
               IF temp-folder NOT = SPACES
                   STRING "Archives\" CURRENT-YEAR "-" account-num "-*"
                       DELIMITED BY SIZE INTO current-folder
                   DISPLAY "Account found!"
                   CLOSE TEMP-FILE
                   STRING "del temp.txt" DELIMITED BY SIZE INTO os-cmd
                   CALL "SYSTEM" USING os-cmd
                   PERFORM PAUSE-SCREEN
                   PERFORM WEDDING-PLAN-MANAGEMENT
               ELSE
                   DISPLAY "Account not found."
                   CLOSE TEMP-FILE
                   STRING "del temp.txt" DELIMITED BY SIZE INTO os-cmd
                   CALL "SYSTEM" USING os-cmd
                   PERFORM PAUSE-SCREEN
                   PERFORM MAIN-MENU
               END-IF
           END-IF.

       WEDDING-PLAN-MANAGEMENT.
           PERFORM CLEAR-SCREEN
           DISPLAY "Wedding Plan Management"
           DISPLAY "1. Manage Guest List"
           DISPLAY "2. Manage Tasks"
           DISPLAY "3. Budget Tracker"
           DISPLAY "4. Event Management"
           DISPLAY "5. Back to Main Menu"
           ACCEPT user-choice
           
           EVALUATE user-choice
               WHEN 1
                   *> Modify this line to CALL the Guest List Management program
                   CALL "GUEST-LIST-MANAGEMENT"
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 2
                   STRING "notepad " current-folder "\tasks.txt"
                       DELIMITED BY SIZE INTO os-cmd
                   CALL "SYSTEM" USING os-cmd
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 3
                   STRING "notepad " current-folder "\budget.txt"
                       DELIMITED BY SIZE INTO os-cmd
                   CALL "SYSTEM" USING os-cmd
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 4
                   STRING "notepad " current-folder "\events.txt"
                       DELIMITED BY SIZE INTO os-cmd
                   CALL "SYSTEM" USING os-cmd
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 5
                   PERFORM MAIN-MENU
               WHEN OTHER
                   PERFORM WEDDING-PLAN-MANAGEMENT
           END-EVALUATE.

       CLEAR-SCREEN.
           CALL "SYSTEM" USING "cls".

       PAUSE-SCREEN.
           DISPLAY "Press any key to continue..."
           ACCEPT PAUSE-KEY.


