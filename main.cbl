       IDENTIFICATION DIVISION.
       PROGRAM-ID. WeddingPlanner.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-NUMBER-FILE 
               ASSIGN TO "./file_number.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ACCOUNT-RECORD-FILE
               ASSIGN TO"./account_record.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FILE-NUMBER-FILE.
       01 FILE-NUMBER-RECORD    PIC X(3).

       FD ACCOUNT-RECORD-FILE.
       01 ACCOUNT                PIC X(12).

       WORKING-STORAGE SECTION.
       01 user-choice           PIC 9.
       01 CHOICE           PIC 9.
       01 os-command            PIC X(100).
       01 wedding-name          PIC X(50).
       01 folder-creation-status PIC 9.
           88 FOLDER-CREATED     VALUE 0.
           88 FOLDER-CREATION-FAILED VALUE 1.
       
       01 FOLDER-GENERATION-VARS.
           05 CURRENT-YEAR        PIC X(4).
           05 FIRST-LETTER        PIC X.
           05 NUM-FILE-NO         PIC 999.
           05 FILE-NUMBER         PIC X(3).
           05 FOLDER-NAME         PIC X(12).

       PROCEDURE DIVISION.
       MAIN-MENU.

           DISPLAY "--------------------------------------------------"
           DISPLAY "                  Wedding Planner System"
           DISPLAY "--------------------------------------------------"
           DISPLAY "1. Create New Wedding Plan"
           DISPLAY "2. Edit Existing Wedding Plan"
           DISPLAY "3. Exit"
           DISPLAY "--------------------------------------------------"
           DISPLAY "Please select an option: "
           ACCEPT user-choice

           EVALUATE user-choice
               WHEN 1
                   PERFORM CREATE-NEW-WEDDING-PLAN
               WHEN 2
        
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 3
                   DISPLAY "Exiting the system. Goodbye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
                   PERFORM MAIN-MENU
           END-EVALUATE.

       CREATE-NEW-WEDDING-PLAN.

           DISPLAY "Enter the name for your wedding plan: "
           ACCEPT wedding-name

           PERFORM CREATE-WEDDING-FOLDER

           DISPLAY "Wedding plan folder created successfully!"
           DISPLAY "Your account number is: "
           DISPLAY NUM-FILE-NO.

           PERFORM  RECORD-ACCOUNT.
           
           PERFORM WEDDING-PLAN-MANAGEMENT.

       CREATE-WEDDING-FOLDER.
           MOVE FUNCTION CURRENT-DATE(1:4) TO CURRENT-YEAR
           MOVE FUNCTION UPPER-CASE(wedding-name(1:1)) TO FIRST-LETTER
           
           PERFORM READ-AND-INCREMENT-FILE-NUMBER
           
           STRING CURRENT-YEAR "-" FILE-NUMBER "-" FIRST-LETTER 
               DELIMITED BY SIZE INTO FOLDER-NAME
           
           STRING "mkdir Archives\" FOLDER-NAME 
                   DELIMITED BY SIZE INTO os-command
           
           CALL "SYSTEM" USING os-command
                   RETURNING folder-creation-status
           
           IF folder-creation-status = 0
                   PERFORM UPDATE-FILE-NUMBER
           ELSE
                   DISPLAY "Failed to create folder"
           END-IF.
       
       READ-AND-INCREMENT-FILE-NUMBER.
           MOVE ZEROS TO NUM-FILE-NO
           OPEN INPUT FILE-NUMBER-FILE
           READ FILE-NUMBER-FILE
        
           MOVE FILE-NUMBER-RECORD TO FILE-NUMBER
           COMPUTE NUM-FILE-NO = FUNCTION NUMVAL(FILE-NUMBER)
           
           CLOSE FILE-NUMBER-FILE
           
           ADD 1 TO NUM-FILE-NO

           MOVE NUM-FILE-NO TO FILE-NUMBER.

       UPDATE-FILE-NUMBER.
           OPEN OUTPUT FILE-NUMBER-FILE
           MOVE FILE-NUMBER TO FILE-NUMBER-RECORD
           WRITE FILE-NUMBER-RECORD
           CLOSE FILE-NUMBER-FILE.

       RECORD-ACCOUNT.
           OPEN OUTPUT ACCOUNT-RECORD-FILE
        
           STRING CURRENT-YEAR "-" FILE-NUMBER "-" FIRST-LETTER 
               DELIMITED BY SIZE INTO ACCOUNT
           
           WRITE ACCOUNT
           CLOSE ACCOUNT-RECORD-FILE.


       WEDDING-PLAN-MANAGEMENT.
           PERFORM CLEAR-SCREEN

           DISPLAY "--------------------------------------------------"
           DISPLAY "                 Wedding Plan Management"
           DISPLAY "--------------------------------------------------"
           DISPLAY "1. Manage Guest List"
           DISPLAY "2. Manage Tasks"
           DISPLAY "3. Budget Tracker"
           DISPLAY "4. Vendor Management"
           DISPLAY "5. Back to Main Menu"
           DISPLAY "--------------------------------------------------"
           DISPLAY "Please select an option: "
           ACCEPT CHOICE
           
           PERFORM CLEAR-SCREEN

           EVALUATE CHOICE
               WHEN 1
                   DISPLAY "Managing Guest List..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 2
                   DISPLAY "Managing Tasks..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 3
                   DISPLAY "Managing Budget..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 4
                   DISPLAY "Managing Events..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 5
                   DISPLAY "Returning to Main Menu..."
                   PERFORM MAIN-MENU
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
                   PERFORM WEDDING-PLAN-MANAGEMENT
           END-EVALUATE.

       CLEAR-SCREEN.
           CALL 'SYSTEM' USING 'cls'.
           