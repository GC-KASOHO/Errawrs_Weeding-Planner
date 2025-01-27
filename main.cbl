       IDENTIFICATION DIVISION.
       PROGRAM-ID. WeddingPlanner.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-NUMBER-FILE 
               ASSIGN TO "./file_number.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FILE-NUMBER-FILE.
       01 FILE-NUMBER-RECORD    PIC X(3).

       WORKING-STORAGE SECTION.
       01 user-choice           PIC 9.
       01 plan-choice           PIC 9.
       01 os-command            PIC X(100).
       01 wedding-name          PIC X(50).
       01 folder-creation-status PIC 9.
           88 FOLDER-CREATED     VALUE 0.
           88 FOLDER-CREATION-FAILED VALUE 1.
       
       01 FOLDER-GENERATION-VARS.
           05 CURRENT-YEAR        PIC X(4).
           05 FIRST-LETTER        PIC X.
           05 NUM-FILE-NO PIC 9(3).
           05 FILE-NUMBER         PIC X(3).
           05 FOLDER-NAME         PIC X(12).

       PROCEDURE DIVISION.
       MAIN-MENU.
           PERFORM CLEAR-SCREEN
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
                   PERFORM CLEAR-SCREEN
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 3
                   DISPLAY "Exiting the system. Goodbye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
                   PERFORM MAIN-MENU
           END-EVALUATE.

       CREATE-NEW-WEDDING-PLAN.
           PERFORM CLEAR-SCREEN
           DISPLAY "Enter the name for your wedding plan: "
           ACCEPT wedding-name

           PERFORM CREATE-WEDDING-FOLDER

           DISPLAY "Wedding plan folder created successfully!"
           PERFORM WEDDING-PLAN-MANAGEMENT.

       CREATE-WEDDING-FOLDER.
           MOVE FUNCTION CURRENT-DATE(1:4) TO CURRENT-YEAR
           MOVE FUNCTION UPPER-CASE(wedding-name(1:1)) TO FIRST-LETTER
           
           PERFORM GET-NEXT-FILE-NUMBER
           
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
       
       GET-NEXT-FILE-NUMBER.
           *> Ensure the directory exists
           CALL "SYSTEM" USING "mkdir -p Archives" 
           
           *> Check if file exists
           CALL "SYSTEM" USING "touch ./file_number.txt"
           
           *> Open input file 
           OPEN INPUT FILE-NUMBER-FILE
           
           *> Read file contents
           READ FILE-NUMBER-FILE
               AT END 
                   MOVE FUNCTION NUMVAL(FILE-NUMBER) TO NUM-FILE-NO
                   ADD 1 TO NUM-FILE-NO
                   MOVE NUM-FILE-NO TO FILE-NUMBER
               NOT AT END
                   MOVE FILE-NUMBER-RECORD TO FILE-NUMBER
           END-READ
           
           CLOSE FILE-NUMBER-FILE
           
           *> Convert back to padded string
           EVALUATE TRUE
               WHEN NUM-FILE-NO < 10
                   STRING "00" NUM-FILE-NO DELIMITED BY SIZE 
                   INTO FILE-NUMBER
               WHEN NUM-FILE-NO < 100
                   STRING "0" NUM-FILE-NO DELIMITED BY SIZE 
                   INTO FILE-NUMBER
               WHEN OTHER
                   MOVE NUM-FILE-NO TO FILE-NUMBER
           END-EVALUATE.

       UPDATE-FILE-NUMBER.
           *> Open output file and write the new number
           OPEN OUTPUT FILE-NUMBER-FILE
           MOVE FILE-NUMBER TO FILE-NUMBER-RECORD
           WRITE FILE-NUMBER-RECORD
           CLOSE FILE-NUMBER-FILE.

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
           ACCEPT plan-choice

           EVALUATE plan-choice
               WHEN 1
                   PERFORM CLEAR-SCREEN
                   DISPLAY "Managing Guest List..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 2
                   PERFORM CLEAR-SCREEN
                   DISPLAY "Managing Tasks..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
                   CALL 'TASK-MANAGER'
               WHEN 3
                   PERFORM CLEAR-SCREEN
                   DISPLAY "Managing Budget..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 4
                   PERFORM CLEAR-SCREEN
                   DISPLAY "Managing Events..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 5
                   PERFORM CLEAR-SCREEN
                   DISPLAY "Returning to Main Menu..."
                   PERFORM MAIN-MENU
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
                   PERFORM WEDDING-PLAN-MANAGEMENT
           END-EVALUATE.

       CLEAR-SCREEN.
           CALL 'SYSTEM' USING 'clear' 
           IF RETURN-CODE NOT = 0
               CALL 'SYSTEM' USING 'cls'       
           END-IF.
