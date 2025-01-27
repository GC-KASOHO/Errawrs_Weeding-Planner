       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDGET-TRACKER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BUDGET-FILE 
               ASSIGN TO "C:\Users\Administrator\Downloads\Errawrs_Weeding-Planner\budget-tracker.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD BUDGET-FILE.
       01 BUDGET-RECORD.
           05 CATEGORY-NAME        PIC X(30).
           05 ESTIMATED-BUDGET     PIC 9(7)V99.
           05 ACTUAL-EXPENSE       PIC 9(7)V99.
           05 EXPENSE-DATE         PIC X(10).
           05 EXPENSE-DESCRIPTION  PIC X(50).

       WORKING-STORAGE SECTION.
       01 WS-BUDGET-RECORD.
           05 WS-CATEGORY-NAME     PIC X(30).
           05 WS-ESTIMATED-BUDGET  PIC 9(7)V99.
           05 WS-ACTUAL-EXPENSE    PIC 9(7)V99.
           05 WS-EXPENSE-DATE      PIC X(10).
           05 WS-EXPENSE-DESC      PIC X(50).

       01 BUDGET-SUMMARY-VARS.
           05 TOTAL-ESTIMATED-BUDGET   PIC 9(10)V99.
           05 TOTAL-ACTUAL-EXPENSES    PIC 9(10)V99.
           05 REMAINING-BUDGET         PIC 9(10)V99.

       01 PROGRAM-CONTROL.
           05 USER-CHOICE          PIC 9.
           05 CATEGORY-COUNT       PIC 99.
           05 EOF-FLAG             PIC X VALUE 'N'.
           05 WS-FILE-STATUS       PIC XX.
           05 WS-ERROR-MESSAGE     PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-PROGRAM
           PERFORM BUDGET-MENU
           STOP RUN.

       INITIALIZE-PROGRAM.
           PERFORM CREATE-FILE-IF-NOT-EXISTS.

       CREATE-FILE-IF-NOT-EXISTS.
           OPEN EXTEND BUDGET-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error creating/opening file: " WS-FILE-STATUS
               DISPLAY "Attempting to create file manually"
               PERFORM MANUAL-FILE-CREATE
           END-IF
           CLOSE BUDGET-FILE.

       MANUAL-FILE-CREATE.
           CALL "system" USING BY REFERENCE "touch C:\Users\Administrator\Downloads\Errawrs_Weeding-Planner\budget-tracker.txt"
           CALL "system" USING BY REFERENCE 
               "echo 'Category,Estimated Budget,Actual Expense,Date,Description' > C:\Users\Administrator\Downloads\Errawrs_Weeding-Planner\budget-tracker.txt".

       BUDGET-MENU.
           DISPLAY "---------------------------------------------"
           DISPLAY "           Wedding Budget Tracker"
           DISPLAY "---------------------------------------------"
           DISPLAY "1. View Budget Overview"
           DISPLAY "2. Add Expense"
           DISPLAY "3. View Remaining Budget"
           DISPLAY "4. View Budget Reports"
           DISPLAY "5. Exit Program"
           DISPLAY "---------------------------------------------"
           DISPLAY "Enter your choice: "
           ACCEPT USER-CHOICE

           EVALUATE USER-CHOICE
               WHEN 1 PERFORM VIEW-BUDGET-OVERVIEW
               WHEN 2 PERFORM ADD-EXPENSE
               WHEN 3 PERFORM VIEW-REMAINING-BUDGET
               WHEN 4 PERFORM VIEW-BUDGET-REPORTS
               WHEN 5 PERFORM RETURN-TO-MAIN-MENU
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
                   PERFORM BUDGET-MENU
           END-EVALUATE.

       ADD-EXPENSE.
           OPEN EXTEND BUDGET-FILE
           IF WS-FILE-STATUS = "00"
               PERFORM CAPTURE-EXPENSE
           ELSE
               DISPLAY "Error opening file for writing. Status: " 
                       WS-FILE-STATUS
           END-IF
           
           CLOSE BUDGET-FILE
           PERFORM BUDGET-MENU.

       CAPTURE-EXPENSE.
           DISPLAY "Enter Category Name: "
           ACCEPT WS-CATEGORY-NAME

           DISPLAY "Enter Estimated Budget (PHP): "
           ACCEPT WS-ESTIMATED-BUDGET

           DISPLAY "Enter Actual Expense (PHP): "
           ACCEPT WS-ACTUAL-EXPENSE

           DISPLAY "Enter Expense Date (YYYY-MM-DD): "
           ACCEPT WS-EXPENSE-DATE

           DISPLAY "Enter Expense Description: "
           ACCEPT WS-EXPENSE-DESC

           MOVE WS-CATEGORY-NAME TO CATEGORY-NAME
           MOVE WS-ESTIMATED-BUDGET TO ESTIMATED-BUDGET
           MOVE WS-ACTUAL-EXPENSE TO ACTUAL-EXPENSE
           MOVE WS-EXPENSE-DATE TO EXPENSE-DATE
           MOVE WS-EXPENSE-DESC TO EXPENSE-DESCRIPTION

           WRITE BUDGET-RECORD
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error writing to file. Status: " WS-FILE-STATUS
           ELSE
               DISPLAY "Expense added successfully!"
           END-IF.

       VIEW-BUDGET-OVERVIEW.
           OPEN INPUT BUDGET-FILE
           IF WS-FILE-STATUS = "00"
               PERFORM READ-BUDGET-FILE
           ELSE
               DISPLAY "Error opening file. Status: " WS-FILE-STATUS
           END-IF
           
           CLOSE BUDGET-FILE
           PERFORM BUDGET-MENU.

       READ-BUDGET-FILE.
           MOVE ZEROS TO TOTAL-ESTIMATED-BUDGET
                         TOTAL-ACTUAL-EXPENSES

           DISPLAY "---------------------------------------------"
           DISPLAY "           Budget Overview"
           DISPLAY "---------------------------------------------"

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ BUDGET-FILE
                   AT END 
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       DISPLAY "Category: " CATEGORY-NAME
                       DISPLAY "Estimated Budget: PHP " ESTIMATED-BUDGET
                       DISPLAY "Actual Expenses: PHP " ACTUAL-EXPENSE
                       DISPLAY "Expense Date: " EXPENSE-DATE
                       DISPLAY "Description: " EXPENSE-DESCRIPTION
                       DISPLAY "---------------------------------------------"

                       ADD ESTIMATED-BUDGET TO TOTAL-ESTIMATED-BUDGET
                       ADD ACTUAL-EXPENSE TO TOTAL-ACTUAL-EXPENSES
           END-PERFORM.

       VIEW-REMAINING-BUDGET.
           OPEN INPUT BUDGET-FILE
           IF WS-FILE-STATUS = "00"
               PERFORM CALCULATE-REMAINING-BUDGET
           ELSE
               DISPLAY "Error opening file. Status: " WS-FILE-STATUS
           END-IF
           
           CLOSE BUDGET-FILE
           PERFORM BUDGET-MENU.

       CALCULATE-REMAINING-BUDGET.
           MOVE ZEROS TO TOTAL-ESTIMATED-BUDGET
                         TOTAL-ACTUAL-EXPENSES

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ BUDGET-FILE
                   AT END 
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       ADD ESTIMATED-BUDGET TO TOTAL-ESTIMATED-BUDGET
                       ADD ACTUAL-EXPENSE TO TOTAL-ACTUAL-EXPENSES
           END-PERFORM

           COMPUTE REMAINING-BUDGET = 
               TOTAL-ESTIMATED-BUDGET - TOTAL-ACTUAL-EXPENSES

           DISPLAY "---------------------------------------------"
           DISPLAY "           Remaining Budget"
           DISPLAY "---------------------------------------------"
           DISPLAY "Total Estimated Budget: PHP " TOTAL-ESTIMATED-BUDGET
           DISPLAY "Total Actual Expenses:  PHP " TOTAL-ACTUAL-EXPENSES
           DISPLAY "Remaining Budget:       PHP " REMAINING-BUDGET
           DISPLAY "---------------------------------------------"

           MOVE 'N' TO EOF-FLAG.

       VIEW-BUDGET-REPORTS.
           OPEN INPUT BUDGET-FILE
           IF WS-FILE-STATUS = "00"
               PERFORM GENERATE-BUDGET-REPORT
           ELSE
               DISPLAY "Error opening file. Status: " WS-FILE-STATUS
           END-IF
           
           CLOSE BUDGET-FILE
           PERFORM BUDGET-MENU.

       GENERATE-BUDGET-REPORT.
           DISPLAY "---------------------------------------------"
           DISPLAY "           Budget Reports"
           DISPLAY "---------------------------------------------"
           DISPLAY "Expense Breakdown by Category:" 
           DISPLAY "---------------------------------------------"

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ BUDGET-FILE
                   AT END 
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       DISPLAY "Category: " CATEGORY-NAME
                       DISPLAY "Estimated: PHP " ESTIMATED-BUDGET
                       DISPLAY "Actual:    PHP " ACTUAL-EXPENSE
                       DISPLAY "Difference: PHP " 
                           FUNCTION ABS(ESTIMATED-BUDGET - ACTUAL-EXPENSE)
                       DISPLAY "---------------------------------------------"
           END-PERFORM.

       CLEAR-SCREEN.
           CALL "SYSTEM" USING "cls".
       RETURN-TO-MAIN-MENU.
           DISPLAY "Exiting Budget Tracker. Goodbye!"
           STOP RUN.

       END PROGRAM BUDGET-TRACKER.