       IDENTIFICATION DIVISION.
       PROGRAM-ID. WeddingPlanner.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 user-choice       PIC 9.
       01 plan-choice       PIC 9.
       01 os-command        PIC X(10).

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
                   PERFORM CLEAR-SCREEN
                   PERFORM WEDDING-PLAN-MANAGEMENT
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

       WEDDING-PLAN-MANAGEMENT.
           PERFORM CLEAR-SCREEN
           DISPLAY "--------------------------------------------------"
           DISPLAY "                 Wedding Plan Management"
           DISPLAY "--------------------------------------------------"
           DISPLAY "1. Manage Guest List"
           DISPLAY "2. Manage Tasks"
           DISPLAY "3. Budget Tracker"
           DISPLAY "4. Vendor Management"
           DISPLAY "5. Event Schedules"
           DISPLAY "6. Back to Main Menu"
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
               WHEN 3
                   PERFORM CLEAR-SCREEN
                   DISPLAY "Managing Budget..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 4
                   PERFORM CLEAR-SCREEN
                   DISPLAY "Managing Vendors..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 5
                   PERFORM CLEAR-SCREEN
                   DISPLAY "Managing Events..."
                   PERFORM WEDDING-PLAN-MANAGEMENT
               WHEN 6
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
