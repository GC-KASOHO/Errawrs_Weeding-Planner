       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVENT-SCHEDULE2.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EVENT-FILE ASSIGN TO "events.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.
           SELECT TEMP-FILE ASSIGN TO "temp.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-TEMP-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD EVENT-FILE.
       01 EVENT-RECORD.
           05 EVENT-NAME   PIC X(30).
           05 EVENT-DATE-YEAR PIC 9(4).
           05 EVENT-DATE-MONTH PIC 9(2).
           05 EVENT-DATE-DAY PIC 9(2).
           05 EVENT-TIME-HOUR PIC 9(2).
           05 EVENT-TIME-MINUTES PIC 9(2).
           05 EVENT-LOCATION PIC X(50).
           
       FD TEMP-FILE.
       01 TEMP-RECORD.
           05 TEMP-NAME   PIC X(30).
           05 TEMP-DATE-YEAR PIC 9(4).
           05 TEMP-DATE-MONTH PIC 9(2).
           05 TEMP-DATE-DAY PIC 9(2).
           05 TEMP-TIME-HOUR PIC 9(2).
           05 TEMP-TIME-MINUTES PIC 9(2).
           05 TEMP-LOCATION PIC X(50).
       
       WORKING-STORAGE SECTION.
       01 WS-USER-CHOICE PIC 9.
       01 WS-CONTINUE PIC X VALUE 'Y'.
       01 WS-EVENT-NAME PIC X(30).
       01 WS-EVENT-DATE-YEAR PIC 9(4).
       01 WS-EVENT-DATE-MONTH PIC 9(2).
       01 WS-EVENT-DATE-DAY PIC 9(2).
       01 WS-EVENT-TIME-HOUR PIC 9(2).
       01 WS-EVENT-TIME-MINUTES PIC 9(2).
       01 WS-EVENT-LOCATION PIC X(50).
       01 WS-FILE-STATUS PIC XX.
       01 WS-TEMP-STATUS PIC XX.
       01 WS-FOUND-FLAG PIC X VALUE 'N'.
       01 WS-EDIT-NAME PIC X(30).
       01 WS-EOF PIC X VALUE 'N'.
       01 WS-DELETE-NAME PIC X(30).
       01 WS-CONFIRM-DELETE PIC X.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL WS-CONTINUE = 'N'
               DISPLAY '------------------------------------------------'
               DISPLAY '                Event Schedules'
               DISPLAY '------------------------------------------------'
               DISPLAY '1. View Event List'
               DISPLAY '2. Add Event'
               DISPLAY '3. Edit Event'
               DISPLAY '4. Delete Event'
               DISPLAY '5. Back to Main Menu'
               DISPLAY '------------------------------------------------'
               DISPLAY 'Please select an option: ' WITH NO ADVANCING
               ACCEPT WS-USER-CHOICE

               EVALUATE WS-USER-CHOICE
                   WHEN 1
                       PERFORM VIEW-EVENTS
                   WHEN 2
                       PERFORM ADD-EVENT
                   WHEN 3
                       PERFORM EDIT-EVENT
                   WHEN 4
                       PERFORM DELETE-EVENT
                   WHEN 5
                       MOVE 'N' TO WS-CONTINUE
                   WHEN OTHER
                       DISPLAY 'Invalid option. Please try again.'
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       DELETE-EVENT.
           DISPLAY "Enter the name of the event to delete: "
           ACCEPT WS-DELETE-NAME
           MOVE 'N' TO WS-FOUND-FLAG
           MOVE 'N' TO WS-EOF
           
           OPEN INPUT EVENT-FILE
           IF WS-FILE-STATUS = "35"
               DISPLAY "No events found. File does not exist yet."
           ELSE
               OPEN OUTPUT TEMP-FILE
               
               PERFORM UNTIL WS-EOF = 'Y'
                   READ EVENT-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF EVENT-NAME = WS-DELETE-NAME
                               MOVE 'Y' TO WS-FOUND-FLAG
                               DISPLAY "Event found:"
                               DISPLAY "Name: " EVENT-NAME
                               DISPLAY "Date: " EVENT-DATE-YEAR "/"
                                   EVENT-DATE-MONTH "/" EVENT-DATE-DAY
                               DISPLAY "Time: " EVENT-TIME-HOUR ":"
                                   EVENT-TIME-MINUTES
                               DISPLAY "Location: " EVENT-LOCATION
                               DISPLAY "Are you sure you want to delete this event? (Y/N)"
                               ACCEPT WS-CONFIRM-DELETE
                               IF WS-CONFIRM-DELETE = 'N' OR 'n'
                                   MOVE EVENT-NAME TO TEMP-NAME
                                   MOVE EVENT-DATE-YEAR TO TEMP-DATE-YEAR
                                   MOVE EVENT-DATE-MONTH TO TEMP-DATE-MONTH
                                   MOVE EVENT-DATE-DAY TO TEMP-DATE-DAY
                                   MOVE EVENT-TIME-HOUR TO TEMP-TIME-HOUR
                                   MOVE EVENT-TIME-MINUTES TO TEMP-TIME-MINUTES
                                   MOVE EVENT-LOCATION TO TEMP-LOCATION
                                   WRITE TEMP-RECORD
                               END-IF
                           ELSE
                               MOVE EVENT-NAME TO TEMP-NAME
                               MOVE EVENT-DATE-YEAR TO TEMP-DATE-YEAR
                               MOVE EVENT-DATE-MONTH TO TEMP-DATE-MONTH
                               MOVE EVENT-DATE-DAY TO TEMP-DATE-DAY
                               MOVE EVENT-TIME-HOUR TO TEMP-TIME-HOUR
                               MOVE EVENT-TIME-MINUTES TO TEMP-TIME-MINUTES
                               MOVE EVENT-LOCATION TO TEMP-LOCATION
                               WRITE TEMP-RECORD
                           END-IF
                   END-READ
               END-PERFORM
               
               CLOSE EVENT-FILE
               CLOSE TEMP-FILE
               
               IF WS-FOUND-FLAG = 'N'
                   DISPLAY "Event not found."
               ELSE
                   IF WS-CONFIRM-DELETE = 'Y' OR 'y'
                       PERFORM REPLACE-FILE
                       DISPLAY "Event deleted successfully."
                   ELSE
                       PERFORM REPLACE-FILE
                       DISPLAY "Delete operation cancelled."
                   END-IF
               END-IF
           END-IF
           
           MOVE 'Y' TO WS-CONTINUE.

       VIEW-EVENTS.
           OPEN INPUT EVENT-FILE
           IF WS-FILE-STATUS = "35"
               DISPLAY "No events found. File does not exist yet."
               DISPLAY "Please add an event first."
           ELSE
               MOVE 'N' TO WS-EOF
               PERFORM UNTIL WS-EOF = 'Y'
                   READ EVENT-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           DISPLAY "Event: " EVENT-NAME
                           DISPLAY "Date: " EVENT-DATE-YEAR "/"
                               EVENT-DATE-MONTH "/" EVENT-DATE-DAY
                           DISPLAY "Time: " EVENT-TIME-HOUR ":"
                               EVENT-TIME-MINUTES
                           DISPLAY "Location: " EVENT-LOCATION
                           DISPLAY " "
                   END-READ
               END-PERFORM
               CLOSE EVENT-FILE
           END-IF
           MOVE 'Y' TO WS-CONTINUE.

       ADD-EVENT.
           DISPLAY "Enter Event Name: "
           ACCEPT WS-EVENT-NAME
           DISPLAY "Enter Event Year: "
           ACCEPT WS-EVENT-DATE-YEAR
           DISPLAY "Enter Event Month: "
           ACCEPT WS-EVENT-DATE-MONTH
           DISPLAY "Enter Event Day: "
           ACCEPT WS-EVENT-DATE-DAY
           DISPLAY "Enter Event Hour: "
           ACCEPT WS-EVENT-TIME-HOUR
           DISPLAY "Enter Event Minutes: "
           ACCEPT WS-EVENT-TIME-MINUTES
           DISPLAY "Enter Event Location: "
           ACCEPT WS-EVENT-LOCATION

           MOVE WS-EVENT-NAME TO EVENT-NAME
           MOVE WS-EVENT-DATE-YEAR TO EVENT-DATE-YEAR
           MOVE WS-EVENT-DATE-MONTH TO EVENT-DATE-MONTH
           MOVE WS-EVENT-DATE-DAY TO EVENT-DATE-DAY
           MOVE WS-EVENT-TIME-HOUR TO EVENT-TIME-HOUR
           MOVE WS-EVENT-TIME-MINUTES TO EVENT-TIME-MINUTES
           MOVE WS-EVENT-LOCATION TO EVENT-LOCATION
           
           OPEN EXTEND EVENT-FILE
           IF WS-FILE-STATUS = "35"
               OPEN OUTPUT EVENT-FILE
           END-IF
           WRITE EVENT-RECORD
           CLOSE EVENT-FILE

           DISPLAY "Event added successfully."
           DISPLAY "Add another event? (Y/N)"
           ACCEPT WS-CONTINUE.

       EDIT-EVENT.
           DISPLAY "Enter the name of the event to edit: "
           ACCEPT WS-EDIT-NAME
           MOVE 'N' TO WS-FOUND-FLAG
           MOVE 'N' TO WS-EOF
           
           OPEN INPUT EVENT-FILE
           IF WS-FILE-STATUS = "35"
               DISPLAY "No events found. File does not exist yet."
           ELSE
               OPEN OUTPUT TEMP-FILE
               
               PERFORM UNTIL WS-EOF = 'Y'
                   READ EVENT-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF EVENT-NAME = WS-EDIT-NAME
                               MOVE 'Y' TO WS-FOUND-FLAG
                               DISPLAY "Current event details:"
                               DISPLAY "Name: " EVENT-NAME
                               DISPLAY "Date: " EVENT-DATE-YEAR "/"
                                   EVENT-DATE-MONTH "/" EVENT-DATE-DAY
                               DISPLAY "Time: " EVENT-TIME-HOUR ":"
                                   EVENT-TIME-MINUTES
                               DISPLAY "Location: " EVENT-LOCATION
                               
                               DISPLAY "Enter new details:"
                               DISPLAY "Enter Event Name: "
                               ACCEPT WS-EVENT-NAME
                               DISPLAY "Enter Event Year: "
                               ACCEPT WS-EVENT-DATE-YEAR
                               DISPLAY "Enter Event Month: "
                               ACCEPT WS-EVENT-DATE-MONTH
                               DISPLAY "Enter Event Day: "
                               ACCEPT WS-EVENT-DATE-DAY
                               DISPLAY "Enter Event Hour: "
                               ACCEPT WS-EVENT-TIME-HOUR
                               DISPLAY "Enter Event Minutes: "
                               ACCEPT WS-EVENT-TIME-MINUTES
                               DISPLAY "Enter Event Location: "
                               ACCEPT WS-EVENT-LOCATION
                               
                               MOVE WS-EVENT-NAME TO TEMP-NAME
                               MOVE WS-EVENT-DATE-YEAR TO TEMP-DATE-YEAR
                               MOVE WS-EVENT-DATE-MONTH TO TEMP-DATE-MONTH
                               MOVE WS-EVENT-DATE-DAY TO TEMP-DATE-DAY
                               MOVE WS-EVENT-TIME-HOUR TO TEMP-TIME-HOUR
                               MOVE WS-EVENT-TIME-MINUTES TO TEMP-TIME-MINUTES
                               MOVE WS-EVENT-LOCATION TO TEMP-LOCATION
                           ELSE
                               MOVE EVENT-NAME TO TEMP-NAME
                               MOVE EVENT-DATE-YEAR TO TEMP-DATE-YEAR
                               MOVE EVENT-DATE-MONTH TO TEMP-DATE-MONTH
                               MOVE EVENT-DATE-DAY TO TEMP-DATE-DAY
                               MOVE EVENT-TIME-HOUR TO TEMP-TIME-HOUR
                               MOVE EVENT-TIME-MINUTES TO TEMP-TIME-MINUTES
                               MOVE EVENT-LOCATION TO TEMP-LOCATION
                           END-IF
                           WRITE TEMP-RECORD
                   END-READ
               END-PERFORM
               
               CLOSE EVENT-FILE
               CLOSE TEMP-FILE
               
               IF WS-FOUND-FLAG = 'N'
                   DISPLAY "Event not found."
               ELSE
                   PERFORM REPLACE-FILE
                   DISPLAY "Event updated successfully."
               END-IF
           END-IF
           
           MOVE 'Y' TO WS-CONTINUE.

       REPLACE-FILE.
           OPEN OUTPUT EVENT-FILE
           OPEN INPUT TEMP-FILE
           MOVE 'N' TO WS-EOF
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ TEMP-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE TEMP-NAME TO EVENT-NAME
                       MOVE TEMP-DATE-YEAR TO EVENT-DATE-YEAR
                       MOVE TEMP-DATE-MONTH TO EVENT-DATE-MONTH
                       MOVE TEMP-DATE-DAY TO EVENT-DATE-DAY
                       MOVE TEMP-TIME-HOUR TO EVENT-TIME-HOUR
                       MOVE TEMP-TIME-MINUTES TO EVENT-TIME-MINUTES
                       MOVE TEMP-LOCATION TO EVENT-LOCATION
                       WRITE EVENT-RECORD
               END-READ
           END-PERFORM
           
           CLOSE EVENT-FILE
           CLOSE TEMP-FILE.
