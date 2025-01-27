       IDENTIFICATION DIVISION.
       PROGRAM-ID. TASK-MANAGEMENT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TASK-FILE
               ASSIGN TO "tasks.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TASK-FILE
           RECORDING MODE IS F.
       01  T-REC.
           05 T-LINE      PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-TASK.
           05 T-ID        PIC 9(5).
           05 T-NAME      PIC X(15).
           05 T-DESC      PIC X(20).
           05 T-DATE.
               10 T-YEAR   PIC 9(4).
               10 T-MON    PIC 9(2).
               10 T-DAY    PIC 9(2).
           05 T-TIME.
               10 T-HOUR  PIC 9(2).
               10 T-MIN   PIC 9(2).
           05 T-STATUS    PIC X(1).

       01 WS-HEAD.
           05 FILLER      PIC X(5)  VALUE "ID".
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 FILLER      PIC X(15) VALUE "NAME".
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 FILLER      PIC X(20) VALUE "DESCRIPTION".
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 FILLER      PIC X(10) VALUE "DUE DATE".
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 FILLER      PIC X(5)  VALUE "TIME".
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 FILLER      PIC X(8)  VALUE "STATUS".

       01 WS-DETAIL.
           05 D-ID        PIC X(5).
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 D-NAME      PIC X(15).
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 D-DESC      PIC X(20).
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 D-DATE      PIC X(10).
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 D-TIME      PIC X(5).
           05 FILLER      PIC X(3)  VALUE SPACES.
           05 D-STATUS    PIC X(8).

       01 WS-CSV          PIC X(200).
       01 WS-EOF          PIC X(1).
       01 WS-OPT          PIC 9.
       01 WS-TMP-ID       PIC 9(5).
       01 WS-FOUND        PIC X(1).
       01 WS-NEXT         PIC 9(5) VALUE 1.
       01 WS-FS           PIC X(2).
       01 WS-DV           PIC X(1).
       01 WS-TV           PIC X(1).
       01 WS-CNT          PIC 9(5) VALUE 0.
       01 WS-SEP          PIC X(90) VALUE ALL "=".
       01 WS-SUBSEP       PIC X(90) VALUE ALL "-".

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INIT-FILE
           PERFORM MENU-OPT
           STOP RUN.

       INIT-FILE.
           OPEN INPUT TASK-FILE
           IF WS-FS = "35"
               OPEN OUTPUT TASK-FILE
               WRITE T-REC FROM WS-SEP
               WRITE T-REC FROM WS-HEAD
               WRITE T-REC FROM WS-SUBSEP
               CLOSE TASK-FILE
           ELSE
               CLOSE TASK-FILE
           END-IF.

       MENU-OPT.
           PERFORM UNTIL WS-OPT = 5
               DISPLAY SPACE
               DISPLAY WS-SEP
               DISPLAY "                        TASK MANAGER"
               DISPLAY WS-SEP
               DISPLAY "1. View Tasks"
               DISPLAY "2. Add Task"
               DISPLAY "3. Edit Task"
               DISPLAY "4. Mark Task as Complete"
               DISPLAY "5. Exit"
               DISPLAY WS-SUBSEP
               DISPLAY "Enter choice: " WITH NO ADVANCING
               ACCEPT WS-OPT
               EVALUATE WS-OPT
                   WHEN 1
                       PERFORM VIEW-TASKS
                   WHEN 2
                       PERFORM ADD-TASK
                   WHEN 3
                       PERFORM EDIT-TASK
                   WHEN 4
                       PERFORM MARK-COMP
                   WHEN 5
                       DISPLAY SPACE
                       DISPLAY "Exiting..."
                       DISPLAY SPACE
                   WHEN OTHER
                       DISPLAY SPACE
                       DISPLAY "Invalid option. Try again."
                       DISPLAY SPACE
               END-EVALUATE
           END-PERFORM.

       COUNT-TASKS.
           MOVE 0 TO WS-CNT
           OPEN INPUT TASK-FILE
           IF WS-FS = "00"
               MOVE "N" TO WS-EOF
               PERFORM UNTIL WS-EOF = "Y"
                   READ TASK-FILE
                       AT END
                           MOVE "Y" TO WS-EOF
                       NOT AT END
                           ADD 1 TO WS-CNT
                   END-READ
               END-PERFORM
               SUBTRACT 3 FROM WS-CNT
               CLOSE TASK-FILE
           END-IF.

       VIEW-TASKS.
           PERFORM COUNT-TASKS
           IF WS-CNT = 0
               DISPLAY SPACE
               DISPLAY "No tasks found. Please add tasks first."
               DISPLAY SPACE
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY SPACE
           DISPLAY WS-SEP
           DISPLAY WS-HEAD
           DISPLAY WS-SUBSEP
           
           OPEN INPUT TASK-FILE
           MOVE "N" TO WS-EOF
           PERFORM READ-HEADER
           PERFORM UNTIL WS-EOF = "Y"
               READ TASK-FILE INTO WS-CSV
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-CSV
                       PERFORM FORMAT-LINE
                       DISPLAY WS-DETAIL
               END-READ
           END-PERFORM
           DISPLAY WS-SEP
           DISPLAY SPACE
           CLOSE TASK-FILE.

       READ-HEADER.
           READ TASK-FILE
           READ TASK-FILE
           READ TASK-FILE.

       PARSE-CSV.
           UNSTRING WS-CSV DELIMITED BY ","
               INTO T-ID
                    T-NAME
                    T-DESC
                    T-YEAR
                    T-MON
                    T-DAY
                    T-HOUR
                    T-MIN
                    T-STATUS.

       CREATE-CSV.
           STRING  T-ID          DELIMITED BY SIZE
                  ","           DELIMITED BY SIZE
                  T-NAME        DELIMITED BY SPACE
                  ","           DELIMITED BY SIZE
                  T-DESC        DELIMITED BY SPACE
                  ","           DELIMITED BY SIZE
                  T-YEAR        DELIMITED BY SIZE
                  ","           DELIMITED BY SIZE
                  T-MON         DELIMITED BY SIZE
                  ","           DELIMITED BY SIZE
                  T-DAY         DELIMITED BY SIZE
                  ","           DELIMITED BY SIZE
                  T-HOUR        DELIMITED BY SIZE
                  ","           DELIMITED BY SIZE
                  T-MIN         DELIMITED BY SIZE
                  ","           DELIMITED BY SIZE
                  T-STATUS      DELIMITED BY SIZE
               INTO WS-CSV.

       FORMAT-LINE.
           MOVE SPACES TO D-ID
           MOVE SPACES TO D-NAME
           MOVE SPACES TO D-DESC
           MOVE SPACES TO D-DATE
           MOVE SPACES TO D-TIME
           MOVE SPACES TO D-STATUS
           
           MOVE T-ID TO D-ID
           MOVE T-NAME TO D-NAME
           MOVE T-DESC TO D-DESC
           STRING T-YEAR "/" T-MON "/" T-DAY
               DELIMITED BY SIZE INTO D-DATE
           STRING T-HOUR ":" T-MIN
               DELIMITED BY SIZE INTO D-TIME
           IF T-STATUS = "C"
               MOVE "Complete" TO D-STATUS
           ELSE
               MOVE "Pending" TO D-STATUS
           END-IF.

       GET-DATE.
           MOVE "N" TO WS-DV
           PERFORM UNTIL WS-DV = "Y"
               DISPLAY "Enter Due Date Year (YYYY): " WITH NO ADVANCING
               ACCEPT T-YEAR
               DISPLAY "Enter Due Date Month (MM): " WITH NO ADVANCING
               ACCEPT T-MON
               DISPLAY "Enter Due Date Day (DD): " WITH NO ADVANCING
               ACCEPT T-DAY
               IF T-YEAR >= 2024 AND
                  T-MON >= 1 AND T-MON <= 12 AND
                  T-DAY >= 1 AND T-DAY <= 31
                   MOVE "Y" TO WS-DV
               ELSE
                   DISPLAY "Invalid date. Try again."
               END-IF
           END-PERFORM.

       GET-TIME.
           MOVE "N" TO WS-TV
           PERFORM UNTIL WS-TV = "Y"
               DISPLAY "Enter Time Hour (00-23): " WITH NO ADVANCING
               ACCEPT T-HOUR
               DISPLAY "Enter Time Minutes (00-59): " WITH NO ADVANCING
               ACCEPT T-MIN
               IF T-HOUR >= 0 AND T-HOUR <= 23 AND
                  T-MIN >= 0 AND T-MIN <= 59
                   MOVE "Y" TO WS-TV
               ELSE
                   DISPLAY "Invalid time. Try again."
               END-IF
           END-PERFORM.

       ADD-TASK.
           OPEN EXTEND TASK-FILE
           IF WS-FS NOT = "00"
               DISPLAY "Cannot open file for writing."
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY SPACE
           MOVE WS-NEXT TO T-ID
           ADD 1 TO WS-NEXT
           
           DISPLAY "Enter Task Name (max 15 chars): " 
               WITH NO ADVANCING
           ACCEPT T-NAME
           
           DISPLAY "Enter Task Description (max 20 chars): " 
               WITH NO ADVANCING
           ACCEPT T-DESC
           
           PERFORM GET-DATE
           PERFORM GET-TIME
           
           MOVE "P" TO T-STATUS
           
           PERFORM CREATE-CSV
           PERFORM FORMAT-LINE
           WRITE T-REC FROM WS-DETAIL
           
           IF WS-FS = "00"
               DISPLAY SPACE
               DISPLAY "Task added successfully!"
               DISPLAY SPACE
           ELSE
               DISPLAY "Error writing task. Status: " WS-FS
           END-IF
           CLOSE TASK-FILE.

       EDIT-TASK.
           PERFORM COUNT-TASKS
           IF WS-CNT = 0
               DISPLAY SPACE
               DISPLAY "No tasks found. Please add tasks first."
               DISPLAY SPACE
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY SPACE
           OPEN I-O TASK-FILE
           DISPLAY "Enter Task ID to edit: " WITH NO ADVANCING
           ACCEPT WS-TMP-ID
           MOVE "N" TO WS-FOUND
           MOVE "N" TO WS-EOF
           PERFORM READ-HEADER
           PERFORM UNTIL WS-EOF = "Y"
               READ TASK-FILE INTO WS-CSV
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-CSV
                       IF T-ID = WS-TMP-ID
                           MOVE "Y" TO WS-FOUND
                           DISPLAY "Enter Task Name (max 15 chars): "
                               WITH NO ADVANCING
                           ACCEPT T-NAME
                           DISPLAY "Enter Description (max 20 chars): "
                               WITH NO ADVANCING
                           ACCEPT T-DESC
                           PERFORM GET-DATE
                           PERFORM GET-TIME
                           PERFORM CREATE-CSV
                           PERFORM FORMAT-LINE
                           REWRITE T-REC FROM WS-DETAIL
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TASK-FILE
           DISPLAY SPACE
           IF WS-FOUND = "N"
               DISPLAY "Task not found!"
           ELSE
               DISPLAY "Task updated successfully!"
           END-IF
           DISPLAY SPACE.

       MARK-COMP.
           PERFORM COUNT-TASKS
           IF WS-CNT = 0
               DISPLAY SPACE
               DISPLAY "No tasks found. Please add tasks first."
               DISPLAY SPACE
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY SPACE
           OPEN I-O TASK-FILE
           DISPLAY "Enter Task ID to mark as complete: " NO ADVANCING
           ACCEPT WS-TMP-ID
           MOVE "N" TO WS-FOUND
           MOVE "N" TO WS-EOF
           PERFORM READ-HEADER
           PERFORM UNTIL WS-EOF = "Y"
               READ TASK-FILE INTO WS-CSV
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-CSV
                       IF T-ID = WS-TMP-ID
                           MOVE "Y" TO WS-FOUND
                           MOVE "C" TO T-STATUS
                           PERFORM CREATE-CSV
                           PERFORM FORMAT-LINE
                           REWRITE T-REC FROM WS-DETAIL
                           DISPLAY SPACE
                           DISPLAY "Task marked as complete!"
                           DISPLAY SPACE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TASK-FILE
           IF WS-FOUND = "N"
               DISPLAY SPACE
               DISPLAY "Task not found!"
               DISPLAY SPACE
           END-IF.

