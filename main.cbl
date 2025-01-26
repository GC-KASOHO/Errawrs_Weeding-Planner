       IDENTIFICATION DIVISION.
       PROGRAM-ID. OddEvenChecker.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM1        PIC 9(4).
       01  WS-NUM2        PIC 9(4).
       01  WS-COUNTER     PIC 9(4).
       01  WS-REMAINDER   PIC 9(4).
       01  WS-MSG         PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter first number: ".
           ACCEPT WS-NUM1.
           DISPLAY "Enter second number: ".
           ACCEPT WS-NUM2.
    
           MOVE WS-NUM1 TO WS-COUNTER.
           PERFORM UNTIL WS-COUNTER > WS-NUM2
               DIVIDE WS-COUNTER BY 2 GIVING WS-REMAINDER REMAINDER WS-REMAINDER
               IF WS-REMAINDER = 0
                   STRING "Number " DELIMITED BY SIZE
                          WS-COUNTER DELIMITED BY SIZE
                          " is EVEN" DELIMITED BY SIZE
                          INTO WS-MSG
                   DISPLAY WS-MSG
               ELSE
                   STRING "Number " DELIMITED BY SIZE
                          WS-COUNTER DELIMITED BY SIZE
                          " is ODD" DELIMITED BY SIZE
                          INTO WS-MSG
                   DISPLAY WS-MSG
               END-IF
               ADD 1 TO WS-COUNTER
           END-PERFORM.
    
           STOP RUN.


           