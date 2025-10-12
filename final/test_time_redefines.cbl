       IDENTIFICATION DIVISION.
       PROGRAM-ID. new.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT TEST-TIME ASSIGN TO "TESTTIME.txt"
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-CURRENT-DATE-DATA.
          05 WS-CURRENT-DATE PIC X(21).
          05 WS-DATE-TIME REDEFINES WS-CURRENT-DATE.
             10 WS-YEAR        PIC 9(4).
             10 WS-MONTH       PIC 9(2).
             10 WS-DAY         PIC 9(2).
             10 WS-HOUR        PIC 9(2).
             10 WS-MINUTE      PIC 9(2).
             10 WS-SECOND      PIC 9(2).
.          05 WS-DATE-TIME-DIS.
             10 WS-YEAR-DIS        PIC 9(4).
             10 FILLER PIC X(1) VALUE "/".
             10 WS-MONTH-DIS       PIC 9(2).
             10 FILLER PIC X(1) VALUE "/".
             10 WS-DAY-DIS         PIC 9(2).
             10 FILLER PIC X(2) VALUE "  ".
             10 WS-HOUR-DIS        PIC 9(2).
             10 FILLER PIC X(1) VALUE ":".
             10 WS-MINUTE-DIS      PIC 9(2).
             10 FILLER PIC X(1) VALUE ":".
             10 WS-SECOND-DIS      PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           DISPLAY WS-CURRENT-DATE
           DISPLAY "YEAR   : " WS-YEAR
           DISPLAY "MONTH  : " WS-MONTH
           DISPLAY "DAY    : " WS-DAY
           DISPLAY "HOUR   : " WS-HOUR
           DISPLAY "MINUTE : " WS-MINUTE
           DISPLAY "SECOND : " WS-SECOND
           MOVE WS-YEAR TO WS-YEAR-DIS
           MOVE WS-MONTH TO WS-MONTH-DIS
           MOVE WS-DAY TO WS-DAY-DIS
           MOVE WS-HOUR TO WS-HOUR-DIS
           MOVE WS-MINUTE TO WS-MINUTE-DIS
           MOVE WS-SECOND TO WS-SECOND-DIS
           DISPLAY WS-DATE-TIME-DIS

           STOP RUN.
       END PROGRAM new.
