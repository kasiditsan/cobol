      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APP-LOG ASSIGN TO "app.log"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD APP-LOG.
          01 APP-REC PIC X(55).
       WORKING-STORAGE SECTION.
       01 WS-DETAIL.
           05 FILLER PIC X(11) VALUE "2025/10/06 ".
           05 FILLER PIC X(9) VALUE "- User ' ".
           05 WS-NAME PIC X(10).
           05 FILLER PIC X(15) VALUE "' logged in.".

       01 WS-STOP PIC X(1) VALUE "N".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-STOP = "Y"

            OPEN EXTEND APP-LOG
            DISPLAY "ENTER USERNAME"
            ACCEPT WS-NAME
            WRITE APP-REC FROM WS-DETAIL
            DISPLAY "WILL YOU STOP NOW?(Y/N)"
            ACCEPT WS-STOP
            CLOSE APP-LOG

           END-PERFORM.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
