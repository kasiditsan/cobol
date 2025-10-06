      ******************************************************************
      * Author: KASIDIT
      * Date: 09/29/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-PROGRAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ACCOUNTS-FILE.
       01 ACCOUNT-REC.
           05 ACC-NO       PIC X(10).
           05 ACC-PIN      PIC X(4).
           05 ACC-NAME     PIC X(20).
           05 ACC-BALAN    PIC 9(8)V99.

       WORKING-STORAGE SECTION.

       01 WS-NEW-ACC-NO       PIC X(10).
       01 WS-NEW-ACC-PIN      PIC X(4).
       01 WS-NEW-ACC-NAME     PIC X(20).
       01 WS-NEW-ACC-BALAN  PIC 9(8)V99.

       01 WS-EOF-FLAG         PIC X VALUE "N".

       01 WS-ACCOUNT-REC.
           05 WS-ACC-NO       PIC X(10).
           05 WS-ACC-PIN      PIC X(4).
           05 WS-ACC-NAME     PIC X(20).
           05 WS-ACC-BALAN  PIC 9(8)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "===== ADD NEW USER TO ACCOUNTS ====="

           DISPLAY "ENTER NEW ACCOUNT NUMBER (10 CHAR): "
           ACCEPT WS-NEW-ACC-NO

           DISPLAY "ENTER PIN (4 DIGITS): "
           ACCEPT WS-NEW-ACC-PIN

           DISPLAY "ENTER NAME (20 CHAR): "
           ACCEPT WS-NEW-ACC-NAME

           DISPLAY "ENTER INITIAL BALANCE (NUMERIC): "
           ACCEPT WS-NEW-ACC-BALAN

              MOVE WS-NEW-ACC-NO      TO ACC-NO
               MOVE WS-NEW-ACC-PIN     TO ACC-PIN
               MOVE WS-NEW-ACC-NAME    TO ACC-NAME
               MOVE WS-NEW-ACC-BALAN TO ACC-BALAN

               OPEN OUTPUT ACCOUNTS-FILE
               WRITE ACCOUNT-REC
               CLOSE ACCOUNTS-FILE

               DISPLAY "NEW USER HAS BEEN ADDED"


           STOP RUN.



       END PROGRAM MY-PROGRAM.
