      ******************************************************************
      * Author:KASIDIT
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT ACCOUNTS-FILE ASSIGN TO "accounts.dat"
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT ACCOUNTS-NEW-FILE ASSIGN TO "account-new-file.dat"
                   ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.

       FILE SECTION.
       FD ACCOUNTS-FILE.
       01 ACCOUNT-REC.
           05 ACC-NO       PIC X(10).
           05 ACC-PIN      PIC X(4).
           05 ACC-NAME     PIC X(20).
           05 ACC-BALAN    PIC 9(8)V99.


       FD ACCOUNTS-NEW-FILE.
           01 ACC-NEW-REC PIC X(44).

       WORKING-STORAGE SECTION.
       01 WS-EOF.
           05 WS-EOF-FLAG PIC A(1) VALUE "N".
               88 WS-END-OF-FILE VALUE 'Y'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT ACCOUNTS-FILE
           OPEN OUTPUT ACCOUNTS-NEW-FILE

            PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNTS-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       COMPUTE ACC-BALAN = ACC-BALAN * 1.015
                       WRITE ACC-NEW-REC FROM ACCOUNT-REC
               END-READ
           END-PERFORM.
               CLOSE ACCOUNTS-FILE
               CLOSE ACCOUNTS-NEW-FILE
               DISPLAY "SUCCESFULLY ADDED".
       END PROGRAM YOUR-PROGRAM-NAME.
