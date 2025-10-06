       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-ACCOUNTS ASSIGN TO "TEMP.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SLIP ASSIGN TO "SLIP.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS SLIP-STATUS.
           SELECT TRANSLOG ASSIGN TO "TRANSLOG.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNTS.
       01 ACCOUNT-REC PIC X(44).

       FD TEMP-ACCOUNTS.
       01 TEMP-ACCOUNT-REC PIC X(44).

       FD SLIP.
       01 SLIP-REC PIC X(80).

       FD TRANSLOG.
       01 TRANSLOG-REC.
          05 TRANS-ACC-NO    PIC X(10).
          05 TRANS-DATE      PIC 9(8).
          05 TRANS-TIME      PIC 9(6).
          05 TRANS-TYPE      PIC X.
          05 TRANS-AMOUNT    PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01 SLIP-STATUS PIC XX VALUE SPACES.
       01 WS-ACC-NO        PIC X(10).
       01 WS-DES-ACC-NO    PIC X(10).
       01 WS-ACC-PIN       PIC X(4).
       01 WS-ACC-NAME      PIC X(20).
       01 WS-ACC-BALANCE   PIC 9(8)V99.
       01 FOUND-FLAG       PIC X VALUE "N".
       01 DES-ACC-FOUND    PIC X VALUE "N".
       01 CHOICE           PIC 9.
       01 AMOUNT           PIC 9(8)V99.
       01 EOF-FLAG         PIC X VALUE "N".
       01 WS-ACCOUNT-REC.
          05 ACC-NO       PIC X(10).
          05 ACC-PIN      PIC X(4).
          05 ACC-NAME     PIC X(20).
          05 ACC-BALANCE  PIC 9(8)V99.

       PROCEDURE DIVISION.
       MAIN-SECTION.

           PERFORM LOGIN-PROCESS
           PERFORM UNTIL CHOICE = 9
              DISPLAY "1. CHECK BALANCE"
              DISPLAY "2. DEPOSIT"
              DISPLAY "3. WITHDRAW"
              DISPLAY "4. TRANSFER"
              DISPLAY "9. EXIT"
              ACCEPT CHOICE
              EVALUATE CHOICE
                 WHEN 1 PERFORM CHECK-BALANCE
                 WHEN 2 PERFORM DEPOSIT
                 WHEN 3 PERFORM WITHDRAW
                 WHEN 4 PERFORM TRANSFER
                 WHEN 9 DISPLAY "GOODBYE!"
                 WHEN OTHER DISPLAY "INVALID OPTION"
              END-EVALUATE
           END-PERFORM
           STOP RUN.


       LOGIN-PROCESS.
           PERFORM UNTIL FOUND-FLAG = "Y"
              DISPLAY "ENTER ACCOUNT NO: "
              ACCEPT WS-ACC-NO
              DISPLAY "ENTER PIN: "
              ACCEPT WS-ACC-PIN
              MOVE "N" TO FOUND-FLAG
              MOVE "N" TO EOF-FLAG
              OPEN INPUT ACCOUNTS
              PERFORM UNTIL EOF-FLAG = "Y" OR FOUND-FLAG = "Y"
                 READ ACCOUNTS
                    AT END MOVE "Y" TO EOF-FLAG
                    NOT AT END
                       MOVE ACCOUNT-REC TO WS-ACCOUNT-REC
                       IF ACC-NO = WS-ACC-NO AND ACC-PIN = WS-ACC-PIN
                          MOVE "Y" TO FOUND-FLAG
                          MOVE ACC-NAME TO WS-ACC-NAME
                          MOVE ACC-BALANCE TO WS-ACC-BALANCE
                          DISPLAY "WELCOME " WS-ACC-NAME
                       END-IF
                 END-READ
              END-PERFORM
              CLOSE ACCOUNTS
              IF FOUND-FLAG = "N"
                 DISPLAY "INVALID ACCOUNT OR PIN."
              END-IF
           END-PERFORM.


       CHECK-BALANCE.
           PERFORM SLIP-CHECK
           DISPLAY "CURRENT BALANCE: " WS-ACC-BALANCE.


       DEPOSIT.
           DISPLAY "ENTER AMOUNT TO DEPOSIT: "
           ACCEPT AMOUNT
           ADD AMOUNT TO WS-ACC-BALANCE
           PERFORM UPDATE-ACCOUNT
           PERFORM SLIP-DEPOSIT

           MOVE "D" TO TRANS-TYPE
           PERFORM WRITE-TRANSLOG

           DISPLAY "NEW BALANCE: " WS-ACC-BALANCE.


       WITHDRAW.
           DISPLAY "ENTER AMOUNT TO WITHDRAW: "
           ACCEPT AMOUNT
           IF AMOUNT > WS-ACC-BALANCE
              DISPLAY "INSUFFICIENT FUNDS!"
           ELSE
              SUBTRACT AMOUNT FROM WS-ACC-BALANCE
              PERFORM UPDATE-ACCOUNT
              PERFORM SLIP-WITHDRAW

              MOVE "W" TO TRANS-TYPE
              PERFORM WRITE-TRANSLOG

              DISPLAY "NEW BALANCE: " WS-ACC-BALANCE
           END-IF.

       TRANSFER.
           DISPLAY "ENTER DESTINATION ACCOUNT TO TRANSFER: "
           ACCEPT WS-DES-ACC-NO
           DISPLAY "ENTER AMOUNT TO TRANSFER: "
           ACCEPT AMOUNT
           IF AMOUNT > WS-ACC-BALANCE
              DISPLAY "INSUFFICIENT FUNDS!"
           ELSE
              PERFORM VALIDATE-DESTINATION-ACCOUNT
              IF DES-ACC-FOUND = "Y"
                 SUBTRACT AMOUNT FROM WS-ACC-BALANCE
                 PERFORM UPDATE-ACCOUNT
                 PERFORM UPDATE-DESTINATION-ACCOUNT
                 PERFORM SLIP-TRANSFER
                 MOVE "T" TO TRANS-TYPE
                 PERFORM WRITE-TRANSLOG
                 DISPLAY "TRANSFER SUCCESSFUL."
                 DISPLAY "NEW BALANCE: " WS-ACC-BALANCE
              ELSE
                 DISPLAY "DESTINATION ACCOUNT NOT FOUND!"
              END-IF
           END-IF.

       VALIDATE-DESTINATION-ACCOUNT.
           MOVE "N" TO DES-ACC-FOUND
           MOVE "N" TO EOF-FLAG
           OPEN INPUT ACCOUNTS
           PERFORM UNTIL EOF-FLAG = "Y" OR DES-ACC-FOUND = "Y"
               READ ACCOUNTS
                   AT END MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       MOVE ACCOUNT-REC TO WS-ACCOUNT-REC
                       IF ACC-NO = WS-DES-ACC-NO
                          MOVE "Y" TO DES-ACC-FOUND
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS.

       UPDATE-ACCOUNT.
           OPEN INPUT ACCOUNTS
           OPEN OUTPUT TEMP-ACCOUNTS
           MOVE "N" TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = "Y"
               READ ACCOUNTS
                   AT END MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       MOVE ACCOUNT-REC TO WS-ACCOUNT-REC
                       IF ACC-NO = WS-ACC-NO
                          MOVE WS-ACC-BALANCE TO ACC-BALANCE
                       END-IF
                       MOVE WS-ACCOUNT-REC TO TEMP-ACCOUNT-REC
                       WRITE TEMP-ACCOUNT-REC
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           CLOSE TEMP-ACCOUNTS

           CALL "SYSTEM" USING "rename TEMP.DAT ACCOUNTS.DAT".

       UPDATE-DESTINATION-ACCOUNT.
           OPEN INPUT ACCOUNTS
           OPEN OUTPUT TEMP-ACCOUNTS
           MOVE "N" TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = "Y"
               READ ACCOUNTS
                   AT END MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       MOVE ACCOUNT-REC TO WS-ACCOUNT-REC
                       IF ACC-NO = WS-DES-ACC-NO
                          ADD AMOUNT TO ACC-BALANCE
                       END-IF
                       MOVE WS-ACCOUNT-REC TO TEMP-ACCOUNT-REC
                       WRITE TEMP-ACCOUNT-REC
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS
           CLOSE TEMP-ACCOUNTS

           CALL "SYSTEM" USING "rename TEMP.DAT ACCOUNTS.DAT".

       SLIP-DEPOSIT.
           OPEN EXTEND SLIP
           PERFORM WRITE-SLIP-HEADER
           MOVE "TRANSACTION: DEPOSIT" TO SLIP-REC
           WRITE SLIP-REC
           STRING "AMOUNT: + " AMOUNT " BAHT"
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           STRING "REMAINING BALANCE: " WS-ACC-BALANCE " BAHT"
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           PERFORM WRITE-SLIP-FOOTER
           CLOSE SLIP.

       SLIP-WITHDRAW.
           OPEN EXTEND SLIP
           PERFORM WRITE-SLIP-HEADER
           MOVE "TRANSACTION: WITHDRAWAL" TO SLIP-REC
           WRITE SLIP-REC
           STRING "AMOUNT: - " AMOUNT " BAHT"
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           STRING "REMAINING BALANCE: " WS-ACC-BALANCE " BAHT"
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           PERFORM WRITE-SLIP-FOOTER
           CLOSE SLIP.

       SLIP-TRANSFER.
           OPEN EXTEND SLIP
           PERFORM WRITE-SLIP-HEADER
           STRING "FROM ACCOUNT NO: " WS-ACC-NO
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           STRING "TO ACCOUNT NO: " WS-DES-ACC-NO
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           MOVE "TRANSACTION: TRANSFER" TO SLIP-REC
           WRITE SLIP-REC
           STRING "AMOUNT: - " AMOUNT " BAHT"
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           STRING "REMAINING BALANCE: " WS-ACC-BALANCE " BAHT"
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           PERFORM WRITE-SLIP-FOOTER
           CLOSE SLIP.

       SLIP-CHECK.
           OPEN EXTEND SLIP
           PERFORM WRITE-SLIP-HEADER
           MOVE "TRANSACTION: CHECK BALANCE" TO SLIP-REC
           WRITE SLIP-REC
           STRING "REMAINING BALANCE: " WS-ACC-BALANCE " BAHT"
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           PERFORM WRITE-SLIP-FOOTER
           CLOSE SLIP.

       WRITE-SLIP-HEADER.
           MOVE "--------------------------------------------" TO
           SLIP-REC
           WRITE SLIP-REC
           MOVE "   *** TRANSACTION SLIP ***" TO SLIP-REC
           WRITE SLIP-REC
           MOVE "--------------------------------------------" TO
           SLIP-REC
           WRITE SLIP-REC
           MOVE SPACES TO SLIP-REC
           WRITE SLIP-REC
           STRING "ACCOUNT NO: " WS-ACC-NO
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC
           STRING "DATE: " FUNCTION CURRENT-DATE(1:4) "/"
                  FUNCTION CURRENT-DATE(5:2) "/"
                  FUNCTION CURRENT-DATE(7:2)
                  " TIME: " FUNCTION CURRENT-DATE(9:2) ":"
                  FUNCTION CURRENT-DATE(11:2) ":"
                  FUNCTION CURRENT-DATE(13:2)
                  DELIMITED BY SIZE
                  INTO SLIP-REC
           WRITE SLIP-REC.

       WRITE-SLIP-FOOTER.
           MOVE SPACES TO SLIP-REC
           WRITE SLIP-REC
           MOVE "   *** THANK YOU ***" TO SLIP-REC
           WRITE SLIP-REC
           MOVE "--------------------------------------------" TO
           SLIP-REC
           WRITE SLIP-REC.

       WRITE-TRANSLOG.
           OPEN EXTEND TRANSLOG
           MOVE WS-ACC-NO TO TRANS-ACC-NO
           MOVE FUNCTION CURRENT-DATE(1:8) TO TRANS-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO TRANS-TIME
           MOVE AMOUNT TO TRANS-AMOUNT
           WRITE TRANSLOG-REC
           CLOSE TRANSLOG.
