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
           SELECT PRODUCT ASSIGN TO "product.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "REPORT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD PRODUCT.
           01 PRODUCT-REC.
               05 PRO-ID PIC X(4).
               05 PRO-NAME PIC X(10).
               05 PRO-PRICE PIC 9(7)V99.

       FD REPORT-FILE.
           01 REPORT-REC PIC X(70).
       WORKING-STORAGE SECTION.

       01 WS-HEADER PIC X(40)

       VALUE "========== PRODUCT REPORT ==========".
       01 WS-DETAIL.
           05 FILLER PIC X(4) VALUE "ID: ".
           05 WS-PRO-ID PIC X(7).
           05 FILLER PIC X(2) VALUE ", ".
           05 FILLER PIC X(10) VALUE "NAME : ".
           05 WS-PRO-NAME PIC X(10).
           05 FILLER PIC X(2) VALUE ", ".
           05 FILLER PIC X(7) VALUE "PRICE: ".
           05 WS-PRO-PRICE-DISP PIC ZZZZ,ZZZ.99.

       01 WS-PRO-PRICE PIC 9(7)V99.
       01 WS-EOF-FLAG PIC A(1) VALUE "N".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT PRODUCT.
           OPEN OUTPUT REPORT-FILE.
           PERFORM UNTIL WS-EOF-FLAG = "Y"
           READ PRODUCT

             AT END

               MOVE "Y" TO WS-EOF-FLAG

               NOT AT END

                   MOVE PRO-ID TO WS-PRO-ID
                   MOVE PRO-NAME TO WS-PRO-NAME
                   MOVE PRO-PRICE TO WS-PRO-PRICE
                   MOVE WS-PRO-PRICE TO WS-PRO-PRICE-DISP
                   WRITE REPORT-REC FROM WS-DETAIL

           END-PERFORM.
               CLOSE PRODUCT.
               CLOSE REPORT-FILE.
       END PROGRAM YOUR-PROGRAM-NAME.
