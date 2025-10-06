      ******************************************************************
      * Author:KASIDIT
      * Date:
      * Purpose:emp
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPSAL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEES-FILE ASSIGN TO "employees.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEES-FILE.
       01  EMPLOYEES-RECORD.
           05 EMP-ID          PIC X(4).
           05 EMP-NAME        PIC X(15).
           05 EMP-DEPARTMENT  PIC X(10).
           05 EMP-SALARY      PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG        PIC A(1) VALUE 'N'.
           88 WS-END-OF-FILE           VALUE 'Y'.
       01  WS-CALCULATIONS.
           05 WS-TOTAL-SALARY PIC 9(8)V99 VALUE ZERO.

       01  WS-DISPLAY-FIELDS.
           05 DISP-TOTAL-SALARY PIC $ZZZ,ZZZ,ZZ9.99.
           05 EMP-SALARY-DIS      PIC $$$,$$$,$$$.99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT EMPLOYEES-FILE.

            PERFORM UNTIL WS-END-OF-FILE
               READ EMPLOYEES-FILE
                AT END
                   SET WS-END-OF-FILE TO TRUE
                NOT AT END

                   PERFORM PROCESS-SINGLE-RECORD
               END-READ
            END-PERFORM.
                CLOSE EMPLOYEES-FILE.
           PERFORM DISPLAY-SUMMARY-REPORT.
            STOP RUN.
       PROCESS-SINGLE-RECORD.
           MOVE EMP-SALARY TO EMP-SALARY-DIS.
           DISPLAY "PROCESSING :" EMP-NAME "| SALARY: " EMP-SALARY-DIS.
           ADD EMP-SALARY TO WS-TOTAL-SALARY.

       DISPLAY-SUMMARY-REPORT.
           DISPLAY " "
           DISPLAY "--- END OF FILE ---".
           DISPLAY "CALCULATION COMPLETE".
           MOVE WS-TOTAL-SALARY TO DISP-TOTAL-SALARY.
           DISPLAY "TOTAL SALARY OF ALL EMPLOYEES: " DISP-TOTAL-SALARY.
       END PROGRAM EMPSAL.
