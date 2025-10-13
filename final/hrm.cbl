******************************************************************
      * Author:
      * Date:
      * Purpose: HR Management System
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EMP-MASTER ASSIGN TO "EMPLOYEE.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EMP-ID.
           SELECT INPUT-SEQ ASSIGN TO "INPUT_SEQ.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT EMP-REPORT ASSIGN TO "EMPLOYEE_REPORT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SUMMARY-REPORT ASSIGN TO "SUMMARY_REPORT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BACKUP-FILE ASSIGN TO "BACKUP_SEQ.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT HR-LOG ASSIGN TO "HR_LOG.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT EMP-SORTED ASSIGN TO 'EMP-SORTED.DAT'
               ORGANIZATION IS SEQUENTIAL.
           SELECT TEMP-SORT ASSIGN TO 'TEMP-SORT.tmp'
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       SD TEMP-SORT.
       01 TEMP-REC PIC X(44).

       FD EMP-SORTED.
       01 EMP-SORTED-REC.
           05 EMP-ID-S       PIC 9(5).
           05 EMP-NAME-S     PIC X(20).
           05 DEPT-NAME-S    PIC X(10).
           05 SALARY-S       PIC 9(7)V99.

       FD EMP-MASTER.
       01 EMP-RECORD.
           05 EMP-ID       PIC 9(5).
           05 EMP-NAME     PIC X(20).
           05 DEPT-NAME    PIC X(10).
           05 SALARY       PIC 9(7)V99.

       FD INPUT-SEQ.
       01 SEQ-RECORD.
           05 S-EMP-ID     PIC 9(5).
           05 S-EMP-NAME   PIC X(20).
           05 S-DEPT-NAME  PIC X(10).
           05 S-SALARY     PIC 9(7)V99.

       FD EMP-REPORT.
       01 REPORT-RECORD PIC X(80).

       FD SUMMARY-REPORT.
       01 SUMMARY-REC PIC X(80).

       FD BACKUP-FILE.
       01 BACKUP-RECORD PIC X(80).

       FD HR-LOG.
       01 LOG-RECORD PIC X(80).


       WORKING-STORAGE SECTION.

       01 EOF-FLAG       PIC X VALUE 'N'.
       01 WS-OPTION      PIC 9 VALUE 0.
       01 COUNT_N        PIC 9(5) VALUE 0.
       01 SUM-SALARY     PIC 9(7)V99 VALUE 0.
       01 PREV-DEPT      PIC X(10) VALUE SPACES.
       01 WS-ACTION      PIC X(10) VALUE SPACES.
       01 WS-LOG-LINE    PIC X(80) VALUE SPACES.
       01 WS-PREV-DEPT-ID PIC X(10).
       01 WS-SUBTOTAL-SALARY PIC 9(7)V99.
       01 WS-TOTAL-SALARY PIC 9(7)V99.
       01 WS-SUBTOTAL-DEPT PIC 9(2).
       01 WS-TOTAL-DEPT PIC 9(3).
       01 WS-USER-ROLE PIC X(10) VALUE SPACES.

       01 TEMP-BACKUP-REC.
               05  BK-EMP-ID     PIC X(5).
               05  BK-EMP-NAME   PIC X(30).
               05  BK-DEPT-NAME  PIC X(20).
               05  BK-SALARY     PIC 9(7)V99.

           01  BK-REC-REDEF REDEFINES TEMP-BACKUP-REC PIC X(80).

       01 WS-EOF-FLAG        PIC A(1) VALUE 'N'.
          88 WS-END-OF-FILE  VALUE 'Y'.

       01 WS-CURRENT-DATE PIC X(21).

       01 WS-DATE-TIME REDEFINES WS-CURRENT-DATE.
             10 WS-YEAR        PIC 9(4).
             10 WS-MONTH       PIC 9(2).
             10 WS-DAY         PIC 9(2).
             10 WS-HOUR        PIC 9(2).
             10 WS-MINUTE      PIC 9(2).
             10 WS-SECOND      PIC 9(2).
       01 WS-DATE.
           15 WS-YEAR-DIS        PIC 9(4).
           15 FILLER PIC X(1) VALUE "/".
           15 WS-MONTH-DIS       PIC 9(2).
           15 FILLER PIC X(1) VALUE "/".
           15 WS-DAY-DIS         PIC 9(2).
           15 FILLER PIC X(2) VALUE "  ".
           15 WS-HOUR-DIS        PIC 9(2).
           15 FILLER PIC X(1) VALUE ":".
           15 WS-MINUTE-DIS      PIC 9(2).
           15 FILLER PIC X(1) VALUE ":".
           15 WS-SECOND-DIS      PIC 9(2).
       01 EMP-REPORT-DETAIL.
           05 HEADER.
               10 COM-NAME PIC X(15) VALUE "EMPLOYEE REPORT".
               10 FILLER PIC X(10) VALUE ALL SPACES.
.              10 WS-DATE-DIS PIC X(20).
               10 FILLER PIC X(10) VALUE ALL SPACES.
               10 WS-PAGE PIC X(10) VALUE "  PAGE : 1".
           05  SUBHEADER.
               10  FILLER          PIC X(10)  VALUE "EMP ID".
               10  FILLER          PIC X(10)  VALUE ALL SPACES.
               10  FILLER          PIC X(10)  VALUE "NAME".
               10  FILLER          PIC X(10)  VALUE ALL SPACES.
               10  FILLER          PIC X(10)  VALUE "  DEPT".
               10  FILLER          PIC X(16)  VALUE ALL SPACES.
               10  FILLER          PIC X(10)  VALUE "SALARY".
           05  DASH-LINE.
               10  FILLER          PIC X(80)  VALUE ALL "-".
           05  DETAIL-LINE.
               10  WS-EMP-ID       PIC 9(5)   .
               10  FILLER          PIC X(10)   VALUE "       ".
               10  WS-EMP-NAME     PIC X(20)   .
               10  FILLER          PIC X(10)   VALUE "  ".
               10  WS-DEPT-NAME    PIC X(10)   .
               10  FILLER          PIC X(10)   VALUE "       ".
               10  WS-SALARY       PIC Z,ZZZ,ZZZ.99 .
           05  EQUAL-LINE.
               10  FILLER          PIC X(80)   VALUE ALL "=".
           05  FOOTER.
               10  FILLER          PIC X(80)   VALUE ALL SPACES.
               10  FILLER          PIC X(20)   VALUE "END OF REPORT".

       01 SUMMARY-REPORT-DEATAIL.
           05 HEADER-S.
               10 COM-NAME-S PIC X(15) VALUE "EMPLOYEE REPORT".
               10 FILLER PIC X(10) VALUE ALL SPACES.
.              10 WS-DATE-DIS-S PIC X(20).
               10 FILLER PIC X(10) VALUE ALL SPACES.
               10 WS-PAGE-S PIC X(10) VALUE "  PAGE : 1".

           05  SUBHEADER-S.
               10  FILLER          PIC X(10)  VALUE "DEPT".
               10  FILLER          PIC X(10)  VALUE ALL SPACES.
               10  FILLER          PIC X(10)  VALUE "EMP AMOUNT".
               10  FILLER          PIC X(10)  VALUE ALL SPACES.
               10  FILLER          PIC X(20)  VALUE "TOTAL DEPT SALARY".

           05  DASH-LINE-SPACE.
               10  FILLER          PIC X(10)  VALUE ALL "-".
               10  FILLER          PIC X(10)  VALUE ALL SPACES.
               10  FILLER          PIC X(10)  VALUE ALL "-".
               10  FILLER          PIC X(10)  VALUE ALL SPACES.
               10  FILLER          PIC X(20)  VALUE ALL "-".
           05  DASH-LINE-ALL.
               10 FILLER           PIC X(50)  VALUE ALL "- ".
           05  EQUAL-LINE-S.
               10  FILLER          PIC X(80)   VALUE ALL "=".
           05  DETAIL-LINE-S.
               10  WS-DEPT       PIC X(5)   .
               10  FILLER          PIC X(15)   VALUE SPACES.
               10  WS-DEPT-AMOUNT     PIC X(13)   .
               10  FILLER          PIC X(10)   VALUE SPACES.
               10  SUBTOTAL-SALARY    PIC Z,ZZZ,ZZZ.99   .
           05 DETAIL-TOTAL.
               10 FILLER PIC X(7) VALUE "TOTAL:" .
               10 FILLER PIC X(13) VALUE ALL SPACES.
               10 TOTAL-DEPT PIC 9(3)  .
               10 FILLER PIC X(20) VALUE ALL SPACES.
               10 TOTAL-SALARY PIC Z,ZZZ,ZZZ.99  .
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "Enter role (HR / ADMIN):"
           ACCEPT WS-USER-ROLE.

           PERFORM MENU-LOOP
           STOP RUN.

       MENU-LOOP.
           DISPLAY "1. Add Employee (HR)"
           DISPLAY "2. Edit Employee (HR)"
           DISPLAY "3. Delete Employee (HR)"
           DISPLAY "4. Search Employee (HR / ADMIN)"
           DISPLAY "5. Employee Report (HR / ADMIN)"
           DISPLAY "6. Summary Report (HR / ADMIN)"
           DISPLAY "7. Backup Employee File (HR / ADMIN)"
           DISPLAY "8. Batch Update Employees (HR)"
           DISPLAY "9. Exit"
           ACCEPT WS-OPTION

           EVALUATE WS-OPTION
               WHEN 1
                    IF WS-USER-ROLE = "HR"
                           PERFORM ADD-EMPLOYEE
                       ELSE
                           DISPLAY "Access Denied"
                       END-IF
               WHEN 2
                    IF WS-USER-ROLE = "HR"
                       PERFORM EDIT-EMPLOYEE
                   ELSE
                       DISPLAY "Access Denied"
                   END-IF
               WHEN 3
                    IF WS-USER-ROLE = "HR"
                       PERFORM DELETE-EMPLOYEE
                   ELSE
                       DISPLAY "Access Denied"
                   END-IF
               WHEN 4 PERFORM SEARCH-EMPLOYEE
               WHEN 5 PERFORM EMPLOYEE-REPORT
               WHEN 6 PERFORM SUMMARY-REPORT-PROC
               WHEN 7 PERFORM BACKUP-EMPLOYEE
               WHEN 8
                    IF WS-USER-ROLE = "HR"
                       PERFORM BATCH-UPDATE
                   ELSE
                       DISPLAY "Access Denied"
                   END-IF
               WHEN 9 DISPLAY "Bye"

                   WHEN OTHER DISPLAY "Invalid option"
                   STOP RUN
           END-EVALUATE

           IF WS-OPTION NOT = 9
               PERFORM MENU-LOOP
           END-IF.



       ADD-EMPLOYEE.
           OPEN I-O EMP-MASTER.
           DISPLAY "Enter Employee ID:".
           ACCEPT EMP-ID.
           DISPLAY "Enter Employee Name:".
           ACCEPT EMP-NAME.
           DISPLAY "Enter Dept:".
           ACCEPT DEPT-NAME.
           DISPLAY "Enter Salary:".
           ACCEPT SALARY.
           WRITE EMP-RECORD
               INVALID KEY
               DISPLAY "Employee ID already exists"
               MOVE "ADD" TO WS-ACTION
               PERFORM LOG-HR-ACTION
           END-WRITE.

           CLOSE EMP-MASTER.


       EDIT-EMPLOYEE.
           OPEN I-O EMP-MASTER.
           DISPLAY "Enter Employee ID to Edit:".
           ACCEPT EMP-ID.
           READ EMP-MASTER KEY IS EMP-ID
               INVALID KEY DISPLAY "Employee Not Found"
           END-READ.
           DISPLAY "Enter New Name:".
           ACCEPT EMP-NAME.
           DISPLAY "Enter New Dept:".
           ACCEPT DEPT-NAME.
           DISPLAY "Enter New Salary:".
           ACCEPT SALARY.
           REWRITE EMP-RECORD
               INVALID KEY DISPLAY "Error updating record"
               MOVE "EDIT" TO WS-ACTION
               PERFORM LOG-HR-ACTION
           END-REWRITE.

           CLOSE EMP-MASTER.

       DELETE-EMPLOYEE.
           OPEN I-O EMP-MASTER.
           DISPLAY "Enter Employee ID to Delete:".
           ACCEPT EMP-ID.
           DELETE EMP-MASTER RECORD
               INVALID KEY DISPLAY "Employee Not Found"
               MOVE "DELETE" TO WS-ACTION
               PERFORM LOG-HR-ACTION
           END-DELETE.


           CLOSE EMP-MASTER.

       SEARCH-EMPLOYEE.
           OPEN INPUT EMP-MASTER.
           DISPLAY "Enter Employee ID to Search:".
           ACCEPT EMP-ID.
           READ EMP-MASTER KEY IS EMP-ID
               INVALID KEY DISPLAY "Employee Not Found"

               NOT INVALID KEY
                   DISPLAY "ID: " EMP-ID
                   DISPLAY "Name: " EMP-NAME
                   DISPLAY "Dept: " DEPT-NAME
                   DISPLAY "Salary: " SALARY
           END-READ.
           CLOSE EMP-MASTER.

       EMPLOYEE-REPORT.
           OPEN OUTPUT EMP-REPORT.
           OPEN INPUT EMP-MASTER.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-YEAR TO WS-YEAR-DIS
           MOVE WS-MONTH TO WS-MONTH-DIS
           MOVE WS-DAY TO WS-DAY-DIS
           MOVE WS-HOUR TO WS-HOUR-DIS
           MOVE WS-MINUTE TO WS-MINUTE-DIS
           MOVE WS-SECOND TO WS-SECOND-DIS.
           MOVE WS-DATE TO WS-DATE-DIS.

           WRITE REPORT-RECORD FROM HEADER.
           WRITE REPORT-RECORD FROM EQUAL-LINE.
           WRITE REPORT-RECORD FROM SUBHEADER.
           WRITE REPORT-RECORD FROM DASH-LINE.

           PERFORM UNTIL EOF-FLAG = 'Y'

               READ EMP-MASTER NEXT RECORD

               AT END
                   MOVE 'Y' TO EOF-FLAG
               NOT AT END

                   MOVE EMP-ID TO WS-EMP-ID
                   MOVE EMP-NAME TO WS-EMP-NAME
                   MOVE DEPT-NAME TO WS-DEPT-NAME
                   MOVE SALARY TO WS-SALARY
                   WRITE REPORT-RECORD FROM DETAIL-LINE
           END-READ
       END-PERFORM


           WRITE REPORT-RECORD FROM EQUAL-LINE.
           WRITE REPORT-RECORD FROM FOOTER.
           DISPLAY "*********CREATED EMPLOYEE REPORT***********"
           CLOSE EMP-MASTER.
           CLOSE EMP-REPORT.


       SUMMARY-REPORT-PROC.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE WS-YEAR TO WS-YEAR-DIS
           MOVE WS-MONTH TO WS-MONTH-DIS
           MOVE WS-DAY TO WS-DAY-DIS
           MOVE WS-HOUR TO WS-HOUR-DIS
           MOVE WS-MINUTE TO WS-MINUTE-DIS
           MOVE WS-SECOND TO WS-SECOND-DIS.
           MOVE WS-DATE TO WS-DATE-DIS-S.
           MOVE 0 TO TOTAL-DEPT
           MOVE 0 TO TOTAL-SALARY
           SORT TEMP-SORT
           ON ASCENDING KEY DEPT-NAME
           USING EMP-MASTER
           GIVING EMP-SORTED.

           OPEN OUTPUT SUMMARY-REPORT
           OPEN INPUT EMP-SORTED

           WRITE SUMMARY-REC FROM HEADER-S
           WRITE SUMMARY-REC FROM EQUAL-LINE-S
           WRITE SUMMARY-REC FROM SUBHEADER-S
           WRITE SUMMARY-REC FROM DASH-LINE-SPACE


           MOVE 'N' TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = 'Y'
            READ EMP-SORTED
           AT END
            MOVE 'Y' TO EOF-FLAG
           NOT AT END
            IF WS-PREV-DEPT-ID NOT = DEPT-NAME-S

                   IF WS-PREV-DEPT-ID NOT = SPACES
                       MOVE WS-PREV-DEPT-ID TO WS-DEPT
                       MOVE WS-SUBTOTAL-DEPT TO WS-DEPT-AMOUNT
                       MOVE WS-SUBTOTAL-SALARY TO SUBTOTAL-SALARY
                       WRITE SUMMARY-REC FROM DETAIL-LINE-S
                       MOVE 0 TO WS-SUBTOTAL-SALARY
                       MOVE 0 TO WS-SUBTOTAL-DEPT

                   END-IF

                       MOVE DEPT-NAME-S TO WS-PREV-DEPT-ID
            END-IF

               ADD 1 TO WS-SUBTOTAL-DEPT
               ADD 1 TO WS-TOTAL-DEPT
               ADD SALARY-S TO WS-SUBTOTAL-SALARY
               ADD SALARY-S TO WS-TOTAL-SALARY
            END-PERFORM
           MOVE WS-PREV-DEPT-ID TO WS-DEPT
           MOVE WS-SUBTOTAL-DEPT TO WS-DEPT-AMOUNT
           MOVE WS-SUBTOTAL-SALARY TO SUBTOTAL-SALARY
           WRITE SUMMARY-REC FROM DETAIL-LINE-S
           MOVE WS-TOTAL-SALARY TO TOTAL-SALARY
           MOVE WS-TOTAL-DEPT TO TOTAL-DEPT

           WRITE SUMMARY-REC FROM DASH-LINE
           WRITE SUMMARY-REC FROM DETAIL-TOTAL
           WRITE SUMMARY-REC FROM EQUAL-LINE-S
           MOVE " " TO WS-PREV-DEPT-ID.
           MOVE 0 TO WS-TOTAL-SALARY
           MOVE 0 TO WS-SUBTOTAL-DEPT
           MOVE 0 TO WS-SUBTOTAL-SALARY
           DISPLAY "*********CREATED SUMMARY REPORT***********"
           CLOSE SUMMARY-REPORT
           CLOSE EMP-SORTED.


       BACKUP-EMPLOYEE.
           DISPLAY "Starting employee backup..."
           OPEN INPUT EMP-MASTER
           OPEN OUTPUT BACKUP-FILE

           MOVE 'N' TO EOF-FLAG

           PERFORM UNTIL EOF-FLAG = 'Y'
                READ EMP-MASTER NEXT RECORD
                    AT END
                        MOVE 'Y' TO EOF-FLAG
                    NOT AT END
                        MOVE EMP-ID     TO BK-EMP-ID
                        MOVE EMP-NAME   TO BK-EMP-NAME
                        MOVE DEPT-NAME  TO BK-DEPT-NAME
                        MOVE SALARY     TO BK-SALARY
                        WRITE BACKUP-RECORD FROM BK-REC-REDEF
                    END-READ
            END-PERFORM

            CLOSE EMP-MASTER
            CLOSE BACKUP-FILE

            MOVE "BACKUP" TO WS-ACTION
            PERFORM LOG-HR-ACTION

            DISPLAY "Employee backup completed successfully.".

       LOG-HR-ACTION.
           OPEN EXTEND HR-LOG
           STRING "Action: " WS-ACTION " - EmpID: " EMP-ID
               DELIMITED BY SIZE INTO WS-LOG-LINE
           END-STRING
           MOVE WS-LOG-LINE TO LOG-RECORD
           WRITE LOG-RECORD
           END-WRITE
           CLOSE HR-LOG.
       BATCH-UPDATE.
           DISPLAY "Starting batch update..."
           OPEN INPUT INPUT-SEQ
           OPEN I-O EMP-MASTER
           MOVE 'N' TO EOF-FLAG

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ INPUT-SEQ
                   AT END MOVE 'Y' TO EOF-FLAG
               NOT AT END
                   MOVE S-EMP-ID     TO EMP-ID
                   MOVE S-EMP-NAME   TO EMP-NAME
                   MOVE S-DEPT-NAME  TO DEPT-NAME
                   MOVE S-SALARY     TO SALARY

                   READ EMP-MASTER KEY IS EMP-ID
                       INVALID KEY
                           WRITE EMP-RECORD
                               INVALID KEY
                                   DISPLAY
                                   "Error writing record for ID " EMP-ID
                               NOT INVALID KEY
                                   MOVE "ADD" TO WS-ACTION
                                   PERFORM LOG-HR-ACTION
                               END-WRITE
                           NOT INVALID KEY
                               REWRITE EMP-RECORD
                                   INVALID KEY
                                       DISPLAY
                                 "Error updating record for ID " EMP-ID
                                   NOT INVALID KEY
                                       MOVE "UPDATE" TO WS-ACTION
                                       PERFORM LOG-HR-ACTION
                                   END-REWRITE
                           END-READ
                   END-PERFORM

                   CLOSE INPUT-SEQ
                   CLOSE EMP-MASTER
                   DISPLAY "Batch update completed.".
       END PROGRAM YOUR-PROGRAM-NAME.
