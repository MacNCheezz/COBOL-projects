      ******************************************************************
      * Author: McCarthy Oliveira
      * Date: 11/7/19
      * Purpose: Print info from input file, one has err, other doesnt
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TERRITORY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TERR-LIST
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Homework #10\OLIV-HW10-Input1.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-OUT
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Homework #10\OLIV-HW10-Output1.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  TERR-LIST.
       01  TERR-IN.
           05 EMPLOYEE-NO                  PIC 9(5).
           05 EMPLOYEE-NAME                PIC X(20).
           05 TERRITORY-NO                 PIC 9(2).
           05 ANNUAL-SALARY                PIC 9(6).
           05 CUST-NO                      PIC 9(4).
           05 CUST-NAME                    PIC X(26).
           05 STORE-NO                     PIC 9(1).
           05 SALESPERSON-NO               PIC 9(4).
           05 SALES-AMT                    PIC 9(5)V99.
           05 DATE-OF-TRANS.
               10 MONTHS-IN                PIC 9(2).
               10 DAYS-IN                  PIC 9(2).
               10 YEARS-IN                 PIC 9(4).
       FD  LIST-OUT.
       01  TERR-OUT.
           05 REC-OUT                      PIC X(100).
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS          PIC XXX VALUE 'YES'.
       01  MORE-DATA                       PIC XXX VALUE 'YES'.
       01  HEADING1.
           05                              PIC X(13) VALUE
               'TERRITORY NO.'.
           05                              PIC X(7) VALUE SPACES.
           05                              PIC X(14) VALUE
               'TOTAL SALARIES'.
       01  FOOTER.
           05                              PIC X(28) VALUE
               'TOTAL SALARIES FOR COMPANY: '.
           05 TOTAL-COMPANY-SALARY         PIC $$$,$$$,$$$.99.
       01  TOTAL-REC.
           05                              PIC X(4) VALUE SPACES.
           05 TERRITORY-OUT                PIC X(2).
           05                              PIC X(14) VALUE SPACES.
           05 TOTAL-SALARY                 PIC $$$,$$$,$$$.99.
               77 TERR-HOLD                PIC X(2).
               77 WS-TOTAL-SALARY          PIC 9(9)V99.
               77 TOTAL                    PIC 9(9)V99.
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT TERR-LIST
               OUTPUT LIST-OUT
           WRITE TERR-OUT FROM HEADING1
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
               READ TERR-LIST
                   AT END
                       MOVE 'NO' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-PROCESS
               END-READ
           END-PERFORM
           COMPUTE TOTAL = TOTAL + WS-TOTAL-SALARY
           MOVE TOTAL TO TOTAL-COMPANY-SALARY
           PERFORM 300-TERR-BREAK
           WRITE TERR-OUT FROM FOOTER
               AFTER ADVANCING 2 LINES
           CLOSE TERR-LIST
                 LIST-OUT
           STOP RUN.
       200-PROCESS.
           EVALUATE TRUE
               WHEN MORE-DATA = 'YES'
                   MOVE TERRITORY-NO TO TERR-HOLD
                   MOVE 'NO' TO MORE-DATA
               WHEN TERRITORY-NO NOT = TERR-HOLD
                   PERFORM 300-TERR-BREAK
           END-EVALUATE
           MOVE TERRITORY-NO TO TERRITORY-OUT
           MOVE ANNUAL-SALARY TO TOTAL-SALARY
           COMPUTE WS-TOTAL-SALARY = WS-TOTAL-SALARY + ANNUAL-SALARY.
       300-TERR-BREAK.
           COMPUTE TOTAL = TOTAL + WS-TOTAL-SALARY
           MOVE WS-TOTAL-SALARY TO TOTAL-SALARY
           WRITE TERR-OUT FROM TOTAL-REC
               AFTER ADVANCING 1 LINES
           MOVE 0 TO WS-TOTAL-SALARY
           MOVE 'YES' TO MORE-DATA.
