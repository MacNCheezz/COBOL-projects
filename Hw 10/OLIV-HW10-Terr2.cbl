      ******************************************************************
      * Author: McCarthy Oliveira
      * Date: 11/7/19
      * Purpose: Print info from input file, one has err, other doesnt
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUST-ERROR.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-LIST
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Homework #10\OLIV-HW10-Input2.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-OUT
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Homework #10\OLIV-HW10-Output2.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CUST-LIST.
       01  CUST-IN.
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
       01  CUST-OUT.
           05 REC-OUT                      PIC X(100).
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS          PIC XXX   VALUE 'YES'.
       01  TOTAL-REC.
           05                              PIC X(2) VALUE SPACES.
           05 CUST-NO-OUT                  PIC Z(4).
           05                              PIC X(10) VALUE SPACES.
           05 CUST-NAME-OUT                PIC X(26).
           05                              PIC X     VALUE SPACES.
           05 STORE-NO-OUT                 PIC 9(1).
           05                              PIC X(14) VALUE SPACES.
           05 SALESPERSON-NO-OUT           PIC Z(4).
           05                              PIC X(10) VALUE SPACES.
           05 SALES-AMT-OUT                PIC $$$,$$$.99.
           05                              PIC X(4)  VALUE SPACES.
           05 DATE-OF-TRANS-OUT.
               10 MONTHS-OUT               PIC 9(2).
               10                          PIC X     VALUE '/'.
               10 DAYS-OUT                 PIC 9(2).
               10                          PIC X     VALUE '/'.
               10 YEARS-OUT                PIC 9(4).
       01  LINE-1.
           05                              PIC X(54) VALUE
               '______________________________________________________'.
           05                              PIC X(54) VALUE
               '______________________________________________________'.
       01  HEADING1.
           05                              PIC X(53) VALUE
               'CUST-NUM        CUST-NAME              STORE-NUM     '.
           05                              PIC X(53) VALUE
               'SALESPERSON-NUM    SALES-AMT    DATE OR TRANS'.
       01  ERROR-1.
           05                              PIC X(50) VALUE
               '   ERROR: CUST-NUM OUT OF RANGE'.
       01  ERROR-2.
           05                              PIC X(50) VALUE
               '   ERROR: STORE-NUM OUT OF RANGE'.
       01  ERROR-3.
           05                              PIC X(50) VALUE
               '   ERROR: SALESPERSON-NUM OUT OF RANGE'.
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT CUST-LIST
               OUTPUT LIST-OUT
           WRITE CUST-OUT FROM HEADING1
           WRITE CUST-OUT FROM LINE-1
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
               READ CUST-LIST
                   AT END
                       MOVE 'NO' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-PROCESS
               END-READ
           END-PERFORM
           CLOSE CUST-LIST
                 LIST-OUT
           STOP RUN.
       200-PROCESS.
           MOVE CUST-NO TO CUST-NO-OUT
           MOVE CUST-NAME TO CUST-NAME-OUT
           MOVE STORE-NO TO STORE-NO-OUT
           MOVE SALESPERSON-NO TO SALESPERSON-NO-OUT
           MOVE SALES-AMT TO SALES-AMT-OUT
           MOVE MONTHS-IN TO MONTHS-OUT
           MOVE DAYS-IN TO DAYS-OUT
           MOVE YEARS-IN TO YEARS-OUT
           EVALUATE CUST-NO
               WHEN 0101 THROUGH 9621
                       EVALUATE STORE-NO
                           WHEN 1 THROUGH 4
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM TOTAL-REC
                                               BEFORE ADVANCING 2 LINES
                                           WRITE CUST-OUT FROM ERROR-3
                                           WRITE CUST-OUT FROM LINE-1
                                   END-EVALUATE
                           WHEN < 1
                               WRITE CUST-OUT FROM TOTAL-REC
                                   BEFORE ADVANCING 2 LINES
                               WRITE CUST-OUT FROM ERROR-2
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM ERROR-3
                                   END-EVALUATE
                               WRITE CUST-OUT FROM LINE-1
                           WHEN > 4
                               WRITE CUST-OUT FROM TOTAL-REC
                                   BEFORE ADVANCING 2 LINES
                               WRITE CUST-OUT FROM ERROR-2
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM ERROR-3
                                   END-EVALUATE
                               WRITE CUST-OUT FROM LINE-1
                       END-EVALUATE
               WHEN < 0101
                   WRITE CUST-OUT FROM TOTAL-REC
                       BEFORE ADVANCING 2 LINES
                   WRITE CUST-OUT FROM ERROR-1
                       EVALUATE STORE-NO
                           WHEN 1 THROUGH 4
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM ERROR-3
                                   END-EVALUATE
                                   WRITE CUST-OUT FROM LINE-1
                           WHEN < 1
                               WRITE CUST-OUT FROM ERROR-2
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM ERROR-3
                                   END-EVALUATE
                                   WRITE CUST-OUT FROM LINE-1
                           WHEN > 4
                               WRITE CUST-OUT FROM ERROR-2
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM ERROR-3
                                   END-EVALUATE
                                   WRITE CUST-OUT FROM LINE-1
                       END-EVALUATE
               WHEN > 9621
                   WRITE CUST-OUT FROM TOTAL-REC
                       BEFORE ADVANCING 2 LINES
                   WRITE CUST-OUT FROM ERROR-1
                       EVALUATE STORE-NO
                           WHEN 1 THROUGH 4
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM ERROR-3
                                   END-EVALUATE
                                   WRITE CUST-OUT FROM LINE-1
                           WHEN < 1
                               WRITE CUST-OUT FROM ERROR-2
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM ERROR-3
                                   END-EVALUATE
                                   WRITE CUST-OUT FROM LINE-1
                           WHEN > 4
                               WRITE CUST-OUT FROM ERROR-2
                                   EVALUATE SALESPERSON-NO
                                       WHEN > 0999
                                           WRITE CUST-OUT FROM ERROR-3
                                   END-EVALUATE
                                   WRITE CUST-OUT FROM LINE-1
                       END-EVALUATE
           END-EVALUATE.
