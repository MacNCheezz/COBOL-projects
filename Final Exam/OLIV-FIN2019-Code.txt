      ******************************************************************
      * Author: McCarthy Oliveira
      * Date: 12/4/19
      * Purpose: Print Employee EMPary Bonus/Annual increase if eligible
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPARY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-LIST
           ASSIGN TO
       'D:\COBOL\MIS 280 Homework\Final\OLIV-FIN2019-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-OUT
           ASSIGN TO
       'D:\COBOL\MIS 280 Homework\Final\OLIV-FIN2019-Output1.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-OUT2
           ASSIGN TO
       'D:\COBOL\MIS 280 Homework\Final\OLIV-FIN2019-Output2.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  EMP-LIST.
       01  EMP-IN.
           05 EMP-NO                       PIC 9(5).
           05 EMP-NAME                     PIC X(20).
           05 EMP-ADDRESS                  PIC X(25).
           05 EMP-CITY                     PIC X(15).
           05 EMP-STATE                    PIC X(2).
           05 EMP-ZIP                      PIC 9(5).
           05 EMP-TN                       PIC 9(2).
           05 EMP-SALARY                   PIC 9(6).
           05 EMP-SL                       PIC 9(2).
           05 EMP-PT                       PIC X(10).
       FD  LIST-OUT.
       01  REC-OUT.
           05                              PIC X(150).
       FD  LIST-OUT2.
       01  ERR-OUT.
           05                              PIC X(150).
       WORKING-STORAGE SECTION.
       01  EMP-OUT.
           05 EMP-NO-OUT                   PIC 9(5).
           05                              PIC X(3).
           05 EMP-NAME-OUT                 PIC X(20).
           05                              PIC X(2).
           05 EMP-PT-OUT                   PIC X(10).
           05                              PIC X(2).
           05 MONTHS                       PIC 99/.
           05 DAYS                         PIC 99/.
           05 YEARS                        PIC 9999.
           05                              PIC X(2).
           05 EMP-SL-OUT                   PIC ZZ.
           05                              PIC X(3).
           05 EMP-SALARY-OUT               PIC $ZZZ,ZZZ.
           05                              PIC X(3).
           05 EMP-TN-OUT                   PIC 99B.
           05                              PIC X(3).
           05 EMP-EL                       PIC XB.
           05                              PIC X(3).
           05 EMP-SALARY-INCREASE          PIC $ZZZ,ZZZ.
           05                              PIC X(2).
           05 EMP-BONUS                    PIC $ZZZ,ZZZ.
       01  SAL-INC                         PIC 9(6).
       01  ANN-BONUS                       PIC 9(6).
       01  ARE-THERE-MORE-RECORDS          PIC XXX         VALUE 'YES'.
       01  HEADER-1.
           05                              PIC X(27)       VALUE
               'EMPLOYEE SALARY INFORMATION'.
       01  HEADER-2.
           05                              PIC X(42)       VALUE
               'EMPLOYEE SALARY INFORMATION - ERROR REPORT'.
       01  H-DESCRIPTION.
           05                              PIC X(27)       VALUE
               'MIS 280: COBOL I FINAL EXAM'.
       01  H-AUTHOR.
           05                              PIC X(25)       VALUE
               'AUTHOR: McCarthy Oliveira'.
       01  H-DATE.
           05                              PIC X(14)       VALUE
               'DATE: 12/11/19'.
       01  H-LINE.
           05                              PIC X(54)       VALUE
               'EMPNO   EMP_NAME              POS_TITLE   DOE         '.
           05                              PIC X(44)      VALUE
               'SL   ANN_SAL    TEN   EL   SAL_INC   ANN_BON'.
       01  SEPERATOR.
           05                              PIC X(54)       VALUE
               '______________________________________________________'.
           05                              PIC X(48)       VALUE
               '________________________________________________'.
       01  ERROR-1.
           05                              PIC X(53)       VALUE
               'ERROR #1: The above record contains a salary that is '.
           05                              PIC X(24)       VALUE
               'out of range for level 1'.
       01  ERROR-2.
           05                              PIC X(53)       VALUE
               'ERROR #2: The above record contains a salary that is '.
           05                              PIC X(24)       VALUE
               'out of range for level 2'.
       01  ERROR-3.
           05                              PIC X(53)       VALUE
               'ERROR #3: The above record contains a salary that is '.
           05                              PIC X(24)       VALUE
               'out of range for level 3'.
       01  ERROR-4.
           05                              PIC X(58)       VALUE
           'ERROR #4: The above record is missing position title data'.
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT EMP-LIST
               OUTPUT LIST-OUT
               OUTPUT LIST-OUT2
      *>      ============================================
      *>      OUTPUT 1
           MOVE 12 TO MONTHS
           MOVE 11 TO DAYS
           MOVE 2019 TO YEARS
           WRITE REC-OUT FROM HEADER-1
           WRITE REC-OUT FROM H-DESCRIPTION
           WRITE REC-OUT FROM H-AUTHOR
           WRITE REC-OUT FROM H-DATE
               BEFORE ADVANCING 2 LINES
           WRITE REC-OUT FROM H-LINE
           WRITE REC-OUT FROM SEPERATOR
      *>      ============================================
      *>      OUTPUT 2
           WRITE ERR-OUT FROM HEADER-2
           WRITE ERR-OUT FROM H-DESCRIPTION
           WRITE ERR-OUT FROM H-AUTHOR
           WRITE ERR-OUT FROM H-DATE
               BEFORE ADVANCING 2 LINES
           WRITE ERR-OUT FROM H-LINE
           WRITE ERR-OUT FROM SEPERATOR
      *>      ============================================
      *>      BEGIN LOOP
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
               READ EMP-LIST
                   AT END
                       MOVE 'NO' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-PROCESS
                       PERFORM 300-PROCESS
               END-READ
           END-PERFORM
      *>      END LOOP
      *>      ============================================
           CLOSE EMP-LIST
                 LIST-OUT
                 LIST-OUT2
       STOP RUN.
      *>      ============================================
      *>      BEGIN OUTPUT 1
       200-PROCESS.
      *>      IF TENURE IS 5 YEARS OR MORE
           IF EMP-TN >= 5
               MOVE 'Y' TO EMP-EL
                   COMPUTE SAL-INC = EMP-SALARY * .09
                   ADD SAL-INC EMP-SALARY GIVING EMP-SALARY-INCREASE
               MOVE 0 TO SAL-INC
                   COMPUTE SAL-INC = EMP-SALARY * .07
                   MOVE SAL-INC TO EMP-BONUS
               MOVE EMP-NO TO EMP-NO-OUT
               MOVE EMP-NAME TO EMP-NAME-OUT
               MOVE EMP-PT TO EMP-PT-OUT
               MOVE EMP-SL TO EMP-SL-OUT
               MOVE EMP-SALARY TO EMP-SALARY-OUT
               MOVE EMP-TN TO EMP-TN-OUT
                   WRITE REC-OUT FROM EMP-OUT
           END-IF
      *>      ============================================
      *>      IF TENURE IS LESS THAN 5 YEARS
           IF EMP-TN < 5
               MOVE 'N' TO EMP-EL
               MOVE 0 TO SAL-INC
               MOVE 0 TO ANN-BONUS
               MOVE SAL-INC TO EMP-SALARY-INCREASE
               MOVE ANN-BONUS TO EMP-BONUS
               MOVE EMP-NO TO EMP-NO-OUT
               MOVE EMP-NAME TO EMP-NAME-OUT
               MOVE EMP-PT TO EMP-PT-OUT
               MOVE EMP-SL TO EMP-SL-OUT
               MOVE EMP-SALARY TO EMP-SALARY-OUT
               MOVE EMP-TN TO EMP-TN-OUT
                   WRITE REC-OUT FROM EMP-OUT
           END-IF
           WRITE REC-OUT FROM SEPERATOR.
      *>      END OUTPUT 1
      *>      ============================================
       300-PROCESS.
      *>      ============================================
      *>      BEGIN OUTPUT 2
      *>      FIND THE ERRORS
           EVALUATE EMP-PT
           WHEN EQUAL TO SPACES
               PERFORM 400-EVALUATE-EMPTY
           WHEN NOT EQUAL TO SPACES
               PERFORM 500-EVALUATE-FILLED
           END-EVALUATE.
      *>      END OUTPUT 2
      *>      ============================================
       400-EVALUATE-EMPTY.
           EVALUATE EMP-SL
                   WHEN 1
                       EVALUATE EMP-SALARY
                           WHEN < 30000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM ERROR-1
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN > 48999
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM ERROR-1
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN 30000 THROUGH 48999
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM SEPERATOR
                       END-EVALUATE
                   WHEN 2
                       EVALUATE EMP-SALARY
                           WHEN < 49000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM ERROR-2
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN > 78999
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM ERROR-2
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN 49000 THROUGH 78999
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM SEPERATOR
                       END-EVALUATE
                   WHEN 3
                       EVALUATE EMP-SALARY
                           WHEN < 79000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM ERROR-3
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN > 100000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM ERROR-3
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN 79000 THROUGH 100000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-4
                               WRITE ERR-OUT FROM SEPERATOR
                       END-EVALUATE
           END-EVALUATE.
       500-EVALUATE-FILLED.
               EVALUATE EMP-SL
                   WHEN 1
                       EVALUATE EMP-SALARY
                           WHEN < 30000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-1
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN > 48999
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-1
                               WRITE ERR-OUT FROM SEPERATOR
                       END-EVALUATE
                   WHEN 2
                       EVALUATE EMP-SALARY
                           WHEN < 49000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-2
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN > 78999
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-2
                               WRITE ERR-OUT FROM SEPERATOR
                       END-EVALUATE
                   WHEN 3
                       EVALUATE EMP-SALARY
                           WHEN < 79000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-3
                               WRITE ERR-OUT FROM SEPERATOR
                           WHEN > 100000
                               WRITE ERR-OUT FROM EMP-OUT
                                   BEFORE ADVANCING 2 LINES
                               WRITE ERR-OUT FROM ERROR-3
                               WRITE ERR-OUT FROM SEPERATOR
                       END-EVALUATE
               END-EVALUATE.
