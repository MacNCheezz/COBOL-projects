      ******************************************************************
      * Author: McCarthy Oliveira
      * Date: 9/21/19
      * Purpose: print data from payroll file
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-INFO-IN
           ASSIGN TO
           'D:\COBOL\MIS 280 Homework\Homework #6\OLIV-HW6-PayIn.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYROLL-INFO-OUT
           ASSIGN TO
           'D:\COBOL\MIS 280 Homework\Homework #6\OLIV-HW6-PayOut.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-INFO-IN.
       01  PAY-IN.
           05  EMPLOYEE-NUMBER-IN      PIC X(5).
           05  EMPLOYEE-NAME-IN        PIC X(20).
           05  TERRITORY-NUMBER-IN     PIC X(2).
           05  OFFICE-NUMBER-IN        PIC X(2).
           05  ANNUAL-SALARY-IN        PIC X(6).
           05  PHONE-AREA-IN           PIC X(3).
           05  PHONE-MIDDLE-IN         PIC X(3).
           05  PHONE-END-IN            PIC X(4).
       FD  PAYROLL-INFO-OUT.
       01  PAY-OUT-REC.
           05                          PIC X.
           05  EMPLOYEE-NUMBER-OUT     PIC X(5).
           05                          PIC X(5).
           05  EMPLOYEE-NAME-OUT       PIC X(20).
           05                          PIC X(3).
           05  TERRITORY-NUMBER-OUT    PIC X(2).
           05                          PIC X(8).
           05  OFFICE-NUMBER-OUT       PIC X(2).
           05                          PIC X(9).
           05  ANNUAL-SALARY-OUT       PIC $9(3),9(3).
           05                          PIC X(5).
           05  PHONE-AREA-OUT          PIC X(3).
           05                          PIC X.
           05  PHONE-MIDDLE-OUT        PIC X(3).
           05                          PIC X.
           05  PHONE-END-OUT           PIC X(4).
       WORKING-STORAGE SECTION.
       01  MORE-DATA                   PIC XXX VALUE 'YES'.
       01  SPACES-1                    PIC X VALUE SPACES.
       01  HEADER-1.
           05                          PIC X(30) VALUE SPACES.
           05                          PIC X(50) VALUE
               'PAYROLL LISTING               PAGE 01   09/21/2019'.
       01  HEADER-2.
           05                          PIC X(53) VALUE
               'EMP. NO.  EMPLOYEE NAME        TERR NO.  OFFICE NO.  '.
           05                          PIC X(27) VALUE
               'ANNUAL SALARY  PHONE #'.


       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT PAYROLL-INFO-IN
               OUTPUT PAYROLL-INFO-OUT
           WRITE PAY-OUT-REC FROM HEADER-1.
           WRITE PAY-OUT-REC FROM SPACES-1.
           WRITE PAY-OUT-REC FROM HEADER-2.
           WRITE PAY-OUT-REC FROM SPACES-1.
           PERFORM UNTIL MORE-DATA = 'NO'
               READ PAYROLL-INFO-IN
                   AT END
                       MOVE 'NO' TO MORE-DATA
                   NOT AT END
                       PERFORM 200-PROCESS
               END-READ
           END-PERFORM
           CLOSE PAYROLL-INFO-IN
                 PAYROLL-INFO-OUT
           STOP RUN.
       200-PROCESS.

           MOVE EMPLOYEE-NUMBER-IN TO EMPLOYEE-NUMBER-OUT
           MOVE EMPLOYEE-NAME-IN TO EMPLOYEE-NAME-OUT
           MOVE TERRITORY-NUMBER-IN TO TERRITORY-NUMBER-OUT
           MOVE OFFICE-NUMBER-IN TO OFFICE-NUMBER-OUT
           MOVE ANNUAL-SALARY-IN TO ANNUAL-SALARY-OUT
           MOVE PHONE-AREA-IN TO PHONE-AREA-OUT
           MOVE PHONE-MIDDLE-IN TO PHONE-MIDDLE-OUT
           MOVE PHONE-END-IN TO PHONE-END-OUT
           WRITE PAY-OUT-REC.
