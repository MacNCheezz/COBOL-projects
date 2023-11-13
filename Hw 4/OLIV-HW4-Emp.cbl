       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-FILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE-IN
           ASSIGN TO
           'D:\COBOL\MIS 280 Homework\Homework #4\OLIV-HW4-EmpIn.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SALES-FILE-OUT
           ASSIGN TO
           'D:\COBOL\MIS 280 Homework\Homework #4\OLIV-HW4-EmpOut.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE-IN.
       01 EMPLOYEE-REC.
           05 EMPLOYEE-NAME-IN     PIC X(20).
           05 SALARY-IN            PIC 9(5).
           05 NO-OF-DEPENDENTS     PIC X(1).
           05 FICA-IN              PIC X(5).
           05 STATE-TAX-IN         PIC X(6).
           05 FEDERAL-TAX-IN       PIC X(6).
           05 DATE-OF-HIRE.
               10 MONTH            PIC 9(2).
               10 DAYS             PIC 9(2).
               10 YEAR             PIC 9(4).
       FD SALES-FILE-OUT.
       01 SALARY-REC-OUT.
           05 EMPLOYEE-NAME-OUT    PIC X(20).
           05 SALARY-OUT           PIC X(5).
       WORKING-STORAGE SECTION.
       01 WS-WORK-AREAS.
           05 MORE-RECORDS         PIC X(3) VALUE 'YES'.
       PROCEDURE DIVISION.
       100-MAIN.
       OPEN INPUT EMPLOYEE-FILE-IN
           OUTPUT SALES-FILE-OUT
       PERFORM UNTIL MORE-RECORDS = 'NO'
           READ EMPLOYEE-FILE-IN
               AT END
                   MOVE 'NO' TO MORE-RECORDS
               NOT AT END
                   PERFORM 200-PROCESS
           END-READ
       END-PERFORM
       CLOSE EMPLOYEE-FILE-IN
             SALES-FILE-OUT
       STOP RUN.
       200-PROCESS.
           MOVE EMPLOYEE-NAME-IN TO EMPLOYEE-NAME-OUT
           MOVE SALARY-IN TO SALARY-OUT
           WRITE SALARY-REC-OUT.
