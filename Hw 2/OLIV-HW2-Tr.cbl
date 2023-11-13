       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPLIES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE
               ASSIGN TO 'D:\COBOL\MIS 280 Homework\Homework #2\OLIV-HW2-TrIn.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SALES-FILE
               ASSIGN TO 'D:\COBOL\MIS 280 Homework\Homework #2\OLIV-HW2-TrOut.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD TRANS-FILE.
       01 TRANS-REC.
           05 NAME-IN          PIC X(20).
           05 UNIT-PRICE-IN    PIC 9(3)V99.
           05 QTY-SOLD-IN      PIC 999.
       FD SALES-FILE.
       01 SALES-REC.
           05 NAME-OUT         PIC X(20).
           05 TOTAL-PRICE-OUT  PIC 9(6)V99.
       WORKING-STORAGE SECTION.
       01 ARE-THERE-MORE-RECORDS PIC X(3) VALUE 'YES'.
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT TRANS-FILE
               OUTPUT SALES-FILE
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
               READ TRANS-FILE
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-CALC-RTN
               END-READ
           END-PERFORM
           CLOSE TRANS-FILE
                 SALES-FILE
           STOP RUN.
       200-CALC-RTN.
           MOVE NAME-IN TO NAME-OUT
           MULTIPLY UNIT-PRICE-IN BY QTY-SOLD-IN GIVING TOTAL-PRICE-OUT
           WRITE SALES-REC.
