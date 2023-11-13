      ******************************************************************
      * Author: McCarthy Oliveira
      * Date: 9/20/19
      * Purpose: find and show outdated (>5 years) vehicles in output
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VEHICLE-YEAR-UPDATE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VEHICLE-INFO-IN
           ASSIGN TO
           'D:\COBOL\MIS 280 Homework\Homework #5\OLIV-HW5-VehIn.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT VEHICLE-INFO-OUT
           ASSIGN TO
           'D:\COBOL\MIS 280 Homework\Homework #5\OLIV-HW5-VehOut.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  VEHICLE-INFO-IN.
       01  VEHICLE-REC-IN.
           05 VIN-IN               PIC X(17).
           05 MAKE-IN              PIC X(13).
           05 TYPE-OF-VEHICLE-IN   PIC X(5).
           05 YEAR-IN              PIC 9(4).
       FD  VEHICLE-INFO-OUT.
       01  VEHICLE-REC-OUT.
           05 VIN-OUT              PIC X(17).
           05                      PIC X.
           05 MAKE-OUT             PIC X(13).
           05                      PIC X.
           05 TYPE-OF-VEHICLE-OUT  PIC X(5).
           05                      PIC X.
           05 YEAR-OUT             PIC 9(4).
       WORKING-STORAGE SECTION.
       01  MORE-DATA               PIC XXX VALUE 'YES'.
       01  YEARS-OLD               PIC 9(2).
       01  CURRENT-YEAR            PIC 9(4).
       01  HEADER-1.
           05                      PIC X(42) VALUE
               '      CARS THAT NEED TO BE REPLACED'.
       01  SPACES-1.
           05                      PIC X VALUE SPACES.
       01  HEADER-2.
           05                      PIC X(42) VALUE
               '        VIN       MAKE          TYPE  YEAR'.
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT VEHICLE-INFO-IN
               OUTPUT VEHICLE-INFO-OUT
           WRITE VEHICLE-REC-OUT FROM HEADER-1.
           WRITE VEHICLE-REC-OUT FROM SPACES-1.
           WRITE VEHICLE-REC-OUT FROM HEADER-2.
           WRITE VEHICLE-REC-OUT FROM SPACES-1.
           PERFORM UNTIL MORE-DATA = 'NO'
           READ VEHICLE-INFO-IN
               AT END
                   MOVE 'NO' TO MORE-DATA
               NOT AT END
                   PERFORM 200-UPDATE
           END-READ
           END-PERFORM
           CLOSE VEHICLE-INFO-IN
                 VEHICLE-INFO-OUT
           STOP RUN.
       200-UPDATE.
           MOVE VIN-IN TO VIN-OUT
           MOVE MAKE-IN TO MAKE-OUT
           MOVE TYPE-OF-VEHICLE-IN TO TYPE-OF-VEHICLE-OUT
           MOVE YEAR-IN TO YEAR-OUT
           MOVE FUNCTION CURRENT-DATE(1:4) TO CURRENT-YEAR
           COMPUTE YEARS-OLD = CURRENT-YEAR - YEAR-OUT
               IF YEARS-OLD > 5
                   WRITE VEHICLE-REC-OUT
               END-IF.
