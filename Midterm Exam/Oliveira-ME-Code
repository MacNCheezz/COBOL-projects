      ******************************************************************
      * Author: McCarthy Oliveira
      * Date: 10/9/19
      * Purpose: submit 3 outputs
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDTERM-EXAM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-LIST
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Midterm\Oliveira-ME-Input'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-OUT1
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Midterm\Oliveira-ME-Output1.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-OUT2
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Midterm\Oliveira-ME-Output2.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-OUT3
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Midterm\Oliveira-ME-Output3.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-LIST.
       01  REC-IN.
           05  ITEM-CODE-IN               PIC X(8).
           05  ITEM-TITLE-IN              PIC X(20).
           05  ITEM-COST-IN               PIC 9(4)V99.
           05  ITEM-PRICE-IN              PIC 9(4)V99.
           05  TIME-MON-IN                PIC 9(2)V9.
           05  INV-LOCATION-IN            PIC X(14).
           05  INV-LOC-CODE-IN            PIC 9(4).
           05  SALES-RANK-IN              PIC 9(4).
           05  SEASON-CODE-IN             PIC X(2).
           05  CURRENT-INV-IN             PIC 9(4).
       FD  LIST-OUT1.
       01  REC-OUT1.
           05  ITEM-CODE-OUT1             PIC X(8).
           05                             PIC X VALUE SPACES.
           05  ITEM-TITLE-OUT1            PIC X(20).
           05                             PIC X VALUE SPACES.
           05  INV-LOCATION-OUT1          PIC X(14).
           05                             PIC X(3) VALUE SPACES.
       FD  LIST-OUT2.
       01  REC-OUT2.
           05  ITEM-CODE-OUT2             PIC X(8).
           05                             PIC X VALUE SPACES.
           05  ITEM-TITLE-OUT2            PIC X(20).
           05                             PIC X VALUE SPACES.
           05  ITEM-COST-OUT2             PIC $Z(4).ZZ.
           05                             PIC X(21) VALUE SPACES.
       FD  LIST-OUT3.
       01  REC-OUT3.
           05  ITEM-CODE-OUT3             PIC X(8).
           05                             PIC X VALUE SPACES.
           05  ITEM-TITLE-OUT3            PIC X(20).
           05                             PIC X VALUE SPACES.
           05  TIME-MON-OUT3              PIC ZZ.Z.
           05                             PIC X VALUE SPACES.
           05  INV-LOCATION-OUT3          PIC X(14).
           05                             PIC X VALUE SPACES.
           05  ITEM-PRICE-OUT3            PIC $Z(4).ZZ.
           05                             PIC X(8) VALUE SPACES.
       WORKING-STORAGE SECTION.
       01  MORE-DATA                      PIC XXX VALUE 'YES'.
       01  SPACING                        PIC X VALUE SPACES.
       01  HEADER-AUTHOR.
           05                             PIC X(26) VALUE
               'AUTHOR: McCarthy Oliveira'.
       01  HEADER-DATE.
           05                             PIC X(16) VALUE
               'DATE: 10/16/2019'.
       01  HEADER-DESCRIPTION             PIC X(47) VALUE
               'REPORT DESCRIPTION: MIS 280: COBOL MIDTERM EXAM'.
       01  REPORT-TITLE-1.
           05                             PIC X(45) VALUE
               'REPORT TITLE: ITEMS LOCATED IN <> - REPORT #1'.
       01  REPORT-TITLE-2.
           05                             PIC X(57) VALUE
               'REPORT TITLE: ITEMS THAT COST $75 OR MORE - REPORT #2'.
       01  REPORT-TITLE-3.
           05                             PIC X(54) VALUE
               'REPORT TITLE: ITEMS ON SHELF BETWEEN 5 AND 11 MONTHS -'.
           05                             PIC X(10) VALUE
               ' REPORT #3'.
       01  HEADER-1.
           05                             PIC X(9) VALUE
               'CODE     '.
           05                             PIC X(21) VALUE
               'TITLE                '.
           05                             PIC X(8) VALUE
               'LOCATION'.
       01  HEADER-2.
           05                             PIC X(9) VALUE
               'CODE     '.
           05                             PIC X(21) VALUE
               'TITLE                '.
           05                             PIC X(4) VALUE
               'COST'.
       01  HEADER-3.
           05                             PIC X(9) VALUE
               'CODE     '.
           05                             PIC X(21) VALUE
               'TITLE                '.
           05                             PIC X(5) VALUE
               'TIME '.
           05                             PIC X(15) VALUE
               'LOCATION       '.
           05                             PIC X(5) VALUE
               'PRICE'.
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT REPORT-LIST
               OUTPUT LIST-OUT1
               OUTPUT LIST-OUT2
               OUTPUT LIST-OUT3
                   WRITE REC-OUT1 FROM REPORT-TITLE-1
                       WRITE REC-OUT1 FROM HEADER-DESCRIPTION
                       WRITE REC-OUT1 FROM HEADER-AUTHOR
                       WRITE REC-OUT1 FROM HEADER-DATE
                       WRITE REC-OUT1 FROM SPACING
                       WRITE REC-OUT1 FROM HEADER-1
                   WRITE REC-OUT2 FROM REPORT-TITLE-2
                       WRITE REC-OUT2 FROM HEADER-DESCRIPTION
                       WRITE REC-OUT2 FROM HEADER-AUTHOR
                       WRITE REC-OUT2 FROM HEADER-DATE
                       WRITE REC-OUT2 FROM SPACING
                       WRITE REC-OUT2 FROM HEADER-2
                   WRITE REC-OUT3 FROM REPORT-TITLE-3
                       WRITE REC-OUT3 FROM HEADER-DESCRIPTION
                       WRITE REC-OUT3 FROM HEADER-AUTHOR
                       WRITE REC-OUT3 FROM HEADER-DATE
                       WRITE REC-OUT3 FROM SPACING
                       WRITE REC-OUT3 FROM HEADER-3
           PERFORM UNTIL MORE-DATA = 'NO'
               READ REPORT-LIST
                   AT END
                       MOVE 'NO' TO MORE-DATA
                   NOT AT END
                       PERFORM 200-PROCESS
                       PERFORM 300-PROCESS
                       PERFORM 400-PROCESS
               END-READ
           END-PERFORM
           CLOSE REPORT-LIST
                 LIST-OUT1
                 LIST-OUT2
                 LIST-OUT3
           STOP RUN.
      *>  ==============================================================
       200-PROCESS.
           MOVE ITEM-CODE-IN TO ITEM-CODE-OUT1
           MOVE ITEM-TITLE-IN TO ITEM-TITLE-OUT1
           MOVE INV-LOCATION-IN TO INV-LOCATION-OUT1
           IF INV-LOCATION-IN = 'Reading'
               WRITE REC-OUT1
           END-IF.
       300-PROCESS.
           MOVE ITEM-CODE-IN TO ITEM-CODE-OUT2
           MOVE ITEM-TITLE-IN TO ITEM-TITLE-OUT2
           MOVE ITEM-COST-IN TO ITEM-COST-OUT2
           IF ITEM-COST-IN >= 0075.00
               WRITE REC-OUT2
           END-IF.
       400-PROCESS.
           MOVE ITEM-CODE-IN TO ITEM-CODE-OUT3
           MOVE ITEM-TITLE-IN TO ITEM-TITLE-OUT3
           MOVE TIME-MON-IN TO TIME-MON-OUT3
           MOVE INV-LOCATION-IN TO INV-LOCATION-OUT3
           MOVE ITEM-PRICE-IN TO ITEM-PRICE-OUT3
           IF TIME-MON-IN >= 05.0 AND <= 11.0
               WRITE REC-OUT3
           END-IF.
