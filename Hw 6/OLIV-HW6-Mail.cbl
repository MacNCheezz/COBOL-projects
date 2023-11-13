      ******************************************************************
      * Author: McCarthy Oliveira
      * Date: 9/21/2019
      * Purpose: print mailing list from name/address file
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-LIST
           ASSIGN TO
           'D:\COBOL\MIS 280 Homework\Homework #6\OLIV-HW6-MailIn.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MAIL-OUT
           ASSIGN TO
           'D:\COBOL\MIS 280 Homework\Homework #6\OLIV-HW6-MailOut.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-LIST.
       01  LIST-IN.
           05 NAME-IN           PIC X(20).
           05 ADDRESS-IN        PIC X(20).
           05 LOCATION-IN       PIC X(15).
           05 LOCATION-ZIP-IN   PIC X(5).
       FD  MAIL-OUT.
       01  MAIL-REC.
           05                   PIC X(60).
       WORKING-STORAGE SECTION.
       01  MORE-DATA            PIC XXX VALUE 'YES'.
       01  HEADER-1             PIC X(41) VALUE
               ' MAILING LIST        PAGE 01   09/21/2019'.
       01  SPACES-1             PIC X VALUE SPACES.
       01  NAME-LINE.
           05                   PIC X(4) VALUE SPACES.
           05 NAME-OUT          PIC X(20).
           05                   PIC X(17) VALUE SPACES.
       01  ADDRESS-LINE.
           05                   PIC X(4) VALUE SPACES.
           05 ADDRESS-OUT       PIC X(20).
           05                   PIC X(17) VALUE SPACES.
       01  LOCATION-LINE.
           05                   PIC X(4) VALUE SPACES.
           05 LOCATION-OUT      PIC X(15).
           05                   PIC X VALUE SPACES.
           05 LOCATION-ZIP-OUT  PIC X(5).
           05                   PIC X(11) VALUE SPACES.
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT MASTER-LIST
               OUTPUT MAIL-OUT
           WRITE MAIL-REC FROM HEADER-1.
           PERFORM UNTIL MORE-DATA = 'NO'
               READ MASTER-LIST
                   AT END
                       MOVE 'NO' TO MORE-DATA
                   NOT AT END
                       PERFORM 200-PROCESS
               END-READ
           END-PERFORM
           CLOSE MASTER-LIST
                 MAIL-OUT
           STOP RUN.
       200-PROCESS.
           WRITE MAIL-REC FROM SPACES-1
           MOVE NAME-IN TO NAME-OUT
           MOVE ADDRESS-IN TO ADDRESS-OUT
           MOVE LOCATION-IN TO LOCATION-OUT
           MOVE LOCATION-ZIP-IN TO LOCATION-ZIP-OUT
           WRITE MAIL-REC FROM NAME-LINE
           WRITE MAIL-REC FROM ADDRESS-LINE
           WRITE MAIL-REC FROM LOCATION-LINE.
