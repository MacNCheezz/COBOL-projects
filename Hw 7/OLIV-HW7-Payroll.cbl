      ******************************************************************
      * Author: McCarthy Oliveira
      * Date: 10/7/19
      * Purpose: Print payroll including dues and taxes
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-LIST
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Homework #7\OLIV-HW7-PayrollIn.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LIST-OUT
           ASSIGN TO
         'D:\COBOL\MIS 280 Homework\Homework #7\OLIV-HW7-PayrollOut.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-LIST.
       01  REC-IN.
           05  NUM-IN              PIC X(5).
           05  NAME-IN             PIC X(20).
           05  SALARY-IN           PIC 9999999.
           05  UNION-IN            PIC 999V99.
           05  INSUR-IN            PIC 999V99.
       FD  LIST-OUT.
       01  REC-OUT.
           05                      PIC X(2).
           05  NUM-OUT             PIC X(5).
           05                      PIC X(3).
           05  NAME-OUT            PIC X(20).
           05                      PIC X(2).
           05  SALARY-PRINT        PIC $Z,ZZZ,ZZZ.99.
           05                      PIC X(3).
           05  UNION-OUT           PIC $ZZZ.99.
           05                      PIC X(2).
           05  INSUR-OUT           PIC $ZZZ.99.
           05                      PIC X(2).
           05  FICA-OUT            PIC $ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  IBT-OUT             PIC $Z,ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  FED-OUT             PIC $Z,ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  STATE-OUT           PIC $ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  SALARY-OUT          PIC $Z,ZZZ,ZZZ.99.
           05                      PIC X(5).
      *>      ==============================================
           05  SALARY-RAISE-OUT    PIC $Z,ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  UNION-OUT2          PIC $ZZZ.99.
           05                      PIC X(2).
           05  INSUR-OUT2          PIC $ZZZ.99.
           05                      PIC X(2).
           05  FICA-OUT2           PIC $ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  IBT-OUT2            PIC $Z,ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  FED-OUT2            PIC $Z,ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  STATE-OUT2          PIC $ZZZ,ZZZ.99.
           05                      PIC X(2).
           05  NEW-SALARY-OUT      PIC $Z,ZZZ,ZZZ.99.
       WORKING-STORAGE SECTION.
       01  MORE-DATA               PIC XXX VALUE 'YES'.
       01  HEADER-1.
           05                      PIC X(20).
           05                      PIC X(14) VALUE 'PAYROLL REPORT'.
       01  HEADER-2.
           05                      PIC X(50) VALUE
               ' EMPLOYEE         NAME             CURRENT        '.
           05                      PIC X(53) VALUE
               'OLD      OLD       FICA           IBT             FED'.
           05                      PIC X(29) VALUE
               '         STATE        OLD NET'.
      *>         ====================================
           05                      PIC X(25) VALUE
               '    |||    RAISED        '.
            05                      PIC X(53) VALUE
               'NEW      NEW       FICA           IBT             FED'.
           05                      PIC X(29) VALUE
               '         STATE        NEW NET'.
       01  HEADER-3.
           05                      PIC X(49) VALUE
               '    NO.                            SALARY        '.
           05                      PIC X(51) VALUE
               'UNION    INSUR.     TAX                            '.
           05                      PIC X(31) VALUE
               'TAX          TAX         SALARY'.
      *>          =======================================
       05                      PIC X(25) VALUE
               '     |||    SALARY       '.
           05                      PIC X(49) VALUE
               'UNION    INSUR.     TAX                          '.
           05                      PIC X(33) VALUE
               '  TAX          TAX         SALARY'.
       01  SPACING                 PIC X.
       01  SALARY-DIFF             PIC 9(7)V99.
       01  SALARY-OLD              PIC 9(7)V99.
       01  SALARY-NEW              PIC 9(7)V99.
       01  SALARY-RAISE            PIC 9(7)V99.
       01  IBT-OLD                 PIC 9(7)V99.
       01  IBT-NEW                 PIC 9(7)V99.
       01  UNION-DIFF              PIC 9(4)V99.
       01  INSUR-DIFF              PIC 9(4)V99.
       01  FICA                    PIC V999 VALUE .062.
       01  FICA-TAX                PIC 9(7)V99.
       01  FICA-TAX2               PIC 9(8)V99.
       01  FED                     PIC V99 VALUE .37.
       01  FED-TAX                 PIC 9(7)V99.
       01  FED-TAX2                PIC 9(7)V99.
       01  STATE                   PIC V999 VALUE .075.
       01  STATE-TAX               PIC 9(6)V99.
       01  STATE-TAX2              PIC 9(6)V99.
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT PAYROLL-LIST
               OUTPUT LIST-OUT
           WRITE REC-OUT FROM HEADER-1
           WRITE REC-OUT FROM HEADER-2
           WRITE REC-OUT FROM HEADER-3
           WRITE REC-OUT FROM SPACING
           PERFORM UNTIL MORE-DATA = 'NO'
               READ PAYROLL-LIST
                   AT END
                       MOVE 'NO' TO MORE-DATA
                   NOT AT END
                       PERFORM 200-PROCESS
               END-READ
           END-PERFORM
           CLOSE PAYROLL-LIST
                 LIST-OUT
           STOP RUN.
       200-PROCESS.
      *>      =======================================================
      *>      CALC OLD SALARY
           MOVE SALARY-IN TO SALARY-OLD
           MULTIPLY SALARY-OLD BY FED GIVING FED-TAX
           MULTIPLY SALARY-OLD BY STATE GIVING STATE-TAX
           MULTIPLY SALARY-OLD BY FICA GIVING FICA-TAX

           COMPUTE IBT-OLD = SALARY-IN - FICA-TAX
               - UNION-IN - INSUR-IN
           COMPUTE SALARY-OLD = IBT-OLD - FED-TAX - STATE-TAX
      *>      =======================================================
      *>      CALC UNION AND INSURANCE INCREASE
           COMPUTE UNION-DIFF = UNION-IN + (UNION-IN * .04)
           COMPUTE INSUR-DIFF = INSUR-IN + (INSUR-IN * .03)
      *>      =======================================================
      *>      CALC SALARY RAISE
           MOVE SALARY-IN TO SALARY-RAISE
           COMPUTE SALARY-RAISE = (SALARY-IN + (SALARY-IN * .07))
           MULTIPLY SALARY-RAISE BY FED GIVING FED-TAX2
           MULTIPLY SALARY-RAISE BY STATE GIVING STATE-TAX2
           MULTIPLY SALARY-RAISE BY FICA GIVING FICA-TAX2

           COMPUTE IBT-NEW =  SALARY-RAISE - (SALARY-RAISE * FICA)
               - UNION-DIFF - INSUR-DIFF
           COMPUTE SALARY-NEW = IBT-NEW - FED-TAX2 - STATE-TAX2
      *>      =======================================================
      *>      DISPLAY FOR DEBUGGING
           DISPLAY 'EMPLOYEE: ' NAME-IN
                       '  |EMPLOYEE: ' NAME-IN
           DISPLAY 'SALARY CURRENT: $' SALARY-IN
                       '        |SALARY RAISE: $' SALARY-RAISE
           DISPLAY 'OLD UNION:        -$' UNION-IN
                       '      |NEW UNION:      -$' UNION-DIFF
           DISPLAY 'OLD INSUR:        -$' INSUR-IN
                       '      |NEW INSUR:      -$' INSUR-DIFF
           DISPLAY 'FICA TAX:     -$' FICA-TAX
                       '      |FICA TAX:      -$' FICA-TAX2
           DISPLAY 'IBT:           $' IBT-OLD
                       '      |IBT:          $' IBT-NEW
           DISPLAY 'FED TAX:      -$' FED-TAX
                       '      |FED TAX:       -$' FED-TAX2
           DISPLAY 'STATE TAX:     -$' STATE-TAX
                       '      |STATE TAX:     -$' STATE-TAX2
           DISPLAY 'OLD SALARY:    $' SALARY-OLD
                       '      |NEW SALARY:   $' SALARY-NEW
           DISPLAY '***************************************************'
                   '**************'
      *>      =======================================================
      *>      MOVE VARIABLES
           move NUM-IN TO NUM-OUT
           MOVE NAME-IN TO NAME-OUT
           MOVE SALARY-IN TO SALARY-PRINT
           MOVE UNION-IN TO UNION-OUT
           MOVE INSUR-IN TO INSUR-OUT
           MOVE FICA-TAX TO FICA-OUT
           MOVE IBT-OLD TO IBT-OUT
           MOVE FED-TAX TO FED-OUT
           MOVE STATE-TAX TO STATE-OUT
           MOVE SALARY-OLD TO SALARY-OUT
               MOVE SALARY-RAISE TO SALARY-RAISE-OUT
               MOVE UNION-DIFF TO UNION-OUT2
               MOVE INSUR-DIFF TO INSUR-OUT2
               MOVE FICA-TAX2 TO FICA-OUT2
               MOVE IBT-NEW TO IBT-OUT2
               MOVE FED-TAX2 TO FED-OUT2
               MOVE STATE-TAX2 TO STATE-OUT2
               MOVE SALARY-NEW TO NEW-SALARY-OUT
           WRITE REC-OUT.
