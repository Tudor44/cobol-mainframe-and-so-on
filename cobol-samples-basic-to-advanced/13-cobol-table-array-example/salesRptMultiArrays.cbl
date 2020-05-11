      *09-05-2020
      *PROGRAM FOR WRITE A FILE THAT CONTAINS
      * A SALES REPORT USING TABLE AND MULTI ARRAYS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES-RPT-MULTI-ARRAY-PROGRAM.
       AUTHOR. GAETANO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO "/Users/gaetanodorsi/S1.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "/Users/gaetanodorsi/S2.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SALES-FILE.
           01 SALES-REC.
               05 SALESPERSON-NO-IN                    PIC 99.
               05 SALES-AMT-IN                         PIC 999.
               05 DATE-OF-TRANS.
                    10 MONTH-IN                        PIC 99.
                    10 DAY-IN                          PIC 99.
                    10 YEAR-IN                         PIC 99.
       FD REPORT-FILE.
           01 PRINT-REC                                PIC X(132).
       WORKING-STORAGE SECTION.
           01 MORE-RECS                             PIC XXX VALUE "YES".
           01 COMPANY-SALES-ARRAY.
               05 SALESPERSON OCCURS 25 TIMES.
                   10 MONTH-AMT OCCURS 12 TIMES PIC 9(4).
           01 HEADING-REC.
               05 FILLER                               PIC X(30).
               05 FILLER         PIC X(102) VALUE "ANNUAL SALES REPORT".
           01 COLUMN-HEADING.
               05 FILLER                           PIC X(41)
                  VALUE "   S1   S2   S3   S4   S5   S6   S7   S8".
               05 FILLER
                  VALUE "  X9  S10  S11  S12  S13  S14  S15  S16".
               05 FILLER
            VALUE "  X17  S18  S19  S20  S21  S22  S23  S124 $25".
           01 SALES-LINE.
               05 FILLER                              PIC X VALUE SPACE.
               05  ITEMX OCCURS 25 TIMES.
                   10 SALES-ITEM                      PIC ZZZ9.
                   10 FILLER                          PIC X VALUE SPACE.
               05 FILLER                              PIC X(6).
           01 SUB1                                    PIC 99.
           01 SUB2                                    PIC 99.
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT SALES-FILE
                OUTPUT REPORT-FILE
           WRITE PRINT-REC FROM HEADING-REC AFTER ADVANCING PAGE
           WRITE PRINT-REC FROM COLUMN-HEADING AFTER ADVANCING 3 LINES
           MOVE ZEROES TO COMPANY-SALES-ARRAY
           PERFORM UNTIL MORE-RECS = "NO"
               READ SALES-FILE
                   AT END
                       MOVE "NO" TO MORE-RECS
                   NOT AT END
                       PERFORM 200-CALC-RTN
               END-READ
           END-PERFORM
           PERFORM 800-WRITE-RNT VARYING SUB2 FROM 1 BY 1 UNTIL SUB2> 12
           CLOSE SALES-FILE
                 REPORT-FILE
       STOP RUN.

       200-CALC-RTN.
           IF MONTH-IN > 0 AND < 13
               AND SALESPERSON-NO-IN > 0 AND < 26
                   ADD SALES-AMT-IN TO
                       MONTH-AMT (SALESPERSON-NO-IN, MONTH-IN)
           ELSE
               DISPLAY "ERROR " SALES-REC
           END-IF.
       800-WRITE-RNT.
           MOVE SPACES TO SALES-LINE
           PERFORM 900-MOVE-RNT VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 25
           WRITE PRINT-REC FROM SALES-LINE AFTER ADVANCING 2 LINES.
       900-MOVE-RNT.
           MOVE MONTH-AMT (SUB1,SUB2) TO SALES-ITEM (SUB1).
