      ******************************************************************
      * 02-05-2020
      * THIS PROGRAM DEPICTS HOW TO PRINT USING CONTROL BREAK IN COBOL.
      * WITH FIXED PAGINATION SUPPORT
      * AND  ERROR LINE ON INPUT FILE FORMAT INVALID
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEP-SALES-REPORT.
       AUTHOR. GAETANO.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT IN-FILE ASSIGN TO
               "/Users/gaetanodorsi/Downloads/SalesIN.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
              SELECT OUT-FILE ASSIGN TO
              "/Users/gaetanodorsi/Downloads/SalesOUT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE
           RECORD CONTAINS 13 CHARACTERS.
           01 IN-REC.
               02 DEPT-IN              PIC XX.
               02 SLS-NO-IN            PIC X(5).
               02 AMT-OF-SALES-IN      PIC 9(3).
       FD OUT-FILE
           RECORD CONTAINS 132 CHARACTERS.
           01 OUT-REC                  PIC X(132).
       WORKING-STORAGE SECTION.
           01 ARE-THERE-MORE-RECORDS   PIC XXX VALUE "YES".
           01 WS-HOLD                  PIC XX.
           01 WS-LINE-CTR              PIC 99  VALUE ZERO.
           01 FIRST-RECORD             PIC XXX VALUE "YES".
           01 WS-DEPT-TOTAL            PIC 9(6).
           01 HD-LINE1.
               02 FILLER               PIC X(50) VALUE SPACES.
               02 FILLER        PIC X(21) VALUE "MONTHLY STATUS REPORT".
               02 FILLER               PIC X(34)  VALUE SPACES.
               02 FILLER               PIC X(4)  VALUE "PAGE".
              02 PAGE-OUT             PIC 999   VALUE ZERO.
               02 FILLER               PIC X(20) VALUE SPACES.
           01 HD-LINE2.
               02 FILLER               PIC X(29) VALUE SPACES.
               02 FILLER               PIC X(4) VALUE "DEPT".
               02 FILLER               PIC X(10) VALUE SPACES.
               02 FILLER              PIC X(15) VALUE "SALESPERSON NO.".
               02 FILLER               PIC X(10) VALUE SPACES.
               02 FILLER               PIC X(12) VALUE "AMT OF SALES".
               02 FILLER               PIC X(52) VALUE SPACES.
           01 DTL-LINE.
               02 FILLER               PIC X(29) VALUE SPACES.
               02 DEPT-OUT             PIC X(2).
               02 FILLER               PIC X(12) VALUE SPACES.
               02 SLS-NO-OUT           PIC X(5).
               02 FILLER               PIC X(25) VALUE SPACES.
               02 AMT-OF-SALES-OUT     PIC 9999.
               02 FILLER               PIC X(49).
           01 TOTAL-LINE.
               02 FILLER               PIC X(98) VALUE SPACES.
               02 FILLER   PIC X(25) VALUE "TOTAL FOR DEPARTMENT IS: ".
               02 TOTAL-OUT           PIC 9999.
           01 ERROR-LINE.
             02 FILLER                PIC X(50) VALUE SPACES.
             02 FILLER PIC X(30) VALUE "SEQUENCE ERROR PROGRAM ABORTED".

       PROCEDURE DIVISION.
      *
           000-MAIN-PROCEDURE.
           PERFORM 100-INITIALIZATION-RTN
           PERFORM 200-HDG-RTN
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = "NO"
               READ IN-FILE
                   AT END
                       MOVE "NO" TO ARE-THERE-MORE-RECORDS
                   MOVE WS-DEPT-TOTAL TO TOTAL-OUT
                   WRITE OUT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINES
                   NOT AT END
                       PERFORM 300-DTL-RTN
               END-READ
           END-PERFORM
           PERFORM 500-CLOSE-RTN
           STOP RUN.
      *
           100-INITIALIZATION-RTN.
           OPEN INPUT IN-FILE
           OPEN OUTPUT OUT-FILE.
      *
           200-HDG-RTN.
           ADD 1 TO PAGE-OUT
           WRITE OUT-REC FROM HD-LINE1 AFTER ADVANCING 2 LINES
           WRITE OUT-REC FROM HD-LINE2 AFTER ADVANCING 2 LINES
           MOVE ZERO TO WS-LINE-CTR.
      *
       300-DTL-RTN.
           EVALUATE TRUE
           WHEN FIRST-RECORD = "YES"
               MOVE "NO" TO FIRST-RECORD
               MOVE DEPT-IN TO WS-HOLD
           WHEN DEPT-IN NOT = WS-HOLD
               PERFORM 400-CTRL-BREAK-RTN
           END-EVALUATE
          IF WS-LINE-CTR > 5
               PERFORM 200-HDG-RTN
           END-IF
           MOVE DEPT-IN TO DEPT-OUT
           MOVE SLS-NO-IN TO SLS-NO-OUT
           MOVE AMT-OF-SALES-IN TO AMT-OF-SALES-OUT
           WRITE OUT-REC FROM DTL-LINE AFTER ADVANCING 2 LINES
           ADD 1 TO WS-LINE-CTR
           ADD AMT-OF-SALES-IN TO WS-DEPT-TOTAL.
      *
           400-CTRL-BREAK-RTN.
           MOVE WS-DEPT-TOTAL TO TOTAL-OUT
           WRITE OUT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINES
           IF DEPT-IN < WS-HOLD
               DISPLAY "SEQUENCE ERROR"
               WRITE OUT-REC FROM ERROR-LINE AFTER ADVANCING 1 LINES
               STOP RUN
           END-IF

           ADD 1 TO WS-LINE-CTR
           MOVE DEPT-IN TO WS-HOLD.
      *
           500-CLOSE-RTN.
           CLOSE IN-FILE
           CLOSE OUT-FILE.
