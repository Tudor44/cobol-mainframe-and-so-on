      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-RELATIVE-FILE.
       AUTHOR. GAETANO.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
       SELECT IN-FILE ASSIGN TO "/Users/gaetanodorsi/Rel1.txt"
               ORGANIZATION IS RELATIVE
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-STATUS.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD IN-FILE
          RECORD CONTAINS 6 CHARACTERS.
          01 IN-REC.
              02 ACCT-NO-IN            PIC 9(2).
              02 AMT-DUE-IN            PIC 9(4).
      *
       WORKING-STORAGE SECTION.
          01 ARE-THERE-MORE-RECORDS     PIC XXX VALUE "YES".
               88 NO-MORE-RECORDS               VALUE "NO".
          01 WS-STATUS                  PIC XX.
       PROCEDURE DIVISION.
      *
       100-MAIN-RTN.
           OPEN INPUT IN-FILE
           PERFORM UNTIL NO-MORE-RECORDS
               READ IN-FILE
                   AT END
                       MOVE "NO" TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-DISPLAY-RTN
               END-READ
           END-PERFORM
           DISPLAY WS-STATUS
           CLOSE IN-FILE
           STOP RUN.

       200-DISPLAY-RTN.
      *   MOVE ACCT-NO-IN TO IN-REC
           DISPLAY " THE RECORD READ IS"
           DISPLAY ACCT-NO-IN
           DISPLAY AMT-DUE-IN.
