      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE-INDEX-FILE.
       AUTHOR. GAETANO.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
       SELECT IN-FILE ASSIGN TO "/Users/gaetanodorsi/file1.txt"
               ORGANISATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS ACCT-NO-IN
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
          01 WS-ACCT-NO-IN              PIC 99.
          01 WS-AMT-DUE-IN              PIC 9999.

       PROCEDURE DIVISION.
      *
       100-MAIN-RTN.
         OPEN I-O IN-FILE
         PERFORM 200-ACCEPT-AND-CORRECT-RTN UNTIL NO-MORE-RECORDS
         CLOSE IN-FILE
         STOP RUN.

       200-ACCEPT-AND-CORRECT-RTN.
           DISPLAY "ENTER THE RECORD NUMBER TO BE UPDATED"
           ACCEPT WS-ACCT-NO-IN
           MOVE WS-ACCT-NO-IN TO ACCT-NO-IN
           READ IN-FILE
               INVALID KEY PERFORM 300-ERROR-RTN
               NOT INVALID KEY PERFORM 400-UPDATE-RTN
           END-READ
           DISPLAY " FILE STATUS CODE IS "  " " WS-STATUS
           DISPLAY " NEW RECORD IS "  " " ACCT-NO-IN AMT-DUE-IN
           DISPLAY " DO YOU WANT TO CONTINUE ? "
           ACCEPT ARE-THERE-MORE-RECORDS.

       400-UPDATE-RTN.
           DISPLAY "ENTER THE NEW AMOUNT"
           ACCEPT AMT-DUE-IN
           REWRITE IN-REC
               INVALID KEY DISPLAY "REWRITE ERROR" " " ACCT-NO-IN
           END-REWRITE.

       300-ERROR-RTN.
           DISPLAY " INVALID RECORD " " " ACCT-NO-IN.
