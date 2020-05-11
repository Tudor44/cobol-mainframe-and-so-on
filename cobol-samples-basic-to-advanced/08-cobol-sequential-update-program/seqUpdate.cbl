      ******************************************************************
       PROGRAM-ID. SEQ-UPDATE.
       AUTHOR. GAETANO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT IN-FILE1 ASSIGN TO "/Users/gaetanodorsi/OLD-MASTER.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
          SELECT IN-FILE2 ASSIGN TO  "/Users/gaetanodorsi/TRANS.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
          SELECT OUT-FILE ASSIGN TO "/Users/gaetanodorsi/NEW-MASTER.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE1.
           01 OLD-MASTER-REC.
               02 M-ACCT-NO-IN                 PIC X(2).
               02 M-AMT-DUE-IN                 PIC 9(4).
       FD IN-FILE2.
           01 TRANS-REC.
               02 T-ACCT-NO                    PIC X(2).
               02 T-AMT-DUE                    PIC 9(4).
       FD OUT-FILE.
           01 NEW-MASTER-REC.
               02 M-ACCT-NO-OUT                PIC 9(2).
               02 M-AMT-DUE-OUT                PIC 9(4).

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
           PERFORM 200-INITIALIZATION-RTN
           PERFORM 300-READ-MASTER
           PERFORM 400-READ-TRANSACTION
           PERFORM 500-COMPUTE-RTN UNTIL M-ACCT-NO-IN = HIGH-VALUES
                                   AND T-ACCT-NO = HIGH-VALUES
           PERFORM 600-CLOSE-RTN
           STOP RUN.
       200-INITIALIZATION-RTN.
           OPEN INPUT IN-FILE1
           OPEN INPUT IN-FILE2
           OPEN OUTPUT OUT-FILE.
       300-READ-MASTER.
           READ IN-FILE1
               AT END
                   MOVE HIGH-VALUES TO M-ACCT-NO-IN
           END-READ.
       400-READ-TRANSACTION.
           READ IN-FILE2
               AT END
                   MOVE HIGH-VALUES TO T-ACCT-NO
           END-READ.
       500-COMPUTE-RTN.
           EVALUATE TRUE
               WHEN T-ACCT-NO = M-ACCT-NO-IN
                   PERFORM 525-REGULAR-UPDATE
               WHEN T-ACCT-NO < M-ACCT-NO-IN
                   PERFORM 550-NEW-ACCOUNT
               WHEN OTHER
                   PERFORM 575-NO-UPDATE
           END-EVALUATE.
       525-REGULAR-UPDATE.
           MOVE OLD-MASTER-REC TO NEW-MASTER-REC
           PERFORM 535-READ-AND-ADD UNTIL T-ACCT-NO NOT = M-ACCT-NO-IN
           WRITE NEW-MASTER-REC
           PERFORM 300-READ-MASTER.

       535-READ-AND-ADD.
           ADD T-AMT-DUE TO M-AMT-DUE-OUT
           PERFORM 400-READ-TRANSACTION.

       550-NEW-ACCOUNT.
           MOVE TRANS-REC TO NEW-MASTER-REC

           WRITE NEW-MASTER-REC
           PERFORM 400-READ-TRANSACTION.

       575-NO-UPDATE.
           WRITE NEW-MASTER-REC FROM OLD-MASTER-REC
           PERFORM 300-READ-MASTER.
       600-CLOSE-RTN.
           CLOSE IN-FILE1
           CLOSE IN-FILE2
           CLOSE OUT-FILE.
