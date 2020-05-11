      *PROGRAM FOR CALCULATE COMOUND INTEREST
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPOUND-INTEREST.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT IN-FILE ASSIGN TO
               "/Users/gaetanodorsi/Desktop/COBOL/lesson4/ACCOUNT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO
           "/Users/gaetanodorsi/Desktop/COBOL/lesson4/PRINCIPAL.txt".
       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE
       RECORD CONTAINS 34 CHARACTERS.
       01 IN-REC.
           02 ACC-NO-IN           PIC 9(5).
           02 DEPO-NAME-IN        PIC X(20).
           02 PRIN-IN             PIC 9(5).
           02 RATE-IN             PIC V99.
           02 PERIOD-OF-INV-IN    PIC 99.
       FD OUT-FILE.
       01 OUT-REC                 PIC X(80).
       WORKING-STORAGE SECTION.
       01 WS-YEAR                    PIC 99.
       01 ARE-THERE-MORE-RECORDS     PIC XXX VALUE "YES".
           88 NO-MORE-RECORDS        VALUE "NO".
       01 WS-NEW-BALANCE             PIC 9(6)V99.
       01 WS-ACR-INTEREST            PIC 9(5)V99.
       01 HD-LINE1.
           02 FILLER                 PIC X(60) VALUE SPACES.
           02 FILLER                 PIC X(16) VALUE "PRINCIPAL  TABLE".
           02 FILLER                 PIC X(4) VALUE SPACES.
       01 HD-LINE2.
           02 FILLER                 PIC X(10) VALUE SPACES.
           02 FILLER                 PIC X(10) VALUE "ACCOUNT NO".
           02 FILLER                 PIC X(3) VALUE SPACES.
           02 ACC-NO-OUT             PIC 9(5).
           02 FILLER                 PIC X(52) VALUE SPACES.
       01 HD-LINE3.
           02 FILLER                 PIC X(10) VALUE SPACES.
           02 FILLER                 PIC X(14) VALUE "DEPOSITOR NAME".
           02 FILLER                 PIC X(3) VALUE SPACES.
           02 DEPO-NAME-OUT          PIC X(20).
           02 FILLER                 PIC X(33) VALUE SPACES.
       01 HD-LINE4.
           02 FILLER                 PIC X(10) VALUE SPACES.
           02 FILLER                 PIC X(9) VALUE "PRINCIPAL".
           02 FILLER                 PIC X(3) VALUE SPACES.
           02 PRIN-OUT               PIC $ZZ,ZZZ.
           02 FILLER                 PIC X(51) VALUE SPACES.
       01 HD-LINE5.
           02 FILLER                 PIC X(10) VALUE SPACES.
           02 FILLER                 PIC X(4) VALUE "RATE".
           02 FILLER                 PIC X(2) VALUE SPACES.
           02 RATE-OUT               PIC .99.
           02 FILLER                 PIC X(63) VALUE SPACES.
       01 HD-LINE6.
           02 FILLER                 PIC X(10) VALUE SPACES.
           02 FILLER                 PIC X(12) VALUE "NO. OF YEARS".
           02 FILLER                 PIC X(3) VALUE SPACES.
           02 PERIOD-OF-INV-OUT      PIC Z9.
           02 FILLER                 PIC X(52) VALUE SPACES.
       01 HD-LINE7.
           02 FILLER                 PIC X(28) VALUE SPACES.
           02 FILLER                 PIC X(4) VALUE "YEAR".
           02 FILLER                 PIC X(9) VALUE SPACES.
           02 FILLER                 PIC X(11) VALUE "NEW BALANCE".
           02 FILLER                 PIC X(9) VALUE SPACES.
           02 FILLER                 PIC X(16) VALUE "ACCRUED INTEREST".
           02 FILLER                 PIC X(4) VALUE SPACES.
       01 DTL-LINE.
           02 FILLER                 PIC X(28) VALUE SPACES.
           02 YEAR-OUT               PIC Z9.
           02 FILLER                 PIC X(11) VALUE SPACES.
           02 NEW-BALANCE-OUT        PIC $ZZZ,ZZZ.99.
           02 FILLER                 PIC X(9) VALUE SPACES.
           02 ACC-INTEREST-OUT       PIC $ZZ,ZZZ.99.
           02 FILLER                 PIC X(2) VALUE SPACES.
      *
       PROCEDURE DIVISION.
      *
       000-MAIN-MODULE.
           PERFORM 100-INITIALIZATION-RTN
           PERFORM UNTIL NO-MORE-RECORDS
               READ IN-FILE
               AT END
                   MOVE "NO" TO ARE-THERE-MORE-RECORDS
               NOT AT END
                   PERFORM 200-COMPUTE-RTN
               END-READ
           END-PERFORM

           PERFORM 300-TERMINATION-RTN
           STOP RUN.
       100-INITIALIZATION-RTN.
           OPEN INPUT IN-FILE
           OPEN OUTPUT OUT-FILE.
       200-COMPUTE-RTN.
           PERFORM 400-HEADING-RTN
           PERFORM 500-COMPUTE-INTEREST-RTN VARYING WS-YEAR FROM 1
           BY 1 UNTIL WS-YEAR > PERIOD-OF-INV-IN.
       300-TERMINATION-RTN.
           CLOSE IN-FILE
           CLOSE OUT-FILE.
       400-HEADING-RTN.
           WRITE OUT-REC FROM HD-LINE1 AFTER ADVANCING PAGE
           MOVE ACC-NO-IN TO ACC-NO-OUT
           MOVE DEPO-NAME-IN TO DEPO-NAME-OUT
           MOVE PRIN-IN TO PRIN-OUT
           MOVE RATE-IN TO RATE-OUT
           MOVE PERIOD-OF-INV-IN TO PERIOD-OF-INV-OUT
           WRITE OUT-REC FROM HD-LINE2 AFTER ADVANCING 3 LINES
           WRITE OUT-REC FROM HD-LINE3 AFTER ADVANCING 2 LINES
           WRITE OUT-REC FROM HD-LINE4 AFTER ADVANCING 2 LINES
           WRITE OUT-REC FROM HD-LINE5 AFTER ADVANCING 2 LINES
           WRITE OUT-REC FROM HD-LINE6 AFTER ADVANCING 2 LINES
           WRITE OUT-REC FROM HD-LINE7 AFTER ADVANCING 2 LINES.
       500-COMPUTE-INTEREST-RTN.
      *     MOVE PERIOD-OF-INV-IN TO WS-YEAR
            MOVE WS-YEAR TO YEAR-OUT
           COMPUTE WS-NEW-BALANCE = PRIN-IN * (1 + RATE-IN) ** WS-YEAR
           MOVE WS-NEW-BALANCE TO NEW-BALANCE-OUT
           SUBTRACT PRIN-IN FROM WS-NEW-BALANCE GIVING ACC-INTEREST-OUT
           WRITE OUT-REC FROM DTL-LINE AFTER ADVANCING 2 LINES.
