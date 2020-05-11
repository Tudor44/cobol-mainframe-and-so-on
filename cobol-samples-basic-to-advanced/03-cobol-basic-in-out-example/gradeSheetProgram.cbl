      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-SHEET.
       AUTHOR. GAETANO D'ORSI
      *PROGRAM FOR SHOW AND CALCULATE THE GRADES AVERAGE OF STUDENTS
      *CHANGE ASSIGN PATH ADAPTED FOR YOUR ENVIRONMENT
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT IN-FILE1 ASSIGN TO
           "/{YOUR_PATH}/Student.txt"
           ORGANISATION IS LINE SEQUENTIAL.
      *    FILE STATUS IS WS-FS.
           SELECT OUT-FILE ASSIGN TO
            "/{YOUR_PATH}ReportStudent.txt"
           ORGANISATION IS LINE SEQUENTIAL.
           DATA DIVISION.
       FILE SECTION.
       FD IN-FILE1.
      *     LABEL RECORDS ARE STANDARD
      *     RECORD CONTAINS 14 CHARACTERS.
           01 IN-REC.
              02 ID-NO-IN          PIC X(2).
              02 NAME-IN           PIC X(4).
              02 EXAM1             PIC 9(2).
              02 EXAM2             PIC 9(2).
              02 EXAM3             PIC 9(2).
              02 EXAM4             PIC 9(2).
      *
       FD OUT-FILE.
      *     LABEL RECORDS ARE OMITTED
      *     RECORD CONTAINS 80 CHARACTERS.
           01 OUT-REC         PIC X(80).
            WORKING-STORAGE SECTION.
      *     01 WS-FS PIC 99.
            01 WS-DATE.
                02 WS-YEAR       PIC XX.
                02 WS-MONTH      PIC XX.
                02 WS-DAY        PIC XX.
            01 WS-LINE-CTR       PIC 99 VALUE ZERO.
            01 ARE-THERE-MORE-RECORDS PIC XXX VALUE "YES".
            01 DTL-LINE.
                02 FILLER        PIC X(5) VALUE SPACES.
                02 ID-NO-OUT     PIC X(2).
                02 FILLER        PIC X(5) VALUE SPACES.
                02 NAME-OUT      PIC X(20).
                02 FILLER        PIC X(5) VALUE SPACES.
                02 AVG-OUT       PIC 999.
                02 FILLER        PIC X(40) VALUE SPACES.
           01 HD-LINE1.
               02 FILLER        PIC X(40) VALUE SPACES.
               02 FILLER        PIC X(12) VALUE "CLASS GRADES".
               02 FILLER        PIC X(10) VALUE SPACES.
               02 DD-OUT        PIC X(2).
               02 FILLER        PIC X VALUE "/".
               02 MM-OUT        PIC X(2).
               02 FILLER        PIC X VALUE "/".
               02 YY-OUT        PIC X(2).
               02 FILLER        PIC X(3) VALUE SPACES.
               02 FILLER        PIC X(4) VALUE "PAGE".
               02 PAGE-OUT      PIC 99.
               02 FILLER        PIC X VALUE SPACES.
           01 HD-LINE2.
                02 FILLER        PIC X(5) VALUE SPACES.
                02 FILLER        PIC X(5) VALUE "ID.NO".
                02 FILLER        PIC X(5) VALUE SPACES.
                02 FILLER        PIC X(4) VALUE "NAME".
                02 FILLER        PIC X(5) VALUE SPACES.
                02 FILLER        PIC X(7) VALUE "AVERAGE".
                02 FILLER        PIC X(49) VALUE SPACES.
      *
       PROCEDURE DIVISION.
      *PROGRAM LOGIC IS CONTROLLED FROM THIS PARAGRAPH
       100-MAIN-PARA.
           PERFORM 200-INITIALIZATION-RTN
           PERFORM 300-HEADING-RTN
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = "NO"
                 READ IN-FILE1
                      AT END
                           MOVE "NO" TO ARE-THERE-MORE-RECORDS
                      NOT AT END
                           PERFORM 400-AVG-RTN
                 END-READ
           END-PERFORM
      *     DISPLAY "FILE STATUS IS "  WS-FS
           PERFORM 500-CLOSE-RTN
           STOP RUN.
       200-INITIALIZATION-RTN.
           OPEN INPUT IN-FILE1
           OPEN OUTPUT OUT-FILE.
       300-HEADING-RTN.
           ACCEPT WS-DATE FROM DATE
           MOVE WS-DAY TO DD-OUT
           MOVE WS-MONTH TO MM-OUT
           MOVE WS-YEAR TO YY-OUT
           ADD 1 TO PAGE-OUT
           WRITE OUT-REC FROM HD-LINE1 AFTER ADVANCING PAGE
           WRITE OUT-REC FROM HD-LINE2 AFTER ADVANCING 2 LINES
           MOVE ZERO TO WS-LINE-CTR.
       400-AVG-RTN.
           IF WS-LINE-CTR >= 5
               PERFORM 300-HEADING-RTN
           END-IF
           MOVE ID-NO-IN TO ID-NO-OUT
           MOVE NAME-IN TO NAME-OUT
           COMPUTE AVG-OUT = EXAM1 + EXAM2 + EXAM3 + EXAM4
           DIVIDE 4 INTO AVG-OUT
      *        ON SIZE ERROR
      *        STOP RUN
           END-DIVIDE
           WRITE OUT-REC FROM DTL-LINE AFTER ADVANCING 2 LINES
           ADD 1 TO WS-LINE-CTR.
       500-CLOSE-RTN.
           CLOSE IN-FILE1
           CLOSE OUT-FILE.
