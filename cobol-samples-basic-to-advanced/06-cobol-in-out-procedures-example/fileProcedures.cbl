      ******************************************************************
      * Author:tudor44
      * Date:30/04/2020
      * Purpose:PROGRAM FOR DEMONSTRATE USE OF INPUT AND OUTPUT PROCEDURE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       AUTHOR. GAETANO.
       PROGRAM-ID. IN-OUT-PROCEDURE-PROGRAM.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
       SELECT IN-FILE ASSIGN TO
       "/Users/gaetanodorsi/Downloads/FILE-IN.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       SELECT OUT-FILE ASSIGN TO
       "/Users/gaetanodorsi/Downloads/FILE-OUT.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
             SELECT WORK-FILE ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE
           RECORD CONTAINS 28 CHARACTERS.
           01 IN-REC.
               02 TERR-IN              PIC XX.
               02 AREAX-IN             PIC XXX.
               02 DEPT-IN              PIC XXX.
               02 LAST-NAME-IN         PIC X(12).
               02 FIRST-NAME-IN        PIC X(8).
       FD OUT-FILE
           RECORD CONTAINS 132 CHARACTERS.
           01 OUT-REC                  PIC X(132).
       SD WORK-FILE.
           01 WORK-REC.
               02 TERR-WORK                 PIC XX.
               02 AREAX-WORK                PIC XXX.
               02 DEPT-WORK                 PIC XXX.
               02 LAST-NAME-WORK            PIC X(12).
               02 FIRST-NAME-WORK           PIC X(8).
       WORKING-STORAGE SECTION.
           01 ARE-THERE-MORE-RECORDS   PIC XXX VALUE "YES".
           01 CTR                      PIC 99 VALUE ZEROES.
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           SORT WORK-FILE
               ON ASCENDING KEY TERR-WORK
                   ASCENDING KEY AREAX-WORK
                       ASCENDING KEY DEPT-WORK
               INPUT PROCEDURE IS 200-COUNT1-INPUT
               OUTPUT PROCEDURE IS 300-ELIM-BLANK-TERR
               DISPLAY CTR " RECORDS COUNTED"
               PERFORM 500-CLOSE-PARA
            STOP RUN.
       200-COUNT1-INPUT.
           OPEN INPUT IN-FILE
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = "NO"
               READ IN-FILE
                   AT END
                       MOVE "NO" TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 250-CALC-RTN
                END-READ
           END-PERFORM.
       250-CALC-RTN.
           ADD 1 TO CTR
           RELEASE WORK-REC FROM IN-REC.

       300-ELIM-BLANK-TERR.
           OPEN OUTPUT OUT-FILE
           MOVE "YES" TO ARE-THERE-MORE-RECORDS
           PERFORM UNTIL ARE-THERE-MORE-RECORDS="NO"
               RETURN WORK-FILE
                   AT END
                       MOVE "NO" TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 400-WRITE-PARA
               END-RETURN
           END-PERFORM.

       400-WRITE-PARA.
           EVALUATE TRUE
               WHEN TERR-WORK = ZEROES
                   CONTINUE
               WHEN TERR-WORK = SPACES
                   CONTINUE
               WHEN OTHER
                   WRITE OUT-REC FROM WORK-REC
           END-EVALUATE.
       500-CLOSE-PARA.
           CLOSE IN-FILE.
           CLOSE OUT-FILE.
