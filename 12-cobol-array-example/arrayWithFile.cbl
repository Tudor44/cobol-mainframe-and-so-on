      *07-05-2020
      *READ AN INPUT FILE AND WRITE AN OUTPUT FILE WHICH CONTAINS
      *THE TEMPERATURE OF EACH HOUR WITHIN 24 HOURS IN 3 FORMATS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRAY-WITH-FILE.
       AUTHOR. GAETANO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO
           "/Users/gaetanodorsi/arrayFile.txt"
               ORGANISATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO
           "/Users/gaetanodorsi/arrayFile1.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
           01 TEMP-REC.
               05 TEMPERATURE OCCURS 24 TIMES         PIC 99.
       FD OUT-FILE.
           01 PRINT-REC                               PIC X(132).
       WORKING-STORAGE SECTION.
       01 STORED-AREAS.
           05 ARE-THERE-MORE-RECS                   PIC XXX VALUE "YES".
           05 SUB                                   PIC 99.
           05 AM-SUB                                PIC 99.
           05 PM-SUB                                PIC 99.
           05 SUB1                                  PIC 99.
           05 SUB2                                  PIC 99.
       01 TEMP-OUT-RECORD-1.
           05 FILLER                               PIC X(50).
           05 TEMPERATURE-OUT                      PIC Z9.
           05 FILLER                               PIC X(78).
       01 TEMP-OUT-RECORD-2.
           05 FILLER                               PIC X(5).
           05 AM-OUT                               PIC Z9.
           05 FILLER                               PIC X(20).
           05 PM-OUT                               PIC Z9.
           05 FILLER                               PIC X(69).
       01 TEMP-OUT-RECORD-3.
           05 FILLER                               PIC X(10).
           05 ENTRIES OCCURS 12 TIMES.
               10 TEMP-OUT                         PIC Z9.
               10 FILLER                           PIC XX.
           05 FILLER                               PIC X(50).
       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
           OPEN INPUT IN-FILE
                OUTPUT OUT-FILE
           READ IN-FILE
               AT END
                   MOVE "NO" TO ARE-THERE-MORE-RECS
           END-READ
           PERFORM 200-ONE-TEMP-PER-LINE
           PERFORM 300-AM-AND-PM-TEMP-PER-LINE
           PERFORM 500-TWELVE-TEMP-PER-LINE
           CLOSE IN-FILE
                 OUT-FILE
           STOP RUN.
       200-ONE-TEMP-PER-LINE.
           MOVE SPACES TO TEMP-OUT-RECORD-1
           PERFORM VARYING SUB FROM 1 BY 1
                   UNTIL SUB > 24
                   MOVE TEMPERATURE (SUB) TO TEMPERATURE-OUT
                   WRITE PRINT-REC FROM TEMP-OUT-RECORD-1
           END-PERFORM.

       300-AM-AND-PM-TEMP-PER-LINE.
           MOVE SPACES TO TEMP-OUT-RECORD-2
           PERFORM 400-EACH-LINE-RTN VARYING AM-SUB FROM 1 BY 1
                                     UNTIL AM-SUB > 12.

       400-EACH-LINE-RTN.
           MOVE TEMPERATURE (AM-SUB) TO AM-OUT
           ADD 12 TO AM-SUB GIVING PM-SUB
           MOVE TEMPERATURE (PM-SUB) TO PM-OUT
        WRITE PRINT-REC FROM TEMP-OUT-RECORD-2 AFTER ADVANCING 2 LINES.

       500-TWELVE-TEMP-PER-LINE.
           MOVE SPACES TO TEMP-OUT-RECORD-3.
           MOVE 1 TO SUB1
           PERFORM 600-PRINT-RTN 2 TIMES.

       600-PRINT-RTN.
            PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 12
                   MOVE TEMPERATURE (SUB1) TO TEMP-OUT (SUB2)
                   ADD 1 TO SUB1
            END-PERFORM
        WRITE PRINT-REC FROM TEMP-OUT-RECORD-3 AFTER ADVANCING 2 LINES.
