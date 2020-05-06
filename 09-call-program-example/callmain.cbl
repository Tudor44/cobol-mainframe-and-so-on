      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLMAIN.
       AUTHOR. GAETANO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 DATA-1             PIC X(5) VALUE "VALUE".
           01 DATA-2             PIC 9(2) VALUE 11.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       CALL 'CALLED' USING DATA-1, DATA-2.
       DISPLAY "DATA-1: "DATA-1.
       DISPLAY "DATA-2: "DATA-2.

       STOP RUN.
