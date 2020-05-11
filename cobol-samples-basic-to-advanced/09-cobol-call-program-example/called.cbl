      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLED.
       AUTHOR. GAETANO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
           01 DATA-3      PIC X(5).
           01 DATA-4      PIC 9(2).
       PROCEDURE DIVISION USING DATA-3 DATA-4.
       MAIN-PROCEDURE.
            DISPLAY "I m in called program now"
            DISPLAY "VALUES OF DATA-3 AND DATA-4 ARE CARRIED FROM"
            DISPLAY "CALLING PROGRAM AND THE ARE " DATA-3 " "DATA-4
               MOVE "UDIT" TO DATA-3
               MOVE 22 TO DATA-4
       EXIT PROGRAM.
