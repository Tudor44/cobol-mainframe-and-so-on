       IDENTIFICATION DIVISION.
       AUTHOR. GAETANO.
       PROGRAM-ID. ARRAY-SIMPLE-EXAMPLE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TEMP-REC.
           10 TEMP-IN          OCCURS 24 TIMES     PIC 99 VALUE 30.
       01 SUB                          PIC 99 VALUE ZERO.
       01 AVG-TEMP                     PIC $9(3)V99.
       01 TOTAL-TEMP                   PIC 9999.
       PROCEDURE DIVISION.

       000-MAIN-PROCEDURE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 24
                   ADD TEMP-IN (SUB) TO TOTAL-TEMP
           END-PERFORM.
                   COMPUTE AVG-TEMP = TOTAL-TEMP / 24
                   DISPLAY TOTAL-TEMP " TOTAL VALUE IS "
                   DISPLAY "AVG TEMP" " " AVG-TEMP
           STOP RUN.
