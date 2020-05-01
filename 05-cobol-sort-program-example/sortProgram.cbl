      ******************************************************************
      * Author:Gaetano
      * Date:30/04/2020
      * Purpose:SORT ALPHABETICALLY UNORDERED RECORD
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. SORT-PROGRAM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT IN-FILE ASSIGN TO
       "/Users/gaetanodorsi/Desktop/COBOL/lesson5/UNSORTED.txt"
       ORGANISATION IS LINE SEQUENTIAL.
       SELECT OUT-FILE ASSIGN TO
       "/Users/gaetanodorsi/Desktop/COBOL/lesson5/SORTED.txt"
       ORGANISATION IS LINE SEQUENTIAL.
       SELECT WORK-FILE ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE
       RECORD CONTAINS 7 CHARACTERS.
       01 IN-REC.
           02 ITEM-NO-IN PIC XXX.
           02 ITEM-QTY-IN PIC 9999.
       FD OUT-FILE
       RECORD CONTAINS 132 CHARACTERS.
       01 OUT-REC PIC X(132).
       SD WORK-FILE.
       01 WORK-REC.
           02 ITEM-NO-WORK PIC XXX.
           02 ITEM-QTY-WORK PIC 9999.
       WORKING-STORAGE SECTION.
      * 01 ARE-THERE-MORE-RECORDS PIC XXX VALUE "YES".
       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.
           SORT WORK-FILE
               ON ASCENDING KEY ITEM-NO-IN
                            USING IN-FILE
                            GIVING OUT-FILE
            STOP RUN.
