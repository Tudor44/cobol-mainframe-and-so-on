      ******************************************************************
      * Author:Tudor44
      * Date:25/04/2020
      * Purpose: Exercise for lesson2
      * Tectonics: cobc
      ******************************************************************
        IDENTIFICATION DIVISION.
        PROGRAM-ID. LES2-READWRITE.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
               01 WS-DATA1 PIC X(20).
               01 WS-DATA2 PIC 99.
               01 WS-DATA3 PIC X(20).
               01 WS-DATA4 PIC 99.
               01 WS-CHOICE PIC XXX VALUE "YES".
        PROCEDURE DIVISION.
        LES2-READWRITE.
         PERFORM UNTIL WS-CHOICE = "NO"
           INITIALISE WS-DATA1 WS-DATA2 WS-DATA3 WS-DATA4
           MOVE "YES" TO WS-CHOICE
           DISPLAY "ENTER THE NAME OF EMPLOYEE : "
           ACCEPT WS-DATA1
           DISPLAY "ENTER THE NUM OF EMPLOYEE : "
           ACCEPT WS-DATA2
           MOVE WS-DATA1 TO WS-DATA3
           MOVE WS-DATA2 TO WS-DATA4
           DISPLAY "NAME OF THE EMPLOYEE IS : " WS-DATA3
           DISPLAY "NUMBER OF THE EMPLOYEE IS : " WS-DATA4
           DISPLAY "ENTER YOUR CHOICE YES OR NO: "
           ACCEPT WS-CHOICE
         END-PERFORM
        STOP RUN.
