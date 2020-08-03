      ******************************************************************
      * AUTHOR:  JOSUE RIVAS
      * DATE:    02/08/2020
      * PURPOSE: A SIMPLY CALCULATOR THAT CAN ADD, SUBTRACT, MULTIPLY
      *          AND DIVIDE, WORKS ONLY WITH INTEGER NUMBERS.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCTOR1.
      ******************************************************************
      *              D  A  T  A     D  I  V  I  S  I  O  N
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *            M  E  S  S  A  G  E  S     A  R  E  A
      ******************************************************************
           01 ME-MESSAGES.
               05 ME-WELCOME                  PIC X(26)   VALUE
               'WELCOME TO YOUR CALCULATOR'.
               05 ME-SELECT-OPTION            PIC X(43)   VALUE
               'ENTER ANY OF THE FOLLOWING OPTIONS TO DO : '.
               05 ME-OPTIONS                  PIC X(43)   VALUE
               'A= ADD, S= SUBTRACT, M= MULTIPLY, D= DIVIDE'.
               05 ME-INCORRECT                PIC X(24)   VALUE
               'OPTION ENTERED INCORRECT'.
               05 ME-RESULT                   PIC X(15)   VALUE
               'THE RESULT IS: '.
               05 ME-FIRST-NUMBER             PIC X(24)   VALUE
               'ENTER THE FIRST NUMBER: '.
               05 ME-SECOND-NUMBER            PIC X(25)   VALUE
               'ENTER THE SECOND NUMBER: '.
               05 ME-GOODBYE                  PIC X(29)   VALUE
               'FINISHING CALCULATOR, GOODBYE'.
      ******************************************************************
      *                    AREA   OF  WORK  VARIABLES
      ******************************************************************
           01 WS-VARS.
               05 WS-OPTIONS                  PIC X(01).
               05 WS-A                        PIC 9(06).
               05 WS-B                        PIC 9(06).
               05 WS-R                        PIC 9(07).
               05 WS-VALID-OPTIONS            PIC X(01).
                   88 WS-LETTERS-ASMD         VALUE 'A','S','M','D'.
      ******************************************************************
      *           C  O   N  S  T  A  N  T  S     A  R  E  A
      ******************************************************************
           01 CT-CONSTANTS.
               05 CT-SPACES                   PIC X(01)   VALUE SPACES.
               05 CT-A                        PIC X(01)   VALUE 'A'.
               05 CT-S                        PIC X(01)   VALUE 'S'.
               05 CT-M                        PIC X(01)   VALUE 'M'.
               05 CT-D                        PIC X(01)   VALUE 'D'.
      ******************************************************************
      *                 F  L  A  G  S     A  R  E  A
      ******************************************************************
           01 SW-FLAGS.
               05 SW-OPTIONS                  PIC 9(1)    VALUE  0.
                   88 SW-OPTION-NO-SELECTED               VALUE  0.
                   88 SW-OPTION-SELECTED                  VALUE  1.
      ******************************************************************
      *       P  R  O  C  E  D  U  R  E     D  I  V  I  S  I  O  N
      ******************************************************************
       PROCEDURE DIVISION.
       000-CONTROL.
      *============
           PERFORM 100-START
           PERFORM 200-PROCESS
           PERFORM 300-END
           .
      *
       100-START.
      *==========
           DISPLAY ME-WELCOME
           .
      *
       200-PROCESS.
      *============
           PERFORM 205-OPERATION-SELECT

           PERFORM 215-OPERATION-ENTER

           PERFORM 240-OPERATION-EXECUTION

           PERFORM 245-OPERATION-DISPLAY
           .
      *
       205-OPERATION-SELECT.
      *=====================
           PERFORM UNTIL   SW-OPTION-SELECTED
                   DISPLAY ME-SELECT-OPTION
                           ME-OPTIONS
                   ACCEPT  WS-OPTIONS
                   PERFORM 210-OPERATION-CHECK
           END-PERFORM
           .
      *
       210-OPERATION-CHECK.
      *====================
           MOVE WS-OPTIONS                    TO       WS-VALID-OPTIONS

           IF   WS-LETTERS-ASMD
                SET     SW-OPTION-SELECTED    TO       TRUE
           ELSE
                DISPLAY ME-INCORRECT
                        CT-SPACES
           END-IF
           .
      *
       215-OPERATION-ENTER.
      *===================
           DISPLAY ME-FIRST-NUMBER
           ACCEPT  WS-A
      *
           DISPLAY ME-SECOND-NUMBER
           ACCEPT  WS-B
           .
       220-OPERATION-ADD.
      *==================
           ADD  WS-A                          TO       WS-B
                                              GIVING   WS-R
           .
      *
       225-OPERATION-SUBTRACT.
      *=======================
           SUBTRACT  WS-A                     FROM     WS-B
                                              GIVING   WS-R
           .
      *
       230-OPERATION-MULTIPLY.
      *=======================
           MULTIPLY  WS-A                     BY       WS-B
                                              GIVING   WS-R
           .
      *
       235-OPERATION-DIVIDE.
      *=====================
           DIVIDE    WS-A                     INTO     WS-B
                                              GIVING   WS-R
           .
      *
       240-OPERATION-EXECUTION.
      *========================
           EVALUATE  WS-OPTIONS
               WHEN  CT-A
                   PERFORM 220-OPERATION-ADD
               WHEN  CT-S
                   PERFORM 225-OPERATION-SUBTRACT
               WHEN  CT-M
                   PERFORM 230-OPERATION-MULTIPLY
               WHEN  CT-D
                   PERFORM 235-OPERATION-DIVIDE
           END-EVALUATE
           .
      *
       245-OPERATION-DISPLAY.
      *======================
           DISPLAY   ME-RESULT  WS-R
           .
      *
       300-END.
      *=========
           DISPLAY   ME-GOODBYE
           STOP RUN
           .
