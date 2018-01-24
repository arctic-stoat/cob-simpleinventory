000000* MIT License
      * Copyright (c) 2018 Christer Stig Åke Landstedt
      * 
      * Permission is hereby granted, free of charge, to any person obtaining a copy
      * of this software and associated documentation files (the "Software"), to deal
      * in the Software without restriction, including without limitation the rights
      * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      * copies of the Software, and to permit persons to whom the Software is
      * furnished to do so, subject to the following conditions:
      * 
      * The above copyright notice and this permission notice shall be included in all
      * copies or substantial portions of the Software.
      * 
      * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      * SOFTWARE.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob-simpleinventory.
       AUTHOR.  "Christer Stig Åke Landstedt".

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT DATAFILE ASSIGN TO "cob-simpleinventory.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS IKEY.
       
       DATA DIVISION.
         FILE SECTION.
         FD DATAFILE
           RECORD CONTAINS 100 CHARACTERS.
         01 DATAFILEFD.
           05 IKEY PIC 9(4).
           05 MN PIC X(9).
           05 NAME PIC X(16).
           05 DES PIC X(40).
           05 INS PIC 9(4)V9(2).
           05 COST PIC 9(5)V9(2).
           05 ICURRENCY PIC X(3).
         WORKING-STORAGE SECTION.
         01 WS-ENDOFFILE PIC 9 VALUE ZERO. 
         01 WS-DATAFILEFD.
           05 WS-IKEY PIC 9(4).
           05 WS-MN PIC X(9).
           05 WS-NAME PIC X(16).
           05 WS-DES PIC X(40).
           05 WS-INS PIC 9(4)V9(2).
           05 WS-COST PIC 9(5)V9(2).
           05 WS-ICURRENCY PIC X(3).
         01 DATEANDTIME.
           05 CURRENTDATE.
             10 YY PIC 99.
             10 MM PIC 99.
             10 DD PIC 99.
           05 CURRENTTIME.
             10 TIMEHH PIC 99.
             10 TIMEMM PIC 99.
             10 TIMESS PIC 99.
         01 CURRENTDATE2.
           05 YY2 PIC 9999.
           05 MM2 PIC 99.
           05 DD2 PIC 99.

         LOCAL-STORAGE SECTION.
         01 USER-SELECTION PIC 9 VALUE ZERO.
         01 IID-SELECTION PIC 9(4) VALUE ZERO.
         01 LS-DATAFILE.
           05 LS-IKEY PIC X(12) VALUE "Part Number".
           05 LS-MN PIC X(15) VALUE "| Model Number".
           05 LS-NAME PIC X(19) VALUE "| Name".
           05 LS-DES PIC X(43) VALUE "| Description".
           05 LS-INS PIC X(11) VALUE "| In Stock".
           05 LS-COST PIC X(6) VALUE "| Cost".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
       0000SELECTIONSTART.
         MOVE 0 TO USER-SELECTION.
         ACCEPT CURRENTDATE2 FROM DATE yyyymmdd.
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".
         DISPLAY "Cobol Simple Inventory 0.1 "YY2"-"MM2"-"DD2.
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".
         PERFORM UNTIL USER-SELECTION>0
           DISPLAY "MENU"
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
                   "---------------"
           DISPLAY "    1 : Inventory"
           DISPLAY "    2 : Information"
           DISPLAY "    3 : Exit application"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 GO TO 0000SELECTIONINVENTORY
             WHEN 2 GO TO 0000SELECTIONINFO
             WHEN 3 GO TO 0000SELECTIONQUIT
             WHEN OTHER PERFORM 0000SELECTIONSTARTERROR
           END-EVALUATE
         END-PERFORM.

       0000SELECTIONSTARTERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONSTART.
       
       0000SELECTIONINVENTORY.
         ACCEPT CURRENTDATE2 FROM DATE yyyymmdd.
         MOVE 0 TO USER-SELECTION.
         DISPLAY " ".
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".
         DISPLAY "Inventory "YY2"-"MM2"-"DD2.
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".
         DISPLAY LS-DATAFILE
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".

         OPEN I-O DATAFILE.
         PERFORM UNTIL WS-ENDOFFILE = 1
           READ DATAFILE INTO WS-DATAFILEFD
             AT END MOVE 1 TO WS-ENDOFFILE
             NOT AT END
               DISPLAY IKEY "        | " MN "    | "
                 NAME " | " DES " | " INS "  | " COST " " ICURRENCY
           END-READ    
         END-PERFORM.
         CLOSE DATAFILE.
         MOVE 0 TO WS-ENDOFFILE.

         PERFORM UNTIL USER-SELECTION>0
           DISPLAY " "
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
                   "---------------"
           DISPLAY "MENU"
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
                   "---------------"
           DISPLAY "    1 : Add Item to Inventory"
           DISPLAY "    2 : Edit Inventory Item"
           DISPLAY "    3 : Delete Item In Inventory"
           DISPLAY "    4 : Delete ALL Inventory"
           DISPLAY "    5 : Go To Main Menu"
           DISPLAY "    6 : Exit Application"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 PERFORM 0000SELECTIONADD
             WHEN 2 PERFORM 0000SELECTIONEDIT
             WHEN 3 PERFORM 0000SELECTIONDELETE
             WHEN 4 PERFORM 0000SELECTIONDELETEALL
             WHEN 5 PERFORM 0000SELECTIONSTART
             WHEN 6 GO TO 0000SELECTIONQUIT
             WHEN OTHER PERFORM 0000SELECTIONCONTACTSERROR
           END-EVALUATE
         END-PERFORM.

       0000SELECTIONCONTACTSERROR.

         DISPLAY " ".
         DISPLAY "!ERROR WRONG INPUT!".
         GO TO 0000SELECTIONINVENTORY.

       0000SELECTIONADD.
       MOVE 0 TO USER-SELECTION.

       DISPLAY " ".
       DISPLAY "Enter Part Number (4 digets):".
       ACCEPT WS-IKEY.
       IF WS-IKEY IS NUMERIC
         ELSE
           DISPLAY "!!!ERROR WRONG INPUT"
           GO TO 0000SELECTIONINVENTORY
       END-IF.
       DISPLAY "Enter Model Number (max 9 characters):"
       ACCEPT WS-MN
       DISPLAY "Enter Name (max 16 characters):"
       ACCEPT WS-NAME
       DISPLAY "Enter Description (max 40 characters):"
       ACCEPT WS-DES
       DISPLAY "Enter In Stock:"
       ACCEPT WS-INS
       DISPLAY "Enter Cost Per Unit:"
       ACCEPT WS-COST
       DISPLAY "Enter Currency (max 3 characters):"
       ACCEPT WS-ICURRENCY

       MOVE WS-IKEY TO IKEY.
       MOVE WS-MN TO MN.
       MOVE WS-NAME TO NAME.
       MOVE WS-DES TO DES.
       MOVE WS-INS TO INS.
       MOVE WS-COST TO COST.
       MOVE WS-ICURRENCY TO ICURRENCY.

       MOVE WS-DATAFILEFD TO DATAFILEFD.

       OPEN I-O DATAFILE.
       WRITE DATAFILEFD
         INVALID KEY DISPLAY"!ERROR RECORD ALREADY EXIST!"
         NOT INVALID KEY DISPLAY "Item Added."
       END-WRITE.
       CLOSE DATAFILE.

       GO TO 0000SELECTIONINVENTORY.

       0000SELECTIONEDIT.
         MOVE 0 TO USER-SELECTION.

         DISPLAY " ".
         DISPLAY "Enter Part Number To Edit:".
         ACCEPT WS-IKEY.

         MOVE WS-IKEY TO IKEY.

         OPEN I-O DATAFILE.
           READ DATAFILE INTO WS-DATAFILEFD
             KEY IS IKEY
             INVALID KEY
               DISPLAY "!ERROR PART NUMBER DOSE NOT EXIST!"
               PERFORM 0000SELECTIONEDITERROR2
             NOT INVALID KEY
               IF WS-IKEY IS NUMERIC
                 DISPLAY " "
                 ELSE
                   DISPLAY "!!!ERROR WRONG INPUT"
                   PERFORM 0000SELECTIONEDITERROR2
                 END-IF
           END-READ.
         CLOSE DATAFILE.

       PERFORM UNTIL USER-SELECTION>0
         DISPLAY " "
         DISPLAY "---------------------------------------------------"
                 "---------------------------------------------------"
                 "-----------"
         DISPLAY "MENU"
         DISPLAY "---------------------------------------------------"
                 "---------------------------------------------------"
                 "-----------"
         DISPLAY "    1 : Edit Model Number"
         DISPLAY "    2 : Edit Name"
         DISPLAY "    3 : Edit Description"
         DISPLAY "    4 : Edit Stock"
         DISPLAY "    5 : Edit Cost"
         DISPLAY "    6 : Edit Currency"
         DISPLAY "    7 : Cancel Edit"
         DISPLAY "Select number and press Enter: "
         ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 PERFORM 0000SELECTIONEDITMODEL
           WHEN 2 PERFORM 0000SELECTIONEDITNAME
           WHEN 3 PERFORM 0000SELECTIONEDITDESCRIPTION
           WHEN 4 PERFORM 0000SELECTIONEDITSTOCK
           WHEN 5 PERFORM 0000SELECTIONEDITCOST
           WHEN 6 PERFORM 0000SELECTIONEDITCURRENCY
           WHEN 7 GO TO 0000SELECTIONINVENTORY
           WHEN OTHER PERFORM 0000SELECTIONEDITERROR
         END-EVALUATE
       END-PERFORM.

         0000SELECTIONEDITMODEL.
           DISPLAY " ".
           DISPLAY "New Model Number:"
           ACCEPT WS-MN.
           GO TO 0000CONTINUEEDIT.

         0000SELECTIONEDITNAME.
           DISPLAY " ".
           DISPLAY "New Name:"
           ACCEPT WS-NAME.
           GO TO 0000CONTINUEEDIT.

         0000SELECTIONEDITDESCRIPTION.
           DISPLAY " ".
           DISPLAY "New Description:"
           ACCEPT WS-DES.
           GO TO 0000CONTINUEEDIT.

         0000SELECTIONEDITSTOCK.
           DISPLAY " ".
           DISPLAY "New Stock:"
           ACCEPT WS-INS.
           GO TO 0000CONTINUEEDIT.

         0000SELECTIONEDITCOST.
           DISPLAY " ".
           DISPLAY "New Cost:"
           ACCEPT WS-COST.
           GO TO 0000CONTINUEEDIT.

         0000SELECTIONEDITCURRENCY.
           DISPLAY " ".
           DISPLAY "New Currency:"
           ACCEPT WS-ICURRENCY.
           GO TO 0000CONTINUEEDIT.

         0000CONTINUEEDIT.

         OPEN I-O DATAFILE.
         MOVE WS-IKEY TO IKEY.
         MOVE WS-MN TO MN.
         MOVE WS-NAME TO NAME.
         MOVE WS-DES TO DES.
         MOVE WS-INS TO INS.
         MOVE WS-COST TO COST.
         MOVE WS-ICURRENCY TO ICURRENCY.
           REWRITE DATAFILEFD
             INVALID KEY DISPLAY"!ERROR CONTACT DOSE NOT EXIST!"
             NOT INVALID KEY DISPLAY "Item Updated."
           END-REWRITE.
         CLOSE DATAFILE.

       GO TO 0000SELECTIONINVENTORY.

       0000SELECTIONEDITERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONEDIT.

       0000SELECTIONEDITERROR2.

         CLOSE DATAFILE.
         GO TO 0000SELECTIONINVENTORY.

       0000SELECTIONDELETE.
         MOVE 0 TO USER-SELECTION.
         DISPLAY " ".
         DISPLAY "---------------------------------------------------"
                 "---------------------------------------------------"
                 "---------------".
         DISPLAY "Enter Part Number Of Item To Be Deleted:".
         ACCEPT WS-IKEY.

         MOVE WS-IKEY TO IKEY.

         OPEN I-O DATAFILE.
           READ DATAFILE INTO WS-DATAFILEFD
             KEY IS IKEY
             INVALID KEY
               DISPLAY "!ERROR PART NUMBER DOSE NOT EXIST!"
               PERFORM 0000SELECTIONDELETEERROR2
             NOT INVALID KEY
               IF WS-IKEY IS NUMERIC
                 MOVE WS-IKEY TO IKEY
                 ELSE
                   DISPLAY "!ERROR WRONG INPUT!"
                   PERFORM 0000SELECTIONDELETEERROR2
               END-IF
           END-READ.
         CLOSE DATAFILE.

         PERFORM UNTIL USER-SELECTION>0
           DISPLAY "Are you sure that you want to delete this item?"
           DISPLAY "    1 : Yes I want to delete this item"
           DISPLAY "    2 : No!"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 PERFORM 0000CONTINUEDELETE
             WHEN 2 PERFORM 0000SELECTIONINVENTORY
             WHEN OTHER PERFORM 0000SELECTIONDELETEERROR
           END-EVALUATE
         END-PERFORM.

         0000CONTINUEDELETE.

         OPEN I-O DATAFILE.
         DELETE DATAFILE
           INVALID KEY DISPLAY "!ERROR CONTACT DOSE NOT EXIST!"
           NOT INVALID KEY DISPLAY "Item Deleted."
         END-DELETE.
         CLOSE DATAFILE.

       GO TO 0000SELECTIONINVENTORY.

       0000SELECTIONDELETEERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONDELETE.

       0000SELECTIONDELETEERROR2.

         CLOSE DATAFILE.
         GO TO 0000SELECTIONDELETE.

       0000SELECTIONDELETEALL.
         MOVE 0 TO USER-SELECTION.
         DISPLAY " ".
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
                   "---------------".
         PERFORM UNTIL USER-SELECTION>0
           DISPLAY "Are you sure that you want to DELETE ALL items?"
           DISPLAY "    1 : Yes I want to DELETE ALL item."
           DISPLAY "    2 : No!"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 PERFORM 0000CONTINUEDELETEALL
             WHEN 2 PERFORM 0000SELECTIONINVENTORY
             WHEN OTHER PERFORM 0000SELECTIONDELETEALLERROR
           END-EVALUATE
         END-PERFORM.

       0000CONTINUEDELETEALL.

         DELETE FILE
           DATAFILE
         END-DELETE.

         OPEN OUTPUT DATAFILE.
           MOVE 0001 TO IKEY.
           MOVE "0002-0003" TO MN.
           MOVE "Product X" TO NAME
           MOVE "Dose this thing." TO DES
           MOVE 1 TO INS
           MOVE 1 TO COST
           MOVE "SEK" TO ICURRENCY
           WRITE DATAFILEFD
           END-WRITE.
         CLOSE DATAFILE.

         GO TO 0000SELECTIONINVENTORY.

       0000SELECTIONDELETEALLERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONDELETEALL.

       0000SELECTIONINFO.
         MOVE 0 TO USER-SELECTION.

         DISPLAY " ".
         DISPLAY "---------------------------------------------------"
                 "---------------------------------------------------"
                 "---------------".
         DISPLAY "Application information".
         DISPLAY "---------------------------------------------------".
         DISPLAY "Application: Cobol Simple Inventory 0.1".
         DISPLAY "Made with: ".
         DISPLAY "* Ubuntu 16.04".
         DISPLAY "* Gedit 3.18.3".
         DISPLAY "* GnuCobol(OpenCobol) 2.2".
         DISPLAY "---------------------------------------------------".
         DISPLAY "MIT License".
         DISPLAY "Copyright (c) 2018 Christer Stig Åke Landstedt".
         DISPLAY " ".
         DISPLAY 
          "Permission is hereby granted, free of charge, to any "
          "person obtaining a copy of this software and "
          "associated documentation files (the ""Software""), "
          "to deal in the Software without restriction, "
          "including without limitation the rights "
          "to use, copy, modify, merge, publish, distribute, "
          "sublicense, and/or sell copies of the Software,"
          "and to permit persons to whom the Software is "
          "furnished to do so, subject to the following "
          "conditions:".
         DISPLAY " ".
         DISPLAY 
          "The above copyright notice and this permission notice "
          "shall be included in all copies or substantial "
          "portions of the Software.".
         DISPLAY " ".
         DISPLAY 
          "THE SOFTWARE IS PROVIDED ""AS IS"", WITHOUT WARRANTY "
          "OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT "
          "LIMITED TO THE WARRANTIES OF MERCHANTABILITY, "
          "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. "
          "IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS "
          "BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER "
          "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR "
          "OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION "
          "WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE "
          "SOFTWARE.".
         GO TO 0000SELECTIONSTART.
       
       0000SELECTIONQUIT.
       STOP-RUN.
