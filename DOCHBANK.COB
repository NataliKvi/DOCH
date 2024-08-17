      *-----------------------------------------------------------------
       IDENTIFICATION                      DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                         DOCHBANK.
      *AUTHOR                              NATALI KVITELASHVILI.
      *DATE-WRITTEN                        15/08/2024.
      *REMARKS      Report that specifies:
      *             *Each customers:account number,name and balance.
      *             *Total balance per page.
      *             *Total balance for the whole report at the end.
      *-----------------------------------------------------------------
       ENVIRONMENT                         DIVISION.
      *-----------------------------------------------------------------
       INPUT-OUTPUT                        SECTION.
      *-----------------------------------------------------------------
       FILE-CONTROL.
           SELECT K01-LAKOCHOT-FILE
             ASSIGN       TO "C:\COBOL-PROJECTS\DOCH\FILES\LAKOCHOT.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS  IS MH01-FILE-STS.

           SELECT D01-DOCH-PRN
             ASSIGN       TO "C:\COBOL-PROJECTS\DOCH\FILES\DOCH.PRN"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS  IS MH01-FILE-STS.
      *-----------------------------------------------------------------
       DATA                                DIVISION.
      *-----------------------------------------------------------------
       FILE                                SECTION.
      *-----------------------------------------------------------------
       FD K01-LAKOCHOT-FILE
           RECORDING MODE F.
       01 K01-LAKOCHOT-REC.
           COPY "C:\COBOL-PROJECTS\DOCH\COPYS\LAKOCHOT.COB".

       FD  D01-DOCH-PRN
           LABEL RECORD IS OMITTED.
       01  D01-SHURA                       PIC X(80).
      *-----------------------------------------------------------------
       WORKING-STORAGE                     SECTION.
      *-----------------------------------------------------------------
      *Cxx-KVUIM
      *-----------------------------------------------------------------
       01 C00.
           03 C01-GODEL-DAF                PIC 99              VALUE 56.
      *-----------------------------------------------------------------
      *EZxx-EZER
      *-----------------------------------------------------------------
       01 EZ00.
           03 EZ01-TAKALOT.
           COPY "C:\COBOL-PROJECTS\DOCH\COPYS\TAKALOT.COB".

           03 EZ02-TAARICH.
               05 EZ02-YYYY                PIC 9(4).
               05 EZ02-MM                  PIC 99.
               05 EZ02-DD                  PIC 99.
      *-----------------------------------------------------------------
      *Hxx-HODAOT
      *-----------------------------------------------------------------
       01 H00.
           03 H01-TAKALOT.
             05 H01-TAKALA                 PIC X(10)           VALUE
                '**TAKALA**'.
             05 H01-SHEM-TUCHNIT           PIC X(14)           VALUE
                'SHEM TUCHNIT: '.
             05 H01-STS-KOVETZ             PIC X(12)           VALUE
                'STS KOVETZ: '.
             05 H01-SHEM-KOVETZ            PIC X(13)           VALUE
                'SHEM KOVETZ: '.
             05 H01-SHEM-KETA              PIC X(11)           VALUE
                'SHEM KETA: '.
      *-----------------------------------------------------------------
      *IXxx-INDEXIM
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      *Kxx-KELET
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      *Mxx-MONIM
      *-----------------------------------------------------------------
       01 M00.
           03 M01-DAF                      PIC 9(3)          VALUE ZERO.
           03 M02-SHUROT                   PIC 9(3).
           03 M03-SCHUM-DAF                PIC S9(8)V99
                                           SIGN IS LEADING SEPARATE.
           03 M04-SCHUM-KLALI              PIC S9(8)V99
                                           SIGN IS LEADING SEPARATE
                                                             VALUE ZERO.
      *-----------------------------------------------------------------
      *MHxx-MAFTECHOT-HASHVAA
      *-----------------------------------------------------------------
       01 MH00.
           03 MH01-FILE-STS                PIC XX.
               88 MH01-FS-TAKIN                              VALUE "00".
      *-----------------------------------------------------------------
      *Pxx-PELET
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      * PRxx-PRAMETRIM
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      *SXxx-SHURUT
      *-----------------------------------------------------------------
       01 S00.
           03 SH01.
               05 FILLER                   PIC XX         VALUE SPACE.
               05 SH01-DD                  PIC Z9/.
               05 SH01-MM                  PIC 99/.
               05 SH01-YYYY                PIC 9(4).
               05 FILLER                   PIC X(52)      VALUE
                  " :TAARICH".
               05 SH01-N-DAF               PIC ZZ9.
               05 FILLER                   PIC X(13)      VALUE
                  " :DAF MISPAR".
           03 SH02.
               05 FILLER                   PIC X(35)      VALUE SPACE.
               05 FILLER                   PIC X(13)      VALUE
                  "DOCH LAKOCHOT".
           03 SH03.
               05 FILLER                   PIC X(35)      VALUE SPACE.
               05 FILLER                   PIC X(13)      VALUE ALL "-".
           03 SH04.
               05 FILLER                   PIC X(13)      VALUE SPACE.
               05 FILLER                   PIC X(5)       VALUE
                  "YITRA".
               05 FILLER                   PIC X(19)      VALUE SPACE.
               05 FILLER                   PIC X(8)       VALUE
                  "CHESHBON".
               05 FILLER                   PIC X(16)      VALUE SPACE.
               05 FILLER                   PIC X(4)       VALUE
                  "SHEM".
           03 SH05.
               05 FILLER                   PIC X(13)      VALUE SPACE.
               05 FILLER                   PIC X(5)       VALUE ALL '-'.
               05 FILLER                   PIC X(19)      VALUE SPACE.
               05 FILLER                   PIC X(8)       VALUE ALL '-'.
               05 FILLER                   PIC X(16)      VALUE SPACE.
               05 FILLER                   PIC X(4)       VALUE ALL '-'.
           03 SD01.
               05 FILLER                   PIC X(10)      VALUE SPACE.
               05 SD01-YITRA               PIC +999999.99.
               05 FILLER                   PIC X(18)      VALUE SPACE.
               05 SD01-CHESHBON            PIC X(5).
               05 FILLER                   PIC X(15)      VALUE SPACE.
               05 SD01-SHEM                PIC X(10).

           03 ST01.
               05 FILLER                   PIC X(10)      VALUE SPACE.
               05 ST01-SD                  PIC +99999999.99.
               05 FILLER                   PIC X(36)      VALUE SPACE.
               05 ST01-SACH                PIC X(26)      VALUE
                  ":SACH HAKOL LEDAF".
           03 ST02.
               05 FILLER                   PIC X(10)      VALUE SPACE.
               05 ST02-SK                  PIC +99999999.99.
               05 FILLER                   PIC X(36)      VALUE SPACE.
               05 ST02-SACH                PIC X(26)      VALUE
                  ":SACH HAKOL KLALI".
           03 SF01.
               05 FILLER                   PIC X(30)      VALUE SPACE.
               05 FILLER                   PIC X(20)      VALUE
                  "HEMSHECH BEDAF HABA ".
               05 FILLER                   PIC X(30)      VALUE SPACE.
           03 SF02.
               05 FILLER                   PIC X(35)      VALUE SPACE.
               05 FILLER                   PIC X(10)      VALUE
                  "-SOF DOCH-".
               05 FILLER                   PIC X(35)      VALUE SPACE.
      *-----------------------------------------------------------------
      *SWxx-METAGIM
      *-----------------------------------------------------------------
       01 SW00.
           03 SW01-K01                     PIC S9(4) BINARY VALUE 99.
           COPY "C:\COBOL-PROJECTS\DOCH\COPYS\SWK.COB".

           03 SW02-D01                     PIC S9(4) BINARY VALUE 99.
           COPY "C:\COBOL-PROJECTS\DOCH\COPYS\SWK.COB".

           03 SW03-DAF-RISHON              PIC 9.
             88 SW03-KEN                   VALUE 0.
             88 SW03-LO                    VALUE 1.
      *-----------------------------------------------------------------
      *TVxx-TAVLAOT
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       PROCEDURE                           DIVISION.
      *-----------------------------------------------------------------
       DECLARATIVES.
      *-----------------------------------------------------------------
       DC1-K01                              SECTION.
      *-----------------------------------------------------------------
           USE AFTER STANDARD ERROR PROCEDURE ON K01-LAKOCHOT-FILE.
       DC1-00.
           MOVE "C:\COBOL-PROJECTS\DOCH\FILES\LAKOCHOT.DAT" TO
                                                             SHEM-KOVETZ
           PERFORM ZT-TAKALOT.
       DC1-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       DC2-D01                             SECTION.
      *-----------------------------------------------------------------
           USE AFTER STANDARD ERROR PROCEDURE ON D01-DOCH-PRN.
       DC2-00.
           MOVE "C:\COBOL-PROJECTS\DOCH\FILES\DOCH.PRN" TO SHEM-KOVETZ
           PERFORM ZT-TAKALOT.
       DC2-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       ZT-TAKALOT                          SECTION.
      *-----------------------------------------------------------------
       ZT-00.
           DISPLAY H01-TAKALA
           DISPLAY H01-SHEM-TUCHNIT
                   SHEM-TUCHNIT IN EZ01-TAKALOT
           DISPLAY H01-STS-KOVETZ
                   MH01-FILE-STS
           DISPLAY H01-SHEM-KOVETZ
                   SHEM-KOVETZ
           DISPLAY H01-SHEM-KETA
                   SHEM-SEC
           STOP RUN.
      *-----------------------------------------------------------------
       END DECLARATIVES.
      *-----------------------------------------------------------------
       A-MAIN                              SECTION.
      *-----------------------------------------------------------------
       A-00.
           PERFORM     ZA-HATCHLAT-TOCHNIT
           PERFORM UNTIL SOF IN SW01-K01
               PERFORM B-TIPUL
               PERFORM C-KRIA
           END-PERFORM
           PERFORM     ZZ-SYIUM-TOCHNIT
           STOP RUN.
       A-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       B-TIPUL                             SECTION.
      *-----------------------------------------------------------------
       B-00.
           IF M02-SHUROT NOT < C01-GODEL-DAF
           THEN
               MOVE    0 TO M02-SHUROT
               PERFORM S-SIYUMOT
               PERFORM K-KOTAROT
           END-IF

           MOVE SHEM     IN K01-LAKOCHOT-REC TO SD01-SHEM
           MOVE CHESHBON IN K01-LAKOCHOT-REC TO SD01-CHESHBON
           MOVE YITRA    IN K01-LAKOCHOT-REC TO SD01-YITRA

           WRITE D01-SHURA FROM SD01
           AFTER ADVANCING 2 LINES

           ADD 2                         TO M02-SHUROT
           ADD YITRA IN K01-LAKOCHOT-REC TO M03-SCHUM-DAF.
       B-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       C-KRIA                              SECTION.
      *-----------------------------------------------------------------
       C-00.
           MOVE "C-KRIA" TO SHEM-SEC

           READ K01-LAKOCHOT-FILE
               AT END
                   SET SOF IN SW01-K01 TO TRUE
           END-READ.
       C-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       K-KOTAROT                           SECTION.
      *-----------------------------------------------------------------
       K-00.
           MOVE "K-KOTAROT" TO SHEM-SEC
           ADD  1           TO M01-DAF
           MOVE M01-DAF     TO SH01-N-DAF

           EVALUATE TRUE
             WHEN SW03-LO
                  WRITE D01-SHURA FROM SH01
                  AFTER ADVANCING PAGE
             WHEN SW03-KEN
                  WRITE D01-SHURA FROM SH01
                  SET   SW03-LO   TO TRUE
           END-EVALUATE

           WRITE D01-SHURA FROM SH02
           AFTER ADVANCING 3 LINES
           WRITE D01-SHURA FROM SH03
           AFTER ADVANCING 1 LINES
           WRITE D01-SHURA FROM SH04
           AFTER ADVANCING 2 LINES
           WRITE D01-SHURA FROM SH05
           AFTER ADVANCING 1 LINES

           ADD  8 TO M02-SHUROT
           MOVE 0 TO M03-SCHUM-DAF.
       K-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       S-SIYUMOT                           SECTION.
      *-----------------------------------------------------------------
       S-00.
           MOVE  "S-SIYUMOT"   TO SHEM-SEC
           MOVE  M03-SCHUM-DAF TO ST01-SD

           WRITE D01-SHURA   FROM ST01
           AFTER ADVANCING 2 LINES
           WRITE D01-SHURA   FROM SF01
           AFTER ADVANCING 3 LINES

           ADD M03-SCHUM-DAF   TO M04-SCHUM-KLALI.
       S-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       ZA-HATCHLAT-TOCHNIT                 SECTION.
      *-----------------------------------------------------------------
       ZA-00.
           MOVE 'DOCH'                TO SHEM-TUCHNIT IN EZ01-TAKALOT

           MOVE "ZA-HATCHLAT-TOCHNIT" TO SHEM-SEC
           MOVE 0                     TO M01-DAF
                                         M02-SHUROT
                                         M03-SCHUM-DAF
                                         M04-SCHUM-KLALI
           MOVE FUNCTION CURRENT-DATE TO EZ02-TAARICH
           MOVE EZ02-DD               TO SH01-DD
           MOVE EZ02-MM               TO SH01-MM
           MOVE EZ02-YYYY             TO SH01-YYYY
           SET  SW03-KEN              TO TRUE

           OPEN INPUT  K01-LAKOCHOT-FILE
           SET         MAMSHICH IN SW01-K01 TO TRUE
           OPEN OUTPUT D01-DOCH-PRN
           SET         MAMSHICH IN SW02-D01 TO TRUE

           PERFORM K-KOTAROT
           PERFORM C-KRIA.
       ZA-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       ZZ-SYIUM-TOCHNIT                    SECTION.
      *-----------------------------------------------------------------
       ZZ-00.
           MOVE "ZZ-SYIUM-TOCHNIT" TO   SHEM-SEC

           MOVE  M03-SCHUM-DAF     TO   ST01-SD
           WRITE D01-SHURA         FROM ST01
           AFTER ADVANCING 2 LINES
           ADD   M03-SCHUM-DAF     TO   M04-SCHUM-KLALI
           MOVE  M04-SCHUM-KLALI   TO   ST02-SK
           WRITE D01-SHURA         FROM ST02
           AFTER ADVANCING 2 LINES
           WRITE D01-SHURA         FROM SF02
           AFTER ADVANCING 3 LINES

           CLOSE K01-LAKOCHOT-FILE
           SET   SAGUR IN SW01-K01 TO TRUE

           CLOSE D01-DOCH-PRN
           SET   SAGUR IN SW02-D01 TO TRUE.
       ZZ-EXIT.
           EXIT.
      *-----------------------------------------------------------------
