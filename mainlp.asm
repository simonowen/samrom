;MAINLP.SAM  - SAM MAIN LOOP

;MAIN PARSER - CHECK LINE FOR SYNTAX

LINESCAN:     LD HL,FLAGS
              RES 7,(HL)       ;SIGNAL SYNTAX CHECK
              XOR A
              LD H,A
              LD L,A
              LD (IFTYPE),HL   ;LONG IF, "REF TYPE" (FN FLAG) NO
              LD (SUBPPC),A    ;FIRST STATEMENT
              LD (ERRNR),A     ;"OK" ERROR
              CALL EVALLINO    ;SKIP ANY LINE NO.
              JR NC,STMTLP1    ;JR IF LINE NO. IN RANGE

NONSENSE:     RST &08
              DB 29

;ENTRY POINT FROM LOOP, RETURN ETC, TO ELINE. C=STAT TO GOTO

LOOPEL:       LD A,C

LOOPEL2:      LD (NSPPC),A

;ENTRY POINT FOR RUNNING OR CHECKING THE E-LINE

LINERUN:      XOR A
              LD (CLA+1),A     ;ELINE SHOWN BY ODD CURRENT LINE ADDR - ALLOWS
                               ;EG RETURN TO RECOGNIZE RET ADDR AS ELINE
              LD HL,&FFFF      ;FFFF - PRINT LINE NO. WILL GIVE 0
              LD (PPC),HL
              CALL AELP        ;ADDRESS ELINE, SET PAGES
              EX DE,HL
              LD HL,(WORKSP)
              DEC HL           ;DE PTS TO ELINE START, HL TO ELINE END
              LD A,(NSPPC)
              JP NEXTLINE

;FINDER
;CALLED BY DO AND DEF PROC WITH D=INTERVENING, E=TARGET, BYTE AFTER CALL=ERROR
;IN CASE NOT FOUND. NO RETURN EVER MADE. SEARCH STARTS FROM CHAD.

SEARCH:    CALL SEARCHALL
           JP NC,&0008          ;JP IF NOT FND - USE BYTE AFTER CALL AS ERR CODE
                                ;ELSE CLA AND CHAD HAVE BEEN SET, A=STAT WHERE
                                ;TARGET FOUND (LOOP OR END PROC)

;ENTRY WITH A=STAT, CHAD SET, CLA=LINE START OR 00?? IF ELINE.
;CHAD IS PAST END PROC, LOOP, LELSE, SELSE, END IF, NEXT (VAR SKIPPED).
;LOOP MIGHT HAVE "WHILE" OR "UNTIL" TO SKIP.
;CONTINUE EXECUTION AT CHAD. EXCHAD2 USED BY FOR, LIF

           POP DE               ;JUNK RET ADDR
           LD (SUBPPC),A

EXCHAD2:   IN A,(URPORT)
           CALL STPGS        ;SET CHADP, NXTLNP, CLAPG **
           LD HL,(CLA)
           INC H
           DEC H
           JR Z,STMHOP       ;JR IF ELINE - PPC AND NXTLINE CORRECT

           LD D,(HL)
           INC HL
           LD E,(HL)            ;GET LINE NO.
           INC HL
           LD (PPC),DE          ;AND SET SYS VAR
           LD E,(HL)
           INC HL
           LD D,(HL)            ;LINE LEN
           INC HL
           ADD HL,DE
           LD (NXTLINE),HL      ;PTR TO NEXT LINE SET

STMHOP:    RST &18
           CP WHILETOK
           JR C,STMTLP2

           CP UNTILTOK+1
           CALL C,SKIPCSTAT     ;IF WHILE/UNTIL, SKIP STATEMENT

           JR STMTLP2

;THIS PART IS ALSO MAIN RUN-TIME CONTROL

STMTLP:       INC HL

STMTLP05:     LD (CHAD),HL

;FROM ELSE

STMTLP1:      CALL SETWORK
              LD HL,SUBPPC
              INC (HL)         ;INC STATEMENT NO.

;FROM ELSE (SYNTAX CHECK)

STMTLP2:      LD HL,(CHAD)

STMTLP25:     LD A,(HL)
              CP &21
              JR NC,STMTLP3    ;JR IF 21-FF

              CP &0D
              JP Z,LINEEND

              INC HL           ;SKIP 0-20 EXCEPT CR.
              LD (CHAD),HL
              JR STMTLP25

STMTLP3:      CP ":"
              JR Z,STMTLP

              LD DE,NEXTSTAT   ;CMDS RETURN TO NEXT STAT AFTER EXECUTION

ON4ENT:       PUSH DE
              LD (CSA),HL      ;CURRENT STATEMENT ADDR
              LD HL,(CMDV)
              INC H
              DEC H
              CALL NZ,HLJUMP   ;ALLOWS ADDING OF EXTRA CMDS

              LD (CURCMD),A    ;USED BY SAVE/LOAD ETC
              SUB &90
              JP C,PROCS

              CP &F7-&90
              JR NC,NONSX

              ADD A,A          ;GET WORD DISPLACEMENT
              LD C,A
              LD B,0
              LD HL,(CMDADDRT)
              ADD HL,BC        ;PT TO WORD IN TABLE OF ROUTINE START ADDRS
              LD C,LRPORT
              IN B,(C)
              SET 6,B
              OUT (C),B        ;ROM1 ON
              LD E,(HL)
              INC HL
              LD D,(HL)
              RST &20          ;SKIP TO CHAR PAST CMD
              BIT 7,D
              JR NZ,R1CMD      ;JR IF CMD IN ROM1

              RES 6,B
              OUT (C),B        ;ROM1 OFF

R1CMD:        EX DE,HL
              JP (HL)         ;EXECUTE CMD. CMDS CALL ABORTER FOR EARLY RETURN
                              ;IN SYNTAX TIME.

NEXTSTAT:     CALL BRKSTOP

NOBREAK:      CALL R1OCHP      ;ROM1 OFF, SELCHADP
              LD A,(NSPPC)
              INC A
              JR Z,STMTNEXT    ;JR IF NSPPC=FF - NO JUMP TO NEW LINE/STAT

              LD HL,(NEWPPC)   ;LINE NUMBER TO JUMP TO
              INC H
              JP Z,LINERUN     ;JR IF LINE FFXX - E LINE

              DEC H
              CALL FNDLNHL   ;HL=LINE START
              PUSH AF
              IN A,(URPORT)
              CALL STPGS
              POP AF
              LD A,(NSPPC)
              JR Z,LINEUSE     ;JR IF LINE FOUND (FLAG FROM FNDLNHL)

              AND A
              JP NZ,STATLOST   ;INSIST RETURN AND SUCH ACTUALLY GOTO DESIRED
                               ;STATEMENT - ONLY GOTO/GOSUB AVOID THIS

              LD C,(HL)        ;GET LINE NO. MSB OR STOPPER
              INC C
              JR NZ,LINEUSE    ;JR IF NOT AT PROGRAM END

OKERR:        RST &08
              DB 0             ;"OK"

STMTNEXT:     RST &18

STMTNEXT1:    CP ":"

STMTLPH:      JP Z,STMTLP

              CP THENTOK       ;NEEDED BY 'IF'
              JR Z,STMTLPH

              CP &0D
              JR Z,LINEEND

NONSX:        RST &08
              DB 29            ;NONSENSE

REMARK:       POP AF           ;JUNK NEXTSTAT
                               ;IGNORE REST OF LINE

LINEEND:      CALL ABORTER     ;RET IF SYNTAX CHECK
              JR LNEND2

;USED BY 'ON' IN RUNTIME

OLNEND:       CALL R1OCHP      ;ROM1 OFF, SELCHADP

LNEND2:       LD HL,(NXTLINE)  ;GET ADDRESS OF FOLLOWING LINE - FOLLOW ON
              LD A,(HL)
              INC A
              JR Z,OKERR       ;JR IF AT PROGRAM END. (FF=STOPPER)

              XOR A            ;STAT 0/1. LATER, 0 RATHER THAN 1 SHOWS
                               ;WE ARE DEALING WITH A GOTO (OF A NON-EXISTENT
                               ;LINE) BUT THAT"S IRREL. HERE.

LINEUSE:      CP 1
              ADC A,0          ;0 BECOMES 1, REST SAME
              BIT 6,H
              JR Z,NONEWCLAPG

              PUSH AF
              CALL INCURPAGE

;JPED TO WITH A=PAGE, (SP)=STAT, HL=LINE START

RLEPI:        CALL STPGS     ;SET CLAPG, CHADP, NXTLINEP
              POP AF

NONEWCLAPG:   LD (CLA),HL      ;CUR. LINE ADDR RECORDED FOR GOSUB RETURNS ETC.
              LD D,(HL)
              INC HL
              LD E,(HL)
              INC HL
              LD (PPC),DE      ;UPDATE PPC
              LD E,(HL)
              INC HL
              LD D,(HL)
              EX DE,HL         ;DE PTS TO LINE LEN MSB.
              INC HL           ;HL=TEXT LEN+1
              ADD HL,DE        ;HL PTS TO FIRST CHAR OF NEXT LINE
              INC DE           ;DE PTS TO FIRST CHAR IN LINE

NEXTLINE:     LD (NXTLINE),HL
              EX DE,HL         ;HL PTS TO FIRST CHAR IN LINE
              LD D,A           ;STAT NO.
              LD A,&FF
              LD (NSPPC),A     ;SIGNAL NO JUMP
              ADD A,D          ;A=STAT NO.-1
              LD (SUBPPC),A
              JP Z,STMTLP05    ;JP IF WE WANT FIRST STAT. -  LD (CHAD),HL

              CALL SKIPSTATS   ;SKIP D STATS, END WITH CHAD PTING TO :/CR/THEN
              JP Z,STMTNEXT1

STATLOST:     RST &08
              DB 31            ;"Statement doesn't exist"

BRKCR:     CALL BRKTST
           RET NZ

BRCERR:    RST &08
           DB 14             ;"BREAK - CONTINUE to repeat"

;CHECK BREAK, STOP IF IT IS PRESSED

BRKSTOP:   CALL BRKTST
           RET NZ            ;RET IF ESC NOT PRESSED

           RST &08
           DB 15             ;"BREAK into program"

;Z IF BREAK PRESSED AND BREAKDI=0

BRKTST:    LD A,&F7
           IN A,(STATPORT)
           AND &20
           RET NZ            ;RET IF ESC NOT PRESSED

           LD A,(BREAKDI)    ;NZ IN BREAKDI DISABLES BREAK
           AND A
           RET

;*******************************************************************************
;USED BY AUTO

AULL:      CALL AUL2         ;AUTO-LIST, AVOIDING EPPC->CLOSEST
           JR MAINX          ;MAIN LOOP, AFTER AUTO-LIST CALL

;MAIN CONTROLLING LOOP FOR ENTIRE INTERPRETER! ENTRY AT MAINEXEC

MAINEADD:     CALL INSERTLN
              LD A,(ERRNR)
              AND A
              JP NZ,MAINER     ;JR IF NO ROOM FOR LINE

MAINEXEC:     CALL AUTOLIST

MAINX:        CALL SETMIN

MAINELP:      CALL STRM0
              CALL EDITOR
              CALL TOKMAIN     ;TOKENIZE LINE
              CALL LINESCAN    ;CHECK FOR CORRECT SYNTAX/INSERT 5-BYTE FORMS
              LD A,(SUBPPC)
              RLA
              JR NC,MAINE1     ;JR IF 127 OR LESS STATEMENTS

              LD A,33          ;ELSE "No room for line"
              LD (ERRNR),A

MAINE1:       LD A,(ERRNR)
              AND A
              JR Z,MAINE2      ;JR IF "ERROR"=0 (OK)

              LD A,(DEVICE)
              DEC A
              JR NZ,MAINER     ;JR UNLESS DEVICE 1 (LOWER SCRN) IN USE - REPORT

              CALL ADDRELN
              CALL REMOVEFP
              CALL RSPNS       ;RASP NOISE, CANCEL ERRNR
              JR MAINELP

MAINE2:       CALL EVALLINO    ;GET LINE NUMBER IN BC, ELSE BC=0, Z SET
              JP C,NONSENSE    ;ERROR IF TOO BIG

              JR NZ,MAINEADD   ;INSERT LINE IF THERE IS A LINE NO

              RST &18
              CP &0D
              JR Z,MAINEXEC    ;JR IF LINE IS JUST <ENTER>

              LD A,(FLAGS2)
              RRA
              CALL C,CLSUP
              CALL CLSLOWER
              LD A,(UWTOP)
              LD B,A
              LD A,(SPOSNU+1)
              SUB B
              INC A            ;LINES USED IN US
              LD (SCRCT),A
              LD HL,FLAGS
              SET 7,(HL)       ;"RUNNING"
              DEC HL
              XOR A
              LD (HL),A        ;ERROR NR=0
              INC A
              LD (NSPPC),A     ;START WITH A JUMP TO STATEMENT 1
              CALL COMPILE     ;IF FLAG SAYS SO, CREATE LABELS, COMPILE DEF FNS
                               ;AND DEF PROC ADDRESSES.
              CALL LINERUN

MAINER:    CALL R1OCHP
           EI
           LD HL,SUBPPC
           LD A,(HL)
           AND A
           JR NZ,MAINER1

           LD A,(ONSTORE)    ;FETCH REAL SUBPPC IF "ON" FIDDLED WITH SYS VAR
           LD (HL),A

MAINER1:   LD HL,0
           LD (DEFADD),HL
           LD (XPTR),HL
           LD A,H
           INC HL
           LD (STREAMS+6),HL ;STREAM ZERO POINTS TO CHANNEL K
           LD (FLAGX),A
           LD (AUTOFLG),A    ;AUTO OFF
           CALL SETDISP      ;DISPLAY 0
           CALL SETMIN

           LD A,(ERRNR)
           AND &EF
           JR Z,MAINER3      ;NO "ON ERROR" IF "OK" OR 'STOP' (0/10H)

           CALL KBFLUSH      ;FLUSH KEYBOARD BUFFER **
           LD HL,FLAGS
           SET 7,(HL)        ;SET RUNNING IN CASE "VAL" TURNED IT OFF
           LD HL,ONERRFLG
           BIT 7,(HL)
           RES 7,(HL)        ;BIT 7=TEMP FLAG (OFF). BIT 0=PERM FLAG UNCHANGED
           JR Z,MAINER3      ;JR IF TEMP FLAG *WAS* OFF

           CALL ERRHAND2     ;SKIP PRINTING PART OF ERRHAND
           LD HL,MAINER
           PUSH HL           ;ERR HANDLER ADDR
           RST &30
           DW SETUPVARS      ;CREATE LINO, STAT, ERROR
           LD A,(ERRNR)
           CP 15             ;"BREAK into program"
           JR NZ,MAINER2

           CALL CONTINUE2    ;CONTINUE GETS THE CORRECT VALUES IF BREAK
                             ;INTERRUPTED A JUMP
           LD (PPC),HL       ;PPC LDED WITH OLDPPC
           DEC D
           LD A,D
           LD (SUBPPC),A     ;SAME STATEMENT FOR RETURN, NOT NEXT ONE

MAINER2:   LD HL,(ERRLN)
           CALL FNDLNHL
           JR NZ,STATLH

           INC HL
           INC HL
           INC HL
           INC HL
           LD A,(ERRSTAT)
           LD D,A
           CALL SKIPSTATS
           RST &20           ;PT TO "ON ERROR"
           CP &DD            ;ONERRORTOK

STATLH:    JP NZ,STATLOST

           RST &20           ;SKIP IT. PT TO E.G. "GOTO 10"/"GOSUB 50"/HANDLER
                             ;CLA/NXTLINE/PPC ETC MATCH LINE WITH ERROR STILL
           JP STMTLP25

MAINER3:   CALL CLSLOWER     ;SETS CHANNEL K ALSO
           LD HL,TVFLAG
           SET 5,(HL)        ;"CLEAR LOWER SCREEN ON KEYSTROKE"
           DEC HL            ;PT TO FLAGS
           RES 7,(HL)        ;"NOT RUNNING" SO FNDLN DOESN"T USE CLA DURING
                             ;EDITING
           LD A,(ERRNR)
           CALL ERRHAND1     ;PRINT REPORT, ETC.
           JP MAINELP

ERRHAND1:  CP &50
           JR NZ,EHZ

;MGT MESSAGE GIVEN IF "REPORT" 50H

           XOR A
           CALL UTMSG        ;"  MILES GORDON TECHNOLOGY PLC"
                             ;"    C 1990  SAM Coup"
           LD HL,BGFLG
           LD A,&82          ; e WITH AN ACCENT
           LD (HL),A         ;NZ=FOREIGN ON
           RST &10
           LD (HL),0
           LD A," "
           RST &10
           LD A,(PRAMTP)
           INC A             ;16 OR 32
           LD L,A
           LD H,0
           ADD HL,HL         ;32 OR 64
           ADD HL,HL         ;64 OR 128
           ADD HL,HL         ;128 OR 256
           ADD HL,HL         ;256 OR 512
           LD B,H
           LD C,L
           RST &30
           DW PRNUMB1
           LD A,"K"
           RST &10

WTFK:      CALL READKEY
           JR Z,WTFK         ;WAIT FOR A KEYPRESS

           CALL CLSLOWER
           LD A,&FF
           LD (LINICOLS),A   ;TURN OFF RAINBOW SCREEN
           JP ERRHAND2

EHZ:       JR C,EH0          ;JR IF NOT DOS ERR CODE

           SUB &51           ;RANGE NOW 0+
           LD C,A
           LD A,(DOSFLG)
           CALL SELURPG      ;DOS AT 8000H
           LD HL,(&8210)     ;DOS ERR MSGS
           LD A,C
           DB &DD            ;"JR+3"

EH0:       LD HL,(ERRMSGS)

EH15:      EX DE,HL
           RST &30
           DW POMSR          ;PRINT MESSAGE TO BUFFER (AND GET BC=LEN)
           LD A,(WINDRHS)
           SUB C             ;A=SPACE APART FROM MESSAGE, -1
           CP 13             ;ALLOW FOR 3, PLUS E.G. ", 12345:11"
           PUSH AF
           JR NC,EH1         ;JR IF OK TO PRINT ERROR MSG ON 1 LINE

           LD A,(LWTOP)
           LD (SPOSNL+1),A  ;PRINT ON TOP LINE

EH1:       PUSH BC
           LD A,(ERRNR)
           PUSH AF
           RST &30
           DW PRAREG         ;PRINT ERROR NUMBER AS 1 OR 2 DIGITS
           LD A," "
           RST &10
           POP AF
           SUB 2
           JR NZ,EH2         ;JR IF NOT "MISSING VARIABLE"
                             ;ELSE PRINT ITS NAME BEFORE " not found"
           LD B,A
           LD HL,TLBYTE
           LD A,(HL)
           AND &1F
           LD C,A            ;BC=LEN OF STR/ARRAY NAME, LEN-1 OF SIMPLE NUM
           BIT 5,(HL)        ;NZ IF NUMERIC ARRAY
           INC HL            ;PT TO FIRST LETTER
           LD D,H
           LD E,L
           ADD HL,BC         ;PT PAST END OF NAME (STR/ARRAY) OR TO END IF NUM
           INC BC            ;TRUE LEN OF SIMPLE NUM VAR NAMES, OR EXTRA ROOM
                             ;FOR "$" OR "("
           JR NZ,PMV1        ;JR IF NUMERIC ARRAY

           LD A,(FLAGS)
           BIT 6,A
           JR NZ,PMV2        ;JR IF A (SIMPLE) NUMERIC VAR

           LD (HL),"$"
           JR PMV2

PMV1:      LD (HL),"("
           INC HL
           LD (HL),")"
           INC C             ;ALLOW FOR ")" IN LEN

PMV2:      CALL PRINTSTR     ;PRINT VAR NAME

EH2:       POP BC
           LD DE,MSGBUFF
           CALL PRINTSTR     ;PRINT ERROR MESSAGE
           POP AF
           LD A,&0D
           JR C,EH3          ;JR IF USING 2 LINES

           LD A,&2c ; ","
           RST &10
           LD A," "

EH3:       RST &10
           LD BC,(PPC)
           RST &30
           DW PRNUMB1        ;PRINT LINE NUMBER
           LD A,":"
           RST &10
           LD A,(SUBPPC)
           RST &30
           DW PRAREG

ERRHAND2:  CALL CLEARSP
           LD A,(ERRNR)
           AND A
           RET Z             ;NO CONTINUE AFTER "O.K." - RET

           SUB 16            ;RESULT OF 0 IF "STOP statement", FF IF "BREAK
                             ;into program", AND CY
           LD B,0
           ADC A,B
           JR NZ,ERRHAND3    ;NO INC OF SUBPPC IF NEITHER STOP OR BREAK

           LD A,(CURCMD)
           CP 193            ;NEXT
           JR Z,ERRHAND3     ;B=0 NOW - CONTINUE AFTER BREAK INTO 'NEXT'
                             ;REPEATS STATMENT
           INC B

ERRHAND3:  LD HL,NSPPC
           LD A,(HL)
           LD (HL),&FF       ;CANCEL ANY JUMP
           LD HL,(NEWPPC)
           BIT 7,A
           JR Z,ERRHAND4     ;JR IF JUMP WAS ABOUT TO HAPPEN

           LD A,(SUBPPC)
           ADD A,B           ;INCR. SUBPPC BY 1 IF STOP OR BREAK
           LD HL,(PPC)

ERRHAND4:  INC H
           RET Z             ;RET IF CONT WOULD HAVE BEEN TO E-LINE

           DEC H
           LD (OLDPPC),HL
           LD (OSPPC),A      ;COPY NSPPC/NEW PPC OR SUBPPC/PPC TO OSPPC/OLDPPC
           RET


DFKNL:     EX (SP),HL        ;JUNK NEXT STAT RETURN, STACK PTR TO REST OF LINE
           CALL STMTNEXT     ;CHECK SYNTAX FOR REST OF LINE
           POP HL            ;REMOVE FLOATING POINT FROM REST OF LINE, RET TO
                             ;MAIN LOOP

;REMOVE INVISIBLE 5-BYTE FORMS FROM (HL) TO 0DH

REMOVEFP:     LD C,6
              LD A,(HL)
              SUB &0E
              LD B,A
              CALL Z,RECLAIM2
              LD A,(HL)
              INC HL
              CP &0D
              JR NZ,REMOVEFP

              RET


;GET LINE NO AT START OF ELINE TO BC, SET Z IF LN=0. CY IF TOO BIG.

EVALLINO:  CALL AELP      ;ADDR ELINE, SET CHADP ETC
           LD (CHAD),HL
           RST &30
           DW SMBW
           RST &18
           CALL INTTOFP
           CALL FPTOBC
           RET C             ;RET IF >64K

           LD A,B
           ADD A,1
           RET C             ;RET IF >65279

           LD A,B
           OR C
           RET               ;Z IF BC=0

AELP:      CALL ADDRELN

STPGS:     AND &1F
           LD (CLAPG),A
           LD (CHADP),A
           LD (NXTLINEP),A
           RET

;*******************************************************************************
;INSERT LINE - INSERT LINE BC FROM ELINE INTO PROGRAM

INSERTLN:     PUSH BC          ;LINE NUMBER
          ;   LD HL,(INSLV)
          ;   INC H
          ;   DEC H
          ;   CALL NZ,HLJUMP

              CALL SCOMP       ;DEF PROCS/DEF FNS AND LABELS NEED DOING -
                               ;ANY PRE-PASS OF LABELS ETC IS OBSOLETE.
              LD HL,(WORKSP)
              LD BC,(CHAD)     ;BC PTS AFTER LINE NUMBER
              LD A,(BC)
              CP " "
              JR NZ,INSLN3

              INC BC
              LD A,(BC)
              CP &0D
              JR NZ,INSLN2     ;JR, LEAVING BC INCED, TO INC CHAD AND DELETE
                               ;FIRST SPACE IN A LINE LIKE: 10 test. PREVENTS
                               ;SPACES ACCUMULATING WITH MULTI-EDIT/ENTERS.
              DEC BC           ;AVOID ANY ACTION IF E.G. 10 (space) CR

INSLN2:       LD (CHAD),BC

INSLN3:       SCF
              SBC HL,BC        ;HL=LEN OF TEXT, INCLUDING 0DH
              LD A,H
              CP &3F
              JP NC,OOMERR     ;LIMIT LINE LEN TO 3EFFH

              EX (SP),HL       ;STACK LEN, GET HL=LINE NO
              LD (EPPC),HL
              CALL FNORECL     ;FIND/RECLAIM LINE
              POP BC           ;TEXT LEN
              LD A,C
              DEC A
              OR B
              RET Z            ;RET IF TEXT IS JUST 0DH (LENGTH OF 1)

              PUSH BC          ;TEXT LEN
              INC BC
              INC BC
              INC BC
              INC BC           ;GET LEN INCLUDING LN AND LEN BYTES
          ;   PUSH BC
          ;   PUSH HL
          ;   CALL GAPSZ
          ;   LD (4020H),HL  ;!!
          ;   POP HL
          ;   POP BC

              CALL MAKEROOM    ;BC BYTES AT (HL)
              LD BC,(EPPC)
              LD (HL),B
              INC HL
              LD (HL),C        ;ENTER LINE NUMBER
              INC HL
              POP BC           ;TEXT LEN
              LD (HL),C
              INC HL
              LD (HL),B
              CALL SPLITBC
              INC HL
              EX DE,HL
              IN A,(URPORT)
              LD C,A           ;CDE=POINTS TO ROOM FOR TEXT
              CALL ADDRCHAD    ;TEXT START IN AHL
              JP FARLDIR

;SKIPCSTAT - SKIP CURRENT STATEMENT E.G. DATA, LABEL, DEF FN, LOOP UNTIL ETC.
;ENTRY: CHAD=POSN

SKIPCSTAT: RST &18
           LD DE,&0100       ;1 STAT, NOT IN QUOTES
           JR SKIPS15

;SKIPSTATS - FIND D"TH STATMENT FROM POSN.
;ENTRY: D=STATMENTS TO SKIP,+1. HL=POSN
;EXIT: CHAD PTS TO BEFORE REQUIRED STAT - TO ":" OR "THEN" OR CR.
;Z,NC IF OK, Z, CY IF HIT END OF LINE AS STAT COUNTER REACHED ZERO, NZ, CY IF
;HIT END OF LINE BEFORE STAT COUNT REACHED ZERO

SKIPSTATS: DEC HL            ;COMP FOR INITIAL INC SO WE DON"T MISS SHORT STATS

;FROM "ON"

SKIPS0:    XOR A             ;"NOT IN QUOTES", NC
           LD E,A
           JR SKIPS5

SKIPS1:    INC HL

SKIPS15:   LD A,(HL)
           CP &0E
           CALL Z,NUMBER

           CP &22
           JR NZ,SKIPS2

           DEC E                ;"INSIDE STRING"

SKIPS2:    CP ":"
           JR Z,SKIPS4

           CP THENTOK
           JR Z,SKIPS4

           CP &0D
           JR NZ,SKIPS1

           DEC D                ;Z IF LINE END=DESIRED STAT.
           SCF                  ;"HIT LINE END"
           JR SKIPS6

SKIPS4:    BIT 0,E
           JR NZ,SKIPS1         ;IF ":"/"THEN"INSIDE STRING, JR, GET NEXT CHAR

SKIPS5:    DEC D                ;DEC "STATS TO SKIP" COUNTER
           JR NZ,SKIPS1

SKIPS6:    LD (CHAD),HL         ;PT CHAD TO JUST BEFORE DESIRED STATMENT
           RET

DATA:      CALL RUNFLG
           JR C,SKIPCSTAT    ;SKIP STAT IF RUNNING

DATA1:     CALL SCANSR
           CP ","
           RET NZ            ;RET IF END OF STAT

           RST &20
           JR DATA1
