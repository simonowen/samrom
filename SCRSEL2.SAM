;SCRSEL2.SAM.

GOSUB:     LD HL,GOSUB2
           DB &DD            ;"JR+3"

GOTO:      LD HL,GOTO2

GSUBTOC:   PUSH HL
           CP &DE            ;ONTOK
           JR Z,GOTON

           CALL EXPT1NUM     ;LINE NUMBER
           POP HL            ;ADDR OF GOTO OR GOSUB ROUTINE
           CALL CHKEND

           JP (HL)

;E.G. GOSUB (OR GOTO) ON X;100,234,400

GOTON:     CALL SEXPT1NUM    ;SKIP 'ON', EVAL NUMBER
           PUSH AF
           CP ";"
           JP NZ,NONSENSE

           POP AF
           JR C,GOTON2       ;JR IF RUNNING

           POP HL            ;GOTO/GOSUB ADDR

GOTONSL:   CALL SEXPT1NUM    ;SKIP E.G. 100,200,400
           CP ","
           JR Z,GOTONSL

           RET               ;TO NEXT STAT

GOTON2:    CALL FPTOA        ;GET NUMBER AFTER 'ON' IN C AND A
                             ;CY IF >255
           SBC A,A           ;A=255 IF OUT OF RANGE, ELSE 0
           JR Z,GOTONRL      ;C IS OK IF A=0

           LD C,A            ;ELSE LET C=255

GOTONRL:   PUSH BC           ;C=EXPR NUMBER
           CALL SEXPT1NUM
           POP BC
           DEC C
           RET Z             ;RET TO GOTO OR GOSUB IF THIS IS DESIRED EXPR

           CALL FDELETE      ;DISCARD EXPR FROM FPCS
           RST &18
           CP ","
           JR Z,GOTONRL      ;LOOP IF THERE ARE MORE LINE NOS TO SKIP

           POP DE            ;GOTO/GOSUB ADDR
           RET               ;TO NEXT STAT - LINE NOS RAN OUT.

GOSUB2:    CALL GETINT
           LD A,H
           INC A
           JP Z,IOORERR

           LD B,&00          ;'GOSUB' TYPE
           PUSH HL           ;LINE NO.
           CALL BSTKE        ;STACK RETURN ADDR
           POP HL
           JP GOTO3

;GET TOKEN SR - VIA JUMP TABLE

GETTOKEN:  LD C,A
           EX AF,AF'         ;NO OF WORDS TO CHECK,+1

GTTOK1:    INC HL

GTTOK2:    BIT 7,(HL)
           JR Z,GTTOK1       ;LOOP TILL WE FIND THE END OF A KEYWORD
                             ;32 TS PER LOOP

           DEC C
           RET Z             ;RET IF WE HAVE CHECKED THEM ALL - Z=FAILED
                             ;DE=FIRST LETTER OF ELINE WORD COPY
           INC HL
           LD A,(DE)
           XOR (HL)
           AND &DF
           JR NZ,GTTOK2      ;LOOP BACK AND CHECK ANOTHER WORD IF MATCH FAILS
                             ;ABOUT 80 TS PER LOOP IF NO MATCH. ABOUT 35USEC
                             ;PER 5-LETTER WORD OR 5.3MSEC FOR 150 WORDS.
           PUSH DE           ;PTR TO FIRST LETTER IN ELINE WORD
           INC DE

GTTOK3:    INC HL
           LD A,(HL)
           CP " "
           LD A,(DE)
           INC DE
           JR NZ,GTTOK4      ;JR IF NO EMBEDDED SPACE IN KEYWORD IN LIST

           CP (HL)
           JR Z,GTTOK3       ;ELSE ACCEPT ONE IN INPUT

           INC HL            ;BUT DON'T INSIST - SKIP LIST PTR IF NO SP IN INPUT

GTTOK4:    XOR (HL)
           AND &DF
           JR Z,GTTOK3       ;LOOP IF STILL MATCHING OK

           AND &7F
           JR NZ,GTTOK5      ;JR IF INPUT AND LIST WORDS DON'T END NOW

           LD A,(HL)
           RLCA
           JR C,GTTOK6       ;OK IF LIST WORD HAS FINISHED

GTTOK5:    POP DE
           JR GTTOK2

GTTOK6:    CP &7E            ;('=' TERMINATOR =7B AFTER RLCA, '>'=7DH, '$'=49H)
           CCF               ;NC IF '=' OR '>' OR '$'
           LD A,(DE)         ;INPUT CHAR AFTER MATCHED WORD
           CALL C,ALDU       ;CALL TO INSIST ON NON-LETTER AFTER E.G. PRINTx
                             ;BUT NOT AFTER <>,>= OR <= OR CHR$a
           JR C,GTTOK5       ;('=' AND '>' NEVER CALL ALPHA OR JR HERE)

           POP HL            ;FIRST LETTER OF ELINE WORD COPY
           EX AF,AF'
           SUB C             ;A=1 IF FIRST WORD MATCHES, ETC.
           RET


MODPT2:    AND &03
           LD HL,MODE
           LD C,(HL)
           PUSH BC           ;SAVE PREVIOUS MODE
           LD (HL),A         ;NEW MODE
           CP 2
           PUSH AF
           CALL NC,SUET      ;SET UP EXPANSION TABLE IF NEEDED

           POP AF
           PUSH AF           ;MODE
           RRCA
           RRCA
           RRCA              ;MODE BITS TO BITS 6 AND 5
           LD C,A
           LD HL,CUSCRNP
           LD A,(HL)         ;KEEP CURRENT VID PAGE,
           XOR C             ;BUT
           AND &9F           ;USE BITS 6 AND 5 OF C TO SET MODE
           XOR C
           LD (HL),A
           IN A,(VIDPORT)
           XOR (HL)
           AND &1F
           JR NZ,MDL2        ;JR IF WE ARE NOT DISPLAYING THIS SCREEN - AVOID
                             ;ALTERING HARDWARE MODE
           LD A,(HL)
           OUT (VIDPORT),A

MDL2:      POP AF            ;NEW MODE
           POP BC            ;C=PREV MODE
           PUSH AF           ;NEW MODE
           CP 1              ;CY IF MODE 0
           SBC A,A           ;MODE 0=FF, REST=0
           LD HL,&0809       ;CHAR WIDTH/HEIGHT FOR MODES 1-3
           ADD A,L
           LD L,A            ;DEC HEIGHT TO 8 IF MODE 0, TO FIT ATTR
           POP AF

           CALL MDSR

           LD A,(TEMPB1)     ;OLD MODE
           JR Z,M2ST         ;JR IF MODE 2 BEING SET

           CP 2
           JR NZ,ECLS        ;JR IF NOT MODE 2 NOW, AND NOT CHANGING TO MODE 2

           ;ELSE IF CHANGING FROM MODE 2 TO ANOTHER MODE, SAVE M2 PALTAB
           ; 0-3, GET BACK NON-MODE-2 COLOURS

           LD HL,(M3PAPP)
           LD (M23PAPP),HL
           LD HL,(M3LSC)
           LD (M23LSC),HL
           LD C,A            ;C=NZ - 'HALVE'
           JR M2ST2

;MODE 2 BEING SET

M2ST:      CP 2
           JR Z,ECLS         ;JR IF MODE 2 ALREADY

           LD HL,(M23PAPP)
           LD (M3PAPP),HL
           CALL M3TO2        ;CONVERT L
           PUSH AF
           LD L,H
           CALL M3TO2        ;CONVERT H
           LD H,L
           POP AF
           LD L,A
           LD (M23PAPP),HL   ;PREVENTS STRIPED INKS AFTER MODE 2 SET.
           LD HL,(M23LSC)
           LD (M3LSC),HL
           CALL CVLSP        ;CONVERT LS PAPER
           LD C,0            ;C='HALVE'

M2ST2:     LD A,(THFATP)     ;WE ARE CHANGING TO/FROM MODE 2 -  IF
           AND A             ;'THIN' PIXEL STATUS WILL CHANGE,
           LD A,C
           CALL Z,SETFP      ;FIDDLE XCOORD/XRG

           CALL PALSW

ECLS:      CALL R1OFFJP
           DW MCLS           ;TURN ROM 1 OFF, JP MCLS

;CALLED FROM CSIZE WITH A AND C=MODE, HL=CSIZE

MDSR:      PUSH AF           ;NEW MODE
           LD A,C            ;PREV MODE (SAME IF CALLED FROM CSIZE)
           LD (TEMPB1),A
           LD (CSIZE),HL     ;H=WIDTH, L=HEIGHT
           LD A,192
           LD E,&FC

MDSL:      INC E
           SUB L             ;SUB CHAR HEIGHT
           JR NC,MDSL        ;CALC SCREEN HEIGHT IN LINES, MINUS 3
                             ;(UW BOT)
           ADD A,L           ;'LEFT OVER' PIXELS
           LD (LSOFF),A      ;LOWER SCREEN OFFSET - SO LEFT-OVER PIX BETWEEN
                             ;UPPER SCREEN AND LOWER SCREEN
           LD A,L
           ADD A,A           ;DISP OF GRAPHICS ORIGIN FROM SCREEN BOTTOM IS
           LD (ORGOFF),A     ;2 CHAR HEIGHTS
           LD D,31           ;RHS FOR MODES 0, 1 AND 3
           POP AF            ;MODE
           PUSH AF
           JR NZ,MDSR3       ;JR IF NOT MODE 2

           LD D,63           ;RHS FOR MODE 2 64-COL MODE
           LD A,(FL6OR8)
           AND A
           JR NZ,MDSR3       ;JR IF 8-BIT

           LD D,84           ;RHS FOR MODE 2 85-COL MODE
           LD A,6
           LD (CSIZE+1),A    ;WIDTH

MDSR3:     LD HL,UWRHS
           LD (HL),D         ;UWRHS
           INC HL
           XOR A
           LD (HL),A         ;UWLHS
           INC HL
           LD (HL),A         ;UWTOP
           INC HL
           LD (HL),E         ;UWBOT
           INC HL
           LD (HL),D         ;LWRHS
           LD (WINDMAX),DE
           INC HL
           LD (HL),A         ;LWLHS
           INC HL
           INC E
           LD (HL),E         ;LWTOP=UWBOT+1
           INC HL
           INC E
           LD (HL),E         ;LWBOT=LWTOP+1
           POP AF            ;Z IF MODE 2
           RET

;FATPIX 0=USE THIN PIXELS IN MODE 2. 1=USE FAT PIXELS

FATPIX:    CALL SYNTAX6

           LD DE,&0200+30    ;LIMIT TO <2 OR IOOR
           CALL LIMBYTE

           LD HL,THFATP
           CP (HL)
           RET Z             ;RET IF NO CHANGE IN FATPIX STATUS

           LD (HL),A
           LD A,(MODE)
           CP 2
           RET NZ            ;DON'T FIDDLE XCOORD AND XRG UNLESS MODE 2

           LD A,(HL)

SETFP:     LD HL,(XCOORD)
           AND A
           JR Z,FPX2         ;JR IF CHANGED FROM 1 TO 0 (FAT TO THIN)

           SRL H
           RR L              ;HALVE X
           DB &3E            ;'JR+1'

FPX2:      ADD HL,HL         ;DOUBLE X COORD TO KEEP SAME SCREEN POSN

           LD (XCOORD),HL    ;A=0 FOR DOUBLE, 29H FOR HALVE

           PUSH AF
           CALL ADDRNV
           POP AF
           LD DE,87
           ADD HL,DE         ;PT TO XRG
           CALL R1OFFJP
           DW CGXRG          ;CHANGE XRG WITH ROM1 OFF **

PXIOOR:    RST &08
           DB 30             ;IOOR

;CSIZE W,H
;WIDTH CAN BE 6 OR 8 BUT 6 ONLY EFFECTIVE IN MODE 2
;HEIGHT CAN BE 6-32, DOUBLE HEIGHT USED IF 16 OR MORE.

WIDTH:     CALL SYNTAX8

           CALL GETBYTE      ;H
           PUSH BC
           CALL GETBYTE      ;W
           CP 6
           JR Z,CSZ2

           CP 8
           JR NZ,PXIOOR

CSZ2:      POP DE            ;E=H
           LD D,A            ;D=W
           LD A,E
           CP 6
           JR C,PXIOOR       ;HEIGHTS OF 0-5 ARE CRAZY - BUT 6-7 MIGHT BE OK
                             ;WITH THE RIGHT CHARACTER SET.
           CP 33
           JR NC,PXIOOR      ;HEIGHTS OF 6-32 ARE ALLOWED

           EX DE,HL
           LD A,H            ;WIDTH
           SUB 6
           LD (FL6OR8),A     ;0 IF WIDTH 6, ELSE NZ
           LD A,(MODE)
           LD C,A            ;'PREVIOUS MODE' NEEDED BY MDST2
           CP 2              ;SET Z IF MODE 2
           PUSH AF
           CALL MDSR         ;MODE - SET WIDOWS, CSIZE. HL=CSIZE
           POP AF
           RET NZ            ;RET IF NOT MODE 2

                             ;ELSE SET UP EXPANSION TABLE IF MODE 2, IN CASE
                             ;SWITCHING BETWEEN M2/M3 O/P TYPES

;SET UP EXPANSION TABLE. ENTRY: A=MODE 2/3, Z IF 2

SUET:      LD HL,EXTAB       ;PT TO EXPANSION TABLE
           LD C,0            ;FIRST VALUE TO EXPAND
           JR Z,DBTABCLP     ;JR IF MODE 2

QUADTCLP:  LD A,C
           CALL QUADBITS     ;A->DE, QUADED BITS
           LD (HL),D
           INC HL
           LD (HL),E
           INC HL
           INC C
           LD A,C
           CP 16
           JR C,QUADTCLP     ;QUAD 00-0FH

           RET

DBTABCLP:  LD A,C
           CALL DBBITS       ;A->E, DOUBLED BITS
           LD (HL),E
           INC HL
           INC C
           LD A,C
           CP 16
           JR C,DBTABCLP     ;DOUBLE 00-0FH

           RET

;A->DE, QUADED BITS

QUADBITS:  CALL DBBITS
           LD A,E

;A->DE WITH ALL BITS DOUBLED

DBBITS:    LD B,8            ;BITS TO DO

DBBITSLP:  RRCA
           RR D
           RR E              ;DE HAS DOUBLED BITS
           RLCA
           RRCA
           RR D
           RR E
           DJNZ DBBITSLP

           RET

;AUTO <LINE><,STEP>

AUTO:      CALL CRCOLON
           JR Z,AO1          ;JR IF JUST 'AUTO', USE BOTH DEFAULTS

           CALL GIR2         ;GET LINE VALUE IN HL AND BC
           LD BC,10          ;DEFAULT STEP IS 10
           CP ","
           JR NZ,AO2         ;JR IF NO STEP

           PUSH HL           ;SAVE LINE VALUE
           CALL GIR          ;GET STEP VALUE IN HL,BC
           POP HL
           JR AO2

AO1:       LD HL,(EPPC)
           LD BC,10
           ADD HL,BC         ;DEFAULT LINE VALUE IS EPPC+10

AO2:       CALL RUNFLG
           RET NC            ;CANNOT USE CHKEND - NEED C

           XOR A
           SBC HL,BC
           CCF
           ADC A,A           ;A=O IF STEP>LINE, ELSE A=1
           LD (AUTOFLG),A    ;0=OFF, NZ=ON
           RET Z             ;RET IF OFF

           LD (EPPC),HL      ;INITIAL EPPC=LINE-STEP
           LD (AUTOSTEP),BC
           POP DE            ;NEXT STAT
           POP DE            ;ERROR HANDLER
           LD BC,AULL
           JP R1XJP          ;TURN ROM1 OFF, JP MAINX

SOUND:     LD DE,INSTBUF

SNDLP:     PUSH DE
           CALL EXPT2NUMS
           POP DE
           JR NC,SND1

           PUSH DE
           CALL GETBYTE
           PUSH AF
           LD DE,&2000+30
           CALL LIMBYTE      ;ALLOW 0-31
           POP BC
           POP DE
           LD (DE),A
           INC E
           LD A,B
           LD (DE),A
           INC E
           JP Z,NRFLERR      ;LIMIT TO 254 VALUES IN INSTBUF (127 PAIRS)

SND1:      RST &18
           CP ";"
           JR NZ,SND2

           RST &20           ;SKIP ';'
           JR SNDLP

SND2:      CALL CHKEND

           LD A,&FF
           LD (DE),A         ;TERMINATE LIST
           LD BC,256+SNDPORT ;SOUND ADDRESS REG PORT
           LD HL,INSTBUF     ;HOLDS AT LEAST ONE PAIR
           JR SND3

SNDOPL:    OUT (C),D         ;SET REG
           DEC B             ;SOUND DATA REG PORT
           OUT (C),E         ;SEND DATA
           INC B

SND3:      LD D,(HL)
           INC L
           LD E,(HL)
           INC L
           CP D
           JR NZ,SNDOPL      ;LOOP UNTIL TERMINATOR HIT

           RET


;PERFORM BOOT ACTION
;FIND A PAGE FOR DOS AND MARK IT.

BOOT:      CALL SYNTAX3

           CALL GETBYTE
           AND A
           JR NZ,BOOTEX      ;E.G. BOOT 1 FORCES BOOT OR RE-BOOT
                             ;NO AUTO-LOAD
           LD A,(DOSFLG)
           AND A
           JR Z,BOOTNR       ;JR IF DOS NOT RESIDENT - ELSE AUTOLOAD ONLY

           RST &08
           DB ALHK           ; DO AUTO-LOAD
           RET

BOOTNR:    CALL BOOTEX

           RST &08
           DB BTHK           ;DO AUTO-LOAD, BUT NO ERROR IF NONE
           RET

BOOTEX:    LD HL,ALLOCT+&1F

FDPL:      LD A,(HL)
           AND A
           JR Z,GDP          ;JR IF FREE PAGE

           CP &60
           JR Z,GDP          ;JR IF RE-BOOTING TO A DOS PAGE

           DEC L
           JR NZ,FDPL

           RST &08
           DB 1              ;'OUT OF MEMORY' IF NO FREE PAGE FOR DOS

GDP:       LD A,L
           CALL SELURPG      ;DOS PAGE IN AT 8000H

                             ;START OF BRUCE GORDONS CODE
                             ;LOAD STUFF NOW
           LD C,&D0          ;RESET CHIP
           CALL SDCX         ;EXITS WITH B=0
           CALL REST

           ;TEST FOR INDEX HOLE

           LD H,&FE          ;HL=FEXX
           LD E,H            ;E COUNTS 2 OUTER LOOPS
           LD B,6

BOOT2:     DEC HL
           LD A,H
           OR L
           JR NZ,BOOT3

           INC E
           JR NZ,BOOT3

           RST &08           ;'Missing disc'
           DB 55

BOOT3:     IN A,(COMM)
           LD D,A
           XOR C
           AND 2
           JR Z,BOOT2        ;LOOP UNTIL HOLE SIGNAL CHANGES

           LD C,D
           DJNZ BOOT2

           CALL REST
           LD DE,&0401       ;TRACK/SECTOR

;READ SECTOR AT TRACK D, SECTOR E

RSAD:      XOR A
           EX AF,AF'

RSA1:      LD A,E
           OUT (SECT),A

RSA2:      CALL BUSY
           IN A,(TRCK)
           CP D
           JR Z,RSA4

           LD C,STPOUT
           JR NC,RSA3

           LD C,STPIN

RSA3:      CALL SADC
           JR RSA2

RSA4:      DI
           LD C,DRSEC
           CALL SADC
           LD HL,&8000
           LD BC,DTRQ
           DB &FE            ;"JR+2" (CP EDH: AND D)

RSA5:      INI

RSA6:      IN A,(COMM)
           BIT 1,A
           JR NZ,RSA5

           RRCA
           JR C,RSA6

           EI

;CHECK DISC ERROR COUNT

           AND &0E
           JR Z,BTNOE        ;JR IF NO ERRORS

           EX AF,AF'
           INC A
           CP 5
           PUSH AF
           CALL Z,REST
           POP AF
           CP 10
           JP NC,TERROR

           EX AF,AF'
           JR RSA1

BTNOE:     LD DE,&80FF
           LD HL,BTWD
           LD B,4

BTCK:      INC DE
           LD A,(DE)
           XOR (HL)
           AND &5F           ;IGNORE MISMATCH ON BITS 7 OR 5
           JR Z,BTLY

           RST &08
           DB 53             ;'NO DOS' IF NOT 'BOOT'

BTLY:      INC HL
           DJNZ BTCK

           JP &8009          ;DOS INITIALISE

SADC:      CALL BUSY

SDCX:      LD A,C
           OUT (COMM),A
           LD B,0

SDC1:      DJNZ SDC1

           RET

;RESTORE DRIVE TO ZERO

REST:      LD C,DRES
           CALL SADC

;TEST FOR CHIP BUSY

BUSY:      IN A,(COMM)
           RRCA
           RET NC

           CALL BRKCR
           JR BUSY
