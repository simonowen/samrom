;MISCX1.SAM - START OF UPPER ROM. FIRST PART IS SECTIONS THAT ARE COPIED
;TO A RAM BUFFER FOR EXECUTION SO THE UPPER ROM CAN BE PAGED OUT AND
;THE BASIC PROGRAM GOT AT.

           ORG INSTBUF

;MOVED TO INSTBUF FOR EXECUTION

RNMP2:     LD HL,10
           LD   (RLINE),HL
           LD   (RSTEP),HL
           CALL BRKLSSL      ;ASSESS <N> TO <M>
           RST  &18
           LD   HL,RLINE
           CP   LINETOK
           CALL Z,REVAL
           LD   HL,RSTEP
           CP   STEPTOK
           CALL Z,REVAL
           CALL CHKEND

           LD B,24
           CALL TESTROOM     ;ABORT IF <6K FREE
           CALL SPSS

MAKETABLE: LD   HL,(RLINE)
           LD   BC,(RSTEP)
           LD   DE,SBN+2

           EXX
           LD   HL,SBO
           PUSH HL
           LD   DE,&0000

MKTBLP:    POP  HL           ;SBO
           LD   (HL),D
           INC  HL
           LD   (HL),E
           DEC  HL
           PUSH HL           ;SBO
           LD   HL,(LAST)    ;FEFF IF END OF PROG WANTED
           SBC  HL,DE
           JR C,RENUM3       ;JR IF LINE NUMBER JUST PLACED IS PAST BLOCK END
                             ;(COULD BE FF?? PROG TERMINATOR)
           CALL GTRLNN
           SCF
           SBC  HL,DE
           JR   NC,MKTBLP

           POP  HL
           INC  HL
           INC  HL
           PUSH HL

           EXX
           EX   DE,HL
           LD   (HL),D
           INC  HL
           LD   (HL),E
           INC  HL
           INC H
           JR Z,NRFLERR

           DEC H
           EX   DE,HL
           ADD  HL,BC        ;ADD LINE,STEP TO GET NEXT NEW LINE NO.
           JR C,NRFLERR

           LD A,H
           EXX

           INC  A
           JR   NZ,MKTBLP    ;LOOP UNLESS LINE NUMBERS TOO HIGH (>FEFF)

NRFLERR:   RST &08
           DB 33             ;'No room for line'

RENUM3:    POP BC
           LD HL,(SBO)
           LD (TEMPW1),HL    ;SAVE IT SO IT CAN BE READ WHEN SCREEN SWITCHED OUT
           CALL RCURP
           LD HL,PPC
           CALL TRANSHL
           PUSH BC
           LD A,(SUBPPC)
           PUSH AF

           LD A,D
           INC A
           JR NZ,REN3        ;JR IF DE IS A REAL LINE NUMBER

           LD E,A            ;ELSE DE=FF00, MAX LINE+1

REN3:      XOR A             ;NC
           PUSH DE
           EXX
           POP  DE
           SBC  HL,BC
           SBC  HL,BC
           SBC  HL,DE
           JR   NC,NRFLERR   ;ERROR IF MOVING LINES ONTO OTHERS

           LD   HL,(TEMPW1)
           LD   D,L
           LD   E,H
           LD   HL,(RLINE)
           SBC  HL,DE
           JR C,NRFLERR

           CALL CHGREF

;CHANGE LINE NUMBERS

           LD   HL,(FIRST)
           CALL FNDLINE

CHGLL:     LD   B,(HL)
           INC  HL
           LD   C,(HL)
           PUSH HL
           CALL TRANSFORM
           POP  HL
           JR C,REN5         ;JR IF FINISHED BLOCK

           LD   (HL),C
           DEC  HL
           LD   (HL),B       ;ALTER LINE NUMBER
           INC  HL
           INC  HL
           LD   C,(HL)
           INC  HL
           LD   B,(HL)       ;BC=LINE LEN
           INC  HL
           ADD  HL,BC        ;PT TO NEXT LINE
           CALL CHKHL
           JR   CHGLL

REN5:      LD HL,EPPC
           CALL TRANSHL
           LD L,SDTOP\256
           CALL TRANSHL
           CALL MCLS         ;CLEAR ENTIRE SCREEN
           JP GT4P           ;UNSTACK STAT, PPC, SET UP FOR GOTO NEXT, COMP

;CHANGE REFERENCES TO LINE NUMBERS

FAILED:    CALL FDELETE

NBLKL:     POP  BC
           POP  BC
           POP  BC
           JP CHGR7

CHGREF:    LD   IX,RENTAB-1

RENCL:     INC  IX
           CALL ADDRPROG
           LD A,(HL)
           INC A
           RET Z

           LD   (CLA),HL
           INC  HL
           INC  HL
           INC  HL
           INC  HL
           LD   (CHAD),HL

RLOOP:     LD   A,(IX+0)
           AND  A
           RET  Z

           LD   E,A
           CALL SRCHPROG
           JR   NC,RENCL

           LD   (SUBPPC),A
           IN   A,(251)
           LD   (CHADP),A
           LD A,(IX+0)
           CP LINETOK
           JR NZ,CHGRY

           DEC HL
           DEC HL
           LD A,(HL)         ;PT TO BYTE BEFORE "LINE"
           CP "$"
           JR Z,CHGRY        ;RENUM E.G. SAVE ASD$ LINE 10

           CP &22
           JR NZ,RLOOP       ;NO RENUM UNLESS E.G. SAVE "NAME" LINE 10

CHGRY:     RST  &18

CHGR1:     CP   TOTOK
           JR   NZ,CHGR2

           RST  &20

CHGR2:     PUSH HL
           LD   B,&FF
           CP   &DE          ;ONTOK
           JR   NZ,CHGR5

CHGR3:     RST  &20
           CP   ";"
           JR   NZ,CHGR3

           RST  &20
           POP  DE
           PUSH HL
           DB &11            ;'JR+2'

CHGR4:     INC HL
           LD A,(HL)

CHGR5:     CALL NUMERIC
           INC  B
           JR   C,CHGR4

           PUSH BC

CHGR6:     LD A,(HL)
           INC HL
           CP " "
           JR Z,CHGR6

           PUSH HL
           CP   &0E
           JR   NZ,NBLKL

           CALL HLTOFPCS
           LD   (CHAD),HL
           CALL GETINT
           RST  &18
           CALL COCRCOTO     ;CHECK FOR CR/:/TO/,
           JR NZ,FAILED

           CALL TRANSFORM
           JR   C,NBLKL

           CALL STACKBC      ;EXITS WITH DE=STKEND
           LD HL,-5
           ADD HL,DE         ;POINT TO TOP ITEM ON FPCS
           POP  DE
           CALL LDI5         ;COPY TO 5-BYTE BUFFER IN LINE
           CALL JPFSTRS      ;BC=NEW LEN OF NUMBER (DIGITS)
           POP  AF           ;ORIG NUMBER OF DIGITS
           POP  DE
           LD   HL,(CLA)

;ADJUST LINE LEN
;ENTRY: HL PTS TO START OF BASIC LINE, DE PTS TO SPACE TO OPEN OR CLOSE IN THE
;LINE, BC=NEW LENGTH OF SPACE, A=OLD LENGTH.

ADJLINE:   SUB C
           JR Z,ADLNO        ;JR IF LENS THE SAME

           INC  HL
           INC  HL
           PUSH BC
           PUSH AF
           LD   C,(HL)
           INC  HL
           LD   B,(HL)       ;CURRENT LINE LEN
           PUSH HL
           NEG
           LD   L,A
           RLA
           SBC  A,A
           LD   H,A
           ADD  HL,BC
           LD   B,H
           LD   C,L
           POP  HL
           LD   (HL),B
           DEC  HL
           LD   (HL),C       ;NEW LINE LEN
           POP  AF
           LD   B,0
           JR   NC,ADJL2     ;JR IF CLOSEING UP SPACE

           NEG
           LD   C,A
           EX   DE,HL
           CALL MAKEROOM
           JR   ADJL3

ADJL2:     LD   C,A
           EX DE,HL
           CALL RECLAIM2

ADJL3:     EX DE,HL
           POP BC

ADLNO:     LD HL,PRNBUFF
           LDIR
           EX DE,HL
           LD C,6
           ADD HL,BC
           LD (CHAD),HL

CHGR7:     CALL RCRC         ;RST 18, CRCOLON
           JP Z,RLOOP

           CALL COCRCOTO     ;Z IF TO/,
           JP NZ,RLOOP

           RST &20
           JP CHGR1

RENTAB:    DB &CD            ;DELETETOK
           DB &DD            ;ONERRORTOK
           DB &8C            ;LINETOK
           DB &BE            ;LLISTTOK
           DB &BD            ;LISTTOK
           DB &BA            ;RESTORETOK
           DB &B4            ;GOTOTOK
           DB &B5            ;GOSUBTOK
           DB &B0            ;RUNTOK
           DB 0

REVAL:     PUSH HL
           CALL GIR
           POP HL
           RET C

           LD A,B
           OR C
           JP Z,IOORERR

           LD (HL),C
           INC HL
           LD (HL),B
           RST &18
           RET

TRANSHL:   LD C,(HL)
           INC HL
           LD B,(HL)
           PUSH HL
           CALL TRANSFORM
           POP HL
           RET C             ;RET IF NOT IN BLOCK

           LD (HL),B
           DEC HL
           LD (HL),C
           RET

TRANSFORM: LD   HL,(LAST)
           AND  A
           SBC  HL,BC
           RET  C

           LD   HL,(FIRST)
           SBC  HL,BC
           JR   Z,TRANS2

           CCF
           RET  C

TRANS2:    CALL SPSS
           LD   HL,SBO+2

TRANS3:    LD A,(HL)
           CP B
           JR NZ,TRANS4

           INC HL
           LD A,(HL)
           DEC HL
           CP C

TRANS4:    JR   NC,TRANS5

           INC  HL
           INC  HL
           JR   TRANS3

TRANS5:    LD   A,H
           ADD  A,0+(SBN-SBO)/256   ;NC
           LD   H,A
           LD   B,(HL)
           INC  HL
           LD   C,(HL)
TRANSF:    JP   RCURP

RENLN:     EQU TRANSF+3-RNMP2

           ORG INSTBUF

GETP2:     CALL SYNTAX1      ;VALID VARIABLE
           CALL CHKEND

           LD HL,FLAGS
           PUSH HL
           RES 5,(HL)        ;'NO KEY'
           CALL WKBR         ;GET KEY WITH BREAK OPTION
           POP HL            ;FLAGS
           BIT 6,(HL)
           JR NZ,GT2         ;JR IF NUMERIC VALUE WANTED

           CALL STACKA

           DB CALC
           DB CHRS           ;CONVERT KEY VALUE TO STRING
           DB EXIT

           JR GT4

GT2:       CALL NUMERIC
           JR C,GT3          ;JR IF '0' TO '9'

           AND &DF           ;FORCE U.C
           SUB 7

GT3:       SUB &30           ;'0'->0, 'A'->10D ETC.
           CALL STACKA

GT4:       CALL NOISE
           JP ASSIGN         ;ASSSIGN STACKED VALUE TO VARIABLE

GETLN:     EQU GT4+6-GETP2

           ORG INSTBUF

DELPT2:    LD HL,1
           CALL DELSR
           RST &18           ;CHAR AFTER 'DELETE'
           CP TOTOK
           JR Z,DL2

           CALL GIR2         ;EVAL NUM, GET TO HL IF RUNNING. SCF IF SYNTAX
           CALL NC,DELSR     ;CALL IF RUNNING

           RST &18
           CP TOTOK
           JP NZ,NONSENSE

DL2:       RST &20           ;SKIP 'TO'
           CALL CRCOLON
           LD HL,&FEFF       ;LAST LEGAL LINE NUMBER
           CALL NZ,GIR2      ;GET LAST LINE NUM

           CALL RUNFLG
           JR NC,DL4          ;JR IF SYNTAX TIME

           CALL FNDLINE
           JR NZ,DL4         ;IF NOT USED, HL IS START OF NEXT LINE

           CALL NEXTONE      ;IF LINE IN USE, GET ADDR OF END+1 IN DE
           EX DE,HL          ; THEN HL

DL4:       CALL CHKHL
           IN A,(251)
           LD D,A
           CALL CHKENDCP

           LD A,(LAST)
           LD C,A
           CALL TSURPG       ;SWITCH IN START
           LD A,D            ;AHL=END ADDR
           LD DE,(FIRST)     ;CDE=START ADDR
           CALL SUBAHLCDE
           RET C             ;RET IF START HIGHER THAN END E.G. DELETE 2 TO 1

           LD B,H
           LD C,L            ;ABC=LEN TO CLOSE
           EX DE,HL          ;HL=START
           CALL RECL2BIG     ;CLOSE UP ABC AT (HL)
           JP GT4R           ;'GOTO' NEXT STATEMENT SO NXTLINE ETC. SET UP AGAIN

DELSR:     CALL FNDLINE      ;DEFAULT LINE IS LINE 1 OR FIRST AFTER IT
           CALL CHKHL
           LD (FIRST),HL
           IN A,(251)
           LD (LAST),A
DELFIN:    JP SELCHADP

DELLN:     EQU DELFIN+3-DELPT2

           ORG HDR

KEYP2:     CALL SYNTAXA

           LD A,&FE
           CALL SBFSR2       ;COPY STRING TO BUFFER. LEN 0-511 OK
           RET Z             ;RET IF LEN Z

           LD HL,(PPC)
           PUSH HL
           LD A,(SUBPPC)
           PUSH AF           ;SAVE LINE/STAT FOR LATER 'GOTO'
           PUSH DE           ;STRING START
           PUSH BC           ;STRING LEN
           CALL CLEARSP      ;CLEAR ELINE
           POP BC
           PUSH BC
           CALL ADDRELN
           CALL MAKEROOM
           EX DE,HL          ;DE PTS TO ROOM
           POP BC
           POP HL            ;STRING START
           LDIR              ;COPY STRING TO ELINE (INDIRECT METHOD MEANS
                             ;EVEN DIRECT KEYIN OF LITERAL STRING WORKS)
           CALL SETESP       ;ERRORS WILL RETURN TO THIS ROUTINE NOW.
                             ;OLD ERRSP IS ON STACK
           CALL TOKMAIN      ;TOKENIZE ELINE
           CALL LINESCAN     ;CHECK SYNTAX
           LD A,(ERRNR)
           AND A
           JR NZ,KI3         ;JR IF ERROR OCCURRED

           LD HL,FLAGS
           SET 7,(HL)        ;'RUNNING'
           CALL EVALLINO     ;CY IF IOOR, Z IF 0
           JP C,NONSENSE

           JR NZ,KI2         ;JR IF THERE WAS A LINE NUMBER

           INC C             ;STAT 1
           CALL LOOPEL       ;RUN ELINE
           SCF

KI2:       CALL NC,INSERTLN

KI3:       POP HL            ;OLD ERRSP
           CALL RESESP       ;RESET ERRSP, RESPOND TO ANY ERRORS
           CALL CLEARSP      ;CLEAR ELINE
KEYFIN:    JP GT4P           ;SET UP SO GOTO HAPPENS. MEANS LINE CAN BE KEYED
                             ;IN BEFORE LINE BEING EXECUTED.

KEYLN:     EQU KEYFIN+3-KEYP2

           ORG INSTBUF

POPP2:     CALL CRCOLON

           CALL NZ,VNUMV        ;EVAL NUM VARIABLE, SET NZ

POP1:      EX AF,AF'         ;Z IF NO VAR
           CALL CHKEND

           EX AF,AF'
           PUSH AF
           LD HL,(BSTKEND)
           LD A,(HL)
           INC A
           JR NZ,POP2        ;JR IF NOT THE STACK TERMINATOR

           RST &08
           DB 11             ;'No POP data'

POP2:      CALL RETLOOP2     ;ACCEPT ANY TYPE OF RET ADDR - DO/GOSUB/PROC
                             ;HL=LINE, A=TYPE/PAGE
           LD B,A
           POP AF
           PUSH BC           ;TYPE/PAGE
           JR Z,POP4         ;RET IF NO VARIABLE TO POP INTO

           INC H
           DEC H
           LD D,H
           LD E,H
           JR Z,POP3         ;JR IF ELINE - USE LINE OF 0

           LD A,B
           CALL TSURPG
           LD D,(HL)
           INC HL
           LD E,(HL)         ;DE=LINE NUMBER

POP3:      EX DE,HL
           CALL STACKHL
           CALL ASSIGN       ;FPCS TO VAR

POP4:      POP AF            ;TYPE/PAGE
           AND &E0
           CP &40
           JP Z,DELOCAL    ;IF 'PROC' TYPE, CLEAR LOCAL VARS TOO

POP5:      RET

POPLN:     EQU POP5-POPP2+1

           ORG INSTBUF

INPP2:     CALL RUNFLG
           CALL C,CLSLOWER     ;CLS AND SELECT CHANNEL 'K' IF RUNNING
           CALL SPACAN       ;NO INDENT **
           INC A             ;A=1
           LD (TVFLAG),A
           CALL INPSL
           XOR A
           LD (FLAGX),A      ;'NOT INPUT LINE' SO LISTING OK
           CALL CHKENDCP

           CALL CLSLOWER
           LD A,(SPOSNU+1)
           LD HL,UWTOP
           SUB (HL)
           INC A
           LD (SCRCT),A      ;EVERYTHING ON SCREEN DOWN TO SPOSN HAS BEEN SEEN,
                             ;SO WE CAN SCROLL THOSE LINES OFF BEFORE 'SCROLL?'
           RET

INPSL:     RST &18
           CALL PRTERM
           JR Z,IP2CR        ;JR IF CR/COLON/CLOSE BRACK.

           CALL PRSEPR
           RET Z             ;RET IF TERMINATOR FOUND AFTER ;/,/'

           CALL C,IPITEM     ;CALL INPUT ITEM SR IF NON-SEPARATOR FOUND
           JR INPSL

IP2CR:     LD A,(DEVICE)
           AND A
           JP Z,RUNCR        ;CR IF RUNNING IF UPPER SCREEN IN USE

           RET

IPITEM:    CP "("
           JR NZ,INP2        ;JR IF NOT AN EMBEDDED PRINT ITEM

           RST &20           ;SKIP '('
           CALL PRINT2       ;HANDLE PRINT ITEM
           RST &18
           JP INSISCBRK      ;CHECK/SKIP ')'

INP2:      CP LINETOK
           JR Z,INP4

           CALL ALPHA
           JP NC,PRITEM      ;JR IF NOT A LETTER

           CALL SYNTAX1      ;EXITS WITH DE=FLAGX
           LD HL,FLAGX
           RES 7,(HL)        ;'NOT INPUT LINE'
           JR INP5

INP4:      CALL SSYNTAX1     ;SKIP 'LINE', EVAL VAR
           CALL RUNFLG
           JP M,NONSENSE     ;ERROR IF NUMERIC VARIABLE

           LD HL,FLAGX
           SET 7,(HL)        ;"USING INPUT LINE"

INP5:      CALL ABORTER

           PUSH HL
           CALL SETWORK      ;CLEAR WORKSPACE
           POP HL            ;FLAGX
           SET 5,(HL)        ;'INPUT MODE'
           SET 6,(HL)        ;'NUMERIC RESULT'
           LD BC,1
           CALL RUNFLG
           JP M,INP6         ;JP IF NUMERIC - USE 1 SPACE

           RES 6,(HL)        ;STRING RESULT
           BIT 7,(HL)
           JR NZ,INP6        ;JR IF INPUT LINE - USE 1 SPACE

           LD C,3

INP6:      CALL WKROOM
           LD (HL),&0D       ;LAST LOCN
           DEC C
           JR Z,INP7         ;JR IF JUST ONE SPACE

           DEC HL
           LD A,&22
           LD (HL),A         ;SECOND LOCN
           LD (DE),A         ;FIRST LOCN

INP7:      LD (KCUR),HL      ;KCUR PTS TO CR, OR 2ND QUOTE
           IN A,(URPORT)
           LD (KCURP),A
           LD A,(FLAGX)
           RLA
           JR C,INP9         ;JR IF INPUT LINE - NO SYNTAX CHECK ON INPUT

           LD A,(CHADP)
           PUSH AF
           LD HL,(CHAD)
           PUSH HL
           LD HL,(ERRSP)
           PUSH HL

INPERR:    LD HL,INPERR
           PUSH HL
           XOR A
           LD (ERRNR),A      ;'NO ERROR'
           CALL KSCHK
           JR NZ,INP8        ;JR IF NOT 'K' OR 'S' CHANNEL

           LD (ERRSP),SP     ;ONLY SET ERRSP FOR K/S

INP8:      CALL ADDRWK       ;PT TO WORKSPACE
           CALL REMOVEFP
           CALL EDITOR
           CALL TOKMAIN
           LD HL,FLAGS
           RES 7,(HL)        ;SYNTAX TIME
           CALL INPAS
           AND A             ;SKIP NEXT INSTR.

INP9:      CALL C,EDCX       ;EDITOR

           CALL KSCHK
           JR NZ,INPA        ;JR IF NOT 'K' OR 'S' CHANNEL

           LD (KCUR+1),A     ;NO CURSOR WANTED
           CALL EDPRT        ;PRINT INPUT LINE
           LD HL,(OLDPOS)
           LD BC,POSTORE
           CALL R1ONCLBC     ;SCREEN POSN=PAST END OF PRINTED INPUT LINE

INPA:      LD HL,FLAGX
           LD A,(HL)
           RES 7,(HL)        ;'NOT INPUT LINE'
           RES 5,(HL)        ;'NOT INPUT MODE'
           RLA
           JR NC,INPC        ;JR IF NOT INPUT LINE

           CALL ADDRWK
           LD D,H
           LD E,L            ;DE=START
           LD BC,&FFFF

INPBL:     LD A,(HL)
           INC HL
           INC BC
           CP &0D
           JR NZ,INPBL       ;COUNT LEN TILL 0D TERMINATOR

           CALL STKSTOREP
           JP ASSIGN

INPC:      POP AF            ;JUNK INPERR
           POP HL
           LD (ERRSP),HL
           POP HL
           LD (PRPTR),HL
           POP AF
           LD (PRPTRP),A     ;ORIG CHAD IN AUTO-ADJUST SYS VAR
           LD HL,FLAGS
           SET 7,(HL)
           CALL INPAS
           LD A,(PRPTRP)
           CALL SETCHADP
           LD HL,(PRPTR)
           LD (CHAD),HL
           RET

INPAS:     LD HL,(WORKSP)
           LD (CHAD),HL
           LD A,(WORKSPP)
           CALL SETCHADP
           RST &18
           CP &B1            ;STOPTOK
           JR NZ,INPA2

           CALL RUNFLG
           RET NC

           RST &08
           DB 17             ;'STOP in INPUT'

INPA2:     LD A,(FLAGX)
           CALL VALFET2
           RST &18
           CP &0D
           RET Z

           RST &08
INPFIN:    DB 29             ;'NONSENSE'

INPLN:     EQU INPFIN+1-INPP2

                                 ;INPUT
