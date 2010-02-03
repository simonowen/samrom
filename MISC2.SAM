;MISC2.SAM  RENUM, GET, AUTO, DELETE, LET,
;OPEN, CLOSE, DEF KEYCODE, LABEL, RUN, CLEAR

;FROM RST 08H

ERROR2:    LD HL,(CHAD)
           LD (XPTR),HL
           EX AF,AF'
           LD A,(CHADP)
           LD (XPTRP),A
           POP DE
           LD A,(DE)
           INC DE
           PUSH DE
           DEC DE
           LD HL,(RST8V)
           INC H
           DEC H
           CALL NZ,HLJUMP

           LD A,(DOSCNT)     ;0 IF DOS NOT IN CONTROL, 1 IF DOS IN CONTROL
           RRCA
           LD A,(DE)         ;ERROR NUMBER
           JR C,NORMERR      ;JR IF DOS RUNNING - DON'T RECURSE!

           LD A,(DOSFLG)
           AND A
           LD A,(DE)
           JR NZ,PTDOS        ;JR IF DOS BOOTED

           CP 128
           JR C,NORMERR      ;JR IF NOT A DOS HOOK CODE

NODOS:     LD A,53           ;'NO DOS'

NORMERR:   LD (ERRNR),A
           LD HL,DOSCNT
           RES 0,(HL)        ;'DOS NOT IN CONTROL'
           LD SP,(ERRSP)
           JP SETSTK

;PASS TO DOS

PTDOS:     AND A
           JR Z,NORMERR

           LD E,A            ;ERROR NUMBER
           LD C,250
           IN B,(C)          ;B=LRPORT
           LD HL,0
           ADD HL,SP
           LD A,(DOSFLG)     ;0 OR DOS PAGE (1-1FH)
           DEC A             ;GET PAGE NO. FOR SECTION A (DOS IN SECTION B)
           DI
           OUT (250),A       ;DOS PAGED IN AT 4000H, ROM0 ON, ROM1 OFF
           LD SP,&8000       ;STACK NOW OK
           EI
           PUSH BC           ;B=PREV LRPORT
           PUSH HL           ;PREV STACK PTR
           LD A,E            ;HOOK CODE
           CP 128
           JR NC,DOSHK       ;JR WITH HOOK CODES

           CALL &4203        ;HANDLE ERROR CODE, RATHER THAN HOOK CODE
           SCF               ;'COMING FROM ERROR'

DOSHK:     CALL NC,&4200     ;HANDLE HOOK CODE IN A
                             ;'COMING FROM HOOK' MUST SET NC!

DOSC:      POP HL            ;PREV STACK PTR
           POP BC
           DI
           OUT (C),B         ;PREV LRPORT RESTORED
           LD SP,HL          ;PREV STACK
           EI

           LD HL,(DOSER)
           INC H
           DEC H
           JR NZ,DHLJ        ;JP TO VECTOR IF WANTED

           JR NC,DOSNC       ;NC/CY FROM 4200/4203 - JR IF WAS HOOK CODE

           LD HL,(ERRSP)
           DEC HL
           DEC HL
           LD SP,HL          ;CLEAR STACK IF WAS ERROR ENTRY

DOSNC:     AND A
           JR NZ,NORMERR     ;JR IF ERROR (NORMAL ERROR, OR DOS ERROR (81-127D)

           DEC E
           JP Z,LDFL         ;JP IF E WAS 1 - LOAD MAIN BODY OF FILE

           DEC E
           JP Z,SVFL         ;JP IF E WAS 2 - SAVE ENTIRE FILE

           DEC E
           JP Z,LKTH         ;JP IF E WAS 3 - LOAD ENTIRE FILE ** NEW JUMP

           RET               ;RET IF NO ERRORS - TO NEXT STAT IF VIA ERROR ENTRY

DHLJ:      JP (HL)

;STUBS OF ROUTINES THAT COPY FROM ROM1 TO BUFFER FOR EXECUTION:

MEPROG:

 LD HL,&C000+RENLN+GETLN+DELLN+KEYLN+POPLN+INPLN+DKLN+RDLN+DFNLN+TOKLN
           DB &DD            ;'JR+3'

INPUT:     LD HL,&C000+RENLN+GETLN+DELLN+KEYLN+POPLN
           DB &DD            ;'JR+3'

RENUM:     LD HL,&C000       ;PART 2 OF RENUM
           LD BC,RENLN
           JR BUFMV

;KEYIN A$

KEYIN:     LD HL,&C000+RENLN+GETLN+DELLN
           LD DE,HDR         ;SO OTHER CMDS DON'T CORRUPT KEYIN, EXCEPT
                             ;LOAD/SAVE EXEC. WILL CORRUPT FIRST PART
                             ;(HARMLESSLY)
           LD BC,KEYLN
           JR BUFMV2

TOKMAIN:   CALL SETDE        ;GET DE=WKSP OR ELINE START

;ENTRY FROM VAL FUNCTION

TOKDE:     EXX
           LD HL,&C000+RENLN+GETLN+DELLN+KEYLN+POPLN+INPLN+DKLN+RDLN+DFNLN
           LD DE,CDBUFF+&80    ; (4D80-4E24)
           LD BC,TOKLN
           JR BUFMV2

;GET X OR GET X$

GET:       LD HL,&C000+RENLN
           DB &DD            ;'JR+3'
;POP <X>

;DELETE N TO M - DELETE PROGRAM LINES

DELETE:    LD HL,&C000+RENLN+GETLN
           DB &DD            ;'JR+3'

POP:       LD HL,&C000+RENLN+GETLN+DELLN+KEYLN
           DB &DD            ;'JR+3'

DEFKEY:    LD HL,&C000+RENLN+GETLN+DELLN+KEYLN+POPLN+INPLN
           DB &DD            ;'JR+3'

DEFFN:     LD HL,&C000+RENLN+GETLN+DELLN+KEYLN+POPLN+INPLN+DKLN+RDLN
           LD BC,&91         ;(DEF KEY LEN =90H)

BUFMV:     LD DE,INSTBUF

BUFMV2:    PUSH DE
           PUSH AF
           LD A,&5F
           OUT (250),A       ;ROM1 ON
           LDIR
           LD A,&1F
           OUT (250),A       ;ROM1 OFF
           POP AF
           RET               ;TO INSTBUF

DEFAULT:   LD A,&FF
           DB &FE            ;'JR+1'

LET:       XOR A

           LD (LTDFF),A
           DB &FE            ;'JR+1'

LETLP:     RST &20

           CALL SYNTAX1      ;CHECK VALID VAR
           RST &18
           CP "="
           JP NZ,NONSENSE

           RST &20
           LD A,(LTDFF)
           INC A
           JR NZ,LET3        ;JR IF 'LET', CONTINUE WITH 'DEFAULT'

           CALL RUNFLG       ;EVEN IF 'DEFAULT',
           JR NC,LET3        ;ALWAYS CHECK ASSIGNED VALUE IF SYNTAX TIME

           LD A,(FLAGX)
           RRA
           JR C,LET3         ;JR IF VAR DOESN'T EXIST - CREATE IT, LIKE 'LET'

;          LD HL,FLAGS
;          BIT 6,(HL)
;          JR Z,LET2         ;JR IF STRING

           LD A,(DFTFB)
           AND A
           JR Z,LET3

LET2:      CALL EXPTEXPR     ;EVAL ASSIGNED VALUE IN ORDER TO SKIP IT. (SLOW,
                             ;BUT SAVES USE OF SPECIAL SKIPEXPR ROUTINE)
           CALL FDELETE
           JR LET4

LET3:      CALL VALFET1

LET4:      RST &18
           CP ","
           JR Z,LETLP

           RET

;E.G. LABEL heaven:

LABEL:     CALL RUNFLG
           JP C,SKIPCSTAT    ;JUST SKIP STATEMENT IF RUNNING

           DB &FE            ;'JR+1'

;SKIP, CHECK FOR VALID NUMERIC VARIABLE
;USED BY COMPILE, LABEL

SVNUMV:    RST &20

;FROM POP, LABEL

VNUMV:     CALL SYNTAX1      ;VALID VAR
           LD HL,FLAGS
           BIT 6,(HL)
           RET NZ            ;RET IF NUMERIC

           RST &08
           DB 29             ;'SYNTAX ERROR'


;TCLR.SAM - CLEAR, RUN

RUN:       CALL SYNTAX3
           CALL GOTO2        ;SET NEWPPC AND NSPPC FOR JUMP
           CALL RESTOREZ     ;DO RESTORE 0. SWITCHES IN PROGP
           JR CLR1           ;NOW DO CLEAR 0

CLEAR:     CALL SYNTAX3      ;NUMBER OR ZERO
           CALL UNSTLEN
           LD C,A
           DEC C
           OR H
           OR L
           SET 7,H
           JR NZ,CLR3        ;JR IF A CLEAR PARAM USED

CLR1:      LD A,(RAMTOPP)
           LD HL,(RAMTOP)
           LD C,A

CLR3:      PUSH BC
           PUSH HL
           CALL ADDRNV
           EX DE,HL
           LD C,A            ;CDE=NVARS
           LD HL,(ELINE)
           LD A,(ELINEP)
           CALL SUBAHLCDE    ;GET ELINE-NVARS IN AHL (AT LEAST 025DH)
           LD BC,&025D
           CALL SUBAHLBC     ;AHL=SPACE TO RECLAIM
           LD B,H
           LD C,L
           LD HL,(NVARS)
           CALL RECL2BIG     ;RECLAIM ABC AT HL
           CALL CLRSR
           CALL DOCOMP       ;COMPILE
           CALL MCLS         ;CLEAR ENTIRE SCREEN
           LD HL,(WKEND)
           LD A,(WKENDP)
           LD BC,180         ;**
           CALL ADDAHLBC     ;AHL=WKEND+180
           POP DE
           POP BC            ;CDE=CLEAR PARAM, OR RAMTOP
           CALL SUBAHLCDE
           JR NC,RTERR       ;JR IF RAMTOP WILL BE TOO CLOSE TO WKEND

           LD A,(LASTPAGE)
           CP C
           JR NC,CLR4        ;OK IF RAMTOP PAGE <= LAST ALLOCATED PAGE

RTERR:     RST &08
           DB 48             ;'Invalid CLEAR address'

;CLEAR MACHINE STACK (SHOULDN'T NEED IT!)

CLR4:      LD A,C
           LD (RAMTOPP),A
           LD (RAMTOP),DE
           POP HL            ;NEXT STAT
           POP BC            ;ERR HANDLER
           LD SP,ISPVAL
           PUSH BC
           LD (ERRSP),SP
           JP (HL)

;CLEAR FPCS, BASIC STACK, NUMERIC AND STRING VARS, TURNS 'RECORD' OFF
;SOUND CHIP OFF, ON ERROR OFF

CLRSR:     CALL CLSND        ;CLEAR SOUND CHIP, A=0
           LD (GRARF),A      ;GRAPHICS RECORDING OFF
           LD (ONERRFLG),A   ;ON ERROR OFF
           LD HL,(BASSTK)
           LD (HL),&FF
           LD (BSTKEND),HL   ;CLEAR DO/GOSUB/ETC STACK
           CALL ADDRNV
           LD B,46

CLNVP:     LD (HL),&FF
           INC HL
           DJNZ CLNVP        ;INIT 23 LETTER PTRS

           EX DE,HL
           LD HL,PSVTAB
           LD C,26
           LDIR              ;COPY 3 PTRS AND YOS/YRG
           LD HL,PSVT2
           LD C,20
           LDIR              ;COPY YOS/YRG AGAIN
           EX DE,HL
           CALL SETNE
           INC H
           INC H
           CALL SETSAV
           LD (HL),&FF       ;FF TERMINATOR OF SAVARS
           DEC H
           DEC H
           DEC HL
           DEC HL
           DEC HL
           LD (HL),B         ;CHANGE 192 YRG TO 0
           INC HL
           LD (HL),1         ;NOW 256
           LD A,(THFATT)
           AND A
           RET NZ

           INC (HL)          ;IF MODE 2 THIN PIX, MAKE XRG=512
           RET

SETSAV:    LD IY,SAVARSP
           JR SETSYS

SETNE:     LD IY,NUMENDP

;SET SYS VAR TO CURRENT PAGE AND ADDR IN HL. ADJUST IF NEEDED TO 8000-BFFF, BUT
;KEEP HL THE SAME. ONLY F AND IY ALTERED

SETSYS:    PUSH AF
           IN A,(251)
           AND &1F
           LD (IY+0),A
           LD (IY+1),L
           LD (IY+2),H
           POP AF
           BIT 6,H
           RET Z

           RES 6,(IY+2)
           INC (IY+0)
           RET



CLSND:     LD A,32           ;CLEAR 32 REGISTERS

CSRL:      LD BC,SNDPORT+&0100 ;SOUND REG ADDR
           DEC A
           OUT (C),A
           DEC B             ;BC=DATA PORT
           OUT (C),B
           AND A
           JR NZ,CSRL

           RET

PSVTAB:    DW &0019          ;X VARS
           DW &0003          ;Y VARS
           DW &FFFF          ;Z VARS

PSVT2:     DB 2
           DW 8
           DM "os"
           DB 0,0,0,0,0      ;YOS

           DB 2
           DW &FFFF
           DM "rg"
           DB 0,0,192,0,0    ;YRG


;STREAM 16 IS CONVERTED TO STREAM -4 INTERNALLY
;CLOSE #16 AND OPEN #16 NOT ALLOWED
;E.G. RECORD TO A$: LET A$="": PRINT #16;"TESTING" ADDS "TESTING (CR)" TO A$
;O/P TO STREAM 16 LOOKS FOR STRING NAME IN (STRM16NM), AND 'PRINTS' CHARS
;TO THE END OF THE STRING.
;ALLOWS 'SERIAL FILES', TOKEN EXPANSION, CAT TO A STRING, RECORDING OF GRAPHICS
;COMMANDS, ETC. CALLED WITH ROM1 OFF


S16OSR:    LD B,A            ;CHAR TO O/P
           IN A,(URPORT)     ;SAVE URPORT STATUS AND RESTORE AT END
           PUSH AF
           PUSH BC
           LD HL,STRM16NM    ;STORED TLBYTE AND NAME OF STRING
           LD DE,TLBYTE
           LD BC,11
           LD A,(HL)
           LDIR
           LD C,A
           CALL STARYLK2
           JP Z,VNFERR

           POP BC            ;B=DATA
           IN A,(URPORT)
           PUSH AF
           PUSH HL           ;SAVE ADDR OF $ LEN DATA
           PUSH BC
           LD A,(HL)
           INC HL
           LD C,(HL)
           INC HL
           RRCA
           RRCA
           OR (HL)
           LD B,A            ;BC=LEN
           INC BC            ;NEW LEN
           LD A,B
           INC A
           JR NZ,S16OK       ;OK UNLESS STRING LEN >FEFF

STLERR:    RST &08
           DB 42             ;'String too long'

S16OK:     PUSH BC
           ADD HL,BC         ;HL PTS TO LAST BYTE
           CALL C,PGOVERF
           CALL CHKHL
           CALL MKRM1        ;OPEN 1 SPACE AT END OF STRING
           POP BC            ;NEW LEN
           POP AF            ;A=DATA
           LD (HL),A         ;ADD CHAR TO END
           POP DE
           POP AF
           OUT (URPORT),A    ;PAGE IN STRING HEADER AT DE
           CALL MBC
           POP AF
           OUT (URPORT),A
           RET

MBC:       CALL SPLITBC
           LD HL,PAGCOUNT
           LD BC,3
           LDIR              ;COPY PAGE/LEN TO STRING
           RET

SSYNTAX3:  RST &20

;SYNTAX 3 - NUMBER, OR USE 0

SYNTAX3:   RST &18
           CALL FETCHNUM
           RET C             ;RET IF RUNNING

           POP AF            ;JUNK RET ADDR
           RET

SSYNTAX6:  RST &20

;SYNTAX 6 - INSIST ON NUMBER

SYNTAX6:   CALL EXPT1NUM
           RET C

           POP AF
           RET

SSYNTAX8:  RST &20

;SYNTAX 8 - INSIST ON NUMBER,NUMBER

SYNTAX8:   CALL EXPT2NUMS
           RET C

           POP AF
           RET

SSYNTAXA:  RST &20

;SYNTAX A - INSIST ON A STRING

SYNTAXA:   CALL EXPTSTR
           RET C

           POP AF
           RET


;COMMA/SEMICOLON RETURN Z

COMMASC:   CP ","
           RET Z

           CP ";"
           RET


COCRCOTO:  CP TOTOK
           RET Z

;COMMA/CR/COLON RETURN Z

COMCRCO:   CP ","
           RET Z

           DB &FE            ;'JR+1'

RCRC:      RST &18

;CR/COLON RETURN Z

CRCOLON:   CP ":"
           RET Z

           CP &0D
           RET

RICSC:     RST &18

;INSIST ON A COMMA OR SEMI-COLON, SKIP

INSISCSC:     CP ";"
              JR Z,INSCOMN

INSISCOMA:    CP ","
              JR NZ,SYNONS

INSCOMN:      RST &20
              RET

;SKIP, INSIST ON OPENING BRACKET

SINSISOBRK:   RST &20

;INSIST ON AN OPENING BRACKET

INSISOBRK:    CP "("
              JR NZ,SYNONS

              RST &20
              RET

;EXPECT NUMBER, THEN CLOSING BRACKET

EX1NUMCB:     CALL EXPT1NUM

;INSIST ON A CLOSING BRACKET

INSISCBRK:    CP ")"
              JR NZ,SYNONS

              RST &20
              RET

;EXPECT COMMA, THEN STRING

EXPTCSTR:     RST &18
              CP ","
              JR NZ,SYNONS

;SKIP, EXPECT STRING

SEXPTSTR:     RST &20

;EXPECT STRING

EXPTSTR:      CALL SCANNING
              ADD A,A             ;CY IF RUNNING
              LD A,C              ;CURRENT CHAR
              RET P               ;RET IF STRING

SYNONS:       RST &08
              DB 29               ;'SYNTAX ERROR'

;EXPECT COMMA, STRING, CLOSING BRACKET

EXPTCSTRB:    CALL EXPTCSTR
              JR EXCBRF         ;CHECK ')', RUN FLAG

;EXPECT '(N,N)'. SET CY IF RUNNING

EXB2NUMB:     CALL SINSISOBRK   ;SKIP, '('
              CALL EXPT2NUMS    ;N,N
              JR EXCBRF         ;CHECK ')', RUN FLAG

;EXPECT Bracket, String, Comma, Number, Bracket. '(a$,n)'. CY IF RUNNING

EXBSCNB:      CALL SINSISOBRK   ;'('
              CALL EXPTSTR      ;A$
              CP ","
              JR NZ,SYNONS

SEX1NUMCB:    CALL SEXPT1NUM    ;SKIP, GET N. CY IF RUNNING

EXCBRF:       CALL INSISCBRK    ;')' . CY IF RUNNING

RUNFLG:       LD A,(FLAGS)
              ADD A,A           ;C IF RUNNING, P IF STRING, M IF NUMBER
              RET

;EXPECT Bracket, Number, Comma, String, Bracket. '(n,a$)'. CY IF RUNNING

EXBNCSB:      CALL SINSISOBRK   ;'('
              CALL EXPT1NUM     ;N
              JR EXPTCSTRB      ;',A$)'

;SKIP, EXPECT N,N,N,N

SEXPT4NUMS:   RST &20

EXPT4NUMS:    CALL EXPT2NUMS
              CP ","
              JR NZ,SYNONS

;SKIP, EXPECT N,N

SEXPT2NUMS:   RST &20

;EXPECT N,N

EXPT2NUMS:    CALL EXPT1NUM

;EXPECT ,N

EXPTCNUM:     CP ","
              JR NZ,SYNONS


;SKIP, EXPECT NUMBER

SEXPT1NUM:    RST &20

;EXPECT NUMBER

EXPT1NUM:     CALL SCANNING
              ADD A,A        ;CY IF RUNNING
              LD A,C
              RET M          ;RET IF NUMERIC

              JR SYNONS

;INSIST A=LETTER

GETALPH:   CALL ALPHA
           RET C

           JR SYNONS

;SYNTAX9 - DEAL WITH COLOUR ITEMS, COORDS
; EG INK 3,PAPER 1;X,Y. USED BY PLOT, CIRCLE, FILL

SYNTAX9:   CALL SYNT9SR
           JR EXPT2NUMS

;EXPECT N OR CR/COLON. RETURN WITH CY IF RUNNING

FETCHNUM:  CALL CRCOLON
           JR NZ,EXPT1NUM    ;EXITS WITH CY IF RUNNING

CONDSTK0:  LD A,(FLAGS)
           RLA
           RET NC

           XOR A
           CALL STACKA
           SCF               ;'RUNNING'
           RET

SEXPTEXPR:    RST &20

;EXPECT AN EXPRESSION, SET Z IF STRING, NZ IF NUMERIC

EXPTEXPR:     CALL SCANNING
              RLA            ;CY IF RUNNING
              BIT 7,A
              LD A,C
              RET

;USED BY INPUT,..

CHKENDCP:  CALL SELCHADP
           DB &FE            ;"JR+1"

;SKIP, ABORT IF SYNTAX TIME. USED BY E.G. PI, HIMEM, FREE.

SABORTER:  RST &20

;JUST RETURN IF RUNNING, ELSE JUNK A RET ADDR, RET TO NEXT LEVEL.

CHKEND:
ABORTER:   LD C,A
           LD A,(FLAGS)
           RLA
           LD A,C
           RET C

           POP AF
           RET

;CY IF A=LETTER, ELSE NC

ALPHA:     CP "A"
           CCF
           RET NC            ;RET IF TOO LOW

           CP "z"+1
           RET NC            ;RET IF TOO HI

           CP "Z"+1
           RET C             ;RET IF UC

           CP "a"
           CCF
           RET

;CY IF A=LETTER OR DIGIT

ALPHANUM:  CALL ALPHA
           RET C

;CY IF A DIGIT

NUMERIC:   CP "9"+1          ;NC IF TOO HIGH
           RET NC

           CP "0"
           CCF
           RET


;CY IF LETTER OR UNDERLINE OR '$'
;(SO E.G. printer, print_out, print$ ARE NOT TOKENISED, BUT print1, print: ARE)

ALDU:      CALL ALPHA
           RET C

           CP "$"
           SCF
           RET Z

           JR CKUND


;CY IF LETTER, DIGIT OR UNDERLINE

ALNUMUND:  CALL ALPHANUM
           RET C

CKUND:     CP "_"
           SCF
           RET Z

           AND A
           RET

;EVAL. BRACKETLESS SLICER E.G. 10 TO 30, TO 100, 100 TO, TO
;ENTRY: CHAD PTS TO POSSIBLE SLICER, A=(CHAD)
;EXIT: SYS VARS SET UP, WITH VALUE, IF GIVEN, OR DEFAULT. CHAD PTS. TO NON-ALPHA
;NUMERIC CHAR, NOT 'TO'. IF RUNNING, CY=OUT OF RANGE. A=0 IF SLICER IS 1 NUMBER

BRKLSSL:   LD HL,1           ;MIN
           LD DE,&FEFF       ;MAX (LINE NUMBERS)
           LD (FIRST),HL
           LD (LAST),DE
           CP TOTOK
           JR Z,BRL2

           CALL ALPHANUM
           RET NC            ;NC='IN RANGE' IF EG LIST

           CALL GIR2         ;GET INT IN BC AND HL IF RUNNING, NC IF RUNNING
           JR C,BRL1         ;JR IF NOT RUNNING

           LD BC,(FIRST)     ;MIN
           SBC HL,BC
           ADD HL,BC
           LD (FIRST),HL
           RET C             ;RET IF MIN>VALUE

BRL1:      CP TOTOK
           LD A,0            ;CANNOT USE XOR A!
           JR NZ,BRL3        ;JR WITH A=0 (FLAG) IF SLICER = 1 NUMBER
                             ;LAST=FIRST

BRL2:      RST &20
           CALL ALPHANUM
           RET NC            ;'IN RANGE' IF EG LIST 10 TO

           CALL GIR2
           CCF
           RET NC            ;END WITH NC IF SYNTAX TIME AND EG LIST 10 TO 20

           LD HL,(LAST)      ;MAX
           SBC HL,BC
           RET C             ;RET IF VALUE>MAX

           LD H,B
           LD L,C

BRL3:      LD (LAST),HL
           AND A
           RET


;GET IF RUNNING - AUTO, DELETE, BRACKETLESS SLICER SR.
;ENTRY: CHAD PTS TO EXPR (GIR2) OR IS BEFORE EXPR (GIR).
;EXIT: CHAD PTS PAST EXPR, A=(CHAD), BC AND HL=INT (IF RUNNING)
;CY IF RUNNING

GIR:       RST &20

GIR2:      CALL EXPT1NUM
           CCF
           RET C             ;RET IF NOT RUNNING

           CALL GETINT
           RST &18
           LD H,B
           LD L,C
           AND A
           RET               ;INT IN BC AND HL, NC



;DISPLAY (SCREEN)

DISPLAY:   CALL SYNTAX3

           CALL GETBYTE      ;IN A AND C

SETDISP:   LD (CURDISP),A
           AND A
           JR Z,DEFDISP      ;'DISPLAY 0' MEANS DISPLAY CURRENT SCREEN

           RST &30
           DW SCRNTLK2       ;GET MODE/PAGE FOR SCREEN C, Z IF UNUSED
           JR NZ,VIDSEL

ISCRERR:   RST &08
           DB 43             ;'INVALID SCREEN NUMBER' (ERRORS SET CURDISP TO 0)

;DEFAULT DISPLAY (USED BY 'DISPLAY' AND REPORTS)

DEFDISP:   LD A,(CUSCRNP)    ;PAGE BEING USED FOR PRINT, PLOT, ETC.(CURRENT)
                             ;CONTAINS MODE TOO.

VIDSEL:    RST &30
           DW CUS2           ;SEE IF DISPLAYED=CURRENT

           RET Z             ;RET IF NO CHANGE IN DISPLAYED SCREEN.

           PUSH DE           ;NEW DISPLAY PAGE IN D
           AND A             ;NC
           CALL SDISR        ;SWITCH PREV DISPLAYED SCREEN IN AT 8000H
                             ;AND COPY WORKING PALTAB TO IT FOR LATER USE IF
                             ;IT IS RE-DISPLAYED
           POP AF
           OUT (VIDPORT),A   ;MODE/PAGE SENT TO HARDWARE - NEW DISPLAY
           SCF

SDISR:     EX AF,AF'
           IN A,(251)
           PUSH AF
           EX AF,AF'
           PUSH AF
           INC A             ;SO SECOND PAGE OF SCREEN AT 8000H
           CALL TSURPG
           LD HL,PALBUF-&4000          ;END OF SECOND SCREEN PAGE
           LD DE,PALTAB
           POP AF
           JR C,SDIS2

           EX DE,HL

SDIS2:     LD BC,&28
           LDIR              ;COPY PALETTE TO/FROM DISPLAYED SCREEN
                             ;AND PALTAB IN SYS VARS
           JP PPORT          ;POP AF, OUT 251,RET

PRSVARS:   LD (CUSCRNP),A

;RESTORE SCREEN VARS FROM SCREEN PAGE

RSVARS:    SCF
           DB &26            ;'JR+1'

;SAVE SCREEN VARS TO SCREEN PAGE

SSVARS:    AND A

           LD HL,BGFLG
           LD DE,PVBUFF      ;POSN, INK ETC. ATTACHED TO SCREEN
           JR NC,SSVRC

           EX DE,HL

SSVRC:     CALL SPSSR
           LD BC,PRPOSN-BGFLG
           LDIR
           LD C,CEXTAB-PRPOSN
           ADD HL,BC
           EX DE,HL
           ADD HL,BC
           EX DE,HL          ;SRC AND DEST ADVANCED TO COPY CEXTAB
           LD C,&40
           LDIR
           JP RCURPR

;ROM 0 INSTRING

R0INST:       PUSH BC             ;START POSN
              LD B,D
              LD C,E              ;BC=BYTES TO CHECK
              LD DE,INSTBUF       ;HL=START-OF-SEARCH PTR, (SP)=START POSN
                                  ;DE=T$ START, A=T$ LEN
              PUSH BC             ;BYTES TO CHECK

LOOKLP:       PUSH AF             ;TARGET$ LEN
              EX   AF,AF'         ;A'=TARGET$ LEN COUNTER
              LD   A,(DE)         ;GET FIRST TARGET$ CHARACTER
              CPIR                ;LOOK FOR IT IN SEARCH$ USING HL AS PTR
              JP PO,NOTFND0       ;JR IF NOT FOUND - BC=0000

              PUSH HL             ;SEARCH$ PTR
              DB &3E              ;'JR +1' - HL IS AT SECOND CHAR ALREADY

CHKNXTC:      INC HL

              EX   AF,AF'         ;GET TARGET$ LEN COUNTER
              DEC  A
              JR   Z,FOUND        ;FOUND IF ALL CHARS MATCHED

              EX   AF,AF'
              INC  DE
              LD   A,(DE)         ;NEXT TARGET$ CHAR
              CP   (HL)           ;CP NEXT SEARCH$ CHAR
              JR   Z,CHKNXTC

              EXX
              CP C                ;C' IS USUALLY '#'
              EXX

              JR   Z,CHKNXTC      ;HASH ALWAYS MATCHES

              LD DE,INSTBUF       ;MATCH FAILS - RESTORE T$ PTR
              POP  HL             ;S$ PTR
              POP  AF             ;REFRESH TARGET$ LEN COUNTER
              JR   LOOKLP


FOUND:        POP  HL             ;SEARCH$ PTR
              POP  HL             ;T$ LEN
              POP  HL             ;BYTES TO CHECK
              AND A
              SBC  HL,BC          ;SBC BYTES TO CHECK, BYTES LEFT TO CHECK
                                  ;TO GET BYTES CHECKED
              POP  BC             ;START POSN
              ADD  HL,BC
              LD   B,H
              LD   C,L
              RET                 ;BC=TARGET$ POSN IN SEARCH$. NC


;ENTRY IF 1ST CHAR NEVER FOUND - HL IS PAST LAST POSN LOOKED AT

NOTFND0:      POP  AF             ;T$ LEN
;              EX AF,AF'
              POP  AF             ;JUNK BYTES TO CHECK

NOTFND2:      POP  DE             ;START POSN

NOTFND3:      LD   BC,&0000
              SCF
              RET


                                 ;LET/DEFAULT, LABEL, RUN, CLEAR, S16OP, SYNTAX
                                 ;SRS, DISPLAY, SCREEN, SETSV
