;EVAL.SAM - SAM EXPRESSION EVALUATOR (ROM0). EXITS WITH PAGING UNCHANGED.
;ENTRY: CHAD PTS TO FIRST CHAR. EXIT: RESULT ON FPCS IF RUNNING, ELSE SYNTAX
;CHECKED AND 5-BYTES INSERTED IF NEEDED. BIT 6,(FLAGS)=0 IF STRING RESULT,
;ELSE NUMERIC. CHAD PTS TO CHAR THAT CANNOT BE PART OF EXPR. A=CHAR, HL=CHAD
;*******************************************************************************

SCANNING:     CALL R1OFFCL
              DW SCANSR
              LD C,A         ;CURRENT CHAR
              LD A,(FLAGS)
              RET

SCANSR:       LD D,0         ;PRIORITY "STOPPER"
              RST &18        ;GET FIRST CHAR. HL=CHAD
              DB &FE         ;"JR+1"

SCANPLP:      RST &20        ;NEXT CHAR

              PUSH DE        ;PRIORITY/CODE

;FROM UNARY PLUS WITH HL=CHAD

SCANLP:       LD E,A
              AND &DF        ;LETTERS BECOME UPPER CASE

              CP &5B         ;"Z"+1
              JR NC,ABOVLETS

              CP "A"
              JP C,BELOWLETS ;JR UNLESS WE HAVE A LETTER

;EVALUATE A VARIABLE

SLETTER:      LD A,(DEFADD+1)   ;PTS PAST DEF FN "(" IF FN BEING EVALED.
              AND A
              JR NZ,SLLKFV      ;CHECK DEF FN BRACKETS FOR VAR IF DEFADD-HI NZ

SLET1:        LD A,(HL)
              CALL LKVARS2
              JP Z,VNFERR    ;ERROR IF NOT FOUND

              LD A,(FLAGS)
              ADD A,A        ;CY IF RUNNING
              JP P,SLET2     ;JP IF STRING

              BIT 5,C
              JR Z,SLET3     ;JR IF NOT (NUMERIC) ARRAY

SLET2:        CALL STKVAR2   ;IF STRING, STACK START AND LEN, IF N ARRAY
                             ;GET HL=START ADDR OF ELEMENT
              LD A,(FLAGS)
              ADD A,A

              JP P,SCONT1    ;JR IF STRING - CHECK FOR SLICER, THEN OPERATOR

SLET3:        CALL C,HLTOFPCS ;STACK NUM IF RUNNING

SLET4:        CALL SELCHADP
              RST &18
              JP OPERATOR    ;AN OPERATOR OR A TERMINATOR MUST FOLLOW

SLLKFV:       CALL LKFNVAR
              JR NC,SLET1    ;JR IF NOT FOUND OR SYNTAX TIME

              CP "$"
              JP Z,SCONT2

              JR SLET4

;*******************************************************************************

ABOVLETS:     INC E
              JR NZ,EVNONSE  ;ERROR UNLESS FF FUNCT LEADER FOUND

              INC HL
              LD A,(HL)      ;GET FUNCTION CODE
              SUB &1A        ;ADJUST 3B-83H TO 21H-69H
              LD E,A
              LD (CHAD),HL   ;SKIP "FF"

              LD HL,(EVALUV)
              INC H
              DEC H
              CALL NZ,HLJUMP ;IF VECTORED JUMP WITH A=FN CODE.

              CP SIN
              JR C,IMMEDCODES

              LD D,&CF       ;"PRIORITY 0F, N ARG, N RESULT"
              CP EOF+3
              JR C,SCANPLP   ;JR IF SIN-EOF/PTR/POS

              CP NOT+1
              JR NC,EVNONSE  ;RANGE NOW UDG-NOT

SCANUMEN:     LD D,0
              LD HL,FNPRIORT-UDGA
              ADD HL,DE
              LD D,(HL)      ;FETCH PRIORITY AND INPUT/OUTPUT TYPE. (BIT 7=1/0
                             ;FOR N/$ RESULT, BIT 6=DITTO FOR ARGUMENT)
              JR SCANPLP     ;STACK DE, GET NEXT CHAR, LOOP

;*******************************************************************************
;INKEY$ - THIS IS MAIN (NON-FPC) ROUTINE

IMINKEYS:  RST &20           ;SKIP "INKEY"
           CP "#"
           LD E,INKEY        ;FPC INKEY$ CODE
           POP BC            ;RET ADDR (STRCONT)
           JR Z,SCANUMEN     ;STREAM VERSION HANDLED BY FPC. "#" WILL BE SKIPPED

           PUSH BC
           CALL ABORTER      ;(WE CANNOT USE THE NORMAL INPUT STREAM FOR INKEY$
                             ;ON ITS OWN, SINCE THAT WOULD NOT CAUSE A KEYSCAN,
                             ;BUT WOULD JUST FETCH LAST-K

           CALL READKEY      ;RETURNS KEY CODE IN A AND CY IF OK
           RST &30
           DW FPINKEN-&8000  ;MAKE 1-CHAR STRING AND STACK PARAMS

;*******************************************************************************
;SOME FUNCTIONS HAVE TO BE HANDLED AT ONCE BECAUSE THEY HAVE NO ARGUMENTS
;(LIKE PI) OR BECAUSE THEY HAVE SEVERAL ARGUMENTS IN BRACKETS (LIKE POINT).

;NUM. RESULT: PI, RND, POINT, FREE, LENGTH, ITEM, ATTR, FN, BIN, HIMEM, XMOUSE,
;YMOUSE, XPEN, YPEN, INARRAY, INSTR.

;STR. RESULT: INKEY$, SCREEN$, MEMORY$, CHAR$, PATH$, STRING$, USING$, SHIFT$


IMMEDCODES:   SUB PI
              JR C,EVNONSE   ;RANGE NOW PI-SHIFT$

              ADD A,A
              LD E,A
              LD D,0
              LD HL,IMFNATAB
              ADD HL,DE
              LD C,(HL)
              INC HL
              LD B,(HL)
              LD HL,NUMCONT
              CP 0+(INSTR-PI)*2+1
              JR C,IMMEDNUM

              LD HL,STRCONT

IMMEDNUM:     PUSH HL
              BIT 7,B
              JP NZ,R1ONCLBC

              PUSH BC
              RET            ;JP TO BC

STRCONT:      CALL SLLPEX    ;SIGNAL STRING RESULT
              JR SCONT2

;*******************************************************************************

BELOWLETS:    LD A,E
              CP "0"
              JP C,BELOWNUM

              CP &3A
              JR C,SDECIMAL

EVNONSE:      RST &08
              DB 29          ;"NONSENSE"

;*******************************************************************************
;HANDLE LITERAL NUMBER (0-9, DECIMAL PT., BIN OR AMPERSAND

IMBIN:        POP AF         ;JUNK RET TO IMMEDNUM+3
              RST &18        ;GET HL=CHAD

SDECIMAL:     LD A,(FLAGS)
              RLA
              JR NC,INSERT5B ;JR AND INSERT THE INVISIBLE FORM IF SYNTAX TIME

LK0ELP:       INC HL
              LD A,(HL)
              CP &0E
              JR NZ,LK0ELP ;LOOP TILL NUMBER MARKER FND.

              INC HL
              LD BC,5
              LD DE,(STKEND)
              LDIR           ;COPY NUMBER TO FPCS
              LD (STKEND),DE

SCHADNUM:     LD (CHAD),HL
              JR NUMCONT

INSERT5B:     CALL CALC5BY   ;FIND 5-BYTE FORM OF DEC, HEX OR BIN NUMBER
              LD HL,(CHAD)
              CALL MAKESIX   ;MAKE 6 SPACES IN ELINE, PLACE 0EH, INC HL
              EX DE,HL
              CALL FDELETE
              LD BC,5        ;HL PTS TO DELETED NUMBER, DE TO ROOM IN ELINE
              LDIR           ;COPY 5 BYTES TO ELINE
              EX DE,HL
              JR SCHADNUM    ;SET NUMERIC, SET CHAD

;*******************************************************************************

SSLICER:      LD HL,FLAGS
              BIT 6,(HL)
              JR NZ,SLOOP    ;NUMBERS CANNOT HAVE A SLICER - TERMINATE

              RST &20        ;SKIP "("
              CALL SLICING
              RST &20
              JR SLSTRLP

;*******************************************************************************

NUMCONT:      LD HL,FLAGS
              SET 6,(HL)     ;SIGNAL "NUMERIC"
              RST &18
              JR OPERATOR

SCONT1:       CALL SELCHADP

SCONT2:       RST &18        ;STRING EXPRESSIONS CAN BE FOLLOWED BY A SLICER
                             ;EG (STR$ 123)(2) SO CHECK FOR THIS BEFORE LOOKING
                             ;FOR AN OPERATOR OR TERMINATOR
SLSTRLP:      CP "("
              JR Z,SSLICER

;DEAL WITH BINARY OPERATORS: +,-,*,/,^,=,>,<,<=,>=,<>,OR,AND,MOD,IDIV,BOR, ETC.

OPERATOR:     LD D,0         ;PRIORITY=0
              INC A
              JR NZ,OPERAT2  ;JR IF NOT FF FN CODE LEADER

              INC HL
              LD A,(HL)      ;FETCH OPERATOR CODE
              SUB MODTOK
              JR C,SLOOP

              CP &0A
              JR NC,SLOOP    ;RANGE NOW 00-09 FOR MOD TO ">="

              LD (CHAD),HL   ;SKIP FF

              ADD A,8        ;MOD TO ">=" BECOME 08-11H
              JR OPERAT3

OPERAT2:      SUB "*"+1      ;MULT IS 0, DIVN IS 5, "<" IS 12H, ">" IS 14H
              JR C,SLOOP     ;JR IF BELOW BINARY OPERATOR RANGE

              CP 4
              JR Z,SLOOP     ;EXCLUDE "."

              CP 6
              JR C,OPERAT3   ;JR IF WITHIN RANGE FOR * TO /

              CP &12
              JR C,SLOOP

              CP &15
              JR C,OPERAT3   ;JR IF "<", "=" OR ">" (NOW 12H-14H)

              CP &34         ;POWER-OF
              LD A,4
              JR NZ,SLOOP

OPERAT3:      LD E,A         ;E=BIN OPERATOR CODE 00-14H
              LD HL,OPPRIORT
              ADD HL,DE
              LD D,(HL)      ;FETCH OPERATOR PRIORITY, OR 0 IF NOT RECOGNISED

SLOOP:        POP BC         ;PREV PRIORITY (B) AND OPERATION CODE (C)
              LD A,B
              SUB D
              AND &10        ;IF PRIORITY NIBBLE IS HIGHER IN D, BIT 4 WILL BE 1
              JR NZ,PRIGRTR  ;JR IF CURRENT PRIORITY HIGHER - WAIT

              OR B           ;ELSE B PRIORITY IS >=D
              JP Z,&0018     ;EXIT IF BOTH PRIORITIES ARE ZERO

              PUSH DE        ;CURRENT PRIORITY/CODE
              LD HL,FLAGS
              LD A,(HL)
              RLA            ;CY IF RUNNING
              JR C,EXECOP    ;PERFORM OPERATION IF RUNNING

              LD A,B         ;PRIORITY CODE OF FN TO BE CHECKED/EXECUTED
              XOR (HL)       ;CHECK THAT FNS THAT WORK ON STRINGS HAVE STRING
                             ;"LAST VALUE"S, AND DITTO FOR NUMERICS. BIT 6 OF
                             ;PRIORITY CODE IS 0 IF FN WORKS ON STRINGS, ELSE 1
              ADD A,A
              JP M,EVNONSE   ;ERROR IF BIT 6,(FLAGS)<>BIT 6 OF FN CODE

              JR CHKEXECC

EXECOP:       PUSH BC
              LD B,C         ;FN CODE TO EXECUTE

              DB CALC
              DB USEB
              DB EXIT

              POP BC
              LD HL,FLAGS

CHKEXECC:     LD A,B         ;PRIORITY CODE OF FN JUST CHECKED/EXECUTED
              POP DE         ;NEXT PRIORITY/CODE
              SET 6,(HL)     ;"LAST VALUE IS NUMERIC"
              RLA            ;BIT 7 OF THE P. CODE IS SET IF ITS RESULT IS A NUM
              JR C,SLOOP     ;JR IF WE SET FLAGS CORRECTLY

              RES 6,(HL)     ;"LAST VALUE IS STRING"
              JR SLOOP

;*******************************************************************************

PRIGRTR:      PUSH BC        ;PREV PRIOR/CODE
              LD A,(FLAGS)
              ADD A,A
              JP M,SCANPLP   ;LOOP IF NUMERIC - I.E. LEAVE TYPE CODE BITS
                             ;ALONE IF A NUMERIC IS FOLLOWED BY A BINARY
                             ;OPERATOR - THEY ARE SET FOR NUMERIC I/P AND O/P

              LD A,E         ;CURRENT CODE
                             ;ALL LEGAL $ BINARY OPS EXCEPT "+" WILL GET
                             ;CODES 7 ABOVE THEIR NUMERIC EQUIVALENTS

              RES 7,D        ;RESULT TYPE WILL BE STRING FOR "+" AND "AND"
              CP &0E         ;CP "AND"  "
              JR Z,SCANPLPH  ;IF "AND" LEAVE "INPUT" BIT AS NUMERIC FOR
                             ;"$ AND N"
              RES 6,D        ;ELSE "INPUT" BIT IS STRING E.G. "$+$", "$<$"
              INC E          ;IF "+", E=2
              CP 1           ;CP "+"
              JR Z,SCANPH2   ;JP WITH STRING INPUT AND OUTPUT FOR "+"

              SET 7,D        ;NUMERIC OUTPUT FOR E.G. "$>$"
              CP &0E         ;CP "AND"
              JP C,NONSENSE  ;E.G. $ MOD $ IS AN ERROR

SCANPLPH:     ADD A,7
              LD E,A

SCANPH2:      JP SCANPLP     ;JP TO STACK DE AND GET NEXT CHAR

;*******************************************************************************
;BINARY OPERATOR PRIORITY TABLE

OPPRIORT:     DB &C8   ;0  2A *
              DB &C6   ;1  2B +
              DB 0     ;2  2C
              DB &C6   ;3  2D -
              DB &CF   ;4  5E TO-POWER-OF
              DB &C8   ;5  2F /
              DB 0     ;6
              DB 0     ;7
              DB &CE   ;8     MOD
              DB &CE   ;9     IDIV
              DB &C2   ;A     BOR
              DB &C2   ;B     BXOR
              DB &C3   ;C     BAND
              DB &C2   ;D  .. OR

              DB &C3   ;E  .. AND  (ADD 7 FOR $ EQUIV. OF "AND" TO ">")
              DB &C5   ;F  .. <>
              DB &C5   ;10 .. <=
              DB &C5   ;11 .. >=
              DB &C5   ;12 3C <
              DB &C5   ;13 3D =
              DB &C5   ;14 3E >

;*******************************************************************************
;UNARY FUNCTION PRIORITY TABLE (FOR MINORITY OF FNS THAT AREN"T PRI. 16,N,N)
;BIT 7=1 IF NUM RESULT. BIT 6=1 IF NUM ARG. BITS 4-0=PRIORITY

FNPRIORT:     DB &8F         ;UDG
              DB &8F         ;NUMBER
              DB &8F         ;LEN
              DB &8F         ;CODE
              DB &0F         ;VAL$
              DB &8F         ;VAL
              DB &0F         ;TRUNC$
              DB &4F         ;CHR$
              DB &4F         ;STR$
              DB &4F         ;BIN$
              DB &4F         ;HEX$
              DB &4F         ;USR$
              DB &4F         ;INKEY$

              DB &C4         ;NOT
              DB &C9         ;NEGATE

;*******************************************************************************
;IMMEDIATE FN ADDRESS TABLE. ALL ARE EVALUATED AT ONCE BECAUSE THEY HAVE NO
;ARGS, OR BRACKETED ARGS, OR #ARG.

IMFNATAB:     DW IMPI        ;NUMERIC RESULT
              DW IMRND
              DW IMPOINT
              DW IMMEM
              DW IMLENGTH
              DW IMITEM
              DW IMATTR
              DW IMFN
              DW IMBIN
              DW IMMOUSEX
              DW IMMOUSEY
              DW IMPENX
              DW IMPENY
              DW IMHIMEM
              DW NONSENSE
              DW IMINSTR

              DW IMINKEYS    ;STRING RESULT
              DW IMSCREENS
              DW IMMEMRYS
              DW NONSENSE
              DW IMPATHS
              DW IMSTRINGS
              DW NONSENSE
              DW NONSENSE

;*******************************************************************************
;BELOW NUMBERS ARE QUOTE, "&", OPEN BRACKET, UNARY PLUS & MINUS, DECIMAL PT.
;(22H,26H,28H,2BH,2DH,2EH)

BELOWNUM:     CP &22
              JR Z,SQUOTE

              CP "&"
              JR Z,SDECIMALH

              CP "("
              JR Z,SBRACKET

              CP "-"
              JR Z,UNARMIN

              CP "."

SDECIMALH:    JP Z,SDECIMAL

              CP "+"
              JP NZ,NONSENSE

UNARPLU:      RST  &20       ;JUST SKIP A UNARY PLUS
              JP SCANLP

UNARMIN:      LD E,NEGATE    ;UNARY MINUS CODE
              JP SCANUMEN

SBRACKET:     RST  &20       ;SKIP "("
              CALL SCANNING
              LD A,C
              CALL INSISCBRK ;INSIST ON ")"
              JP SCONT2

;*******************************************************************************
;PASS PARAMS OF STRING LITERAL TO FPCS IN RUN TIME

SQUOTE:       INC HL         ;SKIP QUOTE
              PUSH HL        ;STRING TEXT START
              LD A,(FLAGS)
              RLA
              EX AF,AF'      ;CY IN F" IF RUNNING
              LD BC,&FFFF    ;INITIALISE LENGTH

QUTSRLP:      LD A,(HL)
              INC HL
              INC BC         ;INC LEN COUNT
              CP &0D
              JP Z,NONSENSE

              CP &22
              JR NZ,QUTSRLP  ;LOOP UNTIL QUOTE FOUND

              POP DE         ;START
              LD A,(HL)
              CP &22         ;CHECK IF DOUBLE QUOTE

              JR Z,SQUOTE2   ;JR IF EMBEDDED QUOTES USED. ELSE STRING IS
                             ;SIMPLE AND CAN STAY IN BASIC LINE.
              LD (CHAD),HL   ;PT PAST CLOSING QUOTE
              EX AF,AF'
              CALL C,STKSTOREP ;STACK PARAMS OF STR IN BASIC LINE, IF RUNNING

STRCONTH:     JP STRCONT

;EMBEDDED QUOTES - HAVE TO COPY STRING TO BUFFER, OMITTING SOME ALT. QUOTES

SQUOTE2:      LD HL,INSTBUF  ;ALLOWS 256 BYTES
              LD C,0
              PUSH HL        ;BUFFER START

SQUCOPY:      LD A,(DE)      ;CHAR FROM BASIC LINE
              INC DE
              CP &22
              JR Z,SQUCO3

SQUCO1:       LD B,A
              EX AF,AF'
              JR NC,SQUCO2

              LD (HL),B
              INC HL

SQUCO2:       EX AF,AF'
              INC C
              JR NZ,SQUCOPY  ;LOOP, COPYING CHARS FROM BASIC LINE TO BUFFER
                             ;(MAX OF 255)
              RST &08
              DB 42          ;"String too long"

SQUCO3:       LD A,(DE)
              INC DE
              CP &22
              JR Z,SQUCO1    ;COPY IN NEXT CHAR IF IT IS A SECOND QUOTE MARK

              LD B,0
              DEC DE         ;PT TO JUST PAST FINAL QUOTE
              LD (CHAD),DE
              POP HL         ;BUFFER START
              EX AF,AF'
              CALL C,CWKSTK  ;IF RUNNING, COPY TO WKSPACE, STACK PARAMS.
              JR STRCONTH

;CALC5BY.SAM
;*******************************************************************************
;CALCULATE A 5-BYTE FORM FOR A DECIMAL, HEX OR BINARY NUMBER
;ENTRY WITH HL AND CHAD PTING TO FIRST CHAR OF NUMBER (&,.,0-9,BIN)
;EXIT WITH VALUE ON FPCS

CALC5BY:      LD A,(HL)
              CP "&"
              JR NZ,NAMP

              RST &30
              DW AMPERSAND-&8000

NAMP:         CP BINTOK
              JP NZ,DECIMAL

              LD BC,0        ;INITIALISE RESULT

NXBINDIG:     RST &20        ;SKIP BIN
              CP "0"
              JR Z,BINDIG

              CP "1"
              SCF
              JP NZ,STACKBC  ;STACK RESULT AS SOON AS NON-1, NON-0 FOUND

BINDIG:       RL C
              RL B
              JR NC,NXBINDIG

              RST &08
              DB 28          ;"Number too large"


;*******************************************************************************
;HANDLE EG 0.123, .123, 1.234, 1E4, 1.23E+4, 7.89E-32, 1.E5

DECIMAL:      CP "."
              JR NZ,DECINT

              RST &20        ;SKIP "."
              CALL NUMERIC
              JP NC,NONSENSE ;INSIST ON E.G. .1 OR .8

              DB CALC
              DB STKZERO     ;INTEGER PART OF A FRACTION IS ZERO
              DB EXIT

              JR CONVFRAC

DECINT:       CALL INTTOFP
              CP "."
              JR NZ,EFORMAT

              RST &20
              CALL NUMERIC
              JR NC,EFORMAT  ;JR IF NOT A DIGIT

CONVFRAC:     DB CALC
              DB STKFONE
              DB STOD0       ;MULTIPLIER (M) STARTS AT 1
              DB EXIT

              RST &18
              JR CONVFRAC2

CONVFRALP:    SUB &30
              LD B,A

              DB CALC
              DB STKBREG
              DB RCL0
              DB STKTEN
              DB DIVN
              DB STO0
              DB MULT
              DB ADDN
              DB EXIT

              RST &20

CONVFRAC2:    CALL NUMERIC
              JR C,CONVFRALP ;JR IF A DIGIT

EFORMAT:      AND &DF
              CP "E"
              RET NZ

              RST &20        ;SKIP "E"
              LD C,"+"
              CP C
              JR Z,GEXSGN1

              CP "-"
              JR NZ,GEXSGN2

              LD C,A

GEXSGN1:      RST &20        ;SKIP +/-

GEXSGN2:      CALL NUMERIC
              JP NC,NONSENSE ;INSIST ON NUMERIC NOW

              PUSH BC        ;C=+/-
              CALL INTTOFP
              CALL FPTOA
              JR C,NTLERR    ;JR IF >255

              RLCA
              JR NC,GEXSGN3   ;JR IF <=127

NTLERR:       RST &08
              DB 28          ;"Number too large"

GEXSGN3:      RRCA
              POP BC
              BIT 1,C
              JR NZ,POFTENH  ;JR IF 0010 1011 (+)

              NEG

POFTENH:      RST &30
              DW POFTEN-&8000 ;MULT FPC LAST VALUE BY E+/-A REGISTER

;*******************************************************************************
;GET VALUE OF ASCII INTEGER IN A AND (CHAD+1...) TO FPCS. ZERO IF NO DIGITS.
;EXIT WITH NC AND A=NON-NUMERIC CHAR

INTTOFP:      LD B,A

              DB CALC
              DB STKZERO     ;TOTAL=0
              DB EXIT

              LD A,B
              JR INTTOFP3

INTTOFPLP:    SUB &30
              LD B,A

              DB CALC        ;B GOES TO BREG
              DB STKTEN
              DB MULT        ;TOTAL=TOTAL*10
              DB STKBREG
              DB ADDN
              DB EXIT

              CALL NXCHAR    ;0074 EQU - NEXT CHAR, DON"T SKIP ANYTHING

INTTOFP3:     CALL NUMERIC
              JR C,INTTOFPLP ;LOOP WHILE NUMERIC ASCII FOUND

              RET
;*******************************************************************************

;USR$. E.G. LET A$=USR$ 12345 OR 18000H

R0USRS:    LD HL,STKSTOS     ;STACKS DE,BC,A
           DB &FD            ;"JR+3"

;USR. E.G. LET X=USR 123456

R0USR:     LD HL,STACKBC

USRCOM:    PUSH HL

;FROM "CALL"

CALLX:     PUSH IX
           CALL PDPSUBR      ;SWITCH ADDR IN. HL=ADDR, A=ORIG URPORT
           LD B,H
           LD C,L
           PUSH AF           ;STACKED IN SECTION B
           LD A,(TEMPB3)     ;JUNK OR NO. OF PARAMS IF CALL
           CALL HLJUMP
           POP AF
           OUT (251),A       ;ORIG URPORT
           POP IX
           RET               ;TO STACKBC OR STKSTOS OR NEXTSTAT (IF CALL)

;*******************************************************************************
;MEMORY$  E.G. MEM$(N1 TO N2) HANDLE READING ROM? HOW?

IMMEMRYS:  CALL SINSISOBRK    ;CHK "("
           CALL EXPT1NUM     ;N1
           CP TOTOK          ;"TO"
           JP NZ,NONSENSE

           CALL SEX1NUMCB    ;SKIP, EXPT "N)", CY IF RUNNING
           RET NC            ;RET IF NOT RUNNING

           RST &30
           DW MEMRYSP2-&8000

;*******************************************************************************
;HIMEM - "RAMTOP"

IMHIMEM:   CALL SABORTER

           LD HL,(RAMTOP)    ;MAINTAINED IN 8000-BFFF FORM,(UNLIKE OLDRT)
           LD A,(RAMTOPP)
           LD B,A

;USED BY TPEEK - ADJUST/STACK BHL

ASBHL:     IN A,(250)
           LD C,A
           LD A,B
           SUB C             ;ADJUST TO RELATIVE PAGE
           JR STKPGFORM

;*******************************************************************************
;MEM - FREE MEMORY.

IMMEM:     CALL SABORTER

           CALL GETROOM      ;AHL=19BIT NUMBER
           JR STK19BIT

;STACK PAGE FORM IN AHL ON FPCS

STKPGFORM: CALL AHLNORM      ;TURN TO 19-BIT NUMBER

STK19BIT:  PUSH AF
           CALL STACKHL
           POP BC

           DB CALC           ;LSW
           DB STKBREG        ;LSW,MSB
           DB STK16K
           DB MULT           ;LSW,MSB*16K
           DB STKHALF
           DB DIVN           ;LSW,MSB*32K
           DB STKHALF
           DB DIVN           ;LSW,MSB*64K
           DB ADDN           ;MSB*64K+LSW
           DB EXIT2

IMMOUSEX:  CALL SABORTER

           LD HL,(MXCRD)
           JP STACKHL

IMMOUSEY:  CALL SABORTER

           LD A,(MYCRD)
           JR STACKAH

IMPENX:    CALL SABORTER

           LD BC,CLUTPORT    ;A8 IS LOW
           IN A,(C)
           RRA
           RRA
           AND &3F
           JR STACKAH

IMPENY:    CALL SABORTER

           LD BC,&0100+CLUTPORT ;A8 HIGH GIVES PEN Y, NOT PEN X
           JR FPIN2

;IN (N1) - IN A,(BC)

FPIN:      CALL GETINT       ;TO BC

FPIN2:     IN A,(C)

STACKAH:   JP STACKA

;*******************************************************************************
;SWOPS. CAN BE CALLED, OR USED AS FPC FUNCTIONS
;EXIT: DE=STKEND
;DIRECT CALLS FROM FOR-NEXT, OPEN, ...

;SWOP TOP AND THIRD ENTRIES

FPSWOP13:  LD C,-10
           JR SWOPCOM1

;SWOP SECOND AND THIRD ENTRIES

FPSWOP23:  LD C,-5
           LD DE,-10
           JR SWOPCOM2

;SWOP TOP AND SECOND ENTRIES

SWOP12:    LD C,-5

SWOPCOM1:  LD DE,-5

SWOPCOM2:  LD B,D            ;B=FF
           LD HL,(STKEND)
           ADD HL,DE
           LD D,H
           LD E,L            ;DE=STKEND-5 (SWOP12 AND SWOP13) OR -10 (SWOP23)
           ADD HL,BC         ;HL=STKEND-10 (SWOP12) OR -15 (SWOP13 AND SWOP23)

;FP "SWOP" ENTERS HERE -
;BINARY OP, SO HL AND DE PT TO STKEND-10 AND STKEND-5

FPSWOP:    LD B,5            ;SWOP 5 BYTES

FPSWOPLP:  LD A,(DE)
           LD C,(HL)
           LD (HL),A
           LD A,C
           LD (DE),A
           INC HL
           INC DE
           DJNZ FPSWOPLP

           LD DE,(STKEND)
           RET

;PI (3.1415 ETC)

IMPI:         CALL SABORTER  ;SKIP "PI", ABORT IF NOT RUNNING

              DB CALC
              DB STKHALFPI
              DB EXIT

              INC (HL)       ;DOUBLE IT
              RET

;*******************************************************************************
;ITEM - RETURN DATA LIST STATUS.
;0=NO DATA LEFT TO READ IN CURRENT DATA STATEMENT
;1=NEXT ITEM IS STRING
;2=NEXT ITEM IS NUMERIC

IMITEM:    CALL SABORTER     ;SKIP "ITEM", ABORT IF NOT RUNNING

           IN A,(URPORT)
           PUSH AF
           CALL ADDRDATA     ;USE DATADD AND DATAPG TO LOOK AT DATA PTR
           LD BC,0
           LD A,(HL)
           CP " "
           JR Z,IMITEM2      ;THERE IS MORE DATA IF DATADD PTS TO A SPACE
                             ;(AFTER "DATA")
           CP ","
           JR NZ,IMITEM3     ;IF NO COMMA, DATA HAS ALL BEEN READ

IMITEM2:   INC C             ;BC=1 ("STRING")
           CALL FORESP       ;SKIP ANYTHING BELOW "!"
           CP &22
           JR Z,IMITEM3      ;END IF QUOTE - STRING

IMITEMLP:  LD A,(HL)
           INC HL
           CALL ALPHANUM
           JR C,IMITEMLP     ;JR IF LETTER OF NUMBER

           CP " "
           JR Z,IMITEMLP     ;SPACE IS ALSO POSSIBLE IN VAR NAMES (_?)

           CP "$"
           JR Z,IMITEM3      ;STRING IF NAME ENDS IN $

           INC C             ;ELSE NUMERIC

IMITEM3:   POP AF
           OUT (URPORT),A
           JP STACKBC
