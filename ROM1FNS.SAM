;ROM1FNS.SAM. CODE, LEN, INKEY$, BUTTON, SVAR, CHR$, BIN$, HEX$, MEM$, STR$,
;CONCAT, SQR, ABS, NEGATE, SGN, INT, TRUNCATE, ATTR, POINT
;*******************************************************************************

FPVAL:     DB &3E            ;'LD A,&AF'

FPVALS:    XOR A
           PUSH IX           ;** BUG FIX
           LD BC,(CHADP-1)   ;B=CHADP
           PUSH BC
           LD HL,(CHAD)
           PUSH HL
           AND A
           PUSH AF           ;A=0 IF VAL$, NZ IF VAL
         ;  CALL UNSTKPRT     ;DE=START, BC=LEN, A=PORT
           CALL GETSTRING
           INC BC            ;LEN OF AT LEAST 1; ROOM FOR TERMINATOR
          ; LD (BCSTORE),BC
;          CALL R1OFFCL      ;**
           CALL SCOPYWK        ;COPY STRING PLUS 1 BYTE OF JUNK TO WKSPACE
       ;    DEC HL           ;TERMINATE WITH 0DH
      ;     LD (HL),&0D       ;TERMINATE STRING COPY IN WKSPACE
           LD (CHAD),DE
           IN A,(URPORT)
           LD (CHADP),A
           PUSH DE
           CALL R1OFFCL
           DW TOKDE         ;TOKENISE FROM DE ON
           POP HL
           LD (CHAD),HL
           PUSH HL
           LD HL,FLAGS
           RES 7,(HL)        ;'SYNTAX CHECK' SO FP FORMS INSERTED
           CALL EXPTEXPR     ;RETURN WITH Z FOR STRING, NZ FOR NUM.
           EX AF,AF'
           LD A,(HL)         ;TERMINATOR
           CP &0D
           JR NZ,VALNONS     ;CHECK FOR TERMINATOR

           POP HL            ;STRING START IN WKSPACE
           POP AF            ;VAL/VAL$ FLAG
           JR NZ,FPVAL2      ;JR IF VAL

           EX AF,AF'
           JR Z,FPVALOK      ;IF VAL$, STRING RESULT IS OK. ELSE EX AF, AF'
                             ;GIVES Z, THEN ERROR
FPVAL2:    EX AF,AF'
           JR Z,VALNONS      ;IF VAL, STRING EXPR IS AN ERROR

FPVALOK:   LD (CHAD),HL
           LD HL,FLAGS
           SET 7,(HL)        ;'RUNNING'
           CALL SCANNING     ;GET EXPR TO FPC STACK
           POP HL
           LD (CHAD),HL      ;ORIGINAL CHAD
           POP AF
           LD (CHADP),A
           CALL SELURPG
           POP IX            ;** BUG FIX

SETUPDE:   LD DE,(STKEND)
           RET

VALNONS:   RST &08
           DB 29

FPDVAR:    RST &08           ;LET DOS STACK ADDR OF ITS VARS
           DB DVHK
           DB &21            ;'JR+2'

FPEOF:     RST &08
           DB EOFHK          ;DOS EOF
           DB &21            ;'JR+2'

FPPTR:     RST &08
           DB PTRHK          ;DOS PTR
           JR SUDH

IMPATHS:   CALL SABORTER

           RST &08
           DB PATHHK

SUDH:      JR SETUPDE


IMRND:     RST &20           ;SKIP 'RND'
           CALL RUNFLG
           JR NC,IMRND4      ;JR IF NOT RUNNING

           LD B,0
           LD HL,(SEED)
           LD E,&FD
           LD D,L
           LD A,H
           ADD HL,HL
           SBC A,B
           EX DE,HL
           SBC HL,DE
           SBC A,B
           LD C,A
           SBC HL,BC
           JR NC,IMRND1

           INC HL

IMRND1:    LD (SEED),HL
           CALL STACKHL

           DB CALC
           DB RESTACK        ;EXP WILL BE 00 IF ZERO, ELSE 81-90H
           DB EXIT

           LD A,(HL)
           SUB &10           ;DIVIDE BY 65536
           JR C,IMRND4       ;LEAVE IT ALONE IF ZERO

           LD (HL),A         ;NEW EXP

IMRND4:    RST &18
           CP "("
           RET NZ

           CALL SEX1NUMCB    ;SKIP, GET 'N)', PASS CLOSING BRACKET
           RET NC            ;RET IF NOT RUNNING

           DB CALC           ;RND,PARAM
           DB STKONE         ;RND,P,1
           DB ADDN           ;RND,P+1
           DB MULT           ;RND*(P+1)
           DB TRUNC
           DB EXIT2          ;EXIT WITH INTEGER BETWEEN 0 AND PARAM

;ATTR(L,C)

IMATTR:    CALL EXB2NUMB  ;CHECK (X,Y). CY IF RUNNING
           RET NC

           LD HL,&1820       ;LINE/COL LIMITS (PLUS 1)
           CALL GETCP        ;GET CHAR POSITION WITHIN LIMITS, IN DE
           LD A,(MODE)
           CP 2
           JP NC,INVMERR      ;INVALID MODE UNLESS 0 OR 1

           AND A
           JR Z,DOATT2       ;JR IF MODE 0

           CALL M1DEADDR     ;GET CHAR ADDR
           LD A,D
           ADD A,&20         ;GET ATTR ADDR (ADD 8K)
           JR DOATT3

DOATT2:    CALL M0DEADDR
           LD A,D
           RRCA
           RRCA
           RRCA
           AND &03
           OR &98            ;FORM ATTR ADDR IN AE

DOATT3:    LD D,A
           EX DE,HL
           CALL SREAD
           JR POATC

;POINT(X,Y)

IMPOINT:   CALL EXB2NUMB  ;CHECK (X,Y). CY IF RUNNING
           RET NC

           CALL GTFCOORDS    ;GET COORDS IN B,C OR B,HL (THIN) AND CY
           CALL ANYPIXAD     ;GET ADDRESS IN HL, PIXEL OFFSET IN A
           INC A
           LD B,A            ;B=X MOD 8+1
           CALL SREAD
           LD E,A            ;E=SCREEN BYTE
           LD D,&01          ;MODE 0/1 MASK
           LD A,(MODE)
           CP 2
           JR C,DPOINTC      ;JR IF MODE 0/1

           LD D,&0F          ;MODE 3 MASK
           JR NZ,M3POINT     ;JR IF MODE 3

           LD D,3            ;MODE 2 MASK
           LD A,E
           RLA
           RR E
           XOR E
           AND &AA
           XOR E
           LD E,A            ;SWAP ODD/EVEN BITS IN MODE 2

           LD A,(THFATT)
           AND A
           JR NZ,M3POINT     ;DO MODE 3 POINT IF FATPIX 1

           LD A,B
           DEC A
           AND D             ;AND 3
           INC A
           ADD A,A           ;IF THIN PIX, A=2/4/6/8 FOR PIX 0/1/2/3
           LD B,A

DPOINTC:   LD A,E

POINLP:    RLCA
           DJNZ POINLP

           JR M3ODPT

M3POINT:   LD A,E
           LD B,4
           BIT 0,C           ;SEE IF X IS ODD
           JR Z,POINLP

M3ODPT:    AND D

POATC:     CALL RCURP
           JR STKAB          ;ATTR OR POINT


;GET CHAR POSITION LESS THAN LIMITS H/L (LINE/COL) IN DE (LINE/COL)

GETCP:     PUSH HL           ;LINE/COL MAX
           CALL GETBYTE      ;COL
           POP HL
           CP L
           JR NC,OSERR

           PUSH BC
           PUSH HL
           CALL GETBYTE      ;LINE
           POP HL
           POP DE
           LD D,A
           CP H
           RET C

OSERR:     RST &08
           DB 32             ;'Off screen'


;*******************************************************************************
;INKEY$ #N

FPINKEY:   LD DE,&1100+30    ;LIMIT <17
           CALL LIMBYTE

           LD HL,(CURCHL)
           PUSH HL
           CALL SETSTRM      ;SET STREAM 'A'
           CALL INPUTAD      ;SCAN INPUT STREAM WITHOUT WAITING
           POP HL            ;PREV. CHANNEL
           PUSH AF           ;INKEY VALUE
           CALL CHANFLAG     ;RESTORE ORIG CHANNEL
           POP AF

FPINKEN:   LD D,A
           JR C,CHRINKC      ;IF GOT KEY, D=BYTE. COPY TO WKSPACE

           XOR A             ;IF NO INPUT - NULL STRING
           JR STKAB


;MOUSE BUTTON N (1/2/3) RETURNS 1 IF PRESSED, 0 IF NOT

FPBUTTON:  LD DE,&0400+30    ;LIMIT TO 0-3
           CALL LIMBYTE
           CP 3              ;CY IF 0/1/2, NC IF 3
           SBC A,&FF         ;00,01,02,04
           JR NZ,FPBT2

           CPL               ;'BUTTON 0' TEST ALL 3

FPBT2:     LD HL,BUTSTAT     ;HAS BITS 2-0 SET FOR BUTTONS 3-1
           AND (HL)
           JR Z,STKAB

           LD A,1

STKAB:     JP STACKA

;SVAR JUST ADDS BASE ADDR OF SYSTEM VARS TO ARGUMENT

FPSVAR:    DB CALC
           DB LKADDRW
           DW VVAR2
           DB ADDN
           DB EXIT2

;*******************************************************************************
;CHR$ - CREATE 1 CHAR STRING IN WKSPACE, PARAMS ON FPCS

FPCHRS:    CALL GETBYTE      ;B=0, L=BYTE
           LD D,L

CHRINKC:   LD BC,1           ;LEN=1

INKYEN:    LD HL,TEMPW1+1
           LD (HL),E
           DEC HL
           LD (HL),D
           JP CWKSTK         ;COPY BC FROM (HL) TO WKSPACE, PARAMS TO FPCS
                             ;EXITS WITH DE=STKEND

;*******************************************************************************
;BIN$ N. RETURN BINARY STRING 8 OR 16 DIGITS LONG

FPBINS:    CALL GETINT
           EX DE,HL          ;INT IN DE
           LD A,D
           LD C,8            ;ASSUME LEN 8 FOR RESULT STRING
           AND A
           JR Z,BINS2

           LD C,16           ;USE 16 IF NEEDED

BINS2:     LD HL,MEMVAL+16   ;GET HL POINTING TEMP $ BUFFER IN MEMS
           LD B,C

BINSLP:    DEC HL
           SRL D
           RR E              ;SHIFT LS BITS OUT FIRST
           LD A,(BIN1DIG)
           JR C,BINS4        ;LEAVE IT IF CY

           LD A,(BIN0DIG)

BINS4:     LD (HL),A
           DJNZ BINSLP
                             ;HL=START, BC=LEN
BCWKHP:    JP CWKSTK         ;COPY BC FROM (HL) TO WKSPACE, PARAMS TO FPCS

;*******************************************************************************
;HEX$ N. RETURN HEX VERSION OF N, 2, 4 OR 6 DIGITS LONG, ACCORDING TO MAGNITUDE

FPHEXS:    CALL UNSTLEN      ;ABC=PAGE/'ADDR' **
           RRCA
           RRCA              ;LS 2 BITS TO POSN 7,6
           LD H,A
           XOR B
           AND &C0
           XOR B
           LD B,A            ;HBC=20-BIT NUMBER
           LD E,0            ;INIT DIGIT COUNTER
           OR H
           JR Z,HEX2DIG      ;JR IF MS 2 BYTES (OF 3)=0

           LD A,H
           AND &0F
           JR Z,HEX4DIG      ;JR IF MSB=0

           EX AF,AF'
           LD A,B
           EX AF,AF'         ;SAVE MIDDLE BYTE IN A'
           LD B,A            ;B=MS BYTE
           CALL HEXASCSR
           EX AF,AF'
           LD B,A            ;MIDDLE BYTE

HEX4DIG:   CALL HEXASCSR     ;DERIVE 2 DIGITS FROM B, STACK THEM

HEX2DIG:   LD B,C            ;LS BYTE
           CALL HEXASCSR
           LD C,E
           LD B,0
           LD HL,MEMVAL+6    ;TEMP BUFFER
           LD B,C

HEXPUTLP:  DEC HL
           POP AF            ;UNSTACK AN ASCII DIGIT
           LD (HL),A
           DJNZ HEXPUTLP
                             ;HL=START-1, BC=LEN
           JR BCWKHP         ;STACK STRING FROM HL ON FPCS


;HEX$ ASCII SR TO CONVERT VALUE IN B TO 2 ASCII HEX DIGITS ON STACK

HEXASCSR:  INC E
           INC E             ;COUNT OF DIGITS ON STACK=COUNT+2
           LD A,B
           LD D,2            ;2 DIGITS
           POP HL            ;RET ADDR
           RRCA              ;MS NIBBLE FIRST
           RRCA
           RRCA
           RRCA

HEXSRLP:   AND &0F
           CP &0A
           SBC A,&69         ;
           DAA               ;A STRANGE TWIDDLE THAT GIVES THE RIGHT ANSWER!
           PUSH AF           ;SAVE ASCII 0-9, A-F
           LD A,B
           DEC D
           JR NZ,HEXSRLP

           JP (HL)           ;RETURN

;*******************************************************************************
;PT 2 OF MEMORY$

MEMRYSP2:  DB CALC
           DB SWOP           ;N2,N1
           DB STO0
           DB SUBN           ;N2-N1
           DB STKONE         ;ALLOW FOR INCLUSIVE CHAR.
           DB ADDN           ;LEN
           DB DUP
           DB LESS0          ;LEN, 1/0
           DB JPFALSE        ;JP IF LEN <0
           DB &03

           DB DROP
           DB STKZERO        ;NULL STRING IF LEN <0

           DB RCL0           ;LEN,N1
           DB EXIT

           CALL UNSTLEN      ;GET AHL=START
           SET 7,H           ;PAGE FORM
           DEC A
           PUSH AF
           PUSH HL
           CALL GETINT       ;GET BC=LEN
           POP DE            ;ADDR
           POP AF
           JP STKSTOS

;*******************************************************************************
;S1+S2. PRODUCE A CONCATENATED STRING IN WKSPACE, PARAMS ON FPCS
;BINARY - ALTHOUGH ONLY RELEVANT FOR ENTRY DE

FPCONCAT:  IN A,(URPORT)
           PUSH AF
           PUSH DE           ;S2 PTR WILL BE NEW STKEND
           CALL STKFETCH     ;ADE=S2 START, BC=S2 LEN
           PUSH AF
           PUSH DE
           PUSH BC           ;S2 LEN
           PUSH BC
           CALL STKFETCH
           POP HL            ;S2 LEN
           ADD HL,BC         ;TOTAL LEN
           JP C,STLERR       ;** BUG FIX

           PUSH AF           ;PAGE OF S1
           PUSH DE           ;START OF S1
           PUSH BC           ;LEN OF S1
           LD B,H
           LD C,L            ;BC=TOTAL LEN
           CALL WKROOM       ;GET DE=START, BC=TOTAL LEN, PAGED IN
           CALL STKSTOREP    ;PARAMS OF STRING TO BE CREATED TO FPCS

           POP BC            ;LEN OF S1
           CALL SPLITBC      ;SET UP PAGCOUNT/MODCOUNT
           IN A,(251)
           LD C,A            ;DEST=CDE
           POP HL            ;START OF S1
           POP AF            ;PAGE OF S1
           CALL FARLDIR
           POP BC            ;LEN OF S2
           CALL SPLITBC
           POP DE            ;START OF S2
           POP HL            ;H=PAGE OF S2
           SCF
           CALL FARLDIR2     ;ENTRY PT 2 BECAUSE TEMPW1 AND TEMPB2 = DEST
           POP DE            ;NEW STKEND
           JP PPORT

;AMPERSAND-PREFACED HEX, E.G. &FC0D OR 12345H (6 DIGITS OK)
;VALUE EVALUATED DURING SYNTAX CHECK AND INSERTED AS A 5-BYTE FORM.

AMPERSAND: XOR A
           LD H,A
           LD L,A            ;INIT RESULT TO ZERO
           EX AF,AF'         ; IN A'HL

AMPDILP:   EX DE,HL
           RST &20
           EX DE,HL          ;HL=RESULT
           SUB &30           ;NUMS NOW 00-09, LETS 11-2A, 31-4A
           JR C,AMPEND

           CP &0A
           JR C,AMPVALID     ;JR IF DIGIT

           ADD A,&30
           OR &20            ;LETS NOW 61-7A
           CP "a"
           JR C,AMPEND

           CP "g"
           JR NC,AMPEND

           SUB &57           ;a-f->0A-0F

AMPVALID:  LD B,4
           EX AF,AF'

AMPERLP:   ADD HL,HL
           RLA
           JP C,NTLERR       ;NUM. TOO LARGE (ONLY SEE MSG WITH EG VAL("&"+A$))

           DJNZ AMPERLP

           EX AF,AF'
           OR L
           LD L,A            ;ADD IN NEW HEX DIGIT
           JR AMPDILP

AMPEND:    LD B,H
           LD C,L
           EX AF,AF'
           AND A
           JR Z,STACKBCH     ;JR IF SMALL INTEGER

           LD B,&98          ;EXP FOR FF FF FF

AMPALLP:   BIT 7,A
           JR NZ,AMPFP

           ADD HL,HL         ;ALIGN MANTISSA IN AHL
           RLA
           DJNZ AMPALLP      ;B NEVER HITS ZERO!

AMPFP:     AND &7F           ;+VE SIGN BIT
           LD E,A
           LD D,H
           LD C,L
           LD A,B            ;EXP
           LD B,0           ;FP NUM IN A E D C B
           JP STKSTORE

FPUSRS:    CALL R1OFFJP      ;JP TO MAIN ROUTINE IN ROM 0 WITH ROM1 OFF
           DW R0USRS

FPUSR:     CALL R1OFFJP
           DW R0USR

;UNARY
FPPEEK:    CALL PDPSUBR      ;GET HL=ADDR (PAGED IN), A=ORIG URPORT
           LD D,A
           XOR A
           JR FPPDPC

;UNARY
FPDPEEK:   IN A,(URPORT)
           PUSH AF
           CALL NPDPS        ;GET HL=ADDR
           INC HL
           CALL R1OFRD
           DEC HL
           POP DE

FPPDPC:    LD B,A
           CALL R1OFRD
           LD C,A
           LD A,D

OSBC:      OUT (251),A

STACKBCH:  JP STACKBC

;*******************************************************************************
;TRUNC$ - RETURN STRING STRIPPED OF TRAILING SPACES (E.G. FROM ARRAYS)
; E.G. LET A$=TRUNC$ B$(34)
;ENTRY WITH STRING ON FPCS

FPTRUSTR:  CALL SBUFFET      ;DE=START, A AND BC=LEN, PAGING UNCHANGED
                             ;LEN 1-255
           LD H,D
           LD L,E
           ADD HL,BC         ;PT PAST END OF STRING
           LD A," "

TRUNCSLP:  DEC HL
           CP (HL)
           JR NZ,FPSTRS2     ;JR WITH DE=START, BC=LEN

           DEC C
           JR NZ,TRUNCSLP    ;TRUNC$ OF ALL-SPACES STRING=NULL STRING

           JR STACKBCH

;STR$
FPSTRS:    CALL PFSTRS       ;GET NUMBER AS BC DIGITS AT (DE) IN COMMON MEM

;FROM TRUNC$

FPSTRS2:   EX DE,HL
           JP CWKSTK

;CODE RETURNS ASCII CODE OF FIRST CHAR OF STRING ON FPCS, OR 0 IF STRING IS NULL

FPCODE:    IN A,(251)
           PUSH AF
           CALL GETSTRING    ;UNSTACK STRING. DE=START, BC=LEN, PAGED IN
           LD A,B
           OR C
           JR Z,FPCODE2      ;JR IF NUL STRING

           LD A,(DE)
           LD C,A
           LD B,0

FPCODE2:   POP AF
           JR OSBC


;LEN RETURNS STRING LENGTH

FPLEN:     CALL STKFETCH     ;BC=LEN, NO PAGING
           JR STACKBCH

;SQUARE ROOT (W.E. THOMSON)

FPSQR:     DB CALC
           DB RESTACK        ;USE FULL F.P. FORMS
           DB STO0
           DB EXIT

           LD A,(HL)
           AND A
           RET Z             ;RET IF SQR(0) WITH ZERO ON FPCS, DE=STKEND

           ADD A,&80         ;GET TRUE EXPONENT. CY IF WAS >=80H
           RRA               ;/2, WITH BIT 7 AS ORIGINAL
           LD (HL),A
           INC HL
           LD A,(HL)         ;FETCH SGN BIT
           RLA
           JP C,INVARG       ;ERROR IF SQR OF -VE NUMBER

           LD (HL),&7F       ;MANTISSA STARTS AT ABOUT ONE
           LD B,5            ;5 ITERATIONS

SQURLP:    DB CALC
           DB DUP            ;X,X
           DB RCL0           ;X,X,N
           DB SWOP           ;X,N,X
           DB DIVN           ;X,N/X
           DB ADDN           ;X+N/X
           DB EXIT

           DEC (HL)          ;DEC EXPONENT (HALVE VALUE)
           DJNZ SQURLP

           RET               ;DE=STKEND

;*******************************************************************************
;ABS FPCS TOP ENTRY - UNARY FPCS FUNCTION.

FPABS:     LD A,(HL)
           INC HL
           AND A
           JR NZ,FPABS2      ;JR IF FP

           LD A,(HL)         ;SGN BYTE
           INC A
           RET NZ            ;RET IF SIGN WAS 0 (+VE), OK
                             ;ELSE SIGN WAS FF, NOW A=0
           JR NEGABSC

FPABS2:    RES 7,(HL)        ;SIGN=POS.
           RET


;*******************************************************************************
;NEGATE FPCS TOP ENTRY. UNARY FPCS FUNCTION.

FPNEGAT:   LD A,(HL)
           INC HL
           AND A
           JR NZ,FPNEGAT2    ;JR IF FP

           INC HL
           OR (HL)
           INC HL
           OR (HL)           ;TEST FOR INTEGER=0
           RET Z             ;DO NOTHING IF SO

           DEC HL
           DEC HL
           LD A,(HL)         ;SGN
           CPL

NEGABSC:   LD (HL),A         ;REVERSE SIGN (OR MAKE +VE IF ABS)
           INC HL
           LD A,(HL)
           CPL
           LD C,A
           INC HL
           LD A,(HL)
           CPL
           LD B,A
           INC BC            ;NEGATE INTEGER
           LD (HL),B
           DEC HL
           LD (HL),C         ;LOAD IT BACK
           RET

FPNEGAT2:  LD A,&80
           XOR (HL)          ;REVERSE SIGN BIT
           LD (HL),A
           RET

;*******************************************************************************
;RETURN SGN N1 (-1, 0 OR 1). UNARY

FPSGN:     CALL TSTZERO2
           EX DE,HL
           RET Z             ;SGN ZERO=0

           PUSH DE
           INC HL
           LD A,(HL)
           RLA
           DEC HL

           SBC A,A           ;-VE=FF, +VE=00
           LD C,A
           LD DE,&0001
           JP STOREI         ;STORE SIGNED INTEGER, POP DE, RET

;*******************************************************************************
;INT - EQUIVALENT TO TRUNCATE (ZERO BITS AFTER BINARY POINT) IF +VE
;FOR -VE, TRUNC E.G. -5.9=-5.0, SO SUB 1 TO GET -6.9 -> 6.0

FPINT:     LD A,(HL)
           AND A
           RET Z             ;RET IF INTEGER ALREADY (OR ZERO)

           DB CALC
           DB DUP            ;N1,N1
           DB GRTR0          ;N1,(0 OR 1)
           DB JPTRUE         ;JP IF +VE, FPINTP
           DB &0B            ;** BUG FIX

           DB DUP            ;N,N
           DB TRUNC          ;N,TRUNC N
           DB DUP            ;N,TRUNC N,TRUNC N
           DB SWOP13         ;TRUNC N,TRUNC N,N
           DB SUBN           ;TRUNC N,0 IF N WAS A WHOLE NUMBER
           DB JPFALSE        ;TRUNC N. JP IF VALUE WAS INTEGER, TO EXIT2
           DB &03

           DB STKONE         ;TRUNC N,1
           DB SUBN           ;TRUNC N-1
           DB EXIT2

FPINTP:    DB TRUNC          ;INT(N)
           DB EXIT2

;*******************************************************************************
;TRUNCATE - ZERO THE BITS AFTER BINARY POINT, FORCE INTEGER FORM IF POSSIBLE.
;UNARY

FPTRUNCT:  LD A,(HL)
           AND A
           RET Z             ;RET IF INTEGER

           LD B,8            ;FOR LATER
           CP &81
           JR C,TRUNC0       ;JR IF LESS THAN 1

           CP &91
           JR NC,TRUNCLG     ;JR IF >=65536

           PUSH DE
           INC HL
           LD D,(HL)
           INC HL
           LD E,(HL)
           DEC HL
           DEC HL
           LD C,&FF
           BIT 7,D
           JR NZ,TRUNCI2     ;JR IF -VE WITH C=SIGN BYTE FOR SMALL INTEGER

           SET 7,D           ;TRUE NUMERIC BIT
           INC C             ;SIGN BYTE FOR +VE=0

TRUNCI2:   CPL
           ADD A,&91
           SUB B             ;B=8
           ADD A,B
           JR C,TRUNCI3

           LD E,D
           LD D,0
           SUB B

TRUNCI3:   JR Z,TRUNCI5

           LD B,A

TRUNCI4:   SRL D
           RR E
           DJNZ TRUNCI4

TRUNCI5:   JP STOREI

TRUNCLG:   ADD A,&60
           RET P

           CPL
           INC A             ;NEG
           DB &22            ;"JR+2"

;NIL-BYTES.

TRUNC0:    LD A,&28          ;ZERO 40 BITS - ALL OF THEM!

NILBYTES:  PUSH DE
           EX DE,HL          ;PT TO AFTER LAST BYTE OF NUMBER
           DB &22            ;"JR+2"

NILBYLP:   LD (HL),0

NILBYT2:   DEC HL
           SUB B             ;B=8
           JR NC,NILBYLP

           ADD A,B           ;CY SET
           JR Z,NILBYEND

           LD B,A
           SBC A,A           ;A=FF

NILBMASK:  ADD A,A           ;MASK FOR RHS BITS PRODUCED
           DJNZ NILBMASK

           AND (HL)
           LD (HL),A         ;DO THE MASKING

NILBYEND:  EX DE,HL
           POP DE
           RET


;*******************************************************************************
;UDG A$. UDG ADDR. E.G. UDG "A". TRANSLATOR CHANGES USR "A" TO UDG "(CHR$ 144)"
;UNARY. ENTRY WITH STRING ON FPCS AT (HL)

FPUDG:     CALL SBUFFET
           DEC A

IAHOP:     JP NZ,INVARG      ;LEN MUST BE 1

           LD A,(DE)         ;READ CHAR FROM BUFFER
           CP &20
           JR C,IAHOP

           LD HL,(CHARS)
           CP &80
           JR C,FPUDG3       ;JR IF RANGE 20H-7FH

           LD HL,(UDG)
           LD DE,-144*8
           ADD HL,DE         ;ALLOW FOR UDG VAR. POINTING TO CHR$ 144
           CP 169
           JR C,FPUDG3       ;JR IF CHR$ 80H-A8H (LOW UDGS)

           SUB 169
           LD HL,(HUDG)

FPUDG3:    EX DE,HL
           LD L,A
           LD H,0
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,DE
           JP STACKHL        ;STACK CHAR PATTERN ADDR
