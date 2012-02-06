;FPCMAIN.SAM
;FLOATING POINT CALCULATOR CODES AND ADDRESSES

;BINARY
FPATAB:       DW FPMULT     ;00 MULT
              DW FPADDN     ;01 ADDN (NUMBERS)
              DW FPCONCAT   ;02 ADDN (STRINGS)
              DW FPSUBN     ;03 SUBN
              DW FPPOWER    ;04 TO-POWER-OF
              DW FPDIVN     ;05 DIVN

              DW FPSWOP     ;06 SWOP
              DW FPDROP     ;07 DROP

              DW FPMOD      ;08 MOD
              DW FPIDIV     ;09 IDIV
              DW FPBOR      ;0A BOR
              DW NONSENSE   ;0B BXOR
              DW FPBAND     ;0C BAND

              DW FPOR       ;0D OR

MULT:      EQU &00
ADDN:      EQU &01
CONCAT:    EQU &02
SUBN:      EQU &03
POWER:     EQU &04
DIVN:      EQU &05
SWOP:      EQU &06
DROP:      EQU &07
MOD:       EQU &08
IDIV:      EQU &09
NUOR:      EQU &0D

              DW FPAND      ;0E N AND N
              DW FPNNOTE    ;0F N<>N
              DW FPNLESE    ;10 N<=N
              DW FPNGRTE    ;11 N>=N
              DW FPNLESS    ;12 N<N
              DW FPNEQUAL   ;13 N=N
              DW FPNGRTR    ;14 N>N

              DW FPSAND     ;15 $ AND N
              DW FPSNOTE    ;16 $<>$
              DW FPSLESE    ;17 $<=$
              DW FPSGRTE    ;18 $>=$
              DW FPSLESS    ;19 $<$
              DW FPSEQUAL   ;1A $=$
              DW FPSGRTR    ;1B $>$

;BINARY INTERNAL OPERATIONS (ALSO DROP, SWOP ABOVE)

              DW FPSWOP13   ;1C SWOP13
              DW FPSWOP23   ;1D SWOP23
              DW FPJPTR     ;1E JPTRUE
              DW FPJPFL     ;1F JPFALSE

NUAND:     EQU &0E
NNOTE:     EQU &0F
NLESE:     EQU &10
NGRTE:     EQU &11
NLESS:     EQU &12
NEQUAL:    EQU &13
NGRTR:     EQU &14

SAND:      EQU &15
SNOTE:     EQU &16
SLESE:     EQU &17
SGRTE:     EQU &18
SLESS:     EQU &19
SEQUAL:    EQU &1A
SGRTR:     EQU &1B

SWOP13:    EQU &1C
SWOP23:    EQU &1D
JPTRUE:    EQU &1E
JPFALSE:   EQU &1F

;UNARY INTERNAL OPERATIONS

              DW FPJUMP     ;20 JUMP
              DW FPLDBREG   ;21 LDBREG
              DW FPDECB     ;22 DECB
              DW FPSTKBR    ;23 STKBREG
              DW FPUSEB     ;24 USEB
              DW FPDUP      ;25 DUP
              DW FP1LIT     ;26 ONELIT
              DW FP5LIT     ;27 FIVELIT
              DW FPSOMELIT  ;28 SOMELIT
              DW FPLKADDRB  ;29 LKADDRB
              DW FPLKADDRW  ;2A LKADDRW
              DW FPREDARG   ;2B REDARG
              DW FPLESS0    ;2C LESS0
              DW FPLESE0    ;2D LESE0
              DW FPGRTR0    ;2E GRTR0
              DW FPGRTE0    ;2F GRTE0
              DW FPTRUNCT   ;30 TRUNC
              DW FPFORM     ;31 RESTACK
              DW FPPOWR2    ;32 POWR2
              DW FPEXIT     ;33 EXIT
              DW FPEXIT2    ;34 EXIT2
              DW FPEXIT2    ;35 SPARE
              DW FPEXIT2    ;36 SPARE
              DW FPEXIT2    ;37 SPARE
              DW FPEXIT2    ;38 SPARE

JUMP:      EQU &20
LDBREG:    EQU &21
DECB:      EQU &22
STKBREG:   EQU &23
USEB:      EQU &24
DUP:       EQU &25
ONELIT:    EQU &26
FIVELIT:   EQU &27
SOMELIT:   EQU &28
LKADDRB:   EQU &29
LKADDRW:   EQU &2A
REDARG:    EQU &2B
LESS0:     EQU &2C
LESE0:     EQU &2D
GRTR0:     EQU &2E
GRTE0:     EQU &2F
TRUNC:     EQU &30
RESTACK:   EQU &31
POWR2:     EQU &32
EXIT:      EQU &33
EXIT2:     EQU &34


;OPERATIONS USED BY BASIC.

;PRIORITY 16,NUMERIC ARG, NUMERIC RESULT

              DW FPSIN      ;39 SIN
              DW FPCOS      ;3A COS
              DW FPTAN      ;3B TAN
              DW FPARCSIN   ;3C ASN
              DW FPARCCOS   ;3D ACS
              DW FPARCTAN   ;3E ATN
              DW FPLOGN     ;3F LOGN
              DW FPEXP      ;40 EXP

SIN:       EQU &39
COS:       EQU &3A
TAN:       EQU &3B
ASN:       EQU &3C
ACS:       EQU &3D
ATN:       EQU &3E
LOGN:      EQU &3F
EXP:       EQU &40

              DW FPABS      ;41 ABS
              DW FPSGN      ;42 SGN
              DW FPSQR      ;43 SQR
              DW FPINT      ;44 INT

              DW FPUSR      ;45 USR
              DW FPIN       ;46 IN
              DW FPPEEK     ;47 PEEK
              DW FPDPEEK    ;48 DPEEK
              DW FPDVAR     ;49 DVAR
              DW FPSVAR     ;4A SVAR
              DW FPBUTTON   ;4B BUTTON
              DW FPEOF      ;4C EOF
              DW FPPTR      ;4D PTR - DISC USE
              DW NONSENSE   ;4E UNUSED

ABS:       EQU &41
SGN:       EQU &42
SQR:       EQU &43
INT:       EQU &44
INP:       EQU &46
PEEK:      EQU &47
EOF:       EQU &4C

;PRIORITY 15, STRING ARG, NUMERIC RESULT

              DW FPUDG      ;4F UDG ADDRESS
              DW NONSENSE   ;50 NUMBER
              DW FPLEN      ;51 LEN
              DW FPCODE     ;52 CODE

;PRIORITY 15, STRING ARG, STRING RESULT

              DW FPVALS     ;53 VAL$

;PRIORITY 15, STRING ARG, NUMERIC RESULT

              DW FPVAL      ;54 VAL

;PRIORITY 15, STRING ARG, STRING RESULT

              DW FPTRUSTR   ;55 TRUNC$

UDGA:      EQU &4F
LEN:       EQU &51
CODE:      EQU &52
VALS:      EQU &53
VAL:       EQU &54

;PRIORITY 16, NUMERIC ARG, STRING RESULT

              DW FPCHRS     ;56 CHR$
              DW FPSTRS     ;57 STR$
              DW FPBINS     ;58 BIN$
              DW FPHEXS     ;59 HEX$
              DW FPUSRS     ;5A USR$
              DW FPINKEY    ;5B INKEY$

CHRS:      EQU &56
STRS:      EQU &57
INKEY:     EQU &5B

;ODD PRIORITY, N ARG, N RESULT

              DW FPNOT      ;5C NOT     PRIORITY 4
              DW FPNEGAT    ;5D NEGATE  PRIORITY 9

NOT:       EQU &5C
NEGATE:    EQU &5D

CALC:      EQU &EF
STOD0:     EQU &C8
STOD1:     EQU &C9
STOD2:     EQU &CA
STOD3:     EQU &CB
STOD4:     EQU &CC   ;USED BY DRCURVE

STO0:      EQU &D0
STO1:      EQU &D1
STO2:      EQU &D2
STO3:      EQU &D3
STO4:      EQU &D4   ;USED BY DRCURVE
STO5:      EQU &D5   ;USED BY DRCURVE

RCL0:      EQU &D8
RCL1:      EQU &D9
RCL2:      EQU &DA
RCL3:      EQU &DB
RCL4:      EQU &DC   ;USED BY DRCURVE
RCL5:      EQU &DD   ;USED BY DRCURVE

STKHALF:   EQU &E0
STKZERO:   EQU &E1
STK16K:    EQU &E2
STKFONE:   EQU &E6
STKONE:    EQU &E9
STKTEN:    EQU &EC
STKHALFPI: EQU &F0


;FPCMAIN.SAM - FLOATING POINT CALCULATOR CONTROL
;ENTRY FROM RST 28H. B=JUNK OR VALUE FOR LOOP COUNTER OR CALC CODE FOR USEB

FPCMAIN:   LD DE,(STKEND)

FPCLP:     LD BC,FPCLP
           PUSH BC           ;'RET' WILL LOOP BACK HERE AT END OF A ROUTINE
           LD (STKEND),DE    ;RESET STKEND IN CASE A BINARY OPERATION HAS
                             ;COMBINED TWO VALUES INTO ONE, OR VALUE HAS BEEN
                             ;STACKED

           LD A,(IX+0)       ;GET A PARAMETER
           INC IX            ;PT TO NEXT ONE

;ENTRY POINT IF 'A' REGISTER ALREADY HAS CALC CODE.

BREGEN:    LD HL,(RST28V)
           INC H
           DEC H
           CALL NZ,HLJUMP    ;CALL WITH A=CODE, IX PAST CODE, DE=STKEND
                             ;ALLOWS DIRECT TRANSLATION OF ALL ZX FPC CODES
                             ;EXCEPT 34H (STK LITERAL) USING A LOOK-UP TABLE.
                             ;SEE FILES FOR 34H METHOD.

           LD B,0            ;USED LATER..
           CP &20
           JR C,FPCBIN       ;JR IF BINARY OPERATOR (00-1FH)

           CP &60
           JR C,FPCUNA       ;JR IF UNARY OPERATOR (20-5FH)

           ADD A,&20         ;E0-FF ->00-1F
           JR C,FPCONST

           ADD A,8           ;D8-DF ->00-07
           JR C,FPRCL

           LD HL,-5
           ADD HL,DE         ;HL=STKEND-5 (SRC FOR STORED VALUE)
           ADD A,8           ;D0-D7 ->00-07
           JR C,FPSTO

           ADD A,8           ;C8-CF ->00-07
           JR C,FPSTOD

           RST &08
           DB 51             ;'FPC error'

;HANDLE BINARY FPC OPERATION

FPCBIN:    LD HL,-5
           ADD HL,DE
           EX DE,HL          ;BACK DE UP TO PT TO STKEND-5

;HANDLE UNARY FPC OPERATION

FPCUNA:    LD C,A
           RLC C             ;BC=PARAM*2 - WORD INDEX
           LD HL,FPATAB
           ADD HL,BC
           LD C,(HL)
           INC HL
           LD B,(HL)
           PUSH BC           ;ROUTINE ADDR
           LD HL,-5
           ADD HL,DE         ;IF UNARY  OP, HL=STKEND-5, DE=STKEND
                             ;IF BINARY OP, HL=STKEND-10, DE=STKEND-5
           RET               ;TO ROUTINE. CY SET

;USE (BCREG) SYS VAR AS SOURCE OF CALCULATOR CODE. USED BY EXPR. EVALUATOR
;UNARY

FPUSEB:    LD A,(BCREG+1)
           JR BREGEN

;STORE-WITH-DELETE USED ABOUT 27 TIMES, SO COMBINE
;STOD0, STOD1, ETC

FPSTOD:    PUSH HL
           DB &FE            ;'JR+1'

;STORE TO A MEMORY (NO DELETE)

FPSTO:     PUSH DE           ;STKEND

           EX DE,HL
           CALL LOCMEM1
           EX DE,HL          ;CALC MEM ADDR TO DE, BC=5

           LDIR              ;COPY TOP STACK ENTRY TO CALC MEMORY
           POP DE            ;STKEND OR STKEND-5 IF DELETE WANTED
           RET

;RECALL A MEMORY

FPRCL:     CALL LOCMEM1      ;FIND CALC MEM ADDR, GET BC=5
           LDIR              ;COPY MEMORY TO STKEND
           RET

LOCMEM1:   LD HL,(MEM)

LOCMEM2:   LD C,A
           ADD A,A
           ADD A,A
           ADD A,C           ;*5
           LD C,A
           LD B,0
           ADD HL,BC
           LD C,5
           RET

FPCONST:   LD C,A            ;C=00-1F. MANY VALUES OF C MAKE NO SENSE.
           LD HL,FPCTAB
           ADD HL,BC         ;PT TO A CONSTANT. DE=STKEND
           LD C,5
           LDIR
           RET               ;TO FPCLP WITH DE=NEW STKEND

FPCTAB:    DB &80            ;DISP 0 - 0.5
           DB 0              ;DISP 1 - ZERO
           DB 0              ;DISP 2 - 16384
           DB 0
           DB 0
           DB &40
           DB &81            ;DISP 6 - FP ONE
           DB 0
           DB 0
           DB 0              ;DISP 9 - INTEGER ONE
           DB 0
           DB 1
           DB 0              ;DISP 0C - TEN
           DB 0
           DB 10
           DB 0

           DB &81,&49,&0F,&DA,&A2   ;DISP 10H - PI/2

;TERMINATE FPC PARAM LIST

FPEXIT:    POP BC            ;JUNK FPC LOOPING ADDRESS
           RET               ;RET TO ROM0'S RST 28H ROUTINE

;TERMINATE FPC PARAM LIST AND JUNK A RET ADDR (REPLACES DB EXIT: RET)

FPEXIT2:   POP BC            ;FPC LOOPING ADDR
           POP BC            ;ROM0 RST 28H RET ADDR
           POP AF            ;PORT STATUS
           POP IX            ;ORIG IX
           JP LRPOUT

;DUPLICATE LAST ENTRY. UNARY OP

FPDUP:     LD BC,5
           LDIR

FPDROP:    RET               ;EXIT IS A UNARY OP SO HL=STKEND-5, DE=STKEND
                             ;DELETE (DROP) IS BINARY SO DE=STKEND-5 - DROP.
FPLDBREG:  LD A,(IX+0)
           CP A              ;SET ZERO
           JR FPLDDCC

;USE (BCREG) SYS VAR AS A COUNTER. DEC AND JR NZ. UNARY

FPDECB:    LD A,(BCREG+1)
           DEC A

FPLDDCC:   LD (BCREG+1),A
           JR Z,FPPSKIP

;UNCONDITIONAL JUMP OF PARAM PTR BY (IX) BYTES. UNARY

FPJUMP:    LD A,(IX+0)
           LD C,A
           RLA
           SBC A,A
           LD B,A            ;B=FF OR 00 ACCORDING TO C
           ADD IX,BC         ;ADJUST IX UP OR DOWN
           RET

;JUMP IF ZERO. BINARY OPERATOR. DE=STKEND-5 SO 'LAST VALUE' DROPPED

FPJPFL:    CALL TSTZERO      ;Z IF VALUE AT (DE) (IE N2) IS ZERO
           JR FPJPTF

;JUMP IF TRUE. BINARY OPERATOR. DE=STKEND-5

FPJPTR:    LD H,D
           LD L,E
           INC HL
           INC HL            ;PT TO 1/0 ON FPCS
           DEC (HL)          ;SET ZERO IF TRUE (1)

FPJPTF:    JR Z,FPJUMP

FPPSKIP:   INC IX            ;SKIP BYTES TO JUMP BY
           RET

;STACK SOME LITERALS FROM PARAM LIST

FPSOMELIT: LD B,(IX+0)       ;FETCH NUMBER OF LITERAL BYTES (5,10,15,20 ETC)
           INC IX
           DB &21            ;"JR+2"

;STACK 5-BYTE LITERAL FROM PARAM LIST

FP5LIT:    LD B,5

FIVLTLP:   LD A,(IX+0)       ;SRC=PARAM LIST
           LD (DE),A
           INC IX
           INC DE
           DJNZ FIVLTLP

           RET               ;DE=NEW STKEND

;'PEEK' USING ADDRESS IN PARAM LIST

FPLKADDRB: CALL LKADDRSR
           JR STACKC

;'DPEEK' USING ADDRESS IN PARAM LIST

FPLKADDRW: CALL LKADDRSR
           JP STACKBC

LKADDRSR:  EX DE,HL
           LD E,(IX+0)
           INC IX
           LD D,(IX+0)
           INC IX
           EX DE,HL     ;STKEND TO DE AGAIN
           LD C,(HL)
           INC HL
           LD B,(HL)
           RET

;STACK BCREG+1 SYSTEM VAR (HOLDS B REGISTER ENTRY VALUE)

FPSTKBR:   LD A,(BCREG+1)
           LD C,A
           JR STACKC

;STACK 1-BYTE LITERAL FROM PARAM LIST. ENTRY: DE=STKEND

FP1LIT:    LD C,(IX+0)
           INC IX
           JR STACKC         ;STACK 00 00 C 00

;SET LAST VALUE TO TRUE (1) IF IT IS >=0 (+VE). UNARY

FPGRTE0:   CALL TSTZERO2     ;SEE IF (HL) NUMBER IS ZERO
           EX DE,HL
           JR Z,SETTRUE

;SET LAST VALUE TO TRUE (1) IF IT IS >0 (+VE AND NON-ZERO). UNARY

FPGRTR0:   INC HL
           LD A,(HL)
           RLA               ;CY IF -VE
           DEC HL
           JR C,SETFALSE

           INC HL
           INC HL
           INC HL
           LD A,(HL)         ;MSB
           DEC HL
           OR (HL)           ;LSB
           DEC HL
           DEC HL
           OR (HL)           ;EXP
           JR Z,SETFALSE

           JR SETTRUE

;SET LAST VALUE TO TRUE (1) IF IT IS 0. UNARY

FPNOT:     CALL TSTZERO2
           EX DE,HL          ;REVERSE EX AT END OF TSTZERO

;SET TRUE IF ZERO, FLASE IF NZ

ZTRUE:     JR Z,SETTRUE

           JR SETFALSE

;SET LAST VALUE TO TRUE (1) IF IT IS <=0. UNARY

FPLESE0:   CALL TSTZERO2
           EX DE,HL
           JR Z,SETTRUE

;SET LAST VALUE TO TRUE (1) IF IT IS <0 (-VE). UNARY

FPLESS0:   INC HL
           LD A,(HL)
           DEC HL
           RLA               ;CY IF -VE

;SET TRUE IF CY, FALSE IF NC

CYTRUE:    JR NC,SETFALSE

;SET LAST VALUE TO 'TRUE' (1). ENTRY: HL PTS TO LAST VALUE

SETTRUE:   LD C,1
           DB &FE            ;'JR+1'

;SET LAST VALUE TO 'FALSE' (0). ENTRY: HL PTS TO LAST VALUE.
;EXIT: HL UNCHANGED, DE=HL+5, A=0

SETFALSE:  LD C,&00
           LD D,H
           LD E,L

;STACK C TO (DE) AS 00 00 C 00 XX

STACKC:    XOR A
           LD (DE),A         ;'INTEGER'
           INC DE
           LD (DE),A         ;'+VE'
           INC DE
           LD A,C
           LD (DE),A         ;LSB FROM PARAM LIST
           XOR A
           INC DE
           LD (DE),A         ;MSB IS ZERO
           INC DE
           INC DE
           RET

;N1 AND N2 - BINARY OPERATION

FPAND:     CALL TSTZERO
           JR Z,SETFALSE     ;SET RESULT TO 0 IF N2 WAS ZERO

           RET               ;ELSE LAST VALUE=N1

;N1 OR N2 - BINARY OPERATION

FPOR:      CALL TSTZERO
           JR NZ,SETTRUE     ;SET RESULT TO 1 IF N2 WAS NON-ZERO

           RET               ;ELSE LAST VALUE=N1

;A$ AND N2 - BINARY OPERATION

FPSAND:    CALL TSTZERO
           RET NZ            ;RETURN A$ IF N2 WAS NON-ZERO

           DEC DE
           LD (DE),A
           DEC DE
           LD (DE),A         ;MAKE STRING LEN=0
           INC DE
           INC DE
           RET

;*******************************************************************************
;STRING COMPARISONS. BINARY
;NOTE: S1<S2 IS TRUE WHEN S1 WOULD APPEAR HIGHER IN AN ALPHABETIC LIST.
;ENTRY: S1,S2 ON STACK. BINARY OP. EXIT FROM STRCOMP WITH HL=S1 PTR, SET TRUE/
;FALSE OVERWRITES S1 WITH 0/1, EXITS WITH DE=NEW STKEND

;S1=S2

FPSEQUAL:  CALL STRCOMP
           JR ZTRUE          ;SET TRUE ON Z, ELSE FALSE

;S1<S2

FPSLESS:   CALL STRCOMP
           JR CYTRUE         ;SET TRUE ON CY

;S1>S2

FPSGRTR:   CALL STRCOMP
           JR Z,SETFALSE

           JR NCTRUE         ;SET TRUE ON NC

;S1<=S2

FPSLESE:   CALL STRCOMP
           JR Z,SETTRUE

           JR CYTRUE         ;SET TRUE ON CY

;S1>=S2

FPSGRTE:   CALL STRCOMP

NCTRUE:    CCF               ;SET TRUE ON NC
           JR CYTRUE

;S1<>S2

FPSNOTE:   CALL STRCOMP

NZTRUE:    JR Z,SETFALSE

           JR SETTRUE

;NUMERIC COMPARISONS. BINARY. N1,N2 -> TRUE/FALSE

;NUMERIC N1=N2

FPNEQUAL:  DB CALC
           DB SUBN
           DB NOT            ;SET TRUE IF ZERO
           DB EXIT2

;NUMERIC N1<N2

FPNLESS:   DB CALC
           DB SUBN
           DB LESS0          ;SET TRUE IF N1-N2 GIVES -VE
           DB EXIT2

;NUMERIC N1>N2

FPNGRTR:   DB CALC
           DB SUBN
           DB GRTR0          ;SET TRUE IF N1-N2 GIVES +VE, NON-ZERO
           DB EXIT2

;NUMERIC N1<=N2

FPNLESE:   DB CALC
           DB SUBN
           DB LESE0          ;SET TRUE IF N1-N2 GIVES -VE OR ZERO
           DB EXIT2


;NUMERIC N1>=N2

FPNGRTE:   DB CALC
           DB SUBN
           DB GRTE0          ;SET TRUE IF N1-N2 GIVES +VE
           DB EXIT2

;NUMERIC N1<>N2

FPNNOTE:   DB CALC
           DB SUBN
           DB NOT            ;SET TRUE IF N1=N2
           DB NOT            ;SET TRUE IF N1<>N2
           DB EXIT2

;TEST IF ZERO AT (DE). Z IF ZERO, ELSE NZ. HL UNCHANGED

TSTZERO:   EX DE,HL

TSTZERO2:  LD A,(HL)         ;EXP
           INC HL
           INC HL
           OR (HL)           ;LSB
           INC HL
           OR (HL)           ;MSB
           DEC HL
           DEC HL
           DEC HL
           EX DE,HL
           RET

;N1 MOD N2. RETURNS N1-N2*INT(N1/N2). BINARY

FPMOD:     DB CALC           ;N1,N2
           DB DUP            ;N1,N2,N2
           DB SWOP13         ;N2,N2,N1
           DB DUP            ;N2,N2,N1,N1
           DB SWOP13         ;N2,N1,N1,N2
           DB DIVN
           DB INT            ;N2,N1,INT(N1/N2)
           DB STO3           ;COMES IN HANDY FOR UNSTADDR
           DB SWOP23         ;N1,N2,INT(N1/N2)
           DB MULT           ;N1,N2*INT(N1/N2)
           DB SUBN           ;N1-N2*INT(N1/N2)
           DB EXIT2

;INTEGER DIVIDE. RETURNS INT (N1/N2). BINARY

FPIDIV:    DB CALC           ;N1,N2
           DB DIVN           ;N1/N2
           DB INT            ;INT(N1/N2)
           DB EXIT2


;N1 BOR N2 (BIT-WISE OR). BINARY

FPBOR:     CALL GETHLBC
           OR L
           LD C,A
           LD A,B
           OR H
           JR BSTKBCH

;N1 BAND N2 (BIT-WISE AND). BINARY

FPBAND:    CALL GETHLBC
           AND L
           LD C,A
           LD A,B
           AND H

BSTKBCH:   LD B,A
           JP STACKBC

;GET 2 NUMBERS FROM FPCS AS INTEGERS IN BC (N1) AND HL (N2). A=C

GETHLBC:   CALL GETINT
           PUSH BC
           CALL GETINT
           POP HL
           RET
