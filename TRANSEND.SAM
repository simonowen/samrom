;TRANSEND.SAM - TRANSCENDENTAL FUNCTIONS, AND ARITHMETIC
;W.E. THOMSON FASTER SINE/COSE

FPSIN:     DB CALC           ;X
           DB REDARG         ;W
           DB DUP
           DB DUP
           DB MULT           ;W,W*W
           DB STOD0          ;W
           DB STKHALFPI      ;W, FIRST TERM
           DB SOMELIT
           DB 25             ;25 BYTES FOLLOW (5 CONSTS)

           DB &80,&A5,&5D,&E7,&2A
           DB &7D,&23,&35,&E0,&36
           DB &79,&99,&68,&97,&AE
           DB &74,&28,&0A,&10,&9E
           DB &6E,&E6,&4F,&48,&19

           DB LDBREG         ;COUNTER
           DB &05

SINELP:    DB RCL0
           DB MULT
           DB ADDN
           DB DECB
           DB &FC            ;LOOP TO SINELP, 5 TIMES

           DB MULT
           DB EXIT2

;COSINE=SINE(ANGLE+PI/2)

FPCOS:     DB CALC
           DB STKHALFPI
           DB ADDN
           DB SIN
           DB EXIT2

;TAN=SIN X/COS X

FPTAN:     DB CALC
           DB DUP
           DB SIN
           DB SWOP
           DB COS
           DB DIVN
           DB EXIT2

;REDARG - REDUCE ANGLE ARGUMENT TO -180 TO +180 DEGREE RANGE

FPREDARG:  DB CALC
           DB FIVELIT
           DB &7E,&22,&F9
           DB &83,&6E        ;1/(2*PI)
           DB MULT           ;X/(2*PI)=X IN 360 DEGREE UNITS
           DB DUP
           DB STKHALF        ;HALF =180 DEGREES IN THIS CONTEXT
           DB ADDN
           DB INT            ;X IN 360 DEGREE UNITS, INT 360 DEGREE UNITS+.5
           DB SUBN           ;X REDUCED TO -180 TO +180 DEGREES
           DB ONELIT
           DB &04
           DB MULT           ;REDUCED X*4
           DB DUP
           DB ABS
           DB STKFONE
           DB SUBN
           DB DUP
           DB GRTR0
           DB JPTRUE         ;TO REDARG2
           DB 3

           DB DROP
           DB EXIT2

REDARG2:   DB STKFONE        ;(FLOATING PT)
           DB SUBN
           DB SWOP
           DB LESS0
           DB JPTRUE         ;TO REDARG3
           DB 2
           DB NEGATE

REDARG3:   DB EXIT2


;EXPONENTIAL - FPC UNARY FUNCTION. (W.E.T.)
;EXP(N)=e^X

FPEXP:     PUSH HL           ;PTR TO EXP. OF X

           DB CALC           ;X
           DB FIVELIT
           DB &81,&38
           DB &AA,&3B,&29    ;X,1/LN 2

           DB MULT           ;X/LN 2=Y. NOW 2^Y IS REQUIRED RESULT
           DB EXIT

           POP HL

;CALCULATE 2^Y

FPPOWR2:   PUSH HL           ;PTR TO EXP. OF TOP ENTRY

           DB CALC
           DB DUP
           DB INT            ;Y,INT Y
           DB STO1           ;THE INTEGER PART OF THE POWER CAN JUST
                             ;BE ADDED TO EXP OF X (LATER).
           DB SUBN           ;Y-INT Y=W. NOW DO CLEVER THINGS WITH THE
                             ;FRACTIONAL PART OF THE POWER

           DB STOD0          ;(M0=W)
           DB STKFONE        ;FIRST CONST IS 1 (FP FORM)
           DB SOMELIT
           DB 35             ;35 BYTES FOLLOW (7 CONSTS)

           DB &80,&31,&72,&18,&16
           DB &7E,&75,&FD,&E5,&E7
           DB &7C,&63,&59,&85,&4A
           DB &7A,&1D,&82,&11,&42
           DB &77,&30,&07,&1F,&00
           DB &74,&15,&F0,&51,&92
           DB &71,&35,&A0,&6F,&0B

           DB LDBREG
           DB &07            ;COUNTER

EXPLP:     DB RCL0
           DB MULT
           DB ADDN
           DB DECB
           DB &FC            ;LOOP TO EXPLP, 7 TIMES

           DB RCL1           ;2^W,INT Y
           DB EXIT

           CALL FPTOA        ;GET INTEGER PART OF POWER. CY IF >FF, Z IF +VE.
           JR Z,FPEXP3       ;JR IF +VE (BC HOLDS RESULT)

           JR NC,FPEXP2      ;JR IF IN RANGE (-255 TO -1 AS FF TO 01)

           POP HL            ;SET ZERO IF UNDERFLOW

SETFALSH:  JP SETFALSE

FPEXP2:    NEG
           LD C,A
           DEC B             ;NEGATE BC IF -VE

FPEXP3:    POP HL            ;PTR TO 2**W
           LD E,(HL)
           LD D,0            ;DE=EXP OF 2^W
           EX DE,HL          ;HL=EXP
           ADD HL,BC         ;ADD POWER TO GET NEW EXPONENT
           EX DE,HL          ;DE=NEW EXP, HL PTS TO LAST VALUE
           INC D
           JR Z,SETFALSH     ;ZERO RESULT IF UNDERFLOW

           DEC D
           JP NZ,NTLERR      ;ERROR IF OVERFLOW

           LD A,E
           AND A
           JR Z,SETFALSH

           LD (HL),A
           JP SETUPDE


;TO POWER - N1**N2. BINARY
;SPECIAL CASES: N1**0=1, 0**NZ=0

FPPOWER:   INC DE
           INC DE
           LD A,(DE)
           AND &3F
           LD B,A            ;IF N2=0-3FH, B=N2

           DB CALC           ;N1,N2
           DB DUP            ;N1,N2,N2
           DB STKBREG        ;N1,N2,N2,BREG
           DB SUBN           ;N1,N2,N2-BREG
           DB JPFALSE        ;JP TO IPOWER IF BREG=N2
           DB &0C

           DB SWOP           ;N2,N1
           DB DUP            ;N2,N1,N1
           DB JPFALSE        ;JP TO ZPOWER IF N1=ZERO
           DB &05

           DB LOGN           ;N2,LN N1
           DB MULT           ;N2*LN N1
           DB EXP            ;E**(N2*LN N1)
           DB EXIT2

;ZERO TO A NON-ZERO POWER

ZPOWER:    DB SWOP           ;0,N2
           DB DROP           ;0
           DB EXIT2          ;0 TO ANY POWER GIVES RESULT 0

;N1 TO POWER 0-3FH

IPOWER:    DB DROP           ;N1
           DB STOD0          ;(MEM0=N1)
           DB STKONE         ;1
           DB STKBREG        ;1,BREG
           DB JPFALSE        ;1
           DB &05            ;JP IF N1 TO POWER 0 - RESULT=1

           DB RCL0           ;T,N1
           DB MULT           ;NEW T
           DB DECB
           DB &FD            ;LOOP, DOING N1*N1*N1*N1...BREG TIMES

           DB EXIT2

;*******************************************************************************
;LOGN - NATURAL LOG

FPLOGN:    DB CALC           ;X
           DB RESTACK
           DB DUP            ;X,X
           DB LESE0          ;X,1/0
           DB DROP           ;X
           DB EXIT

           INC DE
           INC DE
           LD A,(DE)
           AND A
           JP NZ,INVARG      ;INVALID ARG IF X IS ZERO OR -VE

           LD A,(HL)         ;A=EXPONENT OF X
           LD (HL),&80       ;X REDUCED TO 0.5-0.999999 RANGE
           LD B,A

           DB CALC
           DB STKBREG        ;NX,EXP
           DB ONELIT
           DB &80            ;NX,EXP,80H
           DB SUBN           ;NX,TRUE EXP (PERHAPS -VE)
           DB SWOP           ;TE,NX
           DB DUP            ;TE,NX,NX
           DB FIVELIT
           DB &80,&CC,&CC
           DB &CC,&CD        ;TE,NX,NX,-0.8
           DB ADDN           ;TE,NX,NX-0.8
           DB GRTR0          ;TE,NX,1/0
           DB SWOP23         ;NX,TE,1/0
           DB JPTRUE         ;TO LOGN3
           DB &07

           DB STKFONE        ;NX,TE,1
           DB SUBN           ;NX,TE-1
           DB SWOP           ;TE-1,NX
           DB STKHALF        ;TE-1,NX,0.5
           DB DIVN           ;TE-1,2*NX
           DB SWOP           ;2*NX,TE-1

LOGN3:     DB FIVELIT
           DB &80,&31,&72
           DB &17,&F8        ;NX (OR 2*NX), TE (OR TE-1), LN2
           DB MULT           ;NX,TE*LN 2
           DB SWOP
           DB STKFONE        ;TE*LN 2-1,NX,1
           DB SUBN           ;TE*LN 2-1,NX-1
           DB DUP
           DB FIVELIT
           DB &82,&20,&00    ;TE*LN 2-1,NX-1,NX-1,2.5
           DB &00,&00
           DB MULT           ;              ,(NX-1)*2.5
           DB STKHALF        ;
           DB SUBN           ;              ,2.5*NX-3
           DB STOD0
           DB SOMELIT
           DB 60             ;12 CONSTANTS

           DB &80,&6E,&23,&80,&93
           DB &7D,&A7,&9C,&7E,&5E
           DB &7A,&1B,&43,&CA,&36
           DB &77,&A0,&FE,&5C,&FC
           DB &74,&31,&9F,&B4,&00
           DB &71,&CB,&DA,&96,&00
           DB &6E,&70,&6F,&61,&00
           DB &6C,&90,&AA,&00,&00
           DB &69,&30,&C5,&00,&00
           DB &66,&DA,&A5,&00,&00
           DB &64,&09,&00,&00,&00
           DB &61,&AC,&00,&00,&00

           DB EXIT

           JR SERIES

;*******************************************************************************
;ARCTAN

FPARCTAN:  DB CALC
           DB DUP            ;X,X
           DB ABS            ;X,ABS X       **
           DB STKFONE        ;X,ABS X,1
           DB SUBN           ;X,ABS X-1
           DB GRTE0          ;X,1/0
           DB DUP            ;X,1/0,1/0 (1 IF ABS X>=1, ELSE 0)
           DB JPFALSE        ;JP IF ABS X<1, ARCTAN2, WITH X,0 ON FPCS
           DB &0B

           DB SWOP           ;1,X
           DB DIVN           ;1/X
           DB NEGATE         ;-1/X
           DB DUP            ;-1/X,-1/X
           DB STKHALFPI      ;-1/X,-1/X,PI/2
           DB SWOP           ;-1/X,PI/2,-1/X
           DB LESS0          ;-1/X,PI/2,1/0
           DB JPTRUE         ;JP IF -1/X IS -VE, ARCTAN2. (-1/X, PI/2 ON FPCS)
           DB &02

           DB NEGATE         ;NEGATE PI/2 IF -1/X IS -VE

ARCTAN2:   DB SWOP           ;(+/-PI/2 OR 0),V
           DB DUP            ;               V,V
           DB DUP            ;               V,V,V
           DB MULT           ;               V,V*V
           DB STKHALF        ;               V,V*V,0.5
           DB DIVN           ;               V,2*V*V
           DB STKFONE        ;               V,2*V*V,1
           DB SUBN           ;(+/-PI/2 OR 0),V,2*V*V-1
           DB STOD0          ;(+/-PI/2 OR 0),V

           DB SOMELIT
           DB 60             ;12 CONSTANTS

           DB &80,&61,&A1,&B3,&0C
           DB &7C,&D8,&DE,&63,&BE
           DB &79,&36,&73,&1B,&5D
           DB &76,&B5,&09,&36,&BE
           DB &73,&42,&C4,&00,&00
           DB &70,&DB,&E8,&B4,&00
           DB &6E,&00,&36,&75,&00
           DB &6B,&98,&FD,&00,&00
           DB &68,&39,&BC,&00,&00
           DB &65,&E4,&8D,&00,&00
           DB &63,&0E,&00,&00,&00
           DB &60,&B2,&00,&00,&00

           DB EXIT

SERIES:    LD B,12           ;LOOP COUNTER

           DB CALC
           DB RCL0           ;(+/-PI/2 OR 0),V,12 CONSTS,2*V*V-1
                             ;IGNORE ALL EXCEPT CONSTS AND 2*V*V-1 (CALL IT X)
           DB STKHALF        ;C,C,..C,X,0.5
           DB DIVN           ;       ,2*X
           DB STOD0          ;
           DB STKZERO        ;0
           DB STO1           ;0 (INITIAL N)

SERILP:    DB DUP            ;   N,N
           DB RCL0           ;   N,N,M0
           DB MULT           ;   N,N*M0
           DB RCL1           ;   N,N*M0,M1
           DB STO2
           DB SUBN           ;,C,N,N*M0-M1
           DB SWOP23         ;,N,C,N*M0-M1
           DB ADDN           ;,N,C+N*M0-M1
           DB SWOP           ;,C+N*M0-M1,N
           DB STOD1          ;,C+N*M0-M1
           DB DECB           ;DEC BREG, JR NZ, TO SERILP
           DB &F5

           DB RCL2
           DB SUBN
           DB MULT
           DB ADDN
           DB EXIT2

;ARCSIN

FPARCSIN:  DB CALC           ;X
           DB DUP            ;X,X
           DB DUP            ;X,X,X
           DB MULT           ;X,X*X
           DB STKFONE
           DB SWOP
           DB SUBN           ;X,1-(X*X)
           DB SQR
           DB STKFONE
           DB ADDN           ;X,SQR(1-(X*X))+1
           DB DIVN           ;TAN
           DB ATN
           DB STKHALF
           DB DIVN
           DB EXIT2

;ARCCOS. (ACS X=PI/2-ASN X)

FPARCCOS:  DB CALC
           DB ASN
           DB STKHALFPI
           DB SWOP
           DB SUBN
           DB EXIT2
