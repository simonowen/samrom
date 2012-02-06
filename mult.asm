;MULT.SAM - MULTIPLY, DIVIDE, SUBTRACT, ADD

;*******************************************************************************
;QMULT - MULTIPLY HL*DE. RESULT IN HL. FASTER IF EITHER NUMBER IS ONE BYTE.
;USES HL,AF. CY IF OVERFLOW OF 16 BITS.

QMULT:        PUSH BC
              LD B,8           ;BITS TO DO
              LD A,H
              AND A
              JR Z,QMULT2      ;JR IF HL NUMBER IS ONLY 8 BITS - QUICK MULT OK

              EX DE,HL         ;SWOP NUMBERS AND SEE IF ORIG DE IS 8 BIT
              LD A,H
              AND A

QMULT2:       LD C,L
              LD HL,0
              JR Z,SHORTMUL

QHLLP2:       ADD HL,HL        ;DOUBLE RESULT
              JR C,QOVERFL

              RLA              ;TEST NEXT BIT OF ORIG H
              JR NC,NOQADN

              ADD HL,DE
              JR C,QOVERFL

NOQADN:       DJNZ QHLLP2

              LD B,8

SHORTMUL:     LD A,C           ;A=ORIG L

QHLLOOP:      ADD HL,HL        ;DOUBLE RESULT
              JR C,QOVERFL

              RLA              ;TEST NEXT BIT OF ORIG HL
              JR NC,NOQADDIN

              ADD HL,DE
              JR C,QOVERFL

NOQADDIN:     DJNZ QHLLOOP

QOVERFL:      POP BC
              RET              ; RESULT IS IN HL IF NC SIGNALS 'OK'
;*******************************************************************************
;FPC MULTIPLY. BINARY OP WITH HL AND DE PTING TO N1,N2

FPMULT:       LD A,(DE)
              OR (HL)
              JR NZ,FPMULT2    ;JR IF EITHER NUMBER IS FLOATING-POINT

              PUSH DE          ;N2 PTR
              PUSH HL          ;N1 PTR
              CALL FETCHI      ;GET N1 IN DE, SGN IN C
              PUSH DE
              LD B,C
              INC HL
              INC HL           ;PT TO N2
              CALL FETCHI
              POP HL           ;N1
              LD A,C
              XOR B
              LD C,A           ;HL=N1, DE=N2, C=SGN OF RESULT
              CALL QMULT       ;HL=HL*DE
              POP DE           ;N1 PTR
              EX DE,HL         ;HL=N1 PTR, DE=RESULT
              JR C,QMOVERF     ;JR IF OVERFLOW OF 16 BITS

              JR STICZ

;LOAD (HL) WITH INTEGER DE, SGN A

STORADE:   CP 2
           SBC A,0           ;01->00, FF->FF
           CPL               ;01->FF, FF->00
           LD C,A            ;C=SGN
           PUSH DE             ;JUST TO BALANCE STACK **

STICZ:     LD A,D
           OR E
           JR NZ,STOREI

           LD C,A           ;ENSURE SGN OF ZERO IS +VE

;*******************************************************************************
;STORE INTEGER TO (HL). DE=INTEGER, C=SGN. ENTRY WITH STKEND ON STACK

STOREI:       LD (HL),0        ;'INTEGER'
              INC HL
              LD (HL),C        ;SGN
              INC HL
              INC C
              JR NZ,STORPI

              LD A,E           ;NEGATE DE IF SGN IS -VE
              CPL
              LD E,A
              LD A,D
              CPL
              LD D,A
              INC DE

STORPI:       LD (HL),E
              INC HL
              LD (HL),D
              POP DE           ;NEW STKEND
              RET

;*******************************************************************************
;FETCH INTEGER FROM (HL). DE=INTEGER, C=SGN

FETCHI:       INC HL
              LD C,(HL)
              INC HL
              LD E,(HL)
              INC HL
              LD D,(HL)
              DEC C
              INC C
              RET Z            ;RET IF +VE

NEGDE:        DEC DE

CPLDE:        LD A,E           ;NEGATE DE, SINCE SGN IS -VE
              CPL
              LD E,A
              LD A,D
              CPL
              LD D,A
              OR E             ;Z IF DE=0
              RET

NEGDEDE:      CALL NEGDE
              EXX              ;EXIT WITH REGS SWITCHED
              JR NZ,CPLDE      ;CPL MSW UNLESS Z SHOWS

              JR NEGDE


;DEAL WITH INTEGER MULTIPLY OVERFLOW

QMOVERF:      POP DE           ;N2 PTR

;*******************************************************************************
;FLOATING POINT MULTIPLY OF 2 NUMBERS. N1, N2 ARE ON FPCS AT (HL) AND (DE)

FPMULT2:      CALL DFPFORM     ;GET FP FORMS ON FPCS. EXIT WITH Z IF N1 IS A
                               ;POWER OF TWO, CY IF IT IS ZERO. F'=DITTO FOR N2
              RET C            ;IF N1=0, RET (BIN. OP MEANS ONLY 0 LEFT ON FPCS)

              EX AF,AF'
              JR C,MSETZ2      ;JR IF N2 IS ZERO - SET ZERO RESULT **

              PUSH HL          ;N1 PTR
              EX AF,AF'
              JR Z,FPQM1       ;JR IF N1 IS A POWER OF TWO

              EX AF,AF'
              JR NZ,FPMULT3    ;JR IF N2 ISN'T A POWER OF TWO

;SPECIAL CASE - EITHER NUMBER (OR BOTH) IS A POWER OF TWO, I.E. EXP 80 00 00 00
;OR EXP 00 00 00 00. XOR WILL GIVE CORRECT SGN BIT AND COPY NON-ZERO MANTISSA
;BITS. THEN EXPS CAN BE ADDED.

FPQM1:        LD A,(DE)
              LD C,A
              LD B,0
              LD L,(HL)
              LD H,B
              ADD HL,BC
              LD BC,-129

;ENTRY FROM QUICK DIVIDE

FPQMDC:       ADD HL,BC
              INC H
              JR Z,MSETZERO    ;SET ZERO IF UNDERFLOW

              DEC H
              JP NZ,NTLERR     ;ERROR IF OVERFLOW

              LD A,L           ;EXP OF RESULT
              AND A
              JR Z,MSETZERO

              POP HL           ;N1 PTR
              LD (HL),A
              LD B,4           ;4 MANT BYTES

FPQMULP:      INC HL
              INC DE
              LD A,(DE)
              XOR (HL)
              LD (HL),A
              DJNZ FPQMULP

              INC HL
              EX DE,HL         ;DE=NEW STKEND
              RET

MSETZERO:     POP HL

MSETZ2:       JP SETFALSE

FPMULT3:      CALL MUDIADSR    ;GET N1 IN B DE'DE, N2 IN C A'B'C'A, PUT
                               ;RESULT SIGN ON STACK, SET TRUE NUMERIC BITS

              PUSH BC          ;EXP BYTES
              LD HL,0          ;ZERO RESULT LSW
              EXX
              LD HL,0          ;ZERO RESULT MSW
              EXX

;MULTIPLIER IS IN A',B',C',A
;RESULT IS IN HL'HL
;MULTIPLICAND IS IN DE'DE

              LD C,4         ;4 BYTES IN MULTIPLIER

FPMULCLP:     LD B,8         ;8 BITS PER MULTIPLIER BYTE
              AND A
              JR NZ,FPMULBLP ;JR IF ANY BIT IN MULTIPLIER IS HIGH

              LD B,L         ;EXTRA BITS TO B
              LD L,H

              EXX
              LD A,L
              LD L,H
              LD H,0
              EXX

              LD H,A         ;0->H'->L'->H->L->B. SHIFT RESULT RIGHT 8 TIMES -
                             ;THIS IS ALL THAT HAPPENS IF ALL MULTIPLIER BITS=0
              JR NXMBYTE     ;GET NEXT MULTIPLIER BYTE

FPMULBLP:     RRA            ;TEST EACH BIT IN MULTIPLIER, FROM RHS TO LHS
              JR NC,FPMULT25 ;JR IF NO ADD OF MULTIPLICAND

              ADD HL,DE      ;ELSE ADD HL'HL,DE'DE
              EXX
              ADC HL,DE
              EXX

FPMULT25:     EXX            ;HALVE RESULT IN HL'HL
              RR H
              RR L           ;HALVE HL'
              EXX
              RR H
              RR L           ;HALVE HL
              DJNZ FPMULBLP

              RRA
              LD B,A         ;SAVE BITS THAT 'FELL OFF' HL'HL IN CASE THIS IS
                             ;FINAL BYTE OF MULTIPLIER.
NXMBYTE:      EXX
              LD A,C
              LD C,B
              EX AF,AF'
              LD B,A
              EX AF,AF'      ;A'->B'->C'->A. A IS NEXT MULTIPLIER BYTE
              EXX

              DEC C
              JR NZ,FPMULCLP

              LD A,B         ;A=EXTRA BITS (7 AND 6 MATTER)
              LD B,C         ;B=0
              EX DE,HL       ;PROTECT RESULT LSW IN DE
              POP HL         ;EXP BYTES (BOTH NON-ZERO)
              LD C,H         ;BC=N1 EXP (B IS 0)
              LD H,B         ;HL=N2 EXP
              ADD HL,BC      ;HL=EXP SUM+80H+80H
              LD BC,-&80

;ENTRY USED BY DIVIDE
;RESULT IS IN HL'DE, EXTRA BITS IN A. BC=+/-80H. BIT 7 OF STACKED AF=RESULT SGN
;HL=COMBINED EXP. BELOW SGN ON STACK IS N1 PTR

DIVISE:       ADD HL,BC      ;SUBTRACT 80H FROM COMBINED EXP TO CORRECT FOR 2
                             ;+80Hs AS A RESULT OF ADDING 2 EXP BYTES.
                             ;(OR VV. IF DIVIS - ADD 80H TO COMP FOR 80H-80H)
              EXX
              BIT 7,H        ;H'
              JR NZ,MULNORM  ;IF NO LEADING ZERO BITS, OK

              EXX
              DEC HL         ;DEC COMBINED EXP.
                             ;MOVE RESULT TO THE LEFT. ADD IN A 'LOST'
              RLA            ;BIT THAT HAS BEEN IN 'A'
              RL E           ;SINCE LAST HALVING OF RESULT.
              RL D
              EXX            ;SMALLEST MULT IS .1000*.1000 GIVES .01 SO

              ADC HL,HL      ;1 MOVE IS ALWAYS ENOUGH TO NORMALISE RESULT.
                             ;(DITTO FOR DIV)
MULNORM:      EXX
              RLA            ;LOOK AT A 'LOST' BIT FOR FINAL ROUNDING

;ENTRY FROM FPADDN WITH CY IF NEEDS ROUND UP, HL'DE=RESULT, HL=EXP, SGN AND N1
;PTR ON STACK

FPADDNEN:     JR NC,RNDDONE  ;JR IF NO NEED TO ROUND UP

              INC E
              JR NZ,RNDDONE

              INC D
              JR NZ,RNDDONE

              EXX
              INC L
              EXX
              JR NZ,RNDDONE

              EXX
              INC H
              EXX
              JR NZ,RNDDONE

              INC HL         ;ONLY IF ROUNDING RIPPLES RIGHT FORWARDS INC EXP
              EXX
              LD H,&80       ;AND MAKE MANTISSA .1000000
              EXX

RNDDONE:      POP AF         ;BIT 7=SGN BIT

;ENTRY FROM FPSUBN WITH BIT 7,A=SGN, HL'DE=MANT, HL=EXP, (SP)=N1 PTR

FPSUBNEN:     INC H
              JP Z,MSETZERO  ;IF EXP UNDERFLOWED, USE ZERO

              DEC H          ;ZERO UNLESS OVERFLOW
              JP NZ,NTLERR   ;H MUST BE ZERO NOW, SINCE WE ONLY USE 1 BYTE EXPS!

              INC L          ;RESULT NOW IN L HL'DE
              DEC L
              JP Z,MSETZERO  ;JR IF EXP IS ZERO

              EXX
              PUSH HL        ;GET RESULT MSW FROM HL'
              EXX

              POP BC         ;RESULT MSW
              XOR B
              AND &80        ;USE BIT 7 FROM A, REST FROM B. A=SGNED MANT 1
              XOR B
              LD B,L

LDMRSLT:      POP HL         ;N1 PTR
              LD (HL),B
              INC HL
              LD (HL),A
              INC HL
              LD (HL),C
              INC HL
              LD (HL),D
              INC HL
              LD (HL),E
              INC HL
              EX DE,HL       ;DE PTS TO N2 (WILL BE STKEND)
              RET
;DIVISION
;34-BIT DIVISION. MANTISSAS COULD BE 0.999/0.5=1.999 OR 0.5/0.999=0.5
;RANGE OF RESULTS=0.5-1.999 BUT METHOD GIVES HALF THIS (0.25-0.999). ALWAYS
;INC EXPONENT TO DOUBLE VALUE. A 33RD BIT MAY BE NEEDED TO NORMALISE RESULT
;TO 0.5-0.999, AND A FINAL BIT IS NEEDED FOR ROUNDING. (SHARED WITH MULTIPLY)


FPDIVN:       CALL DFPFORM     ;GET FP FORMS ON FPCS. EXIT WITH Z IF N1 IS A
                               ;POWER OF TWO, Z/C IF IT IS ZERO. F'=DITTO FOR N2
              RET C            ;IF N1=0, RET (BIN. OP MEANS ONLY 0 LEFT ON FPCS

              PUSH HL          ;N1 PTR
              EX AF,AF'
              JP C,NTLERR      ;JP IF N2 IS ZERO - OVERFLOW FROM DIVN BY ZERO

              JR NZ,FPDIVN2    ;JR IF N2 ISN'T A POWER OF TWO

;SPECIAL CASE - N2 IS A POWER OF TWO, E.G. EXP 80 00 00 00 OR EXP 00 00 00 00.
;THIS ALLOWS A SIMPLE EXP SUBTRACTION.

FPQD1:        LD A,(DE)
              LD C,A
              LD B,0
              LD L,(HL)
              LD H,B           ;NC HERE
              SBC HL,BC        ;SUB N1 EXP,N2 EXP
              LD BC,129
              JP FPQMDC

FPDIVN2:      CALL MUDIADSR
              EX DE,HL
              EXX
              EX DE,HL
              EXX              ;GET DIVISOR IN DE'DE, DIVIDEND IN HL'HL
                               ;SGN OF RESULT TO STACK, SET TRUE NUM. BITS
              CALL NEGDEDE
              EXX              ;NEGATE DIVISOR IN DE'DE. COMP FOR EXX IN NEGDEDE

              PUSH BC          ;EXP BYTES

           LD BC,&08FC       ;8 BITS PER BYTE OF RESULT.
                             ;C GOES FD,FE,FF,00 FOR 4 BYTES OF RESULT,
                             ;SAVES 33RD BIT AS CY ON FINAL LOOP WITH C=0
           JR FPDIVE

FPDIV3:    LD B,8            ;8 BITS PER BYTE

           EXX
           EX AF,AF'
           LD A,B
           EX AF,AF'
           LD B,C
           LD C,A            ;A'<- B'<- C'<-A. MOVE RESULT BYTES
           EXX

FPDIVLP:   ADD HL,HL
           EXX
           ADC HL,HL         ;MOVE DIVIDEND LEFT
           EXX
           JR NC,FPDIVE

           ADD HL,DE         ;IF CY, ADD NEG DIVISOR, SCF
           EXX
           ADC HL,DE
           SCF
           JR FPDIV4

FPDIVE:    ADD HL,DE         ;ADD NEGATED DIVISOR IN DE'DE TO DIVIDEND IN HL'HL
           EXX
           ADC HL,DE         ;MSW
           JR C,FPDIV4       ;JR IF OK, ELSE REVERSE

           EXX
           SBC HL,DE
           EXX
           SBC HL,DE         ;MSW. NC

FPDIV4:    EXX
           RLA               ;SHIFT RESULT BIT IN
           DJNZ FPDIVLP

           INC C
           JP M,FPDIV3       ;JP IF C=FD/FE/FF

           PUSH AF           ;SAVE LAST WHOLE RESULT BYTE, OR 33RD AND 34TH BITS

           LD B,2
           JR Z,FPDIVLP      ;JR IF STILL GOT 33RD AND 34TH BITS TO DO

           EXX               ;RESULT FROM A'B'C'A TO HL' DE
           LD A,C
           EXX
           LD D,A            ;D
           EXX
           LD L,B            ;L'
           EX AF,AF'
           LD H,A            ;H'
           EXX
           POP AF            ;33/34
           RRCA
           RRCA              ;33/34 BITS TO BITS 7 AND 6
           POP HL
           LD E,H            ;E

           POP BC            ;EXP BYTES
           LD L,B
           LD H,0
           LD B,H            ;HL=N1 EXP, BC=N2 EXP
           AND A
           SBC HL,BC
           LD C,&81          ;80H TO BE ADDED TO COMP FOR 80H-80H IN EXP SBC,
                             ;PLUS 1 TO COMP FOR RESULT IN 0.25-0.999 RANGE

;HL'DE=MANT, EXTRA BITS IN A. BC=81H. BIT 7 OF STACKED AF=SGN. (SP+2)=N1 PTR

           JP DIVISE         ;NORMALISE  (SHARED WITH MULIPLY)

;ADDITION/SUBTRACTION
;FPC BINARY OPERATION.

FPSUBN:       EX DE,HL
              PUSH HL
              CALL FPNEGAT     ;FOR SUBTRACTION, NEGATE N2, THEN ADD
              POP HL
              EX DE,HL

FPADDN:       LD A,(DE)
              OR (HL)
              JR NZ,FPADDN2    ;JR IF EITHER NUMBER IS FLOATING-POINT

              PUSH DE          ;N2 PTR
              INC HL
              PUSH HL          ;PTR TO SGN N1
              INC HL
              LD C,(HL)
              INC HL
              LD B,(HL)        ;BC=N1
              INC HL
              INC HL
              INC HL
              LD A,(HL)        ;SGN N2
              INC HL
              LD E,(HL)
              INC HL
              LD D,(HL)        ;DE=N2
              EX DE,HL         ;HL=N2
              ADD HL,BC
              LD B,0
              EX DE,HL         ;DE=N1+N2
              POP HL           ;PTR TO N1 SGN
              ADC A,(HL)
              RRCA
              ADC A,B
              JR NZ,ADDNOVERF

              SBC A,A
              LD (HL),A        ;SIGN

              LD A,D           ;DEAL WITH MINUS ZERO
              OR E
              JR NZ,MIN0OK     ;JR IF CANNOT BE 00 FF 00 00

              LD A,(HL)
              INC A
              JR NZ,MIN0OK     ;DITTO

              DEC HL
              LD (HL),&91      ;EXP
              INC HL
              LD (HL),&80      ;MAKE RESULT=65536

MIN0OK:       INC HL
              LD (HL),E
              INC HL
              LD (HL),D
              INC HL
              LD (HL),B        ;ZERO MANT 4 IN CASE RESULT IS 65536
              POP DE           ;DE=NEW STKEND
              RET

;*******************************************************************************
ADDNOVERF:    POP DE
              DEC HL           ;GET N1 AND N2 PTRS CORRECT

;N1+N2
FPADDN2:      PUSH HL          ;N1 PTR
              CALL DFPFORM     ;USE FULL FP FORMS
              CALL MUDIADSR    ;N1 IN DE'DE, N2 IN HL'HL, EXPS IN BC
                               ;TRUE NUMERIC BITS, XORED SGNS ON STACK
              LD A,C
              SUB B            ;SUB N2 EXP, N1 EXP. CY IF N1 EXP >N2 EXP
              JR NC,FPADD2     ;JR IF MAX EXP IN C ALREADY

              LD C,B
              NEG              ;A=EXP DIFF.
              EX DE,HL
              EXX
              EX DE,HL         ;SWOP MANTISSAS OF N1 AND N2 SO THAT HL'HL
                               ;IS THE NUMBER WITH THE LARGER EXP. AND DE'DE
              EXX              ;IS THE NUMBER WITH LOWER EXP (NEEDING SHIFT)
              CALL ADDALIGN    ;B=0
              JR FPADD3

FPADD2:       CALL NZ,ADDALIGN ;CALL UNLESS EXPS MATCH - NO NEED TO SHIFT
              EX DE,HL
              EXX
              EX DE,HL
              EXX
              LD B,0           ;BC=COMMON EXP

FPADD3:       POP AF           ;SGNS DO/DONT MATCH FLAG. N1 IN HL'HL NOW
              RLA
              JR C,FPSUBN2     ;SUBTRACT IF SIGNS MISMATCH - SUBTRACT

              ADD HL,DE

              EXX
              ADC HL,DE
              EXX

              EX DE,HL         ;RESULT IN HL'DE
              POP HL           ;N1 PTR
              PUSH HL
              INC HL           ;PT TO SGN OF RESULT
              JR NC,FPADD4     ;JR IF EXP OK AS IT IS

              INC BC           ;INC MAX (COMMON) EXP

              EXX
              RR H             ;BIT 7 BECOMES 1
              RR L
              EXX

              RR D
              RR E             ;NORMALIZE MANTISSA. CY IF NEEDS ROUNDING
                               ;NOTE: -VE NUMBERS ARE ROUNDED IN THE SAM WAY,
                               ;AS WITH MULT AND DIVN, SO BECOME MORE -VE.

FPADD4:       LD A,(HL)
              PUSH AF          ;SGN OF RESULT
              LD H,B
              LD L,C           ;HL=EXP, HL'DE=RESULT, SGN AND N1 PTR ON STACK
              JP FPADDNEN      ;CY IF NEEDS ROUNDING UP


;SGN DON'T MATCH SO SUBTRACT. E.G. N1+(-N2) OR -N1+N2. BC=COMMON EXP

FPSUBN2:      AND A
              SBC HL,DE
              EXX
              SBC HL,DE        ;MSW
              JR NZ,FPSUBN3    ;JR IF MSB OF RESULT IS NZ

              EX AF,AF'        ;PROTECT CY (Z,CY POSSIBLE)**
              EXX
              LD A,H
              OR L
              EXX

              JP Z,MSETZERO    ;JP IF LSW OF RESULT IS ALSO ZERO - ZERO RESULT

              EX AF,AF'        ;** SUBN BUG FIX

;HL'HL=RESULT (NON-ZERO). CY WILL SHOW IF SGN NEEDS REVERSAL

FPSUBN3:      POP DE
              PUSH DE          ;N1 PTR
              INC DE
              LD A,(DE)
              JR NC,FPSUBN4    ;JR IF NO NEED TO REVERSE ORIG SGN AND NEG RESULT

              XOR &80          ;FLIP SGN BIT OF ORIG N1
              EX AF,AF'
              EX DE,HL         ;MSW
              EXX
              EX DE,HL         ;LSW. RESULT IN DE'DE
              CALL NEGDEDE     ;NEGATE DE'DE, EXIT WITH DE' SELECTED
              EX DE,HL         ;MSW
              EXX
              EX DE,HL         ;LSW. RESULT IN HL'HL
              EXX
              EX AF,AF'
              JR FPSUBN4

FPSUBNLP:     EXX
              DEC BC           ;EXP
              ADD HL,HL
              EXX
              ADC HL,HL        ;SHIFT HL'HL LEFT

FPSUBN4:      BIT 7,H          ;H'
              JR Z,FPSUBNLP    ;LOOP UNTIL NUMBER IS NORMALIZED

              EXX
              EX DE,HL         ;RESULT IN HL'DE
              LD H,B
              LD L,C
              JP FPSUBNEN      ;BIT 7,A=RESULT SGN, HL'DE=MANT, HL=EXP
                               ;(SP)=N1 PTR


;ADDALIGN SR. ENTRY:DE'DE=MANT, A=BITS

ADDALIGN:     LD B,A
              CP 33            ;CP BITS TO ROTATE (EXP DIFF)
              JR C,ADDALILP    ;JR IF LESSER WON'T BE SHIFTED TO ZERO

              LD B,33          ;ENSURE DE'DE IS ROTATED TO ZERO

ADDALILP:     EXX
              SRL D
              RR E             ;MSW
              EXX
              RR D
              RR E             ;LSW
              DJNZ ADDALILP    ;LOOP UNTIL NUMBERS ALIGNED FOR COMMON EXP. B=0

              RET NC           ;RET IF NO NEED TO ROUND UP: ALWAYS TRUE IF ZERO

              INC E
              RET NZ

              INC D
              RET NZ

              EXX
              INC DE           ;NEVER CARRIES BECAUSE BIT 7 STARTS AS 0
              EXX
              RET

;MULT/DIV/ADD SR
;ENTRY: HL PTS TO FIRST OF TWO NUMBERS, DE TO SECOND. THEY ARE UNSTACKED TO:
;D'E'DE=(N1 MANTISSA) H'L'HL AND A'B'C'A=(N2 MANTISSA) B/C=(N1/N2 EXP)
;(SP)=SGN OF RESULT (BIT 7). D' AND H' HAVE TRUE NUMERIC BITS SET

MUDIADSR:     INC DE
              LD A,(DE)        ;N2 SGN
              LD B,(HL)        ;N1 EXP
              INC HL           ;PT TO N1 SGN
              XOR (HL)         ;BIT 7 IS XORED SGN BITS - UNLIKE=1
              POP DE           ;RET ADDR
              PUSH AF
              PUSH DE          ;RET ADDR
              LD D,(HL)        ;N1 MANT 1
              SET 7,D          ;TRUE NUMERIC BIT SET
              INC HL
              LD E,(HL)
              INC HL
              PUSH DE          ;N1 MANT 1 AND 2
              LD D,(HL)
              INC HL
              LD E,(HL)        ;DE=N1 MANT 3 AND 4
              INC HL
              LD C,(HL)        ;BC=N1/N2 EXP
              INC HL
              EX AF,AF'
              LD A,(HL)        ;A'=N2 MANT 1
              OR &80
              EX AF,AF'
              INC HL
              LD A,(HL)
              INC HL

              EXX
              LD B,A           ;B'=N2 MANT 2
              LD L,A           ;L'=N2 MANT 2 ALSO
              EX AF,AF'
              LD H,A           ;H'=N2 MANT 1
              EX AF,AF'
              EXX

              LD A,(HL)
              INC HL

              EXX
              LD C,A           ;BC'=N2 MANT 2 AND 3
              POP DE           ;DE'=N1 MANT 1 AND 2
              EXX

              LD L,(HL)        ;L=N2 MANT 4
              LD H,A           ;H=N2 MANT 3
              LD A,L           ;A=N2 MANT 4
              RET

;DOUBLE FPFORM. EQU TO EX DE,HL:CALL FPFORM:EX AF,AF':EX DE,HL: JP FPFORM
;EXIT: NUMBER AT HL AND NUMBER AT DE ARE IN FP FORM. F'=Z IF DE NUMBER IS
;A POWER OF TWO, Z IF HL NUMBER IS.

DFPFORM:      CALL FPFORMX
              EX AF,AF'

FPFORMX:      EX DE,HL

;CHANGE NUMBER AT (HL) TO FP FORM IF NEEDED
;EXIT: HL AND DE UNCHANGED, A, BC CORRUPT, Z IF NUMBER IS A POWER OF 2, CY IF 0

FPFORM:       LD A,(HL)
              AND A
              JR Z,FPFORM2     ;JR IF INTEGER

              INC HL
              LD A,(HL)
              DEC HL
              AND &7F          ;TEST MANT 1, IGNORING SGN BIT
              RET NZ           ;RET IF NOT A POWER OF TWO

              PUSH HL          ;NOW DO A LONGER TEST FOR POWERS OF 2. OTHER MANT
              INC HL           ;BYTES MUST BE ZERO
              INC HL
              OR (HL)          ;TEST MANT 2
              INC HL
              OR (HL)          ;MANT 3
              INC HL
              OR (HL)          ;MANT 4
              POP HL
              RET              ;Z MEANS NUMBER IS AN EXACT POWER OF TWO

FPFORM2:      PUSH DE
              CALL FETCHI      ;INTEGER IN DE, HL PTS TO MANT 3
              XOR A
              INC HL
              LD (HL),A        ;MANT 4
              DEC HL
              LD (HL),A        ;MANT 3 AND 4 WILL BE ZERO
              DEC HL
              DEC HL           ;PT TO MANT 1
              LD A,D
              LD C,&90
              AND A
              JR NZ,FPFORM3    ;JR IF INT IS >8 BITS

              OR E
              JR Z,FPFORM4     ;RET IF INT IS ZERO. ELSE A=E

              LD E,0          ;LS PART OF NUMBER
              LD C,&89        ;INIT EXP

FPFORMLP:     DEC C

FPFORM3:      RL E             ;NC INITIALLY AND LATER...
              RLA
              JR NC,FPFORMLP   ;SHIFT INTEGER TILL IT OVERFLOWS ON THE LHS

              RL (HL)          ;SGN TO CY
              RRA
              RR E             ;ROT. SGN BIT INTO AB - REPLACES TRUE NUMERIC BIT
              LD (HL),A
              INC HL
              LD (HL),E
              DEC HL
              DEC HL
              LD (HL),C        ;PLACE EXPONENT
              AND &7F          ;IGNORE SGN BIT
              OR E             ;Z IF POWER OF TWO
              POP DE
              RET

FPFORM4:      DEC HL
              POP DE
              SCF              ;'ZERO'
              RET
