;PRINTFP.SAM - PRINT A NUMBER


;*******************************************************************************
;RETURN STR$ OF NUMBER ON FPCS AS BC BYTES AT (DE) IN BUFFER

PFSTRS:    LD A,6

PFSTRSC:   LD (FRACLIM),A    ;UP TO 4 LEADING ZEROS IN FRACTIONS BEFORE EFORM
           CALL PRFPBUF      ;IS USED
           LD A,(DIGITS)
           AND A
           JR NZ,PFNRND      ;JR IF LESS THAN 9 CHARS - RESULT IS EXACT
                             ;ELSE ROUND UP ON 9TH. DIGIT. (NOT A DEC. PT)
           LD HL,(NPRPOS)
           DEC HL
           LD (NPRPOS),HL    ;IGNORE 9TH. DIGIT OTHER THAN FOR ROUNDING
           LD A,(HL)
           CP "5"
           JR C,PFNRND       ;NO ROUND IF E.G. 12345678.4

PFRNDLP:   LD (HL),"0"

PFPSLP:    DEC HL
           LD A,(HL)
           CP "."
           JR Z,PFPSLP

           INC A
           CP "9"+1
           JR NC,PFRNDLP     ;JR IF OVERFLOWED - 0 THIS DIGIT AND KEEP ROUNDING
                             ;EXTREME CASE IS ROUND RIGHT BACK TO FIRST ZERO
                             ;BEFORE EXITING LOOP
           LD (HL),A

PFNRND:    LD HL,(NPRPOS)
           LD DE,PRNBUFF
           AND A
           SBC HL,DE
           LD C,L
           LD B,H
           DEC L
           RET Z             ;IF ONLY 1 CHAR, MUST BE ZERO - LEN 1, AT (DE)

           ADD HL,DE
           INC HL            ;HL=NPRPOS
           LD A,(DECPNTED)
           AND A
           JR NZ,MTRZDLP     ;DELETE TRAILING ZEROS IF DEC PT USED E.G. 1.2300

EFCHECK:   LD A,(EPOWER)
           AND A
           JR Z,PFNPNT       ;JR IF NOT EFORM - LEAVE TRAILING ZEROS

MTRZDLP:   DEC C
           DEC HL
           LD A,(HL)
           CP "0"
           JR Z,MTRZDLP      ;DELETE TRAILING ZEROS

           LD A,(HL)
           CP "."
           JR Z,EFCHECK      ;KEEP "." DELETED AND GOTO PFNPNT IF NOT EFORM
                             ;ELSE PNT IS NOT REAL - DELETE MORE ZEROS IF NEEDED

           INC C             ;ELSE "UNDELETE" LAST NON-"0"/NON-"." CHAR

PFNPNT:    LD A,(DE)
           CP "-"
           JR NZ,PFNMIN     ;SKIP A FIRST "-" IF NEEDED

           INC DE
           LD A,(DE)

PFNMIN:    CP "0"
           JR NZ,PFEXIT      ;JR IF FIRST CHAR USED IN ROUNDING
                             ;(=div by 10 if fract: 0998->999 but 0999->1000)
           LD A,(EPOWER)
           DEC A             ;0->FF
           CP &80            ;CY IF +VE, NZ EPOWER (FRACTION) (80H NOT CRITICAL)
           ADC A,1           ;A=ORIG IF 0 OR -VE, ELSE A=A+1
           CP 8
           JR Z,PFNEZ        ;JR IF FRACTION ROUNDED UP TO 1

           LD (EPOWER),A     ;CAN PUT IN AN EXTRA LEAD ZERO TO COMP FOR
                             ;"MISSED" DIV COS FIRST ZERO NOT DELETED.

PFNEZ:     DEC C             ;COMPENSATE LEN FOR LEADING ZERO TO BE LOST
           LD A,C
           LD HL,(NPRPOS)
           DEC HL
           LD (NPRPOS),HL    ;COMPENSATE NPRPOS IN CASE 9-DIGIT INTEGER LOOP
                             ;USED (SEE LATER)
           LD H,D
           LD L,E
           INC HL
           LD C,10
           LDIR              ;ALIGN TO LHS E.G. 01.23 -> 1.23
           LD C,A

PFEXIT:    LD DE,PRNBUFF
           LD A,(DIGITS)
           LD HL,DECPNTED
           OR (HL)
           LD L,A            ;L=0 IF NINE DIGIT INTEGER
           LD A,(EPOWER)
           LD H,A
           AND A
           LD A,L
           JR NZ,PFEFORM     ;JR IF EFORM (FRACTIONS ARE ALWAYS NOMINALLY EFORM)

;NOW CHECK FOR 9-DIGIT INTEGERS WHICH SHOULD BE EFORM TOO

           AND A
           RET NZ            ;RET IF NOT 9 DIGIT INTEGER

           DEC A
           LD (EPOWER),A     ;POWER FF IS CORRECT FOR E+8
           JR PFNRND         ;PRINT EFORMAT NUMBER

PFEFORM:   AND A
           JR Z,PFPOWOK      ;IF A 9-DIGIT INTEGER WAS PRODUCED
                             ;BY MULTIPLYING BY A POWER - JUMP. IF NOT, FORM
                             ;IS EG 12345678.9 OR 12345678, BECAUSE POWER
                             ;CHOSEN WAS TOO -VE (IN THE CASE OF BIG NUMBERS)
                             ;OR TOO SMALL (IN THE CASE OF SMALL NUMBERS)
                             ;IN WHICH CASE RESULT IS EFFECTIVELY 0.XE+/-H
           INC H             ;CHANGE TO X.XE+/-H

PFPOWOK:   LD A,(FRACLIM)    ;NORMALLY 6, TO ALLOW UP TO 4 LEADING ZEROS
           LD L,A
           LD A,H
           SUB 8
           JR Z,PFSPBH       ;JR IF ROUNDING MEANS *NO* LEADING ZEROS
                             ;(0.999999->1)
           CP L
           JR C,PFFRACT      ;JR WITH 1 FOR 0.1, 2 FOR 0.01,... 5 FOR 0.00001

           BIT 7,H
           JR Z,NEGEFORM

           CPL

NEGEFORM:  LD L,&2F          ;"0"-1

CALCELP:   INC L
           SUB 10
           JR NC,CALCELP

           ADD A,&3A
           PUSH AF
           EX DE,HL          ;RESULT IN EA (POWER AS ASCII)
           ADD HL,BC         ;HL=PRNBUFF, BC=LEN - GET PAST END
           LD (HL),"E"
           INC HL
           LD (HL),"+"
           BIT 7,D
           JR NZ,POWSOK

           LD (HL),"-"

POWSOK:    INC HL
           LD A,E
           CP "0"
           JR Z,POWL10

           INC C             ;LEN IS GREATER BECAUSE "E"+/- 2 DIGITS
           LD (HL),E
           INC HL

POWL10:    POP AF
           LD (HL),A
           INC C
           INC C
           INC C             ;ALLOW FOR E(SGN)1 DIGIT
           LD HL,PRNBUFF
           LD A,(HL)
           CP "-"
           JR NZ,NOMINUS2

           INC HL            ;SKIP "-"

NOMINUS2:  INC HL            ;SKIP FIRST DIGIT
           LD A,(HL)
           CP "E"

PFSPBH:    JR Z,PFSETPRB     ;END NOW IF E.G. 1E+7 - NO PT. INSERTION

           INC C             ;ALLOW FOR GREATER LEN
           LD A,1
           CALL PFSPACE      ;MAKE 1 SPACE AFTER ANY MINUS (CAN BE CONSIDERED
                             ;AS SPACE 1 AFTER THIS, AS LOCNS MATCH)
           INC HL            ;PT TO FIRST DIGIT
           INC HL            ;PT TO SPACE AFTER 1ST DIGIT
           LD (HL),"."
           JR PFSETPRB

;PRINT FRACTION - INSERT LEADING 0./0.0/0.00 ETC.

PFFRACT:   INC A
           CP 20             ;LIMIT TO 17 LEADING ZEROS (FOR PRINT USING)
           JR C,PFFRACT2

           LD A,19

PFFRACT2:  LD B,A            ;B=2-6, C=LEN
                             ;2=0.1, 8=0.0000001
           CALL PFSPACE
           INC HL            ;PT TO SPACE
           LD A,C
           ADD A,B
           CP 22
           JR C,PFFRACT3

           LD A,21

PFFRACT3:  LD C,A            ;NEW LEN (MAX=21)
           LD (HL),"0"
           LD A,"."          ;DECIMAL PT. ON FIRST LOOP ONLY
           JR FRACLPEN

FRACLZLP:  LD (HL),A
           LD A,"0"

FRACLPEN:  INC HL
           DJNZ FRACLZLP

PFSETPRB:  LD DE,PRNBUFF     ;BC=LEN
           RET

;ENTRY:    A=SPACE TO OPEN AFTER ANY MINUS SIGN IN PRNBUFF. (1-19D BYTES)
;EXIT: HL=BEFORE SPACE. BC SAVED

PFSPACE:   PUSH BC
           LD H,&FF
           NEG
           LD L,A            ;HL=-SPACE
           PUSH HL
           LD BC,21          ;BUFF LEN (ACTUALLY, OVERLAPS BCD BUFF, BUT OK)
           ADD HL,BC         ;MAX TO MOVE IS 15; LESS IF BIGGER GAP WANTED
                             ;MIN IS 1
           LD B,H
           LD C,L
           POP HL
           LD A,(PRNBUFF)
           CP "-"
           JR NZ,PFSKMIN

           DEC C             ;1 LESS IF LEADING MINUS - LEAVE IT ALONE

PFSKMIN:   LD DE,PRNBUFF+20  ;DEST=END OF BUFFER
           ADD HL,DE         ;SRC IS SOME BYTES BEFORE
           LDDR
           POP BC
           RET

;*******************************************************************************
;PRFPBUF - "PRINT" A NUMBER (ON FPCS) TO PRNBUFF IN "RAW" STATE. (NO ROUNDING
;OR LEADING ZEROS, NO E+/-, ETC.)
;EXIT: (9-DIGITS)=SIGNIF DIGS IN PRNBUFF. EPOWER=POWER OF 10 USED IF EFORM
;NO DECIMAL PT IF EFORM.
;LEADING MINUS IF -VE, ALWAYS 1 LEADING ZERO.
;E.G. -0.123 OR 01234.5  OR 01.2345

PRFPBUF:   LD HL,DIGITS
           LD (HL),10        ;DIGITS ALLOWED
           INC HL
           XOR A
           LD (HL),A         ;POWER=0 "NOT EFORMAT"
           INC HL
           LD (HL),A         ;"NO DECIMAL PT USED YET"
           INC HL
           LD (NPRPOS),HL    ;PRINT POSN=BUFFER START

           DB CALC
           DB RESTACK
           DB DROP
           DB EXIT           ;DE=DROPPED NUMBER

           LD HL,5
           ADD HL,DE

           EX DE,HL          ;HL=NUMBER, DE=OLD STKEND
           INC HL
           BIT 7,(HL)        ;NZ IF -VE
           DEC HL

           LD A,"-"
           CALL NZ,NPRINT    ;MINUS PRINTED BEFORE -VE NUMBERS
           CALL NPRZERO      ;LEADING ZERO IS USEFUL FOR ROUNDING UP LATER
           LD A,(HL)
           AND A
           RET Z             ;EXIT WITH "0" IF ZERO

           CP &81
           JR C,PRBORS       ;JR IF NO BITS BEFORE BINARY POINT (<1)

           SUB &80           ;=BITS BEFORE BINARY POINT (1-7F)
           CP 30
           JR NC,PRBORS      ;IF MORE THAN 29 BITS - MIGHT NOT FIT IN 9 DIG. BCD

PRMEDN:    INC HL
           LD D,(HL)
           SET 7,D           ;TRUE NUMERIC BIT
           INC HL
           LD E,(HL)
           INC HL
           PUSH DE
           EXX
           POP HL            ;MSW TO HL"
           EXX
           LD D,(HL)
           INC HL
           LD E,(HL)
           EX DE,HL          ;LSW TO HL
           CALL DECIMIZE     ;CONVERT "A" BITS OF HL"HL TO BCD
           EX DE,HL          ;PROTECT LSW
           CALL PRBCD        ;PRINT BCD TO PRINT BUFFER
           EX DE,HL
           RET C             ;RET IF BUFFER FULL

           LD A,H
           OR L
           EXX
           OR H
           OR L
           RET Z             ;RET IF NO SIGNIF BITS LEFT

;BITS REMAINING IN HL"HL ARE THE BITS AFTER THE BINARY POINT, EXP 80H FORM
;EXCEPT THERE MAY BE LEADING ZERO BITS.
;NOW PRINT HL"HL BY MULTIPLYING REPEATEDLY BY 10 TILL BUFFER FULL


PRFRACT:   CALL NPRPNT       ;"."
           EX DE,HL
           EXX
           EX DE,HL          ;TRANSFER NUMBER TO DE"DE

PRFRLP:    XOR A             ;START WITH CARRY OF ZERO
           CALL TENX         ;LE=DE*10+A
           LD D,L            ;DE=DE*10. A=CY
           EXX
           CALL TENX         ;LE=DE*10+A
           LD D,L            ;DE=DE*10. A=CY
           EXX
           CALL NPRINTC
           JR NC,PRFRLP      ;LOOP UNTIL BUFFER FULL

           RET

;LE=DE*10+A. CY IN A

TENX:      LD C,E
           CALL TENMUL
           LD E,L
           LD C,D

;HL=C*10+A.

TENMUL:    LD L,C
           LD H,0
           LD B,H
           ADD HL,HL
           ADD HL,HL
           ADD HL,BC         ;*5
           ADD HL,HL         ;*10
           LD C,A
           ADD HL,BC
           LD A,H            ;A=CARRY FOR NEXT TIME, L=RESULT
           RET

;NUMBERS LESS THAN 1 HAVE EXPONENTS OF 1-80H. EXP 80H MEANS THAT THERE ARE
;NO ZERO BITS AFTER THE BINARY POINT, EXP 71H MEANS THERE ARE 15 BITS, ETC.

PRBORS:    LD (STKEND),DE    ;PUT NUMBER BACK ON FPCS
           JR NC,PRBIGN      ;JR IF BIG

           CALL DECDIGN      ;GET MAX LEADING ZEROS (DECIMAL). 0 OR MORE
           ADD A,9           ;GET AN INTEGER OF 8 OR 9 DIGITS
           JR SHIFTDEC


;NUMBER WITH EXP 9E-FFH (NOW 1E-7FH) - AT LEAST 9 DEC DIGITS

PRBIGN:    CALL DECDIGP      ;FIND MAX DIGITS BEFORE DEC PT, -1. (9 OR MORE)
           SUB 9             ;0 OR MORE.
           CPL               ;-1 OR LESS. GET AN INTEGER OF 8 OR 9 DIGITS

SHIFTDEC:  DEC A
           LD (EPOWER),A     ;..FE=E+9,FF=E+8. GAP=0. 9=E-1,10=E-2...
           INC A
           CALL POFTEN       ;GET N1**A (SIGNED). EXIT WITH RESULT ON FPCS

           CALL FDELETE
           LD A,(HL)         ;A=EXP OF DELETED RESULT
           SUB &80
           JP PRMEDN         ;HL PTS TO DROPPED NUMBER (NOW LARGE INTEGER)

NPRPNT:    LD A,"."
           LD (DECPNTED),A   ;SHOW NON-INTEGER
           JR NPRINT

NPRZERO:   XOR A

;PRINT "A" AND DEC "DIGITS TO COME". CY IF BUFFER FULL (CHAR STILL ENTERED, THO)

NPRINTC:   ADD A,&30
           PUSH HL
           LD HL,DIGITS
           DEC (HL)
           JR NZ,NPRINT2     ;JR IF SPACE OK

           SCF               ;CY=BUFFER FULL
           DB &21            ;"JR+2"

;THIS ENTRY FOR CHARS THAT DON"T COUNT - LIKE "-" AND "."

NPRINT:    PUSH HL

NPRINT2:   AND A             ;NC

NPRINT3:   LD HL,(NPRPOS)
           LD (HL),A
           INC HL
           LD (NPRPOS),HL
           POP HL
           RET

;FIND HOW MANY DECIMAL DIGITS-1 THERE ARE IN A POWER OF TWO (MAXIMUM)
;ENTRY WITH A=POWER OF 2 (-VE IF ENTRY AT DECDIGN)
;EXIT WITH A=DECIMAL DIGITS BEFORE POINT-1 IF DECDIGP OR ZEROS AFTER
;PT. IF DECDIGN

DECDIGN:   LD C,A
           LD A,&80
           SUB C             ;1-80H->7F-00H

DECDIGP:   CALL STACKA

           DB CALC
           DB FIVELIT
           DB &7F,&1A,&20
           DB &9A,&85        ;0.30103
           DB MULT
           DB TRUNC
           DB DROP
           DB EXIT

           INC DE
           INC DE
           LD A,(DE)
           RET

;CONVERT A BITS IN HL"HL TO ASCI IN PRNBUFF. RESULT MAY HAVE UP TO 10 BCD DIGITS

DECIMIZE:  LD C,A

           XOR A
           LD B,5
           LD DE,BCDBUFF     ;ZERO 5-BYTE BCD BUFFER.

ZBCDBLP:   LD (DE),A
           INC DE
           DJNZ ZBCDBLP

DECIMCLP:  ADD HL,HL
           EXX
           ADC HL,HL
           EXX               ;SHIFT LEFT BITS IN HL"HL, TO CY

           LD DE,BCDBUFF+4   ;PT TO RHS BYTE IN BCD BUFFER
           LD B,5

DECIMBLP:  LD A,(DE)
           ADC A,A           ;FOR FIRST BYTE, ADD IN CY FROM HL"HL SHIFT
                             ;FOR LATER BYTES, ADD IN CY FROM PREVIOUS DAA
           DAA
           LD (DE),A
           DEC DE
           DJNZ DECIMBLP

           DEC C
           JR NZ,DECIMCLP    ;SHIFT C BITS

           RET

;PRINT CONTENTS OF 10-DIGIT BCD BUFFER TO PRINT BUFFER AS ASCII.
;EXITS IF ALL BCD DIGITS USED (NC), OR, IF PRINT BUFFER IS PART-FILLED ON ENTRY,
;WHEN DIGIT COUNT REACHES 00 (CY)
;USES HL,BC,AF

PRBCD:     LD C,1            ;NZ C SIGNALS "LEADING ZEROS - IGNORE"
           LD A,&0F          ;CLEAR LHS OF A, AND MARK RHS WITH NON-BCD DIGIT

BCDDIG:    LD B,5            ;BCD ROTATE OF 5 BYTES
           LD HL,BCDBUFF+4

BCDROTL:   RLD               ;RHS NIBBLE OF (HL) TO A, LHS NIBBLE TO RHS,
                             ;RHS NIBBLE OF A TO RHS OF (HL)
           DEC HL
           DJNZ BCDROTL

           JR NZ,PRDIGIT     ;JR IF LAST RLD GAVE A REG A NZ DIGIT

           OR C              ;NZ IF "LEADING ZERO" STATUS
           JR NZ,BCDDIG      ;IGNORE ANY. ELSE A=0 FOR NON-LEADING ZERO

PRDIGIT:   LD C,B            ;C=0 - NO LEADING ZEROS NOW
           AND &0F
           CP &0A
           RET NC            ;RET IF TERMINATOR HIT

           CALL NPRINTC
           JR NC,BCDDIG       ;LOOP UNTIL DIGIT COUNT SHOWS FULL

           RET

;POWER-OF-TEN. MULT. TOP OF FPCS BY 10**"A" REG. WORKS WITH -VE POWERS (A>80H)

POFTEN:    LD C,A            ;BIT 7, C=1 IF -VE POWER
           BIT 7,C
           JR Z,POF10L1

           NEG

POF10L1:   LD B,A            ;B=ABSOLUTE POWER (CORRUPTS!) BC IS SAVED BY SAM
                             ;FPCS
           DB CALC
           DB STKTEN         ;N1,10 (CURRENT POWER. CALL IT P)
           DB EXIT

           JR POF10LPE       ;JUMP INTO THE LOOP

POF10LP:   DB CALC
           DB DUP            ;N1*P,P,P
           DB MULT           ;N1*P,P*P. DOUBLE POWER (WE ARE MOVING MORE SIGNIF
                             ;BITS RIGHT IN B)
           DB EXIT

POF10LPE:  SRL B
           JR NC,POF10L3     ;JR IF NO MULT/DIV ON THIS BIT

           BIT 7,C
           JR NZ,POF10L2     ;JR IF -VE POWER - USE DIV

           DB CALC
           DB STO0           ;N1,P
           DB MULT           ;N1*P
           DB RCL0           ;N1*P,P
           DB EXIT

           JR POF10L3

POF10L2:   DB CALC
           DB STO0           ;N1,P
           DB DIVN           ;N1/P
           DB RCL0           ;N1/P,P
           DB EXIT

POF10L3:   INC B
           DJNZ POF10LP      ;JUMP IF B NOT SHIFTED TO ZERO YET

           LD (STKEND),HL    ;HL PTS TO LAST VALUE (HL IS SET BY "EXIT")
           RET               ;DELETE LAST POWER, LEAVE N1**A ON FPCS
