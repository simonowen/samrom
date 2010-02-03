;TADJM.SAM


NMISTOP:   LD SP,ISPVAL
           LD DE,MAINER
           PUSH DE
           LD (ERRSP),SP
           RST &08
           DB 15             ;"BREAK into program"


;GET KEY FROM BUFFER (ACTUALLY, LASTK). NZ=GOT KEY IN A, ELSE A=0

GETKEY:    CALL KEYRD
           RET Z             ;RET IF NO KEY - Z

           JR KBF2

;USED BY INKEY$

READKEY:   RST &30
           DW TWOKSC
           JR Z,RKY2         ;JR IF GOT CODES IN DE

           XOR A
           RET               ;RET IF NO KEY - Z,NC

RKY2:      RST &30
           DW KYVL           ;USE DE TO GET KEYMAP CODE IN A
           AND A             ;NZ
           SCF               ;"GOT KEY"

KBFLUSH:   LD HL,0
           LD (KBQP),HL      ;EMPTY QUEUE

KBF2:      LD HL,FLAGS
           RES 5,(HL)        ;"NO KEY"
           RET

KEYRD:     RST &30
           DW KEYRD2
           LD A,(FLAGS)
           AND &20           ;Z IF NO KEY
           LD A,(LASTK)
           JR KBF2           ;SHOW "NO KEY" IN CASE WE GOT ONE

;FPSTACK.SAM - FPCS SUBROUTINES

;STACKA - STACK "A" REGISTER ON FPCS. EXIT WITH DE=STKEND
;STACKBC - DITTO WITH BC
;STACKHL - DITTO WITH HL

STACKHL:   LD D,L
           LD C,H
           JR STACKCM

STACKA:    LD B,0
           LD C,A

STACKBC:   LD D,C
           LD C,B

STACKCM:   XOR A             ;NC
           LD B,A
           LD E,A

;ENTRY FROM TRUNC$

STKSTOREX: CALL STKSTORE
           EX DE,HL          ;DE=STKEND
           RET

;STORE A STRING. DE=START, BC=LEN. PAGE IS ASSUMED TO BE SWITCHED IN

STKSTOREP: IN A,(251)

;STACK-STORE

STKST0:    AND &7F           ;BIT 7=0 IF ARRAY OR "SLICED". BITS 4-0=START PAGE

STKSTOS:   LD HL,FLAGS
           RES 6,(HL)        ;STRING RESULT

STKSTORE:  LD HL,(STKEND)
           LD (HL),A         ;PAGE AND FLAG IF STRING, EXPONENT IF FP NUMBER
           INC HL
           LD (HL),E         ;E=SIGN IF SMALL INTEGER
           INC HL
           LD (HL),D         ;START IF STRING
           INC HL
           LD (HL),C         ;CD=INTEGER IF SMALL INTEGER
           INC HL
           LD (HL),B         ;LEN IF STRING
           INC HL
           LD (STKEND),HL
           RET

STKFETCH:  LD HL,(STKEND)
           DEC HL
           LD B,(HL)
           DEC HL
           LD C,(HL)
           DEC HL
           LD D,(HL)
           DEC HL
           LD E,(HL)
           DEC HL
           LD A,(HL)

STSTKE:    LD (STKEND),HL
           RET

;FASTER DELETE-TOP-OF-FPCS. TO BE CALLED! THIS IS NOT A FPCS FUNCTION!
;EXIT: HL PTS TO DELETED NUMBER.

FDELETE:   LD HL,(STKEND)
           LD A,L
           SUB 5
           LD L,A
           LD (STKEND),A
           RET NC            ;ALWAYS RETS ON STANDARD SAM...

           DEC H
           JR STSTKE

HLTOFPCS:  LD BC,5
           LD DE,(STKEND)
           LDIR
           LD (STKEND),DE
           RET

;GET FPCS INTEGER TO BC AND HL. A=C. IOOR IF TOO BIG OR -VE

GETINT:    CALL FPTOBC
           JR GETIBC

;GET FPCS BYTE TO A AND C. IOOR IF TOO BIG OR -VE

GETBYTE:   CALL FPTOA

GETIBC:    JR C,IOORERR

           RET Z             ;RET IF +VE

IOORERR:   RST &08
           DB 30             ;"Integer out of range"

;COMPRESS TOP OF FPCS TO BC AND HL. CY IF TOO BIG, NZ IF -VE. A=C

FPTOBC:    LD HL,(STKEND)
           LD BC,-5
           ADD HL,BC
           LD A,(HL)
           AND A             ;NC
           JR Z,FPBCINT

           DB CALC
           DB STKHALF
           DB ADDN
           DB INT            ;CHANGES FORM TO INTEGER IF POSSIBLE
           DB EXIT

FPBCINT:   LD (STKEND),HL    ;"DELETE"
           XOR A
           SUB (HL)          ;NC IF SMALL INTEGER FORM (ELSE IOOR)
           INC HL
           BIT 7,(HL)        ;SET NZ IF -VE
           INC HL
           LD C,(HL)
           INC HL
           LD B,(HL)
           LD A,C
           LD H,B
           LD L,C
           RET Z             ;RET IF +VE

           RET C             ;RET IF OUT OF RANGE

           SBC HL,HL         ;HL=0
           SBC HL,BC         ;NEGATE BC. RESULT NZ, CY
           CCF
           LD B,H
           LD C,L
           LD A,C
           RET               ;NEGATED RESULT IN HL/BC. C=A. NZ, NC

;COMPRESS TOP OF FPCS TO A (AND C). CY IF TOO BIG, NZ IF -VE.

FPTOA:     CALL FPTOBC
           RET C

           EX AF,AF'
           INC B             ;INC B WITHOUT ALTERING THE FLAGS
           EX AF,AF'
           DJNZ FPTOA2       ;JR IF B<>0 - SIGNAL OUT OF RANGE (CY)

           RET

FPTOA2:    SCF
           RET

;CALLED BY MAINER, INIT

SETMIN:    IN A,(URPORT)
           PUSH AF
           CALL ADDRELN
           CALL SETKC2       ;SET KCUR
           LD (HL),&0D
           INC HL
           LD (HL),&FF
           INC HL
           LD (WORKSP),HL   ;CLEAR ELINE
           LD (WORKSPP),A
           POP AF
           OUT (URPORT),A

SETWORK:   LD HL,(WORKSP)
           LD A,(WORKSPP)
           LD (WKEND),HL
           LD (WKENDP),A    ;CLEAR WORKSPACE

SETSTK:    LD HL,(FPSBOT)
           LD (STKEND),HL   ;CLEAR FLOATING POINT CALC STACK
           RET



;SEARCH PROGRAM. CALLED TO FIND DEF FN AND DATA.
;ENTRY: E=TARGET, CHAD PTS TO START

SRCHPROG:  LD D,THENTOK         ;NULL INTERVENING

SEARCHALL: EXX
           LD BC,&FF00+THENTOK  ;ALL PROGRAM/NULL TARGET2 RELOAD
           EXX

SRCHALL2:  LD BC,&0100+THENTOK  ;NO INTERVENING/NULL TARGET2

SRCHALL3:  RST &18              ;START AT CHAD
           LD A,(SUBPPC)        ;STAT NO FOR A"
           JR FINDERS

;ENTRY AT "FINDER" OR "FINDERS" (WITH A=CURRENT STAT NO)
;B=1 FOR NO INTERVENING TOKENS.
;D=INTERVENING TOKS OR "THEN" FOR NULL, E=TARGET, C=TARGET2 (LESLE OR NULL)
;HL PTS TO START
;C"=TARGET 2 RELOAD, B"=ONE LINE/ALL PROG
;EXIT: CY=FOUND JUST BEFORE HL/(CHAD) AT STAT. A

FINCSTAT:  EX AF,AF'            ;INC STATEMENT NO. IN A"
           INC A

FINDERS:   EX AF,AF'
           JR FINDER

FSKIP5:    INC HL

FSKIP4:    INC HL
           INC HL
           INC HL
           INC HL

FINDER:    LD A,(HL)
           INC HL
           CP &0E
           JR Z,FSKIP5          ;SKIP FP FORMS

           CP &0D
           JR Z,FINDER5         ;JR IF LINE END

           CP &B7            ;REMTOK
           JR Z,FREMARK         ;AVOID PROBLEMS FROM EG SINGE QUOTE IN REMS

           CP &22               ;QUOTE
           JR Z,FQUOTE

           CP ":"
           JR Z,FINCSTAT        ;INC STAT NO

           CP THENTOK           ;IF COLON OR "THEN"
           JR Z,FINCSTAT

           CP D
           JR Z,FINTERV         ;INC "INTERVENING" COUNT IN B IF FOUND
                                ; E.G. "DO" OR "IF" (USE "THEN" AS NULL)
                                ;THEN: JR FINDER4

                                ;TARGET2 IS NULL UNLESS LELSE/LIF
                                ;SEARCH, AND INTERVENING COUNT SHOWS
                                ;NO NESTED LIF-ENDIF STRUCTURES (B=1)
           CP C
           JR Z,FOUNDY          ;JR IF TARGET 2 FOUND

FINDER4:   CP E
           JP NZ,FINDER         ;JR IF NOT TARGET

           DJNZ FOUNDX          ;JR IF B<>0 - DON"T ACCEPT FIND

FOUNDY:    LD (CHAD),HL         ;PT CHAD TO TARGET LOCN+1
           EX AF,AF'            ;A=STAT NO.
           SCF                  ;"FOUND"
           RET

FOUNDX:    DJNZ FINDN1          ;JR IF COUNT WASN"T 1

           EXX                  ;OR, IF IT WAS, PRIME TARGET2 TO BE NON-NULL
           LD A,C               ;FETCH TARGET 2 RELOAD FROM C"
           EXX
           LD C,A

FINDN1:    INC B                ;CORRECT INTERVENING COUNTER
           JR FINDER

FREMARK:   LD A,&0D

FREMLP:    CP (HL)
           INC HL
           JP NZ,FREMLP         ;LOOP PAST REMS TO LINE END

FINDER5:   LD A,(HL)            ;MSB OF NEXT LINE NUMBER, OR FF TERMINATOR
           EXX
           CP B                 ;B"=00 FOR 1 LINE SEARCH OR FF FOR WHOLE PROG
           EXX                  ;E-LINE NEEDS AN FF TERMINATOR
           RET NC               ;RET IF FINISHED

           BIT 6,H
           CALL NZ,INCURPAGE    ;INC UPPER RAM PAGE, ADJ HL, IF HL>BFFF
           LD (CLA),HL          ;CURRENT LINE ADDR
           LD A,1               ;STATEMENT 1
           EX AF,AF'            ;IN A"
           JP FSKIP4            ;DO NEXT LINE

FQUOTE:    CP (HL)
           INC HL
           JP NZ,FQUOTE         ;LOOP PAST LITERAL STRINGS

           JR FINDER            ;HL IS PAST END QUOTE

FINTERV:   INC B                ;INC COUNT OF TARGETS TO SKIP
           LD C,THENTOK         ;TARGET2=NULL
           JR FINDER4

;MAKE ROOM, ALLOWING NO OVERHEAD

MKRMCH:    XOR A
           PUSH HL
           CALL TSTRMBIG
           JR MKRM2

;OPEN 1 BYTE AT HL

MKRM1:     LD BC,1

;OPEN BC BYTES AT HL. BC MUST BE <4000H

MAKEROOM:  XOR A

;OPEN ABC BYTES AT HL. A=16K PAGES, BC=MOD 16K

MKRBIG:    PUSH HL           ;LOCN
           CALL TSTRMBIG
           LD HL,150
           SBC HL,DE
           JP NC,OOMERR      ;INSIST ON A 150-BYTE OVERHEAD

MKRM2:     LD D,B
           LD E,C
           LD C,A
           LD A,D
           AND &3F
           LD D,A            ;CDE=ROOM. NC (SIGNALS "MAKEROOM")
           POP HL            ;LOCN
           PUSH DE           ;MOD 16K
           PUSH HL           ;LOCN
           RST &30
           DW XOINTERS       ;EXIT WITH AHL=OLD WORKEND (SRC), MOD/PAGCOUNT SET
           LD DE,(WKEND)
           LD BC,(WKENDP)    ;CDE=DEST (NEW WKEND)
           CALL FARLDDR
           POP DE            ;LOCN
           POP HL            ;MOD 16K
           ADD HL,DE
           EX DE,HL          ;HL=LOCN
           DEC DE            ;DE=END (IF <16K MADE)
           RET

FNORECL:   CALL FNDLINE
           RET NZ

NORECL:    CALL NEXTONE
           JR RECLAIM2      ;DELETE LINE FROM PROGRAM

;ENTRY WITH DE=LOCN, HL=END, DE PAGED IN

RECLAIM1:  CALL DIFFER       ;GET BC, SWOP LOCN TO HL

;ENTRY WITH BC=BYTES TO RECLAIM AT HL (<16K)

RECLAIM2:  XOR A

;RECLAIM ABC BYTES AT HL. A=16K PAGES, BC=MOD 16K

RECL2BIG:  RES 7,B
           RES 6,B
           LD D,A
           OR B
           OR C
           RET Z

           LD A,D
           LD D,B
           LD E,C
           LD C,A            ;CDE=SPACE
           PUSH BC           ;PAGES
           PUSH DE           ;MOD 16K
           PUSH HL           ;LOCN
           SCF               ;"RECLAIMING"
           RST &30
           DW XOINTERS       ;SETS PAG/MODCOUNT TO MOVE
           POP HL            ;LOCN
           POP DE            ;MOD 16K
           POP BC            ;PAGES
           IN A,(251)
           PUSH AF
           PUSH HL           ;LOCN
           BIT 6,H
           JR Z,RECL5

           RES 6,H
           INC A

RECL5:     CALL ADDAHLCDE    ;AHL=SRC
           POP DE
           POP BC
           LD C,B            ;CDE=DEST (LOCN)
           PUSH DE
           CALL FARLDIR
           POP HL
           RET


;MAKE ROOM AT WKSPACE END, BC BYTES LONG. EXIT WITH BC=ROOM SIZE, DE=START,
;HL=END (IF ROOM <16K), A=UNCHANGED. ROOM PAGE SWITCHED IN.

WKROOM:       PUSH AF
              CALL TESTROOM    ;CHECK BC BYTES OK, GET AHL=NEW WKEND
              LD D,A
              LD A,(WKENDP)
              CALL SELURPG     ;SWITCH IN OLD WKEND
              LD A,D
              LD (WKENDP),A    ;NEW WKENDP
              LD DE,(WKEND)    ;START OF ROOM
              LD (WKEND),HL
              LD H,D
              LD L,E
              ADD HL,BC
              DEC HL           ;END OF ROOM, IF ROOM<16K (HL MAY BE >C000)
              POP AF
              RET


;ADJUST SINGLE "SYS VAR" (ACTUALLY, FOR-NEXT OR DO/GOSUB/PROC ADDR)
;ENTRY: HL POINTS TO SVAR, CDE=LOCN, F'=CY IF RECLAIMING

ASSV:      PUSH HL
           POP IY
           LD B,1

PNLP:      LD A,(IY+0)
           LD L,(IY+1)
           LD H,(IY+2)       ;AHL=SVAR
           INC H
           DEC H
           JR Z,NPSV         ;no adj if e.g. eline addr v2.1

;COMPARE AHL AND CDE

           BIT 6,H
           JR Z,PNT2         ;JR IF IN SECTION C

           INC A
           RES 6,H           ;ELSE ADJUST

PNT2:      AND &1F
           CP C
           JR C,NPSV         ;JR IF LOCN PAGE IS HIGHER, SO NO ADJ

           JR NZ,PADJ        ;JR IF LOCN PAGE IS LOWER - ADJUST

           EX DE,HL
           SBC HL,DE         ;ELSE COMPARE OFFSETS - SBC LOCN,SVAR
           ADD HL,DE
           EX DE,HL
           JR NC,NPSV        ;DO NOT ADJUST SVAR IF IT IS <= LOCN

PADJ:      PUSH BC           ;SAVE PTR COUNT AND LOCN PAGE
           PUSH DE
           LD BC,(TEMPW4)
           LD DE,(TEMPW5)    ;CDE=AMOUNT TO ADJ BY
           EX AF,AF'
           JR C,PRECL        ;JR IF RECLAIMING

           EX AF,AF'
           CALL ADDAHLCDE
           JR PNT3

PRECL:     EX AF,AF'
           CALL SUBAHLCDE

PNT3:      POP DE
           POP BC
           LD (IY+0),A
           LD (IY+1),L
           LD (IY+2),H       ;PLACE ADJUSTED SYS VAR

NPSV:      INC IY
           INC IY
           INC IY
           DJNZ PNLP

           RET

;ADJUST 'FOR' LOOPS IF  LOCN C'D'E' IS BEFORE THEM

AFLPS:     EX AF,AF'
           LD C,D
           LD B,26           ;26 LETTER LISTS

AFML:      PUSH HL           ;PT TO CURRENT LETTER LIST

AFLL:      LD E,(HL)
           INC HL
           LD D,(HL)
           ADD HL,DE
           JR C,AFLE         ;JR IF THIS LIST ENDED

           BIT 6,(HL)
           JR Z,AFNF         ;JR IF NOT FOR-VAR

           PUSH HL
           PUSH BC
           LD A,(HL)
           AND &1F           ;NAME LEN-1
           ADD A,18          ;3 TO SKIP TO START OF VALUE, 15 FOR VLS
           LD E,A
           LD D,0
           ADD HL,DE         ;PT TO ADDR
           LD DE,(TEMPW3)    ;CDE=LOCN
           CALL ASSV         ;ADJUST SINGLE "SYSTEM VAR"
           POP BC
           POP HL

AFNF:      INC HL
           JR AFLL           ;NEXT VAR OF THIS LETTER

AFLE:      POP HL            ;THIS LIST ENDED NOW
           INC HL
           INC HL            ;PT TO NEXT LIST
           DJNZ AFML

           EX AF,AF'
           RET


ADDRDEST:     LD L,(DEST-1)\256
              JR ADDRSV

ADDRNV:       LD L,(NVARS-1)\256
              JR ADDRSV

ADDRNE:       LD L,(NUMEND-1)\256
              JR ADDRSV

ADDRSAV:      LD L,(SAVARS-1)\256
              JR ADDRSV

ADDRWK:       LD L,(WORKSP-1)\256
              JR ADDRSV

ADDRKC:       LD A,(KCUR-1)\256
              DB &21

ADDRPROG:     LD A,(PROG-1)\256
              DB &21

ADDRELN:      LD A,(ELINE-1)\256
              DB &21         ;"JR+2"

;USED BY READ AND ITEM

ADDRDATA:     LD A,(DATADD-1)\256
              DB &21         ;"JR+2"

ADDRCHAD:     LD A,(CHAD-1)\256

              LD L,A

;LOOK AT SYS VAR LOCN. ENTRY: HL PTS TO SYS VAR (PAGE, OFFSET)
;EXIT: PAGE SELECTED, HL=OFFSET (IN SECTION C), A=PAGE

ADDRSV:       LD H,VAR2/256

;FROM FN, WITH HL=DEFADDRP

ASV2:         LD A,(HL)
              CALL SELURPG
              INC HL
              LD A,(HL)
              INC HL
              LD H,(HL)
              LD L,A
              IN A,(251)
              AND &1F
              RET


NEXTONE:   PUSH HL
           INC HL
           INC HL
           LD C,(HL)
           INC HL
           LD B,(HL)         ;LINE LEN
           INC HL
           ADD HL,BC
           POP DE            ;DE=OLD START, HL=NEXT LINE START

DIFFER:    AND A
           SBC HL,DE
           LD B,H
           LD C,L
           ADD HL,DE
           EX DE,HL
           RET


;ENTRY AS LIMBYTE, BUT FPCS VALUE DECED BEFORE CHECKING, RETURNED DECED. C IS
;NOT DECED

LIMDB:     LD A,&FF
           DB &FE            ;"JR+1"

;ENTRY: D=LIMIT (VALUE MUST BE BELOW IT), E=ERROR TO GIVE IF VALUE TOO HIGH.
;VALUE IS ON FPCS
;EXIT: A AND C=VALUE, DE=ORIG

LIMBYTE:   XOR A
           PUSH AF
           PUSH DE
           CALL FPTOA
           POP DE            ;LIMIT/ERROR
           JR C,ERRORE       ;JR IF >FF

           JR NZ,ERRORE      ;JR IF -VE

           POP AF
           ADD A,C
           CP D
           RET C

ERRORE:    LD HL,ERRNR
           LD (HL),E
           PUSH HL           ;WILL BE POPPED AND USED AS "RET ADDR"
           JP &0008

;SET PAGCOUNT/MODCOUNT FROM BC (0000-FFFF)

SPLITBC:      PUSH AF
              LD A,B
              RES 7,B
              RES 6,B
              LD (MODCOUNT),BC  ;LEN MOD 16K
              RLCA
              RLCA
              AND &03
              LD (PAGCOUNT),A   ;PAGES (0-3)
              POP AF
              RET

;GET ROOM. RETURN FREE MEMORY (UPPER RAMS) IN AHL AS A 19 BIT NO. NZ IF >=64K

GETROOM:      PUSH BC
              PUSH DE
              CALL WENORMAL

GRM2:         LD C,A
              EX DE,HL         ;CDE=WKSPACE END
              CALL RTNORMAL    ;AHL=RAMTOP (19 BIT)
              SCF
              SBC HL,DE
              SBC A,C
              POP DE
              POP BC
              RET              ;AHL=ROOM (19BIT). NZ IF ROOM >=64K


;PAGE OVERFLOW. USED BY INSTRING, S16OP
;CALLED IF ADD HL,RR GIVES CARRY. CORRECTS THE PAGE AND ADDRESS IN HL
;ENTRY: HL=0000-BFFE (BFFE IF HL WAS BFFF+FFFF)

PGOVERF:      IN A,(URPORT)
              CALL PGOA
              JR PGOE

;CORRECT AHL AFTER PAGE OVERFLOW, BUT DON"T ALTER PAGING

PGOA:         ADD A,2             ;IF THERE WAS A CARRY, ADDR IS AT LEAST 0000
                                  ;SO INC 2 PAGES SO ADDR CAN BE DROPPED 32K
              BIT 6,H             ;IF BIT 6 IS HIGH WE CAN INC AGAIN
              JR Z,PGOA2

              RES 6,H
              INC A

PGOA2:        BIT 7,H
              JR Z,PGOA3          ;IF BIT 7 IS HIGH INC TWICE

              ADD A,2

PGOA3:        SET 7,H
              RET

;ADDRESS ELINE AND DEC PTR TO PT TO SAVARS END

ADDRELND:  CALL ADDRELN

;ENTRY: HL HOLDS AN ADDRESS. IT IS DECREMENTED, AND ADJUSTED IF IT FALLS
;TOO LOW, SO IT PTS TO 8000-BFFF, PAGING AS NEEDED. IF CALLED FROM SYNTAX4,
;HL MAY BE BELOW 8000H ALREADY. COPES WITH UNDERFLOW OF UP TO 32K

DECPTR:    DEC HL

CHKPTR:    BIT 7,H
           RET NZ            ;RET IF STILL AT 8000H OR MORE

           IN A,(251)
           DEC A
           SET 7,H
           BIT 6,H
           JR NZ,DECPT2      ;JR IF FALLEN INTO 4000-7FFF - CORRECT BY 1 PAGE

           DEC A             ;IF IN 0000-3FFF, 2 PAGES

DECPT2:    RES 6,H

PGOE:      OUT (251),A
           RET

;ADD ADDR,BC. ADD BC TO AN ADDRESS IN AHL. A=PAGE, HL=8000-BFFF. BC UNCHNGED.
;BC CAN HAVE ANY VALUE
;SUB ADDR,BC. DITTO
;CARRY IF OVERFLOW

ADDAHLBC:     CALL AHLNORM     ;GET 19 BIT FORM IN AHL
              ADD HL,BC
              ADC A,0          ;ADD AHL,BC
              JR PAGEFORM

SUBAHLBC:     CALL AHLNORM
              AND A
              SBC HL,BC
              SBC A,0          ;SUB AHL,BC
              JR PAGEFORM

;ADD ADDR IN AHL (PAGE FORM) TO THE ADDRESS IN CDE (PAGE FORM)
;RESULT IN AHL (PAGE FORM). CARRY IF OVERFLOW PAGES. CDE UNCHANGED.

ADDAHLCDE:    PUSH BC
              PUSH DE
              CALL TWOCONV
              ADD HL,DE
              ADC A,C
              JR PPFCOM

;SUBTRACT FROM ADDR IN AHL (PAGE FORM) THE ADDRESS IN CDE (PAGE FORM)
;RESULT IN AHL (PAGE FORM). CARRY IF OVERFLOW. CDE UNCHANGED.

SUBAHLCDE:    PUSH BC
              PUSH DE
              CALL TWOCONV
              AND A
              SBC HL,DE
              SBC A,C          ;SUB AHL,CDE

PPFCOM:    POP DE            ;CONTINUE INTO PAGEFORM
           POP BC

;TRANSFORM 19-BIT NUMBER IN AHL TO PAGE, ADDR (8000-BFFF)

PAGEFORM:  RL H
           RLA
           RL H
           RLA               ;NC. PAGE NOW OK
           RR H
           SCF
           RR H              ;ADDR NOW OK IN 8000-BFFF FORM
           CP &20
           CCF               ;SET CARRY IF OVERFLOW
           RET;CONVERT PAGE FORMS IN AHL AND CDE TO 19-BIT. USES B

TWOCONV:   CALL AHLNORM

;CONVERT PAGE FORM IN CDE TO 19-BIT IN CDE. CHANGES CDE ONLY

CDENORM:   PUSH AF
           EX DE,HL
           LD A,C
           CALL AHLNORM
           EX DE,HL
           LD C,A
           POP AF
           RET

RTNORMAL:  LD A,(RAMTOPP)
           LD HL,(RAMTOP)
           JR AHLNORM

WENORMAL:  LD A,(WKENDP)
           LD HL,(WKEND)
           BIT 6,H
           JR Z,AHLNORM

           INC A

;CONVERT PAGE FORM IN AHL TO 19-BIT IN AHL.
;TOP 3 BITS OF ORIG A AND TOP 2 BITS OF HL ARE IRREL.

AHLNORM:   RLC H
           RLC H
           RRA
           RR H
           RRA
           RR H
           AND &07
           RET


;SET ERROR STACK PTR. EXITS WITH OLD ERRSP ON STACK. AFTER A ROUTINE HAS CALLED
;HERE, IT CAN CALL OTHER SRS. AND RETURN WILL STILL OCCUR EVEN AFTER AN ERROR.
;USES HL ONLY

SETESP:    LD HL,(ERRSP)
           EX (SP),HL
           PUSH HL
           LD (ERRSP),SP
           RET


;READ LEN FROM "DESIRED" HEADER (TAPE)

RDRLEN:    LD L,(HDR+HDN+3)\256
           DB &11            ;"JR+2"

;READ LEN FROM LOADED HEADER

RDLLEN:    LD L,(HDL+HDN+3)\256

           LD H,HDR/256

;READ THREE BYTES FROM HL INTO C AND DE

RDTHREE:   LD C,(HL)
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)
           RET

EDGE2:     LD C,0            ;INIT LOOP COUNTER TO 0

EDGEC:     CALL EDGSENS      ;FIND AN EDGE
           RET NC            ;RET IF BREAK OR TIMEOUT

           AND A             ;NC

EDGSENS:   LD A,8            ;DELAY TO AVOID DOUBLE COUNTS, RELATED TO SPEED

EWL:       DEC A
           JR NZ,EWL

EDGLP:     IN A,(KEYPORT)
           INC C
           RET Z             ;TIMEOUT - NC, Z

           XOR B             ;CP EAR BIT
           AND &40           ;WITH LAST EDGE TYPE
           LD A,B
           JR Z,EDGLP        ;LOOP UNTIL SIGNAL LEVEL CHANGES. 47 Ts PER LOOP
                             ;(LENGTHENED TO NEAR MULTIPLE OF 8)
           XOR &67
           LD B,A            ;FLIP "LAST EDGE TYPE" (BIT 6)
                             ;FLIP          BORDER  (BITS 2-0)
                             ;KEEP MIC (BIT 3) UNCHANGED (HI)
                             ;KEEP BEEP (BIT 4) UNCHANGED
                             ;KEEP SOF (BIT 7) UNCHANGED

           AND &1F           ;SOFF BIT (BIT 7) LOW
                             ;THRO MIDI (BIT 6) LOW
                             ;BORDER MSB (BIT 5) LOW

           OUT (KEYPORT),A   ;ALTER BORDER
           LD A,&F7
           IN A,(STATPORT)   ;BIT 5 IS LOW IF ESC PRESSED
           RLCA
           RLCA
           RLCA
           LD A,C            ;A AND C HOLD LENGTH OF PULSE
           RET               ;CY="OK", NC=ESC.
