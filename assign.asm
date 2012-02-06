;ASSIGN.SAM

VALFET1:   LD A,(FLAGS)

VALFET2:   PUSH AF
           CALL SCANNING
           LD A,(FLAGS)
           LD D,A
           POP AF
           XOR D
           AND &40
           JP NZ,NONSENSE

           LD A,D
           RLA
           RET NC            ;RET IF SYNTAX TIME

;RUN TIME ROUTINE

ASSIGN:    CALL ASSISR
           JP SELCHADP

CGXRG:     PUSH HL
           CALL HLTOFPCS
           LD B,A

           DB CALC           ;XRG
           DB ONELIT
           DB 2              ;XRG,2
           DB STKBREG        ;XRG,2,(0/29H)
           DB JPFALSE        ;JP IF ZERO, MULT
           DB 4

           DB DIVN           ;XRG/2
           DB JUMP           ;JP EXIT
           DB 2

           DB MULT           ;XRG*2
           DB EXIT

           POP DE
           JR ASENV          ;DELETE VALUE, COPY TO VARS


;FROM PARPRO

CRTVAR35:  LD A,(TLBYTE)

;CALLED BY MERGE, SETUPVARS

CRTVAR4:   LD C,A
           LD HL,FLAGS
           SET 6,(HL)        ;'NUMERIC'

           CALL NUMLOOK
           CALL SYN14C

ASSISR:    CALL ADDRDEST     ;ADDR OF LAST PTR LSB IF NEW NUMERIC VAR
                             ;ADDR OF 1ST BYTE OF 5 IF EXISTING NUMBER
                             ;ADDR OF SAVARS TERMINATOR IF NEW STRING (NOT USED)
                             ;ADDR OF FIRST CHAR IF EXISTING STRING
           LD A,(FLAGS)
           ADD A,A
           LD A,(FLAGX)
           JP P,ASSTR        ;JR IF STRING

           EX DE,HL
           RRA
           JR C,ASNN         ;JR IF NUMBER IS 'NEW'

;ASSIGN TO EXISTING NUMERIC VAR. DE PTS TO EXISTING VALUE IN NVARS.

ASENV:     CALL FDELETE      ;DELETE FPC DATA, LEAVE HL PTING TO IT

LDI5:      LD BC,5
           LDIR              ;COPY OVER OLD VALUE
           RET

;ASSIGN NEW NUMBER. DE PTS TO LSB OF LAST LINK PTR. CALCULATE NEW VALUE TO POINT
;TO NUMEND, WHERE NEW VARIABLE WILL BE PLACED

ASNN:      LD A,(DESTP)
           AND &1F
           LD C,A
           LD HL,(NUMEND)
           LD A,(NUMENDP)
           SUB C
           JR Z,ANSP         ;JR IF SAME PAGE (USUAL CASE) ELSE A=1,2,3 ETC

           LD BC,&4000

ANSPL:     ADD HL,BC
           DEC A
           JR NZ,ANSPL       ;ADJUST ADDR OF NUMEND UP TILL BOTH NUMBERS
                             ;ARE 'NORMALISED' WITH EACH OTHER. (WRAP-ROUND
           AND A             ;DOESN'T MATTER TILL >64K)

ANSP:      SBC HL,DE         ;GET DISP FROM LSB OF PTR TO FIRST FREE BYTE IN
                             ;NUMS-SAVARS GAP
           DEC HL            ;GET DISP FROM *MSB*.
           PUSH HL           ;SAVE DISP VALUE
                             ;(WE CAN'T ALTER THE LINK YET - THERE MAY NOT BE
                             ;ENOUGH MEMORY TO CREATE THE VARIABLE)
           LD A,(NUMENDP)
           LD C,A
           LD HL,(SAVARS)
           LD A,(SAVARSP)
           CP C              ;Z,NC OR NZ,NC (SAVARS *ALWAYS* HIGHER)
           JR Z,ABSP         ;JR IF (AS UNUSUAL) NUMEND AND SAVARS-START ARE
                             ;IN SAME PAGE
           SET 6,H           ;ELSE SAVARS ARE JUST 1 PAGE HIGHER - ADD 4000H
                             ;TO HL SO BOTH NUMBERS 'NORMALISED'
ABSP:      LD BC,(NUMEND)
           SBC HL,BC
           EX DE,HL          ;DE=FREE GAP BETWEEN NUMS AND STR/ARRAYS
                             ;HL=LSB OF PTR ADDR
           LD A,D
           AND A
           JR NZ,ANOK        ;JR IF AT LEAST 256 BYTES FREE

           LD A,E
           CP 60
           JR NC,ANOK        ;JR IF AT LEAST 60 BYTES FREE (ENOUGH FOR
                             ;LARGEST NUMERIC VARIABLE)

           CALL ADDRSAV
           CALL DECPTR
           LD BC,&0200
           CALL MAKEROOM     ;OPEN 512 BYTES BEFORE SAVARS
           CALL ADDRDEST     ;HL=PTR LSB

;OK TO CREATE NEW NUMERIC

ANOK:      POP DE            ;DISP
           LD (HL),E
           INC HL
           LD (HL),D         ;MAKE LINK OF PREVIOUS LAST-VAR-OF-THIS-LETTER
                             ;PT TO NEW LAST VAR.
           CALL ADDRNE       ;PT HL TO NUMEND (LOCN OF NEW VAR.
           LD A,(TLBYTE+33)
           LD (HL),A
           INC HL
           LD B,&FF
           LD (HL),B
           INC HL
           LD (HL),B
           INC HL            ;PTR=FFFF (LAST VAR OF THIS FIRST LETTER)
           EX DE,HL
           LD HL,FIRLET+34   ;PT TO SECOND LETTER OF NAME
           AND &1F
           JR Z,ASNCL        ;JR IF SINGLE-LET VAR

           LD C,A
           INC B             ;BC=LEN OF NAME (LESS FIRST LETTER)
           LDIR              ;COPY TO VARS

ASNCL:     CALL ASENV        ;COPY FPC VALUE TO VARS

NELOAD:    LD (NUMEND),DE    ;NUMEND IS PAST LAST BYTE OF VALUE
           BIT 6,D
           RET Z             ;RET IF STILL 8000-BFFF

           RES 6,D
           LD A,(NUMENDP)
           INC A
           LD (NUMENDP),A
           JR NELOAD

;ASSIGN A STRING

ASSTR:     RRA               ;TEST BIT 0,(FLAGX)
           JP C,ASNST        ;JP IF IT IS A NEW STRING

;ASSIGN TO EXISTING STRING VAR

           LD BC,(STRLEN)    ;LENGTH OF DESTINATION
           LD A,(DESTP)
           RLA
           JR C,ASDEL        ;JR IF UNSLICED SIMPLE STRING - DELETE OLD VERSION

           LD A,B
           OR C
           RET Z             ;RET IF E.G. LET A$(4 TO 3)="TEST" - DEST LEN=0

           PUSH HL           ;DEST ADDR
           PUSH BC           ;DEST SIZE
           CALL STKFETCH     ;ADE/BC =STRING START/LEN
           POP HL            ;DEST SIZE
           SBC HL,BC         ;DEST SIZE-SRC SIZE (NC HERE)
           JR NC,AES1        ;JR IF TRUNC NOT NEEDED

           ADD HL,BC         ;HL=DEST SIZE AGAIN
           LD B,H
           LD C,L            ;BC=TRUNCATED SRC LEN NEEDED TO FILL DEST
           LD HL,0           ;'PADS' NEEDED=0

AES1:      EX (SP),HL        ;PADS NEEDED TO STACK, DEST ADDR TO HL
           EX DE,HL          ;DE=DEST, HL=SRC
           EX AF,AF'
           CALL SPLITBC      ;LD PAGCOUNT/MODCOUNT WITH BC
           IN A,(251)
           LD C,A            ;CDE=DEST
           EX AF,AF'         ;AHL=SRC
           CALL FARLDIR      ;COPY STRING TO DEST
           POP BC            ;PADS
           LD A,B
           OR C
           RET Z

           LD A,(TEMPB2)
           CALL TSURPG
           LD HL,(TEMPW1)    ;PT TO PAST LAST BYTE FARLDIRED EARLIER
           XOR A
           CP C              ;NC ONLY IF C=0
           ADC A,B           ;A=B+1 UNLESS C WAS ZERO, WHEN A=B
           LD B,C
           LD C,A
           LD A,&20

ASPSL:     LD (HL),A
           INC HL
           DJNZ ASPSL

           DEC C
           RET Z

           CALL CHKHL
           JR ASPSL

;E.G. RECORD TO A$ OR:  RECORD STOP

RECORD:    CP &B1            ;IS IT 'RECORD STOP'? STOPTOK
           JR NZ,RECORD2

           XOR A
           LD (GRARF),A      ;GRAPHICS RECORD FLAG 0 (OFF)
           RST &20           ;SKIP 'RECORD'
           RET

RECORD2:   CP TOTOK
           JR NZ,RCNONS

           RST &20
           CALL LVFLAGS
           JP M,NONSENSE     ;ERROR IF NUMERIC

           JR C,RECORD3      ;JR IN RUN TIME (ALLOWS STR ARRAYS TO BE DELETED)

           BIT 6,C
           RET Z             ;RET IF SIMPLE STRING, ELSE ERROR IF E.G. A$(3)

RCNONS:    RST &08
           DB 29

RECORD3:   EX AF,AF'
           CALL NZ,ASDEL2    ;IF FOUND, DELETE VAR PTED TO BY STRLOC

           LD DE,STRM16NM
           CALL SCOPN1       ;COPY NAME TO STRM16NM.( MAY COPY 12 BYTES,
                             ;HITTING GRARF - BUT IRREL)
           LD A,D
           LD (GRARF),A      ;GRAPHICS RECORD FLAG=NZ (ON)
           CALL SCOPNM       ;EXIT WITH BC=0 (NULL LEN)
           JR ASNS1          ;ASSIGN NULL STRING TO NAME

;ASSIGN STRING, THEN DELETE OLD STRING. E.G. LET A$=A$ OR LET A$=A$+"X"

ASDEL:     CALL ASNST        ;CREATE NEW VERSION FIRST SO E.G. LET A$=A$ WORKS
           CALL ADDRDEST     ;HL PTS TO TEXT OF OLD STRING
           LD DE,-14
           ADD HL,DE         ;PT TO TLBYTE IN VARS
           CALL CHKPTR
           JR ASDEL3

;CALLED BY DIM TO DELETE EXISTING ARRAYS OR STRINGS, WITH A=PAGE

ASDL1:     CALL SELURPG

;CALLED BY END PROC TO DELETE LOCAL STRINGS/ARRAYS, RECORD TO DELETE EXISTING
;STRING/ARRAY. ENTRY WITH STRLOCN PAGED IN

ASDEL2:    LD HL,(STRLOCN)

ASDEL3:    PUSH HL           ;PTR TO TLBYTE
           LD BC,11
           ADD HL,BC         ;PT TO LEN (PAGES)
           CALL ADD14
           LD B,H
           LD C,L
           POP HL            ;PTR TO TLBYTE
           JP RECL2BIG       ;DELETE STR/ARRAY AND 14-BYTE HEADER (ABC AT HL)

;CALLED BY TAPEMN

ADD14:     LD A,(HL)
           INC HL
           LD E,(HL)         ;LEN MOD 16K
           INC HL
           LD D,(HL)
           EX DE,HL          ;AHL=LEN (PAGES, LEN MOD 16K)
           LD BC,14
           ADD HL,BC         ;ADD 14 TO GET LEN INCLUDING HDR
           BIT 6,H
           RET Z             ;RET IF MOD <16K

           INC A
           RET

;ASSIGN A NEW STRING

ASNST:     CALL STKFETCH     ;DATA FOR STRING ASSIGNMENT

           AND &1F
           LD H,A
           LD A,(WKENDP)
           LD L,A
           LD A,H
           CP L
           JR C,ASNS1        ;JR IF SRC PAGE LOWER THAN WKEND

           JR NZ,ASNS0       ;JR IF SRC PAGE HIGHER

           LD HL,(WKEND)
           SBC HL,DE
           JR NC,ASNS1       ;JR IF SRC <=WKEND

ASNS0:     LD (FIRST),DE     ;ELSE SRC>WKEND AND SHOULD NOT BE AUTO-ADJUSTED
           LD (LAST),A
           LD A,&FF          ;SIGNAL 'XPTR NOT USED'

;CALLED BY 'RECORD' TO CREATE NULL STRING

ASNS1:     PUSH AF
           PUSH BC           ;STRING LEN
           LD (XPTR),DE
           LD (XPTRP),A      ;SAVE START IN AUTO-ADJ VAR (MAKEROOM MAY MOVE IT)
           LD A,14           ;ALLOW FOR TYPE/NAME LEN (1) NAME (10), TXT LEN (3)
           ADD A,C           ;ADD TO TEXT LEN TO GET ROOM NEEDED
           LD C,A
           JR NC,ASNS2

           INC B
           JP Z,STLERR       ;TOTAL LEN MUST NOT EXCEED FFFFH

ASNS2:     LD A,B
           RLCA
           RLCA
           AND &03
           CALL SAROOM
           POP BC            ;STRING LEN
           CALL MBC          ;COPY PAG/MOD COUNT TO VARS
           IN A,(251)
           LD C,A            ;CDE=DEST
           POP AF
           INC A
           LD A,(XPTRP)
           LD HL,(XPTR)
           JR NZ,ASNS3       ;JR IF XPTR NOT USED

           LD A,(LAST)
           LD HL,(FIRST)

ASNS3:     LD (XPTR+1),A     ;PAGE SHOULD HAVE BIT 7 LOW - CANCEL XPTR
           JP FARLDIR        ;LDIR PAGCOUNT/MODCOUNT BYTES


;ASSESS FOR-NEXT VAR (USED BY 'FOR' AND 'NEXT')

SYNTAX4:   CALL LVFLAGS
           JP P,NONSENSE     ;ERROR IF STRING

           BIT 5,C
           JP NZ,NONSENSE    ;OR NUMERIC ARRAY NAME

           JR NC,SYNT41      ;JR IF SYNTAX TIME

           EX AF,AF'

SYN42:     JR Z,SYN14C       ;JR IF DOESN'T EXIST

           BIT 6,C           ;C IS FROM VARS
           JR NZ,SYN14C      ;JR IF (EXISTING) FOR-NEXT VAR

           PUSH IX
           POP HL            ;ADDR OF PTR LSB
           SET 5,(IX-1)      ;TYPE BYTE MARKED AS 'UNUSED' ** BUG FIX
 ;         LD C,0FFH         ;'INVIS' AND 'UNUSED' - DON'T EXIST
           CALL NVMLP
           JR SYN42          ;LOOP FOR ALL COPIES

;          LD C,0            ;NON-ARRAY TYPE
;          DB 3EH            ;'JR+1'

SYNT41:    EX AF,AF'

           JR SYN14C

SSYNTAX1:  RST &20

;USED BY LET/READ/INPUT TO ASSESS VAR ABOUT TO BE ASSIGNED TO.

SYNTAX1:   CALL LOOKVARS     ;IF FND, C=T/L FROM VARS, ELSE C=DESIRED T/L

SYN14C:    EX DE,HL          ;IF FND, DE PTS TO START OF NUMBER, OR LEN INFO OF
                             ;STRINGS/ARRAYS (PAGE/LEN MOD 16K)
;FROM PARAM PROCESSING

SYN1PP:    LD HL,FLAGX
           LD (HL),0         ;VAR NOT NEW (BIT 0=0)
           JR NZ,TSYNT12     ;JR IF VAR EXISTS, OR SYNTAX TIME

                             ;ELSE C='DESIRED' TYPE, BITS 6 AND 5 ARE 0
                             ;IF SIMPLE UNSLICED STRING, OR A SIMPLE NUMBER

           INC (HL)          ;'NEW VARIABLE'

           LD A,C
           AND &60
           JR Z,TSYNT14      ;DESTP BIT 7 WILL BE 0 ('KEEP OLD VALUE')

                             ;ERROR IF TRYING TO USE AN UNDIMED ARRAY, OR SLICE
                             ;A NEW STRING.
VNFERR:    RST &08
           DB 2

;VAR EXISTS, OR SYNTAX TIME

TSYNT12:   LD A,(FLAGS)
           ADD A,A           ;P IF STRING, CY IF RUNNING
           JP P,TSYNT13      ;JP IF STRING

           BIT 5,C
           JR Z,TSYNT14      ;JR IF A SIMPLE NUMBER, NOT AN ARRAY
                             ;ELSE STKVAR HANDLES NUMERIC ARRAYS

TSYNT13:   CALL STKVAR       ;PASS ARRAY/STRING DATA TO FPCS. EXIT WITH HL
                             ;PTING TO VALUE, IF NUMERIC ARRAY, ELSE FPCS
                             ;HOLDS STRING DETAILS
           LD A,(FLAGS)
           ADD A,A           ;CY IF RUNNING, -VE IF NUMERIC
           JP M,TSYNT15      ;JP IF NUMERIC - HL PTS TO VALUE, PAGED IN

           CALL C,STKFETCH   ;GET DE=START, BC=LEN, A=PAGE (IN VARS). BIT 7=1
           EX DE,HL          ;IF 'OLD COPY TO BE DELETED' AS IN 'LET A$="SS"'
           JR TSYN16

TSYNT14:   EX DE,HL

;FOR NUMERICS AND NEW STRING/ARRAY VARS, STRLEN=TYPE/LEN FROM VARS AND JUNK;
;FOR EXISTING STRING/ARRAY VARS, STRLEN=LENGTH
;   IF 'OLD COPY TO BE DELETED' BIT 7 OF DESTP=1
;   TLBYTE=REQUESTED TYPE/LEN, FIRLET=NAME

TSYNT15:   IN A,(251)
           AND &1F

TSYN16:    LD (STRLEN),BC
           LD (DEST),HL      ;ADDRESS OF NUMERIC OR STRING VALUE, OR PTR/STOPPER
           LD (DESTP),A      ;PAGE OF STRING START, OR CURRENT PAGE IF NUMBER
           LD B,(HL)
           INC HL
           LD A,(HL)
           INC A
           OR B
           INC HL
           OR (HL)
           INC HL
           OR (HL)           ;IF VAR STARTS 00 FF 00 00, A=0
           LD (DFTFB),A      ;IRREL IF NVAR NON-EXISTENT

;CALLED BY 'DIM'

SCOPNM:    LD DE,TLBYTE+33

;CALLED BY 'RECORD TO'

SCOPN1:    LD HL,TLBYTE
           LD A,(HL)
           AND &1F           ;NAME LEN-1 IF NUMERIC, TRUE NAME LEN IF STR/ARRAY
           ADD A,2           ;ALLOW FOR TLBYTE AND (PERHAPS) ANOTHER LETTER
           LD C,A

;CALLED BY LENGTH SR

SCOPN2:    LD B,0
           LDIR              ;COPY NAME TO BUFFER THAT WON'T BE USED BY EVAL
           JP SELCHADP


;FIND START AND LEN OF AN EXISTING STRING, OR START OF A NUMBER IN AN ARRAY
;ON ENTRY: DE PTS TO PAGES OF VAR LEN, THEN LENGTH MOD 16K. C=T/L. CY IF RUNNING
;CHAD POINTS PAST '$' OR '(' (UNLESS ERROR)

STKVAR:    EX DE,HL          ;HL PTS TO PAGES OF LEN IF RUNNING

STKVAR2:   JR C,SVRUNT       ;JR IF RUNNING

           BIT 6,C
           JR NZ,SVSSL       ;JR IF STRING ARRAY OR SLICED STRING

           BIT 5,C
           RET Z             ;RET IF SIMPLE UNSLICED STRING - NO ACTION
                             ;CONTINUE WITH NUMERIC ARRAYS
           DB &FE            ;'JR+1'

SVDSL:     RST &20

;CALLED BY 'DIM' SYNTAX CHECK - CHECK  N,N,...N)

SVDSK:     CALL EXPT1NUM
           CP ","
           JR Z,SVDSL

SVIBH:     JP INSISCBRK      ;')'


;CHECK STRING ARRAY SYNTAX; E.G. ), N,X TO Y) OR N,N,TO Y) OR N,N,Y TO)

SVSSL:     RST &18
           CP ")"
           JR Z,SVSL3        ;ALLOW '()'

           DB &FE            ;'JR+1'

SVSSLP:    RST &20

           CP TOTOK
           JR Z,SVSL2

           CALL EXPT1NUM
           CP ","
           JR Z,SVSSLP

           CP TOTOK
           JR Z,SVSL2

           CALL INSISCBRK
           JR SLPXHP

SVSL2:     RST &20           ;SKIP 'TO'
           CP ")"
           JR Z,SVSL3        ;SKIP ')' IF IT IS ONE

           CALL EX1NUMCB     ;ELSE ACCEPT 'N)'
           DB &FE            ;'JR+1'

SVSL3:     RST &20           ;SKIP ')'

SLPXHP:    JP SLLPEX         ;SET 'STRING' STATUS (DISTURBED BY 'EXPT1NUM')

;*******************************************************************************
;STACK VAR - RUN TIME

SVRUNT:    LD A,C
           AND &60
           JR NZ,SVARRAYS    ;JR IF ARRAY, ELSE HANDLE SIMPLE STRING BY
                             ;CONVERTING THE PAGE/LEN MOD 16K DATA IN VARS TO 2
                             ;BYTES (SIMPLE STRINGS HAVE LEN 0000-FFFF)
           LD A,(HL)         ;PAGES (0-3)
           INC HL
           LD C,(HL)
           INC HL
           RRCA
           RRCA              ;??00 0000
           OR (HL)
           LD B,A            ;BC=LEN
           LD D,&80

SVSIMPLE:  EX DE,HL
           INC DE            ;PT TO TEXT
           IN A,(251)
           BIT 6,D
           JR Z,SVSS2        ;JR UNLESS STR. STARTED AT E.G. BFFF, PTR NOW C001

           RES 6,D
           INC A

SVSS2:     AND &1F
           OR H              ;BIT 7 SET (DELETE OLD COPY) IF SIMPLE STRING
                             ;BIT 7 RES (OVERWRITE) IF 1-DIM STRING ARRAY
           CALL STKSTORE     ;DE=ST, BC=LEN, A=START PAGE
                             ;BIT 6,(FLAGS) IS CORRECT ALREADY
           CALL SELCHADP
           LD A,(TLBYTE)
           BIT 6,A
           RET Z             ;RET IF NO BRACKET AFTER NAME - NO SLICING

           JP SLCL2

SVARRAYS:  INC HL
           INC HL
           INC HL
           LD B,(HL)         ;NO. OF DIMS
           BIT 5,C
           JR NZ,SVCDIS      ;JR IF NUMERIC ARRAY

           DJNZ SVCKS        ;JR IF MULTI-DIM STRING ARRAY (B=DIMS-1)

           LD D,B            ;D=0 SO BIT 7,A LEFT AS ZERO (SIGNALS 'OVERWRITE')
           INC HL
           LD C,(HL)
           INC HL
           LD B,(HL)         ;BC=LEN OF SINGLE DIMENSION ($)
           JR SVSIMPLE       ;HANDLE LIKE A SIMPLE STRING

SVCKS:     LD A,(TLBYTE)     ;CHECK THAT SLICING WAS USED TO REFER TO MULTI-DIM
           AND &40           ;STRING ARRAY
           JR Z,SWERHP       ;ERROR IF NOT.

SVCDIS:    IN A,(URPORT)
           PUSH AF           ;PAGE OF ARRAY DIM DATA
           PUSH BC           ;B=DIM COUNT (EXCLUDING FINAL DIM IF STRING)
           INC HL            ;PT TO FIRST DIM SIZE
           PUSH HL
           XOR A
           CALL STACKA       ;ZERO TOTAL OF FPCS
           POP HL            ;PTR TO DIMN. DATA IN BUFFER.
           POP BC            ;B=DIMS (1 OR MORE, EXCLUDING LAST DIM IF STRING)

SVLOOP:    POP AF
           PUSH AF
           OUT (URPORT),A    ;PAGE IN DIMS
           PUSH BC           ;DIM COUNTER IN B
           LD C,(HL)
           INC HL
           LD B,(HL)         ;BC=NEXT DIM SIZE=LIMIT VALUE FOR SUBSCRIPT
           INC HL
           PUSH HL           ;PTR TO DIMN. DATA
           PUSH BC           ;DIM SIZE
           CALL SELCHADP     ;PAGE IN SUBSCRIPT
           CALL STACKBC      ;DIM SIZE
           POP BC            ;BC=DIM SIZE AGAIN
           CALL GETSUBS      ;GET SUBSCRIPT IN HL, CHECKING IT'S >0 AND <=LIMIT,
                             ;THEN DECING IT.
SWERHP:    JP NC,SWER2       ;ERROR IF OUTSIDE LIMITS

           CALL STACKHL

           DB CALC           ;TOTAL,DIM SIZE,SUBS VAL
           DB SWOP13         ;SUBS VAL,DIM SIZE,TOTAL
           DB MULT
           DB ADDN           ;TOTAL*DIM SIZE+SUBS VALUE
           DB EXIT

           POP DE            ;ARRAY DATA PTR
           POP BC            ;DIM COUNTER
           RST &18
           DEC B
           JR Z,SVEXLP       ;JR IF ALL DIMS DONE

           CP ","
           JR NZ,SWER2       ;INSIST ON A COMMA NOW

           RST &20           ;SKIP ','
           EX DE,HL          ;HL PTS TO DIMN. DATA
           JR SVLOOP

;PAGE OF ARRAY START IS ON STACK, DE=ADDR OF ARRAY START

SVEXLP:    BIT 5,C
           JR NZ,SVNUMER     ;JR IF NUMERIC ARRAY

           POP AF
           OUT (URPORT),A    ;PAGE IN DIMS
           EX DE,HL          ;ALLOW LAST SUBSCRIPT OR SLICER, FOR STRINGS
           LD C,(HL)
           INC HL
           LD B,(HL)
           INC HL
           EX DE,HL          ;DE PTS TO ARRAY START
           PUSH BC           ;LAST SUBSCRIPT LEN
           CALL SVSR         ;GET START ADDR OF DESIRED STRING IN AHL
           POP BC
           EX DE,HL
           CALL STKST0       ;STORE STRING ADDR, BIT 7,A=0 ('DON'T ERASE OLD')
           CALL SELCHADP
           RST &18
           CP ")"
           JR Z,SVDIM        ;JR IF NO SLICE OF STRING SO FAR. E.G. A$(3)

           CP ","
           JR Z,SLCL         ;OK TO HAVE E.G. A$(3,2 TO 5)

SWER2:     RST &08           ;ANYTHING ELSE IS AN ERROR
           DB 4              ;'Subscript wrong'

SVDIM:     RST &20
           CP "("
           JR NZ,SLLPEX

SLCL:      RST &20           ;SKIP '(' OR ','

SLCL2:     CALL SLICING
           JR SVDIM

SLLPEX:    LD HL,FLAGS
           RES 6,(HL)        ;'STRING'
           RET


;END OF NUMERIC ARRAY

SVNUMER:   CP ")"
           JR NZ,SWER2

           RST &20           ;SKIP ')'
           LD BC,5
           POP AF            ;PAGE
           CALL SVSR
           JP TSURPG

;STACK BC (LAST DIMN. LEN, OR 5 FOR NUMBERS), MULT, ADD ARRAY 'TEXT' START
;EXIT WITH AHL=ADDR OF ELEMENT

SVSR:      PUSH AF           ;PAGE OF ARRAY START
           PUSH DE           ;ARRAY START
           CALL STACKBC

           DB CALC           ;TOTAL, LAST DIM SIZE
           DB MULT           ;DISP TO ELEMENT WANTED
           DB EXIT

           CALL UNSTLEN      ;AHL=PAGES/ MOD 16K FORM
           POP DE
           POP BC
           LD C,B            ;CDE=ADDR OF TEXT START (8000-C???)
           BIT 6,D
           JR Z,SVSR2        ;JR IF PAGE OK

           INC C             ;ELSE INC PAGE (ADDAHLCDE IGNORES BIT 6)

SVSR2:     JP ADDAHLCDE


SLICING:   CALL RUNFLG
           CALL C,STKFETCH   ;GET ADE=START, BC=LEN, IF RUNNING

           PUSH AF           ;PAGE
           RST &18
           POP HL            ;H=PAGE
           CP ")"
           JR Z,SLSTORE      ;JR IF SLICE WAS () (ENTIRE STRING)

           LD (TEMPB2),A     ;NZ SHOWS NO ERROR IN SUBSCRIPT YET
           PUSH DE           ;STRING START
           PUSH HL           ;H=PAGE OF START
           LD DE,0           ;DEFAULT SLICER START
           CP TOTOK
           JR Z,SLSEC        ;JR IF E.G: ( TO X) - USE DE=1

           CALL GETSUBS      ;ELSE EVAL E.G. S OF (S TO T) USING BC AS LIMIT
           EX DE,HL          ;DE=SUBS. VAL, CHECKED >0 AND <=LEN, THEN DECED
           RST &18
           CP TOTOK
           JR Z,SLSEC        ;WE HAVE FIRST NUMBER IN DE - JR IF 'TO' FOLLOWS IT

           CP ")"

NONSH:     JR NZ,SWER2     ;WAS NONS

           LD H,D
           LD L,E            ;LAST NUMB=FIRST NUMB IF EG (5)
           JR SLDEF          ;JR WITH NUMBERS IN HL AND DE

SLSEC:     RST &20
           CP ")"
           LD H,B
           LD L,C
           DEC HL            ;HL=LEN-1 (VALUES ALL USE 'DECED' FORM)
           JR Z,SLDEF        ;JR IF EG (X TO ) OR ( TO ) - USE LEN AS 2ND NUMB.

           PUSH DE           ;FIRST NUM
           CALL GETSUBS      ;EVAL SECOND NUMBER, CHECKING >0, <=LEN, DECING
           POP DE            ;FIRST NUMBER
           JR C,SLSE2        ;JR IF IN RANGE OR SYNTAX TIME

           LD A,H
           OR L
           JR Z,SLND         ;NULL STRING, NOT ERROR, IF E.G. (2 TO 0)

SLSE2:     PUSH HL
           RST &18
           POP HL            ;SECOND NUMB IN HL, FIRST IN DE
           CP ")"
           JR NZ,NONSH

SLDEF:     SBC HL,DE         ;SUB 2ND,1ST (NC HERE)
           LD BC,0           ;NUL LEN IF EG (5 TO 2)
           JR C,SLNUL

           LD A,(TEMPB2)
           AND A
           JP Z,SWER2

           INC HL

SLND:      LD B,H
           LD C,L            ;BC=STR LEN

SLNUL:     POP AF
           POP HL            ;STRING START IN AHL (HL=8000-BFFF)
           ADD HL,DE         ;ADD START, FIRST SLICER NUMBER-1 (DEFAULT=0)
           CALL C,PGOA       ;ADJUST FOR PAGE OVER FLOW IF NEEDED
           BIT 6,H
           JR Z,SLDF2

           RES 6,H
           INC A

SLDF2:     EX DE,HL          ;ADE=SLICER START, BC=LEN
           LD H,A

SLSTORE:   LD A,(FLAGS)
           AND &BF
           LD (FLAGS),A      ;'STRING'
           RLA
           RET NC            ;RET IF NOT RUNNING

           LD A,H            ;PAGE
           JP STKST0         ;STACK STRING, 'NO DELETE OF OLD'

;DIM.SAM

DIM:       CALL LOOKVARS
           IN A,(URPORT)     ;PAGE OF STRING/ARRAY IF FOUND
           EX AF,AF'         ;SAVE NZ IF FOUND
           PUSH BC
           CALL SCOPNM       ;CHADP BACK IN NOW
           POP BC            ;C=TYPE BYTE OF ARRAY
           LD A,(TLBYTE)     ;TYPE BYTE FOR DIM NAME (VARS MAY HOLD STRING)
           AND &60
           JP Z,NONSENSE     ;ERROR IF NO OPENING BRACKET USED

           CALL RUNFLG
           JR C,DIMRUN       ;JR IF RUNNING

           CALL SVDSK        ;CHECK N,N,...N)

DIM2:      CP ","
           RET NZ            ;RET UNLESS ANOTHER ARRAY FOLLOWS

           RST &20           ;SKIP ','
           JR DIM            ;ALLOW 'DIM A(8),B(6,5),A$(2)' ETC.

DIMRUN:    EX AF,AF'         ;PAGE OF STRLOCN
           PUSH BC           ;TYPE BYTE SAVED IN C
           CALL NZ,ASDL1     ;DELETE ARRAY POINTED TO BY STRLOCN IF IT EXISTS

           CALL SELCHADP
           POP BC            ;C=TYPE/LEN BYTE
           BIT 5,C
           LD BC,1
           JR Z,DIM4         ;JR IF A STRING ARRAY OR STRING FOUND IN VARS

           LD C,5

DIM4:      CALL STACKBC      ;ON EXIT B STILL=0...DIM COUNT
           DB &FE            ;'JR+1'

DIMSZLP:   RST &20           ;SKIP ','

           CALL GETSUBS      ;GET SUBS-1 IN HL
           INC HL
           PUSH HL           ;STACK DIM SIZE ON MACHINE STACK BEHIND DIM COUNTER
           PUSH BC
           CALL STACKHL      ;AND ON FPCS

           DB CALC
           DB MULT           ;GET E.G. 5*DIM1*DIM2 OR 1*DIM1
           DB EXIT

           POP BC
           INC B             ;INC DIM COUNTER
           RST &18
           CP ","
           JR Z,DIMSZLP

           CALL INSISCBRK    ;')'

           PUSH BC           ;B=DIMS
           LD L,B
           LD H,0            ;HL=DIMS
           ADD HL,HL         ;GET SPACE NEEDED BY WORD DIM SIZE INFO, PLUS 1
           INC HL            ;FOR NO. OF DIMS
           CALL STACKHL

           DB CALC           ;'TEXT' SIZE, DIM INFO SIZE
           DB SWOP           ;DIM INFO SIZE, 'TEXT' SIZE
           DB DUP            ;DIS, TS, TS
           DB SWOP13         ;TS, TS, DIS
           DB ADDN           ;TS, TS+DIS
           DB DUP            ;TS, TS+DIS, TS+DIS
           DB ONELIT
           DB 14             ;TS, TS+DIS, TS+DIS, 14
           DB ADDN           ;TS, TS+DIS, TS+DIS+14
           DB EXIT           ;ARRAY 'TEXT' SIZE, SIZE LESS HDR, TOTAL ARRAY SIZE

           CALL UNSTLEN      ;GET ABC=TOTAL LEN (PAGE FORM)
           CALL SAROOM       ;OPEN ABC BYTES AT END OF SAVARS, LDIR T/L BYTE,
                             ;NAME TO START, EXIT WITH DE PTING TO PAST NAME
           PUSH DE
           CALL UNSTLEN      ;SIZE EXCLUDING 14-BYTE HEADER
           EX DE,HL          ;ADE=SIZE INFO FOR AFTER 14-BYTE HDR
           POP HL
           LD (HL),A         ;PAGES
           INC HL
           LD (HL),E
           INC HL
           LD (HL),D         ;LEN MOD 16K
           INC HL
           POP AF
           LD (HL),A         ;DIM COUNT
           LD E,A
           LD D,0
           ADD HL,DE
           ADD HL,DE         ;PT TO LOCN FOR MSB OF LAST DIM
           LD D,H
           LD E,L            ;SAVE IT IN DE TOO

DIMENTLP:  POP BC            ;POP A DIM SIZE
           LD (HL),B
           DEC HL
           LD (HL),C         ;ENTER IT IN ARRAY HEADER
           DEC HL            ;(DIM SIZES COME OFF STACK IN REVERSE ORDER)
           DEC A
           JR NZ,DIMENTLP

           PUSH DE
           CALL UNSTLEN
           CALL AHLNORM      ;GET LEN-TO-CLEAR AS 19-BIT NUMBER
           POP DE
           EX DE,HL          ;ADE=19 BIT NO.
           INC HL            ;HL PTS TO START OF AREA TO CLEAR
           LD B,E            ;B=LEN MOD 256
           LD E,D
           LD D,A            ;DE=256-BYTE PAGES
           LD A,B
           AND A
           JR Z,DIMNAC

           INC DE            ;INC DE (UNLESS B=0). ALLOWS B AND DE TO ACT AS
                             ;SEPARATE COUNTERS. DE IS *NEVER* ZERO
DIMNAC:    LD A,(TLBYTE+33)
           AND &40           ;Z IF NUMERIC
           LD C," "
           JR NZ,GARC        ;JR IF STR ARRAY, C=' ' FOR CLEARING ARRAY

           LD C,A            ;USE ZERO TO CLEAR NUMERIC ARRAYS

GARC:      CALL CHKHL        ;CHECK WE ARE IN 8000-BFFF AREA

DIMCLP:    LD (HL),C
           INC HL
           DJNZ DIMCLP

           DEC DE
           LD A,D
           OR E
           JR NZ,GARC

           CALL SELCHADP
           RST &18
           JP DIM2


SAROOM:    PUSH AF
           CALL ADDRELND     ;ADDRESS ELINE AND DEC PTR - PT TO END OF SAVARS
           POP AF
           CALL MKRBIG       ;OPEN ABC BYTES (PAGE FORM) AT (HL)
           EX DE,HL          ;DE PTS TO ROOM
           LD HL,TLBYTE+33
           LD BC,11
           LDIR              ;COPY TYPE/LEN AND NAME TO SAVARS
           RET

;ENTRY: CHAD PTS TO A SUBSCRIPT VALUE. BC=LIMIT

GETSUBS:   PUSH BC
           CALL EXPT1NUM
           JR C,GTSBC

           POP BC
           RET               ;JUST CHECK FOR A NUMBER IN SYNTAX TIME

;ENTRY: CHAD PTS TO A SUBSCRIPT VALUE. RUN TIME!
;ACTION: GET A SUBSCRIPT VALUE IN HL, CHECKING IT IS <= LIMIT IN BC, AND <>0
;BC AND DE UNCHANGED. HL DECED BEFORE RETURN

;GTSUBS:    PUSH BC
 ;          CALL EXPT1NUM

GTSBC:     CALL GETINT       ;IN HL, A=L
           POP BC            ;LIMIT VALUE
           OR H
           JR Z,SWSIG        ;SUBSCRIPT 0 IS ALWAYS AN ERROR

           DEC HL            ;REDUCE BY 1 TO GIVE ALLOWED RANGE 0 TO LIMIT-1
           SBC HL,BC         ;IF BC=FFFF, 1-FFFF IS OK
           ADD HL,BC
           RET C

SWSIG:     XOR A
           LD (TEMPB2),A
           RET
