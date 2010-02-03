;MISCX2.SAM

           ORG INSTBUF

DKP2:      CALL EXPT1NUM     ;NUMBER OF KEY CODE TO DEFINE
                             ;(KEYBOARD HAS 69 KEYS WITH 3 SHIFTS (CAPS, SYM,
                             ;CONTROL) GIVING 276 BYTES IN KEY MAP, ANY OF WHICH
                             ;CAN BE PROGRAMMED TO GIVE ANY KEY CODE. KEY CODES
                             ;ABOVE 168? CAN BE DEFINED
           CALL COMMASC      ;SEE IF COMMA OR SEMI-COLON
           JR Z,DFK4         ;JR IF STRING DEFINITION SHOULD FOLLOW

           CALL RUNFLG
           JP NC,DFKNL       ;JP IF SYNTAX TIME - CHECK, STRIP FP FORMS

           EX DE,HL          ;DE=CHAD
           LD HL,(NXTLINE)
           SCF
           SBC HL,DE
           LD B,H
           LD C,L            ;ASSUMING NXTLINE HAS SAME PAGE PART AS CHAD,
                             ;BC=LEN OF REST OF LINE
           DEC BC            ;DEC BECAUSE WE DON'T USE ':' SEPARATOR
           INC DE            ;SKIP ':'
           LD HL,LINEEND
           EX (SP),HL        ;JUNK NEXT STAT, STACK LINEEND SO WE DON'T
           JR DFK5           ;EXECUTE THE REST OF THIS LINE

DFK4:      CALL SSYNTAXA     ;DEFINITION STRING

           CALL GETSTRING    ;BC=LEN, DE=ST, PAGED IN

DFK5:      PUSH DE
           PUSH BC
           CALL GETBYTE      ;KEY CODE
           INC A             ;255->0
           CP 193            ;ONLY KEY CODES 192-254 ARE DEFINABLE
           JP C,IOORERR

           DEC A
           PUSH AF
           CALL FNDKYD       ;FIND ANY EXISTING KEY DATA
           JR C,DFK55        ;JR IF DOESNT EXIST, ELSE CLOSE IT UP

;DEF KEY CLOSE
;CLOSES BC+3 BYTES AT HL-3 (I.E. ENTRY WITH HL PTING TO TEXT, BC=TEXT LEN,
;ERASES ENTIRE DEFINITION)

           INC BC
           INC BC
           INC BC
           PUSH HL           ;DEF TEXT START
           ADD HL,BC         ;PT TO PAST END OF DEFINITION,+3
           PUSH HL
           CALL DKTR         ;GET HL PTING TO TERMINATOR+3
           POP DE
           AND A
           SBC HL,DE
           INC HL
           LD B,H
           LD C,L            ;BC=BYTES TO MOVE (PAST END TO TERMINATOR, PLUS 1)
           EX DE,HL
           DEC HL
           DEC HL
           DEC HL            ;PT TO PAST DEF END
           POP DE
           DEC DE
           DEC DE
           DEC DE            ;PT TO DEF START
           LDIR

DFK55:     POP AF            ;KEY CODE
           POP BC            ;DEF$ LEN
           PUSH AF
           LD A,B
           CP &FF
           JR Z,DFK6         ;ABORT IF WAS E.G. DEF KEY 1 (CR). LEN IS FFFFH

           OR C              ;ALSO ABORT IF LEN 0

DFK6:      JP Z,PPRET        ;POP POP RET - JUNK KEY, SRC, ABORT

;DEF KEY OPEN
;ENTRY: BC=TEXT LEN (BC+3 IS OPENED TO ALLOW FOR KEY CODE AND LEN)
;EXIT: DE PTS TO SPACE, BC IS UNCHANGED

           PUSH BC
           CALL DKTR         ;HL=TERMINATOR ADDR+3
           POP BC
           PUSH HL
           ADD HL,BC         ;HL=NEW POSN FOR TERMINATOR. NC
           EX DE,HL
           LD HL,(DKLIM)
           SBC HL,DE
           JP C,TMDERR       ;ERROR IF TERMINATOR PAST LIMIT
                             ;ALLOWS TERMINATOR TO BE AT LIMIT...
           EX DE,HL
           LD (HL),&FF       ;NEW TERMINATOR (OLD WILL BE OVER-WRITTEN)
           POP DE
           DEC DE
           DEC DE
           DEC DE            ;PT TO OLD TERMINATOR

           POP AF            ;KEY CODE
           POP HL            ;SRC
           LD (DE),A
           INC DE
           LD A,C
           LD (DE),A
           INC DE
           LD A,B
           LD (DE),A         ;LEN IN PLACE
           INC DE
           LDIR
DKFIN:     RET

DKLN:      EQU DKFIN+1-DKP2


RDLN:      EQU 0   ;RDFIN+3-RDP2

;DEF FN COMMAND (OR MARKER - DOES NOTHING EXCEPT CHECK SYNTAX)

           ORG INSTBUF

DFNP2:     CALL RUNFLG
           JP C,SKIPCSTAT    ;SKIP STATEMENT IF RUNNING

           RST &18
           CALL FNNAME
           PUSH AF           ;A=1 IF STRING DEF FN
           LD (CHAD),HL
           RST &18
           CP "("
           JR NZ,DFN5

           RST &20           ;SKIP '('
           CP ")"
           JR Z,DFN4

DFNPL:     CALL GETALPH      ;INSIST ON A LETTER
           INC HL            ;SKIP VAR LETTER
           LD A,(HL)
           CP "$"
           JR NZ,DFN3

           INC HL            ;SKIP '$'

DFN3:      CALL MAKESIX
           LD (CHAD),DE      ;PT TO LAST OF 5-BYTES
           RST &20
           CP ")"
           JR Z,DFN4

           CALL INSISCOMA
           JR DFNPL

DFN4:      RST &20

DFN5:      CP "="
           JR NZ,DFNNS

           CALL SEXPTEXPR
           POP BC
           JR Z,DFN6         ;JR IF EXPR ASSIGNED TO DEF FN IS STRING

           DEC B
           RET NZ            ;RET IF B WAS NOT 1

DFN6:      DEC B             ;ERROR IF B<>1 (NAME<>STRING TYPE)
           RET Z

DFNNS:     RST &08
           DB 29             ;'Syntax error'

DFNLN:     EQU DFNNS+2-DFNP2
           ORG CDBUFF+&80

TOKPT2:    EXX
           EX DE,HL          ;E-LINE START OR WKSPACE START, OR OTHER,  TO HL

TOKRST:    LD A,(HL)         ;JR TO PT CHAD TO LINE START
           JR TOKQUEN

LOOKNA:    POP HL
           LD A,(HL)
           INC HL
           CP "A"
           JR C,TOKQUEN      ;JR IF WE JUST CONSIDERED '>' OR '<'

           DB &FE            ;'JR+1'

LKNONALP:  INC HL
           LD A,(HL)
           CALL ALPHA
           JR C,LKNONALP     ;LOOP WHILE LETTER

           CP "_"            ;CHECK FOR LETTER, UNDERLINE OR DOLLAR

           JR Z,LKNONALP     ;LOOP WHILE UNDERLINE

TOKQUEN:   LD (CHAD),HL      ;RESET CHAD
           DB &FE            ;'JR+1'

TOKMLP:    RST &20
           EX DE,HL
           CP &0D
           RET Z             ;RET IF END MARKER FOUND

           CALL ALPHA
           JR C,POSFIRST     ;JR IF A POSSIBLE FIRST LETTER FOUND

           CP "<"
           JR Z,POSFIRST     ;COULD BE '<=' OR '<>'

           CP ">"
           JR Z,POSFIRST     ;COULD BE '>='

           INC DE
           CP &FF
           JR Z,FNTS         ;JR WITH (DE)=FN LETTER IF FN LDR

           DEC DE
           CP &22            ;QUOTE
           JR NZ,TOKMLP

QUOTELP:   INC DE
           LD A,(DE)
           CP &0D
           RET Z

           CP &22
           JR NZ,QUOTELP

FNTS:      INC DE
           EX DE,HL
           LD A,(HL)
           JR TOKQUEN

POSFIRST:  PUSH DE
           EX DE,HL
           LD DE,TOKFIN+3    ;END OF THIS ROUTINE, IN CDBUFF
           LD BC,15          ;MAX LEN TO TOKENIZE
           PUSH DE
           LDIR
           POP DE
           LD HL,KEYWTAB-1
           LD A,KEYWNO+1     ;WORDS TO CHECK, PLUS 1
           CALL JGTTOK       ;GET A=1 TO KEYWNUMBER, IF MATCHED
           JR NZ,YGOTM

           LD HL,(MTOKV)
           INC H
           DEC H
           CALL NZ,HLJUMP

           JR Z,LOOKNA       ;JR IF NO MATCH FOUND, OR MTOKV ZERO

YGOTM:     EX DE,HL          ;ELSE HL=WORD START, DE=PAST END (IN BUFFER)
           AND A
           SBC HL,DE         ;HL=LEN
           POP DE            ;ELINE PTR
           ADD HL,DE
           EX DE,HL          ;HL PTS TO START, DE TO PAST END, IN ELINE

           CP &4A
           JR NC,TOK42       ;JR IF IN RANGE OF CMDS

           ADD A,&3A         ;ELSE GET FN CODE 3BH-83H
           LD (HL),&FF       ;OVERWRITE FIRST LETTER WITH FN LEADER
           INC HL
           JR TOK55

TOK42:     ADD A,&3B         ;CONVERT LIST ENTRY TO TOKEN CODE 85H-
           CP &FF            ;'INK'
           JR NZ,TOK43

           LD A,&A1          ;PENTOK - ALLOW 'PEN' TO BE ENTERED AS 'INK'

TOK43:     DEC HL            ;PT TO BEFORE WORD IN INPUT
           EX AF,AF'         ;SAVE TOKEN
           LD A,(HL)
           CP " "
           JR Z,TOK5         ;AVOID INC IF THERE IS A LEADING SPACE - SO IT
                             ;WILL BE OVER-WRITTEN BY TOKEN
           INC HL            ;PT TO FIRST LETTER

TOK5:      EX AF,AF'         ;RESTORE TOKEN

TOK55:     LD (HL),A         ;PLACE AT START OF SPELLED-OUT FORM
           INC HL            ;HL PTS TO FIRST BYTE TO DELETE
           EX DE,HL
           LD A,(HL)
           CP " "
           JR NZ,TOK6        ;JR IF NO TRAILING SPACE, ELSE INC HL TO INCLUDE
                             ;SPACE IN 'CLOSED UP' REGION
           INC HL

TOK6:      PUSH DE
           CALL RECLAIM1     ;CLOSE UP (DE) <- (HL)
           POP DE            ;KEEP LOOKING FROM HL ONWARDS
           LD H,D
           LD L,E
           DEC DE
           LD A,(DE)
           CP &B7            ;REMTOK. DON'T TOKENIZE REM STATEMENTS
           JP NZ,TOKRST      ;KEEP TOKENISING FROM HL ONWARDS

TOKFIN:    RET

TOKLN:     EQU TOKFIN+1-TOKPT2

           ORG INSTBUF

MEPRO2:    CALL SETWORK      ;CLEAR WORKSP IN CASE E.G. MERGE A$+B$
           LD BC,1
           CALL WKROOM       ;OPEN 1 BYTE IN WORKROOM (NOTE: ENSURES WKEND MOVED
                             ;BY MKRBIG)
           LD (HL),&FF       ;TERMINATOR
           PUSH HL
           CALL RDLLEN       ;CDE=FILE LEN TO LOAD
           POP HL
           PUSH BC
           PUSH DE
           LD A,C
           LD B,D
           LD C,E
           CALL MKRBIG       ;OPEN ABC BYTES
           POP DE
           POP BC
           SCF               ;'LOAD' NOT 'VERIFY'
           CALL JLDVD        ;LOAD CDE TO (HL)
           CALL MBASLNS
           CALL MNUMS
           LD HL,HDL+16
           CALL RDTHREE      ;CDE=LEN OF PROG ALONE
           PUSH BC
           PUSH DE
           LD L,(HDL+22)\256
           CALL RDTHREE      ;CDE=LEN OF PROG+NVARS+GAP
           LD A,C
           EX DE,HL          ;AHL=DITTO
           POP DE
           POP BC
           CALL SUBAHLCDE    ;AHL=LEN OF NVARS+GAP
           LD BC,1
           CALL ADDAHLBC     ;ALLOW FOR BASIC PROG TERMINATOR
           PUSH AF
           PUSH HL
           CALL ADDRWK
           POP BC
           POP AF
           CALL RECL2BIG     ;DELETE LOADED NVARS+GAP

;MERGE STRINGS/ARRAYS (PART OF PROGRAM FILE)
;ENTRY: DEST PTS TO LOADED DATA

MSTAR:     CALL ADDRWK       ;PT TO SRC (TLBYTE OR VAR, OR FF)
           LD A,(HL)
           INC A
           JP Z,GT4R         ;END IF ALL DONE - TERMINATOR HIT

           LD DE,TLBYTE
           AND &0F
           LD C,A
           LD B,0
           LD A,(HL)
           LDIR              ;COPY TLBYTE AND NAME TO BUFFER
           CALL STARYLK2     ;CALL WITH A=DESIRED T/L
           CALL NZ,ASDEL2    ;DELETE STRING/ARRAY AT (STRLOCN) IF IT WAS FOUND

           CALL ADDRWK       ;SRC
           LD BC,11
           ADD HL,BC         ;PT TO LEN DATA
           LD A,(HL)
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)
           EX DE,HL
           LD C,14
           CALL ADDAHLBC     ;AHL=TOTAL LEN OF STR/ARRAY (PAGEFORM)
           PUSH AF
           PUSH HL
           CALL ADDRELND
           POP BC
           POP AF
           PUSH AF
           PUSH BC
           CALL MKRBIG       ;MAKE ABC BYTES AT HL FOR STR/ARRAY
           EX DE,HL
           IN A,(251)
           LD C,A            ;ROOM IS AT CDE
           POP HL
           RES 7,H
           LD (MODCOUNT),HL
           POP AF
           LD (PAGCOUNT),A
           CALL ADDRWK       ;PT TO SRC
           IN A,(251)        ;AHL=SRC, CDE=DEST
           CALL FARLDIR      ;COPY TO DEST
           CALL ADDRWK
           CALL ASDEL3       ;DELETE SRC
           JR MSTAR

;MERGE BASIC LINES

MBASLNS:   CALL ADDRWK       ;PT TO LOADED BLOCK (SRC)

MPRG1:     LD B,(HL)
           INC HL
           LD A,B
           INC A
           RET Z             ;RET IF ALL LINES DONE

           LD C,(HL)
           CALL FNDLP        ;FIND LINE BC, STARTING FROM PROG
           PUSH HL
           IN A,(251)
           PUSH AF           ;DESTINATION VARS
           CALL Z,NORECL     ;RECLAIM IF IT EXISTS

           CALL ADDRWK       ;PT TO SRC
           INC HL
           INC HL
           LD C,(HL)
           INC HL
           LD B,(HL)
           INC BC
           INC BC
           INC BC
           INC BC            ;BC=TOTAL SRC LINE LEN
           POP AF
           POP HL
           PUSH HL
           PUSH AF
           PUSH BC
           CALL TSURPG
           CALL MAKEROOM     ;MAKE ROOM FOR LINE
           CALL ADDRWK       ;AHL=SRC
           POP BC            ;LEN
           CALL SPLITBC
           POP BC
           LD C,B
           POP DE            ;CDE=DEST
           CALL FARLDIR      ;COPY TO PROGRAM
           CALL ADDRWK       ;PT TO SRC
           CALL NORECL       ;DELETE LINE FROM WORKSP
           JR MPRG1

;MERGE NUMBERS - WORKS WITH 16K OR LESS OF NUMERIC VARS.

MNUMS:     LD A,"a"

MNUML:     LD (FIRLET),A
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC D
           CALL NZ,MNUMSR    ;CALL IF LETTER LIST HAS BEEN USED

           INC HL
           LD A,(FIRLET)
           INC A
           CP "z"+1
           JR C,MNUML

           RET

MNUMSR:    PUSH HL           ;SRC PTRS LIST POSN

MNSRL:     DEC D
           ADD HL,DE
           LD A,(HL)
           AND &A0           ;MASK ALL EXCEPT 'HIDDEN' AND 'UNUSED' BITS
           JR NZ,MNSR3       ;DO NOT DEAL WITH SUCH VARS

           IN A,(251)
           PUSH AF
           PUSH HL
           LD A,(HL)
           AND &1F
           LD C,A
           LD A,(HL)
           RES 6,A           ;'NOT A FOR-TYPE VARIABLE'**
           LD (TLBYTE),A
           INC HL
           INC HL
           INC HL            ;PT TO NAME SECOND LET, OR VALUE
           JR Z,MNSR2        ;JR IF 1-LET NUMBER

           LD B,0
           LD DE,FIRLET+1
           LDIR              ;COPY REST OF NAME TO BUFFER

MNSR2:     CALL HLTOFPCS
           CALL CRTVAR4      ;CREATE NUMERIC VAR WITH VALUE ON FPCS, NAME IN
                             ;BUFFER, TYPE IN A
           POP HL            ;TLBYTE OF SRC VAR JUST DEALT WITH
           POP AF
           OUT (251),A

MNSR3:     INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC D
           JR NZ,MNSRL       ;JR IF MORE VARS IN LIST

           POP HL            ;LIST POSN
MEEND:     RET

MELN:     EQU MEEND+1-MEPRO2

 ORG &C000+RENLN+GETLN+DELLN+KEYLN+POPLN+INPLN+DKLN+RDLN+DFNLN+TOKLN+MELN

                                 ;MERGE, EOF, PTR, PATH$, FN STUBS, LINK
