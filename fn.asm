;FN.SAM
;COMPILE DEF PROCS

ELCOMAL:   LD A,(REFFLG)
           CP 1              ;CY IF ZERO
           CCF               ;CY IF NZ (FN USED)

COMALL:    CALL C,COMDF      ;COMPILE FNS

COMDP:     CALL COMLEN       ;SWITCH IN PROG, GET BC AND B' AS PROG OR ELINE LEN

CMDPL:     LD D,&FD
           CALL LKCALL       ;LOOK FOR PROC CALLING BUFFER FROM HL ONWARDS
           RET C             ;RET IF NO MORE
                             ;ELSE HL POINTS TO BUFFER, PAGED IN
           PUSH BC           ;BYTES LEFT
           CALL LOOKDP       ;LOOK FOR DEF PROC name, ALTER CALLING BUFFER TO
           POP BC            ;PAGE/ADDR IF FOUND, ELSE FLAG AS 'NO DEF PROC'
           JR CMDPL

;COMPILE DEF FNS
;FIRST, DO A PASS TO MAKE A TABLE OF ALL DEF FNS AS PAGE/ADDR; LOOKING
;THROUGH THE PROGRAM EACH TIME IS TOO SLOW.

COMDF:     LD HL,INSTBUF
           LD (TEMPW1),HL    ;INIT PTR TO TABLE STORE
           CALL ADDRPROG     ;SWITCH IN PROG
           LD A,(HL)
           INC A
           RET Z             ;RET IF NO PROGRAM

           INC HL
           INC HL
           INC HL

DFPPL:     INC HL
           LD (CHAD),HL      ;CHAD STARTS AT 1ST CHAR OF FIRST LINE, LATER
                             ;IS RESET TO JUST AFTER EACH DEF FN FOUND
           LD E,&C8          ;DEFFNTOK
           CALL SRCHPROG     ;LOOK FOR DEF FN FROM CHAD ON
           JR NC,CMDF2       ;JR IF ALL DONE
                             ;ELSE HL POINTS PAST 'DEF FN'
           EX DE,HL
           DEC DE            ;DE PTS TO 'DEF FN'
           LD HL,(TEMPW1)
           LD BC,-INSTBUF-509
           ADD HL,BC
           JR C,TMDERR       ;'Too many definitions' IF PTR >=INSTBUF+509
                             ;ALLOWS UP TO 170 DEF FNS.
           SBC HL,BC
           IN A,(251)
           LD (HL),A
           INC HL
           LD (HL),E
           INC HL
           LD (HL),D
           INC HL
           LD (TEMPW1),HL
           EX DE,HL
           JR DFPPL

;USED BY 'DEF FN' COMPILER AND 'DEF KEYCODE'

TMDERR:    RST &08
           DB 52             ;'Too many definitions'

CMDF2:     CALL COMLEN       ;SWITCH IN PROG, GET BC AND B' AS PROG OR ELINE LEN

CMDFL:     LD D,&FE
           CALL LKCALL       ;LOOK FOR CALLING BUFFER FROM HL ONWARDS
           RET C             ;RET IF NO MORE
                             ;ELSE HL POINTS TO BUFFER, PAGED IN
           PUSH BC           ;PROG LEN LEFT (MOD 16K)
           CALL LOOKDF       ;LOOK FOR DEF FN name, ALTER CALLING
           POP BC            ;BUFFER TO PAGE/ADDR IF FOUND, OR FLAG 'NO DEF FN'
           JR CMDFL


COMLEN:    LD A,(COMPFLG)
           RLA
           JR C,PRGLEN       ;JR IF PROGRAM BEING COMPILED

           CALL ADDRELN      ;ELSE IT IS ELINE
           PUSH HL           ;ELINE
           EX DE,HL
           LD C,A            ;CDE=ELINE
           LD HL,(WORKSP)
           LD A,(WORKSPP)
           JR CPLENC

;SWITCH IN PROG, GET B'=8K BLOCKS IN PROG LEN, PLUS 1, BC=LEN MOD 8K

PRGLEN:    CALL ADDRPROG
           PUSH HL           ;PROG
           EX DE,HL
           LD C,A            ;CDE=PROG
           LD HL,(NVARS)
           LD A,(NVARSP)

CPLENC:    CALL SUBAHLCDE    ;GET PROG LEN (PAGE FORM)
           PUSH HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           RLA               ;A=8K BLOCKS
           EXX
           INC A
           LD B,A
           EXX
           POP BC
           LD A,B
           AND &1F
           LD B,A            ;BC=LEN MOD 8K
           POP HL            ;PROG
           RET

;LKCALL - LOOK FOR PROC OR FN CALL BUFFER
;BY CHECKING FOR NOT-0E 0E FD/FE FD/FE PG+80H/ADDR/LETTER.
;NO CONFUSION WITH NORMAL 0E FORMS BECAUSE THOSE ARE NOT FOLLOWED BY A LETTER,
;OR PRECEDED BY 0EH.
;NO CONFUSION WITH LINE NUMBERS BECAUSE 0E FE FE 80 = LINE LEN >32768!

;ENTRY: HL=START OF SEARCH, D=TARGET (FD/FE=PROC/FN), BC=LEN TO SEARCH, MOD 8K
;B'=8K BLOCKS (ALLOWS ROOM AFTER TARGET FOUND, TO INCREMENT PTRS)
;EXIT: HL POINTS TO LOCN FOR 'PAGE' IN CALLING BUFFER, BUFFER HOLDS LEN/NAME
;OF CALLING NAME, IF NC, ELSE CY SHOWS NO TARGETS FOUND.

LKCALL:    LD A,B
           OR C
           JR Z,LPC5         ;JR IF TIME FOR NEXT BLOCK

           LD A,D            ;FE IF FN BUFFER WANTED, FD IF PROC CALL BUFFER
           CPIR
           JR NZ,LPC5        ;JR IF FD/FE NOT FOUND (BC=0)

           CP (HL)           ;CHECK FOR SECOND FDH/FEH
           JR NZ,LKCALL      ;KEEP LOOKING IF RED HERRING

           DEC HL
           DEC HL
           LD A,&0E
           CP (HL)           ;THERE SHOULD BE A PRECEDING 0EH
           JR Z,LPC3

LPC2:      INC HL
           INC HL
           JR LKCALL         ;IF THERE IS NOT, CONTINUE SEARCH

LPC3:      DEC HL
           CP (HL)           ;THERE SHOULD ONLY BE *ONE* PRECEDING 0EH
           INC HL            ;(EXCLUDE SPURIOUS 0E 0E FE FE 80 41, SAY)
           INC HL
           INC HL
           JR Z,LKCALL       ;LOOP BACK IF SPURIOUS (JUST AN ODD NUMBER)

           INC HL            ;PT TO PROBABLE PAGE
           LD A,(HL)
           DEC HL            ;TO SECOND FD/FE AGAIN
           RLA
           JR NC,LKCALL      ;JR IF NOT 0E FE FE (>=80H).
                             ;(AT SYNTAX CHECK, CALLING BUFFER IS CREATED WITH
                             ;'PAGE' FD OR FEH)
           PUSH HL           ;PTR TO SECOND FD/FE
           PUSH BC           ;LEN LEFT TO SEARCH FOR OTHER BUFFERS
           DEC HL            ;FIRST FD/FE
           DEC HL            ;0EH
           LD BC,&00FF       ;NAME LEN WILL INC TO ZERO START VALUE. B=0

FDFLP:     DEC HL
           INC C             ;NAME LEN
           LD A,(HL)
           CP "$"
           JR Z,FDFLP        ;NAME CAN END IN '$'

           CALL ALNUMUND     ;CHECK FOR VALID NAME CHARS (ALPHA-NUMERICS OR '_')
           JR C,FDFLP        ;LOOP BACK PAST THE NAME, TO THE FFH FN LEADER,
                             ;SPACE, CC, MSB OF LINE LEN OR ':', OR FFH AT
                             ;SAVARS END (IF PROC)
           INC A
           JR NZ,LPC4        ;JR IF NOT FF 42 ('FN')

           LD A,D
           CP &FD
           JR Z,LPC4         ;JR IF PROC

           INC HL            ;SKIP FFH
           DEC C             ;DEC LEN BECAUSE OF FALSE INCLUSION OF 42H

LPC4:      INC HL            ;PT TO FIRST NAME CHAR
           LD DE,NMBUFF
           LD A,C
           DEC A             ;**
           AND &1F           ;**
           INC A             ;** LIMIT LDIR LEN
           LD C,A            ;**
           LD (DE),A         ;LEN AT BUFFER START
           INC DE
           LDIR              ;COPY NAME TO BUFFER+1
           POP BC            ;BYTES LEFT
           POP HL            ;PTR TO SECOND FD/FE (WHERE CPIR HALTED)
           RET

LPC5:      LD B,&20          ;C=0. DO ANOTHER 8K. (IF BLOCKS NOT ZERO YET)
           CALL CHKHL
           EXX
           DEC B
           EXX
           JR NZ,LKCALL      ;DO B' BLOCKS
           SCF               ;NO MORE CALLING BUFFERS
           RET

;LOOK DEF FN
;LOOK FOR 'DEF FN' FOLLOWED BY A SPECIFIC NAME, AND PATCH FN CALL BUFFER

LOOKDF:    PUSH HL           ;CALLING BUFFER ADDR (SECOND FD/FE)
           IN A,(251)
           PUSH AF           ;CALLING BUFFER PAGE
           LD HL,INSTBUF     ;START OF TABLE OF PAGE/ADDR FOR EACH DEF FN

LKDFLP:    LD BC,(TEMPW1)    ;END OF TABLE (PAST LAST ENTRY)
           AND A
           SBC HL,BC         ;NC IF PTR HAS REACHED END
           ADD HL,BC
           JR NC,LKDP4       ;JR IF ALL TABLE ENTRIES TRIED WITHOUT SUCCESS.
                             ;MARK BUFFER 'NO DEF FN'
           LD A,(HL)         ;PORT VALUE
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC HL
           OUT (251),A
           PUSH HL
           CALL MATCHER      ;MATCH DE+1 VS. BUFFER
           POP HL
           JR C,LKDFLP       ;LOOP IF MATCH FAILED

           JR LKDP3          ;ELSE JR AND PATCH BUFFER


;LOOK DEF PROC
;LOOK FOR 'DEF PROC' FOLLOWED BY A SPECIFIC NAME, AND PATCH PROC CALL BUFFER
;AT HL WITH PAGE/ADDR OF ADDR AFTER NAME, OR PAGE FFH IF NOT FOUND
;ENTRY: NAME LEN IS IN BUFFER, FOLLOWED BY NAME. HL POINTS TO PROC CALL BUFFER,
;SWITCHED IN

LOOKDP:    PUSH HL           ;CALLING BUFFER ADDR
           IN A,(251)
           PUSH AF           ;CALLING BUFFER PAGE
           CALL ADDRPROG     ;START AT (PROG)
           DB &FE            ;'JR +1'

LKDPLP:    ADD HL,DE         ;PT TO START OF NEXT LINE

           LD BC,&2100+&CA   ;DEFPROCTOK
           CALL LKFC         ;LOOK FOR A DEF PROC AT LINE STARTS
           JR C,LKDP4        ;JR IF NONE FOUND - MARK BUFFER 'NO DEF PROC'
                             ;ELSE HL PTS TO FIRST CHAR IN LINE,
                             ;CHAD PTS TO 'DEF PROC'

           PUSH DE           ;LEN OF TEXT
           PUSH HL           ;START OF TEXT IN THIS LINE
           LD DE,(CHAD)
           CALL MATCHER      ;MATCH (DE+1) VS. (BUFFER+1)
           POP HL
           POP DE
           JR C,LKDPLP       ;JR IF FAILED TO MATCH

           DEC HL
           DEC HL
           DEC HL
           DEC HL
           EX DE,HL          ;ELSE PT DE TO LINE START

LKDP3:     LD B,&80

LKDP35:    IN A,(251)
           AND &1F
           OR B
           LD B,A            ;B=PAGE WITH DEF PROC NAME, BIT 7 SET
           DB &21            ;'JR+2'

LKDP4:     LD B,&FF          ;'NO DEF PROC/DEF FN'

           POP AF
           OUT (251),A       ;BACK TO PROC CALL PAGE
           POP HL
           PUSH HL
           INC HL
           LD (HL),B         ;PAGE
           INC HL
           LD (HL),E
           INC HL
           LD (HL),D         ;ADDR OF PAST NAME OR LINE START (OR JUNK IF B=FF)
           POP HL            ;PTR TO SECOND FD/FE (WHERE CPIR STOPPED)
           RET

;MATCH (HL) VS (FIRLET) OVER T/L+1 BYTES (NAMELEN)
;EXIT AS MATCHER - NOTE *DE* PTS PAST NAME

MATCHERF:  LD DE,TLBYTE

;CALLED BY FOR WITH DE=TLBYTE+33

MATCHFN:   EX DE,HL
           LD A,(HL)
           AND &1F
           INC A
           LD B,A            ;B=NAME LEN
           INC HL
           JR MTCCM

;MATCH NAME AT (DE+1) VS NAME AT BUFFER+1 OVER (BUFFER) BYTES. SPACES IN
;(DE+1) NAME IRRELEVANT (BUT PROC AND FN NAMES HAVE NO SPACES). SPACES
;SHOULD NOT BE PRESENT IN BUFFER NAME.
;EXIT: NC IF MATCHED OK, DE PTS PAST CANDIDATE NAME, HL PAST BUFFER NAME
;USES HL, B, A. MOVES DE

MATCHER:   LD HL,NMBUFF      ;NAME WE ARE LOOKING FOR IS AT BUFFER+1
           LD B,(HL)         ;NAME LEN
           INC HL

MSKIP:     INC DE

MTCCM:     LD A,(DE)
           CP &20
           JR Z,MSKIP        ;SKIP ANY SPACES IN CANDIDATE NAME
                             ;MIGHT BE USED BY PROC SRS??!!
           XOR (HL)
           INC HL
           AND &DF           ;IGNORE CASE MISMATCH.
           SCF
           RET NZ            ;RET IF FAILED (CY)

           DJNZ MSKIP        ;MATCH B SIGNIF. CHARS

           INC DE
           LD A,(DE)         ;WE MATCHED OK SO FAR - BUT CANDIDATE NAME MUST
           JP ALNUMUND       ;END NOW - GET CY IF NOT TERMINATED

;FN HANDLING (CALLED BY EVALUATOR)
;E.G. PRINT FN OCTAL(345)
;     PRINT FN OCTAL 0E FE FE PG+80H ADDRL ADDRH (345 0E 1 2 3 4 5)
;     PAGE AND ADDR ARE PTR TO '(' OR '=' IN DEF FN

IMFN:      CALL RUNFLG
           JP NC,FNSYN       ;JR IF NOT RUNNING

           RST &20           ;SKIP 'FN'
           LD A,&0E

FNRL:      CP (HL)
           INC HL
           JR NZ,FNRL         ;LOOP TILL 0EH FOUND

           INC HL            ;SKIP FEH
           INC HL            ;SKIP FEH
           LD B,(HL)         ;PAGE OF DEF FN, +80H
           BIT 5,B
           JR NZ,MDFERR      ;ERROR IF NO DEF FN

           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)         ;ADDR OF PAST NAME IN DEF FN
           LD (CHAD),HL      ;PT CHAD TO END OF BUFFER
           RST &20
           CP "("
           JR NZ,FNBF2

           CALL FORESP       ;GET SIGNIF CHAR AFTER '('
           CP ")"
           JR NZ,FNBF        ;JR IF NO '()' TO SKIP

           RST &20           ;SKIP '('
           RST &20           ;SKIP ')'

FNBF:      CP A              ;Z

FNBF2:     EX AF,AF'         ;Z IF FN HAS BRACKETS (IF EMPTY, CHAD PTS PAST)
           EX DE,HL
           LD A,B
           CALL TSURPG       ;HL POINTS TO '(' OR '=' IN DEF FN E.G.
                             ;DEF FN TEST=123 OR DEF FN TEST(A,B)=A*B
           LD A,(HL)
           SUB "="
           JR NZ,FNBC

           PUSH AF           ;DEFADD=00XX
           EX AF,AF'
           JR NZ,FNR6        ;EVAL RESULT OF NO-PARAM DEF FN IF FN HAS NO
                             ;BRACKETS EITHER. ELSE ERROR
PARAMERR:  RST &08
           DB 26             ;'Parameter error'

MDFERR:    RST &08
           DB 7              ;'FN without DEF FN'

FNBC:      EX AF,AF'
           JR NZ,PARAMERR    ;IF DEF FN HAS BRACKETS, FN MUST TOO

           CALL FORESP       ;SKIP '(', GET NEXT SIGNIF CHAR
           SUB ")"
           PUSH AF
           JR Z,FNRLE        ;JR IF NO PARAMS IN DEF FN - DEFADD=00XX

           POP AF
           PUSH HL           ;ADDR USED FOR DEFADD

FNRLA:     INC HL            ;INITIALLY, SKIP VAR LETTER
           LD A,(HL)
           CP &0E
           JR NZ,FNRLA       ;LOOP TILL 0EH MARKER OF PARAM BUFFER IN DEF FN FND

           IN A,(251)
           PUSH AF           ;PAGE OF DEF FN
           DEC HL
           LD A,(HL)         ;'$' OR NUMERIC VAR LETTER PRECEDES 0EH BUFFER
           SUB "$"-1
           INC HL
           INC HL            ;PT TO 5-BYTE BUFFER
           PUSH AF           ;A=1 IF '$'
           PUSH HL
           CALL SELCHADP     ;LOOK AT CHAD (FN '(' OR ',')
           CALL SEXPTEXPR    ;EVAL FN ARG. Z IF STRING
           POP DE            ;5 BYTE BUFFER IN DEF FN
           POP BC            ;B=1 IF STRING VALUE EXPECTED
           JR Z,FNR3         ;JR IF STRING VALUE

           DJNZ FNR4         ;JR IF TYPE OK

FNR3:      DJNZ PARAMERR     ;JR IF TYPE MISMATCH

FNR4:      CP ")"
           JR NZ,FNR5

           EX AF,AF'
           RST &20           ;SKIP FINAL ')' IF WE REACHED IT
           EX AF,AF'

FNR5:      EX AF,AF'         ;SAVE CHAR AFTER FN EXPR.
           POP AF
           OUT (251),A       ;DEF FN PAGE SELECTED
           CALL FDELETE      ;DEL. EXPR (FROM FN PARAM). HL PTS TO IT ON EXIT.
           LD BC,5
           LDIR              ;COPY TO DEF FN BUFFER
           EX DE,HL          ;HL POINTS PAST 5 BYTES IN DEF FN BRACKETS
           CALL FORESP1      ;LOOK FOR SIGNIFICANT CHAR IN DEF FN - ')' OR ','
           LD B,A
           EX AF,AF'         ;CHAR AFTER FN EXPR - ')' OR ','
           CP B
           JR NZ,PARAMERR

           CP ","
           JR Z,FNRLA        ;JR IF ANOTHER PARAM SHOULD FOLLOW

           CP ")"
           JR NZ,PARAMERR

FNRLE:     INC HL
           LD A,(HL)
           CP "="
           JR NZ,FNRLE       ;LOOP TILL DEF FN '=' FOUND

           LD BC,1           ;INC CHAD TO SKIP FINAL FN ')'

FNR6:      LD DE,(CHAD)
           LD A,(CHADP)
           LD B,A
           LD A,(DEFADDP)
           LD C,A
           LD (CHAD),HL      ;PT CHAD TO '=' IN DEF FN
           LD HL,(DEFADD)
           EX (SP),HL
           LD (DEFADD),HL    ;PT DEFADD TO DEF FN BRACKETS
           IN A,(251)
           LD (DEFADDP),A
           LD (CHADP),A
           PUSH DE           ;CHAD
           PUSH BC           ;SAVE CHAD (POINTER TO ')' AFTER FN, OR SOME OTHER
                             ;CHAR IF NO PARAMS. SAVE DEFADD TO ALLOW FN CALLING
                             ;FN.

           CALL TSURPG
           CALL SEXPTEXPR    ;SKIP '=', EVAL
           POP BC
           LD A,C
           LD (DEFADDP),A
           LD A,B
           PUSH AF
           CALL SETCHADP
           POP AF            ;$/N FLAG
           POP HL
           LD (CHAD),HL
           POP HL
           LD (DEFADD),HL

FNTYP:     RET NZ            ;RET IF NUMERIC - 'FN' IS NOMINALLY NUMERIC, SO
                             ;RET SETS FLAGS OK
           POP BC            ;ELSE JUNK RET,
           JP STRCONT        ;GOTO STRING FLAG SETTER

;CHECK FN SYNTAX

FNSYN:     RST &20           ;SKIP 'FN'
           LD (REFFLG),A     ;NZ SHOWS FN USED IN THIS LINE (FOR COMPILER)
           CALL FNNAME
           PUSH AF           ;A=1 IF STRING
           LD A,&FE
           CALL MKCLBF
           CP "("
           JR NZ,FNSY5       ;JR IF NO PARAMS E.G. FN TEST

           RST &20           ;SKIP '('
           CP ")"
           JR Z,FNSY4        ;JR IF NO PARAMS E.G. FN TEST ()

FNARL:     CALL SCANNING     ;ALLOW NUMERIC OR STRING EXPR
           LD A,C
           CP ")"
           JR Z,FNSY4

           CALL INSISCOMA
           JR FNARL

FNSY4:     RST &20           ;SKIP ')'

FNSY5:     POP AF
           DEC A             ;Z IF STRING
           JR FNTYP          ;IF NOT STRING, RET TO SET NUMERIC RESULT
                             ;ELSE JUNK RET ADDR FOR NUMERIC IMMED FNS
                             ;FN NAME ENDED '$'; SET FLAG FOR STRING


;CHECK FN NAME (CALLED BY FN AND DEF FN)

FNNAME:    CALL GETALPH      ;INSIST NAME STARTS WITH A LETTER

DFNLP:     INC HL
           LD A,(HL)
           CALL ALNUMUND
           JR C,DFNLP        ;NAME MAY CONTINUE WITH LETTERS, NUMBERS OR '_'

           SUB "$"
           JR NZ,FNN2

           INC HL            ;SKIP '$'

FNN2:      INC A             ;A=1 IF STRING-TYPE NAME
           RET


LOCAL:     CALL RUNFLG
           LD D," "          ;NULL PREVENTS 'LOCAL REF X' BEING ACCEPTED
           JR NC,DPSY2        ;JR IF SYNTAX TIME

           XOR A
           LD (PRPTRP),A
           LD HL,CARET
           LD (PRPTR),HL     ;PT HL SO LOW THAT IT STAYS ON CR IN ROM
           CALL ADDRCHAD
           LD (DPPTRP),A
           LD (DPPTR),HL
           CALL DPRA         ;GET PROC RET ADDR IN HL, PAGE/TYPE IN A, STAT IN C
           LD B,A
           PUSH BC
           PUSH HL
           CALL PROP2        ;PROPAR, WITHOUT PLACING TERMINATOR
           CALL PTTODP       ;**
           LD HL,(BSTKEND)
           DEC HL
           DEC HL
           DEC HL
           DEC HL
           POP DE            ;ADDR
           POP BC
           LD (HL),B         ;TYPE/PAGE
           LD A,C
           JP SEDA           ;STACK STAT, ADDR

DEFPROC:   CALL RUNFLG
           JR NC,DPROC2

           POP DE            ;NEXT STAT RET ADDR
           LD DE,THENTOK*256+&CB    ;ENDPROCTOK
                             ;(THENTOK=NULL)
           CALL SEARCH       ;CONTINUE AFTER END PROC, OR ERROR
           DB 13             ;'No END PROC'

DPROC2:    RST &18
           CALL GETALPH      ;INSIST ON A LETTER AS FIRST CHAR OF DEF PROC NAME

DPNMLP:    INC HL
           LD A,(HL)
           CALL ALNUMUND
           JR C,DPNMLP

           LD (CHAD),HL
           RST &18
           CP &B9            ;DATATOK
           JR NZ,DPSY1

           RST &20           ;SKIP 'DATA'
           RET

DPSY1:     CALL CRCOLON
           RET Z             ;RET IF PARAMS FINISHED

           LD D,&CE          ;REFTOK

;'LOCAL' SYNTAX CHECK ENTERS HERE WITH D=NULL (SPACE)

DPSY2:     CP D              ;'REF' IF DEF PROC, NULL IF LOCAL
           JR NZ,DPSY3       ;IF D IS NULL, *ALWAYS* JR

           RST &20           ;SKIP 'REF'

DPSY3:     PUSH DE
           CALL VARAR        ;CHECK FOR '()' FORMS
           CALL NZ,LOOKVARS  ;CALL IF NOT ONE
           CALL RCRC         ;RST 18, CRCOLON
           POP DE
           RET Z             ;RET IF PARAMS FINISHED

           CALL INSISCOMA
           JR DPSY2


;CHECK FOR VAR NAME SUCH AS frogs, price of bread, name$, abc2
;ENTRY: CHAD PTS TO NAME START.
;EXIT: CHECK FOR LEN OK FOR SPECIAL FORM E.G.  TEST$(), ALPHA()
;C=LEN (EXCLUDING SPACES). CY IF OK, NC IF TOO LONG, OR NOT A VAR NAME.

VARNAME:   RST &18
           CALL ALPHA
           RET NC            ;RET WITH NC IF NOT LEGAL FIRST CHAR

           PUSH HL
           LD BC,&0B00       ;NAME LEN MAX (STR/ARRAY) OF 10. INIT LEN=0

VNMLP:     RST &20
           INC C             ;LEN
           CALL ALNUMUND
           JR C,VNMLP

           CP "$"
           JR NZ,VNM2

           RST &20           ;SKIP '$'

VNM2:      LD A,C            ;NAME LEN
           CP B
           EX (SP),HL
           LD (CHAD),HL      ;ORIG CHAD
           POP HL
           LD A,(HL)         ;A=CHAR AFTER NAME
           RET               ;CY IF LEN OK


;CHECK FOR ODD FORM SUCH AS FRED$(), DOGSARRAY().
;USED BY 'LOCAL' AND 'DEF PROC'
;IF NOT SPECIAL FORM, CHAD IS UNCHANGED, NZ. ELSE CHAD/HL POINT PAST, A=CHAR

VARAR:     CALL VARNAME
           DEC A             ;NZ - A IS A SIGNIF CHAR.
           RET NC            ;RET WITH ORIG CHAD IF NOT LEGAL FOR '()' FORM

           CP "("-1
           RET NZ            ;RET IF NORMAL VAR.

           CALL FORESP
           CP ")"
           RET NZ            ;RET IF E.G. ALPHA(8) - EXPRESSION

           LD (CHAD),HL
           RST &20           ;SKIP ')'
           CP A              ;Z=SPECIAL FORM SKIPPED
           RET

;PROCEDURES
;ENTRY: CHAR BETWEEN 0-8FH FOUND WHEN CMD EXPECTED

PROCS:     ADD A,&90         ;CORRECT FOR PREVIOUS SUB 90H
           CALL GETALPH
           RST &18
           CALL RUNFLG
           JR NC,PROCSY      ;JR IF SYNTAX CHECK

           LD A,&0E

PRRL:      CP (HL)
           INC HL
           JR NZ,PRRL        ;LOOP TILL CALLING BUFFER FOUND

           INC HL            ;SKIP FD
           INC HL            ;SKIP FD
           LD B,(HL)         ;PAGE
           BIT 5,B
           JR NZ,MDPERR      ;(BIT 7 IS ALWAYS SET, BIT 6=EXTERNAL CMD, BIT 5
                             ;IS SET IF 'NO DEF PROC')
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)         ;ADDR OF DEF PROC LINE OR EXEC CODE
           INC HL
           LD (PRPTR),HL
           IN A,(251)
           LD (PRPTRP),A     ;SET UP PROC PTR
           LD A,B
           LD (DPPTRP),A     ;DEF PROC PTR PAGE
           CALL TSURPG       ;SELECT DEF PROC OR EXEC CODE PAGE
           LD HL,5
           ADD HL,DE         ;SKIP TO FIRST POSSIBLE DEF PROC NAME START POSN
           CALL FORESP1      ;SKIP ANY SPACES/CC

PRPNM:     INC HL
           LD A,(HL)
           CALL ALNUMUND
           JR C,PRPNM        ;LOOP PAST DEF PROC NAME

           LD (DPPTR),HL     ;PAST NAME
           PUSH DE           ;DEF PROC LINE ADDR
           CALL PROPAR       ;PROCESS PARAMETERS
           LD B,&40
           CALL BSTKE        ;STACK A 'PROC-TYPE' RET ADDR
           INC (HL)          ;RET TO *NEXT* STATEMENT
           POP HL            ;DEF PROC LINE ADDR
           POP DE            ;JUNK NEXT STAT
           LD A,(DPPTRP)
           LD C,2            ;STAT 2
           JP RLEPC2

;UNSTACK PROC RETURN ADDR, OR ERROR

DPRA:      LD B,&40          ;'PROC' TYPE
           CALL RETLOOP      ;GET C=STAT, HL=ADDR, NZ IF ERROR, A=PAGE
           RET Z

MDPERR:    RST &08
           DB 12             ;'Missing DEF PROC'

PROCSY:    INC HL
           LD A,(HL)
           CALL ALNUMUND
           JR C,PROCSY

           LD A,&FD
           CALL MKCLBF       ;MAKE BUFFER AFTER NAME
           CALL CRCOLON
           RET Z             ;RET IF NO PARAMS

PCSYL:     CALL VARAR        ;CHECK FOR '()' FORMS, SKIP IF SEEN
           CALL NZ,SCANNING  ;IF NOT '()' FORM ,EVAL
           CALL RCRC         ;RST 18, CRCOLON
           RET Z

           CALL INSISCOMA
           JR PCSYL

;MAKE CALLING BUFFER AT HL (0E A A A ? ?), PT CHAD AFTER IT TO SIGNIF CHAR, GET
;IT IN A. ENTRY: A=BYTE TO FILL FIRST 3 LOCNS WITH.

MKCLBF:    PUSH AF
           CALL MAKESIX      ;OPEN 6 BYTES AFTER NAME FOR ADDRESS BUFFER,
                             ;START WITH 0EH
           POP AF
           LD (HL),A
           INC HL
           LD (HL),A
           INC HL
           LD (HL),A
           INC HL
           INC HL            ;BUFFER= 0E FE FE FE ?? ??
           LD (CHAD),HL      ;PT CHAD TO PAST BUFFER
           RST &20
           RET

MAKESIX:   LD BC,6
           CALL MAKEROOM
           LD (HL),&0E
           INC HL
           RET

;LOOK FOR A FN VARIABLE
;CALLED FROM EVAL WHEN DEFADD IS SET DURING EVAL (OF DEF FN RESULT)
;ENTRY: HL PTS TO FIRST CHAR OF NAME
;RETS WITH NC IF NOT FOUND, HL SAME - CONTINUE SEARCH IN NORMAL VARS AREA.
;ELSE CY, VAR ALREADY STACKED, NAME SKIPPED, STR/NUM SET, A='$' IF STRING

LKFNVAR:   CALL RUNFLG
           RET NC            ;RET TO CALLER (EVAL) WITH NC IF SYNTAX TIME

           PUSH HL           ;IN CASE VAR NOT FOUND
           LD B,(HL)         ;VAR LETTER
           RST &20
           CP "$"
           LD C,&0E
           JR NZ,LKFV0

           LD C,A            ;C='$'
           RST &20

LKFV0:     CALL ALNUMUND
           JR C,LKFVF        ;NO SEARCH FOR E.G. 'TEST', 'X1', 'ABC$' - JUST
                             ;SINGLE LETTER VARS
           LD HL,DEFADD-1
           CALL ASV2         ;PT TO PAST '(' IN DEF FN

LKFV1:     INC HL
           LD A,(HL)
           CP &0E
           JR NZ,LKFV1       ;LOOK FOR A BUFFER MARKER

           DEC HL
           LD A,(HL)
           CP C              ;C='$' IF STRING WANTED, OR 0EH FOR NULL
           JR NZ,LKFV2       ;JR IF A NUMERIC BUFFER FOUND

           DEC HL
           LD A,(HL)         ;STRING LETTER
           INC HL

LKFV2:     XOR B             ;DESIRED LETTER
           AND &DF
           JR Z,LKFVM        ;JR IF MATCHED

           LD DE,7
           ADD HL,DE         ;PT PAST 5-BYTE BUFFER
           CALL FORESP1
           CP ","
           JR Z,LKFV1        ;KEEP CHECKING IF MORE BUFFERS FOLLOW

LKFVF:     POP HL            ;ORIG HL
           AND A             ;NC - SEARCH FAILED
           RET

LKFVM:     INC HL            ;PT TO 0EH
           INC HL            ;PT TO BUFFER
           LD A,C
           CALL HLTOFPCS     ;STACK BUFFER DATA
           POP BC            ;ORIG HL
           LD HL,FLAGS
           RES 6,(HL)
           CP "$"
           SCF
           RET Z             ;CY SHOWS FOUND

           SET 6,(HL)
           RET

;INC PTR HL AND RET WHEN IT POINTS TO SIGNIF CHAR

FORESP:    INC HL

;AVOIDS INITIAL INC

FORESP1:   LD A,(HL)
           CP &21
           RET NC

           JR FORESP

;CALLED BY LOAD, DELETE, KEYIN, RENUM

SCOMP:     LD A,&FF
           LD (COMPFLG),A
           RET

;FROM DELETE/MERGE

GT4R:      LD A,(SUBPPC)
           LD HL,(PPC)
           DB &11            ;'JR+2'

;FROM RENUM/KEYIN

GT4P:      POP AF            ;STAT
           POP HL            ;LINE

;ENTRY: A=STAT-1 TO GO TO, HL=LINE

           INC A
           CALL GOTO4
           LD A,&FF
           LD (PPC+1),A      ;ENSURE "GOTO" SEARCHES PROG FROM START

DOCOMP:    CALL SCOMP

;CALLED AS ELINE IS EXECUTED, AND BY CLEAR/RUN. CHAD IS AUTO-ADJUSTED BECAUSE
;VAR CREATION MIGHT MOVE IT

COMPILE:   LD A,(CHADP)
           LD (KCURP),A
           LD HL,(CHAD)
           LD (KCUR),HL      ;NEEDS AUTO-ADJUST
           LD HL,(CLA)
           PUSH HL
           LD A,(CLAPG)
           PUSH AF
           LD A,(COMPFLG)
           AND A
           JR Z,COMPILEL     ;JR IF ONLY ELINE NEEDS COMPILE

;DO LABELS
           CALL ADDRPROG     ;START AT (PROG)

DOLBLP:    LD BC,&2100+&EA   ;LABELTOK
           CALL LKFC         ;FIND A LABEL
           JR C,LABSD        ;END IF ALL LABELS PROCESSED
                             ;ELSE HL POINTS TO FIRST CHAR IN LINE, CHAD
           IN A,(251)        ;PTS TO 'LABEL', DE=LINE LEN
           LD (CHADP),A      ;KEEP CHAD UP TO DATE FOR ASSIGN ETC.?!!?
           PUSH HL
           ADD HL,DE         ;PTR TO NEXT LINE
           EX (SP),HL
           DEC HL
           DEC HL
           DEC HL
           LD C,(HL)
           DEC HL
           LD B,(HL)         ;BC=LINE NUMBER OF LINE WITH LABEL
           CALL STACKBC
           CALL SVNUMV       ;ASSESS NUMERIC VARIABLE
           CALL ASSIGN       ;ASSIGN FPC VALUE TO VAR
           POP HL
           JR DOLBLP         ;CONTINUE SEARCH AT NEXT LINE

LABSD:                       ;'COMPILING PROGRAM'SHOWN BY BIT 7 COMPFLG HI
           CALL COMALL       ;COMPILE PROCS/FN (CY HERE)

           XOR A
           LD (COMPFLG),A    ;'COMPILING ELINE'SHOWN BY BIT 7 LOW

COMPILEL:  CALL ELCOMAL      ;*ALWAYS* COMPILE ELINE FOR PROCS, AND FNS
                             ;IF NEEDED.
           POP AF
           LD (CLAPG),A
           POP HL
           LD (CLA),HL
           LD HL,(KCUR)
           LD (CHAD),HL
           LD A,(KCURP)
           JP SETCHADP

;LKFC.SAM - LOOK FIRST CHAR
;LOOK FOR CHAR IN C, AT START OF LINES, SKIPPING ANY PRECEDING CC/SPACES
;ENTRY: B=21H FOR CC/SPACE SKIP, C=CHAR, HL=START
;EXIT: NC IF FOUND, HL=FIRST CHAR IN LINE (MAY NOT BE C) (CHAD)=TARGET
;      CY IF SEARCHED PROG AND FAILED.

LKFCLP:    ADD HL,DE

;ENTRY POINT

LKFC:      LD A,(HL)
           ADD A,1
           RET C             ;RET IF END OF PROGRAM

           CALL CHKHL        ;KEEP LINE START IN SECTION C
           INC HL
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)         ;DE=LINE LNE
           INC HL
           LD A,(HL)         ;FIRST CHAR - QUICK CHECK FOR MOST COMMON CASE
                             ;(NO PRECEDING JUNK)
LKFC2:     CP C
           JR Z,LKFC5        ;JR IF GOT IT - BUT MIGHT NEED TO SKIP CC/SPACE

           CP B              ;CP 21H
           JR NC,LKFCLP      ;LOOP IF CHAR WAS SIGNIF - THIS LINE WON'T DO

           PUSH HL           ;SAVE LINE START

LKFCSK:    CP &0D
           JR Z,LKFC4        ;STOP TRYING IF LINE ENDS

           INC HL            ;SKIP CC/SPACE
           LD A,(HL)
           CP B              ;CP 21H
           JR C,LKFCSK       ;SKIP ALL CC/SPACES

           LD (CHAD),HL      ;ALTER CHAD IN CASE WE HAVE A MATCH
           POP HL            ;FIRST CHAR IN LINE PTR
           CP C              ;CP FIRST SIGNIF CHAR IN LINE WITH DESIRED.
           RET Z

           PUSH HL

LKFC4:     POP HL            ;FIRST CHAR IN LINE PTR
           JR LKFCLP

LKFC5:     LD (CHAD),HL
           RET
