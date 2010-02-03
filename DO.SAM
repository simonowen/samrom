;DO.SAM

DO:        CALL WHUNT        ;WHILE/UNTIL SR - ONLY RET HERE IF RUNNING
           JR C,DO3          ;JR IF WANT TO EXECUTE LOOP LINES
                             ;ELSE SKIP TO "LOOP"

;ENTRY FOR EXIT IF

DO2:       POP DE            ;NEXT STAT RET ADDR
           LD DE,&D4D5       ;DOTOK/LOOPTOK
           CALL SEARCH
           DB 9              ;"Missing LOOP"


;STACK A RETURN ADDR AND EXECUTE LINES IN THE DO-LOOP

DO3:       LD B,&80          ;MASK TO SET BIT 7 OF PAGE - SHOW "DO" DATA

;MAKE BASIC STACK ENTRY (TYPE/PAGE, ADDR (OF LINE START), STAT)
;ENTRY WITH B=TYPE BYTE, FROM GOSUB OR PROC, OR DO (SEE ABOVE)
;BITS 7-5: 100=DO, 010=PROC, 000=GOSUB
;EXIT: HL PTS TO STACKED SUBPPC (GOSUB/PROC INCS IT)

BSTKE:     LD HL,(BSTKEND)
           LD DE,-4          ;SPACE NEEDED
           ADD HL,DE         ;FIND NEW BSTKEND. CY
           LD DE,(HEAPEND)   ;LOWER IN MEM THAN STACK - END OF USER CODE
           SBC HL,DE         ;END-(LIM+1) - BSTK CANNOT COME AS FAR DOWN AS HEAP
           JR NC,BSTKOK

BSFERR:    RST &08
           DB 41             ;"BASIC stack full"

BSTKOK:    ADD HL,DE
           INC HL
           LD A,(CLAPG)
           AND &1F
           OR B              ;MARK FOR TYPE
           LD (HL),A         ;TYPE/PAGE
           LD A,(SUBPPC)
           LD DE,(CLA)

SEDA:      LD (BSTKEND),HL   ;BSTKEND IS NOW 4 BYTES LOWER
           INC HL
           LD (HL),E
           INC HL
           LD (HL),D
           INC HL
           LD (HL),A
           RET

LOOPIF:    CALL SYNTAX6

           CALL TRUETST      ;DISCARD AND TEST TRUE/FALSE.
           RET Z             ;NEXT STAT IF FALSE

           SCF
           JR LOOP1          ;UNSTACK "DO" RET ADDR, LOOP

EXITIF:    CALL SYNTAX6

           CALL TRUETST      ;DISCARD AND TEST TRUE/FALSE. NC
           RET Z             ;NEXT STAT IF FALSE

           CALL LOOP1        ;UNSTACK "DO" RET ADDR, DON"T LOOP
           JR DO2            ;SKIP TO LOOP


LOOP:      CALL WHUNT        ;ASSESS WHILE/UNTIL

LOOP1:     EX AF,AF'         ;SAVE LOOP/NO LOOP AS C/NC (ALWAYS NC IF "EXIT IF",
                             ;ALWAYS CY IF LOOP IF)
           LD B,&80          ;"DO" TYPE
           CALL RETLOOP      ;GET C=STAT, HL=ADDR, NZ IF ERROR, A=PAGE
           JR Z,LOOP2

           RST &08
           DB 10             ;"LOOP without DO"

LOOP2:     EX AF,AF'
           RET NC            ;RET TO "NEXT STAT" IF "NO LOOP"
                             ;RET IS TO "EXIT IF" IF CALLED FROM THERE.

           EX AF,AF'         ;PAGE


;USED BY RETURN, END PROC, LOOP, NEXT. ESSENTIALLY A "GOTO" STAT IN LINE AT AHL.
;ENTRY: A=PAGE, HL=ADDR OR 00XX IF ELINE, C=STAT.

RLEPCOM:   POP DE            ;JUNK NEXT STAT RET ADDR
           INC H
           DEC H
           JP Z,LOOPEL       ;JP IF LOOPING BACK TO ELINE (ADDR MSB=0)
                             ;(NSPPC WILL BE SET BY FROM C,
                             ;PAGE WILL BE SET TO ELINEP, CHAD BY SKIP STATS)
           AND &1F

;FROM PROCS:

RLEPC2:    LD B,C
           PUSH BC           ;B=STAT
           CALL SELURPG
           JP RLEPI          ;USE HL AS LINE START, A AS CHAD/CLA/NXTLN PAGE,
                             ;(SP) AS STAT

;*******************************************************************************

;ON VALUE: STAT1: STAT2: STAT3

ON:        CALL SYNTAX6

           CALL GETBYTE
           LD D,A
           LD HL,SUBPPC
           ADD A,(HL)
           LD (HL),A         ;ADJ SUBPPC BY VALUE
           RST &18           ;HL=CHAD
           CALL SKIPS0       ;SKIP D STATS
           RET C             ;RET IF HIT END OF LINE - NEXT STAT ->NXT LINE

           RST &18           ;PT TO ":"
           PUSH HL
           RST &20           ;A=FIRST SIGNIF CHAR IN STAT
           POP HL            ;":" PTR
           CALL ALPHA
           JR C,ON2          ;JR IF LETTER - IT"S A PROC

           CP &B5            ;GOSUBTOK
           JR NZ,ON3         ;JR UNLESS "GOSUB"

;PROCS AND GOSUBS MUST RETURN TO NEXT LINE AFTER EXECUTING

ON2:       LD (CHAD),HL      ;PT TO ":"
           LD HL,(NXTLINE)
           LD (CLA),HL       ;MAKE IT LOOK AS THOUGH WE ARE AT NEXT LINE
           LD HL,SUBPPC
           LD A,(HL)
           LD (ONSTORE),A    ;SAVE SUBPPC, WHICH WILL BE ZERO AFTER INCR
                             ;ANY ERRORS WILL GIVE SENSIBLE STAT NO. BECAUSE
                             ;ERROR HANDLER USES (ONSTORE) IF STAT NO=00. THIS
           LD (HL),255       ;WILL BE INCED TO ZERO. GOSUB/PROCS WILL THINK
                             ;WE ARE AT A STAT ZERO, NEXT LINE, AND WILL RETURN
                             ;TO STAT 1, NEXT LINE
           RET               ;TO NEXT STAT

ON3:       POP DE            ;NEXT STAT
           CP &B4            ;GOTOTOK
           JR Z,ON4          ;GOTO KEEPS DE AS NEXT STAT ADDR

           LD DE,OLNEND      ;ELSE USE LINEEND SO ONLY 1 STAT EXECUTED

ON4:       CP ":"            ;SEE IF NULL STATEMENT
           JP NZ,ON4ENT      ;PUSHES DE (NEXT STAT OR LINE END),EXECUTES STAT.
                             ;RETURN GOES TO LINE END UNLESS GOTO WAS USED

           EX DE,HL
           JP (HL)           ;NULL STAT (:) JPS TO LINE END

GOTO2:     CALL GETINT
           LD A,H
           INC A

GTERRHP:   JP Z,IOORERR     ;RANGE 0-65279 (0000-FEFFH)

GOTO3:     XOR A             ;STAT NO. ZERO

GOTO4:     LD (NSPPC),A
           LD (NEWPPC),HL
           RET

CONTINUE:  CALL CHKEND

CONTINUE2: LD A,(OSPPC)
           LD HL,(OLDPPC)
           JR GOTO4

;CALBAS - CALL BASIC SUBROUTINE FROM MACHINE CODE
;ENTRY: HL=LINE TO CALL.
;EXIT: Z IF OK, ELSE A=ERROR NUMBER

CALBAS:    CALL GOTO3        ;LD (NEWPPC),HL: ZERO (NSPPC)
           LD B,A            ;TYPE/PAGE=GOSUB/PAGE 0
           DEC A
           LD (SUBPPC),A     ;"STATMENT" FF SHOWS M/C
           IN A,(251)
           PUSH AF
           CALL BSTKE        ;STACK RETURN ADDR, STAT FFH. INSTEAD OF DOING A
                             ;BASIC RETURN, "RETURN" CAUSES RET TO ERR HANDLER.
           CALL SETESP       ;SET ERRSP SO ERRORS RETURN TO THIS ROUTINE
           CALL NEXTSTAT     ;RUN LINE. NORMAL BASIC STACK AT "NEXTSTAT" HOLDS
                             ;MAINER ADDR, WITH ERRSP POINTING TO IT. NOW ERRSP
                             ;PTS TO THIS ROUTINE
           POP HL
           LD (ERRSP),HL     ;RESTORE ORIG.
           POP AF
           OUT (251),A       ;ORIG URPAGE
           LD A,(ERRNR)
           AND A
           RET


RETURN:    CALL CHKEND

           LD B,0            ;"GOSUB" TYPE
           CALL RETLOOP      ;GET RET ADDR
           JR NZ,RWGERR      ;HL=LINE ADDR, A=TYPE/PAGE, C=STAT, Z IF TYPE OK

           INC C             ;RETURN TO *NEXT* STAT
           JR NZ,ENDP1       ;STAT IS ONLY 0FFH IF CALL CAME FROM M/C

           POP BC
           RET               ;TO ERROR HANDLER (M/C)

RWGERR:    RST &08
           DB 8              ;"RETURN without GOSUB"

ENDPROC:   CALL CHKEND

           CALL DPRA         ;GET RET ADDR. C=STAT, HL=ADDR, A=PAGE
           PUSH HL
           PUSH BC
           PUSH AF
           CALL DELOCAL
           POP AF
           POP BC
           POP HL

;FROM RETURN, IF TYPE OK

ENDP1:     LD B,A
           LD A,(ONERRFLG)   ;T BIT,000000,P BIT
           RRA
           JR NC,ENDP2       ;JR IF "ON ERROR" PERM OFF

           LD A,&81          ;END PROC/RETURN RESET TEMP ERROR BIT TO PERM
                             ;STATUS SO ERROR PROC OR SR EASY.
           LD (ONERRFLG),A   ;TEMP AND PERM NOW ON

ENDP2:     LD A,B
           JP RLEPCOM        ;USE HL AS LINE ADDR, A AS PAGE, C AS STAT


;WHILE/UNTIL SR OF DO AND LOOP COMMANDS

WHUNT:     CP WHILETOK
           JR Z,WHUNT2

           CP UNTILTOK
           SCF
           JR Z,WHUNT2

           POP HL            ;RET ADDR IN DO OR LOOP ROUTINE
           CALL RUNFLG
           RET NC            ;NEXT STAT IF SYNTAX TIME - CHECK "DO" OR "LOOP"

           JP (HL)           ;RET WITH C IF NO QUALIFIERS (NO WHILE OR UNTIL)

WHUNT2:    PUSH AF           ;WHILE/UNTIL FLAG
           CALL SEXPT1NUM    ;SKIP WHILE/UNTIL, GET EXPR
           CALL RUNFLG
           JR NC,WHUNT3      ;GOTO NEXT STAT NOW IF SYNTAX TIME

           CALL TRUETST      ;DROP AND TEST EXPR.
           JR Z,WHUNT4       ;JR IF FALSE

           POP AF            ;IF TRUE AND UNTIL, NC
           CCF               ;IF TRUE AND WHILE, C
           RET

WHUNT3:    POP AF            ;ENTRY TO JUNK FLAG, RET ADDR, RET TO NEXT STAT

WHUNT4:    POP AF            ;IF FALSE AND UNTIL, C
           RET               ;IF FALSE AND WHILE, NC

;IF CALLED FROM LOOP, C MEANS EXECUTE THE LOOP, ELSE CONTINUE
;IF CALLED FROM  DO , C MEANS EXECUTE DO, ELSE FIND LOOP AND JP THERE

;RETURN/LOOP SR. ENTRY: B=DESIRED TYPE OF DATA TO UNSTACK FROM BASIC STACK.
;80H=DO, 40H=PROC, 00=GOSUB
;EXIT: A=STAT NR, HL=ADDR OF LINE, C=TYPE/PAGE
;IF ENTRY WAS AT RETLOOP, NZ=WRONG TYPE/EMPTY STACK, Z=OK

RETLOOP:   LD HL,(BSTKEND)
           LD A,(HL)         ;TYPE/PAGE
           AND &E0           ;ISOLATE TYPE BITS
           CP B
           RET NZ            ;RET IF WRONG TYPE OR STACK MT (FF STOPPER)

RETLOOP2:  LD A,(HL)         ;TYPE/PAGE
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)         ;ADDR
           INC HL
           LD C,(HL)         ;STAT
           INC HL
           LD (BSTKEND),HL
           EX DE,HL
           RET


;FIND LINE NO. "HL" (OR "BC", WITH LATER ENTRY)
;STARTS SEARCH AT PROG, OR IF RUNNING AND TARGET IS AT OR PAST EPPC, SEARCHES
;FROM CURRENT LINE (PPC) ADDRESS. USES HL,DE,BC,AF, *TEMPW1*
;ENTRY:HL=LINE NO.
;EXIT: HL PTS. TO LINE NO. MSB IN PROGRAM, DE PTS TO PREVIOUS LINE. DE IS IN
;8000-BFFF AREA, HL MIGHT HAVE CROSSED IN TO C000 BY A LINE LEN OR SO.
;IF NO PROGRAM, DE=HL
; Z=LINE FOUND
;NZ=FOUND A LATER LINE, OR FF STOPPER

FNDLNHL:   LD B,H
           LD C,L

FNDLNBC:   CALL RUNFLG

           JR NC,FNDLP       ;JR IF NOT RUNNING (E.G. EDITING)

           LD HL,(PPC)
           DEC HL            ;SO CY IF PPC=TARGET
           AND A
           SBC HL,BC
           JR NC,FNDLP       ;JR IF DESIRED LINE IS BEFORE CURRENT LINE
                             ;(ALWAYS, IF PPC=FFFF (ELINE))

           LD HL,(CLA)       ;START LOOKING FROM PPC LINE START
           LD A,(CLAPG)      ;NEEDED? OR USE JR+2
           JR FNDL0

;ENTRY HERE IF WANT TO INSIST ON STARTING AT PROG (RENUM)

FNDLINE:   LD B,H
           LD C,L

FNDLP:     LD HL,(PROG)
           LD A,(PROGP)

FNDL0:     CALL TSURPG       ;SWITCH IN A PROGRAM BLOCK
           LD (TEMPW1),HL    ;KEEP PTR TO LINO MSB
           JR FNDL2

FNDL1:     BIT 6,H
           CALL NZ,INCURPAGE
           LD (TEMPW1),HL    ;KEEP PTR TO LINO MSB
           INC HL
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC HL
           ADD HL,DE         ;ADD LINE LEN TO PT TO NEXT LINE NO.

FNDL2:     LD A,(HL)         ;GET MSB OF LINE NO.
           CP B              ;CP MSB OF TARGET
           JP C,FNDL1        ;JP IF NOT AT OR PAST TARGET LINE YET

           JR NZ,FNDL3       ;JR IF MSB SHOWS WE ARE PAST TARGET LINE NO.

           INC HL
           LD A,(HL)         ;LSB
           DEC HL
           CP C
           JP C,FNDL1        ;JP IF LSB SHOWS WE ARE NOT AT TARGET YET

FNDL3:     LD DE,(TEMPW1)
           RET

;IF.SAM - 2.3.89
;*******************************************************************************
;E.G. IF x=1 THEN PRINT
;     IF X=1 THEN PRINT "Y": ELSE PRINT "N"
;     IF X=1: PRINT: PRINT: END IF

;BOTH LONG AND SHORT IFS COME HERE

LIF:
SIF:       LD HL,(CHAD)

SIFLP:     DEC HL
           LD A,(HL)
           CP &21
           JR C,SIFLP           ;PT HL TO CMD CODE

           PUSH HL
           CALL EXPT1NUM
           POP HL
           LD (IFTYPE),A        ;RECORD LAST IF TYPE AS SHORT/LONG USING
                                ; "THEN"/NON "THEN"
                                ;(IF TYPE IS SET TO "LONG" AT START OF LINE
                                ;SYNTAX CHECK)
           CP THENTOK
           LD D,A
           JR NZ,IFL1           ;JR IF NOT "THEN" (SHOULD BE ":")

           LD (HL),&D8          ;SIFTOK
                                ;AT LINE ENTRY, I-F IS TOKENISED AS THE "LIF"
                                ;TOKEN BECAUSE IT OCCURS FIRST IN TOKEN LIST
                                ;(LIF TOKEN LISTS AS I-F, LIKE THE IF TOKEN)
                                ;NOW FORCE TOKEN TO "SIF" BECAUSE "THEN" USED.

IFL1:      CALL CHKEND          ;IF SYNTAX CHECK, CHECK FOR OD/:/THEN, EXIT

           CALL TRUETST         ;DROPS EXPR AND TESTS IT
           RET NZ               ;RET IF TRUE, DO NEXT STATMENT (FPCS IS CLEARED)

           POP BC               ;NEXT STAT
           LD A,D
           CP THENTOK
           JR Z,SHORTIF         ;IF SHORT AND NOT TRUE, LOOK FOR ELSE OR CR

EIFLP:     EXX
           LD BC,&FF00+&D9      ;LELSETOK
           EXX                  ;WHOLE PROG (OR ELINE), TARGET 2 RELOAD=LELSE

           LD BC,&0100+&D9      ;LELSETOK. COUNT=1, TARGET2=LELSE
           LD DE,&D7DB          ;LIFTOK/ENDIFTOK
                                ;LOOK FOR LELSE OR ENDIF WITH LIF INTERVENING
           CALL SRCHALL3
           JR NC,MEIERR         ;ERROR IF NEITHER WAS FOUND

           LD (SUBPPC),A        ;STAT
           EX AF,AF'            ;FINAL TARGET
           CP E                 ;WAS IT "END IF"?
           JP Z,XCHDH           ;IF IT WAS, CONTINUE AFTER "END IF"

           RST &18
           CP &D8               ;SIFTOK
           JP NZ,XCHDH          ;JP IF "LELSE", NOT "LELSE SIF"; CONTINUE AFTER
                                ;"LELSE"

           CALL SEXPT1NUM       ;SKIP "SIF", EVAL CONDITION
           CALL TRUETST
           JR Z,EIFLP           ;IF FALSE, KEEP LOOKING FOR ENDIF/LELSE

XCHDH:     JP EXCHAD2           ;IF TRUE, CONTINUE AFTER "ELSE IF cond"

MEIERR:    RST &08
           DB 39                ;"Missing END IF"

SHORTIF:   EXX
           LD BC,THENTOK        ;ONE LINE SRCH, NULL TARGET2 RELOAD
           EXX

           LD DE,&D8DA          ;SIFTOK/ELSETOK
                                ;LOOK FOR "ELSE" WITH SIF "INTERVENING"
           CALL SRCHALL2
           JP NC,LINEEND        ;JP TO LINE END IF NO "ELSE" FOUND

           LD (SUBPPC),A
           JP STMTLP2           ;JP TO NEXT STATEMENT, JUST AFTER "ELSE"
                                ;(CHAD, CLA, NXTLINE STILL OK - SAME LINE)

;E-L-S-E IS TOKENISED AS LELSE TOKEN BECAUSE IT APPEARS FIRST IN LIST

LELSE:     LD C,A               ;CHAR AFTER "ELSE"
           CALL RUNFLG
           JR C,RLELSE          ;JR IF RUNNING A LONG ELSE

           LD HL,(CHAD)

FELSLP:    DEC HL
           LD A,(HL)
           CP &21
           JR C,FELSLP          ;PT TO CMD CODE

           LD A,(IFTYPE)
           CP THENTOK
           JR Z,NLELS           ;JR IF "SHORT" STATUS

           LD A,C
           SUB &D8              ;SIFTOK
           ADC A,0              ;SIF/LIF BOTH BECOME 0
           JR NZ,ELSE2          ;CHECK SYNTAX AFTER "LELSE"

           LD HL,(CHAD)
           LD (HL),&D8          ;"LELSE LIF" BECOMES "LELSE SIF"
           CALL SEXPT1NUM       ;EVAL condition
           CP THENTOK
           RET NZ               ;TO CHECK SYNTAX OF NEXT STATEMENT

DNS:       RST &08              ;"LELSE SIF cond THEN" NOT ALLOWED AS IT
           DB 29                ;WILL WORK IN A CONFUSING WAY

NLELS:     LD (HL),&DA          ;ELSETOK
                                ;FORCE "SHORT ELSE" IF "SHORT IF" PRECEDED IT.
                                ;(THERE WAS A PRECEDING "SHORT IF" ON THIS LINE)
           JR ELSE2             ;CHECK SYNTAX FROM CHAD ON

;LONG ELSE REQUIRES A SEARCH FOR ENDIF, WITH LIF INTERVENING

RLELSE:    POP DE               ;JUNK NEXT STAT ** BUG FIX
           LD DE,&D7DB          ;LIFTOK/ENDIFTOK
           CALL SEARCH          ;LOOK FOR "ENDIF" WITH LIF "INTERVENING"
           DB 0                 ;"OK" IF NOT FOUND

;SHORT ELSE - SHOULD ALWAYS BE RUNNING!

ELSE:      CALL RUNFLG
           JP C,REMARK          ;JP, SKIP REST OF LINE IF RUNNING A SHORT ELSE

ELSE2:     POP BC               ;NEXT STAT
           JP STMTLP2           ;CHECK SYNTAX FROM CHAD ONWARDS WITHOUT
                                ;REQUIRING CR/COLON

;DROP EXPRESSION FROM FPCS, TEST FOR TRUE/FALSE

TRUETST:   LD HL,(STKEND)    ;PT TO END OF RESULT OF THE EXPRESSION
           DEC HL
           DEC HL
           LD A,(HL)         ;MSB IF INTEGER
           DEC HL
           OR (HL)           ;LSB
           DEC HL
           OR (HL)           ;SGN - PROB NOT NEEDED IF MINUS ZERO EXCLUDED!
           DEC HL
           OR (HL)           ;EXP.
           LD (STKEND),HL    ;"DISCARD"
                             ;NZ IF TRUE, DO NEXT STATMENT (FPCS IS CLEARED)

ENDIF:     RET                  ;DOES NOTHING - ACTS AS A MARKER ONLY


;FOR.SAM 26.5.89
;*******************************************************************************

FOR:          CALL SYNTAX4     ;ASSESS FOR-VARIABLE

              RST &18
              CP "="
              JR NZ,DNS        ;NONSENSE

              CALL SEXPT1NUM   ;START VALUE
              CP TOTOK
              JP NZ,DNS        ;NONSENSE

              CALL SEXPT1NUM   ;LIMIT
              CP STEPTOK       ;"STEP"
              JR Z,FORSTEP

              CALL CHKEND

              DB CALC
              DB STKONE        ;DEFAULT STEP OF 1
              DB EXIT

              INC D            ;NZ

FORSTEP:      CALL Z,SSYNTAX6  ;GET THE STEP VALUE

                               ;VALUE/LIMIT/STEP ON FPCS
FOR2:         RST &18
              PUSH AF          ;CR OR COLON
              CALL SWOP12      ;V/S/L
              CALL FPSWOP13    ;L/S/V

              LD HL,TLBYTE+33
              SET 6,(HL)       ;"FOR-NEXT" TYPE MARKED ON T/L BYTE
              CALL ASSISR      ;IF NORMAL VAR EXISTS, DEST PTS TO PREV PTR, AND
                               ;FLAGX BIT 0 SHOWS "NEW", SO VAR IS "LINKED OUT"
                               ;ASSIGNS V TO FIRST 5 LOCNS. DE PTS AFTER
                               ;THESE ON EXIT (14 EXTRA LOCATIONS AVAILABLE IF
                               ;"OLD" FOR-NEXT, ELSE NUMEND IS PAST VAR, NEEDS
                               ;MOVING 14 ON)
              LD HL,(STKEND)
              LD BC,10
              AND A
              SBC HL,BC
              LD (STKEND),HL   ;DELETE L,S
              LDIR             ;COPY TO VARIABLE BUFFER TO GIVE V,L,S
              POP AF           ;STAT END CHAR
              PUSH DE
              DEC DE
              EX DE,HL         ;SRC=END OF S
              LD DE,MEMVAL+14
              LD C,15
              LDDR             ;COPY V,L,S TO MEM 0,1,2 FOR NEXTTEST TO USE
              CP &0D
              JR Z,FOR22       ;JR IF LOOPING ADDRESS IS ON NEXT LINE

              LD A,(SUBPPC)
              INC A
              LD C,A
              LD A,(PPC+1)
              INC A
              LD H,A
              JR Z,FOR25       ;JR IF ELINE

              LD HL,(CLA)      ;ELSE THIS LINE, NEXT STAT
              JR FOR25

FOR22:        LD HL,(NXTLINE)
              LD C,1           ;FIRST STATEMENT

FOR25:        EX DE,HL
              POP HL           ;VARS PTR
              LD A,(NXTLINEP)  ;PAGE OF CURRENT LINE (SAME AS CLAPG)
              LD (HL),A        ;ORDER CHANGE VS. ROM 1.0**
              INC HL
              LD (HL),E
              INC HL
              LD (HL),D        ;LINE ADDRESS
              INC HL
              LD (HL),C        ;STAT
              INC HL
              LD A,(FLAGX)
              RRA
              EX DE,HL
              CALL C,NELOAD    ;SET NUMEND IF "NEW" VARIABLE

              CALL NEXTTEST
              RET NZ           ;RET IF A LOOP IS POSSIBLE

              CALL SELCHADP

FORMLP:       LD E,&C1         ;NEXTTOK
              CALL SRCHPROG    ;LOOK FOR "NEXT" FROM CHAD ONWARDS
              JR C,FOR3        ;JR IF ONE FOUND

              RST &08
              DB 6             ;"FOR without NEXT"

FOR3:         LD (SUBPPC),A    ;STAT
              LD DE,TLBYTE+33
              CALL MATCHFN     ;CHECK (HL) VS (TLBYTE+33) OVER T/L+1 BYTES
              JR C,FORMLP      ;LOOP IF THE WRONG NEXT VARIABLE

              LD (CHAD),DE     ;SKIP var - DE PTS TO PAST VAR NAME
              POP DE           ;JUNK NEXT STAT
              JP EXCHAD2       ;CONTINUE EXECUTION AFTER "NEXT var"

NWFERR:       RST &08
              DB 5             ;"NEXT without FOR"

;*******************************************************************************

NEXT:         CALL SYNTAX4     ;ASSESS "FOR" VARIABLE
              CALL CHKEND

              CALL BRKSTOP   ;TEST FOR BREAK (RLEPCOM BELOW AVOIDS NORMAL
                             ;BETWEEN-STATEMENT TEST)

              LD A,(STRLEN)    ;TYPE BYTE (FROM NVARS, IF VAR FOUND)
              AND &40          ;BIT 6 SET=FOR-NEXT TYPE, FOUND
              JR Z,NWFERR
                               ;5 BYTE VALUE, 5 BYTE LIMIT, 5 BYTE STEP
                               ;2 BYTE ADDRESS, 1 BYTE PAGE, 1 BYTE STAT. NO.
              CALL ADDRDEST    ;PT TO VALUE
              CALL NEXTSR
              JR Z,NEXT1       ;JR IF INTEGER MATHS ALREADY DONE

              LD HL,(DEST)
              PUSH HL
              LD DE,(MEM)
              LD BC,15
              LDIR             ;COPY VAR TO CALC MEMS 0,1,2

              DB CALC
              DB RCL0          ;V
              DB RCL2          ;V,S
              DB ADDN          ;V+S
              DB STOD0
              DB EXIT

              EX DE,HL         ;HL PTS TO DROPPED NEW V
              POP DE
              LD BC,5
              LDIR             ;COPY NEW V BACK TO VARS

              CALL NEXTTEST
              RET Z            ;RET IF "NO LOOP"

              DB &21           ;="JR +2"

NEXT1:        AND A
              RET Z            ;RET IF INTEGER MATHS SHOWS "NO LOOP"

              LD DE,15
              LD HL,(DEST)
              ADD HL,DE
              LD A,(HL)        ;A=PAGE OF LOOPING LINE **
              INC HL
              LD E,(HL)
              INC HL
              LD D,(HL)        ;DE=ADDR OF LOOPING LINE (8000-BFFF) OR
                               ;00?? IF ELINE IS THE LOOPING LINE
              INC HL
              LD C,(HL)        ;LOOPING STATMENT NO.
              EX DE,HL         ;HL=ADDR, A=PAGE, C=STAT
              JP RLEPCOM       ;GOTO STAT C IN LINE AT AHL


;CHECK TO SEE IF "LIMIT" HAS BEEN EXCEEDED BY "VALUE"

NEXTTEST:     DB CALC
              DB RCL0
              DB RCL1
              DB RCL2          ;V,L,S
              DB GRTR0         ;V,L,TRUE/FALSE
              DB JPTRUE
              DB &02           ;TO NEXTTST1

              DB SWOP          ;SWOP IF STEP IS NEGATIVE

NEXTTST1:     DB SUBN          ;V-L IF +VE STEP
              DB SGN
              DB DROP
              DB EXIT

              INC DE           ;DE PTS TO DROPPED SGN(V-L) (OR L-V) SGN BYTE
              INC DE
              LD A,(DE)
              DEC A            ;SGN IS -1/0/1 SO A=1 IF SGN 1, ELSE 00 OR FF
              RET              ;NZ IF LOOP POSSIBLE, Z IF NOT (SGN=+VE)

;*******************************************************************************
;NEXT SUBROUTINE
;ENTRY: HL AND TEMPW1 PTS TO FIRST BYTE OF FOR-NEXT VARIABLE
;EXIT: NZ="USE FLOATING POINT". Z=INTEGER MATH DONE. A=00 IF NO LOOP, OR FF

NEXTSR:       XOR  A
              CP   (HL)
              RET  NZ          ;RET IF VALUE=FP

              INC  HL
              LD   B,(HL)
              INC  HL
              LD   E,(HL)
              INC  HL
              LD   D,(HL)      ;DE=VALUE, B=SGN OF VALUE
              INC  HL
              INC  HL
              CP   (HL)
              RET  NZ          ;RET IF LIMIT=F.P.

              INC HL
              INC HL
              INC HL
              INC HL
              INC HL
              CP (HL)
              RET  NZ          ;RET IF STEP=F.P.

              INC  HL
              LD   A,(HL)
              EX   AF,AF'      ;A AND A"=SGN OF STEP
              LD   A,(HL)
              INC  HL
              LD   C,(HL)
              INC  HL
              LD   H,(HL)
              LD   L,C         ;HL=STEP
              ADD  HL,DE       ;ADD STEP, VALUE
              ADC  A,B         ;ADD SGN STEP,SGN VALUE,CARRY FLAG
              RRCA
              ADC  A,0
              RET  NZ          ;RET IF OVERFLOW OF INTEGER MATHS

              SBC  A,A         ;A=SGN OF NEW VALUE
              EX   DE,HL       ;DE=NEW VALUE
              LD   HL,(DEST)
              INC  HL          ;SKIP 00
              LD   (HL),A      ;PLACE SGN
              LD   B,A         ;B=SGN OF NEW VALUE
              INC  HL
              LD   (HL),E
              INC  HL
              LD   (HL),D      ;PLACE NEW VALUE
              INC HL
              INC HL
              INC HL
              LD A,(HL)        ;A=SGN OF LIMIT
              INC  HL
              LD   C,(HL)
              INC  HL
              LD   H,(HL)
              LD   L,C         ;HL=LIMIT
              XOR  B           ;XOR SGN OF LIMIT,SGN OF VALUE
              JR   NZ,NEXTSR1  ;JR IF THEY DO NOT MATCH

              DEC  A           ;A=FF ("LOOP")
              SBC  HL,DE
              RET  Z           ;RET IF LIMIT=VALUE - LOOP

              SBC  A,A
              CPL              ;A=00 IF C, FF IF NC
              LD   B,A         ;B=LOOP/NO LOOP

NEXTSR1:      EX   AF,AF'      ;A=SGN OF STEP
              XOR  B           ;REVERSE LOOP/NO LOOP DECISION IF SGN NEWVAL=-VE
              CP   A           ;SET Z ("INTEGER MATHS DONE")
              RET              ;RET WITH LOOP/NO LOOP (FF/00)

ONERROR:   POP HL            ;NEXT STAT
           CALL RUNFLG
           JP NC,STMTLP1     ;CHECK SYNTAX FROM CHAD ONWARDS

           PUSH HL
           RST &18
           CP &B1            ;STOPTOK
           JR NZ,ONERR2

           RST &20           ;SKIP "STOP"
           XOR A             ;"OFF"
           JR ONERR3

ONERR2:    LD HL,(PPC)
           LD (ERRLN),HL
           LD A,(SUBPPC)
           LD (ERRSTAT),A
           PUSH HL
           CALL SKIPCSTAT
           POP AF
           INC A
           JR Z,ONERR3       ;OFF IF "ON ERROR" USED IN ELINE

           LD A,&81          ;TEMP/PERM BITS ARE "ON" (BITS 7/0)
                             ;IF AN ERROR OCCURS NOW, ERRSTAT/LN ARE USED TO
                             ;FIND "ON ERROR" AND DO WHAT IT SAYS
ONERR3:    LD (ONERRFLG),A
           RET

                                 ;RETURN, END PROC, FNDLNHL, IF, ELSE
                                 ;CALBAS, FOR, NEXT
