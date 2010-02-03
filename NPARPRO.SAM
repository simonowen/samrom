;PARPRO.SAM
;PARAM PROCESSING
;E.G.     TEST 1,A,C,2
;DEF PROC TEST A,B,C,D

;CHECK DEF PROC.
;IF NON-REF NUMERIC:
;  LOOK FOR VAR:
;    IF NOT FOUND:
;      GOTO STEP 2
;    IF FOUND:
;    1.SAVE ADDR OF TYPE/LEN BYTE SO IT CAN BE MADE "INVISIBLE" WHEN ALL PARAMS
;      HAVE BEEN PROCESSED, AND "REVEALED" AGAIN AT END PROC TIME.
;      KEEP LOOKING SO LAST LINK IN LETTER"S "CHAIN" FOUND.
;    2.CREATE NEW VARIABLE WITH SAME NAME, USING VALUE FROM PROC CALL. THIS IS
;      "INVIS" TILL ALL PARAMS PROCESSED, SINCE BIT 7 SET ON TLBYTE.
;      KEEP PTR TO NEW VARIABLE SO IT CAN BE MARKED "UNUSED" (BIT 5 SET) AT END
;      PROC TIME. PROCS USE SPECIAL ASSIGN ROUTINE THAT LOOKS FOR AN "UNUSED"
;      VAR OF THE SAME TYPE (FOR-NEXT/NORMAL) AND NAME LEN IN THE LETTER LIST
;      AND DOES A NORMAL ASSIGN IF THERE ISN"T ONE, OR OVERWRITES THE NAME AND
;      VALUE OF THE UNUSED VAR AND RESETS BIT 5 TO SHOW "USED". THIS AVOIDS
;      ACCUMULATION OF "UNUSED" VARS.

;IF REF NUMERIC:
;  TREAT LIKE NON-REF NUMERIC, EXCEPT THAT DURING ASSIGNMENT OF LOCAL VERSION,
;  THE "VALUE" SUPPLIED BY THE PROC LIST MUST BE A VARIABLE NAME. IT IS MADE
;  INVISIBLE WHEN PARAMS HAVE BEEN PROCESSED, AND ITS ADDR IS STORED SO THAT AT
;  END PROC IT CAN BE "REVEALED" AND ITS VALUE RESET FROM THE LOCAL VERSION.

;IF NON-REF STRING:
;  LOOK FOR VAR:
;    IF NOT FOUND:
;      FLAG "NO INVIS VERSION"
;      GOTO STEP 2
;    IF FOUND:
;    1.SAVE ADDR OF TYPE/LEN BYTE SO IT CAN BE MADE "INVISIBLE" WHEN ALL PARAMS
;      HAVE BEEN PROCESSED, AND FLAG "INVIS EXISTS".
;    2.CREATE NEW VARIABLE WITH SAME NAME, USING VALUE FROM PROC CALL. THIS WILL
;      BE IGNORED IF VAR. USED LATER IN PROC CALL LIST (IT IS AT SAVARS END).
;      KEEP NAME OF NEW VARIABLE SO IT CAN BE DELETED AT END PROC TIME, IF IT
;      EXISTS, AND SO A SEARCH CAN BE MADE FOR ANY "INVIS" VERSION IF FLAG
;      SAYS ONE EXISTS

;IF REF STRING/ARRAY:
;  LOOK FOR VAR:
;    IF NOT FOUND:
;      GOTO STEP 2
;    IF FOUND:
;    1.SAVE ADDR OF TYPE/LEN BYTE SO IT CAN BE MADE "INVISIBLE" WHEN ALL PARAMS
;      HAVE BEEN PROCESSED, AND FLAG "INVIS EXISTS".
;    2.LOOK FOR VAR NAMED IN PROC CALL. COPY TLBYTE AND NAME TO STORE. COPY
;      NAME GIVEN IN DEF PROC TO TLBYTE. (RENAME).
;      AT END PROC, FIND VAR, COPY ORIG NAME BACK FROM STORE.

;PROCESS PARAMS
;ENTRY: PRPTR PTS TO PROC PARAMS, DPPTR PTS TO DEF PROC PARAMS

PROPAR:    LD HL,(BSTKEND)
           DEC HL
           XOR A
           LD (HL),A
           DEC HL
           LD (HL),A         ;BSTK TERMINATOR
           LD (BSTKEND),HL

PROP2:     XOR A
           PUSH AF           ;MACHINE STACK TERMINATOR
           LD HL,HDR         ;USE TAPE HDR AREA AS RENAME STACK
           LD (HL),A
           LD (RNSTKE),HL
           CALL PTTODP       ;POINT TO DEF PROC LIST
           JP Z,PPM2         ;JP IF NO LIST

           CP &B9            ;DATATOK
           JR NZ,PPML

           POP AF
           LD HL,(PRPTR)
           LD A,(PRPTRP)
           JR RESTORE3

;RESTORE CMD STUCK IN HERE TO ALLOW JR!

RESTORE:   CALL SYNTAX3      ;EXPT1NUM OR USE ZERO IF CR/COLON

           CALL GETINT

RESTORE2:  CALL FNDLINE      ;RETURNS ADDR OF LINE BC. PAGE MAY BE SWITCHED
           DEC HL
           IN A,(251)

RESTORE3:  LD (DATADD),HL
           LD (DATADDP),A
           RET

;RESTORE-ZERO. USED BY CLEAR AND LOAD (PROGRAM)

RESTOREZ:  LD HL,0
           JR RESTORE2


PPLOOP:    CALL PTTODP       ;POINT TO DEF PROC LIST
           JP Z,PPM2         ;JP IF LIST ENDED

PPML:      LD HL,(BSTKEND)
           LD DE,-23         ;MAX SPACE NEEDED BY 1 PARAM
           ADD HL,DE         ;FIND NEW BSTKEND. CY
           LD DE,(HEAPEND)   ;LOWER IN MEM THAN BSTKEND
           SBC HL,DE         ;BSTKEND-(HEAPEND+1) - BSTK CANNOT COME AS FAR DOWN
                             ;HEAPEND.
           JP C,BSFERR       ;ERROR IF STACK SPACE TOO SMALL

           SUB &CE           ;REFTOK
           LD (REFFLG),A     ;Z IF REF VAR
           JR NZ,PPNREF

           RST &20           ;SKIP "REF"

PPNREF:    CALL LVFLAGS      ;LOOK FOR VAR IN DEF PROC LIST
           EX DE,HL          ;PROTECT ADDR OF PREV LINK (IF NOT FND)
           LD HL,(CHAD)
           LD (DPPTR),HL     ;(DPPTRP IS OK STILL)
           JP P,PPAS         ;JP IF STRING

           BIT 5,C           ;BIT 5=1 IF NUMERIC ARRAY
           JP NZ,PPAS        ;JP IF NUMERIC ARRAY

           EX AF,AF'
           JR Z,PPA2         ;JR IF NVAR IN DEF PROC LIST DOESN"T EXIST

           PUSH IX
           POP HL
           DEC HL            ;PT TO TLBYTE OF VAR
           PUSH HL           ;SAVE ADDR OF VAR TO MAKE "INVIS"
           IN A,(URPORT)
           OR &E0            ;BIT 7 SET SO "ADDR TO REVEAL" STORED. 111xxxxx
           PUSH AF
           INC HL

PPNLP:     CALL NVMLP        ;KEEP LOOKING. C IS TLBYTE
           JR Z,PPA15        ;JR IF NO SECOND COPY (D=0)

           LD A,0
           LD (FIRLET),A
           PUSH HL
           LD (HL),A
           INC HL
           LD (HL),&FF
           INC HL
           LD (HL),A
           INC HL
           LD (HL),A         ;SECOND COPY=MINUS ZERO
           POP DE            ;VALUE LOCN
;          JR PPA2           ;VARIABLE EXISTS. IF ASSIGNMENT IS MADE FROM
           DB &21            ;PROC VALUE, MINUS ZERO OVERWRITTEN. IF NOT,
                             ;DEFAULT RECOGNISES MINUS ZERO AS "NON-EXISTENT"

PPA15:     LD C,D            ;SIGNAL SIMPLE NUMERIC (F-N BIT SEEN AS ARRAY)
           EX DE,HL

PPA2:      CALL SYN1PP       ;SET UP FOR ASSIGNMENT - SETS "NEW VAR" FLAG
           CALL PTTOPR       ;POINT TO PROC LIST
           JP Z,PPD2         ;JP IF PROC LIST ENDED

;LOOK FOR "UNUSED" NUMERIC VAR
;ENTRY: TLBYTE/FIRLET SET

UNVLK:     LD BC,(TLBYTE)    ;C=TLBYTE, B=FIRLET
           LD A,B
           ADD A,A
           JR Z,PPA3         ;JR IF SEARCH NOT WANTED COS 2ND COPY EXISTS

           SUB &61*2
           LD E,A            ;LETTER TRANSFORMED TO WORD OFFSET (A=0, B=2..)
           LD D,0
           CALL ADDRNV       ;PT. HL AT NUMERIC VARS, PAGED IN
           SET 5,C           ;"UNUSED"
           LD A,C            ;DESIRED TYPE/LEN
           ADD HL,DE         ;INDEX INTO TABLE OF WORD PTRS.

UNVLP:     LD E,(HL)
           INC HL            ;PTR=FFFFH IF NO MORE VARS START WITH REQUIRED
           LD D,(HL)         ; LETTER. CAUSES CARRY AND CHECK FOR FF IN NVSPOV
           ADD HL,DE         ;ELSE DE IS A PTR TO NEXT VAR STARTING
                             ; WITH REQUIRED LETTER.
           JR C,PPA3         ;RET IF LIST ENDED OR SEVERE PAGE OVERFLOW

;           BIT 6,H
;          CALL NZ,INCURPAGE
           CALL CHKHL
           LD A,C            ;IN CASE CALL WAS MADE
           CP (HL)
           INC HL            ;PT TO PTR LSB
           JR NZ,UNVLP

           DEC HL            ;PT TO TLBYTE OF UNUSED VAR
           PUSH HL
           LD A,(HL)
           SET 7,(HL)        ;hidden** SO NOT USED TWICE!
           INC HL
           INC HL
           INC HL            ;PT TO OTHER LETTERS OF NAME (OR VALUE)
           AND &1F
           JR Z,PPA25        ;JR IF PTING TO VALUE (1-LET VAR)

           EX DE,HL
           LD HL,FIRLET+1
           LD C,A
           LD B,0
           LDIR              ;COPY NAME TO "UNUSED" VAR NAME AREA
           EX DE,HL

PPA25:     LD (DEST),HL      ;PTR TO VALUE AREA OF REUSED VAR
           POP HL            ;TLBYTE PTR
           PUSH HL           ;ADDR OF VAR TO MARK "USED"
           IN A,(URPORT)
           AND &1F
           LD (DESTP),A
           OR &20            ;001XXXXX
           PUSH AF           ;PAGE, AND BITS FOR "MARK AS USED"
           XOR A
           LD (FLAGX),A      ;"VAR EXISTS" (BIT 0)
           DB &DD            ;"JR+3"

PPA3:      LD HL,(NUMEND)    ;VAR WILL BE CREATED HERE SINCE AN "UNUSED" VAR
                             ;AREA IS NOT BEING RE-USED.

PPA4:      LD B,&40          ;BIT 7 OF PAGE WILL BE RESET. BIT 6 SET SO NOT
                             ;SEEN AS "TERMINATOR"
           CALL RUAHL        ;RECORD ADDR OF VAR ASSIGNED TO, ALLOWING
                             ;MARKING AS "UNUSED" BY END PROC
           CALL PTTOPR       ;PT TO PROC
           LD A,(REFFLG)
           AND A
           JR NZ,PPA5        ;JR UNLESS VAR BEING "ASSIGNED" TO IS REF-TYPE
                             ;NZ NEEDED HERE!!!

           CALL LVFLAGS      ;INSIST PROC SUPPLIES A VAR NAME
           JP P,PARERR       ;ERROR IF STRING

           EX AF,AF'
           JR NZ,PPRNE       ;JR IF VAR EXISTS

           DB CALC
           DB STKZERO
           DB DUP
           DB EXIT

           CALL ASSISR       ;IF PROC VAR DOES NOT EXIST CREATE DP VAR
                             ;WITH VALUE ZERO USING NAME AT T/L+33. C=0

           CALL CRTVAR35     ;LOOK FOR NON-EXISTENT PROC VAR AGAIN.
                             ;SO NO MATCH.
                             ;SET UP FOR ASSIGN TO PROC VAR.
                             ;CREATE PROC VAR WITH VALUE ZERO
           LD A,(TLBYTE)
           LD C,A
           CALL NUMLOOK      ;POINT TO (NOW EXISTING) PROC VAR
           XOR A             ;Z

PPRNE:     EX AF,AF'         ;SAVE Z IF ASSIGN TO DP VAR ALREADY MADE
           DEC IX            ;PT TO TLBYTE OF PROC VAR BEING REFFED
           PUSH IX           ;SAVE ADDR OF VAR TO MAKE "INVIS"
           IN A,(URPORT)
           OR &E0            ;BIT 7 SET SO "ADDR TO REVEAL" STORED. 111xxxxx
           PUSH AF
           LD B,&A0          ;SET BITS 5 AND 7 OF PAGE TO SHOW "REF" AND "PROC"
           PUSH HL
           EX DE,HL
           LD HL,(BSTKEND)
           SET 5,(HL)        ;SET BIT 5 OF PREVIOUS "VAR TO MARK UNUSED" TO SHOW
           EX DE,HL          ;"REF". BIT 7 BEING RES SHOWS "DEF PROC"
           CALL RUAHL        ;RECORD ADDR OF VALUE BEING ASSIGNED *FROM* SO IT
                             ;CAN BE ASSIGNED BACK *TO* AT END PROC E.G.
                             ;          TEST Z
                             ; DEF PROC TEST REF X
                             ; VALUE OF Z HAS BEEN ASSIGNED TO X. END PROC
                             ; ASSIGNS X'S VALUE TO Z, MARKS X "UNUSED"
           POP HL            ;PTR TO VALUE
           EX AF,AF'
           CALL NZ,HLTOFPCS  ;(NO CHANGE TO Z FLAG)
           CALL NZ,ASSIGN
           CP A              ;Z -  JR PPD1H

;PPA5:     CALL SELCHADP     ;ENSURE SEARCH OF VARS HASN'T ALTERED PAGE **
;PPA5:     CALL RCRC         ;RST 18H, CALL CRCOLON
;          JR NZ,PPA55

;          DB CALC
;          DB STKZERO
;          DB EXIT

;          INC HL
;          DEC (HL)          ;NZ FLAG, VALUE=MINUS ZERO
;          JR PPA45

PPA5:      CALL NZ,VALFET1   ;ASSIGN VALUE FROM PROC LIST TO VAR FROM DEF PROC
                             ;EXITS WITH DEST PAGED IN
PPD1H:     JR PPD1

;STRINGS/ARRAYS

PPAS:      LD B,&10          ;BIT 7 RES=NO VAR TO REVEAL, BIT 4 SET=STRING
           EX AF,AF'
           JR Z,PAS2         ;JR IF DEF PROC STRING/ARRAY DOESN"T EXIST

           LD B,&90          ;BIT 7 SET SO "GLOBAL S/A TO REVEAL" SHOWN
                             ;BIT 4 SET=STRING/ARRAY
           LD HL,(STRLOCN)   ;PTS TO TLBYTE OF S/A JUST FOUND
           PUSH HL           ;SAVE ADDR OF DEF PROC S/A TO MAKE "INVIS"
           IN A,(URPORT)
           AND &1F           ;BIT 7 RES SO NO RECORD MADE OF ADDR TO REVEAL
           OR &60            ;SET BIT 5 - ENSURE NOT ZERO. 011xxxxx
           PUSH AF

PAS2:      CALL PPSUB        ;RECORD DEF PROC NAME ON BSTK, WITH FLAG BITS
           CALL PTTOPR       ;POINT TO PROC LIST
           JR Z,PPD2         ;JR IF PROC LIST ENDED

           CALL SCOPNM       ;COPY DEF PROC NAME TO TLBYTE+33
           LD A,(REFFLG)
           AND A
           JR NZ,PPD0        ;JR IF NOT A REF VAR

;REF STRINGS/ARRAYS

           LD A,(FLAGS)
           PUSH AF
           CALL LVFLAGS      ;LOOK FOR PROC NAME
           JP P,REFSTR       ;JP IF PROC VAR=STRING

           POP AF            ;BIT 6,A=1 IF DP NAME=NUMERIC
           RL C              ;BIT 6,C=1 IF PROC VAR=ARRAY
           AND C             ;BIT 6=1 IF NUM ARRAYS
           CPL
           DB &0E            ;"JR+1"

REFSTR:    POP AF

           AND &40
           JP NZ,PARERR

           EX AF,AF'
           JR Z,PAS3         ;JR IF VAR DOESN'T EXIST

           LD HL,(RNSTKE)
           LD DE,(BSTKEND)   ;PTR TO DEF PROC NAME IN BSTK
           INC HL
           LD (HL),E
           INC HL
           LD (HL),D
           LD DE,(STRLOCN)   ;TLBYTE OF PROC NAME JUST FOUND
           INC HL
           LD (HL),E
           INC HL
           LD (HL),D
           IN A,(URPORT)
           OR &80            ;ENSURE NOT ZERO
           INC HL
           LD (HL),A
           LD (RNSTKE),HL

PAS3:      LD B,0            ;KEEP ORIG TLBYTE
           CALL PPSUB        ;RECORD ORIGINAL PROC VAR NAME SO IT CAN
                             ;BE RESTORED AT END PROC TIME.
                             ;NAME "UNDER" IT MUST BE DEF PROC NAME
           LD HL,(BSTKEND)
           DEC HL
           LD (HL),&FF       ;PRECEDE NAME WITH FF
           LD (BSTKEND),HL
           JR PPD1

;NON-REF STRINGS/ARRAYS

PPD0:      LD A,(FLAGS)
           ADD A,A
           JP M,PARERR       ;ERROR IF NUMERIC ARRAY

           LD HL,TLBYTE+33
           RES 6,(HL)        ;ENSURE THAT THE TYPE IS NOT ARRAY EVEN IF DP NAME
                             ;IS E.G. A$().
           CALL EXPTSTR
           CALL ASNST        ;CREATE STRING AT END OF SAVARS

PPD1:      CALL SELCHADP
           RST &18

PPD2:      LD (PRPTR),HL     ;UPDATE PROC PTR
           CP ")"
           JR NZ,PPD3

           RST &20           ;SKIP CLOSING BRACKET OF E.G. "NUM()"
           LD (PRPTR),HL

PPD3:      CP ","
           JR NZ,PPD35       ;JR IF NO MORE PROC PARAMS ** BUG FIX

           RST &20           ;SKIP "," IN PROC LIST
           LD (PRPTR),HL

PPD35:     CALL PTTODP
           CP ")"
           JR NZ,PPD4

           RST &20           ;SKIP CLOSING BRACKET OF E.G. "NUM()"

PPD4:      CP ","
           JR NZ,PPM2        ;EXIT IF NO MORE DEF PROC VARS **

           RST &20           ;SKIP "," IN DEF PROC LIST
           JP PPML           ;LOOP UNTIL PROC PARAMS FINISHED

;  PPD5:      LD A,(REFFLG)
 ;         AND A
  ;        JR NZ,PARERR      ;ERROR IF NO VALUE AND "REF" - VAR NAME REQUIRED

   ;       RST 20H           ;SKIP COMMA, GET HL=CHAD
    ;      JR PPD2

PPMIL:     PUSH AF
           CALL SELURPG
           POP AF            ;011xxxxx IF STRING/ARRAY, NO ADDR TO REVEAL
                             ;111XXXXX IF NUMBER, ADDR TO REVEAL
                             ;001XXXXX IF NUMBER, ADDR TO MAKE "USED"
           ADD A,A           ;CY IF NUMERIC WITH DISP TO STORE
                             ;P IF NEED TO MAKE USED AND NO NEED TO MAKE
                             ;INVISIBLE
           POP HL
           RES 7,(HL)        ;VISIBLE**
           JP P,PPM3

           SET 7,(HL)        ;EXISTING NUM/STR VAR MADE "INVISIBLE"
           LD B,&80
           CALL C,RUAHL      ;RECORD DISP FROM NVARS OF A "NUMBER TO REVEAL"
           DB &21            ;"JR+2"

PPM3:      RES 5,(HL)        ;"USED"

PPM2:      POP AF
           AND A
           JR NZ,PPMIL       ;LOOP TILL STACKED ADDRS TO MARK "INVIS" ALL DONE

RNMLP:     LD HL,(RNSTKE)
           LD A,(HL)
           AND A
           JR Z,RNMF

           CALL SELURPG
           DEC HL
           LD D,(HL)
           DEC HL
           LD E,(HL)
           PUSH DE           ;VARS PTR
           DEC HL
           LD D,(HL)
           DEC HL
           LD E,(HL)
           DEC HL
           LD (RNSTKE),HL
           EX DE,HL
           POP DE            ;HL=BSTK PTR TO DP NAME, DE=VARS PTR
           LD A,(HL)
           AND &0F
           LD C,A            ;C=LEN (OF DP NAME)
           LD A,(DE)
           AND &70           ;"VISIBLE"
           OR C              ;UPPER BITS (TYPE) FROM VARS, LEN FROM D.P. NAME
           LD B,A            ;NEW TLYBTE
           LD A,(DE)
           RLA
           CALL C,NEGVTR     ;IF VAR HAD BEEN MADE INVISIBLE, REVERSE "VAR TO
                             ;REVEAL" BIT ON D.P. NAME IN BSTK. (HAPPENS IF
                             ;ANY PROC VAR PASSED BY REFERENCE HAS SAME NAME
                             ;AS AS A DEF PROC VAR. VARS THAT ARE RENAMED NEED
                             ;NOT AND SHOULD NOT BE INVIS)
           LD A,B
           LD B,0
           CALL ILDISR       ;OVERWRITE NAME OF PROC VAR WITH NAME OF DEF PROC
           JR RNMLP

RNMF:      CALL PTTOPR
           RET Z             ;RET IF PROC LIST ENDED

PARERR:    RST &08
           DB 26             ;"Parameter error"

;LOOK FOR A PARTICULAR BSTK ENTRY SPECIFYING "VAR TO REVEAL" AND RESET BIT
;ENTRY: DE PTS TO TLBYTE OF VAR NAME IN VARS THAT IS ABOUT TO BE RENAMED

NEGVTR:    PUSH BC           ;B=NEW TLBYTE, C=LEN OF DP NAME
           PUSH DE           ;VARS PTR
           PUSH HL
           LD HL,(BSTKEND)

FDPNL:     LD A,(HL)
           AND A
           JR Z,PARERR       ;ERROR IF NOT FOUND

           BIT 4,A
           JR NZ,FDPN2       ;JR IF STRING, ELSE SKIP NUMERIC DISP

           LD C,3
           JR FDPN4

FDPN2:     CP &FF
           JR NZ,FDPN3       ;JR IF NOT REF NAME

           INC HL            ;SKIP FF
           LD A,(HL)

FDPN3:     AND &0F
           LD C,A
           LD A,(DE)
           XOR (HL)
           INC HL
           AND &AF
           JR Z,FDPN5        ;JR IF MATCH ON "VAR TO REVEAL" BIT, TYPE BITS
                             ;AND LEN BITS - IGNORE BIT 4 MISMATCH (ALWAYS
                             ;ZERO IN VARS AREA) AND BIT 6 ($/$ ARRAY)

FDPN4:     LD B,0
           ADD HL,BC         ;SKIP STRING NAME
           JR FDPNL

FDPN5:     PUSH HL
           PUSH DE
           LD B,C            ;LEN TO MATCH ON

FDPBL:     INC DE
           LD A,(DE)
           CP (HL)
           INC HL
           JR NZ,FDPN6

           DJNZ FDPBL

FDPN6:     POP DE            ;DP NAME PTR
           POP HL            ;BSTK SRCH PTR
           JR NZ,FDPN4       ;JR IF MATCH FAILED

           DEC HL
           RES 7,(HL)        ;RESET "VAR TO REVEAL" BIT
           POP HL            ;HL=DP NAME PTR
           POP DE            ;VARS PTR.
           POP BC
           RET


;CALCULATE DISP FROM NVARS TO CURRENT PAGE/HL, STORE IN BSTK IN PAGE/MOD 16K
;FORM, WITH BIT 7 OF PAGE SET IF VAR TO MARK UNUSED, RES IF TO MAKE VISIBLE.
;(USED LATER BY END PROC)

RUAHL:     LD A,(NVARSP)
           LD C,A
           LD DE,(NVARS)     ;CDE=NVARS
           IN A,(URPORT)     ;AHL=TLBYTE
           CALL SUBAHLCDE
           EX DE,HL          ;ADE=DISP FROM NVARS (PAGEFORM)
           LD HL,(BSTKEND)
           DEC HL
           LD (HL),D
           DEC HL
           LD (HL),E
           DEC HL
           AND &0F           ;BIT 4 LOW SHOWS NUMERIC
           OR B              ;SHOW "DISP TO VAR TO MARK "UNUSED"" AT END PROC
                             ;(BIT 7 SET) OR "DISP TO VAR TO REVEAL" (BIT 7 RES)
           LD (HL),A

BSSET:     LD (BSTKEND),HL
           RET

;RECORD TLBYTE AND NAME ON BSTK, USING B REG TO SET BITS 7 AND 4 OF TLBYTE
;EXIT: BC=0

PPSUB:     LD DE,TLBYTE      ;TLBYTE OF VAR NAME LOOKED FOR
           LD HL,(BSTKEND)
           LD A,(DE)
           AND &0F
           LD C,A
           LD A,(DE)
           AND &6F           ;**

           OR B              ;BIT 4 ALWAYS SET TO SHOW STRING NAME, BIT 7 SET
                             ;IF "GLOBAL TO REVEAL"
           LD B,0            ;BC=NAME LEN
           SBC HL,BC
           DEC HL            ;ALLOW SPACE FOR TLBYTE (NON-STANDARD FORM)
           LD (BSTKEND),HL
           EX DE,HL

ILDISR:    LD (DE),A
           INC DE
           INC HL
           LDIR              ;COPY NAME TO BSTK
           RET

;SUBROUTINE USED BY END PROC AND POP (PROC ADDR)
;ACTION: FOR NUMERICS, MARKS VAR (DISP ON BSTK) "UNUSED" OR "REVEALS" IT.
;FOR STR/ARRS, DELETE, AND REVEAL LAST INVIS VERSION IF ONE WAS HIDDEN BY PROC.

DELOCAL:   LD HL,(BSTKEND)
           LD A,(HL)         ;0000=TERMINATOR
                             ;FF=       ORIG NAME OF A REF VAR FOLLOWS. LOOK FOR
                             ;          NEW NAME (FOLLOWS ORIG), RENAME. KEEP
                             ;          NEW NAME ON BSTK TO ALLOW DELOCAL.

                             ;BIT 4 SET IF STRING/ARRAY NAME
                             ;          BIT 7 SET IF GLOBAL VERSION TO REVEAL,
                             ;          AS WELL AS LOCAL VERSION TO DELETE.

                             ;BIT 4 RES IF DISP OF NUMBER FROM NVARS
                             ;          BIT 7 SET IF IT IS A GLOBAL VERSION TO
                             ;          REVEAL.
                             ;          BIT 7 RES IF IT IS LOCAL VAR TO MARK
                             ;          "UNUSED".
                             ;             BITS 7/5 SET IF VALUE OF LOCAL VAR
                             ;             IS POINTED TO. COPY TO BUFFER.
                             ;             BIT 7 RES, 5 SET IF BUFFER VALUE
                             ;             TO BE COPIED TO VAR (ADDR=TLBYTE)
                             ;             AND TLBYTE TO BE MARKED "UNUSED"
           INC HL
           INC HL
           AND A
           JR Z,BSSET        ;JR IF TERMINATOR OF DATA USED BY END PROC
                             ;NO NEED TO CHECK FOR 2 ZEROS HERE
           DEC HL
           BIT 4,A
           JR NZ,DLOCS       ;JR IF STRING/ARRAY NAME

           PUSH AF
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC HL
           LD (BSTKEND),HL
           AND &0F
           LD C,A            ;CDE=DISP (PAGE FORM)
           CALL ADDRNV
           CALL ADDAHLCDE
           POP AF
           BIT 5,A
           JR Z,DLOC3        ;JR IF NOT ADDR OF REF VALUE/DP VAR TLBYTE

           RLA
           JR NC,DLOC2       ;JR IF HL POINTS TO DEF PROC VAR TLBYTE
                             ;(ALWAYS FOLLOWS REF VALUE ADDR)

           IN A,(URPORT)     ;WE DON"T NEED THIS REF VALUE ADDR YET - SAVE
           PUSH AF           ;PAGE
           PUSH HL           ;AND ADDR.
           JR DELOCAL

DLOC2:     SET 5,(HL)        ;VAR THAT WAS LOCAL MARKED "UNUSED"
           LD A,(HL)
           AND &1F
           ADD A,7
           LD C,A
           LD B,0
           ADD HL,BC         ;PT TO VALUE OF LOCAL VAR (LAST BYTE)
           LD DE,TEMPW1+4    ;USE TEMP AREA AS BUFFER
           LD C,5
           LDDR              ;COPY LOCAL VAR VALUE TO BUFFER
           INC DE
           EX DE,HL          ;HL PTS TO TEMPW1
           POP DE
           POP AF
           OUT (URPORT),A    ;DE PTS TO VALUE AREA OF REF VAR
           LD C,5
           LDIR
           JR DELOCAL

DLOC3:     RLA
           RES 7,(HL)        ;"VISIBLE" (IN CASE GLOBAL VAR NEEDS IT)
           JR C,DELOCAL      ;JR IF IT WAS

           SET 5,(HL)        ;VAR THAT WAS LOCAL MARKED "UNUSED"
           JR DELOCAL

DLOCS:     CP &FF
           JR NZ,DLCS2       ;JR IF NOT A REF STRING/ARRAY

           LD A,(HL)         ;ORIG TLBYTE
           AND &0F
           INC A
           LD C,A
           LD B,0
           PUSH HL
           ADD HL,BC         ;PT TO DEF PROC NAME FOLLOWING ORIG NAME.
           LD C,(HL)         ;DEF PROC CODE BYTE (BIT 7 HOLDS EXTRA
                             ;DATA, BIT 4 IS SET)
           PUSH BC
           LD A,C
           AND &6F           ;NORMAL T/L BYTE FOR DP NAME
                             ;(SIMPLE $ VS ARRAY $ IRREL. TO SEARCH ROUTINE)
           INC HL
           CALL LKBSV        ;LOOK FOR DEF PROC NAME
           JP Z,VNFERR

           POP BC            ;B=0, C=CODED BYTE
           POP DE            ;DE PTS TO ORIG NAME
           PUSH BC           ;CODE BYTE IN C
           LD A,(DE)
           AND &0F
           LD C,A
           INC C             ;BC=ORIG NAME LEN, INC T/L BYTE
           LD HL,(STRLOCN)   ;ADDR OF T/L BYTE IN VARS
           XOR (HL)
           AND &0F
           XOR (HL)          ;TYPE FROM VARS, NAME LEN FROM ORIG
           EX DE,HL
           LD (HL),A
           LDIR              ;COPY ORIG TLBYTE/NAME BACK TO VAR, BUT USE
                             ;ACTUAL VAR'S TYPE BITS. ($ ARRAY VS $)
           POP BC
           LD A,C
           AND &EF           ;RES 4,A **
           JR DLCS3          ;THERE"S NEVER A VERSION TO ERASE - RENAMED INSTEAD

;LOOK FOR STRING/ARRAY AND ERASE IT IF FOUND; ALSO, IF A GLOBAL VERSION WAS
;HIDDEN, LOOK FOR IT (LAST "INVIS" VERSION OF VAR) AND REVEAL IT.

DLCS2:     AND &EF           ;RES BIT 4 (VARS FORM ALWAYS HAS BIT 4 RESET)
           PUSH AF
           CALL LKBSV        ;LOOK FOR VAR NAMED ON BSTK. NZ=FND

           CALL NZ,ASDEL2    ;DELETE STRING/ARRAY WITH PAGE/LEN AT HL IF IT
                             ;WAS FOUND
           POP AF

DLCS3:     BIT 7,A
           CALL NZ,STARYLK2  ;LOOK FOR INVIS VERSION IF THERE WAS A GLOBAL
                             ;VERSION
           JR Z,DELCLH       ;JR IF NONE (SHOULD BE AT LEAST ONE...)

;FIND LAST INVISIBLE VERSION

DLOCL:     PUSH DE           ;PTR TO T/L BYTE IN VARS
           IN A,(URPORT)
           PUSH AF
           CALL FLNOMTCH
           POP BC
           POP HL            ;BHL=ADDR OF PREVIOUS "FOUND" VAR (T/L BYTE)
           JR NZ,DLOCL       ;JR IF WE JUST FOUND ANOTHER

           LD A,B
           OUT (URPORT),A    ;PT TO LAST INVIS VERSION
           RES 7,(HL)        ;MAKE VISIBLE

DELCLH:    JP DELOCAL


;POINT CHAD TO DEF PROC PARAMETER LIST OR PROC PARAMETER LIST
;EXIT: Z IF CHAD POINTS TO CR/COLON. A COMMA WILL BE SKIPPED.

PTTODP:    LD HL,(DPPTR)
           LD A,(DPPTRP)
           JR PTTOC

PTTOPR:    LD HL,(PRPTR)
           LD A,(PRPTRP)

PTTOC:     LD (CHAD),HL
           LD (CHADP),A
           CALL SELURPG
           JP RCRC
;           RST 18H
 ;          CP 0DH
  ;         RET Z

;           CP ":"
 ;          RET
