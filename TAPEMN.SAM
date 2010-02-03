;TAPEMN.SAM - SAVE/LOAD/MERGE/VERIFY CONTROL
;SYNTAX CHECK FOR ALL TAPE COMMANDS STARTS AT SLMVC

;THERE ARE 2 BUFFERS - HDR (HEADER - REQUESTED) AND HDL (HEADER - LOADED)
;HEADER BUFFER FORMAT:

;0       - TYPE (16=BAS, 17=NUM ARRAY, 18=STR ARRAY, 19=CODE, 20=SCREEN$)
;1-10    - (10) FILE NAME, PADDED WITH SPACES IF NEEDED
;11-14   - (4)  ALLOWS LONGER FILE NAME IF SLDEV<>T
;15      - (1)  FLAGS. BIT 0=INVIS. NAME, BIT 1=PROTECTED CODE
;16-26   - (11) IF TYPE 17 OR 18=ARRAY/STRING TLBYTE/NAME (11)
;16             IF TYPE 20     =SCREEN MODE
;16-18          IF TYPE 16     =PROG LEN EXCLUDING VARS (3)
;19-21          IF TYPE 16     =PROG LEN PLUS NVARS, EXCLUDING GAP AND SAVARS (3)
;22-24          IF TYPE 16     =PROG LEN PLUS NVARS, EXCLUDING SAVARS (3)
;27        (1) DIRE - DIRECTORY ENTRY NUMBER (HDR ONLY) NOT USED
;28-30   - (4)  SPARE

;DISP OF HDN FROM BUFFER START:

;31-33   - (3)  REL PAGE FORM START ADDR IF CODE, ACTUAL ADDR IF PROG OR DATA.
;               (OR FFXXXX IF ARRAY DOESN'T EXIST)
;34-36   - (3)  DATA LENGTH  (PAGEFORM) OR FFFFFF IF E.G. LOAD "" CODE
;               PROG+VARS LENGTH, OR ARRAY LEN (OR FFXXXX IF NON-EXISTENT)
;37-39   - (3)  EXECUTE ADDR. IF CODE, REL PAGE FORM, OR FFXXXX IF NO EXEC.
;               IF BAS AND NO AUTO-RUN, FIRST BYTE=FF, ELSE FIRST=0 AND THE
;               AUTO-RUN LINE NUMBER IS IN THE NEXT TWO BYTES.

;40-79          COMMENT. NOT INITIALISED - CAN BE POKED, PEEKED.

;SAVE, LOAD, VERIFY, MERGE ALL ENTER HERE

SLMVC:     SUB &A6           ;OVERTOK
           LD (OVERF),A
           JR NZ,NOVE

           RST &20           ;SKIP "OVER"

NOVE:      LD HL,(PSLD)
           LD (SLDEV),HL     ;PERM DEVICE TO TEMP
           CALL EXPTSTR      ;NAME
           JR NC,HDR2        ;JR IF NOT RUNNING

           LD HL,HDR+1       ;HEADER BUFFER ADDR
           LD B,25

HDCLP:     LD (HL),&20       ;CLEAR NAMES AREAS WITH SPACES
           INC HL
           DJNZ HDCLP

           LD B,14

HDCLP2:    LD (HL),&FF       ;CLEAR REST WITH FFH
           INC HL
           DJNZ HDCLP2

           CALL SBFSR        ;BC=NAME LEN, DE=START, IN SYS PAGE BUFFER **
           LD H,NMLEN+1
           CALL TCHK
           JR Z,NLTP

           LD H,NMLEN+5      ;ALLOW EXTRA CHARS IF NON-T

NLTP:      LD A,C
           CP H
           JR NC,IFNER       ;LIMIT NAME LEN

           LD HL,HDR+1       ;DEST FOR FIRST NAME CHAR
           LD (HL),&FF       ;ASSUME NAME IS NULL
           AND A
           LD A,(CURCMD)
           JR NZ,TNMOK       ;JR IF NAME LEN IS 1-MAX CHARS LONG

           CP SAVETOK
           JR NZ,HDR2        ;JR IF NOT SAVE (WITH A NULL NAME)

IFNER:     RST &08
           DB 18             ;'Invalid file name'

TNMOK:     CP SAVETOK
           JR NZ,MVTNM

           LD A,(DE)         ;FIRST CHAR OF SAVE NAME CAN BE
           CP 4              ;0 FOR NOT INVIS, NOT PROT
           JR NC,MVTNM       ;1 FOR     INVIS, NOT PROT
                             ;2 FOR NOT INVIS, PROT
                             ;3 FOR INVIS, PROT
           LD (HDR+HFG),A
           INC DE            ;(INVIS. MEANS NAME NOT PRINTED DURING LOADING,
           DEC C             ;PROT. MEANS AUTO-RUN CODE FILE CANNOT BE STOPPED)
           JR Z,HDR2         ;FIRST CHAR IS NOT INCLUDED IN NAME PROPER.

MVTNM:     EX DE,HL
           LDIR              ;COPY NAME TO BUFFER

HDR2:      RST &18
           CP &FF
           JR Z,HDR4         ;JR IF FN LEADER FOUND (SHOULD BE SCREEN$ OR CODE)

HDR3:      CP LINETOK
           JP NZ,HDR6        ;IF NOT 'LINE', CHECK FOR 'DATA', OR CR/COLON

           LD A,(CURCMD)
           CP VERIFYTOK
           JP Z,NONSENSE     ;VERIFY "NAME" LINE n NOT ALLOWED

           CALL SEXPT1NUM
           JR NC,HDRLNOK     ;JR IF NOT RUNNING

           CALL GETINT       ;LINE
           LD HL,HDR+HDN+6   ;PTR TO AUTO-RUN LINE AREA
           LD (HL),0         ;FLAG 'AUTORUN'
           INC HL
           LD (HL),C
           INC HL
           LD (HL),B         ;PLACE LINE NO.

;S/L/V/M "NAME":   ENTERS HERE, AND S/L/M "NAME" LINE n

HDRLNOK:   CALL CHKEND

           CALL ADDRPROG
           LD (HDR+HDN),A
           LD (HDR+HDN+1),HL ;START
           EX DE,HL
           LD C,A            ;CDE=PROG
           CALL ADDRELN
           DEC HL            ;DEC AHL SO PROG LEN-1 USED. END OF SAVARS MARKER
           BIT 7,H           ;NOT SAVED. (SO ELINE NEVER = PROG WHEN PROG
           JR NZ,HDRPLC      ;SPACE IS RECLAIMED, MAKEROOM AT (PROG) ALWAYS OK)

           DEC A

HDRPLC:    CALL SUBAHLCDE    ;GET AHL=PAGE FORM OF DIFFERENCE
           LD (HDR+HDN+3),A
           LD (HDR+HDN+4),HL ;FILE LEN=ELINE-PROG

           LD B,3            ;DO NVARS, NUMEND, SAVARS
           LD IX,NVARS+1
           LD IY,HDR+16      ;STORE IN HDR+16/+19/+22

RNTVL:     LD H,(IX+0)
           DEC IX
           LD L,(IX+0)
           DEC IX
           LD A,(IX+0)
           DEC IX
           CALL SUBAHLCDE
           LD (IY+0),A
           INC IY
           LD (IY+0),L
           INC IY
           LD (IY+0),H
           INC IY
           DJNZ RNTVL
                             ;PROGRAM (EX. STR/ARRAYS AND GAP) LEN
                             ;PROGRAM (EX. STR/ARRAYS) LEN

           LD A,16           ;TYPE=PROG
           JP SLVMC          ;JP TO COMMON ROUTINE

HDR4:      RST &20           ;SKIP FFH
           CP CODETOK
           JR NZ,HDR5

;HANDLE "NAME" CODE <START><,LENGTH><,EXECUTION ADDR>

           LD DE,HDR+HDN     ;PT TO HDR AREA FOR NUMBERS
           LD B,3            ;MAX OF 3 NUMS FOLLOW 'CODE'

           RST &20           ;SKIP 'CODE'
           CALL CRCOLON
           JR NZ,HDRNMS      ;JR IF NOT JUST "NAME" CODE

           LD A,(CURCMD)
           CP SAVETOK
           JR Z,HDNSHP       ;SAVE "NAME" CODE NEEDS PARAMS

           JR HDRNOK

HDRNLP:    DEC B

HDNSHP:    JP Z,NONSENSE     ;JR IF MORE THAN 3 NUMBERS USED

           RST &20

HDRNMS:    PUSH BC           ;COUNT
           PUSH DE           ;PTR
           CALL EXPT1NUM
           JR NC,HDRN1       ;JR IF NOT RUNNING

           CALL UNSTLEN      ;GET NUMBER IN AHL IN PAGE/ADDR FORM
           POP DE
           EX DE,HL          ;HL=PTR, ADE=ADDR
           LD (HL),A
           INC HL
           LD (HL),E
           INC HL
           SET 7,D
           LD (HL),D
           INC HL
           PUSH HL           ;PTR

HDRN1:     POP DE
           POP BC            ;NO. COUNT
           RST &18
           CP ","
           JR Z,HDRNLP       ;LOOP FOR ANOTHER NO. IF A COMMA FOLLOWED THE LAST

           LD A,(CURCMD)
           CP SAVETOK
           JR NZ,HDRNOK

           LD A,B
           CP 3
           JR Z,HDNSHP       ;SAVE CANNOT HAVE JUST SAVE "NAME" START

HDRNOK:    CALL CHKEND

;EXECUTION ROUTINE FOR SAVE/LOAD/VERIFY "NAME" CODE ETC.

           LD A,19           ;TYPE=CODE
           JP SLVMC          ;JP TO COMMON ROUTINE

HDR5:      CP SCRNTOK
           JP NZ,NONSENSE

           CALL SABORTER     ;SKIP 'SCREEN$'

           CALL FLITE        ;GET BC=LINE INT TABLE LEN
           LD HL,&0028
           ADD HL,BC         ;ALLOW FOR PALTAB
           LD A,(MODE)
           LD (HDR+16),A     ;MODE
           CALL SCRLEN       ;GET SCREEN LEN FOR MODE IN ADE
           LD C,A
           PUSH HL           ;LEN OF PALTAB AND LINITAB
           PUSH DE           ;SCREEN LEN
           ADD HL,DE
           EX DE,HL          ;ADE=TOT LEN
           LD HL,HDR+HDN
           LD A,(CUSCRNP)
           LD (HL),A
           INC HL
           LD (HL),0
           INC HL
           LD (HL),&80       ;START IS AT 8000H IN SCREEN PAGE
           INC HL
           LD (HL),C         ;LEN MSB
           INC HL
           LD (HL),E
           INC HL
           LD (HL),D         ;LEN
           POP DE
           SET 7,D           ;DE=SCREEN END IF M0/1
           INC C
           DEC C
           JR Z,HDRSC2       ;JR IF M0 OR M1

           INC A             ;SECOND PAGE OF M2/M3 SCREEN

HDRSC2:    POP BC            ;PALTAB+LINITAB LEN
           LD HL,PALTAB
           CALL TSURPG
           LDIR              ;COPY TO END OF SCREEN
           LD A,20           ;'SCREEN$' TYPE
           JR SLVMC          ;JR TO COMMON ROUTINE

HDR6:      CP &B9            ;DATATOK
           JP NZ,HDRLNOK

           LD A,(CURCMD)
           CP MERGETOK
           JR Z,SVDNH        ;MERGE "NAME" DATA NOT ALLOWED

           RST &20           ;SKIP 'DATA'
           CALL R1OFFCL
           DW LENGSR         ;** BUG FIX
           PUSH AF           ;FOUND/NOT FOUND STATUS
           LD B,18           ;TYPE ASSUMED TO BE STRING
           LD A,(FLAGS)
           ADD A,A
           JP M,SVDNU        ;JP IF NUMERIC

           JR NC,SVDS1       ;JR IF NOT RUNNING

           RST &18
           CP ")"
           JR NZ,SVDTT       ;MUST BE NAME$

           RST &20           ;SKIP ')'
           JR SVDTT          ;MUST BE NAME$()

SVDS1:     BIT 6,C
           JR Z,SVDTT        ;JR IF NO OPENING BRACKET USED

           JR SVNSC

SVDNU:     DEC B             ;TYPE=17
           BIT 5,C

SVDNH:     JP Z,NONSENSE     ;ERROR IF NON-ARRAY NUMERIC

SVNSC:     RST &18
           CALL INSISCBRK    ;INSIST ON ')', SKIP

SVDTT:     POP AF
           PUSH BC           ;B=TYPE, 17 (NUMERIC) OR 18 (STRING)
           JR Z,SVNWAR       ;JR IF NOT FOUND

           LD A,(MEMVAL+2)
           LD (HDR+HDN),A
           LD (HDR+HDN+1),DE ;START OF VARIABLE
           LD HL,MEMVAL+3    ;TEXT LEN DATA STORED HERE
           CALL ADD14        ;ALLOW FOR T/L,NAME,PG,MOD 16K
           LD (HDR+HDN+3),A
           LD (HDR+HDN+4),HL ;LEN OF VARIABLE INC. HEADER
           JR SVDTCE

;NEW ARRAY

SVNWAR:    LD A,(CURCMD)
           CP LOADTOK
           JP NZ,VNFERR      ;ERROR IF VERIFYING OR SAVING A NEW ARRAY

SVDTCE:    POP AF            ;TYPE
           CALL CHKEND

           LD HL,TLBYTE
           LD DE,HDR+16
           LD BC,11
           LDIR              ;COPY DETAILS OF VAR NAME TO HDR

SLVMC:     LD (HDR),A

SLVM1:     LD A,(CURCMD)
           CP SAVETOK
           JP Z,SAMAIN       ;SAVE PROGRAM

;LOAD/VERIFY/MERGE MAIN

LVMMAIN:   CALL TORN
           JR Z,LKTH         ;JR IF TAPE, OR N AND NO DOS

           LD IX,HDR
           RST &08
           DB FOPHK          ;CALL DOS TO LOAD HDR (OPEN FILE FOR READ)
           JR LDFL

;JUMP FROM DOS (E=3) - LOAD HEADER, THEN FILE FROM TAPE/NET

LKTH:      CALL LKHDR        ;GET THE CORRECT HEADER

;JUMP FROM DOS (E=1) - LOAD JUST FILE FROM TAPE/NET

LDFL:      LD A,(HDL)
           SUB 19
           JR NZ,LVM2        ;JR IF NOT CODE

           LD A,(CURCMD)
           CP MERGETOK
           JR NZ,CDSCVE      ;JR IF LOAD/VERIFY CODE

           LD A,(HDL+HFG)
           AND &02
           JR NZ,CDSCVE      ;JR IF PROT - CANNOT STOP AUTO-RUN CODE

           DEC A
           LD (HDL+HDN+6),A  ;PREVENT AUTO-RUN IF MERGE UNPROTECTED CODE
           JR CDSCVE

LVM2:      DEC A
           JP Z,LDSCRN       ;JR IF TYPE=SCREEN$ (20)

           LD A,(CURCMD)
           CP LOADTOK
           JP Z,LDPRDT       ;JR IF LOAD (A PROGRAM OR ARRAY - TYPES 16-18)

           CP MERGETOK
           JP Z,MEPROG       ;JP IF MERGE PROGRAM - CONT WITH VERIFY

;VERIFY PROGRAM, DATA, CODE OR SCREEN, LOAD CODE

CDSCVE:    CALL RDRLEN       ;CDE='DESIRED' LEN FROM HDR
           LD B,C
           PUSH BC
           PUSH DE
           CALL RDLLEN       ; CDE=ACTUAL LEN OR FFFFFF
           POP HL
           POP AF
           INC A
           JR Z,HDLNM        ;JR IF DESIRED LEN UNSPECIFIED (ONLY IF CODE)

           DEC A
           AND A
           SBC HL,DE
           SBC A,C
           JP C,TERROR       ;ERROR IF LEN OF FILE IS LONGER THAN REQUESTED

           OR H
           OR L
           JR Z,HDLNM        ;EQUAL LENS OK FOR VERIFY OR LOAD

           LD A,(CURCMD)
           CP VERIFYTOK
           JP Z,TERROR       ;IF LOAD, LEN CAN BE LESS THAN 'DESIRED'
                             ;BUT VERIFY INSISTS ON EQUALITY

HDLNM:     LD HL,HDR+HDN
           CALL RDTHREE      ;CDE=DESIRED START
           LD A,(HDR)
           CP 19
           LD A,C
           JR NZ,STSPEC3     ;FOR ANYTHING APART FROM CODE, 'START' IS ALWAYS
                             ;'DESIRED' (E.G. CUR. PROG, SCREEN OR VAR. ADDR)
           INC A
           JR NZ,STSPEC      ;JR IF DESIRED START WAS SPECIFIED: E.G.
                             ;VERIFY/LOAD "NAME" CODE 40000
           LD HL,HDL+HDN
           CALL RDTHREE      ;CDE=LOADED START
           JR STSPEC2

STSPEC:  ;  IN A,(LRPORT)     ;PAGE AT 0000
         ;  ADD A,C           ;CONVERT REL LOAD ADDR TO ABS
         ;  LD C,A
           DEC C

STSPEC2:   LD A,(LDCO)
           ADD A,C

STSPEC3:   PUSH DE
           CALL TSURPG       ;START SWITCHED IN AT HL
           CALL RDLLEN       ;CDE=ACTUAL LEN FROM LOADED HDL (PAGE FORM)
           POP HL            ;START
           CALL LDVDBLK      ;LOAD OR VERIFY BLOCK
           LD A,(CURCMD)
           CP VERIFYTOK
           RET Z

;MUST BE LOAD CODE

           LD A,(HDL+HFG)
           BIT 1,A
           JR NZ,HDNSTP      ;JR IF PROT - CANNOT STOP AUTO-RUN CODE

           LD A,(HDR+HDN+6)
           LD HL,(HDR+HDN+7)
           CP &FF
           JR NZ,HDLDEX      ;JR IF LOAD EXEC - OVER-RIDES ANY SAVED EXEC

HDNSTP:    LD A,(HDL+HDN+6)
           LD HL,(HDL+HDN+7) ;AHL=LOADED EXEC ADDR (REL PAGE FORM) OR FFXXXX
           CP &FF
           RET Z             ;RET IF NO EXEC ADDR

HDLDEX:    CALL PDPSR2       ;SWITCH IN CORRECT PAGE, FIDDLE HL IF NEEDED
           LD B,H
           LD C,L
           JP R1OFFCLBC      ;CALL BC WITH ROM1 OFF

;LOAD OR VERIFY DATA BLOCK ACCORDING TO CURCMD

LDVDBLK:   LD A,(CURCMD)
           CP VERIFYTOK
           JR Z,LDVD2        ;JR WITH NC IF VERIFY

;CALLED TO LOAD A DATA BLOCK

LDDBLK:    SCF               ;LOAD

LDVD2:     LD A,&FF          ;DATA BLOCK
           PUSH AF
           CALL TORN         ;Z IF 'T', OR 'N' AND NO DOS
           JR Z,LDVD3        ;IF NO DOS LET ROM HANDLE NET LOAD

DOSLV:     LD IX,HDR
           POP AF            ;HL=DEST, CDE=LEN
           JR C,DOSLD

           RST &08
           DB VFYHK          ;DOS VERIFY FOR DISC/NET ETC
           RET

DOSLD:     RST &08
           DB LDHK           ;DOS LOAD FOR DISC/NET ETC
           RET

LDVD3:     POP AF
           CALL LDBYTES
           RET C

TERROR:    RST &08
           DB 19             ;'LOADING ERROR'


;LOAD/VERIFY SCREEN$ FILE

LDSCRN:    LD A,(MODE)
           LD C,A
           LD HL,HDL+16
           LD A,(HL)
           CP &20            ;20H IF THIS IS A CODE FILE LOADED AS SCREEN$
           JR NZ,LDSC2       ;JR IF IT IS A SCREEN$ FILE LOADED AS SCREEN$

           LD (HL),C         ;SET MODE=CURRENT MODE FOR CODE FILE
           LD A,C            ;SKIP MODET

LDSC2:     CP C
           CALL NZ,MODET     ;SELECT MODE IF A NEW ONE IS NEEDED

           CALL SELSCRN
           LD HL,HDL+HDN+3
           CALL RDTHREE      ;CDE=LOADED LEN
           LD HL,&8000       ;SCREEN START
           PUSH DE
           CALL LDVDBLK
           POP DE
           LD A,(CURCMD)
           CP VERIFYTOK
           RET Z

           LD A,(HDL+16)     ;MODE
           PUSH DE
           CALL SCRLEN       ;DE=SCREEN LEN MOD 16K FOR MODE. NC
           POP HL            ;FILE LEN MOD 16K
           RES 7,H
           SBC HL,DE         ;HL=PALTAB/LINICOLS
           RET Z             ;RET IF EXACT LEN CODE FILE

           RET C             ;OR SHORT

           SET 7,D           ;DE=9B00 IF MODE 0, BB00 IF MODE 1, ELSE A000 (AND
                             ;LOAD BLOCK MEANS C/D PAGED).
           LD BC,40          ;SUB PALTAB LEN
           SBC HL,BC
           PUSH HL           ;HL=LINICOL LEN IN FILE
           EX DE,HL          ;HL PTS TO PALTAB AND LINICOLS DATA
           LD A,(CUSCRNP)
           CALL CUS2         ;Z IF CURRENT SCREEN IS DISPLAYED
           LD DE,PALTAB
           JR Z,PMV          ;MOVE LOADED PALTAB TO ACTIVE PALTAB, OR STORED
                             ;AREA, IF CURRENT SCRN IS INVIS.
           LD DE,PALBUF-&4000

PMV:       LDIR              ;COPY PALTAB (40 BYTES)

           LD A,(HL)
           CP 195
           LD (DE),A
           POP BC            ;LINICOL LEN
           RET NC            ;RET IF NO LINICOLS

           LD A,B
           CP 2
           RET NC            ;LIMIT LINICOL LEN TO 01FFH FOR SAFETY

           DI
           LDIR
           LD A,&FF
           OUT (STATPORT),A  ;PREVENT ANY LINE INTS TILL AFTER NEXT FRAME
           EI
           RET

;LOAD PROGRAM OR DATA FILE

LDPRDT:    CALL RDLLEN       ;CDE=LEN OF FILE TO LOAD (PAGEFORM)
           PUSH DE
           LD A,C
           CALL RDRLEN       ;CDE=LEN OF CURRENT ('DESIRED') AREA
           POP HL            ;AHL=LOADING LEN
           INC C
           JR Z,LDNAR        ;JR IF LOADING A NEW ARRAY (CUR LEN=FFXXXX)

           DEC C
           CALL SUBAHLCDE
           JR C,LDSZOK       ;JR IF CURRENT FILE IS BIGGER THAN LOADING

LDNAR:     CALL TSTRMAHL     ;CHECK AHL BYTES OK. (PAGEFORM)

LDSZOK:    LD HL,HDR+HDN
           CALL RDTHREE      ;START OF EXISTING ARRAY (PAGE FORM) OR FFXXXX
           LD A,C
           INC C
           JR Z,LDCRA        ;JR IF NEW ARRAY

           PUSH AF
           PUSH DE           ;START
           CALL RDRLEN       ;CURRENT LEN TO CDE (PAGEFORM)
           PUSH DE
           LD D,C
           POP BC            ;DBC=LEN
           POP HL
           POP AF            ;AHL=START
           CALL SELURPG
           LD A,(HDR)
           SUB 16
           JR NZ,LDCRX       ;JR IF NOT PROGRAM (IT'S AN ARRAY)

           LD (NVARSP),A
           LD (NVARS+1),A    ;SINCE PROG AND NVARS HAVE BEEN DELETED,
                             ;PREVENT XOINTERS ALTERING NVARS, CAUSING
                             ;SADJ WITH NO FOR-NEXTS, OR ODD BSTK
           LD A,D            ;ABC=LEN
           CALL RECL2BIG     ;DELETE CURRENT PROGRAM
           LD HL,(BASSTK)
           LD (BSTKEND),HL   ;CLEAR BASIC STACK - DATA OBSOLETE
           CALL ADDRPROG     ;ADDR PROG
           JR LDCR3

LDCRX:     LD A,D            ;ABC=LEN
           CALL RECL2BIG     ;DELETE CURRENT ARRAY

LDCRA:     CALL ADDRELN      ;ADDRESS ELINE
           CALL DECPTR       ;DEC HL AND CHANGE PAGE IF BELOW 8000H

LDCR3:     PUSH HL
           CALL RDLLEN       ;CDE=LOADING LEN (PAGEFORM)
           POP HL
           PUSH BC
           PUSH DE
           LD A,C
           LD B,D
           LD C,E
           CALL MKRBIG       ;OPEN ABC AT HL
           LD (HL),&FF       ;IN CASE LOAD FAILS!!
           POP DE
           POP BC
           IN A,(251)
           PUSH AF
           PUSH HL
           CALL LDDBLK       ;LOAD MAIN BLOCK
           POP DE
           POP BC            ;BDE=START OF MAIN BLK
           LD A,(HDR)
           CP 16
           JR Z,LDPROG       ;JR IF PROGRAM WAS LOADED

           LD A,B
           CALL TSURPG       ;DE PTS TO LOADED ARRAY TYPE/LEN BYTE
           LD HL,HDR+16
           LD A,(DE)
           XOR (HL)
           AND &F0
           XOR (HL)
           LD (DE),A         ;TYPE FROM LOADED ARRAY, NAME LEN FROM REQUESTED
           INC DE            ;PT TO NAME
           INC HL
           LD BC,10
           LDIR              ;COPY REQUESTED NAME TO SAVARS
           RET

;BASIC FILE LOADED - SET VARS

LDPROG:    LD B,3            ;DO NVARS, NUMEND, SAVARS
           LD HL,HDL+16
           LD IY,NVARS+1

SNTVL:     CALL RDTHREE      ;CDE=HDL DATA (LEN OF PROG ALONE, WITH NVARS, ETC)
           INC HL
           PUSH HL
           CALL ADDRPROG
           CALL ADDAHLCDE
           LD (IY+0),H
           DEC IY
           LD (IY+0),L
           DEC IY
           LD (IY+0),A
           DEC IY
           POP HL            ;HDL PTR
           DJNZ SNTVL        ;SET NVARS, NUMEND, SAVARS

           CALL RESTOREZ     ;SET DATA PTR TO START OF PROG
           CALL R1OFFCL
           DW DOCOMP         ;DEF PROCS/DEF FNS AND LABELS NEED DOING -
                             ;ANY PRE-PASS OF LABELS ETC IS OBSOLETE.
           LD A,(HDR+HDN+6)
           LD HL,(HDR+HDN+7)
           AND A
           JR Z,LDUSLN       ;JR IF THERE'S A DESIRED START LINE (LD "N" LINE n)

           LD A,(HDL+HDN+6)
           LD HL,(HDL+HDN+7)
           AND A
           RET NZ

LDUSLN:    DEC A             ;A=FF
           LD (PPC+1),A      ;ENSURE GOTO SEARCH STARTS FROM PROG, NOT CLA
           JP GOTO3          ;SET VARS SO GOTO LINE HAPPENS NEXT

;LOOK FOR A HEADER FROM TAPE OR NET.
;LOOK FOR A MATCHING HEADER (USING HDR AREA), LOADING HEADERS TO HDL AREA
;PRINT NAMES IF BIT 0,(TPROMPTS)=0. LOOP TILL MATCH FOUND.

LKHDR:     CALL LDHDR
           JR NC,LKHDR        ;LOOP IF ERROR

           CALL STREAMFE     ;"S"
           LD A,(HDL)        ;TYPE
           LD C,A
           OR &10
           CP &15
           JR NC,LKHDR

           LD HL,HDL+HFG
           LD A,(TPROMPTS)
           AND 1             ;ISOLATE SYS VAR 'PRINT NAMES' BIT.
           OR (HL)           ;OR WITH HDR VERSION
           LD (HL),A
           RRA
           JR C,LKHNP        ;JR IF NO PRINTING OF NAMES WANTED

           LD A,&0D
           RST &10
           LD A,"*"
           BIT 4,C
           CALL Z,&0010      ;PRINT "*" BEFORE NAME IF ZX TYPE
           LD A,C            ;A=16-20
           OR &10
           SUB 12            ;A=4-8
           CALL UTMSG        ;PRINT TYPE MSG

LKHNP:     LD BC,NMLEN*256   ;C=0
           LD DE,HDL
           LD HL,HDR
           LD A,(DE)         ;LOADED TYPE
           CP (HL)           ;CP DESIRED TYPE
           JR Z,LKHTM

           SUB 20            ;19->255, 20->0
           ADC A,0           ;19->0,20->0
           JR NZ,SNMN        ;JR IF NOT CODE/SCREEN$

           LD A,(HL)         ;ELSE
           LD (DE),A         ;LOADED=DESIRED (DESPITE UTMSG)
           SUB 20
           ADC A,0
           JR Z,LKHTM        ;IF OTHER FILE IS CODE/SCREEN$, ACCEPT MATCH

SNMN:      INC C             ;'NO MATCH' - TYPES DIFFER

LKHTM:     INC HL
           INC DE
           LD A,(HL)
           INC A
           JR NZ,PRHDLP

           LD H,D            ;IF DESIRED NAME=NULL (STARTS 0FFH) THEN
           LD L,E            ;ENSURE 'MATCH' OF LOADED NAME BY COMP VS. SELF!

PRHDLP:    LD A,(DE)
           XOR (HL)
           AND &DF           ;IGNORE MISMATCH ON BIT 5 (LC/UC)
           JR Z,PRHDC        ;JR IF MATCH BETWEEN LOADED NAME CHAR AND DESIRED

           INC C             ;NZ C REG SHOWS A MISMATCH

PRHDC:     LD A,(HDL+HFG)
           RRA
           LD A,(DE)         ;LOADED NAME CHAR
           CALL NC,&0010     ;PRINT UNLESS TURNED OFF
           INC HL
           INC DE
           DJNZ PRHDLP

           LD A,C
           AND A
           JR NZ,LKHDR       ;JR IF ANY MISMATCH OCCURRED - LOOK FOR ANOTHER

           LD A,(HDL+HFG)
           RRA
           LD A,&0D
           RET C

           RST &10           ;CR AFTER LAST (MATCHED) FILE NAME
           RET

;LOAD A HEADER

LDHDR:     LD HL,HDL
           LD DE,80
           XOR A
           LD C,A            ;CDE=80 BUT DE WILL CHANGE TO 17 IF ZX HDR FOUND
           INC A             ;'HEADER'
           SCF               ;'LOAD'
           CALL LDBYTES
           RET NC            ;RET IF LOAD FAILED

           LD HL,HDL
           LD A,(HL)         ;TYPE 0-3 IF ZX HEADER, 16-20 IF SAM
           BIT 4,A
           RET NZ            ;RET IF A SAM HEADER. CY STILL SHOWS 'OK'

           SUB 3
           LD A,0
           LD (HDL+HFG),A    ;ENSURE VISIBLE, UNPROTECTED
           RET NZ            ;RET IF OTHER THAN ZX CODE HDR (CY)

;TRANSFORM 17-BYTE ZX CODE HEADER INTO SAM HEADER

           LD (HL),19        ;TYPE=SAM CODE
           EX DE,HL
           XOR A
           LD HL,(HDL+11)    ;ZX FILE LEN
           CALL PAGEFORM
           LD (HDL+HDN+3),A
           LD (HDL+HDN+4),HL ;SAM LENGTH FORMAT
           LD C,&20          ;NORMAL 'SCREEN MODE' FOR SAM CODE FILE **
           LD HL,(HDL+13)    ;ZX START ADDR
           LD A,H            ;DEST ADDR MSB
           CP &40
           JR C,HDTR5

           CP &5B
           JR NC,HDTR5

;MUST BE LOADING TO SCREEN

           LD A,20
           LD (DE),A         ;FILE TYPE=SCREEN$
           LD C,0            ;MODE 0

HDTR5:     LD A,C
           LD (HDL+16),A     ;SET SCREEN MODE OR 'NO MODE'
           XOR A
           LD (HDL+HFG),A    ;NO PROTECT OR INVIS
           CALL PAGEFORM
           DEC A
           LD (HDL+HDN),A
           LD (HDL+HDN+1),HL ;SAM START ADDR FORMAT (IRREL IF SCREEN$)
           LD A,&FF
           LD (HDL+HDN+6),A  ;NO EXEC ADDR
           SCF               ;'OK'
           RET

SAMAIN:    LD HL,HDR+HDN
           LD A,(HDR)
           CP 19
           LD A,0
           JR NZ,SAPA2       ;JR IF NOT CODE

           IN A,(250)        ;ADJUST START PAGE

SAPA2:     ADD A,(HL)
           LD (HL),A
           CALL TORN
           JR Z,SVFL         ;LET ROM HANDLE T SAVE, OR NET SAVE IF NO DOS

SADOS:     LD IX,HDR
           RST &08
           DB SVHK           ;DOS SAVE
           RET

;JUMP FROM DOS (E=2) - SAVE ENTIRE FILE TO TAPE OR NET

SVFL:      LD A,(TPROMPTS)
           RRA
           RRA
           JR C,SAHDDB       ;JR IF NO SAVE PROMPTS WANTED

           CALL TCHK
           LD A,3            ;'START TAPE..' MSG NO.

           CALL Z,WTBRK      ;GIVE MSG IF TEMP DEVICE=T

;SAVE HEADER, AND DATA BLOCK SPECIFIED BY HEADER

SAHDDB:  ;  IN A,(250)
          ; INC A             ;PAGE WITH HDR IN
         ;  CALL TSURPG       ;PAGE IN AT 8000H
           XOR A
           LD C,A
           INC A             ;'HEADER' (A=1)
           LD DE,80          ;CDE=80 (HEADER LEN)
         ;  LD HL,HDR+4000H
           LD HL,HDR
           CALL SABYTES      ;SAVE HEADER

           LD HL,HDR+HDN
           LD A,(HL)
           CALL TSURPG
           INC HL
           LD E,(HL)
           INC HL
           LD D,(HL)         ;DE=ADDR TO START SAVING FROM (SWITCHED IN)
           PUSH DE
           INC HL
           CALL RDTHREE      ;CDE=LEN
           POP HL            ;START
           LD A,&FF          ;MAIN BLOCK
           JP SABYTES


;GET SCREEN LEN
;ENTRY: A=MODE. EXIT: ADE=001B00H IF MODE 0, 003800H IF MODE 1, ELSE 012000H

SCRLEN:    LD DE,&1B00       ;MODE 0 SCRN LEN
           AND A
           RET Z

           LD D,&38          ;MODE 1 SCRN LEN=3800H
           DEC A
           RET Z

           LD A,1            ;A=16K PAGE. MODES 2 AND 3 LEN=4000H+2000H
           LD D,&20
           RET

TORN:      CALL TCHK
           RET Z

           CP "N"
           RET NZ

           LD A,(DOSFLG)
           AND A
           RET

TCHK:      LD A,(SLDEV)
           CP "T"
           RET

;CALLED BY VIDSEL AND TAPEMN

CUS2:      AND &7F           ;MIDI OUT BIT INACTIVE
           LD D,A
           IN A,(VIDPORT)
           AND &7F
           CP D
           RET
