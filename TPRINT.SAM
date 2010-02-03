;TPRINT.SAM - PRINT ASCII AND CR.

PROM1:     CP &20
           JP C,PRCRLCDS

           CP &80
           JP NC,PRGR80

PRASCII:   LD DE,(CHARS)
           LD HL,FLAGS
           SET 0,(HL)        ;'SPACE WAS LAST CHAR'
           CP &20
           JR Z,PRINTMN1

           RES 0,(HL)        ;'LAST CHAR WAS NOT SPACE'

PRINTMN1:  LD L,A
           LD H,0
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,DE         ;HL PTS TO CHAR DATA

IOPENT:    LD (OPCHAR),A     ;USED BY LPRINT
           LD B,A

;FROM POUDGS, AND AFTER CR

NLENTRY:   CALL POFETCH      ;GET DE=COL/ROW, A=RHS LIMIT
           CP E              ;CURRENT COL
           JR NC,PRNONWLN    ;JR IF COL=LESS THAN OR EQUAL TO RHS LIMIT

           DEC E
           SUB E             ;Z IF JUST 1 PAST RHS LIMIT - LINE FULL; ELSE CR
           LD C,A            ;WAS USED JUST BEFORE
           PUSH BC
           LD E,&FF          ;COL FF SHOWS 'RECURSIVE' CR
           LD A,(DEVICE)
           ADD A,&FE         ;CY IF PRINTER
           PUSH HL
           CALL PRENTER
           POP HL            ;GET CHAR PATTERN BACK
           POP BC            ;B=CHAR CODE
           LD A,(INDOPFG)
           AND A
           JR Z,NLENTRY      ;JR IF NO INDENT O/P WANTED - NOT LISTING.

           LD A,C
           AND A
           JR NZ,NLENTRY

           PUSH BC
           CALL INDOPEN        ;O/P SPACES IF POSN WAS JUST 1 PAST RHS LIMIT -
           POP AF              ;LINE WAS FULL. ELSE CR WAS USED.
           RST &10
           RET

PRNONWLN:  PUSH DE           ;CURRENT PRINT POSN
           LD A,(OVERT)      ;0-3
           CP 1              ;0->CY
           SBC A,A           ;0->FF
           CPL               ;0->00, 1,2,3->FF
           LD B,A
           LD A,(INVERT)     ;0=INVERSE 0, FF=INVERSE 1
           LD C,A
           LD IX,(PATOUT)    ;USUALLY=ENDOUTP
           CALL IXJUMP
           POP HL            ;POSN
           INC L             ;MOVE RIGHT

POSTORE:   LD A,(DEVICE)     ;0=UPPER SCREEN, 1=LOWER, 2=PRINTER OR OTHER
           AND A
           JR Z,POSUSCRN

           DEC A
           JR Z,POSLSCRN

           LD (PRPOSN),HL    ;L=PRINTER COL
           RET

POSLSCRN:  LD (SPOSNL),HL
           RET

POSUSCRN:  LD (SPOSNU),HL
           RET

ENDOUTP:   LD A,(DMPFG)
           AND A
           RET NZ

           LD A,(DEVICE)
           CP 2
           JR Z,ENDOP2       ;JR IF PRINTER

           LD A,(CSIZE)
           CP 16
           JP C,EPSUB        ;JR IF NOT DOUBLE HEIGHT

           CALL DBCHAR       ;EXITS WITH HL PTING TO DOUBLE HEIGHT CHAR MATRIX
           PUSH BC
           PUSH DE
           CALL EPSUB        ;PRINT TOP HALF
           LD A,8
           LD (DHADJ),A      ;ADJUST ADDRESSES TO PRINT 8 SCANS LOWER
           POP DE
           POP BC
           LD HL,MEMVAL+16   ;PTR TO BOTTOM HALF
           CALL EPSUB
           XOR A
           LD (DHADJ),A
           RET

ENDOP2:    LD A,(OPCHAR)
           JP CHBOP          ;SEND CHAR ON 'B' CHANNEL

;*******************************************************************************

PRGR80:    LD C,A
           LD A,(INQUFG)     ;BIT 0=1 IF IN QUOTES
           RRCA
           LD H,A
           LD A,(FLAGX)      ;BIT 7=1 IF INPUT LINE
           OR H
           RLA
           JR C,POUDGH       ;JR IF IN QUOTES, OR INPUT LINE - PRINT UDGS,
                             ;NOT TOKENS
           LD A,C
           CP &85

POUDGH:    JP C,POUDG        ;80-84H ARE ALWAYS UDGS

;USED BY CHANNEL 'R'

PRGR802:   LD HL,(PRTOKV)
           CALL JPOPT

           INC A
           JP Z,POFN         ;JP IF FF FN LEADER

           SUB &86           ;(PLUS 1 TO COMPENSATE FOR INC)
           LD DE,KWDS85      ;SPLIT CMDS LIST INTO 4 SUB-LISTS FOR SPEED
           CP &1B
           JR C,POBTL        ;85-9FH

           SUB &1B
           LD H,&20
           LD DE,KWDSA0      ;A0-BF
           CP H
           JR C,POBTL

           SUB H
           LD DE,KWDSC0      ;C0-DF
           CP H
           JR C,POBTL

           SUB H
           LD DE,KWDSE0      ;E0-FE
           JR POBTL

;*******************************************************************************
;ROUTINES FOR PRINTING ERROR MSGS, CMDS, FNS, MSGS

POFN:      LD DE,POSTFF
           JP SVSETOP        ;SAVE CURRENT O/P ADDR AND SET O/P ADDR TO DE

PSTFF2:    CALL RESTOP       ;RESTORE O/P ADDR. A= CHAR PRINTED AFTER FF
           SUB PITOK
           CP SINTOK-PITOK
           JR C,POIMFN       ;JR IF IMMEDIATE FN

           SUB MODTOK-PITOK
           JR C,POFPCFN      ;JR IF FPC FN

           LD DE,BINFNTL     ;BINARY FN TOKEN LIST
           CP ANDTOK-MODTOK+1
           JR C,POBTL        ;MOD-AND (BIN OPS) HAVE LEADING AND TRAILING SPACE

           JR POMSP2         ;<>, >=, <= HAVE NO SPACES


POIMFN:    LD DE,IMFNTL      ;IMMEDIATE FN TOKEN LIST
           CP FNTOK-PITOK
           JR Z,POTRNL

           CP BINTOK-PITOK
           JR Z,POTRNL       ;FN AND BIN HAVE TRAILING BUT NO LEADING SPACES

           JR POMSP2         ;REST HAVE NEITHER

POFPCFN:   ADD A,MODTOK-SINTOK
           LD DE,FPCFNTL     ;FPC FN TOKEN LIST

POTRNL:    AND A
           PUSH AF           ;TRAILING SPACE
           SCF               ;NO LEADING SPACE
           JR POGEN1

POMSPX:    SCF
           PUSH AF
           PUSH AF
           LD HL,MSGBUFF+11
           CALL POMSR2
           JR POGEN2

POMSP2:    SCF               ;NO LEADING OR TRAILING SPACES
           DB &26            ;"JR+1". FOR MSGS OR E.G. PI/ITEM/POINT

POBTL:     AND A             ;BOTH TRAILING AND LEADING SPACES

POGEN:     PUSH AF           ;TRAILING STATUS

POGEN1:    PUSH AF           ;LEADING  STATUS
           CALL POMSR        ;GET MSG TO BUFFER

POGEN2:    POP AF            ;LEADING STATUS
           JR C,POMSG3       ;NO LEADING SPACE FOR MSG OR FNS (EXCEPT AND/OR)

           LD A,(FLAGS)
           RRA
           LD A," "
           CALL NC,&0010     ;PRINT LEADING SPACE IF PREVIOUS CHAR WASN'T ONE

POMSG3:    CALL PRINTSTR     ;OUTPUT BC FROM (DE)
           POP AF
           RET C             ;RET IF NO TRAILING SPACE WANTED

           DEC DE
           LD A,(DE)         ;LAST CHAR
           CP "A"
           JR NC,POMSG4      ;JR IF LETTER - DO TRAILING SPACE

           CP "$"
           RET NZ            ;'$' IS ALSO FOLLOWED BY A SPACE, BUT NOT =/>/#

POMSG4:    LD A," "
           RST &10
           RET

;A=MSG NUMBER 00->, DE=START OF MSG LIST

POMSR:     LD HL,MSGBUFF

POMSR2:    PUSH HL           ;BUFFER START
           CALL POMSR3
           POP DE            ;BUFFER ST
           AND A
           SBC HL,DE
           LD B,H
           LD C,L            ;LEN
           RET

POMSR3:    LD B,A
           INC B
           JR POMSR4

LKHIBTLP:  LD A,(DE)         ;WITH ABOUT 4 CHARS/TOKEN, TAKES ABOUT 0.7 MS
           INC DE            ;(ON SAM) TO FIND 30TH WORD.
           RLA
           JR NC,LKHIBTLP

POMSR4:    DJNZ LKHIBTLP

MVWORDLP:  LD A,(DE)
           AND &7F
           CP &20
           JR NC,MVWORD2     ;JR WITH ALL EXCEPT COMPRESSION CODES 00-1F

           PUSH DE           ;SAVE MAIN MSG PTR
           LD DE,COMPLIST    ;LIST OF COMPRESSION CODED WORDS
           CALL POMSR3       ;RECURSIVE CALL
           POP DE            ;MAIN MSG PTR
           DB &01            ;'JR+2'

MVWORD2:   LD (HL),A
           INC HL

MVWORD3:   LD A,(DE)
           INC DE
           RLA
           JR NC,MVWORDLP    ;MAIN MSG CAN HAVE COMP CODES AND ASCII. BIT 7 IS
                             ;HI IN LAST CHAR. COMPRESSED WORD CANNOT HAVE COMP
                             ;CODES.
           RET


;*******************************************************************************

POUDG:     LD A,(DEVICE)
           CP 2
           JR NZ,PUDGS       ;JR IF NOT PRINTER

           LD A,C
           LD (OPCHAR),A     ;USED BY LPRINT
           LD HL,(LPRTV)
           CALL JPOPT

PUDGS:     LD A,(BGFLG)      ;BLOCK GRAPHICS FLAG
           AND A
           LD A,C
           JR NZ,POFUDG      ;JR IF FOREIGN SET/UDGS WANTED, NOT BLOCKS

           CP &90
           JR NC,POFUDG      ;>=90H ARE FOREIGN/UDGS ANYWAY

           CALL QUADBITS     ;GET LOWER 4 BITS OF A INTO DE, QUADRUPLED
           LD HL,MEMVAL
           PUSH HL
           LD B,4

BKGRL1:    LD (HL),E
           INC HL
           DJNZ BKGRL1

           LD B,4

BKGRL2:    LD (HL),D
           INC HL
           DJNZ BKGRL2

           POP HL
           LD B,C
           JP NLENTRY

POFUDG:    LD HL,(HUDG)      ;HI UDG FOR CHARS. >A8H
           SUB &A9
           JR NC,POUDG1      ;JR IF HI UDG

           LD A,C
           LD DE,&FB80       ;COMP. FOR A BEING 80H-A8H AND UDG VAR PTING
           LD HL,(UDG)       ;TO CHR$ 144
           ADD HL,DE

POUDG1:    EX DE,HL
           JP PRINTMN1

;PRINT CONTROL CODES
;*******************************************************************************
;ENTRY: JR FROM MAIN PRINT ROUTINE. A=00-1FH

PRCRLCDS:  CP 24
           JR NC,PRQUERY

           CP 6
           JR C,PRQUERY

           LD E,A
           LD D,0
           LD HL,CCPTB-6       ;CONTROL CODE PTR TABLE
           ADD HL,DE
           LD E,(HL)
           ADD HL,DE
           LD C,A
           CALL POFETCH      ;D=ROW, E=COL, A=RHS MAX, CY IF LPRINTING
           JP (HL)

CCPTB:

;           DB PRQUERY-CCPT      ;0
 ;          DB PRQUERY-CCPT-1    ;1
  ;         DB PRQUERY-CCPT-2    ;2
   ;        DB PRQUERY-CCPT-3    ;3  CLS?
    ;       DB PRQUERY-CCPT-4    ;4
     ;      DB PRQUERY-CCPT-5    ;5
           DB PRCOMMA-CCPTB      ;6  PRINT COMMA
           DB PRQUERY-CCPTB-1    ;7  (EDIT)
           DB CURLF-CCPTB-2      ;8  CURSOR LEFT
           DB CURRT-CCPTB-3      ;9  CURSOR RIGHT
           DB CURDN-CCPTB-4      ;10 CURSOR DOWN
           DB CURUP-CCPTB-5      ;11 CURSOR UP
           DB PRDELL-CCPTB-6     ;12 DELETE LEFT
           DB PRENTER-CCPTB-7    ;13 ENTER
           DB PRDELR-CCPTB-8     ;14 DELETE RIGHT
           DB PRQUERY-CCPTB-9    ;15
           DB CC1OP-CCPTB-10     ;16 INK
           DB CC1OP-CCPTB-11     ;17 PAPER
           DB CC1OP-CCPTB-12     ;18 FLASH
           DB CC1OP-CCPTB-13     ;19 BRIGHT
           DB CC1OP-CCPTB-14     ;20 INVERSE
           DB CC1OP-CCPTB-15     ;21 OVER
           DB CC2OPS-CCPTB-16    ;22 AT
           DB CC2OPS-CCPTB-17    ;23 TAB

;*******************************************************************************
;D=LINE, E=COL

PRCOMMA:   LD A,E            ;COL
           LD HL,WINDLHS
           SUB (HL)          ;A=DIST FROM WIND LHS
           PUSH AF
           LD A,(TABVAR)
           AND A
           LD BC,&F010       ;FOR 16-COLUMN TAB
           JR Z,PCOM2

           LD BC,&F808       ;FOR 8-COLUMN TAB

PCOM2:     LD A,(WINDRHS)
           CP E
           JR C,PC25         ;JR IF LINE FULL

           POP AF
           AND B             ;GET DIST DIV 16 OR DIV 8
           ADD A,C           ;ADD 16 OR 8
           ADD A,(HL)        ;ABS DESIRED COLUMN
           DEC HL            ;PT TO RHS
           CP (HL)
           JR C,PCOM3        ;JR IF NEW POSN WOULD NOT REACH RHS MAX

           LD A,(HL)
           INC A

PCOM3:     SUB E

OPAORZ:    RET Z

OPASPACES: LD B,A

OPSPLP:    LD A," "
           RST &10
           DJNZ OPSPLP

           RET

PC25:      POP AF
           LD B,C
           JR OPSPLP


PRQUERY:   LD A,"?"
           RST &10
           RET

;*******************************************************************************
;CURSOR LEFT/UP/RIGHT

CURLF:     LD A,(WINDLHS)
           CP E
           LD A,(WINDRHS)
           JR Z,CURLF2       ;JR IF AT LHS OF WINDOW

           DEC E             ;DEC COL.
           CP E
           JR NC,POSTOREH    ;JR IF NOT PAST RHS ('LINE FULL')

           LD E,A            ;ELSE COL=RHS

POSTOREH:  EX DE,HL
           JP POSTORE


CURLF2:    LD E,A            ;COL=RHS
           LD A,(DEVICE)
           ADD A,&FE         ;CY IF PRINTER (DEVICE 2)

CURUP:     JR C,PRQUERY      ;PRINT '?' IF LPRINTING - POSN UNCHANGED

           LD A,(WINDTOP)
           CP D
           RET Z             ;RET IF WE ARE AT TOP OF WINDOW

           DEC D             ;UP A ROW. C=RHS+1
           JR POSTOREH


CURRT:     LD HL,(OVERT)     ;OVERT/INVERT
           PUSH HL
           LD A,(M23PAPT)
           PUSH AF
           LD HL,&0001       ;OVER 1;INVERSE 0
           LD B,1
           JR SPOX

;DELETE LEFT

PRDELL:    JR C,PRQUERY      ;JR IF LPRINTING

           LD A,8
           RST &10           ;BACKSPACE
           CALL SPO0         ;PRINT OVER 0;' ';
           LD A,8            ;BACKSPACE
           RST &10
           RET

;ERASE OLD CURSOR, GET H=TABLE MSB, GET C=LNPTR

EROC2:     LD A,(WINDTOP)    ;ALLOW FOR WINDOW
           LD C,A
           LD A,(LNPTR)
           SUB C
           JR NC,EROC3

           XOR A

EROC3:     LD C,A
           LD E,5
           CALL ATSR2        ;PRINT AT A,5;
           CALL SPO0         ;ERASE OLD CURSOR
           LD A,C
           LD H,LPT/256
           RET

;DELETE RIGHT=SPACE (OVER 0)

PRDELR:

;PRINT OVER 0;INVERSE 0;" "; (IN PERMANENT PAPER IF MODES 2 AND 3)

SPO0:      LD B,1            ;1 SPACE

;O/P B SPACES (OVER 0;INVERSE 0)

OPBSP:     LD HL,(OVERT)     ;OVERT/INVERT
           PUSH HL
           LD A,(M23PAPT)
           PUSH AF
           LD A,(M23PAPP)
           LD (M23PAPT),A    ;PERM PAPER
           LD HL,&0000       ;OVER 0, INVERSE 0

SPOX:      LD (OVERT),HL

OPSL:      LD A,&20
           RST &10           ;PRINT SPACE
           DJNZ OPSL

           POP AF
           LD (M23PAPT),A
           POP HL
           LD (OVERT),HL
           RET

CURDN:     PUSH DE           ;SAVE COL.
           LD E,&FE          ;FORCE ACTUAL MOVE DOWN
           CALL PRENTER      ;EXIT WITH HL=POSN
           POP DE
           LD L,E            ;UNCHANGED COL.
           JR PRENT5

PRENTER:   JR C,LPRENT       ;JR IF LPRINT

           LD A,E            ;COL
           CP &FE
           JR C,PRENT3       ;IF PREVIOUS CHAR WAS NOT CR, JUST SET COL 0FEH.
                             ;('LINE FULL') ELSE DROP DOWN A LINE
           LD A,(WINDBOT)
           CP D
           JR NZ,PRENT2      ;JR IF NOT ON BOTTOM LINE ALREADY

           PUSH DE
           CALL SCRLSCR      ;SCROLL WINDOW UP
           POP DE
           DEC D             ;LINE VALUE WILL END AS BOTTOM LINE STILL

PRENT2:    INC D             ;DOWN A LINE
           INC E             ;Z IF COL WAS FF (RECURSIVE)
           LD A,(WINDLHS)
           JR Z,PRENT4       ;JR IF WAS RECURSIVE CR FROM 'LINE FULL' - LHS COL
                             ;FOR CHAR COMING NEXT

PRENT3:    LD A,&FE          ;'LINE FULL, NOT RECURSIVE'

PRENT4:    LD E,A
           EX DE,HL

PRENT5:    JP POSTORE

LPRENT:    LD A,&0D
           CALL CHBOP        ;O/P CR ON CHANNEL 'B'
           XOR A
           LD (PRPOSN),A
           LD A,(AFTERCR)
           AND A
           RET Z
                             ;ELSE LPRINT LF IF (AFTERCR)= 0AH
CHBOP:     LD HL,(CHANS)
           LD DE,25
           ADD HL,DE         ;PT TO CHANNEL B
           JP HLJPI
;           LD E,(HL)
 ;          INC HL
  ;         LD D,(HL)
   ;        EX DE,HL          ;HL=O/P ADDR (USUALLY 'SENDA')
    ;       JP (HL)

;CONTROL CODES WITH OPERANDS
;ONE OPERAND - INK, PAPER, OVER, INVERSE, BRIGHT, FLASH

CC1OP:     LD DE,CCRESTOP
           JR SETADCOM

;TWO OPERANDS - TAB OR 'AT' (TAB JUNKS THE SECOND OPERAND!)

CC2OPS:    LD DE,PRERESTOP

SETADCOM:  CALL SVSETOP
           LD A,C            ;CONTROL CODE
           LD (TVDATA),A
           RET

;SAVE CURRENT O/P ADDR AND SET IT TO DE

SVSETOP:   PUSH DE
           CALL SVCUROP
           POP DE
           JP POCHNG

CCRP2:     LD HL,(TVDATA)    ;H=1ST OPERAND (IF 2),L=CONTROL CODE

           LD D,A            ;LAST OPERAND (ONLY OPERAND IF 1, ELSE 2ND)
           LD A,L            ;CONTROL CODE
           CP &16
           JP C,PRCOITEM

           JR Z,POATCC       ;JR IF 'AT'

;DEAL WITH TAB. H=ONLY RELEVANT OPERAND (FIRST - COLUMN)

           CALL POFETCH      ;D=ROW, E=COL, A=RHS MAX, CY IF LPRINTING
           LD C,A
           CP E
           LD A,H            ;PARAM
           JR C,TAB2         ;JR IF LINE FULL

           LD A,(WINDLHS)
           ADD A,H           ;ABS TAB POSN
           SUB E             ;FIND SPACES REQUIRED TO REACH TAB POSN.
           RET Z

           JR NC,TAB2        ;JR IF NOT ALREADY PAST TAB POSN

           LD A,H            ;CALCULATE SPACES NEEDED TO FILL LINE
           ADC A,C           ;AND REACH TAB POSN.
           SUB E

TAB2:      AND A
           JP OPAORZ

;*******************************************************************************
;'AT' ROUTINE

POATCC:    EX DE,HL
           LD E,H            ;D=1ST OPERAND (ROW), E=2ND (COLUMN)
           LD HL,WINDLHS
           LD A,(HL)
           ADD A,E           ;ADD COL
           LD E,A            ;ABS SCREEN COL
           DEC HL
           LD A,(HL)         ;WINDRHS
           CP E
           JR C,PATER        ;ERROR IF TOO FAR RIGHT

           INC HL            ;WINDLHS
           INC HL            ;WINDTOP
           LD A,(HL)
           ADD A,D           ;ABS SCREEN ROW
           LD D,A
           INC HL
           LD A,(HL)         ;WINDBOT
           SUB D
           JR NC,POSTORH     ;JR IF 'AT' NOT OFF BOTTOM

           NEG
           LD B,A            ;NUMBER OF EXTRA ROWS NEEDED IN LS
           LD A,(DEVICE)
           AND A
           JR Z,PATER        ;ERROR IF UPPER SCREEN, BUT LS CAN INCREASE SIZE

           LD D,(HL)
           PUSH DE           ;D=WINDBOT, E=ABS 'AT' COLUMN

SLSLP:     PUSH BC
           CALL SCRLS
           POP BC
           DJNZ SLSLP        ;SCROLL LS UP 'B' TIMES TILL POSN IS ON RIGHT ROW
                             ;(I.E. ADD ROWS TO TOP OF LOWER SCREEN)
           POP DE

POSTORH:   EX DE,HL
           JP POSTORE

PATER:     RST &08
           DB 32             ;'OFF SCREEN'

;SAVE CURRENT O/P ADDR

SVCUROP:   LD HL,(CURCHL)
           LD E,(HL)
           INC HL
           LD D,(HL)
           LD (OPSTORE),DE
           RET

;SCROLL SCREEN

SCRLSCR:   LD A,(TVFLAG)
           AND &10
           JR Z,SCRLSCR2     ;JR IF NOT AUTO-LIST

           LD A,(BCREG)
           DEC A
           JR NZ,DOSCRL      ;JR IF CURRENT LINE NOT PRINTED YET

           CALL SETSTRM      ;(STREAM ZERO)
           LD SP,(LISTSP)    ;CLEAR STACK
           JP AULX           ;JP TO SET 'AUTOLIST FINISHED'

SCRLSCR2:  LD A,(DEVICE)
           DEC A
           JR Z,SCRLS        ;JR IF USING LOWER SCREEN

           CALL BRKCR
           LD A,(SPROMPT)
           AND A
           JR NZ,DOSCRL      ;JR IF PROMPS TURNED OFF

           LD HL,SCRCT
           DEC (HL)
           JR NZ,DOSCRL      ;JR IF NOT TIME FOR PROMPP YET

           CALL SETSCRCT
           CALL SVTEMPS      ;SAVE TEMPORARY ATTRIBUTES ETC
           LD HL,(CURCHL)
           PUSH HL           ;PROBABLY CHANNEL 'S'
           EXX
           PUSH DE
           LD A,1            ;'Scroll?'
           CALL WTBRK
           POP DE
           EXX
           CP " "
           JR Z,BRCH

           AND &DF
           CP "N"

BRCH:      CALL Z,IOPOF      ;INDENT O/P OFF. EXIT WITH Z
           JP Z,BRCERR

           POP HL
           LD (CURCHL),HL
           CALL CHANFLAG
           CALL RSTTEMPS     ;RESTORE TEMPORARY ATTRIBUTES
           CALL COLEX

DOSCRL:    JP EDRS1UP

SETSCRCT:  LD HL,(WINDTOP)
           LD A,H
           SUB L
           INC A             ;A=WINDOW HEIGHT
           LD (SCRCT),A
           RET


;SCROLL LOWER SCREEN. IF THE TOP OF THE LOWER SCREEN HITS UPPER SCREEN PRINT
;POSN THEN THE UPPER SCREEN HAS TO BE SCROLLED AS WELL, FROM TOP TO PRINT POSN

SCRLS:     LD HL,LWTOP
           LD A,(HL)         ;LS TOP WILL MOVE UP BY ONE LINE
           DEC A
           JR Z,PATER        ;OFF SCREEN ERROR IF WINDOW HIT SCREEN TOP

           LD (HL),A         ;NEW LWTOP
           LD (WINDTOP),A    ;ALSO RECORD NEW TOP FOR CURRENT (TEMP) WINDOW (LS)
           LD HL,KPOS+1
           DEC (HL)          ;CURSOR LINE KEPT UP-TO-DATE
           LD HL,SPOSNU+1
           CP (HL)
           JR NZ,DOSCRL      ;JR IF NO CLASH WITH UPPER SCREEN PRINT POSN
                             ;(JR ALSO IF WINDTOP IS ABOVE US PRINT POSN - SIC)
           PUSH AF
           DEC (HL)          ;US PRINT POSN MOVES UP ONE LINE
           CALL STREAMFE     ;MAIN WINDOW
           POP AF
           LD (WINDBOT),A    ;WIND BOT=FORMER PRINT POSN
           CALL EDRS1UP      ;SCROLL MAIN WINDOW UP ONE
           CALL STREAMFD     ;LOWER WINDOW
           JR DOSCRL         ;SCROLL LS

RSTTEMPS:  AND A
           DB &26            ;'JR +1'

SVTEMPS:   SCF
           LD HL,THFATT
           LD DE,TEMPW1      ;8 BYTES OF TEMP STORE AVAILABLE HERE
           JR C,SVRSTTMPS

           EX DE,HL

SVRSTTMPS: JP LDIR8          ;LD BC,8:LDIR:RET


;COPY 8 BYTES FROM HL TO MEM1 AND MEM2, DOUBLING
;EXIT WITH HL PTING TO MEM1

DBCHAR:    PUSH BC
           PUSH DE
           LD DE,MEMVAL+8    ;(MEMVAL+0 MIGHT BE IN USE)
           PUSH DE
           LD B,8

DHLP:      LD A,(HL)
           INC HL
           LD (DE),A
           INC DE
           LD (DE),A
           INC DE
           DJNZ DHLP         ;LOOP DOUBLES CHARACTER BYTES

           POP HL            ;PTR TO TOP HALF OF CHAR PAT
           POP DE
           POP BC
           RET

;USED TO GIVE 'START TAPE..' PROMPT TO LOWER SCREEN, AND
;TO GIVE 'SCOLL?' PROMPT. ON ENTRY, A=UTMSG NUMBER

WTBRK:     CALL WTB2
           CALL UTMSG
           CALL READKEY      ;CLEAR BUFFER
           CALL GTKBK

WTB2:      PUSH AF
           CALL CLSLOWER
           POP AF
           RET

;USED TO PRINT CURSOR

FONOP2:    LD BC,(MNOP)
           PUSH BC
           RET
