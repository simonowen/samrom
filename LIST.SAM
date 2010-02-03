;LIST.SAM   LIST, CLS

AUTOLIST:  CALL CLSLOWER
           LD HL,EPPC
           CALL REALN        ;MAKE SURE EPPC HAS THE NUMBER OF A REAL LINE

;USED BY AUTO

AUL2:      LD HL,SDTOP
           CALL REALN        ;DITTO FOR SDTOP

           CALL CLSUP        ;"S"
           LD A,&10
           LD (TVFLAG),A     ;"AUTOLIST, UPPER SCREEN"
           LD HL,FLAGS2
           SET 0,(HL)        ;"SCREEN IS NOT CLEAR"
           LD HL,(SDTOP)
           LD DE,(EPPC)
           LD (LAST),DE      ;LAST LINE THAT *MUST* BE LISTED IS EPPC
           AND A
           SBC HL,DE
           EX DE,HL          ;HL=EPPC
           JR NC,AUL4        ;JR IF EPPC IS ABOVE OR EQU TO TOP LINE IN
                             ;AN AUTOLIST - ALTER SDTOP
           CALL FNDLINE      ;ELSE ENSURE THAT SDTOP IS NOT TOO FAR ABOVE EPPC
           IN A,(URPORT)
           PUSH AF
           PUSH HL           ;HL SHOULD BE ABOUT 0100 HIGHER THAN ADDR FOR SDTOP
           LD HL,(SDTOP)     ;SO THAT IT APPEARS WITHOUT SCROLLING
           CALL FNDLINE
           POP DE            ;EPPC ADDR
           DEC C
           DEC D             ;ADDR IS 512 BEFORE EPPC IN LISTING
           POP AF            ;EPPC PAGE

AULLP:     LD C,URPORT
           IN B,(C)          ;B=SDTOP PAGE
           CP B
           JR NZ,AUL25       ;JR IF SDTOP AND EPPC PAGES DON"T MATCH

           SBC HL,DE         ;SBC SDTOP ADDR, EPPC-0200H
           ADD HL,DE         ;HL=SDTOP ADDR AGAIN
           JR NC,AUL3        ;JR IF DIFF <100H

AUL25:     INC HL
           INC HL
           LD C,(HL)
           INC HL
           LD B,(HL)
           ADD HL,BC
           PUSH AF
           CALL CHKHL
           POP AF
           INC HL            ;NEXT POSSIBLE SDTOP ADDR TO CONSIDER
           JR AULLP

AUL3:      LD D,(HL)
           INC HL
           LD E,(HL)
           EX DE,HL          ;NEW SDTOP LINE NO.

AUL4:      LD (SDTOP),HL
           LD (LISTSP),SP    ;SO "SCROLL" CAN ABORT AT END OF SCREEN.
           CALL LIST5        ;LIST LINES FROM LINE HL, INDENTED

AULX:      LD HL,TVFLAG      ;ALWAYS COME BACK HERE, EVEN IF "SCROLL" ABORTED
           RES 4,(HL)
           LD BC,IOPOF
           JP R1XJP          ;ENSURE INDENTED OP IS OFF, TURN ROM1 OFF

LLIST:     LD C,&03
           DB &21            ;"JR+2"

LIST:      LD C,&02
           XOR A
           LD (TVFLAG),A     ;NON-AUTOLIST (BIT 4=0)
           CALL RUNFLG
           LD A,C
           CALL C,SETSTRM
           RST &18
           CP &91            ;FORMATTOK
           JR NZ,LIST1

           CALL SSYNTAX6     ;SKIP, GET NUMBER

           LD DE,&0300+30
           CALL LIMBYTE
           LD (LISTFLG),A    ;0 IF LIST FORMAT 0 - NO INDENT
           RET

LIST1:     CALL PRHSH1       ;POSSIBLE "#"
           RST &18
           CALL COMMASC      ;COMMA OR SEMICOLON
           JR NZ,LIST2

           RST &20           ;SKIP ,/;

LIST2:     CALL BRKLSSL      ;CALL BRACKETLESS SLICER SR - POSSIBLE ST TO FIN
           LD HL,FLAGS       ;CY IF ERROR
           BIT 7,(HL)
           RET Z             ;RET IF NOT RUNNING

           PUSH AF           ;CY SHOWS ERROR  !!
           AND A
           JR NZ,LIST3       ;JR IF NOT 1-NUMBER SLICER
                             ;ELSE TURN EG LIST 10 INTO LIST 10 TO END (BUT
                             ; DELETE KEEPS DELETE 10 AS IS)
           DEC A             ;A=FF
           LD (LAST+1),A     ;LAST LINE=HIGH NUMBER

LIST3:     LD HL,(FIRST)     ;GET FIRST LINE
           LD A,H
           OR L
           JR Z,LIST4        ;IGNORE OUT OF RANGE CAUSED BY LIST 0

           POP AF            ;CY SHOWS IF OUT OF RANGE
           JP C,IOORERR

           PUSH AF

LIST4:     POP AF
           LD (EPPC),HL      ;NEW EPPC=FIRST LISTED LINE

LIST5:     CALL FNDLINE
           LD BC,LSTLNS
           JP IOPCL

LSTLNS:    RST &30
           DW LSTR1-&8000

;CANCEL SPACES

SPACAN:    XOR A
           LD (NXTSPCS),A
           LD (NXTHSPCS),A
           RET

MCLS:      XOR A
           JR CLSBL

CLS:       CP "#"            ;28 BYTES FOR CLS #
           JR NZ,CLSNH

           RST &30
           DW CLSHS-&8000

CLSNH:     CALL SYNTAX3

           CALL GETBYTE      ;0 IF CLEAR ENTIRE SCREEN
           LD E,6            ;BLITZ CODE FOR "CLS"
           CALL GRAREC
           LD A,C            ;PARAM OR 0

;BLITZ ENTRY

CLSBL:     CP 1
           JR Z,CLU1

           CALL CLU1

CLSLOWER:  LD HL,LWBOT
           LD A,(HL)
           DEC HL
           SUB (HL)          ;SUB LWTOP
           DEC A
           JR Z,CLSL2        ;JR IF LW IS ONLY 2 LINES HIGH

                             ;DEFINE A WINDOW COVERING LW OVERLAP ONTO UPPER
                             ;WINDOW (ABOVE NORMAL 2-LINE SIZE)
           LD A,(HL)         ;LWTOP
           LD (WINDTOP),A
           INC HL
           LD A,(HL)         ;LWBOT
           DEC A
           DEC HL
           LD (HL),A         ;LWTOP IS 1 LESS THAN LWBOT
           DEC A
           LD (WINDBOT),A
           LD HL,(LWRHS)
           LD (WINDRHS),HL   ;RHS AND LHS
           LD HL,(M23PAPP)
           LD (M23PAPT),HL
           LD A,(ATTRP)
           LD (ATTRT),A      ;ENSURE UW COLOURS
           CALL CLSWIND      ;TEMP OVERLAP WINDOW CLEARED

CLSL2:     CALL STREAMFD     ;"K"
           LD A,1
           LD (TVFLAG),A     ;LS, DO NOT CLEAR ON KEYSTROKE, NOT AUTOLIST
           CALL CLWC         ;CLEAR WINDOW AND RESET CHANNEL
           INC H
           LD (SPOSNL),HL    ;POSN=LHS, TOP+1
           RET


CLSUP:     LD A,1            ;"CLEAR WINDOW, NOT ENTIRE SCREEN"

CLU1:      PUSH AF

           DB CALC           ;GRAPHICS COORDS 0,0
           DB STKZERO
           DB STKZERO
           DB EXIT

           CALL SETESP       ;ENSURE NO ERROR ON CLS EVEN IF XOS ETC ODD
           CALL GTFCOORDS    ;GET PHYSICAL COORDS FROM XOS ETC AND 0,0
           JR C,CLU2         ;JR IF THINPIX - HL=X, B=Y

           LD L,C
           LD H,0            ;HL=X NOW

CLU2:      LD (XCOORD),HL
           LD A,B
           LD (YCOORD),A
           POP HL
           LD (ERRSP),HL
           XOR A
           LD (XPTR+1),A     ;RESET ANY "?" ERROR PTR
           LD (ERRNR),A      ;"OK"
           INC A
           LD (SCRCT),A
           CALL STREAMFE     ;"S"
           POP AF
           AND A
           JR NZ,CLS2

           CALL CLSE         ;CLEAR ENTIRE SCREEN IF CLS OR CLS 0
           CALL CLWC2        ;RESET CHANNEL
           CP A              ;Z

CLS2:      CALL NZ,CLWC      ;CLEAR WINDOW, RESET CHANNEL
           LD (SPOSNU),HL
           LD HL,FLAGS2
           RES 0,(HL)        ;"SCREEN IS CLEAR"

;ZERO TABLE ENTRIES

           LD HL,LPT
           LD B,30
           XOR A

ZTEL:      LD (HL),A
           INC HL
           DJNZ ZTEL

           DEC A
           LD (LNPTR),A      ;FF SHOWS NO CURSOR LINE
           RET

CLWC:      CALL CLSWIND

CLWC2:     LD DE,(CURCHL)
           LD HL,MNOP
           LD BC,4
           LDIR              ;REFRESH CHANNEL O/P AND I/P ADDRESSES IN CASE
                             ;THEY HAVE BEEN ALTERED E.G. FOR COLOUR I/P
           LD HL,(WINDLHS)   ;GET TOP LEFT OF WINDOW (FOR PRINT POSN)
           RET

;CLEAR ENTIRE SCREEN - QUICKLY

CLSE:      LD A,(MODE)
           CP 2
           JR NC,CLS1

           LD H,&98          ;END OF MODE 0/1 PATTERN DATA
           LD E,0            ;CLEAR WITH ZEROS
           LD BC,&8002       ;DO 1800H BYTES
           PUSH AF
           CALL CLSG         ;CLEAR M0/M1 PATTERN DATA
           POP AF            ;MODE
           LD DE,(ATTRP)
           LD H,&B8          ;END OF MODE 1 ATTRS
           LD BC,&8002       ;DO 1800H BYTES
           AND A
           JR NZ,CLSG        ;JR IF MODE 1

           LD H,&9B          ;END OF MODE 0 ATTRS
           LD BC,&3001       ;DO 0300H BYTES
           JR CLSG

CLS1:      LD H,&E0
           LD DE,(M23PAPP)
           LD BC,&0006       ;CLEAR DFFF-8000H WITH M3PAPP

CLSG:      CALL SPSSR        ;STORE PAGE, SELECT SCREEN
           LD D,E
           DI
           LD (TEMPW1),SP
           LD L,0
           LD SP,HL

CLSLP:     PUSH DE
           PUSH DE
           PUSH DE
           PUSH DE
           PUSH DE
           PUSH DE
           PUSH DE
           PUSH DE           ;DO 16 BYTES AT A TIME AT ABOUT 7 Ts/BYTE
           DJNZ CLSLP

           DEC C
           JR NZ,CLSLP       ;DO 1000H BYTES

           LD SP,(TEMPW1)
           EI
           JP RCURPR         ;RESET CURRENT UR PAGE


;BASIC"S PRINT COMMAND

LPRINT:    LD C,3
           DB &21

PRINT:     LD C,2
           CALL RUNFLG
           LD A,C
           LD HL,INQUFG
           SET 0,(HL)        ;"IN QUOTES" - MEANS KEYWORDS NOT EXPANDED
           CALL C,SETSTRM
           CALL TEMPS

PRINT2:    RST &18
           CALL PRTERM
           JR Z,PRINT3       ;JR IF E.G. PRINT :

MPRSEPLP:  CALL PRSEPR
           RET Z             ;RET IF TERMINATOR FOUND

           JR NC,MPRSEPLP    ;LOOP UNTIL A NON-SEPARATOR IS FOUND

           CALL PRITEM
           CALL PRSEPR
           RET Z
           JR NC,MPRSEPLP

PRINT3:    CP ")"
           RET Z             ;AVOID CR AFTER EMBEDDED PRINT ITEMS IN INPUT
                             ;E.G. INPUT "OLD VALUE:";(X);" NEW:";X

;ENTRY FROM INPUT. CR IF RUNNING

RUNCR:     LD C,&0D

;PRINT C IF RUNNING

PRCIFRN:   CALL RUNFLG
           RET NC

           LD A,C
           RST &10
           RET

;PRINT SEPARATORS - CONSIDER ;/,/"

;EXIT: Z IF SEPARATOR FOLLOWED BY TERMINATOR, NZ, CY IF NOT SEPARATOR, NZ, NC
;IF SEPARATOR NOT FOLLOWED BY TERMINATOR.

PRSEPR:    RST &18
           CP ";"
           JR Z,PRSEPR3

           LD C,6
           CP ","
           JR Z,PRSEPR2

           CP "'"
           SCF
           RET NZ

           LD C,&0D

PRSEPR2:   CALL PRCIFRN      ;PRINT C IF RUNNING - CHR$ 6 FOR COMMA, CR FOR """

PRSEPR3:   RST &20           ;SKIP SEPARATOR

PRTERM:    CP ")"
           RET Z

           CP ":"
           RET Z

           CP &0D
           SCF
           CCF               ;NC WITHOUT ALTERING ZERO FLAG
           RET


PRITEM:    RST &18
           CP TABTOK
           JR NZ,PRITEM2

           CALL SSYNTAX6

           CALL GETINT
           LD D,C
           LD A,&17
           JR ATSR4

PRITEM2:   CP ATTOK
           JR NZ,PRITEM4

           CALL SSYNTAX8

           CALL GETBYTE
           PUSH AF           ;COL
           CALL GETBYTE      ;C=ROW
           LD D,C
           POP AF
           LD E,A            ;D=ROW, E=COL
           DB &FE            ;"JR+1"

;PRINT AT LINE A, COL E

ATSR2:     LD D,A

ATSR3:     LD A,&16          ;"AT" CONTROL CODE

ATSR4:     RST &10
           LD A,D
           RST &10
           LD A,E
           RST &10
           RET

PRITEM4:   CALL CITEMSR
           RET NC

           CP "#"
           JP Z,PRHSH2

           CALL EXPTEXPR
           RET NC            ;RET IF NOT RUNNING

           IN A,(URPORT)
           PUSH AF
           JR NZ,PRITEM5     ;JR IF NUMERIC EXPR.

           CALL GETSTRING    ;UNSTACK STRING, SEL PAGE
           CP A              ;Z

PRITEM5:   CALL NZ,JPFSTRS   ;GET STR$ OF NUMBER AS BC BYTES AT (DE)

           CALL PRINTSTR
           POP AF
           OUT (URPORT),A
           RET



;LPT = LINE PTR TABLE
;TABLE OF 30 BYTES, EACH 0 IF NO LINE NUMBER ON THAT LINE, OR FF IF THERE IS.
;LNPTR VAR. HOLDS NO OF LINE WITH CURSOR, OR >3FH IF NONE

FUPDN:     PUSH AF           ;DIRECTION CODE - 0BH=UP, 09H=DOWN
           CALL STREAMFE     ;"S" CHANNEL
           POP BC
           CALL FUPDN2
           JP STRM0

FUPDN2:    LD A,(LNPTR)
           CP &40
           JP NC,AUTOLIST    ;JP IF THERE IS NO CURSOR ON-SCREEN

           LD A,B
           CP &0B            ;UP
           JR NZ,LPD         ;JR IF DOWN

;CURSOR UP

           LD HL,(EPPC)
           PUSH HL
           CALL FNDLINE
           LD H,D
           LD L,E            ;HL AND DE PT TO LINE *BEFORE* EPPC. DE USED LATER
           LD B,(HL)
           INC HL
           LD C,(HL)         ;BC=PREV LINE NO
           LD (EPPC),BC
           POP HL            ;OLD EPPC
           AND A
           SBC HL,BC
           RET Z             ;RET IF AT TOP OF PROG - EPPC UNCHANGED

           PUSH DE
           RST &30
           DW EROC2          ;GET A/C=LNPTR, H=TABLE MSB, ERASE OLD CURSOR
           POP DE

LPUL:      AND A
           JR Z,MWDN         ;JR IF AT WINDOW TOP ALREADY - MOVE WINDOW DOWN

           DEC A
           ADD A,LPT\256     ;ADD OFFSET
           LD L,A            ;HL=ADDR IN TABLE (MUST BE INSIDE A PAGE)
           SUB LPT\256
           LD B,(HL)
           INC B
           DJNZ LPC          ;JR IF FOUND A LINE WITH A LINE NUMBER

           JR LPUL           ;ELSE LOOP

MWDN:      PUSH DE           ;START OF NEW EPPC

;GET SCREEN LINES TAKEN BY PROGRAM LINE STARTING AT DE (LEN OF NEW EPPC)

           INC A             ;A=1
           LD (DMPFG),A      ;"DUMP OUTPUT"
           LD HL,(SPOSNU)
           PUSH HL
           LD HL,(WINDLHS)   ;L=LHS
           LD A,&41
           SUB C             ;LINES FROM TOP OF PREV CURSOR POSN
           LD H,A
           LD (SPOSNU),HL    ;START AT HI LN, PAST ANY WINDBOT, NO SCROLLING

           EX DE,HL          ;PT HL TO FIRST CHAR
           CALL IOUTLNC      ;DUMMY PRINT OF LINE TO GET LENGTH
           XOR A
           LD (DMPFG),A
           LD HL,WINDTOP
           LD A,(WINDBOT)
           SUB (HL)
           INC A
           LD H,A
           LD A,(SPOSNU+1)   ;END LINE POSN
           SUB &40           ;A=LINES TAKEN BY PROGRAM LINE
           CP H              ;CP WINDOW HEIGHT
           JR C,MWDN2        ;JR IF LINE <WIND HEIGHT

           LD A,H

MWDN2:     POP HL
           LD (SPOSNU),HL
           CALL EDRSADN      ;SCROLL DOWN "A" LINES
           LD HL,(WINDLHS)   ;L=LHS, H=TOP
           LD (SPOSNU),HL    ;PRINT POSN AT 0,0
           POP HL            ;LINE START

IOUTLNC:   CALL SPACAN       ;**

IOUTLN:    LD BC,OUTLINC

IOPCL:     LD A,1
           LD (INDOPFG),A
           CALL BCJUMP       ;CALL BC WITH INDENT O/P ON

IOPOF:     XOR A
           LD (INDOPFG),A
           RET

OUTLINC:   RST &30
           DW OUTLINE-&8000

;CURSOR DOWN

LPD:       LD HL,(EPPC)
           PUSH HL
           CALL ADVEPPC      ;MOVE EPPC DOWN ONE - NEW EPPC IN DE
           POP HL
           AND A
           SBC HL,DE
           RET Z             ;RET IF EPPC HASN"T CHANGED - END OF PROG

           ADD HL,DE
           PUSH HL           ;OLD EPPC
           LD DE,(WINDTOP)
           LD A,D            ;BOT
           SUB E
           INC A
           PUSH AF           ;LINE NUMBER OF BOTTOM LINE+1, REL. TO WINDOW
           RST &30
           DW EROC2          ;GET A/C=LNPTR, H=TABLE MSB, ERASE OLD CURSOR
           POP DE            ;D=BOTTOM LINE+1

LPDL:      INC A
           CP D
           JR NC,MWUP        ;JR IF AT WINDOW BOT ALREADY - MOVE WINDOW UP

           ADD A,LPT\256     ;ADD OFFSET
           LD L,A            ;HL=ADDR IN TABLE (MUST BE INSIDE A PAGE)
           SUB LPT\256
           LD B,(HL)         ;B=0 IF NO LINE NO, OR FF
           INC B
           DEC B
           JR Z,LPDL         ;LOOP IF NO LINE NO. ON LINE

           POP HL            ;JUNK OLD EPPC

LPC:       LD C,A
           LD A,(WINDTOP)
           ADD A,C
           LD (LNPTR),A      ;NEW LINE WITH CURSOR
           LD A,C
           LD E,5
           CALL ATSR2
           RST &30
           DW PRLCU-&8000

MWUP:      CALL ADVSTOP      ;MOVE TOP OF SCREEN PROG LINE DOWN BY ONE
           XOR A
           LD (OVERT),A      ;OVER 0 SO OVERPRINTING LOOKS OK
           LD E,A            ;LH COLUMN
           LD HL,SCRCT
           LD (HL),E         ;PREVENT SCROLL PROMPT
           LD HL,WINDTOP
           LD A,(LNPTR)
           SUB (HL)
           CALL ATSR2
           POP HL            ;OLD EPPC
           CALL FNDLINE
           LD A,(OLDSPCS)
           LD (NXTSPCS),A    ;** RESET SPACES TO SAME FOR 2ND PRINT
           CALL IOUTLN       ;OLD EPPC MAY OVERLAP SCREEN BOTTOM - SO PRINT IT
           LD A,&0D          ;AGAIN, PERHAPS FORCING SCROLLING.
           RST &10           ;CR
           JR IOUTLN         ;PRINT NEW EPPC - IT WILL HAVE A ">" CURSOR


;ENSURE SYS VAR PTED TO BY HL CONTAINS A LINE NUMBER THAT EXISTS. (IF IT DOES
;NOT, REPLACE IT WITH NO. OF FOLLOWING "REAL" LINE)

REALN:     LD A,(HL)
           INC HL
           PUSH HL
           LD H,(HL)
           LD L,A            ;HL=LINE NO
           JR ADVAC

;ADVANCE LINE NUMBER IN SYS. VAR TO NEXT LINE IF POSSIBLE
;EXIT: DE=NEW LINE NUMBER, HL IS UNCHANGED

ADVSTOP:   LD HL,SDTOP       ;SCREEN DISPLAY TOP PROGRAM LINE
           JR ADVAN

ADVEPPC:   LD HL,EPPC

ADVAN:     LD A,(HL)
           INC HL
           PUSH HL
           LD H,(HL)
           LD L,A            ;HL=LINE NO FROM SYS VAR.
           INC HL

ADVAC:     CALL FNDLINE      ;FIND ADDR OF NEXT LINE, OR PROG END
           CALL LNNM         ;GET LINE NO. IN DE (OR PREV LINE NO. IF PROG END)
           POP HL
           LD (HL),D
           DEC HL
           LD (HL),E
           RET


                                 ;TABSR, CLS, PRINT
                                 ;FUPDN, IOUTLN, PRLCU, ZTENTS, LNLEN,
                                 ;ATSR, REALN, ADVAN
