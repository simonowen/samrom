;SCRFN.SAM - TEXT/GRAPHICS COPY, SCREEN$ FUNCTION, ATTR FN, POINT, BORDER,
;LOOK SAM, WINDOW, ZAP ETC.

;MAIN COPY ROUTINE:

COPY:      CP &FF
           JR NZ,GRCOPY

;TEXT COPY

           RST &20           ;SKIP 0FFH
           CP CHRSTOK
           JP NZ,NONSENSE

           RST &20

GRCOPY:    CALL CHKEND

JGCOPY:    DB &3E            ;'LD A,0AFH' **

JTCOPY:    XOR A

           LD HL,(DMPV)

JPOPT:     INC H
           DEC H
           RET Z

           JP (HL)           ;A=0 FOR TEXT COPY, 0AF FOR GRAPHICS

;SCREEN$(L,C)

IMSCREENS: CALL EXB2NUMB  ;CHECK (X,Y). CY IF RUNNING
           RET NC

           LD A,(UWRHS)
           INC A
           LD L,A            ;L=RHS LIMIT FOR COL, PLUS 1
           LD A,(UWBOT)
           ADD A,3           ;ALLOWS FOR LW OF 2 LINES
           LD H,A            ;H=BOT LIMIT FOR LINE, PLUS 1
           CALL GETCP        ;GET POSN IN DE
           CALL IMSCSR
           JP NC,CWKSTK      ;IF FOUND, STACK 1 BYTE STRING, COPY TO WORKSP

           XOR A
           JP STACKA         ;ELSE STACK NULL STRING

;CALLED BY TEXT COPY AND SCREEN$ FN
;ENTRY: DE=LINE/COL
;EXIT: CY IF FOUND, (HL)=CHAR, BC=1, ELSE NC, A=0

IMSCSR:    LD HL,DEVICE
           LD A,(HL)
           LD (HL),0         ;ENSURE DEVICE=UPPER SCREEN, OR ANYDEADDR FAILS
           PUSH AF
           PUSH HL
           CALL ANYDEADDR    ;GET DE=SCREEN ADDR, CY IF 6-PIX CHARS IN USE,
                             ;(IN WHICH CASE NZ/Z=ODD/EVEN COL)
           POP HL
           POP BC
           LD (HL),B
           EX DE,HL
           LD DE,SCRNBUF     ;BUFFER WILL HOLD TARGET CHAR IN A STANDARD FORM
           PUSH AF
           LD A,(MODE)
           CP 2
           JR C,SCM01

           POP AF            ;CY IF 6-PIX, NZ/Z=ODD/EVEN COL
           PUSH AF
           CALL CHARCOMP     ;COMPRESS SCREEN AREA TO BUFFER USING TOP LHS
                             ;PIXEL OF CHAR AS BACKGROUND
           POP AF
           JR NC,SCBMCH      ;JR IF NOT 6-PIX - BUFFER OK

           LD BC,&087E       ;8 BYTES TO ROTATE BY 1 PIX LEFT OR RIGHT FOR
                             ;CENTRAL POSITION, C=MASK TO TRIM LHS AND RHS BITS
           LD HL,SCRNBUF
           JR Z,SC6EAL       ;JR IF EVEN COLUMN

SC6OAL:    LD A,(HL)
           RLCA
           AND C             ;MASK OFF BITS 7 AND 0, WHICH MAY BE JUNK
           LD (HL),A
           INC HL
           DJNZ SC6OAL

           JR SCBMCH

SC6EAL:    LD A,(HL)
           RRCA
           AND C
           LD (HL),A
           INC HL
           DJNZ SC6EAL

           JR SCBMCH

;MODES 0 AND 1 DO NOT NEED COMPRESSION OF SCRN CHAR DATA - BUT TRANSFER IT

SCM01:     POP AF            ;JUNK FLAGS FOR 6-PIX
           LD BC,&0800       ;BYTES TO DO/NO INVERSE
           CALL SREAD
           RLA
           JR NC,SCM01L

           DEC C             ;MASK=FF FOR INVERTING CHAR IF TOP LH PIX=FORGROUND

SCM01L:    CALL SREAD
           XOR C
           LD (DE),A
           CALL NXTDOWN      ;DROP HL TO NEXT SCAN
           INC DE            ;NEXT BUFF POSN
           DJNZ SCM01L

;SCREEN$ BUFFER MATCH - MATCH BUFFER WITH CHAR PATTERNS

SCBMCH:    LD HL,(CHARS)
           INC H
           LD A,96           ;CHARS TO CHECK (32-127)
           CALL SCREENSR
           JR NC,SCRNFND

           LD HL,(UDG)       ;PTS TO CHR$ 144
           LD BC,-128
           ADD HL,BC         ;PT TO CHR$ 128
           LD A,41           ;128-168
           CALL SCREENSR
           RET C             ;RET IF NOT FOUND

UDGFND:    ADD A,96          ;1-41 -> 97-137

SCRNFND:   ADD A,32          ;1-96 -> 32-127, 97-137 -> 128-168
           LD BC,1           ;LEN=1
           LD HL,TEMPW1
           LD (HL),A         ;NC='FOUND'
           RET


;SR TO CHECK B CHARS IN CHAR. GENERATOR AT (HL) VS. CHAR IN SCRNBUF
;ENTRY: A=NO. OF CHARS, HL PTS TO CHARSET
;EXIT WITH NC IF MATCHED, A=CHAR MATCH POSN, 0 TO B-1, OR CY SET=FAIL

SCREENSR:  LD B,A
           EX AF,AF'         ;A'=CHARS TO CHECK (KEPT TILL END)
           LD DE,SCRNBUF+4
           LD A,(DE)
           LD C,A            ;C=BYTE 4 OF TARGET CHAR
           DEC DE
           LD A,(DE)         ;A=BYTE 3 OF TARGET CHAR
           INC HL
           INC HL
           INC HL            ;PT HL TO BYTE 3 OF CHAR GEN. CHARACTER
           LD DE,8           ;FOR STEPPING TO NEXT CHAR
           DB &FE            ;'JR+1'

SCREENLP:  ADD HL,DE
           CP (HL)           ;CHECK BYTE 3 FOR MATCH
           JR Z,SCREEN2      ;JR TO CHECK BYTE 4 IF NEEDED

SCREEN1:   DJNZ SCREENLP

           SCF               ;'FAILED'
           RET

SCREEN2:   LD A,C            ;BYTE 4 OF TARGET CHARACTER
           INC HL
           CP (HL)           ;VS. BYTE 4 IN CHAR. GEN. CHARACTER
           DEC HL
           LD A,(HL)
           JR NZ,SCREEN1

           PUSH HL
           PUSH BC
           DEC HL
           DEC HL
           DEC HL            ;PT TO BYTE 0 IN CHAR. GEN CHARACTER
           LD DE,SCRNBUF     ; AND BYTE 0 OF TARGET CHAR IN BUFFER
           LD B,8            ;DOING A FULL CHECK IS NOW WORTHWHILE

FULCKLP:   LD A,(DE)
           CP (HL)
           JR Z,FULCK1       ;JR IF FULL MATCH STILL OK

           POP BC
           POP HL
           LD A,(HL)
           LD DE,8
           JR SCREEN1

FULCK1:    INC HL
           INC DE
           DJNZ FULCKLP      ;LOOP TILL ALL 8 MATCHED OK

           POP BC
           POP HL
           EX AF,AF'         ;CHARS TO CHECK
           SUB B             ;FIRST CHAR MATCH GIVES 0, LAST GIVES ORIG B-1
           RET               ;WITH NC ='MATCHED'.

;PT 2 OF LIST

LSTR1:     CALL SPACAN

LSTLNL:    CALL OUTLINE
           RET C             ;RET IF HIT END OF PROG

           RST &10           ;PRINT 0DH
           CALL R1OFRD
           LD B,A
           CALL CHKHL        ;**
           INC HL
           CALL R1OFRD
           LD C,A
           DEC HL            ;BC=NUMBER OF NEXT LINE
           EX DE,HL
           LD HL,(LAST)    ;LAST LINE
           AND A
           SBC HL,BC
           EX DE,HL
           JR NC,LSTLNL      ;LOOP UNTIL PAST LAST LINE ASKED FOR ('LAST LINE'=
                             ;EPPC IF AUTO-LIST)
           LD A,(TVFLAG)
           AND &10
           RET Z             ;RET IF NOT AN AUTO-LIST, ELSE FILL REST OF SCREEN

           LD A,(WINDBOT)
           LD E,A
           LD A,(SPOSNU+1)   ;PRINT POSN ROW
           SUB E
           JR C,LSTLNL

           RET

;EXIT: CY=HIT END OF PROG, ELSE NC AND 0D JUST PRINTED.

OUTLINE:   CALL R1OFRD       ;LN MSB
           LD B,A
           ADD A,1
           RET C             ;RET IF FF END-OF-PROG STOPPER

           INC HL
           CALL R1OFRD
           LD C,A            ;BC=CURRENT LN NO.
           LD A,(NXTSPCS)
           LD (OLDSPCS),A    ;IN CASE LINE IS LAST ON SCREEN AND NEEDS
                             ;TO BE PRINTED TWICE - ALLOWS SPACES TO BE
                             ;RESET BEFORE 2ND TIME **
           XOR A             ;'BEFORE CURRENT LINE'
           EX DE,HL
           LD HL,(EPPC)
           SBC HL,BC         ;NC IF CUR LN=EPPC OR IS LESS THAN EPPC
           EX DE,HL          ;NC,Z IF '>' TO BE PRINTED
           PUSH AF
           JR NC,OUTLN2      ;JR IF 'BEFORE CURRENT LINE'

           INC A

OUTLN2:    LD (BCREG),A      ;1=CURRENT LINE HAS BEEN PRINTED
           CALL PRNUMB2      ;O/P NUMBER BC WITH LEADING SPACES
           LD A,(DEVICE)
           AND A
           JR NZ,OUTLN22     ;JR IF NOT UPPER SCREEN

           LD A,(WINDTOP)
           LD E,A
           LD A,(SPOSNU+1)   ;LINE
           SUB E             ;GET LINE REL. TO WINDOW
           ADD A,LPT\256
           LD E,A
           LD D,LPT/256
           LD A,D
           LD (DE),A         ;ENTRY IN LPT SHOWS LINE HAS A LINE NUMBER

OUTLN22:   INC HL
           INC HL
           INC HL            ;PT TO FIRST CHAR IN LINE
           POP AF
           JR NZ,OUTLN3

           CALL PRLCU        ;PRINT '>'

;USED BY EDPRNT

OUTLN25:   EX DE,HL
           LD HL,FLAGS
           SET 0,(HL)        ;'NO LEADING SPACE NOW'
           EX DE,HL
           JR OUTLN4

OUTLN3:    LD A," "
           RST &10           ;PRINT SPACE (ALSO SETS 'NO LEADING SPACE')

OUTLN4:    XOR A
           LD (NXTHSPCS),A
           LD A,(LISTFLG)
           AND A
           CALL R1OFRD       ;FIRST CHAR IN LINE
           PUSH HL           ;LINE PTR
           CALL NZ,SPACES    ;INDENT IF FLAG SAYS 'PRETTY LISTING'
           POP HL
           EX DE,HL          ;SAVE LINE PTR IN DE
           XOR A
           LD (INQUFG),A     ;'NOT IN QUOTES'

OUTLNLP:   LD HL,(XPTR)
           AND A
           SBC HL,DE
           CALL Z,PRFLQUERY  ;PRINT A FLASHING '?' IF SYNTAX ERROR PTR
           CALL OPCURSOR
           EX DE,HL
           CALL R1OFFCL
           DW RDCN           ;=LD A,(HL): CALL NUMBER, WITH ROM1 OFF
           LD (LSPTR),HL     ;LINE SCAN PTR - USED BY CUOP O/P ROUTINE
           INC HL
           CP &0D
           RET Z

           EX DE,HL          ;DE=LINE PTR

           CP ":"
           JR NZ,OUTCH2

           LD H,A
           LD A,(LISTFLG)
           AND A
           LD A,H
           JR Z,OUTCH3       ;JR IF NO PRETTY LISTING

           LD A,(INQUFG)
           RRCA              ;BIT 0=1 IF INSIDE
           LD A,H            ;':'
           JR C,OUTCH3       ;JR IF ':' IS INSIDE QUOTES

           LD A,(FLAGX)
           AND &20
           LD A,H
           JR NZ,OUTCH3      ;JR IF INPUT MODE

;DO TAB 0 IF USING LOWER SCREEN (TO OVER-WRITE JUNK WITH SPACES) OR DO CR FOR
;SPEED IF NOT; FOLLOW EITHER WITH 6 SPACES TO INDENT TEXT

           LD A,(INDOPFG)
           PUSH AF
           XOR A
           LD (INDOPFG),A    ;NO INDENTED O/P SO TAB FILLING LINE AVOIDS INDENT
           LD A,(DEVICE)
           DEC A
           LD A,&0D
           JR NZ,TABS2      ;JR IF NOT LOWER SCREEN

           LD A,&17          ;TAB
           RST &10
           XOR A
           RST &10
           XOR A

TABS2:     RST &10
           LD B,6
           CALL OPBSP
           POP AF
           LD (INDOPFG),A

           EX DE,HL
           CALL R1OFRD
           EX DE,HL          ;A=(DE) (CHAR TO O/P NEXT)
           CALL SPACES       ;SET SPACES, O/P CURRENT SPACES
           JR OUTLNLP

OUTCH2:    CP &22
           JR NZ,OUTCH3

           LD HL,INQUFG
           INC (HL)          ;FLIP BIT 0 (INSIDE/OUTSIDE QUOTES FLAG)

OUTCH3:    RST &10
           JR OUTLNLP


;SCROLL TABLE ENTRIES
;ENTRY: A=LINES TO SCROLL BY, D=1 IF UP, NOT 1 IF DOWN

STENTS:    LD L,A
           LD A,30
           SUB L
           LD C,A
           XOR A
           LD B,A
           LD H,A
           LD A,L

;HL=LINES TO SCROLL BY, BC=30-LINES TO SCROLL BY, A=LINES TO SCROLL BY, NC

           DEC D
           JR Z,PTU          ;JR IF SCROLL UP

           LD DE,LPT+29
           PUSH DE
           EX DE,HL          ;HL=LPT+29, DE=LINES
           SBC HL,DE         ;HL=LPT+29-LINES
           POP DE            ;LPT+29
           LDDR
           LD B,A
                             ;(NEW LINE FOR CURSOR SET BY PRINTING OF LINE)
           XOR A

PDCL:      LD (DE),A         ;NEW ENTRIES IN TABLE ARE ZEROS
           DEC DE
           DJNZ PDCL

           RET

PTU:       LD DE,LPT
           ADD HL,DE
           LDIR
           LD B,A
           LD HL,LNPTR
           LD A,(HL)
           SUB B
           LD (HL),A         ;NEW LINE FOR CURSOR
           XOR A

PTCL:      LD (DE),A
           INC DE
           DJNZ PTCL

           RET

SPACES:    LD HL,NXTSPCS     ;POINT TO 'NEXT-SPACES' VAR.
           PUSH HL
           LD B,(HL)
           INC HL
           LD (HL),B         ;CURRENT SPACES=NEXT SPACES
           INC HL
           LD B,(HL)
           INC HL
           LD (HL),B         ;CURRENT THEN SPACES=NEXT THEN SPACES
           POP HL            ;PTR TO NEXT-SPACES

           CP &D4            ;DOTOK
           JR Z,INCSPCS

           CP &D7            ;LIFTOK
           JR Z,INCSPCS

           CP &CA            ;DEFPROCTOK
           JR Z,INCSPCS

           CP &C0            ;FORTOK
           JR Z,INCSPCS      ;THESE CMDS NEED NEXT SP=NEXT SP+INDENT
                             ;(INDENT FROM NEXT STATEMENT ONWARDS)
           CP &D5            ;LOOPTOK
           JR Z,DECSPCS

           CP &CB            ;ENDPROCTOK
           JR Z,DECSPCS

           CP &DB            ;ENDIFTOK
           JR Z,DECSPCS      ;THESE CMDS (AND NEXT) NEED TO CANCEL INDENT FOR
                             ;CURRENT AND LATER STATEMENTS.

           CP &C1            ;NEXTTOK
           JR NZ,SPACES2

DECSPCS:   CALL SPACESR      ;CURRENT=CURRENT-INDENT
           LD B,(HL)
           DEC HL
           LD (HL),B         ;NEXT SPACES=CURRENT
           JR SPACES4

SPACES2:   CP &D6            ;EXITIFTOK
           JR Z,SPACES3

           CP &D9            ;LELSETOK
           JR Z,SPACES3

           CP &D3            ;LOOPIFTOK
           JR Z,SPACES3      ;LOOP IF, LELSE AND EXIT IF CANCEL INDENT FOR
                             ;CURRENT STAT ONLY.
           INC HL
           INC HL            ;PT TO SPACES THAT WILL CANCEL AT LINE END (SIF)
           CP &DA            ;ELSETOK
           JR NZ,SPACES35    ;SHORT ELSE CANCELS SOME OF THEM, AS DOES 'ON'

SPACES3:   CALL SPACESR
           JR SPACES4

SPACES35:  CP &D8            ;SIFTOK
           JR Z,INCSPCS      ;SHORT IF INDENTS FOR LATER STATEMENTS, THIS LINE

           CP &DE            ;ONTOK. AS DOES 'ON'
           JR NZ,SPACES4

INCSPCS:   LD A,(LISTFLG)
           ADD A,(HL)
           LD (HL),A
           DB &FE            ;"JR+3" (CP 3EH: LD B,FEH)

;CALLED FROM MAIN PRINT ROUTINE WHEN LINE FULL

INDOPEN:   LD A,6
           DB &FE            ;'JR+1'

SPACES4:   XOR A
           LD HL,CURSPCS
           ADD A,(HL)
           INC HL
           INC HL
           ADD A,(HL)
           RET Z

           LD B,A
           LD HL,(WINDRHS)
           LD A,L
           SUB H             ;SUB RHS,LHS=WIDTH-1
           CP 10
           RET C             ;NO INDENT IF 10 COLS OR LESS

           SUB 5
           CP B
           JR NC,SPCS5       ;FULL INDENT IF ROOM

           LD B,A            ;ELSE INDENT BY WIDTH-6

SPCS5:     JP OPBSP

SPACESR:   INC HL
           LD A,(LISTFLG)
           LD B,A
           LD A,(HL)
           SUB B
           LD (HL),A
           RET NC

           LD (HL),&00
           RET



EDPTR2:    CALL TEMPS
           CALL POFETCH
           PUSH DE           ;SAVE SCRN POSN SO THAT SEVERAL EDPRTS CAN ALL
                             ;START AT THE SAME POSN, NOT CONCATENATED!
           LD A,(WINDTOP)
           LD (TEMPB2),A
           LD HL,TVFLAG
           RES 5,(HL)        ;'NO NEED TO CLEAR LS ON KEYSTROKE'
           RES 3,(HL)        ;'NO NEED TO COPY LINE TO LOWER SCRN'
           LD HL,(ERRSP)
           PUSH HL
           LD HL,EDPE        ;NEW ERROR HANDLER
           PUSH HL
           LD (ERRSP),SP
           CALL SETDE        ;DE=START OF ELINE OR INPUT LINE
           LD HL,(OLDPOS)
           PUSH HL           ;POSN OF END OF LINE IN LOWER SCREEN
           EX DE,HL          ;HL=LINE START
           LD BC,OUTLN25
           LD A,(FLAGX)
           AND &20
           JR NZ,EDIM        ;JR IF INPUT MODE - NO INDENT

           LD A,(HL)
           CP &0D
           JR Z,EDCOP        ;JR IF LINE EMPTY ** BUG FIX

           CALL SPACAN
           CALL IOPCL        ;PRINT THE LINE, INDENTED
           CP A              ;Z

EDIM:      CALL NZ,BCJUMP

EDCOP:     EX DE,HL          ;DE=LINE PTR
           CALL OPCURSOR     ;PRINT THE CURSOR IF IT IS AT THE END OF THE LINE
                             ;(OR IT HAS BEEN PRINTED ALREADY)
           CALL TEMPS
           CALL POFETCH      ;USE VALUE AS NEW 'OLDPOS'
           POP HL            ;OLDPOS
           CALL LSASR        ;ADJUST IF LS SCROLLED
           PUSH DE           ;NEW OLDPOS
           PUSH HL

EDBL:      POP BC            ;OLDPOS
           CALL POFETCH      ;NEW POSN OF END OF LINE IN LOWER SCREEN
           EX DE,HL
           AND A
           SBC HL,BC         ;NORMALLY NEW POSN IS FURTHER RIGHT, OR DOWN, AS
           JR NC,EDBE        ;TEXT IS ADDED, SO NC. BUT IF DELETE USED, CY

           PUSH BC
           LD A," "
           CALL FONOP        ;NORMAL OUTPUT
           JR EDBL           ;LOOP TILL LINE BLANKED TO OLD END-OF-LINE.

;EDPRNT ERRORS COME HERE

EDPE:      CALL WARNBZ
           CALL POFETCH      ;FOR OLDPOS
           DB &21            ;'JR+2'

EDBE:      POP DE            ;LAST END-OF-LINE POSN
           POP HL            ;ERROR HANDLER ADDR

           POP HL
           LD (ERRSP),HL
           LD (OLDPOS),DE
           POP HL            ;ORIG SCRN POSN
           CALL LSASR
           CALL POSTORE      ;ORIG SCRN POSN, MODIFIED IF SCOLLED UP
           XOR A
           LD (XPTR+1),A     ;CANCEL ANY '?' ERROR MARKER
           RET

LSASR:     LD A,(TEMPB2)
           LD B,A
           LD A,(WINDTOP)
           SUB B             ;A IS EG -1 IF LW HAS EXPANDED UP BY ONE
           ADD A,H           ;EG ADJUST PRINT LINE UP BY ONE IF LS SCROLLED
           LD H,A
           RET

;EDITOR'S SPECIAL CURSOR OUTPUT TO SET SCREEN POSN

CUOPP:
           LD BC,(SPOSNL)
           LD HL,(KPOS)
           AND A
           SBC HL,BC
           JR NZ,CUOP2

           LD HL,(LSPTR)
           LD (KCUR),HL
           PUSH AF
           CALL OPCUR2       ;PREVENT LINE BEING BRIEFLY PRINTED 1 CHAR SHORTER
           POP AF            ;AS OLD CURSOR VANISHES AND NEW CURS. POSN FOUND

CUOP2:     JP FONOP


;OP CURSOR IF DE=KCUR.

OPCURSOR:  LD HL,(KCUR)
           AND A
           SBC HL,DE
           RET NZ            ;RET IF THIS ISN'T CURSOR'S ADDR IN LINE

           IN A,(251)
           LD H,A
           LD A,(KCURP)
           XOR H
           AND &1F
           RET NZ            ;**

           LD HL,(SPOSNL)
           LD (KPOS),HL      ;SAVE SCREEN POSN OF CURSOR

OPCUR2:    LD HL,(KURV)
           CALL JPOPT

           LD HL,(KURCHAR)
           LD A,(FLAGS2)
           AND &08
           LD A,L
           JR Z,PRINVERT     ;JR IF CAPS LOCK OFF

           LD A,H
           JR PRINVERT

;PRINT LINE CURSOR - USED BY MAIN LIST ROUTINE AND FUPDN

PRLCU:     LD A,(SPOSNU+1)
           LD (LNPTR),A      ;STORE LINE NO. WITH CURSOR
           LD A,(LNCUR)
           DB &FE            ;"JR+2" (CP 3EH: CCF). PRINT INVERSE '>'

PRFLQUERY: LD A,"?"

;PRINT CHAR IN 'A' REG IN INVERSE VIDEO
;USE CALL TO MNOP (VIA FONOP), NOT RST 10H, IN CASE INVERSE CHAR IS PRINTED
;BETWEEN CC AND ITS PARAMETER (AND CHANNEL ADDR HAS BEEN CHANGED).

PRINVERT:  LD B,A
           PUSH HL
           LD HL,PFLAGT
           LD A,(HL)
           PUSH AF
           SET 2,(HL)        ;INVERSE 1
           LD A,(INVERT)
           PUSH AF
           LD A,(INQUFG)
           PUSH AF
           LD A,(BGFLG)
           PUSH AF
           LD A,&FF
           LD (INQUFG),A     ;'IN QUOTES' SO CURSORS CAN BE UDGS
           LD (INVERT),A
           LD (BGFLG),A
           LD A,B
           PUSH DE
           CALL FONOP
           POP DE
           POP AF
           LD (BGFLG),A
           POP AF
           LD (INQUFG),A
           POP AF
           LD (INVERT),A
           POP AF
           LD (PFLAGT),A
           POP HL
           RET


;PRINT INTEGER IN A, AS 1-3 CHARS. HL IS SAVED. USED FOR STAT AND ERROR NUMS.

PRAREG:    LD C,A
           LD B,0

;USED FOR LINE NUMS IN REPORTS

PRNUMB1:   LD E,0            ;NO LEADING SPACES
           LD A,B
           INC A
           JR NZ,PRNUMBC

           LD C,A
           LD B,A
           DB &22            ;"JR+2". NUMBERS >=FF00 PRINT AS 0 (EG ELINE)

;PRINT INTEGER IN BC, PADDED TO 5 CHARS WITH LEADING SPACES. HL IS SAVED.
;USED FOR LINE NUMS IN LISTINGS. USES BC, DE, AF.

PRNUMB2:   LD E,&20          ;LEADING SPACES

PRNUMBC:   PUSH HL
           PUSH BC           ;NUMBER
           LD HL,SUBTAB

PRNUOLP:   LD C,(HL)
           INC HL
           LD B,(HL)
           INC HL
           EX (SP),HL        ;HL=NUMBER
           LD A,L
           INC C
           DEC C
           JR Z,PRNTNO1      ;JR IF WE ARE AT 'UNITS' STAGE (C=TERMINATOR)

           XOR A

PRNUILP:   INC A
           ADD HL,BC
           JR C,PRNUILP      ;SUCCESSIVE SUBTRACTIONS OF EG. 10, 100

           SBC HL,BC
           DEC A             ;A=DIGIT FOR 1000'S, 100'S ETC.
           JR Z,PRNTNO2      ;LEAVE E ALONE IF IT'S A ZERO DIGIT

PRNTNO1:   LD E,&30          ;AFTER A NON-ZERO DIGIT USE '0' FOR ZERO, RATHER
                             ;THAN ' ' OR ''.
PRNTNO2:   ADD A,E
           CALL NZ,&0010     ;CALL UNLESS LEADING SPACES SUPPRESSED.
           EX (SP),HL        ;HL=TABLE PTR
           INC C
           DEC C
           JR NZ,PRNUOLP     ;LOOP IF NOT AT 'UNITS' YET

           POP HL            ;NUMBER
           POP HL            ;ORIG
           RET

                                 ;OPCURSOR, PRLCU, PRINVERT, STENTS, PRAREG,
                                 ;PRNUMB
