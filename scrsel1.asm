;SCRSEL1.SAM.  SCREEN, OPEN SCREEN, CLOSE SCREEN, SOUND

;SELECT SCREEN C

JSCRN:     CALL SCRNTLK2
           JR JSCR2

;SCREEN N - PRINT/PLOT ETC ON SCREEN N, DISPLAY PAGE N IF (CURDISP)=0

SCREEN:    CALL SYNTAX6

           CALL SCRNTLK1     ;GET MODE/PAGE FOR SCREEN C, Z IF UNUSED

JSCR2:     JR Z,ISCEH        ;'INVALID SCREEN NUMBER' IF NOT OPEN

           LD A,(CUSCRNP)
           LD DE,(SCPTR)
           LD (DE),A         ;UPDATE SCLIST MODE AS WE SWITCH OUT THIS SCREEN
           LD (SCPTR),HL
           LD A,(HL)
           PUSH AF
           CALL SSVARS       ;SAVE SCREEN VARS TO STORE IN SCREEN PAGE
           POP AF
           CALL PRSVARS       ;COPY VARS FROM SELECTED SCREEN TO SYS VARS
           LD A,(CURDISP)
           AND A
           JP Z,DEFDISP

           RET               ;RET - DISPLAY FIXED ON A GIVEN SCREEN


SCRNTLK1:  CALL GETBYTE      ;GET SCREEN NUMBER FROM FPCS

;GET MODE/PAGE FOR SCREEN C FROM LIST. RETURN NZ IF OPEN, Z AND A=FF IF CLOSED
;EXIT WITH HL POINTING TO ENTRY FOR SCREEN. SCREEN NUMBER 1-16 OR ERROR MSG.

SCRNTLK2:  DEC C
           LD A,C
           CP &10

ISCEH:     JP NC,ISCRERR     ;LIMIT SCREENS TO ORIG. OF 1-16

           LD HL,SCLIST
           LD B,0
           ADD HL,BC
           LD A,(HL)         ;BIT 7=0, BITS 6-5=MODE, BITS 4-0=PAGE (BIT 0=0)
           CP &FF
           RET               ;Z IF FF (CLOSED)

;*******************************************************************************
;CLOSE SCREEN N
;CLOSE #N
;CLOSE N  - CLOSE N PAGES

CLSCRN:    CP "#"
           JR NZ,CLNCH       ;JR IF NOT A STREAM CLOSE

           CALL SSYNTAX6     ;CLOSE #S

           CALL STRMINFO
           RET Z             ;RET IF CLOSED

           PUSH HL
           CALL CHLTCHK
           JR Z,CLOS1        ;OK IF K, S, P, $ OR B **
                             ;HL=PTR TO LETTER IN CHANNEL, A=LETTER
           POP DE            ;STREAM PTR
           RST &08           ;CLOSE A NON-K/S/P STREAM.
           DB CSHK           ;DOS CLOSE.
           RET

CLOS1:     LD DE,0           ;VALUE FOR STRM PTR IF CLOSING 4-15
           LD A,C
           CP 4
           JP NC,OLT4

           LD E,A
           LD HL,STRMTAB+5
           ADD HL,DE         ;PT HL TO INITIAL VALUE FOR STREAMS 0-3
           JP OPEN2

CLNCH:     CP &E7            ;SCREENTOK
           JR Z,CLSC0

;CLOSE N PAGES

           CALL SYNTAX6      ;NUMBER OF PAGES TO CLOSE

CLTO:      CALL OCPSR        ;GET L=PAGES USED NOW, HL PTING TO PAST LAST
                             ;ENTRY IN ALLOCT, B/C=PAGES TO CLOSE
           LD A,(RAMTOPP)
           INC A             ;NO. OF PAGES THAT *MUST* BE USED BY BASIC
           LD E,A            ;(UNLESS RAMTOP MOVED)
           LD A,L
           SUB C             ;PAGES THAT WILL BE LEFT AFTER CLOSE
           JR C,OMH

           CP E

OMH:       JP C,OOMERR       ;MUST BE >=MINIMUM NO.

           XOR A

CLPL1:     DEC HL
           LD (HL),A         ;FREE PAGE
           DJNZ CLPL1

SETLPG:    DEC HL
           LD A,L
           LD (LASTPAGE),A
           RET

JCLSCR:    CALL SCRNTLK2
           JR JCS2

CLSC0:     CALL SSYNTAX6      ;N

           CALL SCRNTLK1

JCS2:      RET Z             ;END IF NOT USED YET

           INC C
           DEC C             ;Z IF ORIG SCREEN NUMBER WAS 1 (C=0)
           JP Z,ISCRERR      ;CANNOT CLOSE SCREEN 1!

           AND &1F           ;ISOLATE PAGE USED BY SCREEN TO CLOSE
           LD B,A
           LD A,(CUSCRNP)
           AND &1F
           CP B
           JR NZ,CNCS

           RST &08           ;CANNOT CLOSE CURRENT SCREEN
           DB 46             ;'Current screen'

CNCS:      LD (HL),&FF       ;MARK SCREEN ENTRY AS CLOSED
           LD L,B            ;HL WILL PT TO ALLOC TABLE ENTRY FOR SCRN PAGE
           LD H,ALLOCT/256
           XOR A
           LD (HL),A
           INC HL
           LD (HL),A
           RET

;*******************************************************************************
;OPEN SCREEN N,M<,C> (NUMBER, MODE)
;OPEN #N,A$  (OPEN A STREAM TO A CHANNEL)
;OPEN N      (OPEN A NUMBER OF PAGES FOR USE)

OPSCRN:    CP "#"
           JP NZ,OPNCH       ;JR IF NOT 'OPEN TO A CHANNEL'

;CHECK FOR OPEN #S;A$ OR OPEN #S,A$

           CALL SEXPT1NUM
           CALL INSISCSC     ;',/;'
           CALL SYNTAXA

           CALL SWOP12       ;GIVES NAME,STREAM
           CALL STRMINFO     ;Z IF CLOSED. HL PTS TO STRM PTR MSB
           PUSH HL
           JR Z,OPEN1        ;JR IF CHANNEL IS CLOSED

           CALL CHLTCHK
           JR NZ,SAOERR      ;ERROR IF NOT OPEN TO K/S/P/$/B/N ALREADY

           JR NC,OPEN1       ;JR IF OPEN TO K/S/P, ERROR IF OPEN TO $/B/N

SAOERR:    RST &08
           DB 45             ;'Stream is already open'

OPEN1:     CALL SBFSR        ;COPY FILE NAME TO BUFFER. DE=START, A/C=LEN
           JP Z,IFNER        ;JP IF LEN ZERO

           DEC A
           JR NZ,INVCHP      ;ERROR (OR DOS) IF NAME LONGER THAN 1. E.G.
                             ;OPEN #4;"S" IS OK BUT OPEN #4;"FILE" JUMPS
           LD A,(DE)
           OR &20            ;LETTERS BECOME L.C. AND '$' UNCHANGED
           LD HL,CLTAB
           LD B,5            ;CHECK 5 CHANNEL TYPES

OPCL:      CP (HL)
           INC HL
           JR Z,OPEN2

           INC HL
           DJNZ OPCL

INVCHP:    POP HL            ;PTR TO STREAM PTR MSB
                             ;DE POINTS TO NAME, C=LEN
           RST &08           ;OPEN #S,A$ WITH A$ NOT K, S, P, $, OR B OR LEN>1
           DB OSHK           ;DOS OPEN
           RET

OPEN2:     LD E,(HL)         ;GET DISPLACEMENT FROM TABLE

OLT4:      POP HL
           LD (HL),0
           DEC HL
           LD (HL),E
           RET

CLTAB:     DB "k",1
           DB "s",6
           DB "p",16
           DB "$",21
           DB "b",26

OPNCH:     CP &E7            ; SCREENTOK
           JR Z,OPSCR0

;OPEN N PAGES  OR OPEN TO PAGE N

           CP TOTOK
           JR NZ,NOTP

           CALL SSYNTAX6

           CALL GETBYTE
           DEC C             ;E.G. OPEN TO 1 DOES NOTHING IF LASTPAGE=0
           LD A,(LASTPAGE)
           SUB C
           RET Z             ;RET IF OPEN TO CORRECT PAGE ALREADY

           JR C,TOPO

           CALL STACKA
           JP CLTO

TOPO:      NEG               ;GET PAGES TO OPEN
           CALL STACKA
           CP A

;OPEN N PAGES - NZ ON ENTRY

NOTP:      CALL NZ,SYNTAX6   ;NUMBER OF PAGES TO OPEN

           CALL OCPSR        ;GET HL PTING TO PAST LAST CURRENT PAGE
                             ;ENTRY IN ALLOCT, B/C=PAGES TO CLOSE

OPL1:      INC (HL)
           DEC (HL)
           JP NZ,OOMERR      ;ERROR IF NOT ENOUGH FREE PAGES ABOVE CONTEXT'S
                             ;CURRENT PAGES
           INC HL
           DJNZ OPL1

           LD B,C
           CALL SETLPG       ;DEC HL, SET LAST PAGE

OPL4:      LD (HL),&40       ;RESERVE PAGE
           DEC HL
           DJNZ OPL4

           RET

JOPSCR:    PUSH BC           ;MODE IN B
           CALL SCRNTLK2     ;CHECK SCREEN C
           JR JOPS2

;OPEN SCREEN N,M    (NUMBER, MODE)

OPSCR0:    CALL SSYNTAX8     ;N,M

           LD DE,&0400+34
           CALL LIMDB        ;MODE 0-3 FROM ORIG OF 1-4
           PUSH AF           ;MODE
           CALL SCRNTLK1     ;GET N FROM FPCS, LOOK IN TABLE FOR SCREEN N

JOPS2:     JR Z,OPSCR4       ;OK IF NOT USED YET

           RST &08
           DB 44             ;'Screen already open'

OPSCR4:    PUSH HL           ;SCREEN LIST PTR
           LD HL,ALLOCT+&20  ;HL PTS TO ALLOCT TERMINATOR

OPSCRLP:   DEC L
           LD A,(HL)
           DEC L
           JP Z,OOMERR       ;OUT OF MEMORY IF NO SPACE FOR NEW SCREEN

           OR (HL)           ;Z IF 2 PAGES UNUSED (0)
           JR NZ,OPSCRLP     ;LOOP UNTIL FOUND A FREE 32K BLOCK, EVEN START PAGE

           POP DE            ;SCLIST PTR
           POP AF            ;MODE TO OPEN IN
           PUSH AF

;A=MODE, HL PTS TO ALLOCT ENTRY, DE TO SCLIST ENTRY
;MARK ALLOCT, SCLIST

           LD (HL),&C0
           INC HL
           LD (HL),&C0       ;RESERVE PAGES IN SYSTEM PAGE ALLOC TABLE
           DEC HL            ;L=PAGE NUMBER 02-1EH
           RRCA
           RRCA
           SRL A             ;0MMX XXXX
           XOR L
           AND &E0           ;0MM FROM A, PAGE FROM L
           XOR L
           LD (DE),A         ;MODE/PAGE DATA TO SCRN LIST
           PUSH AF           ;MODE/PAGE FOR NEW SCREEN
           CALL SSVARS       ;STORE CURRENT SCREEN VARS SINCE WE ARE FIDDLING...
           CALL SDISRC       ;STORE CURRENT PALTAB
           LD HL,CUSCRNP
           LD B,(HL)
           POP AF            ;NEW MODE/PAGE
           LD (HL),A
           POP DE            ;D=MODE FOR NEW SCREEN
           PUSH BC           ;B=NORMAL CUSCRNP
           PUSH DE           ;MODE
           LD HL,THFATP
           LD B,(HL)
           LD (HL),1         ;ENSURE NO XRG CHANGE
           POP AF            ;MODE
           PUSH BC
           CALL MODPT2       ;CLEAR NEW SCREEN IN DESIRED MODE, SET UP EXPAN.
                             ;TABLES, ETC

           CALL SDISRC       ;COPY PALTAB TO NEW SCREEN AREA
           POP AF
           LD (THFATP),A     ;ORIG STATUS
           CALL SSVARS       ;SET UP VARS IN NEW SCREEN PAGE
           POP AF            ;ORIG CUSCRNP
           CALL PRSVARS      ;RESTORE SCREEN VARS
           SCF               ;"RESTORE PALTAB"
           DB &3E            ;"JR+1"

SDISRC:    AND A             ;NC - "SAVE PALTAB"

           LD A,(CUSCRNP)
           JP SDISR


;SEE IF PAGE L IS IN USE AS A SCREEN PAGE

SPGLOOK:   LD B,&10
           LD DE,SCLIST

SPGLKLP:   LD A,(DE)
           INC DE
           AND &1F
           CP L
           RET Z             ;RET IF THIS CONTEXT USES PG L AS A SCREEN ALREADY

           DJNZ SPGLKLP
           RET               ;NZ IF UNUSED

;OPEN/CLOSE PAGE SR

OCPSR:     LD DE,&1E00+30
           CALL LIMDB        ;ALLOW ONLY 1-30
           LD B,C            ;B AND C=PAGES TO OPEN
           LD HL,ALLOCT

OCL1:      INC HL
           LD A,(HL)
           CP &40
           JR Z,OCL1         ;LOOK PAST LAST PAGE RESERVED BY BASIC

           LD D,L             ;NUMBER OF PAGES CURRENTLY USED
           RET


;CHANNEL LETTER CHECK.
;ENTRY: DE=DISP FROM CHANS TO CHANNEL.
;EXIT: HL POINTS TO CHANNEL LETTER, Z,NC IF K/S/P. Z,CY IF $/B

CHLTCHK:   LD HL,(CHANS)
           ADD HL,DE         ;PT TO 2ND BYTE OF CHANNEL
           INC HL
           INC HL
           INC HL
           LD A,(HL)
           CP "K"
           RET Z

           CP "S"
           RET Z

           CP "P"
           RET Z

           CP "$"
           SCF
           RET Z

           CP "B"
           SCF
           RET Z

           AND A
           RET


;INTERRUPTS. ENTRY VIA RST 38H

INTS:      LD A,C
           LD (LASTSTAT),A
           PUSH BC           ;HMPR/STAT
           PUSH DE
           RRA
           JP NC,LINEINT

           RRA
           JR NC,COMINT

           RRA
           JR NC,MIPINT

           RRA
           JR NC,FRAMINT

;MIDI OUTPUT (OR OTHER) INTERRUPT

           LD HL,(MOPV)
           JR CMMDIC

;COMS (EX MOUSE) INTERRUPT

COMINT:    LD HL,(COMSV)
           JR CMMDIC

;MIDI INPUT INTERRUPT

MIPINT:    LD HL,(MIPV)
           IN A,(MDIPORT)    ;READ MIDI INPUT BYTE (AUTOMATIC INT CANCEL?)

CMMDIC:    INC H
           DEC H
           CALL NZ,HLJUMP

           JP INTEND

FRAMINT:   LD HL,(FRAMIV)
           LD A,H
           OR L
           CALL NZ,HLJUMP

           LD HL,LINICOLS
           LD (LINIPTR),HL           ;RESET LINE INT COL CHANGE LIST PTR
                                     ;TO START
           LD A,(HL)                 ;LINE TO INT ON,
           OUT (STATPORT),A          ;OR FF FOR 'NEVER INTERRUPT'

           LD A,(PALFLAG)
           LD HL,PALTAB+15
           RRA
           JR NC,FRMI3

           LD L,(PALTAB+35)\256

FRMI3:     LD BC,16*256+CLUTPORT
           OTDR                      ;SET UP PALETTE MEMORIES

           LD HL,SPEEDIC             ;COUNTER FOR DELAY BETWEEN SWAPPING INKS
           DEC (HL)
           JR NZ,FRMI5

           INC HL                    ;PT TO PALETTE FLAG
           INC (HL)                  ;FLIP BIT 0 OF PALFLAG - USE OTHER PALET
           LD A,(SPEEDINK)
           DEC HL
           LD (HL),A                 ;RELOAD COUNTER TILL NEXT FLASH
           LD HL,LINICOLS
           JR FRMI4

FISCL:     INC HL
           INC HL
           LD A,(HL)         ;COL 1
           INC HL
           LD B,(HL)         ;COL 2
           LD (HL),A
           DEC HL
           LD (HL),B
           INC HL
           INC HL            ;NEXT LINE

FRMI4:     LD A,(HL)
           INC A
           JR NZ,FISCL

FRMI5:     LD HL,FRAMES
           INC (HL)
           JR NZ,INTS3

           LD A,(SOFFCT)     ;DECED EVERY 5.1 SECS OR SO, SET TO 0 BY KEYBD USE.
           DEC A             ;IF DECED TO ZERO, KYBD NOT USED FOR ABOUT 22 MINS.
           LD (SOFFCT),A
           JR NZ,INTS2       ;JR IF USED WITHIN LAST 22 MINS.

           LD A,(SOFE)
           AND A
           JR NZ,INTS2       ;JR IF 'SCREEN OFF' DISABLED

           LD A,&80
           OUT (KEYPORT),A
           LD (SOFLG),A      ;'SCREEN OFF'

INTS2:     INC HL
           INC (HL)          ;INC SECOND BYTE OF FRAMES
           JR NZ,INTS3

           INC HL
           INC (HL)          ;AND THIRD
           JR NZ,INTS3

           LD HL,(FRAMES34)
           INC HL            ;FOURTH AND FIFTH
           LD (FRAMES34),HL

INTS3:     LD HL,(MOUSV)
           DEC H
           LD A,H
           INC H
           JR NZ,INTS4       ;IF JR NOT TAKEN, A=FF

           IN A,(KEYPORT)
           LD HL,MSEDP
           LD B,8            ;READ MOUSE 9 TIMES TO CANCEL IT

MSDML:     LD A,&FF
           IN A,(KEYPORT)
           LD (HL),A
           INC HL
           DJNZ MSDML        ;ALWAYS Z HERE

INTS4:     CALL NZ,HLJUMP

INTS5:     CALL KEYRD2
           JR INTEND


;ENTRY:
;LINIPTR POINTS TO SCAN LINE VALUE IN 4-BYTE CURRENT ENTRY (SCAN/PAL/INK1/INK2)
;BORDER TIME HAS FINISHED BEFORE WE GET HERE

LINEINT:     LD HL,(LINIPTR)

LNINSCAN:    LD D,(HL)                 ;D=SCAN LINE IN LINICOL ENTRY
             LD BC,&0100+CLUTPORT      ;PORT BC READS PEN Y (SCAN LINE)
             IN A,(KEYPORT)
             AND &20
             JR NZ,LINILP              ;JR IF LIGHT PEN KEEPS PEN Y FROZEN

LNWAITLP:    IN A,(C)
             CP D
             JR Z,LNWAITLP             ;WAIT FOR NEXT SCAN'S BORDER

LINILP:      INC HL
             LD B,(HL)                 ;PALETTE MEMORY NO.
             INC HL
             LD A,(HL)                 ;NEW VALUE
             OUT (C),A                 ;OUT (BC) WRITES DESIRED PAL. MEM.
             INC HL                    ;SKIP ALTERNATE VALUE FOR FLASHING
             INC HL
             LD A,(HL)                 ;NEXT SCAN VALUE (MIGHT BE THE SAME)
             SUB D
             JR Z,LINILP               ;DO MORE CHANGES FOR THIS SCAN (SCAN D+1)
                                       ;TAKES ABOUT 32 T'S FOR FIRST COLOUR,
                                       ;THEN ABOUT 80 T'S PER LOOP, SO SHOULD
                                       ;GET 2 CHANGES IN BORDER TIME, AND ABOUT
                                       ;3 IN MIDDLE PART.
                                       ;(IF WE USE TOO MANY CHANGES, WE MIGHT
                                       ;MISS A CLOSE-FOLLOWING LINE INT CHANGE
                                       ;AND THEN MISS ALL LATER CHANGES COS
                                       ;LINE INT REG NOT SET UP.)

             CP 1                      ;IS NEXT ENTRY FOR NEXT SCAN?
             JR Z,LNINSCAN             ;IF SO, WAIT FOR IT

             ADD A,D
             LD (LINIPTR),HL
             OUT (STATPORT),A          ;IF A>191, NO MORE COLOUR CHANGES

INTEND:      LD HL,(SPSTORE)
             POP DE
             POP AF          ;A=FORMER HMPR
             RET


;FROM INTS

KEYRD2:    CALL KINTER       ;SCAN KEYBD, PLACE CHAR IN BUFFER IF THERE IS ONE.
           LD HL,(KBQP)      ;L=KEY BUFFER QUEUE END (POSN TO PLACE CHAR AT)
                             ;H=QUEUE HEAD (PTS TO NEXT CHAR TO BE READ)
                             ;BOTH ARE DISPLACEMENTS FROM KBQB (BUFFER START)
                             ;IF H=L, BUFFER IS EMPTY.
           LD A,H
           CP L
           RET Z             ;RET IF NO CHARS IN BUFFER

           LD HL,FLAGS
           BIT 5,(HL)
           RET NZ            ;RET IF LAST KEY NOT READ YET (KEY AVAILABLE)

           SET 5,(HL)        ;'KEY PRESSED'
           LD L,A
           LD H,0
           INC A
           AND &07
           LD (KBQP+1),A     ;NEW HEAD POSN REFLECTS CHAR TRANSFER TO COME
           LD DE,KBQB
           ADD HL,DE         ;PT TO HEAD
           LD A,(HL)
           LD (LASTK),A      ;TRANSFER TO LASTK
           RET

KINTER:    CALL KEYSCAN
           LD A,0
           JR NZ,LDLH        ;JR IF NO KEY PRESSED
                             ;(NLASTH=0 IF NO KEY OR JUST A SHIFT KEY. USED BY
                             ;AUTO-REPEAT)
           LD HL,NLASTH
           LD A,E            ;UNSHIFTED KEY CODE
           CP (HL)
           JR Z,KBCR         ;JR IF SAME KEY AS LAST TIME

           INC HL
           CP (HL)
           JR Z,KBCR         ;OR TIME BEFORE

           CP 65             ;CODE FOR ENTER KEY - PRONE TO STUTTER
           JR NZ,KBCR

           INC HL            ;EXTRA CHECK FOR ENTER KEY ONLY
           CP (HL)           ;TIME-BEFORE-TIME BEFORE

KBCR:      PUSH AF
           CALL KYVL         ;GET A=CHAR, USING D AND E
           LD C,A
           LD HL,REPCT
           POP AF
           JR NZ,KBDI        ;JR IF NOT SAME KEY AS LAST TIME, OR TIME BEFORE

           DEC (HL)
           RET NZ            ;RET IF NOT TIME TO REPEAT KEY

           PUSH HL           ;ELSE CHECK IT IS STILL HELD DOWN (NOT SOME OTHER
           LD DE,KBUFF+8     ;KEY)
           LD HL,(LKPB)
           LD A,H            ;A=1 FOR PORT FE, 9 FOR FF
           ADD A,E
           LD E,A
           LD A,(DE)         ;A=BYTE FOR DESIRED PORT FROM KBUFF BIT MAP
                             ;'CURRENT' DATA
                             ;L=1 FOR BIT 7, 8 FOR BIT 0
KBXL:      RLCA
           DEC L
           JR NZ,KBXL        ;GET DESIRED KEY BIT TO CARRY

           POP HL
           RET C             ;NO AUTO-REPEAT IF KEY NO LONGER HELD DOWN

           INC (HL)
           LD A,(FLAGS)
           AND &20
           RET NZ            ;RET IF STILL KEYS IN BUFFER (DO NOT ACCUMULATE
                             ;AUTO-REPEATING KEYS)

           LD A,(REPPER)     ;RELOAD REPCT FROM REPPER (DELAY BETWEEN REPEATS)
           JR KBRK

KBDI:      LD A,(REPDEL)

KBRK:      LD (HL),A         ;NEW KEY - SET UP REPCT FOR INITIAL DELAY BEFORE
                             ;AUTO-REPEAT

           LD HL,(KBQP)      ;L=KEY BUFFER QUEUE END (POSN TO PLACE CHAR AT)
                             ;H=QUEUE HEAD (PTS TO NEXT CHAR TO BE READ)
                             ;BOTH ARE DISPLACEMENTS FROM KBQB (BUFFER START)
                             ;IF H=L, BUFFER IS EMPTY. IF *NEW* POSN OF H=L,
                             ;BUFFER IS FULL
           LD A,L
           INC A
           AND &07
           CP H
           RET Z             ;RET IF BUFFER FULL (WRAPPED SO END=HEAD)

           LD (KBQP),A
           LD H,0
           LD DE,KBQB
           ADD HL,DE         ;PT TO END
           LD (HL),C         ;PLACE CHAR IN BUFFER
           LD A,(LASTKV)

LDLH:      LD HL,NLASTH
           LD C,(HL)
           LD (HL),A
           INC HL
           LD B,(HL)
           LD (HL),C
           INC HL
           LD (HL),B
           RET

;GET KEY CODE FROM MAP USING E AND D (SHIFT)

KYVL:      LD HL,(KBTAB)
           LD A,D            ;00 IF NO SHIFT, OR 1/2/3 FOR CAPS/SYM/CNTRL
           LD D,0
           ADD HL,DE
           AND A
           JR Z,KINT4       ;JR IF NO SHIFT USED

           LD E,70

KYVLP:     ADD HL,DE         ;PT TO CAPS SHIFT/SYM/CNTRL TABLE
           DEC A
           JR NZ,KYVLP

KINT4:     LD A,(FLAGS2)
           AND &08           ;Z=CAPS LOCK OFF
           LD A,(HL)
           RET Z

           CALL ALPHA
           RET NC            ;RET IF NOT A LETTER

           AND &DF           ;FORCE LOWER CASE LETTERS TO UPPER CASE
           RET

;KEYSCAN.
;ACTION: LOOK FOR CHANGED BITS IN KEYBOARD STATE SINCE LAST SCAN, SO THAT
;SEVERAL KEYS CAN BE PRESSED DOWN AND A NEW KEY WILL STILL BE NOTICED. IGNORE
;KEY RELEASES. IF THERE HAVE BEEN NO NEW PRESSES, RETURN THE SAME KEYSCAN
;CODES AS LAST TIME, UNLESS ALL KEYS ARE RELEASED - THEN RETURN NULL CODE.

;ENTRY: NO CONDITIONS
;EXIT: IF NZ, NO KEY PRESSED AND DE=FFFF
;      IF Z, E=KEY VALUE
;            IF D=0, NO SHIFT, ELSE D=1 (CAPS SH.) 2 (SYM. SH.) OR 3 (CONTROL)

;ENTER AT KEYSCAN+3 WITH HL=DIFFERENT 18-BYTE KBUFF IF DESIRED

;USED BY INKEY$ - 2 SCANS IN CASE OF INTERRUPT

TWOKSC:    CALL KEYSCAN
           RET Z

KEYSCAN:   LD HL,KBUFF       ;18-BYTE STORE
           PUSH HL
           LD BC,&FE00+KEYPORT
                             ;C HAS PORT FOR BITS 4-0, B STARTS
                             ;WITH A8 LOW FOR 1ST KEY ROW
           LD D,&E0          ;MASK TO KEEP BITS 7-5 OF A, USE 4-0 OF E

KBSL:      IN E,(C)          ;READ BITS 4-0
           LD A,B            ;A WILL BE ON HI ADDR LINES DURING PORT READ
           IN A,(STATPORT)   ;READ BITS 7-5
           XOR E
           AND D
           XOR E
           INC B
           JR Z,KBEL         ;JR IF WE JUST DID 'SPECIAL' PORT FFFEH

           LD (HL),A
           DEC B
           INC HL
           RLC B             ;NEXT PORT (FEFE, FDFE, FBFE...7FFE)
           JR C,KBSL         ;LOOP UNTIL BACK TO PORT FEFE AGAIN

           LD B,&FF
           JR KBSL

KBEL:      OR &E1            ;ONLY BITS 4-0 VALID FOR PORT FFFE, AND FORCE
           LD (HL),A         ;BIT 0 (CONTROL KEY BIT) HI TOO
           POP HL            ;KBUFF
           LD D,H
           LD E,L
           SET 0,(HL)        ;'NO CAPS SHIFT' (SCAN SEPARATELY FOR IT)
           INC HL
           INC HL
           INC HL
           SET 5,(HL)        ;'NO ESC'
           INC HL
           INC HL
           INC HL
           INC HL
           SET 1,(HL)        ;'NO SYM. SHIFT' (SCAN SEPARATELY FOR IT)
           INC HL
           INC HL            ;PT TO LAST SCAN DATA (BYTES 10-18)
           LD B,9            ;(HL=KBUFF+9, DE=KBUFF)

KBCL:      LD A,(DE)         ;THIS SCAN DATA
           LD C,(HL)         ;LAST SCAN DATA
           LD (HL),A         ;LAST SCAN DATA UPDATED WITH NEW DATA - BUT WE
                             ;HAVE IT IN C NOW
           XOR C
           CPL               ;GET ALTERED BITS SINCE LAST SCAN AS 0'S
           OR (HL)           ;KEEP AS 0'S IF CHANGE WAS 1->0 (PRESSED)
           LD (DE),A         ;'CHANGED' DATA TO KBUFF
           INC HL
           INC DE
           DJNZ KBCL

           LD B,9

KBDL:      DEC DE
           LD A,(DE)
           INC A
           JR NZ,KBYK        ;JR IF ANY BIT RESET

           DJNZ KBDL

;NO NEW PRESSES SINCE LAST SCAN. HL=KBUFF+18

           LD B,9
           LD DE,(LASTKV)    ;E=LAST VALUE

KBAKL:     DEC HL
           LD A,(HL)
           INC A
           JR NZ,KBSH        ;JR IF KBUFF SHOWS ANY KEY PRESSED APART FROM SHIFT
                             ;USE LAST VALUE, PLUS CURRENT SHIFT STATUS
           DJNZ KBAKL

           INC B             ;NZ
           RET

KBYK:      DEC A
           LD C,9

KBBL:      DEC C
           RRA
           JR C,KBBL         ;CHANGE BIT POSN TO NUMBER IN C (1-8)

           LD (LKPB),BC      ;LAST KEY PORT/BIT SAVED FOR AUTO-REPEAT CHECKING
                             ;B=1 IF LAST PORT = FEFE, 9 IF FFFE. C=8 FOR BIT
                             ;0, 1 FOR BIT 7
           LD A,C
           ADD A,A           ;*2
           ADD A,A           ;*4
           ADD A,A           ;*8
           ADD A,C           ;*9 (09-48H)
           SUB B             ;SUB 1-9 TO GET 00-47H
           LD E,A            ;SCAN CODE TO E
           LD HL,SOFFCT
           XOR A
           LD (HL),A         ;ZERO SCREEN OFF COUNTER - KEYBOARD USED
           INC HL
           ADD A,(HL)        ;SOFLG
           JR Z,KBSH         ;JR IF SCREEN NOT TURNED OFF BY NO KEY USE

           XOR A
           LD (HL),A         ;'ON'
           LD A,(BORDCOL)
           AND &7F
           LD (BORDCOL),A
           OUT (KEYPORT),A

KBSH:      LD BC,&FEFE
           IN A,(C)
           LD D,1
           AND D
           JR Z,KBLD         ;JR IF CAPS SHIFT - D=1
                             ;ELSE D=1
           INC B
           IN A,(C)
           AND D
           LD D,3
           JR Z,KBLD         ;JR IF CONTROL - D=3

           DEC D
           LD B,&7F
           IN A,(C)
           AND D
           JR Z,KBLD         ;JR IF SYM SHIFT - D=2

           DEC D
           DEC D             ;Z=KEY OBTAINED. D=0 FOR NO SHIFT

KBLD:      LD (LASTKV),DE
           RET
                                  ;KEYRD2
