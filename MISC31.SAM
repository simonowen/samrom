;MISC31.SAM -  CALL, BGRAPHICS, KEY, STRM16, NEW


;'CALL' CMD

CALLER:    CALL EXPT1NUM     ;GET ADDR
           LD C,0            ;PARAMS COUNTER TO ZERO

FCALERLP:  CALL RCRC         ;RST 18H, CRCOLON
           JR Z,FCALERCE     ;EXIT LOOP IF CR OR ':'

           CALL INSISCOMA
           PUSH BC
           CALL EXPTEXPR     ;Z=STR, NZ=NUM, CY IF RUNNING. (BIT 7,A GAVE Z/NZ)
           POP BC
           INC C
           JR NC,FCALERLP

   ;        RLA               ;NC=STR, CY=NUMB
    ;       SBC A,A           ;STR=00, NUM=FF BUG**A IS CHAR BY THIS TIME
     ;      INC A             ;STR=01, NUM=00
           LD A,(FLAGS)
           LD B,A            ;NOW STACK EXPR AND TYPE UNDER ADDR
                             ;C IS SAVED

           DB CALC           ;ADDR,EXPR
           DB SWOP           ;EXPR,ADDR
           DB STKBREG        ;EXPR,ADDR,TYPE
           DB SWOP           ;EXPR,TYPE,ADDR
           DB EXIT

           JR FCALERLP

FCALERCE:  LD A,C
           LD (TEMPB3),A
           CALL ABORTER

           CALL R1OFFJP
           DW CALLX


;CREATE ERROR, STAT, LINO

SETUPVARS: LD A,(ERRNR)
           LD DE,ERVT        ;PTR TO NAME
           CALL CRTVAR2
           LD HL,(PPC)
           LD DE,ERVT+6
           CALL CRTVAR3
           LD A,(SUBPPC)
           LD DE,ERVT+11

CRTVAR2:   LD H,0
           LD L,A

CRTVAR3:   PUSH DE           ;PTR
           CALL STACKHL
           POP HL
           LD DE,TLBYTE
           LD A,(HL)         ;TLBYTE FOR SEARCH
           LD BC,6           ;ENOUGH FOR 'ERROR' VAR
           LDIR
           CALL R1OFFJP
           DW CRTVAR4


ERVT:      DB 4
           DM "error"
           DB 3
           DM "lino"
           DB 3
           DM "stat"


MNINIT:    XOR A
           LD I,A
           IM 1

RMPS:      LD BC,CLUTPORT
           LD D,A
           ADD A,A
           ADD A,A
           ADD A,A
           OUT (C),A         ;SET COLOUR 0
           LD A,D
           OUT (URPORT),A
           LD HL,&8000
           LD DE,&8001
           LD BC,&3FFF
           LD (HL),L
           LDIR              ;CLEAR A PAGE
           LD HL,&8000
           INC B             ;BC=0100
           LD E,&40

RMCK:      LD (HL),&FF
           LD D,(HL)
           INC D
           JR NZ,RAMEX       ;CHECK ALL BITS CAN BE ONES

           LD (HL),D         ;CHECK CAN BE ZEROED
           LD D,(HL)
           INC D
           DEC D
           JR NZ,RAMEX       ;CHECK HOLDS ONE

           ADD HL,BC
           DEC E
           JR NZ,RMCK

           INC A
           CP &20
           JR C,RMPS

           LD A,&FE
           IN A,(&FE)
           RRA
           LD A,&10          ;256K
           JR NC,RAMEX       ;FORCE 256K SYSTEM IF SHIFT PRESSED

           ADD A,A           ;20H=512K

RAMEX:     LD B,A
           DEC A
           LD E,A
           LD (PRAMTP),A     ;PHYSICAL RAMTOP PAGE (USUALLY 0F OR 1F)
           OUT (VIDPORT),A   ;ANY NZ VAL STOPS SYS VARS APPEARING AS ATTR
           LD SP,ISPVAL
           LD HL,ALLOCT
           PUSH HL

ATIF:      LD (HL),0         ;'FREE'
           INC HL
           DJNZ ATIF

           SUB &21
           CPL               ;IF PRAMTP WAS 0F, A=11H; IF 1F, A=01
           LD B,A

ATIX:      LD (HL),&FF       ;'NON-EXISTENT/TERMINATOR'
           INC HL
           DJNZ ATIX         ;FILL REST OF TABLE WITH FF'S

           POP HL
           LD A,&40          ;'IN USE, CONTEXT 0'
           LD B,4            ;PAGES TO RESERVE

ATIU:      LD (HL),A         ;'USED BY CONTEXT A'
           INC HL
           DJNZ ATIU

           LD A,L
           DEC A
           LD (LASTPAGE),A   ;LAST PAGE USED BY BASIC
           LD (RAMTOPP),A    ;RAMTOP PAGE

           LD L,E
           DEC L             ;HL PTS TO SCREEN PAGES IN ALLOCT
           LD A,L
           OR &60            ;SCREEN PAGE=LAST POSSIBLE PAIR, MODE=3, MIDI
                             ;BIT LOW (INACTIVE)
           LD (FISCRNP),A
           LD A,&C0          ;'SCREEN'
           LD (HL),A
           INC HL
           LD (HL),A
           LD HL,&BFFF
           LD (RAMTOP),HL

           LD HL,KSRC
           LD DE,KTAB+1
           LD C,70*3
           LDIR              ;INIT MAIN KEYBOARD TABLES

           EX DE,HL          ;DE PTS TO TABLE OF VALUES AND DISPS TO NEXT
                             ;DEST, ENDING IN ZERO
           LD HL,KTAB+238    ;DEST FOR BRIGHT CC
           CALL PBSL         ;B SHOULD BE ZERO ON ENTRY. EXITS UNCHANGED

           LD HL,&5CB6+31+&4000
           LD (PROG),HL

           LD DE,&5CB6
           LD (CHANS),DE
           LD HL,CHANTAB
           LD C,31
           LDIR              ;INIT 6 CHANNELS

           LD HL,DKSRC
           LD DE,DKBU
           LD C,DKEN-DKSRC+1
           LDIR              ;INIT DEF KEYS

   ;       LD HL,CHIT
           LD DE,LNCUR
           LD C,18
           LDIR              ;INIT FIRST 18 BYTES OF SYS VARS

      ;    LD HL,MAIT
           LD DE,BASSTK
           LD C,26
           LDIR              ;13 IMPORTANT ADDRESSES INITED

           LD HL,NMISTOP
           LD (NMIV),HL

           LD HL,ANYI
           LD (ANYIV),HL

           CALL UPACK        ;UNPACK CHAR SET
           LD HL,CHARSVAL-256
           LD (CHARS),HL
           LD HL,CHARSVAL+896
           LD (UDG),HL
           JR NEW2

NEW:       CALL CHKEND
           DI

NEW2:      LD HL,SCLIST
           LD (SCPTR),HL
           DEC HL            ;PT TO FISCRNP
           LD A,(HL)
           LD (CUSCRNP),A
           OUT (VIDPORT),A
           LD B,16

SCLI:      INC HL
           LD (HL),A
           LD A,&FF          ;'CLOSED'
           DJNZ SCLI

           LD A,(PRAMTP)
           DEC A
           DEC A             ;SKIP PAST SCREEN 1
           LD L,A
           LD H,ALLOCT/256

CSPL:      LD A,(HL)
           CP &C0
           JR NZ,DCSP        ;JR IF NOT A SCREEN PAGE

           LD (HL),B         ;CLOSE SCREEN PAGE

DCSP:      DEC L
           JR NZ,CSPL

           LD C,L
           LD HL,STREAMS-4
           LD DE,STRMTAB     ;MUST BE IN ROM0!
           LD B,9            ;INITIALISE 9 STREAMS

STRIL:     LD A,(DE)
           INC DE
           LD (HL),A
           INC HL
           LD (HL),C
           INC HL
           DJNZ STRIL

           LD B,24           ;12 MORE STREAM PTRS TO ZAP

CLSTL:     LD (HL),C
           INC HL
           DJNZ CLSTL

           CALL ADDRPROG
           LD (HL),&FF       ;PROGRAM TERMINATOR
           INC HL
           LD (NVARS),HL
           LD (NVARSP),A
           LD (ELINEP),A
           CALL RESTOREZ
           LD HL,&0321
           LD (REPDEL),HL    ;REPDEL=33, REPPER=3
           LD HL,MEMVAL
           LD (MEM),HL
           LD A,H
           LD (RASP),A
           LD (SPEEDIC),A    ;ANY NON-ZERO VALUE WILL DO
           LD (THFATT),A     ;TEMP FAT
           XOR A
           LD (THFATP),A     ;PERM THIN
           LD (MODE),A       ;NOT MODE 3 SO XRG NOT HALVED
           CALL CLRSR        ;CLEAR FPCS, BASIC STACK, NVARS, SAVARS
           LD HL,(SAVARS)
           INC HL
           LD (ELINE),HL
           CALL SETMIN
           LD A,3
           CALL MODET        ;SET MODE, CSIZE, WINDOWS
           CALL CLSHS2
           LD SP,ISPVAL
           LD HL,MAINER
           PUSH HL
           LD (ERRSP),SP

;RAINBOW SCREEN

           LD DE,PALTAB+1
           LD HL,LINICOLS    ;L=0
           LD B,L            ;INIT SCAN NUMBER IN B
           LD C,L

RBOWL:     LD (HL),B
           INC HL
           LD (HL),C         ;PAL MEM ZERO
           INC HL
           LD A,(DE)
           INC DE
           LD (HL),A
           INC HL
           LD (HL),A         ;ALT COLOUR=MAIN
           INC HL
           LD A,B
           ADD A,11
           LD B,A            ;NEXT SCAN TO ALTER AT
           CP 166
           JR C,RBOWL

           LD (HL),&FF

           RST &08
           DB &50            ;COPYRIGHT MSG


;UPACK - CALLED BY INITIALIZATION ROUTINE
;UPACK CHARSET FROM 5-BIT TO 8-BIT WITH 2 ZEROS AT LHS, 1 AT RHS. MAKE BOTTOM
;SCAN ZERO EXCEPT IN 13 CASES DEALT WITH SEPARATELY.
;CHAR DEFS. THIS ROUTINE TAKES 101 BYTES, WHEREAS A NORMAL LDIR WOULD TAKE 11.
;NET GAIN: 402 BYTES (NOTE:OUT OF DATE NOW FOREIGN SET DROPPED!)

UPACK:     LD HL,99*256+7    ;H COUNTS 99 CHARS, L COUNTS 7 BYTES/CHAR
           EXX
           LD HL,CHARSRC
           LD DE,CHARSVAL
           XOR A
           LD B,5            ;5 BITS TO UNPACK

UPKGC:     LD C,(HL)
           INC HL
           SCF               ;AFTER FIRST PASS, C ROTATES IN NC. WHEN C=0, LAST
                             ;VALID DATA BIT HAS GONE, CY=JUNK (SET)
UPKBL:     RL C
           JR Z,UPKGC        ;GET NEXT DATA BYTE IF C USED UP

           RLA
           DJNZ UPKBL        ;GET 5 BITS IN 'A' REG

           RLCA              ;CENTRE DATA.
           LD (DE),A
           INC DE
           XOR A             ;NC, A=0
           LD B,5            ;GET 5 BITS
           EXX
           DEC L
           EXX
           JR NZ,UPKBL       ;LOOP FOR 7 BYTES/CHAR

           LD (DE),A         ;8TH BYTE IS ZERO
           INC DE

           EXX
           LD L,7
           DEC H
           EXX
           JR NZ,UPKBL       ;LOOP FOR 137 CHARS

           LD HL,95*8+CHARSVAL+1  ;PT TO SECOND BYTE OF COPYRIGHT SIGN
                                   ;(FIRST OF 5 WHICH NEED BIT 6 SET)
SB5L:      SET 6,(HL)
           INC HL
           DJNZ SB5L               ;DO 5 BYTES

;NOW PATCH CHARS WHERE BOTTOM SCAN IS USED

           LD DE,U8TAB
           LD HL,CHARSVAL+103 ;SCAN 7 OF COMMA

;CALLED TO UNPACK CONTROL KEY VALUES

PBSL:      LD A,(DE)         ;BYTE TO PLACE
           INC DE
           LD (HL),A         ;TO DEST
           LD A,(DE)         ;DISP TO NEXT DEST, OR 0
           INC DE
           LD C,A            ;(B=0)
           ADD HL,BC         ;PT HL TO NEXT DEST
           AND A
           JR NZ,PBSL        ;LOOP TILL TERMINATOR HIT

           RET

;CLS #

CLSHS:     CALL SABORTER

;CALLED BY NEW

CLSHS2:    CALL STREAMFE     ;'S'
           LD BC,&0510

CLHSL:     LD A,B
           ADD A,C
           RST &10           ;PRINT CHR$ 15H/14H/13H/12H/11H
           XOR A
           RST &10           ;OVER/INVERSE/BRIGHT/FLASH/PAPER 0
           DJNZ CLHSL

           LD A,C
           RST &10           ;CHR$ 10H - PEN
           LD A,7
           RST &10           ;PEN 7
           CALL PER3         ;COPY TEMP EFFECTS OF COLOUR CODES TO PERMS.
 ;         CALL ZBCI
;          JP MCLS

;ZBCI:      XOR A
           XOR A
           CALL SETBORD
           CALL COLINIT
           JP MCLS

;COLOUR.SAM
;E.G. PALETTE I,C
;     PALETTE I,B,C
;     PALETTE I,C LINE L or COLOUR I,B,C LINE L
;     PALETTE I LINE L=DELETE CHANGE OF I AT LINE L
;     PALETTE   CLEARS ALL INTERRUPT CHANGES, RE-INITS PAL MEMORIES

;I=LOGICAL INK NUMBER (PALETTE MEMORY) 0-15.
;C IS A COLOUR NUMBER FROM 0-127. B AND C ARE 2 SUCH COLOURS TO ALTERNATE UNDER
;INTERRUPT CONTROL. VALUES ARE SIMPLY ENTERED INTO TWO TABLES USED BY THE
;FRAME INTERRUPT TO SET UP THE PALETTE 50 TIMES/SEC. IF TABLES MATCH, NO
;INKS FLASH, OTHERWISE FLASH HAPPENS AFTER (SPEEDINK) INTS.
;IF 'LINE L' IS USED, THE COLOUR(S) WILL ONLY BE SET FROM START OF SPECIFIED
;SCAN LINE (ACTUALLY RHS BORDER OF PRECEDING LINE). LINE CAN BE 174 TO -16.
;=SCANS 1-191. GIVES INT AT SCANS 0-190 (ACTUALLY END OF PRECEDING LINE), AND
;COLOUR CHANGE AT SCANS 1-191
;UP TO 127 CHANGES CAN BE MADE PER SCREEN

COLOUR:      CALL CRCOLON
             JR NZ,COLOUR1             ;JR UNLESS JUST 'PALETTE'

             CALL CHKEND

COLINIT:     LD A,&FF
             LD (LINICOLS),A          ;CLEAR INTERRUPT COL CHNG LIST
             LD DE,PALTAB
             CALL COLINIT1            ;DO IT TWICE
             CALL COLINIT1
             LD A,(MODE)
             CP 2
             RET NZ

PALSW:     LD DE,PALTAB      ;IF CHANGING TO MODE 2, SAVE NON-MODE-2 PALTAB 0-3
           LD HL,PALTAB+16   ;TO STORE, AND GET BACK MODE 2 PALTAB 0-3
           CALL PL4S         ;SWOP MAIN COLOURS. EXIT WITH HL=PALTAB+20
           LD DE,PALTAB+36

PL4S:      LD B,4
           JP FPSWOPLP     ;SWOP ALTERNATE COLOURS

COLINIT1:    LD BC,20
             LD HL,INITCOLS
             LDIR
             RET

COLOUR1:     CALL EXPT1NUM             ;I
             CP LINETOK
             JR NZ,COLOUR5

             CALL SSYNTAX6             ;L OF 'PALETTE I LINE L'

             CALL COLATSR
             PUSH AF                   ;LINE
             LD DE,&1000+23            ;LIMIT OF 16, INKVALERR
             CALL LIMBYTE              ;I
             POP DE                    ;D=LINE
             LD E,A                    ;E=I
             LD HL,LINICOLS

COLDELP:     LD A,(HL)
             INC HL
             CP D
             JR Z,COLDEL2              ;EXIT IF FOUND RIGHT SCAN ENTRY

             RET NC                    ;RET IF PAST ANY ENTRIES FOR LINE D

COLDEL1:     INC HL
             INC HL
             INC HL
             JR COLDELP

COLDEL2:     LD A,(HL)
             CP E
             JR NZ,COLDEL1             ;JR IF WRONG I

             LD D,H                    ;ELSE DELETE ENTRY
             LD E,L
             DEC DE                    ;DE PTS TO LINE VALUE
             INC HL
             INC HL
             INC HL                    ;HL PTS TO LINE VALUE OF NEXT ENTRY
             PUSH HL
             CALL FLITD                ;GET LEN TO TABLE END
             POP HL
             DI
             LDIR                      ;DELETE ENTRY. IF LINIPTR PTS BEFORE
             EI                        ;DELETED ENTRY, NO PROBLEM. IF PTS TO
             RET                       ;

INKVALERR:   RST &08
             DB 23                     ;'Invalid colour'

COLOUR5:     CALL EXPTCNUM             ;,C OF 'PALETTE I,C....'
             CP ","
             JR Z,COLOURFL             ;GET THIRD PARAM IF THERE IS ONE

             CP LINETOK
             JR NZ,COLSING             ;JR IF 'PALETTE I,C'

             CALL SSYNTAX6             ;LINE OF 'PALETTE I,C LINE L

             CALL COLATSR
             JR COLOUR10

COLSING:     CALL CHKEND

             LD A,&FF

COLOUR10:    PUSH AF                   ;NUL LINE
             LD DE,&8000+24            ;LIMIT TO <128
             CALL LIMBYTE              ;GET SINGLE COLOUR
             PUSH AF                   ;STACK IT AS THE 'SECOND' COLOUR TOO
             JR COLOUR2                ;FPCS=I, (SP)=COL, (SP+2)=LINE, A=COL

COLOURFL:    CALL SEXPT1NUM            ;SECOND COLOUR
             CP LINETOK
             JR Z,COLATL

             CALL CHKEND

             LD A,&FF                  ;NUL LINE
             PUSH AF
             JR COLOUR15               ;FPCS=I,B,C. (SP)=LINE

COLATL:      CALL SSYNTAX6             ;LINE

COLATL2:     CALL COLATSR
             PUSH AF                   ;LINE

COLOUR15:    LD DE,&8000+24            ;LIMIT TO <128
             CALL LIMBYTE              ;GET SECOND COLOUR TO A

             PUSH AF                   ;SECOND COLOUR
             CALL LIMBYTE              ;FIRST COLOUR

COLOUR2:     PUSH AF                   ;FIRST COLOUR
             LD DE,&1000+23            ;LIMIT TO <16
             CALL LIMBYTE              ;PALETTE ENTRY (I)

             LD E,A                    ;E=I
             POP BC                    ;B=FIRST COLOUR
             POP AF
             LD C,A                    ;C=SECOND
             POP AF                    ;LINE

;A=LINE (FF IF NONE), LETTE ENTRY

JPALET:      CP &FF
             JR NZ,COLRLINE

             LD HL,PALTAB              ;PT TO PALETTE TABLE
             LD D,0
             ADD HL,DE
             LD (HL),B                 ;FIRST COLOUR
             LD E,20
             ADD HL,DE                 ;POINT TO ALTERNATE PALETTE TABLE
             LD (HL),C                 ;SECOND COLOUR (OFTEN THE SAME)
             RET

COLFULERR:   RST &08
             DB 25                     ;'Too many palette changes'

COLRLINE:    LD HL,LINICOLS
             LD D,A                    ;D=LINE

COLRLP:      LD A,(HL)
             INC HL
             CP D
             JR NC,COLRL2              ;JR IF WE FOUND END (FF) OR LINE>=L

COLRLP2:     INC HL
             INC HL
             INC HL
             JR COLRLP

COLRL2:      JR NZ,COLRL3              ;JR IF LINE HAS NO ENTRIES ALREADY

             LD A,(HL)
             CP E                      ;CP I
             JR NZ,COLRLP2             ;JR IF LINE DOES NOT CHANGE THIS I
                                       ;ELSE OVERWRITE ENTRY

             DI                        ;(IN CASE INTERRUPT COLOUR SWOP OCCURS)
             JR LD2COL

;WE NEED TO OPEN 4 BYTES AT (HL-1) - MIGHT BE TERMINATOR POSN

COLRL3:      DEC HL
             PUSH BC                   ;COLOURS
             PUSH DE                   ;LINE, I
             PUSH HL                   ;LINE OR TERMINATOR POSN
             CALL FLITE                ;GET HL=TABLE END, BC=LEN
             INC BC
             INC BC
             INC BC
             INC BC
             LD A,B
             ADD A,&FE
             JR C,COLFULERR            ;ERROR IF LIST WITH NEW ENTRY WOULD BE
                                       ;MORE THAN 01FFH LONG
             POP DE                    ;LOCN FOR NEW ENTRY
             PUSH HL
             SBC HL,DE                 ;GET TERMINATOR-LOCN
             LD B,H
             LD C,L
             INC BC                    ;ALLOW FOR INCLUSIVE BYTE=LEN TO MOVE
             POP DE                    ;TERMINATOR
             LD HL,4
             ADD HL,DE
             EX DE,HL                  ;DE=TERMINATOR+4, HL=TERMINATOR
             DI
             LDDR
             INC HL                    ;PT TO 4 BYTE SPACE
             POP DE
             POP BC
             LD (HL),D                 ;LINE
             INC HL
             LD (HL),E                 ;I

LD2COL:      INC HL
             LD (HL),B
             INC HL
             LD (HL),C
             EI
             RET



;FIND LINE INT TABLE END. EXIT WITH BC=TABLE LEN, HL=END.

FLITE:       LD HL,LINICOLS

;FIND LINE INT TABLE DISP. EXIT WITH BC=DISP TO TERMINATOR FROM ENTRY HL,+1.

FLITD:       LD BC,1

FLITL:       LD A,(HL)
             INC A
             RET Z

             INC HL
             INC HL
             INC HL
             INC HL
             INC BC
             INC BC
             INC BC
             INC BC
             JR FLITL

;GET LINE FROM TOP OF FPCS AS 0-191 FROM BASIC'S 175 TO -16

COLATSR:     DB CALC
             DB DUP                    ;L,L SO COORDFID CAN BE USED
             DB EXIT

             CALL COORDFID
             CALL USYCOORD             ;GETS Y AS 0 TO 191 (175 TO -16)
             PUSH AF                   ;LINE
             CALL FDELETE              ;JUNK DUMMY X
             POP AF                    ;A=LINE
             SUB 1
             RET NC                    ;RET UNLESS A WAS ZERO

             RST &08
             DB 30                     ;IOOR
                                       ;COLOUR CHANGE AT LINE 175 NOT POS.
                                       ;0-190 FOR ORIG 174 TO -16
                                       ;EG 1 GIVES INT AT END OF SCAN 0, COLOUR
                                       ;CHANGE AT END OF SCAN 1.

                                 ;RECORD, FATPIX, CSIZE, WINDOW,
