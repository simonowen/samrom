;ENDPRINT.SAM - FINAL SCREEN OUTPUT ROUTINES FOR MODES 0-3 PRINTABLE CHARS.
;ENTRY: DE=SCREEN ROW/COL, HL PTS TO CHAR DATA, B=OVER AND C=INVERSE MASK

;CALLED FROM ROM1

EPSUB:     CALL R1OSR
           CALL SELSCRN
           CALL EPSSR

POPOUT:    POP AF
           OUT (250),A

PPORT:     POP AF
           OUT (251),A
           RET

R1OSR:     POP IY
           IN A,(251)
           PUSH AF
           IN A,(250)
           PUSH AF
           AND &BF
           OUT (250),A       ;ROM1 OFF
           JP (IY)

;USED BY RENUM

GTRLNN:    EX DE,HL
           INC HL
           IN A,(251)
           PUSH AF
           CALL FNDLINE
           LD   D,(HL)
           INC  HL
           LD   E,(HL)       ;GET LINE NUMBER
           LD HL,(FIRST)
           JR PPORT

;COPY STRING FROM COMMON MEMORY TO WORKSPACE, STACK PARAMS ON FPCS.
;MOVE BC BYTES FROM (HL) TO WKSPACE. USED BY E.G. HEX$, CHR$, STR$.
;FOR STRINGS LEN 1-16K. EXIT: DE=STKEND, PAGING UNCHANGED.

CWKSTK:    CALL R1OSR
           PUSH HL
           CALL WKROOM       ;DE=DEST, BC UNCHANGED
           POP HL
           PUSH DE
           PUSH BC
           LDIR
           POP BC
           POP DE
           CALL STKSTOREP
           EX DE,HL          ;DE=STKEND
           JR POPOUT


EPSSR:     LD A,(CSIZE)      ;HEIGHT **
           PUSH HL
           EXX
           POP DE            ;DE'=DATA PTR IF MODE 3/4
           CP 8
           JR C,HPL2

           LD A,8

HPL2:      LD B,A
           EXX               ;** BUG FIX - LIMIT O/P SCANS TO 8
           LD A,(MODE)
           CP 2
           JR Z,M2PRINT

           JP NC,M3PRINT

           DEC A
           JR Z,M1PRINT

;*******************************************************************************
;MODE 0 PRINT. ENTRY: DE=SCREEN ROW/COL, HL=CHAR DATA, B=OVER AND C=INVERSE MASK

M0PRINT:   CALL M0DEADDR     ;GET DE=SCREEN ADDR
           PUSH DE
           EXX

M0PRLP:    EXX               ;SCAN COUNTER IN B'
           LD A,(DE)         ;SCREEN DATA
           AND B             ;OVER MASK
           XOR (HL)          ;XOR SCRN
           XOR C             ;XOR INVERSE MASK
           LD (DE),A
           INC HL
           INC D
           LD A,D
           AND &07
           JR NZ,M0PRNT2      ;JR IF NOT CROSSING CHAR BOUNDARY

           EX DE,HL
           CALL NXTDOWN1
           EX DE,HL

M0PRNT2:   EXX
           DJNZ M0PRLP

           POP HL
           CP A
           JP POATTR0

;*******************************************************************************
;MODE 1 PRINT. ENTRY: DE=SCREEN ROW/COL, HL=CHAR DATA, B=OVER AND C=INVERSE MASK

M1PRINT:   CALL M1DEADDR     ;GET ADDR OF ROW/COL DE IN DE
           EXX

M1PRLP:    EXX
           LD A,(DE)         ;FETCH SCREEN DATA
           AND B             ;AND 0 IF OVER 0, AND FF IF OVER 1 OR 2
           XOR (HL)          ;XOR CHAR DATA
           XOR C             ;INVERSE MASK USED
           LD (DE),A         ;PLACE ON SCREEN
           INC HL            ;NEXT CHAR DATUM
           LD A,E
           ADD A,32
           LD E,A            ;DROP DE BY 1 SCAN.

           JR NC,M1PRNC

           INC D

M1PRNC:    EXX
           DJNZ M1PRLP       ;LOOP FOR 8 SCANS

           EXX
           LD HL,&1F00       ;DISP TO ATTR AREA (TOP ROW)
           ADD HL,DE
           CALL SETATTR      ;USE ATTRT TO CHANGE ATTR AT (HL). HL PRESERVED, A=
                             ;NEW ATTR
           LD B,7            ;7 MORE TO CHANGE
           LD DE,32

M1PRATTR:  ADD HL,DE         ;DROP TO ATTR FOR NEXT CHAR ROW
           LD (HL),A
           DJNZ M1PRATTR

           RET

;*******************************************************************************
;MODE 2 PRINT. ENTRY: DE=SCREEN ROW/COL, HL=CHAR DATA, B=OVER AND C=INVERSE MASK

M2PRINT:   CALL M2DEADDR     ;CY SET IF 6-PIX CHARS, Z/NZ=EVEN/ODD
           LD H,CEXTAB/256   ;H IS EXPANSION TABLE MSB
           LD A,C
           EXX
           LD C,A            ;C"=INVERSE MASK
           JR C,PR80COL

           LD HL,MEMVAL      ;IF 64-COL, MUST ROTATE CHAR DATA FOR CORRECT
                             ;ALIGNMENT WHEN USING 85-COL O/P ROUTINE
           PUSH BC           ;**

P64AL:     LD A,(DE)
           RRCA
           LD (HL),A
           INC HL
           INC DE
           DJNZ P64AL

           POP BC            ;**
           LD DE,MEMVAL      ;NEW CHAR LOCN.
           EXX
           LD C,B            ;MAKE RHS OVER MASK=LHS OVER MASK
           JR PR80EVEN

;85-COLUMN PRINT
;ENTRY: NZ IF ODD COLUMN, Z IF EVEN COLUMN.

PR80COL:   EXX
           LD A,B            ;OVER MASK
           JR NZ,PR80ODD

           OR &0F
           LD C,A            ;RHS OVER MASK FOR EVEN COLS=0F OR FF

PR80EVEN:  EXX               ;LHS OVER MASK FOR EVEN COLS=00 OR FF

M2PREVLP:  LD A,(DE)         ;GET CHAR DATA FROM (DE")
           XOR C             ;INVERSE MASK
           INC DE
           EXX

           PUSH AF
           RRCA              ;01234560->00123456
           RRCA              ;        ->60012345
           RRCA              ;        ->56001234
           AND &0F           ;GET VALUE OF HIGHER CHAR NIBBLE
           LD L,A            ;HL PTS TO ENTRY IN 16-BYTE TABLE CONTAINING
                             ;COLOURED EXPANDED DATA
           LD A,(DE)         ;SCREEN DATA
           XOR (HL)
           AND B             ;LHS OVER MASK
           XOR (HL)          ;EXPANDED CHAR DATA
           LD (DE),A

           INC E             ;NEXT SCREEN COLUMN
           POP AF            ;01234560
           RLCA              ;12345600
           AND &0F           ;GET VALUE OF LOWER CHAR NIBBLE.
           LD L,A            ;HL PTS TO ENTRY IN 16-BYTE TABLE CONTAINING
                             ;COLOURED EXPANDED DATA
           LD A,(DE)
           XOR (HL)
           AND C             ;RHS OVER MASK
           XOR (HL)
           LD (DE),A
           LD A,E
           ADD A,127
           LD E,A            ;DROP DE TO NEXT SCAN, BACK UP 1 BYTE
           JR NC,M2PRNCE

           INC D

M2PRNCE:   EXX
           DJNZ M2PREVLP

           RET

PR80ODD:   OR &F0
           LD C,A            ;LHS OVER MASK FOR ODD COLS=F0 OR FF
           EXX               ;RHS OVER MASK FOR ODD COLS=00 OR FF

M2PRODLP:  LD A,(DE)         ;GET CHAR DATA; 01234560
           XOR C             ;INVERSE MASK
           INC DE
           EXX

           PUSH AF
           RLCA              ;12345600
           RLCA              ;23456001
           RLCA              ;34560012
           AND &0F           ;GET VALUE OF HIGHER CHAR NIBBLE.
           LD L,A            ;HL PTS TO ENTRY IN 16-BYTE TABLE CONTAINING
                             ;COLOURED EXPANDED DATA
           LD A,(DE)         ;SCREEN DATA
           XOR (HL)
           AND C             ;LHS OVER MASK
           XOR (HL)          ;EXPANDED CHAR DATA
           LD (DE),A

           INC E             ;NEXT SCREEN COLUMN
           POP AF            ;GET ORIG CHAR DATUM AGAIN
           RRCA              ;00123456
           AND &0F           ;GET VALUE OF LOWER CHAR NIBBLE.
           LD L,A            ;HL PTS TO ENTRY IN 16-BYTE TABLE CONTAINING
                             ;COLOURED EXPANDED DATA
           LD A,(DE)
           XOR (HL)
           AND B             ;RHS OVER MASK
           XOR (HL)
           LD (DE),A
           LD A,E
           ADD A,127
           LD E,A            ;DROP DE TO NEXT SCAN, BACK UP 1 BYTE
           JR NC,M2PRNCO

           INC D

M2PRNCO:   EXX
           DJNZ M2PRODLP

           RET

;*******************************************************************************
;MODE 3 PRINT. ENTRY: DE=SCREEN ROW/COL, HL=CHAR DATA, B=OVER AND C=INVERSE MASK
;USES HL,DE,BC, DE",BC"
;TAKES ABOUT 294*8 Ts vs. POSSIBLE 218*8 FOR OVER 0 ONLY ROUTINE USING LDI

M3PRINT:   CALL M3DEADDR
           LD H,CEXTAB/256   ;H IS EXPANSION TABLE MSB
           LD A,C

           EXX
           LD C,A            ;C"=INVERSE MASK

M3PRLP:    LD A,(DE)         ;GET CHAR DATA
           XOR C             ;INVERSE MASK
           INC DE
           EXX

           LD C,A
           RRA
           RRA
           RRA
           AND &1E           ;GET VALUE OF HIGHER CHAR NIBBLE,*2
           LD L,A            ;HL PTS TO ENTRY IN 16-WORD TABLE CONTAINING
                             ;COLOURED EXPANDED DATA
           LD A,(DE)         ;SCREEN DATA
           AND B             ;OVER MASK
           XOR (HL)          ;EXPANDED CHAR DATA
           LD (DE),A
           INC L             ;NEXT EXPANDED DATUM
           INC E             ;NEXT SCREEN COLUMN

           LD A,(DE)
           AND B             ;OVER MASK
           XOR (HL)
           LD (DE),A
           INC E             ;NEXT SCREEN COLUMN

           LD A,C            ;GET ORIG CHAR DATUM AGAIN

           RLA
           AND &1E           ;GET VALUE OF LOWER CHAR NIBBLE,*2
           LD L,A            ;HL PTS TO ENTRY IN 16-WORD TABLE CONTAINING
                             ;COLOURED EXPANDED DATA
           LD A,(DE)
           AND B             ;OVER MASK
           XOR (HL)
           LD (DE),A
           INC L             ;NEXT EXPANDED DATUM
           INC E             ;NEXT SCREEN COLUMN

           LD A,(DE)
           AND B             ;OVER MASK
           XOR (HL)
           LD (DE),A
           LD A,E
           ADD A,125
           LD E,A            ;DROP DE TO NEXT SCAN, BACK UP 3 BYTES
           JR NC,M3PRNC

           INC D

M3PRNC:    EXX
           DJNZ M3PRLP

           RET


;POFETCH -  GET D=ROW, E=COL, A=RHS LIMIT, CY IF PRINTER OR OTHER

POFETCH:   LD A,(DEVICE)     ;0=UPPER SCREEN, 1=LOWER, 2=PRINTER OR OTHER
           AND A             ;NC, TEST FOR ZERO
           JR Z,POF2         ;JR IF UPPER SCREEN

           LD DE,(SPOSNL)
           DEC A
           JR Z,POF3         ;JR IF LOWER SCREEN

           LD DE,(PRPOSN)
           LD A,(PRRHS)      ;RHS LIMIT (LINE LEN-1)
           SCF               ;"PRINTER"
           RET

POF2:      LD DE,(SPOSNU)

POF3:      LD A,(WINDRHS)
           RET

CCRESTOP:  CALL RESTOP
           RST &30
           DW CCRP2-&8000    ;JP TO ROM1 TO DEAL WITH TAB, AT, PAPER, ETC.

POSTFF:    RST &30
           DW PSTFF2-&8000   ;JP TO ROM1 TO DEAL WITH FN NAME

;UTILITY MESSAGES. ENTRY WITH A=NUMBER

UTMSG:     LD DE,(UMSGS)

;PRINT MSG "A" FROM LIST AT DE

POMSG:     RST &30
           DW POMSPX-&8000

;*******************************************************************************
;ANYDEADDR - GET IN DE ADDR OF ROW D, COL E FOR ANY MODE
;EXIT WITH CY IF MODE 2 6-PIX CHARS, Z/NZ =EVEN/ODD

ANYDEADDR: LD A,(MODE)
           AND A
           JR Z,M0DEADDR

           DEC A
           JR Z,M1DEADDR

           DEC A
           JR Z,M2DEADDR

;GET IN DE ADDR OF ROW D, COL E FOR MODE 3 (8000H+ROW*CSIZEH*80H+COL*4)

M3DEADDR:  CALL CLCPO
           LD A,E
           ADD A,A
           ADD A,A
           ADD A,A           ;0-248
           SCF
           RR D
           RRA
           LD E,A
           RET

;GET IN DE ADDR OF ROW D, COL E FOR MODE 1 (8000H+ROW*CSIZEH*20H+COL)

M1DEADDR:  CALL CLCPO        ;SAY A=256*PIX
           LD A,D
           RRCA              ;128*
           RRCA              ;64*
           RRCA              ;32*
           LD D,A
           XOR E
           AND &E0           ;COMBINE LOWER 3 BITS OF SCAN LINE AND COL
           XOR E
           LD E,A
           LD A,D
           AND &1F           ;UPPER 5 BITS OF SCAN LINE
           OR &80
           LD D,A
           RET

;GET IN DE ADDR OF ROW D, COL E FOR MODE 0

M0DEADDR:  CALL CLCPO
           PUSH BC
           LD B,D
           LD A,E
           ADD A,A
           ADD A,A
           ADD A,A
           LD C,A            ;BC=PIX COORDS
           EX DE,HL
           CALL M0PIXAD
           EX DE,HL
           POP BC
           RET

;GET IN DE ADDR OF ROW D, COL E FOR MODE 2 (8000H+ROW*CSIZEH*80H+COL*2)
;                                      OR  (8000H+ROW*CSIZEH*80H+COL*3/2)
;EXIT WITH NC IF 8-PIX CHARS, OR CY AND Z/NZ=EVEN/ODD COL

M2DEADDR:  CALL CLCPO
           LD A,(FL6OR8)
           AND A
           LD A,E
           JR Z,M2DEADDR2    ;JR IF 6-PIX CHARS

           ADD A,A           ;0-126
           ADD A,A           ;0-252
           SCF
           RR D
           RRA               ;NC=8-PIX CHARS
           LD E,A
           RET

M2DEADDR2: ADD A,A
           ADD A,E           ;A=ORIG COL*3 (0-252)
           SCF
           RR D
           RRA               ;(0-126 - OFFSET FROM LHS)
           BIT 0,E           ;Z IF ORIG COL=EVEN
           LD E,A
           SCF               ;SIGNAL 6-PIX, Z/NZ=EVEN/ODD
           RET

;CALC PIX IN D ROWS, ADD OFFSET IF LOWER SCREEN

CLCPO:     CALL CALCPIXD
           LD D,A
           LD A,(DEVICE)
           AND A
           RET Z             ;RET IF UPPER SCREEN

           LD A,(LSOFF)
           ADD A,D
           LD D,A
           RET

;*******************************************************************************
;ANY MODE PIXEL ADDRESS FOR PT. B,C (OR POINT B,HL IF THIN PIX)
;EXIT: HL=ADDR (8000+) A=X MOD 8
;IF MODE 3, CY IF ODD PIXEL

ANYPIXAD:  LD A,(THFATT)
           AND A
           JR NZ,NTTHINPIX

           LD C,L            ;SAVE ORIG X LSB
           RR H
           RR L              ;HALVE X IF THIN PIX
           DB &FE            ;"JR+1"

NTTHINPIX: LD L,C

           LD H,B
           LD A,(MODE)
           AND A
           JR Z,M0PIXAD

           DEC A
           JR Z,M1PIXAD

           LD A,C
           JR M1PIXAD2

;GET PIXEL ADDR OF POINT C,B IN HL. MODE 0. (Y AXIS HAS ZERO AT TOP)
;ALTERS HL AND A. ADDR=8000-97FF. A=PIX OFFSET

M0PIXAD:   LD L,B
           LD A,B
           OR A
           RRA
           RRA
           SCF
           RRA
           AND &9F
           XOR L
           AND &F8
           XOR L
           LD H,A
           LD A,C
           RLCA
           RLCA
           RLCA
           XOR L
           AND &C7
           XOR L
           RLCA
           RLCA
           LD L,A
           LD A,C
           AND &07
           RET

;GET MODE 1 PIXEL ADDR. ENTRY: L=X, H=Y
;EXIT: HL=ADDR, B=PIXEL OFFSET (0-7), A=B

M1PIXAD:   LD A,L
           AND A
           RR H              ;NC ROTATED IN
           RR L
           AND A
           RR H
           RR L


M1PIXAD2:  AND &07
           LD B,A
           SCF
           RR H
           RR L              ;HL=Y/8+X/8+8000H=ADDRESS
           RET


;POATTR.SAM

;*******************************************************************************
;POATTR01. SET ATTR OF PATTERN DATA AT (HL) FOR MODE 0 OR 1
;ENTRY: HL=SCREEN ADDR. USES HL, BC AND AF

POATTR01:  LD A,(MODE)
           AND A
           LD A,H
           SET 5,A           ;ADD 2000H FOR USE IF MODE 1

;THIS ENTRY CAN BE USED DIRECTLY BY MODE 0

POATTR0:   CALL Z,CTAA

           LD H,A

;ENTRY POINT IF HL ALREADY PTS TO ATTR

SETATTR:   LD BC,(ATTRT)
           LD A,(HL)
           XOR C
           AND B
           XOR C
           LD BC,(PFLAGT)
           BIT 4,C
           JR Z,POATTR1        ;JR IF NOT INK 9

           OR &07
           BIT 5,A
           JR Z,POATTR1

           XOR 7

POATTR1:   BIT 6,C
           JR Z,POATTR2        ;JR IF NOT PAPER 9

           OR &38
           BIT 2,A
           JR Z,POATTR2

           XOR &38

POATTR2:   LD (HL),A
           RET

;COMPARE TWO STRINGS FROM STACK (OVER <16K). PAGING UNALTERED ON RETURN.
;EXIT: Z IF STRINGS MATCH, CY IF S1<S2, NZ,NC IF S1>S2. HL=S1 PTR
;USES HL,BC,AF, HL" DE", BC" AF"

STRCOMP:   CALL R1OSR
           PUSH HL           ;S1 PTR
           CALL UNSTKPRT     ;BC=S2 LEN, DE=S2 ST, A=PORT VALUE
           PUSH DE           ;S2 ST
           PUSH AF           ;S2 PORT VALUE
           PUSH BC           ;S2 LEN
           CALL UNSTKPRT     ;BC=S1 LEN, DE=S1 ST, A=PORT
           POP HL            ;S2 LEN
           AND A
           SBC HL,BC         ;S2 LEN-S1 LEN
           ADD HL,BC
           JR NC,STRCOMP2    ;JR IF BC<=HL

           LD B,H
           LD C,L            ;MAKE BC=SHORTEST LEN

STRCOMP2:  LD L,A            ;L=S1 PORT
           EX AF,AF'         ;SAVE Z IF LENS EQUAL, CY IF S1 LEN GRTR.
           LD A,B
           CP &40
           JP NC,STLERR      ;ONLY DEAL WITH STRINGS<16K

           POP AF
           LD H,A            ;H=S2 PORT
           PUSH BC
           LD C,251

           EXX
           POP BC            ;BC"=SHORTEST LEN
           POP HL            ;HL"=S2 ST
           JR SCOMPBG

SCOMPLP:   EXX
           OUT (C),L         ;S1 PAGE SEL
           LD A,(DE)         ;S1 CHAR
           INC DE
           OUT (C),H         ;S2 PAGE SEL

           EXX
           CP (HL)           ;S2 CHAR
           JR NZ,SCOMPEX     ;JR WITH CY IF S2 GRTR; NZ,NC IF S1 GRTR

           INC HL
           DEC BC

SCOMPBG:   LD A,B
           OR C
           JR NZ,SCOMPLP     ;LOOP UNTIL STRINGS MATCH OVER BC CHARS

           EX AF,AF'         ;Z IF LENS EQUAL - STRINGS MATCH
           JR Z,SCOMPEX      ;** COMPARISON BUG FIX

           CCF               ;CY IF S2 STRING LONGER - IE GREATER
                             ;NZ,NC IF S1 STRING LONGER - IE GREATER

SCOMPEX:   POP HL            ;S1 PTR

SCOMPC:    EX AF,AF'
           POP AF
           OUT (250),A
           POP AF
           OUT (251),A
           EX AF,AF'
           RET


;STRING BUFFER FETCH. COPY STRING ON FPCS TO "INSTBUF" IN COMMON MEM. ERROR IF
;LEN >255 OR 0. ON EXIT, BC AND A=LEN, DE=START, PAGING UNALTERED.

SBUFFET:   CALL SBFSR
           RET NZ

INVARG:    RST &08
           DB 27

;AS SBUFFET, BUT LEN ZERO GIVES Z FLAG, NOT AN ERROR MSG

SBFSR:     LD A,&FF

SBFSR2:    EX AF,AF'
           CALL R1OSR
           CALL GETSTRING    ;AND SELECT PAGE
           EX AF,AF'
           ADD A,B           ;ADD FF OR FE, LEN MSB
           JR C,INVARG       ;ERROR IF T$ LEN >FF  OR >01FF

           LD A,B
           OR C
           JR Z,SCOMPC       ;RET IF LEN=0
                             ;A=LEN.
           EX DE,HL
           LD DE,INSTBUF     ;256 BYTES IN PAGE 0
           PUSH BC
           PUSH DE
           LDIR              ;COPY T$ TO PAGE 0
           POP DE
           POP BC            ;BC=LEN, A=C. NC
           JR SCOMPC         ;NZ HERE FROM 'OR C' - SCOMPC PRESERVES IT TOO

;UNSTACK STRING AND GET PORT VALUE NEEDED TO SWITCH IT IN

UNSTKPRT:  CALL STKFETCH     ;A=PAGE, DE=START, BC=LEN
           LD H,A
           IN A,(251)
           XOR H
           AND &E0           ;USE UPPER 3 BITS FROM PORT
           XOR H
           RET               ;A=PORT VALUE

;ADDRESS OF IDERR GOES INTO SOME CHANNELS

IDERR:     RST &08
           DB 20             ;"Invalid device"

;CHECK IF OK TO USE ABC BYTES (PAGE FORM). EXITS WITH ABC, DE CHANGED.

TSTRMBIG:     PUSH AF
              PUSH BC
              CALL TSTRMABC
              POP BC
              POP AF
              RET

;CHECK IF OK TO USE ABC BYTES (PAGE FORM). EXITS WITH DE CHANGED. NC. AHL=
;PAGE FORM OF NEW WKEND

TSTRMABC:     LD H,B
              LD L,C

TSTRMAHL:     CALL AHLNORM
              LD B,H
              LD C,L         ;ABC=19-BIT SPACE
              DB &FE         ;"JR+1"

;CHECK IF OK TO USE BC BYTES (0-FFFF). ERROR IF NOT. EXITS WITH BC UNCHANGED, NC
;AHL=PAGE FORM OF NEW WKEND, DE=FREE, OR A HIGH VALUE IF FREE>=64K

TESTROOM:     XOR A

              PUSH BC
              LD D,A         ;DBC=SPACE
              CALL WENORMAL  ;GET AHL=WKEND (19 BIT)
              ADD HL,BC
              ADC A,D        ;AHL=NEW VALUE AFTER BC USED
              CALL PAGEFORM  ;AHL=PAGE FORM OF NEW WKEND
              PUSH AF
              PUSH HL
              EX DE,HL
              LD C,A
              LD A,(RAMTOPP)
              LD HL,(RAMTOP) ;AHL=RAMTOP
              CALL SUBAHLCDE ;AHL=ROOM
              JR C,OOMERR    ;ERROR IF NEW WKEND PAGE WOULD BE ABOVE RAMTOP

              CALL AHLNORM   ;AHL=19 BIT ROOM
              EX DE,HL
              AND A
              JR Z,TRM2

              SET 7,D        ;IF >64K FREE, MAKE DE A HI VALUE

TRM2:         POP HL
              POP AF         ;AHL=NEW WKEND
              POP BC         ;SPACE
              RET

OOMERR:       RST &08
              DB 1           ;"OUT OF MEMORY"


;COPY STRING FROM (DE), LEN BC, PORT VALUE A, TO WKSPACE. SOURCE CAN BE ANYWHERE
;AND NEED NOT BE SWITCHED IN ON ENTRY. LEN TRUNCATED TO <=255 WITHOUT ANY MSG.
;LEN 0 WILL CRASH! ROM1 MUST BE SWITCHED OFF ON ENTRY!
;EXIT: HL=PAST ROOM END, DE=ROOM START, DE"=PAST STRING END, B"=0, C"=URPORT,
;H"=SRC PORT VALUE, L"=WKSPACE PORT. WORKSPACE IS SWITCHED IN.
;NOTE: REASONABLY FAST FOR SHORT STRINGS
;USED BY READ AND VAL ONLY

;SCOPYWK:   PUSH DE           ;SRC
 ;          INC B
  ;         DEC B
   ;        JR Z,SCOPYWK2     ;JR IF LEN<=256

;           LD BC,&FF         ;ELSE TRUNCATE - STRMOVE ONLY ALLOWS SHORT STRINGS

;SCOPYWK2:  CALL WKROOM       ;DE=ROOM, HL=END BYTE, BC AND A UNCHANGED
 ;          LD B,C            ;LEN COUNTER
  ;         LD H,A            ;H=STRING PORT VALUE
   ;        IN A,(251)
    ;       LD L,A            ;L=WKSPACE PORT VALUE
     ;      PUSH DE           ;WKSPACE

;           EXX
 ;          POP HL            ;DEST TO HL"
  ;         LD D,H
   ;        LD E,L            ;KEEP A COPY OF THE START IN DE
    ;       EXX

;           POP DE            ;SRC

;;MOVE B BYTES FROM (DE) PORT VALUE H TO (HL") PORT VALUE L. EXIT WITH ALT REGS.

;           LD C,251

;STRMVLP:   OUT (C),H         ;SRC PAGE
 ;          LD A,(DE)         ;SRC BYTE
  ;         INC DE
   ;        OUT (C),L         ;DEST PAGE

;           EXX
 ;          LD (HL),A         ;TO WKSPACE AT HL"
  ;         INC HL
;           EXX
;
 ;          DJNZ STRMVLP      ;ABOUT 80 Ts/BYTE

;           EXX               ;RET WITH REGS SWAPPED

;COPY BC BYTES FROM DE TO WKSPACE

SCOPYWK:   INC B
           DEC B
           JR Z,SCOPYWK2     ;JR IF LEN<=256

           LD BC,&FF         ;ELSE TRUNCATE

SCOPYWK2:  CALL R1OSR
           EX DE,HL          ;SRC TO HL
           LD DE,INSTBUF
           PUSH DE
           PUSH BC
           LDIR
           POP BC
           PUSH BC
           CALL WKROOM
           POP BC
           POP HL
           PUSH DE
           LDIR
           EX DE,HL          ;HL=PAST END
           POP DE            ;START
           DEC HL
           LD (HL),&0D
           POP AF
           OUT (250),A
           POP AF
           RET

;CALLED BY LENGTH FN AND TAPEMN

LENGSR:    CALL LOOKVARS     ;HL POINTS TO NUMERIC VALUE, OR STRING LEN DATA
           PUSH AF           ;FOUND/NOT FOUND FLAG
           PUSH BC
           PUSH DE
           LD (MEMVAL),HL
           IN A,(URPORT)
           LD DE,MEMVAL+2
           LD (DE),A
           INC DE
           LD C,7
           CALL SCOPN2       ;LDIR 7 BYTES, SELCHADP
           POP DE
           POP BC            ;TYPE
           POP AF            ;FLAGS
           RET

           DS &3F8C-$

;FIXED ROUTINES
;*****************************************************************************
;UNSTACK A 5-BYTE NUMBER TO AN ADDRESS IN A (PAGE) AND HL (8000-BFFF)
;(RES 7,H IF 5-BYTE IS A LENGTH, TO GIVE PAGE, +0000-3FFFH). GIVES IOOR ERROR
; IF NUMBER IS NEGATIVE OR >07FFFF

UNSTLEN:   DB CALC           ;N
           DB STK16K         ;N,16K
           DB MOD            ;N MOD 16K
           DB RCL3           ;N MOD 16K,INT(N/16K) (PLACED BY MOD)
           DB EXIT

           CALL GETBYTE
           CP &21
           JP NC,IOORERR     ;PAGE MUST BE 00-20H (0=ROM, 1-20=RAM)

           PUSH AF
           CALL GETINT       ;TO HL AND BC
           POP AF
           RET               ;A=PAGE

;NEW POKE/DPOKE SR

NPDPS:     CALL PDPSUBR
           LD A,H
           CP &C0
           RET C

           JR INCURPAGE

;SELECT SCREEN, ROM1 OFF

SPSSR:     IN A,(250)
           LD (CLRP),A
           AND &BF
           OUT (250),A       ;ROM1 OFF

;STORE PAGE SCREEN SELECT

SPSS:      IN A,(251)
           LD (CURP),A

SELSCRN:   LD A,(CUSCRNP)    ;SCREEN PAGE
           JR SELURPG

;READ BYTE FROM SCREEN AT HL, FORCING SCREEN ON, ROM1 OFF

SREAD:     CALL SPSSR        ;SELECT SCREEN, ROM1 OFF
           LD A,(HL)

RCURPR:    EX AF,AF'
           LD A,(CLRP)
           OUT (250),A
           DB &3E            ;'JR+1'

TRCURP:
RCURP:     EX AF,AF'

RCUR2:     LD A,(CURP)
           OUT (251),A
           EX AF,AF'
           RET

;SET CHADP VAR AND SWITCH IT IN

SETCHADP:     LD (CHADP),A

;ROM1 OFF, SELCHADP

R1OCHP:       IN A,(250)
              AND &BF
              OUT (250),A         ;ROM1 OFF

;SWITCH IN CHADP

SELCHADP:     LD A,(CHADP)        ;CHAD PAGE
              JR TSURPG

;UNSTACK A STRING AND SELECT IT"S PAGE. DE=START (8000-BFFF), BC=LEN

TGTSTR:
GETSTRING:    CALL STKFETCH       ;A=PAGE, DE=START, BC=LEN

;SELECT UPPER RAM PAGE

SELURPG:
TSURPG:    PUSH HL
           LD H,A
           IN A,(251)
           XOR H
           AND &E0           ;KEEP TOP 3 BITS FROM PORT
           XOR H
           OUT (251),A
           POP HL
           RET

;INCREMENT UPPER RAM PAGE, ENSURE DE IS NOT IN C000-FFFF AREA
;USES A, ALTERS D

INCURPDE:     RES 6,D
              JR INCURCOM

;INC PAGE AND ADJUST HL POINTER IF NEEDED

CHKHL:        BIT 6,H
              RET Z

;INCREMENT UPPER RAM PAGE, ENSURE HL IS NO IN C000-FFFF AREA
;USES A, ALTERS H

INCURPAGE:    RES 6,H

INCURCOM:     IN A,(251)
              INC A
              JR SELURPG

DECURPAGE:    SET 6,H
              IN A,(251)
              DEC A
              JR SELURPG
