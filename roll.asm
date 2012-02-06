;ROLL.SAM - EG ROLL DIR,PIX,X,Y,W,L
;              ROLL DIR,PIX
;              ROLL DIR
; SCROLL IS SIMILAR BUT NO WRAP-ROUND
; SCROLL CLEAR -   SCROLL PROMPT OFF
; SCROLL RESTORE - SCROLL PROMPT ON

;FROM JUMP TABLE:
;B=PIX, C=DIR (1-4), HL=TOP LHS COORDS, D=LEN, E=W, A=ROLL/SCROLL

JROLL:     DEC E
           LD (TEMPB3),A     ;ROLL=FF, SCROLL=00
           PUSH DE
           PUSH HL
           LD A,B
           PUSH AF
           JR JROLL2

ROLL:      LD A,&FF
           JR RSCOMM

SCROLL:    CP &B3            ;CLEARTOK
           JR Z,SETPROMPT    ;JR IF PROMPT TO BE TURNED OFF

           SUB &BA           ;RESTORETOK - ZERO RESULT FOR "PROMPT ON"
           JR NZ,SCRNINOT

SETPROMPT: LD B,A
           CALL SABORTER     ;SKIP CLEAR/RESTORE

           LD A,B
           LD (SPROMPT),A    ;0=PROMPTS ON
           RET

SCRNINOT:  XOR A

RSCOMM:    LD (TEMPB3),A     ;ROLL=FF, SCROLL=00
           CALL EXPT1NUM     ;DIRECTION
           CP ","
           JR Z,ROLL4        ;GET PIX IF SPECIFIED

           CALL CHKEND       ;ELSE CHECK END AND USE DEFAULT
           LD A,1            ;OF 1-PIXEL ROLL
           JR ROLL5

ROLL4:     CALL SEXPT1NUM    ; PIXELS
           CP ","
           JR Z,ROLL6        ;GET AREA IF SPECIFIED

           CALL CHKEND

           CALL GETBYTE      ;PIXELS

ROLL5:     LD HL,&C0FF       ;LEN=192, W-1=255 ARE DEFAULTS
           PUSH HL
           LD HL,&0000       ;Y=0, X=0 (TOP LHS) ARE DEFAULTS
           PUSH HL
           JR ROLL7

ROLL6:     CALL SEXPT4NUMS   ;SKIP X,Y,W,L - SR SHARED WITH GRAB
           CALL CHKEND

           CALL GETBYTE      ;L
           AND A
           JR Z,IOORHP2      ;LENGTH MUST BE 1-255 INITIALLY

           PUSH AF           ;LENGTH
           CALL GETINT       ;WIDTH. LEGAL=2-256
           RES 0,C           ;EVEN WIDTHS ONLY
           DEC BC            ;1-255
           LD A,B
           AND A
           JR NZ,IOORHP2

           POP AF            ;L
           LD B,A            ;L,W-1 IN B,C
           PUSH BC
           CALL GTFIDFCDS    ;B=Y, C=X (FAT COORDS FORCED)
           RES 0,C           ;EVEN X ONLY (OR FOR THINPIX, MULTIPLES OF 4 ONLY)
           PUSH BC
           CALL GETBYTE      ;A=PIX

ROLL7:     PUSH AF
           CALL GETBYTE      ;DIRECTION TO C

JROLL2:    PUSH BC
           CALL CHKMD23
           CALL GRATEMPS     ;SET TEMPS FROM PERM LS OR US VARS - USE FOR SCROLL
           CALL SPSSR        ;STORE PAGE, SELECT SCREEN
           POP BC            ;C=DIRECTION
           POP AF            ;PIX
           POP HL            ;COORDS
           POP DE            ;L, W-1
           AND A
           JR Z,IOORHP2

           LD B,A            ;PIX
           LD A,C            ;DIR. 1234=L/U/R/D
           DEC A
           CP 4
           JR C,ROLL75       ;ORIG DIR MUST BE 1-4

IOORHP2:   RST &08
           DB 30

ROLL75:    LD A,H            ;TOP=0, BOT=191
           ADD A,D
           JR C,IOORHP2

           CP 193
           JR NC,IOORHP2     ;ERROR IF AREA FALLS OFF BOTTOM

           BIT 2,C
           JR Z,NTRDOWN

           LD H,A
           DEC H             ;H=Y IF ROLL DOWN

NTRDOWN:   LD A,L
           ADD A,E           ;ADD X,W-1
           JR C,IOORHP2      ;JR IF OFF SCREEN ON RHS

           LD A,D

           EXX
           LD B,A            ;B"=LENGTH (1-192)
           EXX

           SCF
           RR H
           RR L              ;HL=SCR ADDR (LHS, TOP IF LT, RT OR UP; BOT IF DN)
           LD A,E
           ADD A,1           ;WIDTH=2-256. CY IF 256
           RRA               ;GET WIDTH IN BYTES (1-128)
           LD E,A            ;E=WIDTH IN BYTES
           LD D,B            ;D=PIX
           BIT 0,C           ;01=L,10=U,11=R,100=D
           JP Z,RUPDN        ;JP IF UP OR DOWN

           DEC E             ;E=W-1, IN BYTES (0-127)
           DJNZ RLBYTE       ;JR IF MOVING MORE THAN 1 PIX - USE BYTES

           LD D,C

;LEFT OR RIGHT BY 1 PIXEL (1 NIBBLE IN MODE 3)
;C/D=DIR, E=WIDTH-1 IN BYTES, HL=SCREEN ADDR, B"=LENGTH, A=WIDTH

           RST &30
           DW CRBBFN
           DEC D             ;DEC DIR
           JR NZ,NTNRL       ;JR IF ROLL RIGHT

           LD A,L
           ADD A,E
           LD L,A            ;HL=RHS IF ROLL LEFT BY 1 PIX
           LD A,E
           NEG
           LD E,A            ;E=NEGATED WIDTH-1 IF ROLL LEFT. ALLOWS PT TO LHS

NTNRL:     LD A,(TEMPB3)     ;ROLL=FF, SCROLL=00
           LD D,A
           LD C,128          ;SCAN LEN
           EXX

NLRLP:     EXX
           LD B,L
           LD A,L
           ADD A,E
           LD L,A            ;PT TO OTHER END OF LINE. CY IF E -VE (ROLL LEFT)
           LD A,(M23PAPT)
           INC D
           DEC D
           JR Z,ROLL8        ;JR IF SCROLL - A=BG COLOUR

           LD A,(HL)         ;GET NIBBLE TO WRAP ROUND
           JR NC,ROLL8       ;JR IF ROLL RIGHT

           RRCA              ;ELSE GET NIBBLE TO OTHER SIDE OF A
           RRCA
           RRCA
           RRCA

ROLL8:     LD L,B            ;HL PTS TO ORIG LINE END AGAIN
           PUSH HL
           CALL CDBUFF
           POP HL
           LD B,0
           ADD HL,BC
           EXX
           DJNZ NLRLP        ;DO B" SCANS

           JR RCURPH         ;RESET UR PORT

;RIGHT OR LEFT BY BYTES

RLBYTE:    INC B             ;B=PIX (2+)
           LD A,B
           RRA               ;BYTES OF MOVEMENT (1+). CALL IT M
           LD D,A
           LD A,E            ;WIDTH-1 IN BYTES (0-127)
           SUB D             ;BYTES OF MOVEMENT (M)
           JP C,IOORHP2      ;JR IF M ISN"T LESS THAN WIDTH

           INC A
           RST &30
           DW CRTBF
           SCF               ;CY=LEFT
           DEC C             ;Z IF LEFT, NZ IF RIGHT
           JR Z,RLBY2

           LD A,L
           ADD A,E
           LD L,A            ;HL PTS TO RHS IF RIGHT AND BYTE MOVING. NC

RLBY2:     INC C
           LD B,0
           LD A,(TEMPB3)
           CP 1              ;C IF SCROLL, NC IF ROLL
           DEC C             ;Z IF LEFT, NZ IF RIGHT
           LD A,D            ;A=M
           EXX
           JR C,SCROLLLR

           JR NZ,RRBMLP

;ROLL LEFT BYTES

RLBMLP:    EXX
           LD C,A            ;BC=M
           PUSH HL
           PUSH HL           ;SCRN PTR
           LD DE,RSBUFF
           LDIR              ;SAVE M BYTES FROM LINE START, ADVANCE SRC
           POP DE            ;ORIG HL
           CALL CDBUFF       ;COPY THE SCAN
           LD HL,RSBUFF
           INC B             ;B=0
           LD C,A            ;BC=M
           LDIR              ;WRAP BYTES FROM BUFFER
           POP HL            ;LINE START
           LD C,128
           ADD HL,BC         ;DROP 1 SCAN
           EXX
           DJNZ RLBMLP

RCURPH:    JP RCURPR

;ROLL RIGHT BYTES

RRBMLP:    EXX
           LD C,A            ;BC=M
           PUSH HL
           PUSH HL           ;SCRN PTR
           LD DE,RSBUFF+127
           LDDR              ;SAVE M BYTES FROM LINE END, MOVE SRC PTR LEFT
           POP DE            ;ORIG HL
           CALL CDBUFF       ;MOVE THE SCAN
           LD HL,RSBUFF+127
           INC B
           LD C,A            ;BC=M
           LDDR              ;WRAP BYTES FROM BUFFER
           POP HL            ;LINE START
           LD C,128
           ADD HL,BC         ;DROP 1 SCAN
           EXX
           DJNZ RRBMLP

           JR RCURPH

SCROLLLR:  JR NZ,SRBYPRE     ;JR IF SCROLL RIGHT

;SCROLL LEFT BYTES

SLBMLP:    EXX
           LD C,A            ;BC=M
           PUSH HL           ;SCRN PTR
           LD D,H
           LD E,L
           ADD HL,BC
           CALL CDBUFF       ;MOVE SCAN LEFT
           LD B,A
           LD C,A
           LD A,(M23PAPT)

SLBBLP:    LD (DE),A         ;BLANK END OF SCAN
           INC DE
           DJNZ SLBBLP

           LD A,C
           POP HL
           LD C,128
           ADD HL,BC         ;DROP 1 SCAN
           EXX
           DJNZ SLBMLP

RCURH2:    JR RCURPH

;SCROLL RIGHT BYTES.

SRBYPRE:   AND A             ;NC FOR FIRST SBC

SRBMLP:    EXX
           LD C,A            ;BC=M
           PUSH HL           ;SCRN PTR
           LD D,H
           LD E,L
           SBC HL,BC         ;DE PTS TO RHS. MOVE HL SLIGHTLY (M) BYTES LEFT
           CALL CDBUFF       ;MOVE SCAN RIGHT
           LD B,A            ;B=M
           LD C,A            ;SAVE M BRIEFLY
           LD A,(M23PAPT)

SRBBLP:    LD (DE),A         ;BLANK LHS (M BYTES)
           DEC DE
           DJNZ SRBBLP

           LD A,C            ;A=M
           POP HL
           LD C,128
           ADD HL,BC         ;DROP 1 SCAN. NC
           EXX
           DJNZ SRBMLP

           JR RCURH2

;*******************************************************************************
;RUPDN - ROLL/SCROLL UP OR DOWN
;ENTRY: E=WIDTH IN BYTES, HL=TOP LHS SCRN ADDR (UP) OR BOT LHS (DOWN)
;B"=LENGTH, D=PIX OF DISP, C=DIR (1-4)

RUPDN:     BIT 2,C           ;010=UP, 100=DOWN
           LD BC,128         ;BC = DISP TO ROW BELOW IF ROLL UP
           JR Z,RUPDN2

           DEC B             ;BC=FF80=-128 IF ROLL DOWN

;THIS ENTRY FROM EDRS WITH BC SET UP

RUPDN2:    LD A,E
           RST &30
           DW CRTBFI       ;CREATE BUFFER OF A LDI"S
           LD A,E
           EX AF,AF'
           LD A,D            ;PIX

           EXX
           LD C,A            ;C"=PIX
           LD A,B            ;GET LENGTH FROM B"
           LD B,C            ;B"=PIX
           EXX

           SUB D             ;SUB LEN,PIX
           JP C,IOORHP2      ;ERROR IF MOVEMENT IS GREATER THAN WIN LEN

           PUSH AF           ;SAVE MAIN BLOCK SCANS, Z IF MOVE=WIND LEN
           LD A,(TEMPB3)
           AND A
           CALL NZ,RSSTBLK   ;STORE WRAPPED AREA IF ROLL
           POP AF            ;LINES IN MAIN BLOCK
           JR Z,RUPFIN       ;JR IF MOVEMENT=LENGTH OF WINDOW (CLS)

           EXX
           LD B,A            ;B"=LINES IN MAIN BLOCK (AREA NOT WRAPPED)
           EXX

           LD A,D            ;PIX
           LD D,H
           LD E,L            ;DE IS SCREEN DEST

ADDDISP:   ADD HL,BC
           DEC A             ;DEC PIX TO MOVE BY
           JR NZ,ADDDISP     ;MOVE HL UP OR DOWN TO START OF MAIN BLOCK

           CALL RSMOVSR
           EX DE,HL

RUPFIN:    EX DE,HL          ;DE=SCRN DEST
           LD A,(TEMPB3)
           AND A
           JR Z,SCRUDBLK     ;JR IF SCROLL AND BLANKING OF NEW AREA NEEDED

                             ;ELSE WRAP DATA FROM BUFFER
           LD HL,RSBUFF
           EXX
           LD B,C            ;B"=PIX

;DE PTS TO BLOCK, BC=SGNED SCAN LEN, B"=SCANS TO DO

SCRUDWRAP: EXX
           PUSH DE           ;SCRN DEST
           PUSH BC           ;DISP TO NEXT SCAN
           CALL CDBUFF       ;COPY A SCAN
           POP BC
           POP DE
           EX DE,HL
           ADD HL,BC         ;MOVE SCRN PTR UP OR DOWN A SCAN
           EX DE,HL
           EXX
           DJNZ SCRUDWRAP    ;COPY "PIX" SCANS

           JR RCUHP

;DE PTS TO BLOCK, BC=SGNED SCAN LEN, C"=SCANS TO DO, M23PAPT=VALUE

SCRUDBLK:  EX DE,HL          ;HL PTS TO BLOCK TO CLEAR
           LD A,(M23PAPT)
           EXX
           LD B,C            ;B"=PIX

SCRUBOLP:  EXX
           LD D,H
           LD E,L
           INC E
           LD (HL),A
           PUSH BC
           PUSH HL
           CALL CDBUFF+2
           POP HL
           POP BC
           ADD HL,BC         ;MOVE UP OR DOWN A SCAN
           EXX
           DJNZ SCRUBOLP    ;BLANK "PIX" SCANS AT TOP OR BOTTOM

RCUHP:     JP RCURPR

;*******************************************************************************
;RSSTBLK - STORE BLOCK. ALSO USED BY GRAB
;ENTRY: HL=SCRN ADDR (TOP OR BOT), BC=SGNED SCAN LEN, D=PIX, B"=PIX,A"=WIDTH
;EXIT: SAME EXCEPT B"=0. TEMPW2=SPACE

RSSTBLK:   PUSH DE
           PUSH HL
           EX AF,AF'         ;WIDTH
           LD E,A
           LD HL,0
           LD A,D            ;A=PIX
           LD D,H            ;DE=WIDTH

CALCSPLP:  ADD HL,DE
           DEC A
           JR NZ,CALCSPLP    ;CALC WIDTH*PIX=STRIP MEM USE

           LD (TEMPW2),HL    ;SAVE SPACE REQUIRED FOR DATA (FOR GRAB)
           LD DE,RSBUFF+16   ;E013H
           ADD HL,DE
           JR NC,STSTPOK

           RST &08
           DB 36             ;"Stored area too big"
                             ;ERROR IF STRIP USES MORE THAN (8K-19 BYTES)
                             ;(16 BYTES FOR PUT STACK, 3 FOR CC,W,L)
STSTPOK:   POP HL
           PUSH HL
           LD DE,RSBUFF
           EXX

STSTPLP:   EXX
           PUSH BC           ;SCAN LEN
           PUSH HL           ;SCRN SRC
           CALL CDBUFF
           POP HL
           POP BC
           ADD HL,BC         ;PT TO SCAN ABOVE OR BELOW (BC IS SIGNED SCAN LEN)
           EXX
           DJNZ STSTPLP

           EXX
           POP HL            ;SCRN ADDR
           POP DE            ;D=PIX
           RET

;******************************************************************************
;CLS AND EDITOR SCROLL ROUTINES.
;CLEAR WINDOW - VARS DEFINE CHAR WINDOW

CLSWIND:   LD HL,WINDBOT
           LD A,(HL)
           DEC HL
           SUB (HL)          ;SUB WINDTOP
           INC A             ;GET ROWS OF WINDOW LEN

           LD C,2            ;"UP"
           CALL EDRSSR       ;GET B"=LEN, D=DISP (SAME)
           LD A,(DEVICE)
           AND A
           LD A,D
           JR NZ,CLSW1       ;JR IF NOT UPPER SCREEN

           LD A,(LSOFF)
           ADD A,D           ;INCLUDE "LEFT OVER" SCANS
           LD D,A

CLSW1:     EXX
           LD B,A
           EXX
           JR EDRSF

;SCROLL WINDOW DOWN BY "A" ROWS

EDRSADN:   LD D,0            ;SCROLL LPT DOWN
           PUSH AF
           RST &30
           DW STENTS         ;SCROLL LINE PTR TABLE
           POP AF
           LD C,4            ;DOWN
           JR EDRS

;SCROLL WINDOW UP BY 1 ROW

EDRS1UP:   LD A,1            ;LPT SCROLL BY 1 ROW
           LD D,A            ;SCROLL LPT UP AS D=1
           RST &30
           DW STENTS
           LD A,1            ;WINDOW SCROLL BY 1 ROW

EDRSAUP:   LD C,2            ;UPWARDS

;EDRS - EDITOR"S ROLL/SCROLL ROUTINE
;ENTRY: WINDOW VARS DEFINE CHAR WINDOW, A=ROWS TO SCROLL, C=2 IF UP, 4 IF DN.

EDRS:      CALL EDRSSR

EDRSF:     CALL SPSSR        ;STORE PAGE, SELECT SCREEN
           LD A,(MODE)
           CP 2
           JP NC,RUPDN

           BIT 2,C
           LD BC,32
           LD IX,NEXTDOWN    ;IN CASE MODE 0
           JR Z,EDRSM1       ;JR IF UP

           LD BC,-32
           LD IX,NEXTUP      ;IN CASE MODE 0

EDRSM1:    LD A,(MODE)
           AND A
           JR Z,EDRSM0       ;JR IF MODE 0

           XOR A
           LD (M23PAPT),A    ;MAKE "PAPER COLOUR" BLANK PIXEL PATTERN
           PUSH BC           ;SIGNED SCAN LEN
           PUSH DE           ;D=PIX,E=WIDTH
           PUSH HL           ;SCREEN ADDR
           EXX
           PUSH BC           ;LEN IN SCANS
           EXX
           CALL RUPDN2
           EXX
           POP BC
           EXX
           POP HL
           SET 5,H           ;ADD 2000H - PT TO MODE 1 ATTR
           POP DE
           POP BC
           LD A,(ATTRT)
           LD (M23PAPT),A    ;SET COLOUR FOR M1 ATTRIBUTE SCROLL
           CALL SPSSR
           JP RUPDN2

;MODE 0 SCROLL
;D=PIX TO MOVE BY, E=WIDTH, HL=SCRN ADDR, BC=SGNED ATTR ROW LEN, B"=WIND LEN

EDRSM0:    LD A,E
           RST &30
           DW CRTBFI
           PUSH BC           ;+/-32

           EXX
           LD A,B            ;GET PIX OF WINDOW LEN
           EXX

           SUB D             ;SUB WINDOW LEN, PIX TO MOVE BY="MAIN BLOCK" LEN
           JP C,IOORHP2      ;ERROR IF MOVED BY MORE THAN WIND LEN

           EXX
           LD B,A            ;MAIN BLOCK LEN IN B"
           EXX

           LD A,D            ;PIX. Z IF WIND LEN=MOVEMENT (CLS)
           PUSH AF
           LD C,E            ;C=WIDTH
           LD B,D            ;B=PIX
           LD D,H
           LD E,L            ;DE=SCRN DEST
           JR Z,EDRSM0L1


EDRSM0P:   CALL IXJUMP       ;MOVE UP OR DOWN A SCAN
           DJNZ EDRSM0P      ;PT HL TO SRC
                             ;B=0 SO BC=WIDTH
EDRSM0L1:  POP AF            ;PIX. Z/NZ
           PUSH AF           ;PIX
           PUSH DE
           PUSH HL           ;SAVE FOR ATTR SCROLL
           PUSH AF           ;PIX, Z/NZ

           EXX
           JR Z,EDRSM0L2

EDRSM0LP:  EXX
           PUSH HL
           PUSH DE
           PUSH BC
           CALL CDBUFF
           POP BC
           POP HL
           CALL IXJUMP       ;ADJ DEST PTR
           EX DE,HL
           POP HL
           CALL IXJUMP       ;ADJ SRC PTR
           EXX

           DJNZ EDRSM0LP     ;LOOP FOR ALL SCANS

EDRSM0L2:  LD B,D            ;B"=ROWS FOR ATTR SCROLL
           EXX

           EX DE,HL          ;HL=SCRN DEST
           POP DE            ;D=PIX TO MOVE BY (AND BLANK)
           LD E,&00

EDRSM0DL:  PUSH HL           ;SCAN START
           LD B,C            ;USE B AS WIDTH COUNTER

EDRSM0BL:  LD (HL),E
           INC L
           DJNZ EDRSM0BL

           POP HL
           CALL IXJUMP
           DEC D
           JR NZ,EDRSM0DL

           POP HL
           CALL CTAA         ;CONVERT SRC TO ATTR ADDR
           EX DE,HL          ;SRC IN DE
           POP HL
           CALL CTAA         ;CONVERT DEST
           EX DE,HL
           POP AF            ;Z/NZ
           POP BC            ;+/-32
           CALL NZ,RSMOVSR
           LD A,(ATTRT)
           LD (M23PAPT),A    ;SET COLOUR FOR M1 ATTRIBUTE SCROLL

;DE PTS TO BLOCK, BC=SGNED SCAN LEN, C"=SCANS TO DO, A"=WIDTH, M23PAPT=VALUE

           JP SCRUDBLK

;CONVERT HL TO ATTR ADDR

CTAA:      LD A,H
           RRCA
           RRCA
           RRCA
           AND 3
           OR &98
           LD H,A
           RET

;SCROLL M0 ATTRS / MAIN R/S UP/DOWN ROUTINE

RSMOVSR:   EXX

UPDNLP:    EXX
           PUSH HL           ;SCRN SRC PTR
           PUSH DE           ;DEST
           PUSH BC           ;DISP TO ROW ABOVE OR BELOW
           CALL CDBUFF
           POP BC            ;DISP
           POP HL            ;DEST
           ADD HL,BC         ;ADJUST BY SCAN LEN
           EX DE,HL
           POP HL
           ADD HL,BC         ;SRC IS AJUSTED BY SIGNED SCAN LEN
           EXX

           DJNZ UPDNLP       ;DO B" SCANS

           EXX
           RET

;EDRSSR
;ACTION: GET E=WIDTH IN BYTES, HL=TOP LHS SCRN ADDR (UP) OR BOT LHS (DOWN)
;B"=LENGTH, D=PIX OF DISP, KEEP C

EDRSSR:    EXX
           LD C,A            ;C"=ROWS TO MOVE BY
           EXX

           CALL CALCPIX
           PUSH AF           ;SAVE AMOUNT TO MOVE BY, IN SCANS (PIX)
           XOR A
           LD (TEMPB3),A     ;"SCROLL"
           LD DE,(WINDLHS)   ;DE=TOP/LHS ROW
           BIT 1,C
           JR NZ,EDRS1       ;JR IF SCROLL UP

           LD A,(WINDBOT)    ;ELSE GET BOTTOM
           INC A
           LD D,A
           CALL ANYDEADDR    ;GET ADDR 1 SCAN (SIC) LOWER THAN NEEDED - NOW
           LD A,(MODE)       ;BACK UP BY 1
           LD HL,&FF80       ;MINUS SCAN LEN FOR M2 OR M3
           CP 2
           JR NC,EDRS0       ;JR IF M2 OR M3

           LD L,&E0          ;MINUS SCAN LEN FOR M1=FFE0
           DEC A
           JR Z,EDRS0

           EX DE,HL
           CALL NEXTUP       ;IF MODE 0
           DB &FE            ;"JR+1"

EDRS0:     ADD HL,DE

           EX DE,HL          ;DE=DESIRED SCREEN ADDR
           CP A              ;SET Z

;ENTRY AT EDRS1 IS NZ

EDRS1:     CALL NZ,ANYDEADDR  ;USES DE/A ONLY. GETS DE=SCRN ADDR
           LD HL,WINDBOT
           LD A,(HL)
           DEC HL
           SUB (HL)          ;SUB WINDTOP
           INC A             ;GET ROWS OF WINDOW LEN

           EXX
           SUB C
           LD D,A            ;ROWS TO DO (IN CASE M0 ATTR SCROLL)
           ADD A,C           ;WINDOW LEN IN ROWS
           CALL CALCPIX
           LD B,A            ;B"=SCANS OF WINDOW LEN
           EXX

           DEC HL
           DEC HL
           LD A,(HL)         ;WINDRHS
           INC HL
           SUB (HL)          ;SUB WINDLHS
           INC A             ;WIDTH IN CHARS.
           EX DE,HL          ;HL=SCREEN ADDR
           POP DE            ;D=AMOUNT TO MOVE BY IN SCANS
           LD E,A            ;MODE 0 OR 1 USES 1 BYTE/CHAR
           LD A,(MODE)
           CP 2
           RET C

           CP 3
           JR Z,EDRS3

           LD A,(FL6OR8)
           AND A
           LD A,E
           JR NZ,EDRS2       ;JR IF 8 PIXEL, 2 BYTE CHARS

           INC E
           SRL E

EDRS2:     ADD A,E           ;A=WIDTH*1.5, ROUNDED UP, WIDTH*2
           LD E,A
           RET

EDRS3:     LD A,E            ;MODE 3 USES 4 BYTES/CHAR
           ADD A,A
           ADD A,A
           LD E,A
           RET

CALCPIXD:  LD A,D

;CALCULATE NO. OF PIXELS IN "A" ROWS. RESULT IN A. ADD 8 IF BOTTOM HALF OF
;DOUBLE-HEIGHT CHARACTER

CALCPIX:   PUSH BC
           LD C,A            ;C=ROWS
           LD A,(CSIZE)
           SUB 5
           LD B,A            ;B=HEIGHT-5 (AT LEAST 1)
           LD A,C
           ADD A,A
           ADD A,A
           ADD A,C           ;A=ROWS*5

CLPXL:     ADD A,C
           DJNZ CLPXL        ;A=ROWS*6 IF B=1, ROWS*7 IF B=2 ETC

           LD C,A
           LD A,(DHADJ)      ;8 IF BOTTOM HALF OF DOUBLE-HEIGHT CHAR BEING
                             ;PRINTED, ELSE 0
           ADD A,C
           POP BC
           RET

;MODE 0 MOVE HL UP BY 1 SCAN

NEXTUP:    DEC H
           LD A,H
           OR &F8
           INC A
           RET NZ

           LD A,L
           SUB 32
           LD L,A
           RET C

           LD A,H
           SUB &F8
           LD H,A
           RET


;GET ADDR OF POSN 1 SCAN BELOW (HL) IN HL, MODES 0/1. USES AF,HL ONLY
;SCREEN$ SR

NXTDOWN:   LD A,(MODE)
           AND A
           JR Z,NEXTDOWN

           LD A,&20
           ADD A,L
           LD L,A
           RET NC

           INC H
           RET

;USED BY ROLL/SCROLL

NEXTDOWN:  INC H
           LD A,H
           AND &07
           RET NZ              ;NC=NO CRSSING OF CHAR BORDER

NXTDOWN1:  LD A,L
           ADD A,32
           LD L,A
           RET C               ;RET IF NEW THIRD

           LD A,H
           ADD A,&F8           ;SET CY
           LD H,A
           RET
