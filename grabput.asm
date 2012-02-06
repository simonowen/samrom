;GRAB.SAM - GRAB A$,X,Y,W,L

GRAB:      CALL SYNTAX1      ;ASSESS VAR FOR ASSIGNMENT
           LD HL,FLAGS
           BIT 6,(HL)
           JR NZ,GNONSH      ;ERROR IF NUMERIC TYPE

           RST &18
           CP ","

GNONSH:    JP NZ,NONSENSE

           CALL SEXPT4NUMS   ;SKIP, EXPECT 4 NUMBERS
           CALL CHKEND

           CALL CHKMD23      ;INSIST ON MODE 2 OR 3
           LD DE,&C000+30
           CALL LIMDB        ;LEN TO A, DECED (0-191) (ORIG MUST BE 1-192)
           INC A
           PUSH AF           ;A=LEN
           CALL GETINT
           DEC BC            ;0->FFFF, 256->255
           LD A,B
           AND A
           JP NZ,IOORERR     ;INSIST ON WIDTH OF 1-256

           INC BC            ;BC=1-256
           INC BC            ;IF BC IS EVEN, SET BIT 0 (WHICH WILL BE LOST)
                             ;IF BC IS ODD,ROUND UP E.G. WIDTH 1->2
           SRL B             ;B=0
           RR C              ;C=1-128
           POP DE            ;D=LEN
           LD E,C            ;WID
           PUSH DE           ;LEN/WID
           CALL GTFIDFCDS    ;UNSTK X,Y TO CB. CHECK FOR LEGALITY
           POP DE            ;LEN/WID
           CALL JGRAB
           CALL STKSTOS      ;STACK REGISTERS - STRING
           JP ASSIGN

;D=LEN (PIX), E=WIDTH (BYTES), B=Y, C=X
;EXIT: DE=START (IN CUSCRNP), BC=LEN (INCLUDES 3 LEADER BYTES)

JGRAB:     CALL SPSS         ;GET CURRENT SCREEN AT 8000+
           LD HL,RSBUFF-3    ;E000H
           LD (HL),0         ;CONTROL CODE
           INC HL
           LD (HL),E         ;WID
           INC HL
           LD (HL),D         ;LEN
           CALL GPVARS       ;GET A'=WID,D/B'=LEN, HL=SCRN ADDR, BC=128
           LD A,E
           RST &30
           DW CRTBFI
           LD A,E
           EX AF,AF'         ;(A' WAS CORRUPTED BY RST 30)
           CALL RSSTBLK      ;STORE ROLL/SCROLL STORE BLOCK SR
           LD DE,RSBUFF-3    ;PT TO CC,W,L
           LD BC,(TEMPW2)    ;LEN OF DATA
           INC BC
           INC BC
           INC BC            ;ALLOW FOR CC,W,L
           IN A,(251)
           JP RCURP

;*******************************************************************************
;SR TO TRUNCATE LENGTH OF A BLOCK IF HANGING OFF BOTTOM OF SCREEN
;ENTRY: D=BLOCK LENGTH, B=Y COORD

GPTRUNC:   LD A,D
           DEC A             ;LENGTH-1
           ADD A,B           ;ADD Y COORD
           JR C,GPTRUNC2

           SUB 192           ;ALLOW UP TO 191
           RET C             ;RET IF WONT FALL OFF BOTTOM

           SUB &40           ;COMPENSATE FOR FOLLOWING 'ADD'

GPTRUNC2:  ADD A,&40         ;GET 'NUMBER OF SCANS HANGING OFF' - 40H+
           CPL               ;ELSE A=0-3FH ->FF-C0
           ADD A,D
           LD D,A            ;ADJUST LENGTH
           RET

;GET/PUT VARS SETTER. ENTRY: E=WID, D=LEN, BC=YX COORDS.
;EXIT: A'/E=WIDTH, D/B'=LEN, HL=SCRN ADDR, BC=128

GPVARS:    LD A,E
           EX AF,AF'         ;A'=WIDTH
           CALL GPTRUNC      ;SHORTEN D IF NEEDED.
           LD A,D

           EXX
           LD B,A            ;B'=LENGTH
           EXX

           LD H,B            ;Y
           LD L,C            ;X
           SCF
           RR H
           RR L              ;GET SCREEN ADDR IN 8000 AREA IN HL
           LD BC,128
           RET

;PUT.SAM
;******************************************************************************
;E.G. PUT X,Y,A$. OVER 0-3 ALLOWED, ALSO INVERSE
;     PUT X,Y,A$,M$ USES M$ AS A MASK FOR 'CLIPPING'.

PUT:       CALL SYNTAX9      ;INK/PAPER IRREL, PUT OVER/INVERSE WORK
           CALL EXPTCSTR     ;COMMA, STRING
           CP ","
           JR NZ,PUTL1

           CALL SSYNTAXA     ;SKIP, EXPECT STRING

           LD HL,RSBUFF+&0FFD ;E003+0FFD= F000H
           CALL PSCHKMHL     ;CHECK MASK STRING AND MOVE TO 2ND HALF OF BUFFER
           LD A,5            ;'MASKED'
           JR PUTLC

PUTL1:     CALL CHKEND

           XOR A

PUTLC:     PUSH AF           ;0=NO MASK, 5=MASKED
           LD HL,RSBUFF-3    ;E000H
           CALL PSCHKMHL     ;CHECK STRING AND MOVE TO RSBUFF
           CALL GTFIDFCDS    ;B=Y,C=X.
           CALL SPSS
           POP AF
           AND A
           JR Z,PUTL2        ;JR IF NOT MASKED PUT. NC

           EXX
           LD HL,RSBUFF+&1000 ;HL'= F003H - PTR TO MASK STRING
           EXX

           LD HL,(RSBUFF-2)  ;E001/E002
           LD DE,(RSBUFF+&0FFE) ;F001/F002
           SBC HL,DE
           JR Z,PUT05        ;A=5 STILL... JR IF W,L THE SAME FOR BOTH STRINGS

           RST &08
           DB 38             ;'PUT mask mismatch'

PUTL2:     LD DE,(INVERT)    ;E=INVERT, D=GOVERT
           LD A,E            ;0-3
           OR D
           LD A,4
           JR Z,PUT05        ;OVER 0, INVERSE 0 USES SPECIAL ROUTINE NO. 4

           LD A,D
           AND 3             ;NEUROT...

PUT05:     LD HL,RSBUFF-2    ;STR PTR (E001H)
           JR PUT06

;A=PUT TYPE 0-5, B=Y, C=X, HL PTS TO W (BYTES), LEN (PIX), HL' TO MASK DATA
;BLOCK TRUNCATED IF HANGS OFF SCREEN BOTTOM

JPUT:      LD E,A
           CALL SPSS
           LD A,E            ;**

PUT06:     ADD A,A
           LD E,A
           ADD A,A
           ADD A,A           ;*8
           ADD A,E           ;*10
           LD E,A
           LD D,0
           LD IY,PUTSRTAB
           ADD IY,DE         ;IY=ADDR OF SUBROUTINE FOR 'OVER' VARIATIONS

           LD E,(HL)         ;GET WIDTH FROM STRING (BYTES)
           INC HL
           LD D,(HL)         ;LEN
           INC HL
           PUSH HL
           CALL GPVARS       ;GET A'/E=WID,D/B'=LEN, HL=SCRN ADDR, BC=128
           LD A,C
           SUB E             ;SCAN LEN-WIDTH=DISP TO NXT SCAN
           POP DE            ;STRING PTR
           LD IX,PUTRET      ;SET UP IX TO AVOID CALL OVERHEADS

           EXX
           LD E,A            ;E'=DISP
           LD A,(INVERT)     ;INVERSE MASK 00/FF TO A'
           EX AF,AF'         ;A=WIDTH
           LD C,A            ;C'=BLOCK WIDTH

PUTSCLP:   EXX

;AT THIS POINT:
;HL=SCRN PTR, DE=DATA SRC, A=WIDTH
;B'=LEN, C'=WIDTH, E'=DISP TO NEXT SCAN, A'=INVERSE MASK
;HL' CAN PT TO MASK STRING
;B IS USED AS A WIDTH COUNTER, AND C AS AN INVERSE MASK

           LD B,A            ;BLOCK WIDTH COUNTER SET UP
           EX AF,AF'
           LD C,A            ;INVERSE MASK
           JP (IY)           ;JUMP TO XOR, OR, LD or AND LOOP
                             ;ENTRY WITH DE=SRC, HL=SCRN DEST, B=BYTES, C=INVER
                             ;EXIT WITH B=0
PUTRET:    LD A,C            ;INVERSE
           EX AF,AF'         ;PROTECT INVERSE MASK IN A'

           EXX
           LD A,E            ;DISP TO NEXT SCAN FROM E'
           EXX

           LD C,A            ;BC=DISP TO NEXT SCAN (SCAN LEN-BLOCK WIDTH)
           ADD HL,BC         ;DROP 1 SCAN

           EXX
           LD A,C            ;GET WIDTH VALUE FROM C'
           DJNZ PUTSCLP      ;DEC LENGTH COUNTER, LOOP TILL ALL SCANS DONE

           JP RCURP

;******************************************************************************
;PUT SUBROUTINES FOR OVER 0,1,2,3.
;ENTRY WITH DE PTING TO DATA SRC, HL TO SCRN, B=BYTES TO DO, C=INVERSE MASK
;IX='RET' ADDR
;EXIT WITH B=0.

PUTSRTAB:

OVER0LP:   LD A,(DE)         ;DATA FROM STRING
           XOR C             ;INVERSE MASK
           LD (HL),A
           INC DE
           INC HL
           DJNZ OVER0LP
           JP (IX)
           NOP               ;MAKE ALL SRs HAVE LENGTH OF 10

OVER1LP:   LD A,(DE)         ;DATA FROM STRING
           XOR C             ;INVERSE MASK
           XOR (HL)          ;OVER 1
           LD (HL),A
           INC DE
           INC HL
           DJNZ OVER1LP
           JP (IX)

OVER2LP:   LD A,(DE)         ;DATA FROM STRING
           XOR C             ;INVERSE MASK
           OR (HL)           ;OVER 2
           LD (HL),A
           INC DE
           INC HL
           DJNZ OVER2LP
           JP (IX)

OVER3LP:   LD A,(DE)         ;DATA FROM STRING
           XOR C             ;INVERSE MASK
           AND (HL)          ;OVER 3
           LD (HL),A
           INC DE
           INC HL
           DJNZ OVER3LP      ;ABOUT 64 T'S PER BYTE (USE INC L,=56)
           JP (IX)

;FASTER VERSION FOR WHEN INVERSE 0, OVER 0

NOINVER:   EX DE,HL
           LD C,B
           LD B,0
           LDIR              ;32 T'S PER BYTE
           EX DE,HL
           JP (IX)
           NOP

;USE 'VALID DATA' MASK PTED TO BY HL'. 1'S=VALID
;MAKE MASK BY: FILL INK 0, BORDER AREA: GRAB A$: FILL INK 15, BORDER AREA
;PUT OVER 1, A$. (MAKES BORDER 1'S, FIGURE 0'S). GRAB A$: PUT INVERSE 1, A$
;GRAB A$. GIVES BORDER 0'S, FIGURE 1'S.

PMASKLP:   LD A,(DE)         ;DATA FROM STRING
           XOR C             ;INVERSE MASK
           XOR (HL)          ;XOR SCRN
           EXX
           AND (HL)          ;AND MASK
           INC HL
           EXX
           INC DE
           XOR (HL)
           LD (HL),A
           INC HL
           DJNZ PMASKLP      ;ABOUT 96 T'S PER BYTE
           JP (IX)

PUTBLKERR: RST &08
           DB 37             ;'Invalid PUT block'.

;PUT STRING CHECK/MOVE TO (HL)
;CHECK PUT STRING STARTS WITH CHR$ 0, LEN <>0. COPY TO (HL) IN SCRN MEM

PSCHKMHL:  LD (TEMPW1),HL    ;DEST
           CALL CHKMD23
        ;  CALL SPSS         ;GET CURRENT SCREEN AT 8000+
           CALL GETSTRING    ;DE=STRING ADDR, BC=LEN, PAGE IS SELECTED
           LD A,B            ;!!!SPSS ACTION NEGATED!!
           OR C
           JR Z,PUTBLKERR    ;REQUIRE NON-ZERO LENGTH

           LD A,(DE)
           AND A
           JR NZ,PUTBLKERR   ;REQUIRE CHR$ 0 AS 1ST. CHAR

         ; CALL SCRMOV
         ; JP RCURP

;SCRNMOV - COPY BC BYTES FROM (DE) TO (TEMPW1) IN SPARE SCRN MEMORY
;USES HL,DE,BC,AF,AF'

SCRMOV:    LD HL,(TEMPW1)
           ADD HL,BC
           JR C,PUTBLKERR    ;JR IF STRING WILL NOT FIT

           CALL SPLITBC
           LD A,(CUSCRNP)
           LD C,A            ;C:(TEMPW1)=DEST
           IN A,(251)        ;ADE=SRC
           SCF
           JR SFLDIR

FARLDDR:   BIT 6,H
           JR NZ,FLD3        ;JR IF SRC ALREADY IN SECT D

           DEC A             ;PAGE IN SRC AT C000-FFFF
           SET 6,H

FLD3:      BIT 6,D
           JR NZ,FLD4

           DEC C
           SET 6,D           ;DITTO DEST

FLD4:      AND A             ;'LDDR'
           DB &06            ;'JR +1'

;LDIR (PAGCOUNT) PAGES AND (MODCOUNT) BYTES FROM AHL TO CDE. MODCOUNT<=3FFF.
;PAGCOUNT OR MODCOUNT CAN BE ZERO WITHOUT PROBLEMS. PAGING UNCHANGED ON EXIT.
;EXIT WITH TEMPW1=PAST DEST, TEMPB2=PAGE OF PAST DEST, DE=PAST SRC END
;HL OR DE CAN BE ABOVE COOOH ON ENTRY WITHOUT PROBLEMS

FARLDIR:   SCF

           EX DE,HL
           LD (TEMPW1),HL    ;DEST ADDR

;C/(TEMPW1) =DEST, ADE=SRC

SFLDIR:    LD H,A
           LD A,C
           LD (TEMPB2),A

;ENTRY: HDE=SRC, TEMPB2/TEMPW1=DEST. USED IF FARLDIR HAS MOVED SOME DATA ALREADY

;FROM CONCATENATE

FARLDIR2:  EX AF,AF'         ;CY' IF LDIR
           CALL R1OSR        ;ROM1 OFF, SAVE PORT STATUSES (STATI?)
           LD A,H
           CALL TSURPG       ;SRC PAGE
           LD A,(PAGCOUNT)
           AND A
           JR Z,FLDIE

FARLDILP:  PUSH AF
           LD BC,&4000
           CALL STRMOV1
           POP AF
           DEC A
           JR NZ,FARLDILP    ;DO PAGCOUNT 16K MOVES

FLDIE:     LD BC,(MODCOUNT)
           CALL STRMOV
           JP POPOUT

;STRMOV: MOVE BC BYTES FROM (DE) IN CURRENT UR PAGE TO (TEMPW1) IN
;PAGE (TEMPB2), VIA SYS PAGE. WORKS WITH 0000 TO FFFF BYTES
;CY' IF LDIR, NC' IF LDDR
;EXIT WITH DE=PAST SRC END, PAGE SWITCHED IN, (TEMPW1)=PAST DEST END,
;(TEMPB2)=PAGE OF THE LATTER. HL=0
;USED BY FARLDIR

STRMOV:    LD A,B
           OR C
           RET Z

STRMOV1:   LD HL,(INSLV)
           INC H
           DEC H
           JP NZ,HLJUMP

           LD H,B
           LD L,C            ;HL=BYTES REMAINING TO DO

STRMOVL:   LD BC,&0100
           LD A,H
           AND A
           JR NZ,STRMOV2     ;JR IF 256 OR MORE TO DO STILL

           LD B,H
           LD C,L            ;COUNT=REMAINING. EXIT AFTER THIS MOVE

STRMOV2:   PUSH HL           ;BYTES REMAINING
           PUSH BC
           EX DE,HL
           LD DE,BUFF256
           EX AF,AF'
           JR C,DLDIR        ;JR IF LDIR
                             ;DEST=256 BYTE BUFFER, HL=SRC, BC=256 (USUALLY)
           EX AF,AF'
           DEC E             ;PT TO OTHER END OF BUFFER (E=255)
           LDDR
           POP BC
           BIT 6,H
           CALL Z,DECURPAGE  ;IF SRC FALLEN INTO SECTION C, DEC PAGE AND SET 6,H
           JR STRM32

DLDIR:     EX AF,AF'
           LDIR              ;COPY BYTES TO BASE PAGE
           POP BC
           CALL CHKHL

STRM32:    PUSH HL           ;SRC PTR (8000-BFFF OR C000-FFFF IF LDDR)
           IN A,(251)
           PUSH AF           ;SAVE SRC PAGE
           LD A,(TEMPB2)
           CALL TSURPG       ;GET DEST AT 8000+
           LD DE,(TEMPW1)    ;DEST PTR
           PUSH BC           ;BYTE COUNT
           EX AF,AF'
           LD HL,BUFF256
           JR C,STRM34

           EX AF,AF'
           DEC L             ;POINT TO OTHER END OF BUFFER
           LDDR
           EX DE,HL
           BIT 6,H
           CALL Z,DECURPAGE

           JR STRM38

STRM34:    EX AF,AF'
           LDIR              ;COPY BYTES FROM SYS PAGE TO DEST
           EX DE,HL
           CALL CHKHL

STRM38:    IN A,(URPORT)
           LD (TEMPB2),A     ;POSSIBLY NEW DEST PAGE (BITS 7-5 MAY BE HI)

STRMOV4:   LD (TEMPW1),HL    ;NEW DEST ADDR (SECTION C IF LDIR, D IF LDDR)
           EX DE,HL
           POP BC            ;BYTE COUNT
           POP AF
           OUT (251),A       ;RESTORE SRC PAGE
           POP DE            ;SRC PTR
           POP HL            ;BYTES REMAINING TO MOVE
           AND A
           SBC HL,BC
           JR NZ,STRMOVL

           RET
