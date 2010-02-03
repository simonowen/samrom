;MISC1.SAM
;MODE, TEMPS, DATA, RESTORE
;CITEM, PERMS, SETSTRM

;CALLED BY LIST

PRHSH1:    CP "#"
           RET NZ

;ENTRY FROM PRINT

PRHSH2:    CALL SSYNTAX6     ;NUMBER

           CALL GETBYTE
           CALL STRMINF2
           JR STSM2

;ENTRY: FPCS HOLDS STREAM NUMBER. EXIT: DE=STREAM PTR, HL PTS TO STRM-H, C=STRM

STRMINFO:  CALL GETBYTE      ;IN A AND C

STRMINF2:  CP &11
           JR NC,INVSTRM     ;STREAMS 0-16 ARE LEGAL

;FROM SETSTRM

STRMINF3:  CP &10
           JR NZ,STRMINF4

           LD A,&FC          ;TRANSFORM STREAM 16 TO -4 (OUTPUT TO STRING)
           LD C,A

STRMINF4:  ADD A,&0B         ;(07, 0B-1A IF FROM ABOVE, OR 06-0E IF FROM SETSTRM
           ADD A,A           ;0EH, 16-34H OR 0CH-1CH
           LD L,A
           LD H,&5C          ;5C0C-5C34H
           LD E,(HL)
           INC HL
           LD D,(HL)
           LD A,D
           OR E              ;Z IF CLOSED
           RET

INVSTRM:   RST &08
           DB 21             ;"Invalid stream number"

SNOTOPER:  RST &08
           DB 47             ;"Stream is not open"

STREAMFE:  LD A,&FE
           DB &21

STREAMFD:  LD A,&FD

;ENTRY: A=FCH-03H

SETSTRM:   LD (STRNO),A      ;KEEP FOR DOS
           CALL STRMINF3

STSM2:     JR Z,SNOTOPER     ;JR IF STREAM NOT OPEN

           LD HL,(CHANS)
           ADD HL,DE         ;PT TO 2ND BYTE OF CHANNEL
           DEC HL

CHANFLAG:  LD (CURCHL),HL
           INC HL
           INC HL
           INC HL
           INC HL
           LD A,(HL)         ;CHANNEL LETTER
           LD (CLET),A       ;KEEP IT FOR INPUT TO LOOK AT
           LD C,2
           CP "P"
           JR Z,STSMD

           DEC C
           CP "K"
           JR Z,STSMD

           DEC C
           CP "S"
           RET NZ            ;ONLY SET "DEVICE" OR CALL TEMPS FOR K/S/P

STSMD:     LD A,C
           LD (DEVICE),A     ;P=2,K=1,S=0

;COPY PERMANENT GRAPHIC VARS TO TEMP VARS.
;TEMPS FOR PRINT (COLOUR EXPANSION TABLE IF NEEDED)
;HAVE TO COLOUR TABLE *AFTER* EACH PRINTED INK/PAPER/BRIGHT ALSO - CALL COLEX

TEMPS:     CALL GTEMPS
           LD HL,(UWRHS)
           LD DE,(UWTOP)     ;UPPER WINDOW DATA
           LD A,(DEVICE)
           DEC A
           JR NZ,TEMUS       ;JR IF NOT LOWER SCREEN

           LD HL,(LWRHS)
           LD DE,(LWTOP)

TEMUS:     LD (WINDRHS),HL
           LD (WINDTOP),DE

COLEX:     LD A,(MODE)
           SUB 2
           RET C             ;RET IF M0/M1

           LD B,32           ;COLOUR 32 BYTES IF M3
           JR NZ,COLEX1      ;JR IF MODE 3

           LD B,16           ;MODE 2 O/P NEEDS 16-BYTE COLEX TABLE

COLEX1:    LD DE,(M23PAPT)   ;D=M3INKT, E=M3PAPT
           LD HL,EXTAB       ;EXPANSION TABLE
           EXX
           LD HL,CEXTAB      ;COLOURED EXPANSION TABLE
           EXX
           LD A,D
           XOR E
           LD C,A            ;C=PAPER XOR INK

COLEXLP:   LD A,C
           AND (HL)          ;USE EXPANDED NIBBLES AS MASKS TO CHOOSE INK/PAP
           INC L
           XOR E
           EXX
           LD (HL),A
           INC L
           EXX
           DJNZ COLEXLP

           RET


;GRAPHIC TEMPS LEAVES EXPANSION TABLE ALONE (FOR SPEED) AND SETS "UPPER SCRN"

GRATEMPS:  XOR A
           LD (DEVICE),A

GTEMPS:    LD HL,THFATP
           LD DE,THFATT
           LD BC,9
           LDIR
           LD A,(DEVICE)
           AND A
           JR Z,TEMPS1       ;JR IF UPPER SCREEN, ELSE USE LOWER SCRN COLOURS

           LD H,B
           LD L,B
           LD (OVERT),HL     ;OVER 0, INVERSE 0
           LD A,(BORDCR)
           LD L,A
           LD (ATTRT),HL     ;MASKT WILL BE ZERO
           LD HL,(M23LSC)    ;MODES 2/3 LOWER SCREEN COLOURS
           LD (M23PAPT),HL   ;LD PAPER AND INK

TEMPS1:    LD A,(MODE)
           CP 2
           RET Z

           LD A,1
           LD (THFATT),A     ;NZ=FAT UNLESS MODE 2, THEN=COPY OF THFATP
           RET


;POKE, DPOKE, CALL, USR, PEEK & DPEEK ALLOW ADDRESSES OF 00000-1FFFF, RELATIVE
;TO THE CONTEXT BASE PAGE. 0000-3FFF WILL BE ROM0, 4000-7FFF WILL BE BASE PAGE,
;8000-BFFF WILL BE THE PAGE ABOVE THE BASE PAGE. 10000-13FFF WILL SWITCH PAGE
;3 ABOVE BASE PAGE INTO 8000H (IRRELEVANT EXCEPT TO NON-RELOCATABLE CALLED OR
;USRED CODE)

POKE:      CALL EXPT1NUM     ;EVAL ADDR
           CALL INSISCOMA
           CALL EXPTEXPR
           JR NZ,POKE2       ;JR IF NUMERIC, ELSE DO POKE N,A$

           RET NC            ;RET IF SYNTAX TIME

           CALL STKFETCH     ;ADE=SRC, BC=LEN
           PUSH AF
           PUSH DE
           CALL SPLITBC
           CALL UNSTLEN
           LD C,A
           DEC C
           EX DE,HL
           SET 7,D           ;CDE=DEST ADDR
           POP HL
           POP AF            ;AHL=SRC ADDR
           JP FARLDIR

POKE2:     JR NC,POKE3

           DB CALC           ;ADDR,N
           DB SWOP           ;N,ADDR
           DB STOD0          ;N
           DB EXIT

POKE3:     LD DE,0           ;EXTRA NUMBERS COUNT
           RST &18
           JR POKE4

POKENL:    PUSH DE           ;EXPR COUNT
           CALL SEXPT1NUM
           POP DE
           INC E
           BIT 5,E
           JP NZ,NONSENSE    ;LIMIT EXTRAS TO 31

POKE4:     CP ","
           JR Z,POKENL

           CALL CHKEND

           PUSH DE

           DB CALC
           DB RCL0           ;N1,N2,N3...Nn,ADDR
           DB EXIT

           CALL NPDPS        ;GET ADJUSTED ADDR TO HL, FORMER PAGE TO A
           POP DE            ;DE=0 IF ONLY 1 NUMBER
           ADD HL,DE
           INC E

PKALP:     PUSH HL           ;DEST ADDR FOR TOP NUMBER ON FPCS
           PUSH DE           ;E=NUMBER OF NUMBERS ON FPCS
           CALL FPTOA
           JP C,IOORERR

           JR Z,POKE5        ;JR IF +VE

           NEG

POKE5:     POP DE
           POP HL
           LD (HL),A
           DEC HL
           DEC E
           JR NZ,PKALP

           RET

DPOKE:     CALL SYNTAX8      ;EVAL ADDR, NUMBER

           CALL GETINT       ;WORD TO POKE TO BC
           CALL NPDPS
           LD (HL),C
           INC HL
           LD (HL),B
           RET


;PDPSUBR - USED BY POKE, DPOKE, PEEK, DPEEK, CALL
;ENTRY: ADDR ON FPCS.
;EXIT: IF ADDR IS 0-64K, THEN PAGING=ROM0 (OR BASE-1), BASE PAGE, BASE+1, BASE+2
;IF ADDR>64K, IT IS REDUCED TO 8000-BFFF RANGE AND PAGED IN. E.G. 10000H WOULD
;SWITCH PAGES BASE+3/BASE+4 IN AT 8000-FFFF, HL WOULD BE 8000H
;HL=ADDR, A=ORIG URPAGE. (LRPAGE UNCHANGED)

PDPSUBR:   PUSH BC           ;PRESERVE BC THROUGHOUT
           IN A,(251)
           PUSH AF
           CALL UNSTLEN
           SET 7,H           ;AHL=ADDR IN PAGE, 8000-BFFF FORM
           DB &11            ;"JR+2"

;PDPSR2. USED BY LOAD CODE (EXEC)
;ENTRY: AHL=EXEC ADDR

PDPSR2:    PUSH BC
           PUSH AF           ;KEEP STACK HAPPY

PDPC:      CP 4
           JR NC,PDPSUBR4    ;JR IF NOT 0000-FFFF

           LD C,2            ;PAGING WILL BE ROM0,BASE,BASE+1,BASE+2
           CP C
           JR Z,PDPSUBR3     ;ADDR IS OK IF PAGE IS 2

           JR NC,PDPSUBR2    ;JR IF PAGE 3 - ADD 4000H TO ADDR

           RES 7,H           ;ADDR NOW 0000-3FFF
           AND A
           JR Z,PDPSUBR3     ;JR IF PAGE 0 - ADDR OK
                             ;ELSE ADD 4000H FOR PAGE 1 ADDR

PDPSUBR2:  SET 6,H           ;ADD 4000H TO ADDR

PDPSUBR3:  LD A,C

PDPSUBR4:  DEC A
           CALL TSURPG
           POP AF            ;ORIG URPORT
           POP BC
           RET


;CHECK MODE 2 OR MODE 3

CHKMD23:      LD A,(MODE)
              CP 2
              RET NC

INVMERR:      RST &08
              DB 34          ;"Invalid screen mode"


;READ.  E.G. READ A, READ A$, READ LINE A$

READ:         CP LINETOK
              PUSH AF             ;Z IF LINE
              JR NZ,READ2

              CALL SSYNTAX1       ;SKIP 'LINE', ASSESS VAR FOR ASSIGNMENT
              LD HL,FLAGS
              BIT  6,(HL)
              JP NZ,NONSENSE      ;READ LINE NOT ALLOWED WITH NUMERICS
                                  ;SKIP NEXT CALL
READ2:        CALL NZ,SYNTAX1
              CALL RUNFLG
              JP NC,RJUNKFLG      ;JR IF SYNTAX TIME

              RST  &18
              LD (PRPTR),HL       ;SAVE CHAD IN AUTO-ADJUST VAR SO IF IT PTS TO
              LD A,(CHADP)        ;E-LINE THE ASSIGNMENT WON'T BOLIX IT
              LD (PRPTRP),A
              CALL ADDRDATA       ;ADDRESS DATADD - SWITCH IN ITS PAGE, LD HL
              LD (CHADP),A        ;WITH ADDR PART
              LD   A,(HL)
              CP   &20
              JR   Z,READ3

              CP ","
              JR   Z,READ3        ;SPACES AND COMMAS ARE OK TO READ FROM.
                                  ;OTHERWISE, NEED TO LOOK FOR NEXT DATA STAT.

              LD (CHAD),HL        ;NEEDED BY 'SRCHPROG'
              LD E,&B9            ;DATATOK
              LD HL,(CLA)
              PUSH HL
              CALL SRCHPROG       ;LOOK FOR 'DATA' FROM CHAD ONWARDS
              POP DE
              LD (CLA),DE
              IN A,(251)          ;CHAD PTS TO JUST AFTER 'DATA'
              LD (CHADP),A        ;ASSUME FOUND...
            ;  LD (DATADDP),A      ;** BUG FIX
              JR C,READ4          ;JR IF FOUND OK. PAGE MAY BE SWITCHED

              RST &08
              DB 3                ;'DATA has all been read'

READ3:        INC HL
              LD (CHAD),HL

READ4:        POP  AF
              JR   Z,READLN       ;JR IF 'READ LINE'

              CALL VALFET1        ;ASSIGNMENT FOR NON-LINE READ
              JR   READ7

READLN:       LD   BC,&FFFF
              PUSH HL

READ5:        LD A,(HL)
              CP &22
              JR NZ,READ6

RDSTRL:       INC HL
              INC BC
              CP (HL)
              JR NZ,RDSTRL

READ6:        CALL NUMBER
              LD   (CHAD),HL      ;SKIP FP FORMS
              INC  HL
              INC  BC             ;INC COUNT OF NON-INVISIBLE CHARS
              CALL COMCRCO        ;CHECK IF COMMA, CR OR COLON
              JR   NZ,READ5       ;LOOP UNTIL ONE IS FOUND

              POP  DE
              PUSH BC             ;LEN WITH NO FP FORMS
              SBC  HL,DE          ;FIND DISTANCE CHAD MOVED
              LD   B,H
              LD   C,L            ;LEN WITH FP FORMS,+1
              CALL SCOPYWK
              EX DE,HL            ;HL=ROOM START
              PUSH HL
              CALL REMOVEFP       ;FROM (HL) TO 0DH
              POP  DE             ;FIRST CHAR IN WKSPACE
              POP  BC             ;NON-INVISIBLE CHAR COUNT
              CALL STKSTOREP      ;STORE REGS FOR A STRING
              CALL ASSIGN

READ7:        RST  &18            ;GET CHAD
              LD (DATADD),HL      ;UPDATE DATA PTR
              IN A,(251)
              LD (DATADDP),A
              LD HL,(PRPTR)       ;GET REAL CHAD
              LD (CHAD),HL
              LD A,(PRPTRP)
              CALL SETCHADP
              DB &FE              ;'JR +1'

RJUNKFLG:     POP AF              ;JUNK F
              RST  &18
              CP ","
              RET NZ

              RST &20             ;SKIP COMMA
              JP READ


;CITEM.SAM - COLOUR ITEMS.
;CALLED BY SYNTAX 9

SYNT9SR:   CALL RUNFLG
           JR NC,SYN9SR1     ;JR IF SYNTAX CHECK

           XOR A
           LD (DEVICE),A     ;UPPER SCREEN
           CALL GRATEMPS
           LD HL,MASKT
           LD A,(HL)
           OR &F8
           LD (HL),A
           INC HL
           RES 6,(HL)        ;RES 6,PFLAGT=NOT PAPER 9

SYN9SR1:   RST &18

CITEM:        CALL CITEMSR
              RET  C

              RST  &18
              CALL INSISCSC          ;CHECK FOR ,/; THEN SKIP
              JR CITEM

;CITEMSR - CALLED BY CITEM AND BASIC"S PRINT/INPUT
;EXIT WITH CY IF COLOUR ITEM NOT FOUND

CITEMSR:      CP &A1                 ;  INKTOK
              RET  C                 ;RET IF BELOW "INK"

              CP &A7                 ;  OVERTOK+1
              CCF
              RET  C                 ;RET IF ABOVE "OVER"

              LD C,A
              RST  &20               ;SKIP INK/PAPER ETC
              LD A,C

;CALLED BY PERMS
COTEMP4:      SUB &A1-16             ;INK. CHANGE TO CONTROL CODE RANGE (16-21)
              PUSH AF
              CALL EXPT1NUM          ;PARAM
              POP  BC                ;CONTROL CODE
              CALL RUNFLG
              RET NC                 ;ABORT WITH NC IF SYNTAX TIME
                                     ;"COLOUR ITEM DEALT WITH"
              PUSH BC
              CALL GETBYTE
              LD   D,A               ;PARAM TO D
              POP  AF                ;CONTINUE INTO PRCOITEM (ZX USED RST 8)


;CALLED BY PRINT WITH A=CONTROL CODE, D=PARAM
;NOTE: TRANSLATOR ALTERS INK/PAPER 8 OR 9 TO INK/PAPER 17/18. THERE WILL BE
;SOME FAILURES - E.G. INK N.
;INK I; BRIGHT B SELECTS INK I+8*B IN MODE 3
;BRIGHT IS IGNORED IN MODE 2, ALTHOUGH M0/1 SYS VARS ALTER.
;INK I WITH I>7 SELECTS INK I-8; BRIGHT 1
;OVER 0/1 ALTERS PFLAG AND OVERT, OVER 0-3 ALSO ALTERS GOVERT TO GIVE GRAB CMD"S
;XOR/AND OPTIONS.

PRCOITEM:     RST &30
              DW PRCOITEM2
              CALL COLEX             ;COLOUR EXPANSION TABLE IF NEEDED
              AND A                  ;NC SHOWS COLOUR ITEM DEALT WITH
              RET

PERMS:        CALL RUNFLG
              JR NC,PER2             ;JR IF NOT RUNNING

              XOR A
              LD (DEVICE),A          ;UPPER SCREEN
              CALL TEMPS

PER2:         LD A,(CURCMD)          ;GET CMD VALUE
              CALL COTEMP4           ;ALTER TEMP VALUES ACCORDING TO COLOUR CMDS
              CALL CHKEND

;CALLED BY CLS#

PER3:         LD HL,ATTRT
              LD DE,ATTRP

;USED BY SCREEN SCROLL

LDIR8:        LD BC,8
              LDIR                   ;COPY TEMP VALUES TO PERMS
              RET                    ;(ATTRT-GOVERT)


                                 ;CHLETCHK, TEMPS, DATA, RESTORE, CITEM, PERMS,
                                 ;POKE, DPOKE, PDPSR, CHKM23
