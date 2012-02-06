;USING.SAM
;POINTERS
;ENTRY: HL=LOCN (PAGED IN), CDE=ADJUSTMENT, CY IF SUBTRACT
;EXIT: PAGCOUNT/MODCOUNT=BLOCK SIZE TO MOVE, AHL=OLD WKEND

; XOI3:      LD B,3
 ;         LD IY,NUMENDP
  ;        JR X31

XOINTERS:  LD B,14           ;14 SYS VARS TO DO
           LD IY,SAVARSP     ;FIRST SYS VAR TO CONSIDER

X31:       EX AF,AF'         ;CY'=ADD/SUB FLAG
           LD (TEMPW4),BC
           LD (TEMPW5),DE
           EX DE,HL
           IN A,(251)
           AND &1F
           LD C,A
           BIT 6,D
           JR Z,PNT1

           INC C
           RES 6,D           ;MAKE LOCN IN SECTION C

PNT1:    ;  PUSH BC
;           PUSH DE           ;ADJUSTED LOCN
           LD A,(WKENDP)
           LD HL,(WKEND)
           BIT 6,H
           JR Z,PNT15

           INC A
           RES 6,H

PNT15:     PUSH AF           ;SAVE ORIG WKEND (END OF BLOCK TO MOVE)
           PUSH HL
           IN A,(251)
           PUSH AF
           CALL ADDRNV
           PUSH HL
           CALL PNLP         ;DO B SYS VARS
           LD A,C
           POP BC
           LD HL,(NVARS)
           AND A
           SBC HL,BC
           CALL NZ,SADJ      ;IF NVARS HAVE MOVED, PROGRAM HAS ALTERED, SO
                             ;CHECK ADDRESSES ON GOSUB/DO/PROC STACK.
                             ;NOTE: DELETE OF 16K EXACTLY WILL BE MISSED..
           LD C,A
           POP AF
           OUT (251),A
           LD A,(ELINEP)
           CALL SMBW         ;MAKE WORKSP AND
                             ;WKEND SAME BASE PAGE AS ELINE
           LD L,A
           LD A,(CLA+1)
           AND A
           LD A,L
           LD L,CHADP\256
           CALL Z,SMBS       ;IF RUNNING ELINE, MAKE CHAD SAME BASE AS ELINE

           POP HL
           POP AF            ;AHL=OLD WKEND
;           POP DE
 ;          POP BC            ;CDE=ORIG LOCN
           PUSH AF
           PUSH HL           ;OLD WORKEND
           CALL SUBAHLCDE    ;AHL=BLOCK SIZE TO MOVE IF MAKEROOM
           EX AF,AF'
           JR NC,PNBS        ;JR IF MAKEROOM

           EX AF,AF'
           LD BC,(TEMPW4)
           LD DE,(TEMPW5)
           CALL SUBAHLCDE    ;SUBTRACT BLOCK SIZE IF RECLAIM
           EX AF,AF'

PNBS:      EX AF,AF'
           RES 7,H
           LD (PAGCOUNT),A
           INC HL            ;ONE EXTRA (MODCOUNT MAY BE 4000H NOW)
           LD (MODCOUNT),HL
           POP HL
           POP AF            ;AHL=OLD WKEND
           RET


SMBW:      LD HL,WORKSPP
           CALL SMBS
           LD L,WKENDP\256
           CALL SMBS
           LD L,KCURP\256

SMBS:      PUSH AF
           XOR (HL)
           AND &1F
           JR Z,SMBS2

           DEC (HL)          ;IF WORKSPACE IS ON PAGE ABOVE, ADJUST
                             ;TO SAME BASE AS ELINE
           INC HL
           INC HL
           SET 6,(HL)

SMBS2:     POP AF
           RET

;STACK ADJUST
;80H=DO, 40H=PROC, 00=GOSUB

SADJ:      PUSH BC           ;OLD (STILL CORRECT!) NVARS
           LD C,A            ;CDE=LOCN
           LD HL,(BSTKEND)

SADJL:     LD A,(HL)         ;TYPE/PAGE
           CP &FF
           JR NZ,SADJ2

           POP HL            ;NVARS
           PUSH DE
           LD (TEMPW3),DE
           LD D,C
           EX AF,AF'
           CALL R1OFFCL
           DW AFLPS          ;ADJUST FOR-NEXT LOOPS
           EX AF,AF'
           POP DE
           LD A,C
           RET

SADJ2:     PUSH AF           ;SAVE B BITS 7-5 OF PAGE
           PUSH HL
           CALL ASSV         ;ADJUST SINGLE "SYS VAR"
           POP HL
           POP AF
           AND &E0
           OR (HL)
           LD (HL),A         ;KEEP TYPE BITS 7-5
           INC HL            ;SKIP PAGE
           INC HL
           INC HL            ;SKIP OFFSET
           INC HL            ;SKIP STAT
           AND &E0           ;ISOLATE TYPE BITS
           CP &40
           JR NZ,SADJL       ;JR IF NOT PROC

FPTLP:     LD A,(HL)
           INC HL
           OR (HL)
           JR NZ,FPTLP

           INC HL
           JR SADJL        ;LOOP WHEN PROC DOUBLE ZERO TERMINATOR FOUND



;SEND BYTE IN 'A' TO PRINTER

SNDA2:     PUSH BC
           PUSH AF
           LD BC,(LPTPRT1)   ;C=CONTROL PORT, B=1

SENDLP:    CALL BRKCR        ;STOP IF BREAK PRESSED
           IN A,(C)
           RRCA              ;TEST BIT 0
           JR C,SENDLP       ;LOOP TILL NOT BUSY

           DEC C
           POP AF
           OUT (C),A         ;DATA TO DATA PORT
           INC C
           OUT (C),B         ;STROBE SIGNAL TO CONTROL PORT
           DEC B
           OUT (C),B         ;CANCEL STROBE PULSE (ABOUT 2.6 USECS)
           POP BC
           RET

;HEAP ROOM
;ENTRY: BC=BYTES TO RESERVE (IF +VE) OR BYTES TO RELEASE (IF -VE)
;EXIT: DE=OLD HEAPEND (ROOM START). RELEASE OF >=HEAP SIZE MAKES IT EMPTY.
;E.G. LD BC,0C000H:CALL HEAPRM WILL MAKE HEAP MT.
;HL=NEW HEAPEND (ROOM+1). CAN BE CALLED WITH BC=0. NC INDICATES NOT ENOUGH
;ROOM. (HL=DEGREE OF OVERFLOW, 0+ BYTES). BC IS PRESERVED.

HEAPROOM:  LD HL,(HEAPEND)   ;(4200H TO ABOUT 4A00H)
           ADD HL,BC
           LD A,H
           CP &40
           JR NC,HEAPR2      ;JR IF HEAP NOT EMPTY AFTER ADJUSTMENT

           LD HL,(HPST)      ;EMPTY HEAP
           JR HEAPR3

HEAPR2:    LD DE,(BSTKEND)
           SBC HL,DE
           RET NC            ;RET IF NEW HEAPEND WOULD BE >=BSTKEND

           ADD HL,DE

HEAPR3:    LD DE,(HEAPEND)
           LD (HEAPEND),HL
           RET

;USING.SAM. USING AND INARRAY/INSTRING, LENGTH, STRING$
;*******************************************************************************
;INSTRING FUNCTION - E.G. PRINT INSTR(2,S$,T$) OR INSTR(S$,T$)


IMINSTR:      CALL SINSISOBRK     ;'('
              CALL EXPTEXPR       ;EXITS WITH Z IF $, NZ IF NUM, C IF RUNNING
              JR NZ,INSTR2        ;JR IF FOUND NUMBER FOR SEARCH START

              JR NC,INSTR3        ;AVOID STACKING DEFAULT OF 1 IF SYNTAX CHK

              DB CALC             ;S$
              DB STKONE           ;S$,1
              DB SWOP             ;1,S$
              DB EXIT

              JR INSTR3

INSTR2:       CALL EXPTCSTR        ;COMMA, SEARCH$

INSTR3:       CALL EXPTCSTRB      ;COMMA, TARGET$, ')'
              RET NC              ;DON'T RET HERE IF NOT RUNNING

              IN A,(URPORT)
              PUSH AF

;FETCH TARGET$, COPY TO PAGE 0 BUFFER IF LENGTH 1-255,
;FETCH SEARCH$. GET DE=S$ ST, BC=S$ LEN, A=T$ LEN, NC. S$ PAGED IN

              CALL SBFSR          ;COPY $ ON FPCS TO INSTBUF
                                  ;INV ARG IF T$ LEN >255. A=LEN, NC ON EXIT
              PUSH AF             ;T$ LEN, NC
              CALL GETSTRING      ;S$ ST TO DE, LEN TO BC, PAGE IN
              PUSH DE             ;S$ ST
              PUSH BC             ;S$ LEN
              CALL GETINT         ;START TO BC. A=C
              OR B
              JP Z,SWER2          ;ERROR IF POSN=0

              LD A,(INSTHASH)

              EXX
              LD C,A              ;C'=HASH OR EQUIVALENT
              EXX

              DEC  BC             ;POSN >=0
              POP HL              ;S$ LEN
              POP DE              ;S$ ST
              POP AF              ;T$ LEN
              CALL INARRAYEN
              POP AF
              JP OSBC             ;OUT (URPORT),A: JP STACKBC
            ;  OUT (URPORT),A
             ; JP STACKBC

INARRAYEN:    PUSH BC             ;POSN
              LD C,A              ;A=T$ LEN
              LD B,0              ;BC=T$ LEN
                                  ;DE=S$ START
                                  ;HL=S$ LEN
                                  ;STACK=POSN
              AND A
              JP Z,NOTFND2        ;NOT FOUND IF T$ LEN=0

              SBC HL,BC           ;SBC S$ LEN,T$ LEN
              POP BC              ;POSN
              JR C,NOTFND3H       ;NOT FOUND IF TARGET$ LEN > SEARCH$ LEN

              INC HL
              SBC HL,BC           ;SUB START POSN

NOTFND3H:     JP C,NOTFND3        ;JR IF START POSN TOO FAR UP SEARCH$

              EX DE,HL
              INC DE              ;DE=BYTES TO CHECK
              EX AF,AF'
              ADD HL,BC           ;MODIFY START-OF-SEARCH PTR NOW IN HL
              CALL C,PGOVERF      ;IF OVERFLOW, GET HL IN 8000-BFFF AREA

INSTBKLP:     CALL CHKHL          ;IF NEEDED, GET HL IN 8000-BFFF AREA
              LD A,D
              CP &3F
              JR C,MINSR          ;JR IF OK FOR A SEARCH WITHOUT OVERFLOW

              PUSH DE             ;LEN >=3F00
              LD DE,&3EFF
              CALL MINSR          ;SEARCH 3EFF BYTES
              JR NC,JUNKS         ;IF FOUND, JUNK PREV LEN, NC. BC=POSN

;              POP DE              ;JUNK PREVIOUS LEN
 ;             RET                 ;FOUND - BC=POSN FOUND, NC

              EX DE,HL            ;ELSE A'=T$ LEN
                                  ;DE=S$ PTR, HL=START POSN
              LD BC,&3EFF
              ADD HL,BC           ;INCR START POSN FOR NEW SEARCH
              EX (SP),HL          ;HL=PREV. LEN, (SP)=START POSN
              SBC HL,BC
              EX DE,HL            ;DE=REMAINING LEN TO SEARCH, HL=S$ PTR
              POP BC              ;START POSN
              JR INSTBKLP

;ENTRY: A'=T$ LEN, HL=S$ PTR, DE=BYTES TO SEARCH, BC=START POSN

MINSR:        EX AF,AF'
              PUSH AF
              LD (BCSTORE),BC
              CALL R1OFFCL
              DW R0INST

JUNKS:        EX AF,AF'
              POP AF
              EX AF,AF'
              RET

;E.G. LENGTH(1,A$) OR LENGTH(2,B())

IMLENGTH:  CALL SINSISOBRK
           CALL EXPT1NUM
           CALL INSISCOMA
           CALL R1OFFCL
           DW LENGSR         ;SETS  BIT 6,(FLAGS) FOR NUM/STR
           JP Z,VNFERR       ;ERROR IF NOT FOUND IN RUNTIME

           RST &18
           CALL EXCBRF       ;INSIST ON A$() OR XXX()
           EX AF,AF'         ;CY IF RUNNING
           BIT 5,C
           JR Z,IMLEN2       ;JR IF NOT NUMERIC ARRAY

           RST &18
           CALL EXCBRF       ;INSIST ON E.G. (1,AL() )
           RET NC            ;RET IF SYNTAX TIME

           JR IMLEN3

;E.G. ALPHA OR ASD$

IMLEN2:    EX AF,AF'
           RET NC            ;RET IF SYNTAX TIME

           JP P,IMLEN3       ;JR IF STRING VAR

           CALL GETBYTE
           AND A
           JR NZ,IMLERR

           LD HL,(MEMVAL)    ;ADDR OF NUMERIC VALUE

IMLENC:    LD  A,(MEMVAL+2)
           LD B,A
           JP ASBHL          ;ADJUST BHL TO REL PAGE, STACK

IMLEN3:    PUSH BC           ;C=T/L
           CALL GETBYTE
           POP BC
           LD HL,MEMVAL+3    ;PTR TO LEN DATA
           PUSH AF

;ENTRY: C=TYPE BYTE, HL PTS TO HEADER DATA
;EXIT: HL PTS TO START OF TEXT, DE=EL LEN,
;BC=NO OF ELS. SIMPLE STRINGS HAVE EL LEN OF 1.

           LD A,C
           AND &60
           LD A,(HL)
           INC HL
           LD C,(HL)
           INC HL
           JR NZ,ASSAR4      ;JR IF ARRAY

           RRCA              ;A=PAGES OF SIMPLE STRING LEN
           RRCA
           OR (HL)
           LD B,A            ;BC=STRING LEN ('EL NUMBERS')

ASSAR3:    LD DE,1           ;DE='ELEMENT' LEN
           JR ASSAR5

ASSAR4:    INC HL
           LD A,(HL)         ;DIMS
           INC HL
           LD C,(HL)
           INC HL
           LD B,(HL)
           DEC A
           JR Z,ASSAR3       ;IF DIMS=1, USE BC AS EL. NOS, 1 AS EL LEN

           INC HL            ;IF DIMS=2, DE IS LOADED WITH EL. LEN
           LD E,(HL)
           INC HL
           LD D,(HL)

ASSAR5:    LD A,L
           SUB (MEMVAL+2)\256 ;GET BYTES FROM DIMS TO TEXT
           LD HL,(MEMVAL)
           ADD A,L
           LD L,A
           JR NC,ASSAR6

           INC H

 ; HL=ARRAY ST, DE=EL. LEN, BC=EL NOS

ASSAR6:    POP AF
           AND A
           JR Z,IMLENC

           DEC A
           JP Z,STACKBC

           EX DE,HL
           DEC A
           JP Z,STACKHL

IMLERR:    RST &08
           DB 30             ;IOOR

;*******************************************************************************
;STRING$(N,A$)

IMSTRINGS: CALL EXBNCSB      ;(N,$)
           RET NC            ;RET IF SYNTAX TIME

           DB CALC           ;N,$
           DB STOD0          ;N
           DB DUP            ;N,N
           DB RCL0           ;N,N,$
           DB DUP            ;N,N,$,$
           DB LEN            ;N,N,$, LEN $
           DB SWOP23         ;N,$,N, LEN $
           DB MULT           ;N,$,RESULT LEN
           DB EXIT

           CALL GETINT
           OR B
           JR Z,STRINGSN     ;JR IF RESULT LEN ZERO

           LD A,B
           CP 2
           JP NC,STLERR      ;LIMIT LEN TO <512 CHARS TO FIT BUFFER

           PUSH BC           ;RESULT LEN
           CALL SBUFFET      ;COPY $ TO BUFFER, ERROR IF LEN >255. BC=LEN
           PUSH BC           ;$ LEN
           PUSH DE           ;BUFFER START
           CALL GETBYTE      ;N IN A
           POP HL            ;BUFFER START
           LD D,H
           LD E,L
           POP BC            ;$ LEN

STRSL:     PUSH BC
           PUSH HL
           LDIR              ;MAKE REPEATED COPIES FROM BUFFER START
                             ;(FIRST ONE ON TOP OF ITSELF!)
           POP HL
           POP BC
           DEC A
           JR NZ,STRSL

           POP BC            ;RESULT LEN
           JP CWKSTK         ;COPY RESULT TO WKSPACE AND STACK PARAMS.

;NULL STRING RESULT IF EG STRING$(0,"AA") OR STRING$(10,"")
;BC=0

STRINGSN:  DB CALC           ;N,$
           DB DROP           ;N
           DB DROP           ;
           DB EXIT

           JP STACKBC

;CIRCLE/FILL SR. UNSTACKS COORDS TO B,C (NC) OR FIDDLED COORDS IN BC (CY)
; AND TEMPW1 IS SET UP AS OFFSET TO X COORD IF THIN PIX.

CIFILSR:   CALL R1OFFCL      ;v2.6
           DW  GTFCOORDS     ;Y COORD IN B, X IN C, OR X IN HL AND CY IF THIN
           JR C,CIFISR2      ;JR IF THINPIX

           LD A,(CURCMD)
           LD HL,MODE
           ADD A,(HL)
           SUB &EB+2         ;FILLTOK+2
           SCF
           CCF               ;NC
           RET NZ            ;RET UNLESS: MODE 2, FATPIX, FILL
                             ;(CUR CMD MUST BE 9A,9D OR EB)

           LD L,C
           LD H,A
           ADD HL,HL         ;X=X*2. (ALWAYS USE THIN PIX SYSTEM FOR FILL)

;HL=X, B=Y

CIFISR2:   LD A,H
           AND A
           LD A,L
           JR NZ,THINC2

;X IS <=00FF
           CP &80
           JR C,THINC3      ;IF X IS <80H, JR, USE L (X LSB) AS X, ADD HL OF 0

           JR THINC4

;X>=0100H
THINC2:    CP &80
           JR C,THINC4       ;IF X>017FH, NO JR, USE L AS X, ADD HL OF 0100H

;X>017FH
THINC3:    LD C,L
           LD L,0
           JR THINC5

;X IS BETWEEN 80H AND 017FH

THINC4:    LD DE,&80
           LD C,E            ;PRETEND CENTRE X IS 128
           AND A
           SBC HL,DE         ;HL=0-FF=DISP TO ADD TO 80H CENTRE TO GET REAL CRD

THINC5:    LD (TEMPW1),HL
           SCF               ;"THINPIX"
           RET

;PIXEL ROLL/SCROLL SR

CRBBFN:    PUSH DE
           PUSH HL
           LD B,A
           LD DE,&672C       ;2ND BYTE OF RRD/INC L
           DEC C
           JR NZ,CRBB2       ;JR IF MOVE RIGHT

           LD DE,&6F2D       ;2ND BYTE OF RLD/DEC L

CRBB2:     LD A,&ED          ;1ST BYTE OF RLD OR RRD
           LD HL,CDBUFF

CRBBL:     LD (HL),A
           INC HL
           LD (HL),D
           INC HL
           LD (HL),E
           INC HL
           DJNZ CRBBL

           LD (HL),&C9       ;"RET"
           POP HL
           POP DE
           RET

CRTBF:     PUSH BC
           DEC C
           LD C,&A8
           JR NZ,CRTB2       ;JR IF LDD WANTED

           DB &0E            ;"JR+1"

CRTBFI:    PUSH BC

CRTB1:     LD C,&A0          ;ELSE LDI

CRTB2:     PUSH HL
           LD B,A
           LD A,&ED
           LD HL,CDBUFF

CRTBL:     LD (HL),A
           INC HL
           LD (HL),C
           INC HL
           DJNZ CRTBL

           LD (HL),&C9       ;"RET"
           POP HL
           POP BC
           RET

;DRCURVE

;DRAW TO X,Y,Z. MEM0 HOLDS Z, REGS HOLD FIDDLED COORDS (NOW DISPLACEMENTS).
;PUT COORDS BACK ON STACK, REPLACE Z.
;ENTRY: B=Y DIFF, C (OR HL IF THIN)=X DIFF, D=SGN Y, E=SGN X (01/FF)

DRTCRV:    LD A,(THFATT)
           AND A             ;Z IF THIN
           LD A,E            ;SGN X
           NEG
           LD E,B
           PUSH DE           ;SGN Y, Y DIFF
           EX DE,HL
           JR Z,DRT2         ;JR IF THIN PIX - DE=X DIFF

           LD D,0
           LD E,C            ;DE=X DIFF

DRT2:      LD HL,(STKEND)
           CALL STORADE      ;FIDDLE SGN AND PUT DE ON FPCS
           INC HL
           INC HL            ;PT TO NEXT NUMBER AREA ON FPCS
           POP DE            ;SGN Y, Y DIFF
           LD A,D
           LD D,0
           CALL STORADE
           INC HL
           INC HL
           LD (STKEND),HL

           DB CALC
           DB RCL0           ;GET CURVATURE BACK
           DB EXIT

DRCURVE:   DB CALC           ;X,Y,-Z
           DB NEGATE         ;X,Y,Z (REVERSE ANGLE SO CURVES IN RIGHT DIRECTION)
           DB STO5           ;X,Y,Z (M5=Z)
           DB STKHALF        ;X,Y,Z,0.5
           DB MULT           ;X,Y,Z/2
           DB SIN            ;X,Y,SIN(Z/2)
           DB DUP            ;X,Y,SIN(Z/2),SIN(Z/2)
           DB NOT            ;X,Y,SIN(Z/2),1/0
           DB JPTRUE        ;JP IF APPROX. A STRAIGHT LINE, DROPEX
DRHLB:     DB DROPEX-DRHLB

           DB STOD0          ;X,Y  (M0=SIN(Z/2)
           DB NEGATE         ;X,Y  (REVERSED BECAUSE OF INVERSE Y AXIS)
           DB SWOP           ;Y,X
           DB DUP            ;Y,X,X
           DB ABS            ;Y,X,ABS X
           DB SWOP           ;Y,ABS X,X
           DB SWOP13         ;X,ABS X,Y
           DB DUP            ;X,ABS X,Y,Y
           DB ABS            ;X,ABS X,Y,ABS Y
           DB SWOP23         ;X,Y,ABS X,ABS Y
           DB ADDN           ;X,Y,ABS X+ABS Y
           DB RCL0           ;X,Y,ABS X+ABS Y,SIN(Z/2)
           DB DIVN           ;X,Y,(ABS X+ABS Y)/SIN(Z/2)
           DB ABS            ;X,Y,FF
           DB RCL0           ;X,Y,FF,SIN(Z/2)
           DB SWOP           ;X,Y,SIN(Z/2),FF
           DB DUP            ;X,Y,SIN(Z/2),FF,FF
           DB STKFONE        ;X,Y,SIN(Z/2),FF,FF,1
           DB SUBN           ;X,Y,SIN(Z/2),FF,FF-1
           DB GRTE0          ;X,Y,SIN(Z/2),FF,1/0 (1 IF FF>=1)
           DB JPTRUE         ;JP IF NOT A STRAIGHT LINE, DRCURV3
           DB &07

           DB DROP           ;X,Y,SIN(Z/2)

DROPEX:    DB DROP           ;X,Y
           DB EXIT

           JP LINEDRAW

DRCURV3:   DB DUP            ;X,Y,SIN(Z/2),FF,FF
           DB SQR            ;X,Y,SIN(Z/2),FF,SQR FF
           DB ONELIT
           DB &02
           DB SWOP           ;"     "     ,FF,2,SQR FF
           DB DIVN           ;"     "     ,FF,2/SQR FF
           DB RCL5           ;"     "     ,FF,2/SQR FF,Z
           DB SWOP           ;"     "     ,FF,Z,2/SQR FF
           DB DIVN           ;            ,FF,Z*SQR FF/2
           DB ABS            ;            ,FF,ABS(Z*SQR FF/2)
           DB EXIT

           CALL FPTOA
           LD B,&FC
           JR C,DRCURV4      ;JR IF TOO BIG

           AND B             ;A=00-FC
           ADD A,4           ;A=04-00H
           JR Z,DRCURV4      ;JR IF TOO BIG

           LD B,A

DRCURV4:   PUSH BC           ;ARCS IN B

           DB CALC           ;            ,FF
           DB RCL5           ;            ,FF,Z
           DB STKBREG        ;            ,FF,Z,ARCS
           DB DIVN           ;            ,FF,Z/ARCS
           DB DUP            ;            ,FF,Z/ARCS,Z/ARCS
           DB SIN            ;            ,FF,Z/ARCS,SIN(Z/ARCS)
           DB STOD4          ;            ,FF,Z/ARCS
           DB DUP            ;            ,FF,Z/ARCS,Z/ARCS
           DB STKHALF        ;            ,FF,Z/ARCS,Z/ARCS,0.5
           DB MULT           ;            ,FF,Z/ARCS,Z/2*ARCS
           DB SIN            ;            ,FF,Z/ARCS,SIN(Z/2*ARCS)
           DB STO1           ;            ,FF,Z/ARCS,SIN(Z/2*ARCS)
           DB SWOP           ;            ,FF,SIN(Z/2*ARCS),Z/ARCS
           DB STOD0          ;            ,FF,SIN(Z/2*ARCS)
           DB DUP            ;            ,FF,SIN(Z/2*ARCS),SIN(Z/2*ARCS)
           DB MULT           ;            ,FF,SIN(Z/2*ARCS)*SIN(Z/2*ARCS)
           DB STKHALF        ;            ,FF,SIN(Z/2*ARCS)*SIN(Z/2*ARCS),0.5
           DB DIVN           ;            ,FF,SIN(Z/2*ARCS)*SIN(Z/2*ARCS)*2
           DB STKFONE        ;            ,FF,SIN(Z/2*ARCS)*SIN(Z/2*ARCS)*2,1
           DB SWOP           ;            ,FF,1,SIN(Z/2*ARCS)*SIN(Z/2*ARCS)*2
           DB SUBN           ;            ,FF,1-SIN(Z/2*ARCS)*SIN(Z/2*ARCS)*2
           DB STOD3          ;            ,FF

           DB DROP           ;X,Y,SIN(Z/2)
           DB RCL1           ;X,Y,SIN(Z/2),SIN(Z/2*ARCS)
           DB SWOP           ;X,Y,SIN(Z/2*ARCS),SIN(Z/2)
           DB DIVN           ;X,Y,SIN(Z/2)/SIN(Z/2*ARCS) (CALL IT V)
           DB STOD1          ;X,Y (M1=V)
           DB SWOP           ;Y,X
           DB DUP            ;Y,X,X
           DB RCL1           ;Y,X,X,V
           DB MULT           ;Y,X,X*V
           DB STOD2          ;Y,X            (M2=X*V)
           DB SWOP           ;X,Y
           DB DUP            ;X,Y,Y
           DB RCL1           ;X,Y,Y,V
           DB MULT           ;X,Y,Y*V
           DB STOD1          ;X,Y            (M1=Y*V)
           DB RCL5           ;X,Y,Z
           DB RCL0           ;X,Y,Z,Z/A
           DB SUBN           ;X,Y,Z-Z/A
           DB STKHALF        ;X,Y,Z-Z/A,0.5
           DB MULT           ;X,Y,(Z-Z/A)/2 (CALL IT T)
           DB DUP            ;X,Y,T,T
           DB SIN            ;X,Y,T,SIN T
           DB STO5           ;X,Y,T,SIN T    (M5=SIN T)
           DB SWOP           ;X,Y,SIN T,T
           DB COS            ;X,Y,SIN T,COS T
           DB STOD0          ;X,Y,SIN T      (M0=COS T)
           DB RCL1           ;X,Y,SIN T,Y*V
           DB MULT           ;X,Y,SIN T*Y*V
           DB RCL0           ;X,Y,SIN T*Y*V,COS T
           DB RCL2           ;X,Y,SIN T*Y*V,COS T,X*V
           DB MULT           ;X,Y,SIN T*Y*V,COS T*X*V
           DB ADDN           ;X,Y,SIN T*Y*V+COS T*X*V (CALL IT P)
           DB RCL1           ;X,Y,P,Y*V
           DB SWOP           ;X,Y,Y*V,P
           DB STOD1          ;X,Y,Y*V        (M1=P)
           DB RCL0           ;X,Y,Y*V,COS T
           DB MULT           ;X,Y,Y*V*COS T
           DB RCL5           ;X,Y,Y*V*COS T,SIN T
           DB RCL2           ;X,Y,Y*V*COS T,SIN T,X*V
           DB MULT           ;X,Y,Y*V*COS T,SIN T*X*V
           DB SUBN           ;X,Y,Y*V*COS T-SIN T*X*V (CALL IT J)
           DB STO2           ;X,Y,J
           DB ABS            ;X,Y,ABS J
           DB RCL1           ;X,Y,ABS J,P
           DB ABS            ;X,Y,ABS J,ABS P
           DB ADDN           ;X,Y,ABS J+ABS P
           DB DROP           ;X,Y
           DB EXIT           ;(DE)=EXP OF ABS J+ABS P

           POP BC
           LD A,(DE)
           CP &81
           JP C,LINEDRAW

           DB CALC
           DB SWOP           ;Y,X
           DB LKADDRW
           DW XCOORD         ;Y,X,XCOORD
           DB STO0           ;Y,X,XCOORD
           DB ADDN           ;Y,X+XCOORD
           DB SWOP           ;X+XCOORD,Y
           DB LKADDRB
           DW YCOORD         ;X+XCOORD,Y,YCOORD
           DB STO5
           DB ADDN           ;X+XCOORD,Y+YCOORD
           DB RCL0           ;X+XCOORD,Y+YCOORD,XCOORD
           DB RCL5           ;X+XCOORD,Y+YCOORD,XCOORD,YCOORD (END/START COORDS)
           DB EXIT

           DJNZ CUENTRY

           JR CURVEND

CURVLP:    DB CALC
           DB RCL1           ;PREV X INC
           DB DUP
           DB RCL3
           DB MULT
           DB RCL4
           DB RCL2           ;PREV Y INC
           DB MULT
           DB SUBN
           DB STOD1          ;NEW X INC
           DB RCL4
           DB MULT
           DB RCL3
           DB RCL2
           DB MULT
           DB ADDN
           DB STOD2          ;NEW Y INC
           DB EXIT

CUENTRY:   DB CALC
           DB STOD0
           DB RCL1           ;X INC
           DB ADDN
           DB DUP
           DB LKADDRW
           DW XCOORD         ;PEEK X COORD
           DB SUBN
           DB RCL2           ;Y INC
           DB RCL0
           DB ADDN
           DB STO0
           DB SWOP
           DB LKADDRB
           DW YCOORD         ;PEEK Y COORD
           DB RCL0
           DB SUBN
           DB EXIT

           PUSH BC
           CALL R1OFFCL
           DW DRAWFD         ;DRAW A LINE USING DISPS. ON FPCS, WITH XRG/YRG
           POP BC            ;ADJUSTMENTS
           DJNZ CURVLP

CURVEND:   DB CALC
           DB DROP
           DB DROP
           DB SWOP
           DB LKADDRW
           DW XCOORD
           DB SUBN
           DB SWOP
           DB LKADDRB
           DW YCOORD
           DB SWOP
           DB SUBN
           DB EXIT

LINEDRAW:  CALL R1OFFCL
           DW DRAWFD
           JP TRCURP
