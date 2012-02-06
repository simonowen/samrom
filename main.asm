;SAM COUPE ROM 3.0 SOURCE CODE Copyright Andrew J.A. Wright 1989-90
;MAIN.SAM

PAGE0:     EQU 0
PAGE1:     EQU 1
PAGE1F:    EQU &1F

           ORG &0000

L0000:     DI
           JP MINITH

           POP HL            ;04 FOWIA
HLJUMP:    JP (HL)

IYJUMP:    JP (IY)

;RST 08H - ERRORS

           NOP               ;SOME HARDWARE MIGHT LIKE THIS...
           EXX
           JP ERROR2

NRWRITE:   LD (HL),A
           RET

           DB 30             ;0FH. ROM VERSION NUMBER

;RST 10H - PRINT A

           JP RST102

;0013H - PRINT BC FROM (DE)

PRINTSTR:  JP SOP2           ;13

BCJUMP:    PUSH BC           ;16
           RET

;RST 18 - GET CHAR. SKIP ALL CONTROL CODES EXCEPT CR.

GETCHAR:   LD HL,(CHAD)      ;18
           PUSH BC
           JP GETCHAR1

           DB 0

;RST 20H - NEXT CHAR         ;20

NEXTCHAR:  LD HL,(CHAD)
           INC HL
           PUSH BC
           JP NEXTCHAR1

           JP FPCP2          ;28 FLOATING POINT CALCULATOR

DELAYB:    DJNZ DELAYB       ;2B

IXJUMP:    JP (IX)           ;2D

           DS 1

           JP RST30L2        ;30

           PUSH DE           ;33 DEJUMP
           RET

DELYB:     DJNZ DELYB

           RET

           PUSH AF           ;38H INTERRUPTS
           PUSH BC
           IN A,(STATPORT)   ;READ IN ABOUT 36Ts (56) 6 (9) USEC
           LD C,A
           IN A,(250)
           LD B,A            ;B=LMPR, C=STATUS
           PUSH HL
           LD A,PAGE1F+&40
           OUT (250),A       ;BOTH ROMS ON, PAGE ZERO IN SECTION B
           LD HL,(ANYIV)
           JP (HL)

ANYI:      LD (SPSTORE),SP   ;ARRIVE IN ABOUT 113 (139) T 19 (23)
           LD SP,INTSTK
           CALL INTS
           LD SP,HL
           OUT (250),A
           POP HL
           POP BC
           POP AF
           EI
           RET

           DB 0

           OUT (251),A
           JP (HL)

DELBC:     LD A,B
           OR C
           DEC BC
           JR NZ,DELBC

           RET

           NOP

;NON-MASKABLE INTERRUPT

           PUSH AF
           PUSH HL           ;REGS SAVED IN ORIG PAGE - MAY CORRUPT 4 BYTES
                             ;IF EG SP BEING USED TO CLS
           IN A,(250)
           LD H,A
           LD A,PAGE1F
           OUT (250),A       ;ROM0 ON, ROM1 OFF, PAGE 0 AT 4000H
           LD A,H
           LD (NMILRP),A     ;SAVE ORIG LRPORT STATUS
           LD (NMISP),SP
           LD SP,NMISTK      ;PUT STACK SOMEWHERE SAFE
           LD HL,(NMIV)
           LD A,H
           OR L
           CALL NZ,HLJUMP    ;NORMALLY SUPER-BREAK

           LD SP,(NMISP)
           LD A,(NMILRP)
           OUT (250),A
           POP HL
           POP AF
           RETN

           LDIR
           RET

           LDDR
           RET

           CPIR
           RET

           CPDR
           RET

           OTIR
           RET

           OTDR
           RET

;READ, SKIP NUMBER

RDCN:      LD A,(HL)

;SKIP 5-BYTE INVIS. NUMBER IN BASIC LINE. IF SKIP, TAKES ABOUT 34 Ts. vs 54 ZX

NUMBER:    CP &0E
           RET NZ         ;RET IF NOT NUMBER MARKER

           LD A,6
           ADD A,L
           LD L,A
           LD A,(HL)
           RET NC         ;RET IF HL OK AND A=NEXT CHAR

           INC H

NRREAD:    LD A,(HL)
           RET

RDDE:      LD A,(DE)
           RET

MINITH:    LD B,250

IDEL:      DEC BC
           LD A,B
           OR C
           JR NZ,IDEL        ;DELAY APPROX 1.2 MSEC *B

           LD A,&40+PAGE1F
           OUT (250),A       ;ROM1 ON
           JP MNINIT

NEXTCHAR1: LD (CHAD),HL

GETCHAR1:  IN A,(250)
           LD B,A
           AND &BF           ;FORCE BIT 6 LOW
           OUT (250),A       ;ROM1 OFF

GTCH1:     LD A,(HL)
           CP &21
           JR C,GTCH3         ;JR IF 00-20H

GTCH2:     LD C,250
           OUT (C),B         ;ORIG ROM1 STATUS
           POP BC
           RET

GTCH3:     CP &0D
           JR Z,GTCH2          ;RET IF CR

           INC HL              ;SKIP SPACES AND CONTROL CODES
           LD (CHAD),HL
           JR GTCH1

NXCHAR:    LD HL,(CHAD)
           INC HL
           LD (CHAD),HL
           LD A,(HL)
           RET


;FLOATING-POINT CALCULATOR

FPCP2:     EX (SP),IX        ;SAVE PTR IN USE BY ANY CALLING RST 28H ROUTINE
                             ;MAKE PTR=ADDR AFTER RST 28H
           LD (BCREG),BC
           IN A,(250)
           PUSH AF           ;ORIG PORT STATUS
           OR &40            ;BIT FOR ROM1=HI (ACTIVE)
           OUT (250),A
           CALL FPCMAIN
           POP AF
           EX (SP),IX        ;GET ORIG IX AS PTR, (SP) PTS. PAST EXIT OR EXIT2

LRPOUT:    LD BC,(BCREG)
           OUT (250),A
           RET

           DS &0100-$


;JUMP TABLE AT 0100H

           RST &30
           DW JSCRN-&8000    ;0100
           JP JSVIN          ;0103
           RST &30
           DW HEAPROOM-&8000 ;0106
           JP WKROOM         ;0109
           JP MKRBIG         ;010C  OPEN ABC AT HL
           JP CALBAS         ;010F  CALL BASIC LINE
           JP SETSTRM        ;0112  SET STREAM IN A REG
           JP POMSG          ;0115  O/P MSG A FROM LIST AT DE
           JP EXPT1NUM       ;0118  EXPECT A NUMERIC EXPR. AT (CHAD)
           JP EXPTSTR        ;011B  EXPECT A STRING EXPR AT (CHAD)
           JP EXPTEXPR       ;011E  EXPECT AN EXPRESSION AT (CHAD)
           JP GETINT         ;0121  UNSTACK WORD FROM CALCULATOR STACK TO
                             ;      BC. HL=BC, A=C
           JP STKFETCH       ;0124  GET STRING PARAMS. A=START PAGE, DE=START
                             ;      BC=LEN
           JP STKSTORE       ;0127  STACK STRING PARAMS
           JP SBUFFET        ;012A  UNSTACK STRING PARAMS AND COPY TO BUFFER IN
                             ;      SYS PAGE. ERROR IF >255 BYTES

           JP FARLDIR        ;012D  MOVE (PAGCOUNT/MODCOUNT) BYTES FROM PAGE A,
                             ;      HL TO PAGE C, DE, USING LDIR
           JP FARLDDR        ;0130

           JP JPUT           ;0133
           JP JGRAB          ;0136
           JP JPLOT          ;0139
           JP JDRAW          ;013C
           JP JDRAWTO        ;013F
           JP JCIRCLE        ;0142
           JP JFILL          ;0145
           JP JBLITZ         ;0148
           JP JROLL          ;014B
           JP CLSBL          ;014E CLEAR ENTIRE SCREEN IF A=0, ELSE CLEAR WINDOW
           JP CLSLOWER       ;0151
           RST &30
           DW JPALET-&8000   ;0154
                             ;A=LINE (OR FFH IF NONE) B/C=COLOURS, E=PAL. ENTRY.
           RST &30
           DW JOPSCR-&8000   ;0157

MODET:     RST &30
           DW MODPT2-&8000   ;015A
           RST &30
           DW JTCOPY-&8000   ;015D  TEXT COPY
           RST &30
           DW JGCOPY-&8000   ;0160  GRAPHICS COPY

           JP RECLAIM2       ;0163
           JP KBFLUSH        ;0166
           JP READKEY        ;0169
                             ;READ KEYBOARD, FLUSH BUFFER (INKEY$). Z, NC=NONE
                             ;CY, NZ IF A=KEY
           JP KYIP2          ;016C **
                             ;WAIT FOR A QUEUED KEY IN A.

           RST &30
           DW BEEPP2-&8000   ;016F
                             ;DO DE-1 CYCLES AT PERIOD HL 8-T UNITS

           RST &30
           DW SABYTES-&8000  ;0172

           RST &30
           DW LDBYTES-&8000  ;0175

JLDVD:     RST &30
           DW LDVD2-&8000    ;0178  LOAD (IF CY) OR VERIFY CDE AT HL DISC/TAPE

           JP EDGE2          ;017B  TAPE EDGE TIMER

JPFSTRS:   RST &30
           DW PFSTRS-&8000   ;017E  STR$ OF FPCS TO BUFFER

SENDA:     RST &30
           DW SNDA2-&8000    ;0181 SEND BYTE IN A TO PRINTER

;NEW TO V11

           RST &30
           DW IMSCSR-&8000   ;0184 SCREEN$

           JP GRCOMP         ;0187 GRAPHIC COPY SR

JGTTOK:    RST &30
           DW GETTOKEN-&8000 ;018A MATCH FOR A-1 WORDS FROM LIST AT HL+1

           RST &30
           DW JCLSCR-&8000   ;018D

;MODE 1/MODE 2/MODE 3/MODE 4
;DOES FULL CLS, SETS UP EXPANSION TABLE IF NEEDED

MODECMD:   CALL SYNTAX6      ;INSIST ON A NUMBER

           LD DE,&0400+34
           CALL LIMDB        ;ALLOW ORIG OF 1-4, DEC
           JR MODET

OTCD:      LD E,&30
           ADD A,E

RST102:    PUSH IX
           PUSH HL
           PUSH DE
           PUSH BC
           LD HL,(CURCHL)
           CALL HLJPI
           POP BC
           POP DE
           POP HL
           POP IX
           RET

S16OP:     EX AF,AF'
           PUSH AF
           LD BC,S16OSR
           CALL R1OF2        ;CALL S16 O/P (IN ROM0) WITH ROM1 OFF
           POP AF
           EX AF,AF'
           RET

INPUTAD:   EXX
           PUSH HL
           LD HL,(CURCHL)
           INC HL
           INC HL
           CALL HLJPI
           POP HL
           EXX
           RET

HLJPI:     LD E,(HL)
           INC HL
           LD D,(HL)
           EX DE,HL
           JP (HL)

PRMAIN:    RST &30
           DW PROM1-&8000    ;JP MAIN PRINT ROUTINE IN ROM1

RST30L2:   EX (SP),HL        ;GET CALLER'S ADDRESS IN HL
           PUSH AF
           LD A,H
           CP &40
           JR NC,RST30L4     ;JR IF CALLED FROM OUTSIDE ROM0 - USER

           LD (BCSTORE),BC
           LD C,(HL)
           INC HL
           LD B,(HL)
           INC HL
           BIT 7,B
           JR NZ,RST30L3     ;JR IF NORMAL 'CALL ROM1'

           SET 7,B           ;ELSE BIT 7 SHOWED 'JP ROM1'
           POP AF
           POP HL            ;ORIG HL - JUNK 1 RET ADDR
           JR R1ONCLBC

RST30L3:   POP AF
           EX (SP),HL

R1ONCLBC:  EX AF,AF'
           IN A,(250)
           PUSH AF
           OR &40
           JR R1OFON

RST30L4:   POP AF
           EX (SP),HL
           PUSH HL
           LD HL,(RST30V)
           EX (SP),HL
           RET              ;'RET' TO VECTOR, ALL REGS INTACT

;CALL PARAM ADDR WITH ROM1 OFF. JUNK CALLING ADDR SO EQUIV. OF 'JUMP'
;(ORIG ROM1 STATUS STILL RESTORED, THOUGH)

R1OFFJP:   EX (SP),HL
           LD C,(HL)
           INC HL
           LD B,(HL)
           POP HL
           JR R1OFFCLBC

;CALL PARAM ADDR WITH ROM1 OFF. CALL CAN COME FROM ANYWHERE
;ORIG ROM1 STATUS RESTORED AT END

R1OFFCL:   EX (SP),HL
           LD C,(HL)
           INC HL
           LD B,(HL)
           INC HL
           EX (SP),HL        ;BC=PARAMETER

;R1OFFCLBC. CALL BC WITH ROM1 OFF. ALL REGS CARRIED IN EXCEPT AF' (CORRUPT)
;ALL REGS CARRIED OUT EXCEPT AF'. ORIG ROM1 STATUS RESTORED AT END

R1OFFCLBC: EX AF,AF'

R1OF2:     IN A,(250)
           PUSH AF           ;ORIG URPORT STATUS
           AND &BF           ;ROM1 BIT OFF

R1OFON:    OUT (250),A       ;ROM1 OFF/ON
           EX AF,AF'
           CALL LDBCJP
           EX AF,AF'
           POP AF
           OUT (250),A
           EX AF,AF'
           RET

LDBCJP:    PUSH BC
           LD BC,(BCSTORE)
           RET


;TURN ROM 1 OFF, JP (BC)

R1XJP:     IN A,(250)
           AND &BF
           OUT (250),A
           PUSH BC
           RET

;STRING O/P - FROM 0013H
;ENTRY: DE PTS TO DATA, SWITCHED IN. BC=LEN
;ACTION: DO NOTHING IF BC=0, ELSE PRINT BC BYTES FROM (DE)
;EXIT: DE PTS TO JUST PAST LAST BYTE ON EXIT, BC=0, HL CORRUPT

SOP2:      PUSH DE
           LD HL,(CURCHL)    ;FETCH AN OUTPUT ADDRESS POINTER
           LD E,(HL)
           INC HL
           LD D,(HL)
           EX DE,HL
           POP DE            ;DE=SRC, HL=CHANNEL
           IN A,(250)
           PUSH AF
           AND &BF           ;ROM1 OFF
           OUT (250),A
           LD A,(HL)
           CP &40
           JR NZ,SOP3        ;JR IF SPECIAL STRING OUTPUT NOT PROVIDED BY
                             ;CHANNEL - USE MULTIPLE CALLS OF RST 10H.
           INC HL
           INC HL
           INC HL            ;SKIP '40H, JR XX' TO PT TO STRING O/P ROUTINE
           CALL HLJUMP
           JR SOP4

SOPL:      LD A,(DE)
           RST &10
           INC DE
           LD A,D
           CP &C0
           CALL NC,INCURPDE

SOP3:      LD A,B
           OR C
           DEC BC
           JR NZ,SOPL

SOP4:      POP AF
           OUT (250),A
           RET

R1OFRD:    PUSH BC
           CALL R1OFFCL
           DW NRREAD
           POP BC
           RET

JSVIN:     EXX
           POP HL            ;RET ADDR
           LD E,(HL)
           INC HL
           LD D,(HL)
           INC HL
           PUSH HL           ;RET ADDR TO CALLER
           LD C,A            ;SAVE ENTRY A BRIEFLY
           IN A,(250)
           LD B,A            ;ENTRY LRPORT
           LD A,&1F
           DI
           OUT (250),A       ;SYS PAGE IN AT 4000H, ROM0 ON, ROM1 OFF
           LD (JVSP),SP
           LD SP,ISPVAL-&40
           EI
           PUSH BC
           LD HL,JSVIN2      ;RET ADDR TO PT 2
           PUSH HL
           PUSH DE           ;PARAM TO CALL
           LD A,C            ;ENTRY A
           EXX
           RET               ;TO PARAM ADDR WITH MAIN REGS INTACT
                             ;THEN 'RET' TO VARSIN2
JSVIN2:    EX AF,AF'
           POP AF
           DI
           LD SP,(JVSP)
           OUT (250),A       ;ORIG
           EI
           EX AF,AF'
           RET



;INITIAL STREAM DISPLACEMENTS

STRMTAB:   DB 26,21,1,6,11   ;B,$,K,S,R (FIXED) FOR STREAMS FB,FC,FD,FE,FF
           DB 1,1,6,16       ;K,K,S,P FOR STREAMS 0,1,2,3
