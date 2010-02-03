;GRAPH1.SAM

;PLOT.SAM

JPLOT:     LD A,(THFATT)
           AND A
           JR NZ,JPLOT3      ;JR IF FAT

           JR THINPLOT

PLOT:      CALL SYNTAX9
           CALL CHKEND

;ENTRY POINT FOR BLITZ

PLOTFD:    CALL GTFCOORDS    ;CORRECT Y SO 0 AT TOP, 191 AT BOT. CHECK X AND Y
           JP C,THINPLOT     ;JR IF THIN PLOT - HL=X, B=Y. ELSE C=X, B=Y

           LD E,1            ;BLITZ CODE FOR PLOT (FAT)
           CALL GRAREC

JPLOT3:    CALL SPSS         ;SAVE PAGE, SELECT SCREEN
           LD H,C
           LD L,B
           LD (YCOORD),HL    ;UPDATE Y COORD AND X COORD LSB
           LD H,B            ;H=Y WITH 0 AT TOP
           LD L,C            ;L=X
           CALL SETIY
           EXX
           LD D,A            ;INK TO D'
           EXX
           LD IX,TRCURP
           JP (IY)

;*******************************************************************************
;THIN PIXEL PLOT. PLOT HL,B. HL CHECKED ALREADY, 00-01FF

THINPLOT:  CALL SPSS         ;USED BY PLOT
           CALL TDPLOT
           JP TRCURP

TDPLOT:    LD (XCOORD),HL    ;USED BY DRAW - AVOIDS PAGING SCREEN IN AND OUT
           LD A,B
           LD (YCOORD),A

M2CTPLOT:  POP IX            ;USED BY CIRCLE - AVOIDS ALTERING COORD VARS.
           PUSH HL
           PUSH DE
           PUSH BC
           LD A,L            ;A=X
           RR H
           RR L              ;L=X/2
           LD H,B            ;H=Y
           AND &03           ;ISOLATE BIT OFFSET
           INC A
           LD B,A            ;B COUNTS TIMES TO ROTATE MASK
           SCF
           RR H
           RR L              ;HL=Y/2+X/4+8000H=ADDRESS
           LD A,&FC          ;BIT MASK

P80RLP:    RRCA
           RRCA
           DJNZ P80RLP

           LD C,A
           LD DE,(OVERT)       ;E=00/01 FOR OVER 0/1, D=00/FF FOR INVERSE 0/1
           LD A,(M23INKT)
           INC D
           JR NZ,M2TPIN0       ;JR IF INVERSE 0

           LD A,(M23PAPT)

M2TPIN0:   LD B,A
           DEC E
           JR Z,M2TPOV1         ;JR IF OVER 1

           LD A,(HL)
           XOR B
           AND C                ;FORCE PIXEL TO INK OR PAPER COLOUR
           XOR B
           JR M2TPC

M2TPOV1:   DEC D
           JR NZ,DRPLEND2       ;DO NOTHING IF INVERSE 1, OVER 1

           LD A,C
           CPL
           XOR (HL)             ;REVERSE PIXEL

M2TPC:     LD (HL),A
           JR DRPLEND2

;*******************************************************************************
;MODE 0 PLOT - USED BY DRAW. PLOT L,H

M0DPLOT:   PUSH HL
           PUSH DE
           PUSH BC
           LD B,H
           LD C,L
           CALL M0PIXAD
           LD B,A
           JR M01DPCOM

;******************************************************************************
;MODE 1 PLOT -  USED BY DRAW. PLOT L,H

M1DPLOT:   PUSH HL
           PUSH DE
           PUSH BC
           CALL M1PIXAD      ;HL=Y/8+X/8+8000H=ADDRESS, B=BIT OFFSET (0-7)

M01DPCOM:  LD A,&FE
           INC B

DPM2FLP:   RRCA
           DJNZ DPM2FLP

;OVER 0,INVERSE 0=FORCE PIXEL HIGH
;OVER 1,INVERSE 0=REVERSE PIXEL
;OVER 0,INVERSE 1=FORCE PIXEL LOW
;OVER 1,INVERSE 1=NO EFFECT

           LD C,A
           LD DE,(OVERT)       ;E=0 IF OVER 0, D= 00/FF FOR INVERSE 0/1
           LD A,(HL)
           DEC E
           JR Z,DYOVER1        ;JR IF OVER 1

           AND C

DYOVER1:   INC D
           JR Z,DRPLEND        ;JR IF INVERSE 1

           XOR C
           CPL

DRPLEND:   LD (HL),A
           CALL POATTR01       ;CALL ATTR SETTER FOR MODES 0,1

DRPLEND2:  POP BC
           POP DE
           POP HL
           JP (IX)

;**************************************************************************
;MODE 2 OR 3 PLOTS FOR DRAW. ALL PLOT H,L
;ENTRY: HL=COORDS (Y,X), IX=RETURN ADDR, D'=INK COLOUR
;OVER 0:

M3DPOV0:   SCF
           RR H
           RR L              ;HL=Y/2+X/2+8000H=ADDR
           LD A,(HL)
           EXX
           JR C,M3DPOV0OD

           XOR D
           AND &0F
           XOR D
           EXX
           LD (HL),A
           ADD HL,HL         ;RESTORE HL
           JP (IX)

M3DPOV0OD: XOR D
           AND &F0
           XOR D
           EXX
           LD (HL),A
           ADD HL,HL
           INC L             ;RESTORE HL
           JP (IX)

;MODE 2 OR 3 ROUTINE FOR OVER 1 - XOR INK WITH WHAT IS THERE ALREADY
;HL=Y/X, D'=INK

M3DPOV1:   SCF
           RR H
           RR L              ;HL=Y/2+X/2+8000H=ADDR
           EXX
           LD A,D
           EXX
           JR C,M3DPOV1OD    ;JR IF ODD PIXEL

           AND &F0           ;NO EFFECT ON RHS BITS (ODD PIX)
           XOR (HL)
           LD (HL),A
           ADD HL,HL         ;RESTORE HL
           JP (IX)

M3DPOV1OD: AND &0F           ;NO EFFECT ON LHS BITS (EVEN PIX)
           XOR (HL)
           LD (HL),A
           ADD HL,HL
           INC L             ;RESTORE HL

M3DPNUL:   JP (IX)           ;INVERSE 1, OVER 1 ROUTINE DOES NOTHING

;MODE 2 OR 3 ROUTINE FOR OVER 2 - OR INK WITH WHAT'S THERE ALREADY
;HL=Y/X, D'=INK

M3DPOV2:   SCF
           RR H
           RR L              ;HL=Y/2+X/2+8000H=ADDR
           EXX
           LD A,D            ;FETCH INK
           EXX
           JR C,M3DPOV2OD    ;JR IF ODD PIXEL

           AND &F0           ;NO EFFECT ON RHS BITS (ODD PIX)
           OR (HL)           ;OR INK WITH SCREEN
           LD (HL),A
           ADD HL,HL         ;RESTORE HL
           JP (IX)

M3DPOV2OD: AND &0F
           OR (HL)
           LD (HL),A
           ADD HL,HL
           INC L             ;RESTORE HL
           JP (IX)

;MODE 2 OR 3 ROUTINE FOR OVER 3 - 'AND' INK WITH WHAT'S THERE ALREADY
;HL=Y/X, D'=INK

M3DPOV3:   SCF
           RR H
           RR L              ;HL=Y/2+X/2+8000H=ADDR
           EXX
           LD A,D            ;FETCH INK
           EXX
           JR C,M3DPOV3OD    ;JR IF ODD PIXEL

           OR &0F            ;SO NO EFFECT ON RHS BITS (ODD PIX)
           AND (HL)          ;AND INK WITH SCREEN
           LD (HL),A
           ADD HL,HL         ;RESTORE HL
           JP (IX)

M3DPOV3OD: OR &F0
           AND (HL)
           LD (HL),A
           ADD HL,HL
           INC L             ;RESTORE HL
           JP (IX)

;*******************************************************************************
;SET IY TO APPROPRIATE PLOT ROUTINE. A=INK TO USE IF MODE 2 OR 3. CY IF MODE 2
;THIN PLOT, IY NOT SET

SETIY:     LD A,(SETIYV+1)
           AND A
           LD A,(MODE)
           JR NZ,STIY6

           LD IY,M0DPLOT
           AND A
           RET Z             ;RET IF MODE 0

           LD IY,M1DPLOT
           DEC A
           RET Z             ;RET IF MODE 1

           DEC A
           JR NZ,STIY1       ;JR IF NOT MODE 2

           LD A,(THFATT)
           AND A
           SCF               ;'THIN PLOT'
           RET Z             ;RET IF THIN PLOT, ELSE USE M2/M3 ROUTINES

STIY1:     LD A,(GOVERT)
           LD IY,M3DPOV0
           AND A
           JR Z,STIY3        ;JR IF OVER 0

           DEC A
           JR Z,STIY5        ;JR IF OVER 1

           LD IY,M3DPOV2     ;ORING ROUTINE
           DEC A
           JR Z,STIY3        ;JR IF OVER 2

           LD IY,M3DPOV3     ;ANDING ROUTINE

STIY3:     LD A,(INVERT)
           AND A

STIY4:     LD A,(M23INKT)
           RET Z             ;RET WITH A=INK IF INVERSE 0

           LD A,(M23PAPT)    ;ELSE A=PAPER
           RET

;OVER 1

STIY5:     LD IY,M3DPOV1     ;ROUTINE TO XOR INK WITH SCREEN
           LD A,(INVERT)
           AND A
           JR Z,STIY4        ;XOR INK TO SCREEN IF OVER 1,INVERSE 0

           LD IY,M3DPNUL     ;DO NOTHING IF INVERSE 1,OVER 1
           RET

;VECTORED
STIY6:     PUSH HL
           LD HL,(SETIYV)
           CALL HLJUMP
           POP HL
           AND A             ;NC - NO THIN PIX
           RET
