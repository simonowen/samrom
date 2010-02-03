;GRAPH0.SAM
;*******************************************************************************

;ENTRY FROM JUMP TABLE WITH A=RADIUS, C,B=COORDS, HL=OFFSET IF THINPIX

JCIRCLE:   PUSH AF
           LD (TEMPW1),HL
           JR JCIRC3

CIRCLE:    CALL SYNTAX9      ;GET COLOURS, X,Y
           CALL INSISCOMA    ;REQUIRE COMMA
           CALL SYNTAX6      ;RADIUS (X,Y ALREADY ON FPCS)

           CALL GETBYTE      ;COMPRESS RADIUS INTO A AND C REGS.

CIRCLEFD:                    ;RETURN TO CIRCEXT AFTER PLOT
           PUSH BC           ;RAD IN C
           PUSH AF           ;RAD
           RST &30
           DW CIFILSR        ;Y COORD IN B, X IN C, POSSIBLE OFFSET IN TEMPW1

           POP AF

JCIRC3:    LD IX,CIRCEXT     ;RET SEE ABOVE
           LD D,A            ;RADIUS
           SRL A
           EX AF,AF'         ;RAD/2 PROTECTED TO A"
           LD E,3            ;BLITZ CODE FOR "CIRCLE"
           LD A,(THFATT)
           AND A
           CALL NZ,GRAREC    ;CALL IF FAT
           CALL SPSS         ;STORE PAGE, SELECT SCREEN
           CALL SETIY        ;SET IY TO PLOT ROUTINE
           JR NC,CIRCTK

           LD IY,THCIRCSR

CIRCTK:    EXX
           LD D,A            ;D"=M3 PLOT INK (OR JUNK IF NOT M3)
           EXX

           LD D,B
           LD E,C            ;DE=Y/X
           POP HL            ;L=RADIUS
           INC L
           DEC L
           JR NZ,DOCIRCLE    ;JR IF RADIUS NON-ZERO

           EX DE,HL          ;HL=COORDS OF CENTRE
           LD IX,TRCURP
           JP (IY)           ;PLOT POINT, THEN GOTO TRCURP


DOCIRCLE:  LD H,0

           CALL CIRCP5       ;FOLLOWING SECTION DEALS WITH FIRST 4. R & L..
           LD A,L
           LD L,H
           LD H,A
           CALL CIRC6        ;TOP
           PUSH HL
           XOR A
           SUB H
           LD H,A
           CALL CIRC6        ;BOTTOM
           POP HL
           INC L             ;START WITH DISP OF 1. OTHERWISE FIRST 4 POINTS ARE
                             ;DONE TWICE (0 ABOVE AND BELOW AXIS ARE THE SAME)
                             ;WHICH MAKES GAPS WITH OVER 1

;MAIN PLOT A POINT ON EACH SEGMENT LOOP
CIRC0:     CALL CIRCEX

           INC L
           EX AF,AF'
           SUB L
           JR NC,CIRC2

           ADD A,H
           DEC H

CIRC2:     EX AF,AF'
           LD A,L
           SUB H
           JP C,CIRC0

           DEC A
           CALL NZ,CIRCP4    ;IF ARCS HAVE NOT MET YET, DO 4 POINTS.

           JP TRCURP         ;CALL TEMPS, RESET PAGE

;DO 8 POINTS
CIRCEX:    CALL CIRC3        ;CALL DO 4 POINTS, THEN ENTER IT AGAIN...

;DO 4 POINTS
CIRC3:     LD A,L
           LD L,H
           LD H,A            ;SWOP DISPS

CIRCP4:    CALL CIRC4        ;CALL DO 2 POINTS, THEN ENTER IT AGAIN...

;DO 2 POINTS
CIRC4:     XOR A
           SUB H
           LD H,A            ;NEGATE H FOR BOTTOM HALF

CIRCP5:    LD A,E            ;X COORD
           ADD A,L
           CALL NC,CIRC5     ;DO RHS UNLESS WRAPPED ROUND (DO 1 POINT)

CIRC6:     LD A,E
           SUB L
           RET C             ;RET IF WRAPPED, ELSE DO LHS

;DO 1 POINT
CIRC5:     LD B,H
           LD C,L            ;SAVE DISPS
           LD L,A            ;X
           LD A,D
           ADD A,H
           CP 192
           JR NC,CIRCEXT

           LD H,A

CIPLOT:    JP (IY)           ;PLOT HL, RETURN TO CIRCEXT

CIRCEXT:   LD H,B
           LD L,C            ;DISPS RESTORED
           RET


;ENTRY: H=Y (RANGE OK), C=X

THCIRCSR:  PUSH IX
           PUSH DE
           PUSH BC
           LD B,H            ;B=Y
           EX DE,HL          ;E=X
           LD D,0
           LD HL,(TEMPW1)    ;OFFSET OF ZERO IF CIRCLE CENTRE X IS <80H
                             ;OFFSET OF 0100 IF CIRCLE CENTRE X IS >017F
                             ;OFFSET 0-FF OTHERWISE.
                             ;E OF 0-FF+OFFSET 0-0100 IS ALWAYS IN RANGE

           ADD HL,DE         ;REAL X
           CALL M2CTPLOT     ;PLOT HL,B WITHOUT ALTERING COORDS SYS VARS
           POP BC
           POP DE
           POP IX
           JP (IX)           ;TO CIRCEXT OR TEMPS

;*******************************************************************************
;DRAW.SAM. SAM DRAW COMMAND.

;FROM JUMP TABLE WITH X,Y IN C,B OR HL,B

JDRAWTO:      LD A,(THFATT)
              CP 1
              EX AF,AF'         ;CY IF THIN DRAW
              JR JDRTO3

DRAW:         SUB TOTOK         ;VAR IS ZERO IF "DRAW TO" USED
              LD (TEMPB3),A
              JR NZ,PASTTO

              RST &20           ;SKIP "TO"

PASTTO:       CALL SYNTAX9
              CP ","
              JR NZ,DRNOCU

              CALL SSYNTAX6     ;GET CURVATURE

              LD HL,TEMPB3
              LD A,(HL)
              AND A
              JR Z,DRNC2       ;JR IF NOT DRAW X,Y,Z

              RST &30
              DW DRCURVE-&8000

DRNC2:        LD (HL),H         ;TEMPB3=NZ, MEANS "CURVED"

              DB CALC
              DB STOD0
              DB EXIT           ;DELETE CURVATURE SO X,Y CAN BE ACCESSED

              JR DRAWTOFD

DRNOCU:       CALL CHKEND

              LD A,(TEMPB3)
              AND A
              JR NZ,DRAWFD      ;JR IF NOT DRAW TO.
                                ;ELSE DO DRAW TO. TEMPB3=0, MEANS "STRAIGHT"

;NOTE: COORDS SYSTEM WITH XOS, YOS ETC NORMAL IS 0 TO 255/512, AND -16 TO 175
;Y COORD STORED INVERTED AS 0 (TOP) TO 191 (BOTTOM)

DRAWTOFD:     CALL GTFCOORDS    ;FIDDLE USING XOS/YOS/XRG/YRG IF "DRAW TO"
                                ;UNSTACK COORDS.
                                ;B=Y, WITH 0 AT TOP. IF THIN PIX, HL=X, CY
                                ;ELSE C=X. RANGES CHECKED ALREADY.
              JR C,FDRNR

              LD A,(TEMPB3)
              AND A
              LD E,2            ;BLITZ CODE FOR "DRAW TO - FAT"
              CALL Z,GRAREC     ;CALL IF STRAIGHT (CURVES ARE RECURSIVE
                                ;STRAIGHT LINE DRAWS)
              AND A

FDRNR:        EX AF,AF'         ;CY IF THIN

;NOW CONVERT TO SIGNED DISPLACEMENT FROM CURRENT POSN TO NEW ONE

JDRTO3:       LD A,(YCOORD)
              SUB B             ;SUB Y COORD, Y DEST
              LD D,&FF          ;ASSUME -VE
              JR NC,ADJOK2      ;JR IF COORD NEEDS -VE INCREMENT TO REACH DEST

              NEG
              LD D,&01

ADJOK2:       LD B,A            ;B=Y DIFF, D=Y DIFF SGN
              EX AF,AF'         ;CY IF THIN PIX
              JR C,THINADJ

              LD A,(XCOORD)
              SUB C             ;SUB X COORD, X DEST
              LD E,&FF          ;ASSUME -VE
              JR NC,ADJOK3      ;JR IF COORD NEEDS -VE INCREMENT TO REACH DEST

              NEG
              LD E,&01

ADJOK3:       LD C,A            ;C=Y DIFF, E=Y DIFF SGN
              JR ADJOK5

THINADJ:      PUSH DE
              LD DE,(XCOORD)
              XOR A
              SBC HL,DE         ;SUB X DEST, X COORD
              INC A             ;ASSUME +VE
              JR NC,ADJOK4      ;JR IF COORD NEEDS +VE INCREMENT TO REACH DEST

              EX DE,HL
              LD L,A            ;L=1
              DEC A
              LD H,A            ;H=0. HL=1 COMPS FOR CARRY SET HERE
              SBC HL,DE         ;NEGATE HL.
              DEC A             ;A=FF

ADJOK4:       POP DE
              LD E,A

ADJOK5:       LD A,(TEMPB3)
              AND A
              JR Z,JDRAW

              RST &30
              DW DRTCRV-&8000
            ;  JP DRTCRV

;REL. DRAW

DRAWFD:       CALL DRCOORDFD    ;FIDDLE USING XRG/YRG ONLY

;REG USE: HL=LARGER AND SMALLER DIFFS, DE=SGN Y AND X, THEN D=M3 INK
;         B=POINT COUNT, C=TRACKING ERROR
;         HL"=Y AND X COORDS, DE"=HOR OR VERT STEP, BC"=DIAG STEP

DRAWLINE:     CALL TWONUMS      ;B=Y DIFF, C=X DIFF, D=SGN Y, E=SGN X (01/FF)
                                ; OR HL=X AND CY IF THINPIX
        ;      JR C,JDRAW        ;JR IF THIN

              DEC E             ;SGN X -> FE/00 (BLITZ CODE FOR FAT REL DRAW)
              CALL NC,GRAREC
              INC E             ;NORMAL AGAIN

JDRAW:        CALL SPSS         ;SAVE PAGE, SELECT SCREEN
              CALL SETIY        ;POINT IY TO APPROPRIATE PLOT ROUTINE.
                                ;A=INK IF M3, CY SET IF THIN PIX M2
              JP C,THINDRAW

              EX AF,AF'         ;A"=INK TO USE FOR MODE 3
              LD IX,PRLABEL     ;RETURN ADDR FROM PLOT ROUTINE
              LD HL,(YCOORD)    ;H=X COORD, L=Y
              LD A,H
              LD H,L
              LD L,A            ;H=Y, L=X
              PUSH HL

              EXX
              POP HL            ;HL"=COORDS
              EXX

              LD A,C
              AND A
              JR Z,CHKYCO2      ;IF X DISP=0 X WON"T RUN OFF. AVOID +/- 0

              LD A,L
              DEC E             ;DEC X SGN
              JR Z,DOXADD       ;JR AND ADD XCOORD AND DISTANCE IF +VE

              AND A
              JR Z,OSERRHP      ;ERROR IF MOVE LEFT AND X IS ZERO

              SUB C             ;SUB X,X DISP
              JR CHKYCO

DOXADD:       CP 255
              JR Z,OSERRHP      ;ERROR IF MOVE RIGHT AND X IS AT RHS

              ADD A,C           ;ADD X,X DISP

CHKYCO:       INC E
              JR C,RUNOFF       ;JR IF ADD OR SUB CARRIED

CHKYCO2:      LD A,B
              AND A
              LD A,H
              JR Z,FINCHK2      ;Y WON"T RUN OFF IF DISP=0

              DEC D             ;INC Y SGN
              JR Z,DOYADD       ;JR IF +VE (MOVE DOWN)

              AND A
              JR Z,OSERRHP      ;ERROR IF MOVE UP AND Y IS ZERO (AT TOP)

              SUB B             ;SUB Y,Y DISP
              JR FINCHK

DOYADD:       CP 191

OSERRHP:      JR Z,OSERROR      ;ERROR IF MOVE DOWN AND Y IS AT BOTTOM

              ADD A,B           ;ADD Y,Y DISP

FINCHK:       INC D             ;(D=SGN AGAIN. CY NOT ALTERED)
              JR C,RUNOFF

FINCHK2:      CP 192
              JR C,DRMSUBC      ;JR IF Y COORD IN RANGE

RUNOFF:       LD IX,PLOTCHK     ;CHECK AFTER PLOT IF EDGE HAS BEEN REACHED,
                                ;(IF LINE WILL RUN OFF-SCREEN) RATHER THAN
                                ;JUST RETURNING TO DRAW ROUTINE. IF OK, RETS
                                ;TO DRAW, ELSE EXITS WITH ERROR

DRMSUBC:      CALL DRMSUB
              EXX            ;GET HL=FINAL COORDS
              SCF            ;SIGNAL "OK"
              JP DRAWEND

DRMSUB:       PUSH DE           ;U/D AND L/R FLAGS

              EXX
              POP BC
              INC C
              JR NZ,NDHDIAG

              DEC B          ;IF C WAS FF, THEN ADDING C (-VE X DISP) TO L
                             ; (X COORD) WILL ALWAYS CARRY TO H+B, SO DEC B.
NDHDIAG:      DEC C          ;BC"=DIAG STEP
              EXX

              LD   A,C
              CP   B
              JR   NC,XGRTR  ;JR IF X DIFF >=Y DIFF

              LD   L,C       ;L=ABS X DIFF (LESS THAN Y DIFF)
              LD   E,0       ;WE WILL SOMETIMES NEED TO USE LR=STAY, SINCE X
                             ; DIFF IS LESS THAN Y DIFF.
                             ;B=GREATEST DIFFERENCE (Y)
              JR   DRPREL

XGRTR:        OR   B
              RET  Z         ;RET IF BOTH DIFFS EQUAL - NO LINE!

              LD   L,B       ;L=ABS Y DIFF ( <= X DIFF)
              LD   B,C       ;B=GREATEST DIFFERENCE (X)
              LD   D,0       ;SOMETIMES NEED UP/DOWN=STAY

DRPREL:       PUSH DE

              EXX
              POP DE
              INC E
              JR NZ,NDHFLAT

              DEC D          ;IF E WAS FF, THEN ADDING E (-VE X DISP) TO L
                             ; (X COORD) WILL ALWAYS CARRY TO H+D, SO DEC D.
NDHFLAT:      DEC E          ;DE"=HORIZ OR VERT STEP
              EXX

              EX AF,AF'
              LD D,A         ;D=M3 INK
              LD H,B         ;H=GREATEST COORD DIFF
              LD A,B         ;INITIALISE TRACKING ERROR BYTE. (IT ACCUMULATES
              SRL A          ;ERRORS FROM NOT MOVING ALONG LESS-DIF AXIS)

DRLOOP:       ADD A,L        ;ADD LESSER COORD DIFF TO TRACKING ERROR
              JR C,TWOMOVE   ;JR C AND SUB GREATER DIFF, MOVE IN BOTH AXES
                             ;(ERROR IS BOUND TO MOVE BACK INTO RANGE)

              CP H           ;CP GREATER COORD DIFF
              JR C,ONEMOVE   ;THE ERROR IS NOT OUT OF RANGE SO FAR. IF WE CANNOT
                             ;YET MAKE A MOVE ON THE LESSER-CHANGING AXIS (SMALL
                             ;ADDITIONS TO THE TRACKING ERROR DO NOT YET JUSTIFY
                             ;IT) MOVE JUST ON THE GREATER-CHANGING AXIS (JR)

TWOMOVE:      SUB H          ;SUB GREATER
              LD C,A         ;SAVE TRACKING ERROR BYTE IN C

              EXX
              ADD HL,BC      ;ADD DIAGONAL STEP
              JP (IY)        ;JP TO CORRECT PLOT ROUTINE, THEN TO PRLABEL

ONEMOVE:      LD   C,A       ;SAVE TRACKING ERROR

              EXX
              ADD HL,DE      ;ADD HORIZ OR VERT MOVE
              JP (IY)        ;JP TO CORRECT PLOT ROUTINE, THEN TO PRLABEL

PRLABEL:      EXX
              LD   A,C       ;GET TRACKING ERROR BACK
              DJNZ DRLOOP    ;LOOP FOR GREATER AXIS DIFF. OF POINTS

              RET            ;RETURN WITH HL"=COORDS, DE", BC"=DIR FLAGS

;POST-CHECK ROUTINE USED AFTER PLOT IF LINE WILL RUN OFF-SCREEN

PLOTCHK:      LD A,L
              INC A
              CP 2
              CCF
              JR NC,DRAWEND  ;JR IF X COORD=255 OR 0

              LD A,H
              DEC A          ;0->255,191->190
              CP 190
              JR C,PRLABEL   ;STILL OK - SO BACK TO DRAW LOOP

DRAWEND:      PUSH AF        ;SAVE NC IF HIT EDGE
              LD A,H
              LD H,L
              LD L,A
              LD (YCOORD),HL
              CALL TRCURP      ;TEMPS, RESET PAGE
              POP AF           ;NC IF HIT EDGE, C IF OK
              RET C

OSERROR:      RST &08
              DB 32            ;"Off screen"

;THIN PIXEL DRAW
;ENTRY: B=Y DIFF,HL=X DIFF, D=SGN Y, E=SGN X (01/FF)

THINDRAW:     EXX
              LD HL,0          ;"COORDS"=0
              EXX
              LD IY,THINDR2

DUBT:         LD A,H
              AND A
              JR Z,EASYTHIN

              RRA
              LD A,L           ;HALVE X DIFF
              RRA              ;A=HALF X DIFF
              LD C,A
              PUSH AF          ;SAVE HALF X DIFF AND CY IF DIFF IS ODD
              LD A,B
              AND A
              RRA
              LD B,A
              PUSH AF          ;SAVE HALF Y DIFF AND CY IF DIFF IS ODD
              PUSH DE
              CALL DRMSUB
              POP DE           ;SGN FLAGS
              POP AF           ;Y/2
              ADC A,0          ;INC IF WAS ODD
              LD B,A
              POP AF           ;X/2
              LD H,1
              ADC A,0          ;INC IF WAS ODD - COULD NOW BE ZERO IF ORIG=1FF
              LD L,A
              JR C,DUBT        ;JR WITH HL=0100 IF DOUBLE NEEDED AGAIN

EASYTHIN:     LD C,L
              CALL DRMSUB
              JP TRCURP

;THIN DRAW KEEPS SETTING DRAW"S COORDS IN HL TO 0000, SO HL WILL EQUAL BC OR DE

THINDR2:      PUSH BC
              PUSH DE
              EX DE,HL
              LD A,(YCOORD)
              ADD A,D
              LD B,A
              LD HL,(XCOORD)
              LD A,E
              AND A
              JR Z,TPNCHNG

              JP P,TPINC

              INC B            ;COMPENSATE FOR FIDDLED DIR FLAGS
              DEC HL           ;DEC X
              DEC HL           ;COMP FOR NEXT INSTR

TPINC:        INC HL

              LD A,H
              CP 2
              JR NC,OSERROR    ;ERROR IF X INCED TO 0200H OR DECED TO FFFFH

              LD A,B
              CP 192
              JR NC,OSERROR

TPNCHNG:      CALL TDPLOT
              LD HL,0
              POP DE
              POP BC
              JP PRLABEL
