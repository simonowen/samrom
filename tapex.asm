;TAPEX.SAM - TAPE/NET EXECUTIVE ROUTINES

;SAVE CDE BYTES FROM HL TO NET. TYPE ON STACK

                             ;HDR/DATA FLAG ON STACK
WNF:       PUSH BC
           LD B,100
           CALL DELBC
           POP BC
           CALL CKNET        ;WAIT FOR FREE NET
           POP AF
           CALL NMOUT        ;SEND TYPE

           LD IX,NOSR
           CALL NIXR

;NET SEND PARITY

           EXX
           LD A,H
           EXX

;NET/MIDI OUT

NMOUT:     PUSH AF

NMOUT1:    CALL BRKCR
           XOR A
           IN A,(CLUTPORT)
           AND 2
           JR NZ,NMOUT1

           CALL BRKCR
           POP AF
           OUT (MDIPORT),A
           RET

;ENTRY: HL POINTS TO DATA TO SAVE, CDE=LEN TO SAVE (PAGEFORM) A=TYPE (AND FIRST
;BYTE TO SAVE). 01=HEADER, FF=DATA
;EXIT: NC IF ALL SAVED, CY IF SPACE PRESSED

SABLK:     DI
           PUSH AF
           CALL TCHK
           JR NZ,WNF

           CALL CDENORM      ;CONVERT CDE TO 19-BIT
           POP AF
           PUSH AF
           EXX
           LD B,A
           EX AF,AF'         ;INITIALISE PARITY BYTE WITH TYPE
           LD A,(SLDEV+1)    ;SPEED. MAX SPEED IF 19 (GIVES ABOUT 5*ZX)
                             ;HIGHER VALUES GIVE LOWER SPEEDS.
           LD L,A
           INC A
           ADD A,A
           JR NC,SVLC

           LD A,&FE          ;HI VALUE IF CY

SVLC:      INC A
           LD H,A            ;H PRODUCES DELAY ABOUT TWICE AS LONG AS L
           PUSH HL

           LD C,A            ;LEADER DELAY

;NOW FIDDLE NO. OF LEADER CYCLES TO USE TO KEEP LEN ROUGHLY CONST. WHATEVER THE
;SVSPEED. 128 OR MORE=LC*2, 86-127=LC*3, 64-85=LC*4, 52-63=LC*5, 43-51=LC*6
;37-42=LC*8,.... 19=LC*14

           XOR A
           LD H,A
           LD L,A
           LD DE,3000

SVCLC:     ADD HL,DE
           ADD A,C
           JR NC,SVCLC

           INC B
           DJNZ SVTYP        ;JR IF DATA

           ADD HL,HL         ;LONGER LEADER IF HEADER

SVTYP:     LD DE,&0202       ;D'=BORDER LS 3 BITS - RED

SVHDR:     LD A,D
           XOR &0F
           LD D,A
           OUT (KEYPORT),A
           LD B,C

SVDL0:     DEC B
           JR NZ,SVDL0       ;DELAY REG*16

           DEC HL
           LD A,H
           OR L
           JR NZ,SVHDR       ;LOOP TILL LEADER SAVED

           SRL C
           SRL C             ;QUARTER LENGTH PULSES NOW
           INC C
           INC C
           INC C             ;IN CASE VERY SMALL - HELPS AVOID SYNC BEING MISSED
           LD L,2            ;2 HALF-CYCLES ONLY
           DEC E
           JR NZ,SVHDR       ;JR IF SYNC PULSE TIME

           POP HL            ;DELAY CONSTANTS FOR HI/LOW BITS
           POP AF            ;TYPE BYTE
           LD E,A            ;FIRST BYTE
           LD D,1            ;D'=BORDER LS 3 BITS - BLUE
           SCF
           RL E

SVBL:      LD C,H            ;ASSUME HI BIT
           JR C,SV2

           LD C,L            ;GET DELAY FOR A LOW BIT

SV2:       LD A,D            ;BORDER COLOUR.
           XOR &0F           ;FLIP MIC AND BORDER BITS
           OUT (KEYPORT),A   ;38 OR 37 FROM SVBL TO END OF OUT INSTRUCTION
                             ;55 OR 54 FROM PREV OUT, PLUS DELAY (IF WITHIN BYTE
           LD B,C

SVDL:      DJNZ SVDL         ;THESE 2 INSTRUCTIONS GIVE DELAY OF 13*C, -1

           DEC C
           DEC C             ;SUB 26 Ts FROM DELAY
           XOR &0F           ;NC
           RL E
           OUT (KEYPORT),A   ;34 TO END OF OUT FROM PREV OUT, PLUS DELAY

           JP Z,SVBY         ;JP IF TIME FOR NEXT BYTE
                             ;ELSE CY/NC SHOWS NEXT BIT
           LD B,C

SVDL2:     DJNZ SVDL2

           JR SVBL

;FROM SVBL: 38/37,OUT,13*R-1,34,OUT,13*R-27,22; 38/37,OUT ETC. R=REGISTER
;E.G. REG=20: 38/37,OUT,293,OUT,293/291,OUT,293   (0.098 MSEC AT 6MHZ)
;E.G. REG=43: 38/37,OUT,592,OUT,592/591,OUT,      (0.197 MSEC AT 6MHZ)

SVBY:      EXX
           LD A,D
           OR E
           LD A,C            ;FF IF PARITY SENT, FE IF LAST JUNK BYTE SENT
                             ;(SO PARITY LAST BIT TERMINATED CORRECTLY)
                             ;0 IF LAST 64K BLOCK SENT
           JR NZ,SV4         ;JR IF MOD 64K NOT SAVED YET

           INC A
           INC A
           RET Z             ;RET IF ALL BYTES SAVED - NC, Z

           DEC C             ;DEC NUMBER OF 64K BLOCKS
           CP 3              ;
           JR NC,SV4         ;JR IF ORIG WASN'T FF OR 0 - SOME BLOCKS LEFT

           EX AF,AF'         ;PARITY
           LD B,A
           JR SV45

SV4:       DEC DE
           LD B,(HL)         ;BYTE TO SAVE
           EX AF,AF'
           XOR B
           EX AF,AF'         ;PARITY

SV45:      LD A,&F7
           IN A,(STATPORT)
           AND &20
           SCF
           RET Z             ;RET IF ESC KEY HIT - CY

           INC HL
           LD A,H
           CP &C0
           IN A,(251)
           INC A             ;READY IN CASE PASSED BFFF
           JR C,SV5          ;JR IF OK (ADDR HASN'T PASSED BFFF YET)

           LD H,&80
           OUT (251),A       ;NEXT PAGE

SV5:       LD A,B
           EXX

           LD E,A            ;NEXT BYTE TO SAVE
           LD A,C            ;ORIG DELAY, MINUS 2
           INC B             ;B=1
           SUB 17            ;COMP FOR EXTRA TIME TAKEN GETTING NEXT BYTE
           JR C,SV6          ;USE B=1 IF DELAY COUNTER CARRIED

           INC A             ;ENSURE NOT ZERO
           LD B,A            ;DELAY 182 Ts LESS
           SCF               ;SHOWS BYTE ALL SENT WHEN ROTATED OUT.

SV6:       RL E
           JR SVDL2          ;ABOUT 187 EXTRA TO HANDLE THIS BYTE

SABYTES:   CALL SABLK
           CCF               ;NOW CY=OK
           JR SVLDCOM

LDBYTES:   CALL LDBLK

SVLDCOM:   EI
           EX AF,AF'         ;NC IF ERROR
           LD A,(BORDCOL)
           OUT (KEYPORT),A
           CALL BRKCR        ;CHECK BREAK, STOP IF REQUIRED
           EX AF,AF'
           RET               ;C IF OK


;LOAD CDE BYTES TO HL FROM NET. TYPE ON STACK

WNH:       POP AF
           LD B,A
           EX AF,AF'         ;CY IF LOAD
           LD A,B

WNHL:      PUSH AF
           CALL CKNET        ;WAIT FOR FREE NET

WTNI:      CALL NMIN         ;WAIT FOR A BYTE
           JR NC,WTNI

           LD B,A
           POP AF
           CP B
           JR NZ,WNHL        ;WAIT FOR HDR OR DATA MARKER

           LD IX,NISR
           CALL NIXR

;NET PARITY CHECK

           CALL NMIN

NERR:      JP NC,TERROR

           EXX
           XOR H
           EXX

           CP 1
           RET

;NET OUT SR. OUTPUTS THE BYTE AT (HL), INCLUDES IN PARITY

NOSR:      LD A,(HL)
           CALL NMOUT
           JR NTVL

;NET INPUT SR. LOAD OR VERIFY NET BYTE WITH (HL), INCLUDE IN PARITY

NISR:      CALL NMIN
           JR NC,NERR

           EX AF,AF'
           JR NC,NTV         ;JR IF VERIFYING

           EX AF,AF'
           LD (HL),A         ;LOAD BYTE
           JR NTVL

NTV:       EX AF,AF'
           CP (HL)           ;VERIFY BYTE
           JR NZ,NERR

NTVL:      EXX
           XOR H
           LD H,A            ;PARITY BYTE
           EXX

           RET

;ENTRY: HL=DEST TO LOAD DATA TO, CDE=LEN TO SAVE (PGFORM), A=TYPE (CHECKED VS
;FIRST BYTE ON TAPE). 01=HEADER, FF=DATA. (IF TAPE TYPE=ZX HEADER, 17 BYTES
;WILL BE LOADED

;EXIT: CY IF LOADED OK, NC IF ERROR

LDBLK:     DI
           PUSH AF
           CALL TCHK
           JR NZ,WNH

           POP AF
           CALL CDENORM      ;CONVERT CDE TO 19-BIT
           LD (TEMPW1),BC    ;SAVE C (64K BLOCKS)
           LD (TEMPW2),HL    ;DEST ADDR
           INC C             ;NZ
           EX AF,AF'         ;NZ=FIRST BYTE, NC=VERIFY, CY=LOAD
           LD A,8
           OUT (KEYPORT),A   ;ENSURE MIC BIT STARTS OFF OR WE CANNOT READ EAR!
                             ;INIT EDGE TYPE. BORDER WILL GO WHITE

LDERR:     CALL BRKTST
           RET Z             ;RET IF ESC PRESSED - NC

LDSTRT:    LD B,8
           CALL EDGE2        ;LOOK FOR ALTERED EAR SIGNAL
           JR NC,LDERR       ;LOOP IF TIMEOUT OR ESC

           CALL EDGE2
           JR NC,LDERR

           EXX
           LD L,A
           LD B,0
           EXX

           LD L,0            ;LOOK AT 256 SAMPLES

;A SIGNAL HAS BEEN FOUND - SEE IF LEADER

LDLDR:     CALL EDGE2       ;LOOK FOR TWO EDGES
           JR NC,LDERR      ;JR IF BREAK OR TIMEOUT

           EXX
           LD H,B            ;HL=AV. B=0
           LD C,L            ;BC=AV
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL         ;*16
           SBC HL,BC         ;*15
           LD C,A            ;BC=SAMPLE
           ADD HL,BC         ;*15+NEW SAMPLE
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL
           ADD HL,HL         ;H=NEW AV (HL/16)
           LD L,H            ;KEEP RUNNING AVERAGE OF PULSE WIDTH IN L
           LD A,L
           SUB C             ;LAST PULSE WIDTH
           JR NC,BLWAV

           NEG               ;GET ABS DIFF FROM AV.

BLWAV:     ADD A,A
           ADD A,A
           CP L              ;4*ABS DIFF FROM AVERAGE SHOULD BE LESS THAN AV.
           EXX               ;(+/-25%)

           JR NC,LDSTRT

           DEC L
           JR NZ,LDLDR       ;LOOK FOR 256 LEADER PULSES
                             ;L ENDS AS ZERO - PARITY BYTE INIT STATE.
           LD A,B
           AND &F8
           OR &02
           LD B,A            ;RED BORDER NOW

           EXX
           LD A,L
           SRL A
           LD B,A
           SRL A
           ADC A,L           ;A=1.25*AV LDR PULSE LEN
           LD E,A            ;E'=MAX LEN FOR 'LONG' (HI) BIT
           LD D,E
           DEC D
           DEC D             ;FIDDLE A BIT TO CENTRALIZE..
           SRL D             ;D'=HALF THAT - SHORT/LONG DECISION VALUE
           LD A,B
           ADD A,L
           LD BC,(TEMPW1-1)
           INC B             ;B'=64K BLOCKS, PLUS 1
           EXX

           LD H,A
           SRL H             ;0.75*AV

;NOTE: WAITING FOR SYNC THIS WAY ENSURES BOTH HALVES OF ALL PULSES KEPT TOGETHER
;WHATEVER POLARITY OF SIGNAL, AS WELL AS SHOWING START OF DATA

WTSYNC:    LD C,0
           CALL EDGSENS
           CP H              ;CP 1.50*AV HALF-CYCLE LEN
           JR NC,LDSTRT      ;ERROR IF TOO LONG (>0.75*AV)

           ADD A,A           ;2*HALF-CYCLE
           CP H              ;CP 1.50*AV HALF-CYCLE LEN
           JR NC,WTSYNC      ;LOOP UNTIL FIRST HALF OF SYNC PULSE*2<1.50*AV
                             ;HALF CYCLE. IE WAIT UNTIL 0.75 OR LESS OF AV.
           CALL EDGSENS      ;C IS CARRIED IN AS START VALUE
           CP H              ;A=TIME FOR WHOLE POSSIBLE SYNC PULSE
           JR NC,WTSYNC      ;INSIST WHOLE SYNC PULSE<0.5*AV WHOLE CYCLE

           LD A,B
           XOR 3
           LD B,A            ;NEW BORDER COLOURS ARE BLUE AND YELLOW
           EXX
           LD HL,(TEMPW2)    ;DEST PTR
           EXX
           JR LDSTART        ;JUMP TO LOAD TYPE BYTE

LDTYPE:    RL C              ;SAVE CY
           INC H
           DEC H
           JR NZ,LDTCP       ;JR IF NOT TYPE 0 (ZX HEADER)

           INC H             ;NOW TYPE=1 (SAM HEADER)
           LD E,17           ;LEN=ZX HEADER LEN

LDTCP:     XOR H             ;SEE IF TYPE FLAG A' MATCHES TYPE BYTE LOADED
           RET NZ            ;RET IF NOT - NC, NZ

           LD A,C
           RRA               ;GET CY BACK. CAN'T USE RR C - CHANGES Z
           EX AF,AF'
           JR LDSTART

LDLOOP:    EXX

LDDE:      EX AF,AF'
           LD A,H
           EXX
           JR C,LDNVER

LDVERIF:   XOR (HL)
           RET NZ            ;RET IF MATCH FAILED - NC,NZ

           DB &3E            ;"JR+1"

LDNVER:    LD (HL),A         ;STORE LOADED BYTE

LDNEXT:    EX AF,AF'         ;PROTECT FLAGS
           INC HL            ;INC DEST PTR
           LD A,H
           CP &C0
           IN A,(251)
           INC A
           JR C,LDDEC        ;JR IF NOT PAST BFFFH

           LD H,&80
           OUT (251),A       ;NEXT PAGE

LDDEC:     EXX
           DEC DE            ;DEC BYTE COUNT

LDSTART:   LD H,1            ;MARKER BIT SET IN DEST SHIFT REG
           LD C,3            ;COMP FOR TIME LOST BY HANDLING BYTE

LDBITS:    CALL EDGEC
           RET NC            ;RET IF TIMEOUT OR SPACE PRESSED

           EXX
           CP E
           RET NC            ;RET IF TOO LONG

           CP D
           CCF               ;NC IF SHORTER THAN 'DECISION' REGISTER
           EXX

           RL H              ;ACCUMULATE BITS IN H
           LD C,0
           JR NC,LDBITS      ;LOOP FOR ALL 8 BITS (TILL THE MARKER ROTATED OUT)

           LD A,L            ;PARITY BYTE
           XOR H             ;MODIFIED
           LD L,A            ;WITH NEW DATA

           EX AF,AF'
           JR NZ,LDTYPE      ;NO DEC B IF TYPE BYTE

           EX AF,AF'
           LD A,E
           OR D
           JR NZ,LDDE        ;LOOP TO LOAD DE BYTES

           EXX
           DJNZ LDLOOP
           EXX

           LD A,L            ;PARITY
           CP 1              ;CY SET IF OK (ZERO)
           RET


;CHECK IF NET BUSY

CKNET:     PUSH HL

CKNT1:     LD H,4

CKNT2:     CALL BRKCR
           IN A,(VIDPORT)
           RLA
           JR NC,CKNT1       ;LOOP TILL FREE

           DEC HL
           LD A,H
           OR L
           JR NZ,CKNT2       ;ENSURE FREE FOR A WHILE

           POP HL
           RET


;NET/MIDI IN. GETS BYTE IN A (CY) OR NC AND A=0 IF NO BYTE

NMIN:      PUSH HL
           LD HL,300

NMIN1:     DEC HL
           LD A,H
           OR L
           JR Z,NMIN3

           CALL BRKCR
           IN A,(STATPORT)
           BIT 2,A
           JR NZ,NMIN1

NMIN2:     IN A,(STATPORT)
           BIT 2,A
           JR Z,NMIN2

           IN A,(MDIPORT)
           SCF

NMIN3:     POP HL
           RET


NIXR:      EXX
           LD H,A            ;PARITY BYTE
           EXX

           RES 7,D
           LD A,C            ;16K BLOCKS
           AND A
           JR Z,NIXJ

NIXL:      PUSH AF
           PUSH DE
           LD DE,&4000       ;DO 16K
           CALL NIXJ
           POP DE
           POP AF
           DEC A
           JR NZ,NIXL

;NET SAVE/LOAD/VERIFY 0-16K BYTES (DE BYTES FROM HL)

NIXJ:      LD A,D
           OR E
           RET Z

           LD A,H
           CP &C0
           CALL NC,INCURPAGE ;** BUG FIX AND RE-WRITE
           CALL IXJUMP       ;INPUT OR OUTPUT
           INC HL
           DEC DE
           JR NIXJ
