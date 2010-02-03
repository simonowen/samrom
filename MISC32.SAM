;MISC32.SAM

BEEP:      CALL SYNTAX8

           DB CALC           ;LENGTH,NOTE
           DB ONELIT
           DB 27             ;L,N,27
           DB ADDN           ;L,N+27
           DB ONELIT
           DB 12
           DB DIVN           ;L,(N+27)/12
           DB POWR2          ;L,2^(N+27)/12
           DB ONELIT
           DB 55
           DB MULT           ;L,FREQUENCY
           DB DUP            ;L,F,F
           DB SWOP13         ;F,F,L
           DB RESTACK        ;F,F,L (FP FORM)
           DB EXIT

           LD A,(HL)         ;L EXPONENT
           CP &85            ;**
           JR C,BEEP2        ;ALLOW DURATION UP TO 16 SECS

           RST &08
           DB 50             ;'note too long'

BEEP2:     LD BC,-5
           ADD HL,BC
           LD A,(HL)         ;F EXPONENT
           CP &84
           JR C,INVNOTE

           CP &8F
           JR C,BEEP3        ;ALLOW FREQUENCIES OF 8Hz TO 16KHz.

INVNOTE:   RST &08
           DB 49             ;'Invalid note'

BEEP3:     DB CALC           ;F,F,L
           DB MULT           ;F,F*L=CYCLES
           DB SWOP           ;CY,F
           DB FIVELIT
           DB &93,&37
           DB &1B,0,0        ;CY,F,375000=8T UNITS IN A 1HZ HALF-CYCLE
           DB SWOP           ;CY,375000,F
           DB DIVN           ;F,375000/F=8T UNITS PER HALF-CYCLE
           DB ONELIT
           DB 15
           DB SUBN           ;ALLOW FOR TIME TAKEN BY LOOP EVEN IF NIL 8T UNITS
                             ;(15*8=120 TS)
           DB EXIT

           CALL GETINT
           PUSH BC
           CALL GETINT
           EX DE,HL          ;DE=CYCLES (ALSO, BC=DE=BA)
           POP HL            ;HL=8 T UNITS
           OR B
           RET Z             ;RET IF NONE TO DO

           DEC DE            ;DE=0 GIVES 1 CYCLE

;ENTRY: DE=CYCLES TO DO, MINUS 1
;       HL=8T UNITS PER HALF-CYCLE (ON ENTRY, H=2048 Ts, L=8 Ts)
;TAKES 118 TS EVEN IF HL=0, CONTENTION OF 'OUT' MAKES THAT 120 TS

BEEPP2:    DI
           PUSH BC
           LD BC,BEEPLP
           SRL L             ;L=16T UNITS NOW
           JR C,BEEPER2      ;USE A LOOP 8 TS LONGER IF L WAS ODD

           INC BC
           INC BC

BEEPER2:   PUSH BC
           POP IX            ;IX IS LOOPING ADDR
           LD A,(BORDCOL)    ;BITS=STI01GRB
                             ;(SOFF/THRO MIDI/INTENSITY/0/MIC/GRB)
           OR &18            ;SPKR BIT HI (ON), MIC BIT HI (OFF)
           JR BPLENT         ;JUMP INTO LOOP - PULSE SPEAKER OFF

BEEPLP:    LD B,B
           LD B,B

;ENTER HERE (IX) IF SHORTER LOOP

           INC C
           INC B             ;INC IN CASE B OR C=0

BPTMLP:    DEC B
           JR NZ,BPTMLP      ;EACH INNER LOOP=16 TS

           LD B,127          ;OUTER LOOP TAKES 32 TS. 127 GIVES 126*16+32=2048
           DEC C
           JP NZ,BPTMLP

BPLENT:    XOR &10           ;REVERSE BIT 4
           OUT (&FE),A       ;PULSE SPEAKER
           LD B,A            ;SAVE A FOR LATER
           LD C,H
           BIT 4,A
           JR NZ,BEEPR4      ;JR IF JUST DONE FIRST HALF-CYCLE

           LD A,D
           OR E
           JR Z,BEEPR5       ;END IF DONE ALL CYCLES

           DEC DE

BEEPR3:    LD A,B
           LD B,L
           JP (IX)

BEEPR4:    LD A,B            ;DELAY 4TS SO BOTH PATHS TAKE THE SAME TIME
           JR BEEPR3

BEEPR5:    POP BC
           EI
           RET


ZAP:       CALL CHKEND

           LD B,6

ZPL:       PUSH BC
           LD BC,2
           LD E,1
           LD A,30
           CALL PAF
           DI
           POP BC
           DJNZ ZPL

           EI
           RET


BOOM:      CALL CHKEND

           LD E,1
           LD BC,12
           JR ZPC

ZOOM:      CALL CHKEND

           LD E,6
           LD BC,65533

ZPC:       LD A,78

PAF:       LD L,255
           PUSH AF

           LD H,0
           LD D,H

SAD:       EXX
           POP BC

SELP:      EXX
           PUSH DE
           PUSH HL
           CALL BEEPP2
           DI
           POP HL
           POP DE
           ADD HL,BC
           EXX
           DJNZ SELP

           EXX
           EI
           RET

POW:       CALL CHKEND
           EX DE,HL
           LD B,0

PWLP:      LD A,(DE)
           LD L,A
           LD H,0
           INC DE
           PUSH DE
           LD DE,1
           CALL BEEPP2
           DI
           POP DE
           DJNZ PWLP

           EI
           RET

BGRAPHICS: CALL SYNTAX6

           LD DE,&0200+30    ;LIMIT TO <2 OR IOOR
           CALL LIMBYTE

           DEC A
           LD (BGFLG),A      ;0 IF USE BLOCKS, FF IF USE UDGS
           RET               ;(BGRAPHICS 1=USE BLOCKS, 0=USE UDGS)

;KEY POSN,VALUE

KEY:       CALL SYNTAX8

           CALL GETBYTE
           PUSH AF
           CALL GETINT
           POP AF
           LD HL,-281
           ADD HL,BC
           JP C,IOORERR      ;LIMIT POSN TO 0-280 (0 NOT USED)

           LD HL,(KBTAB)
           ADD HL,BC
           LD (HL),A
           RET


;SELECT SAVE/LOAD DEVICE E.G. DEVICE M: DEVICE T: DEVICE N: DEVICE M2
;DEVICE T45 SETS SPEED=45

SLDEVICE:  CALL GETALPH
           AND &DF           ;FORCE UPPER CASE
           PUSH AF
           RST &20
           CALL FETCHNUM     ;NUMBER OR USE 0
           POP DE            ;LETTER
           CALL CHKEND

           PUSH DE
           CALL GETBYTE      ;IN C
           POP AF            ;LETTER
           CP "N"
           JR Z,DEVI3        ;IF NET, USE NUMBER OR DEFAULT TO 0

           CP "T"
           JR NZ,DEVI2

           INC C
           DEC C
           JR NZ,DEVI3

           LD C,TSPEED       ;DEFAULT TAPE SPEED

DEVI2:     INC C
           DEC C
           JR NZ,DEVI3

           INC C             ;DEFAULT TO 1 IF DISC

DEVI3:     LD HL,PSLD
           LD (HL),A         ;LETTER
           INC HL
           LD (HL),C         ;DRIVE/STATION/SPEED
           RET


PAUSE:     CALL SYNTAX3

           CALL GETINT
           LD E,7            ;BLITZ CODE FOR 'PAUSE'
           CALL GRAREC       ;USE PAUSE MOD 256 AS PARAM (C)

PAU1:      CALL KBFLUSH      ;(HL=FLAGS)
           HALT
           CALL BRKCR        ;CHECK BREAK, STOP IF SO
           LD A,(LASTSTAT)   ;STATPORT VALUE ON LAST INTERRUPT
           AND 8
           JR NZ,PAU1        ;JR IF IT WASN'T A FRAME INTERRUPT

           LD A,B
           OR C
           JR Z,PAU2         ;DON'T DEC BC IF PARAM WAS ZERO

           DEC BC
           LD A,B
           OR C
           RET Z             ;RET IF COUNTED TO ZERO

PAU2:      BIT 5,(HL)
           JR Z,PAU1         ;LOOP IF NO KEY PRESSED

           RES 5,(HL)        ;'NO KEY'
           RET


PRCOITEM2:    LD B,D                 ;B ALSO IS PARAM
              SUB 16
              JP Z,COINK

              DEC A
              JP Z,COPAPER

              DEC A
              JR Z,COFLASH

              DEC A
              JR Z,COBRIGHT

              DEC A
              JR NZ,COOVER

COINVERSE:    LD C,4                 ;MASK FOR BIT 2 - INVERSE
              LD A,D
              CP 2
              JR NC,INVCOLERR        ;ALLOW INVERSE 0 OR 1 ONLY

              DEC A
              CPL                    ;0/1 ->00/FF
              LD (INVERT),A
              LD A,D
              RLCA
              RLCA
              LD B,A                 ;PARAM BIT TO BIT 2
              JR COINOVC

COOVER:       LD C,1                 ;MASK FOR BIT 0 - OVER
              LD A,D                 ;CHECK ORIG PARAM FOR 0/1
              CP 4
              JR NC,INVCOLERR        ;OVER 0/1/2/3 ALLOWED

              LD E,4                 ;BLITZ CODE FOR 'OVER'
              PUSH BC
              LD C,A
              PUSH AF
              CALL GRAREC
              POP AF
              POP BC
              LD (GOVERT),A          ;GRAPHICS OVER IS 0-3
              CP 2
              RET NC                 ;NO FURTHER ACTION IF OVER 2 OR 3

              LD (OVERT),A
              LD A,B

COINOVC:      LD HL,PFLAGT
              JP COCHNG              ;ADD MODIFIED PARAM TO PFLAG

COBRIGHT:     LD C,&40               ;MASK FOR BIT 6 - BRIGHT
              LD A,(MODE)
              CP 2
              LD A,D                 ;PARAM
              JR Z,COBRI2

              CP 2
              JR NC,COBRI2           ;JR IF BRIGHT 8

              AND A
              LD A,(M23PAPT)
              LD E,A
              LD A,(M23INKT)
              JR Z,COBRI1            ;JR IF BRIGHT 0

              OR &88                 ;ADD 8 TO INK
              PUSH AF
              LD A,E
              OR &88                 ;AND PAPER, IF <8
              JR COBRI15

COBRI1:       AND &77
              PUSH AF
              LD A,E
              AND &77

COBRI15:      LD (M23PAPT),A
              POP AF
              LD (M23INKT),A
              LD A,D

COBRI2:       RRCA                   ;PARAM 0/1 ENDS IN BIT 6...
              JR COBRFLC

COFLASH:      LD C,&80               ;MASK FOR BIT 7 - FLASH
              LD A,D                 ;PARAM

COBRFLC:      RRCA                   ;PARAM 0/1 TO BIT 7 (OR 6 IF FLASH)
              LD B,A                 ;MODIFIED PARAM
              LD A,D
              CP 16
              JR NZ,COBFC2           ;ALLOW ORIG PARAM OF 0/1, OR 8/16=TRANSP

              RRCA                   ;A=8
              RRC B                  ;ADJ B TOO

COBFC2:       CP 8
              JR Z,COFBOK

              CP 2
              JR C,COFBOK

INVCOLERR:    RST &08
              DB 23

COFBOK:       LD HL,ATTRT
              LD A,B
              CALL COCHNG            ;ADD FLASH 0/1 TO ATTRT
              LD A,B
              RRCA
              RRCA
              RRCA                   ;FLASH/BRIGHT 8 MOVED TO CORRECT BIT FOR
              JR COCHNG              ;ADDING TO MASKT

COINK:        LD C,7                 ;MASK FOR INK
              JR CINKPAPC

COPAPER:      LD C,&38               ;MASK FOR PAPER
              LD A,D
              RLCA
              RLCA
              RLCA
              LD B,A                 ;GET PARAM BITS IN PLACE FOR PAPER

CINKPAPC:     LD HL,ATTRT
              LD A,D                 ;PARAM
              CP 16
              JR NC,CINKPAP0

              RLCA
              RLCA
              RLCA
              RLCA
              OR D                   ;A=2 COPIES OF PARAM BITS 3-0
              LD E,A                 ;MODE 3 INK/PAP
              RLCA
              JR NC,CONFBRI          ;JR IF NO NEED TO FORCE BRIGHT 1 (COL<8)

              SET 6,(HL)             ;SET BRIGHT BIT IN ATTRT FOR COLOURS 8-15

CONFBRI:      LD A,(MODE)
              CP 2
              LD A,E
              JR NZ,COIPM3

              RLCA
              RLCA                   ;BITS 5,4 AND 1,0 TO 7,6 AND 4,3
              XOR E
              AND &CC                ;KEEP BITS 7,6,4,3 OF A
              XOR E                  ;A=4 COPIES OF PARAM BITS 1,0 IF MODE 2
                                     ;WHICH ONLY HAS 4 INKS
              RLCA                   ;CORRECT BIT SWOP

COIPM3:       BIT 0,C                ;TEST MASK BIT
              JR Z,COPAPM3           ;JR IF PAPER

              LD (M23INKT),A
              LD E,5                 ;BLITZ CODE FOR 'PEN'
              PUSH BC
              LD C,A
              CALL GRAREC
              POP BC
              JR CINKPAP2

COPAPM3:      LD (M23PAPT),A
              JR CINKPAP2

CINKPAP0:     CP 18
              JR NC,INVCOLERR        ;ONLY ALLOW INK/PAPER 16 OR 17 HERE

              LD   A,(HL)            ;GET (ATTRT)
              JR   Z,CINKPAP1        ;JR IF INK/PAPER 16 - NO CHNG TO ATTRT,
                                     ;JUST MASKT
              OR   C                 ;MAKE INK OR PAPER WHITE
              CPL                    ;THEN BLACK. FLIP OTHER BITS
              AND  &24               ;00 100 100
              JR Z,CINKPAP2          ;JR IF INK/PAPER WAS LIGHT

              LD   A,C               ;USE WHITE INK OR PAPER TO CONTRAST

CINKPAP1:     LD B,A

CINKPAP2:     LD A,B
              CALL COCHNG            ;CHANGE ATTRT
              LD A,15                ;SO GET CARRY FOR INK/PAPER 16/17
              CALL COCHNGP           ;CHANGE MASKT
              RLCA
              RLCA
              AND &50
              LD C,A
              LD A,16                ;SO GET CARRY FOR INK/PAPER 17
                                     ;THEN ALTER PFLAGT
COCHNGP:      CP D
              SBC A,A

COCHNG:       XOR (HL)
              AND C
              XOR (HL)
              LD (HL),A
              LD A,C
              INC HL
              RET


BORDER:    CALL SYNTAX6

           LD DE,&1000+23    ;LIMIT TO <16 OR INVALID COLOUR
           CALL LIMBYTE      ;A=BORDER

SETBORD:   LD C,A
           LD L,C
           RLCA
           RLCA
           LD B,A
           XOR C
           AND &20           ;USE BIT 5 FROM A
           XOR C
           OR &08            ;MIC OFF
           EX DE,HL
           LD HL,BORDCOL
           XOR (HL)
           AND &3F           ;USE BITS 7 AND 6 (SOFF/THRO) FROM SYS VAR
           XOR (HL)
           LD (HL),A         ;USED BY SAVE/LOAD TO RESTORE BORDER, AND BEEP
                             ;BIT 7=SOFF STATUS
           EX DE,HL
           OUT (KEYPORT),A
           LD A,B
           RLCA              ;0XXX X000
           LD H,0            ;BLACK INK
           BIT 5,A           ;IS BORDER COLOUR LIGHT?
           JR NZ,BORD1       ;JR IF SO - INK IS BLACK

           LD H,&FF          ;COLOUR 15/3 (WHITE) FOR LS INK (MODE 3/2)
           OR &07            ;AND BORDER COLOUR

BORD1:     LD (BORDCR),A
           LD A,L
           RLCA
           RLCA
           RLCA
           RLCA
           OR L
           LD L,A            ;DOUBLE PAPER NIBBLE
           LD A,(MODE)
           CP 2
           JR NZ,BORD3

           CALL M3TO2
           INC A
           JR Z,BORD2             ;USE INK 0 IF PAPER FF

           LD A,&FF               ;(USUALLY) WHITE INK OTHERWISE

BORD2:     LD H,A

BORD3:     LD (M23LSC),HL
           RET

;CONVERT LS PAPER TO NON-STRIPED MODE 2 COLOUR - CALLED BY MODE

CVLSP:     CALL M3TO2        ;CONVERT L
           JR BORD3

M3TO2:     LD A,L
           RLCA
           RLCA                   ;BITS 5,4 AND 1,0 TO 7,6 AND 4,3
           XOR L
           AND &CC                ;KEEP BITS 7,6,4,3 OF A
           XOR L                  ;A=4 COPIES OF PARAM BITS 1,0 IF MODE 2
           RLCA                   ;CORRECT BIT SWOP
           LD L,A
           RET


WINDOW:    CALL CRCOLON
           JR Z,ALLWIND

           CALL EXPT4NUMS    ;L,R,T,B
           CALL CHKEND

           CALL GETBYTE      ;B
           LD A,(WINDMAX)    ;LOWEST BOTTOM
           SUB C
           JR C,WDERR        ;ERROR IF WINDOW BOTTOM WILL BE TO LOW

           LD B,C
           PUSH BC
           CALL GETBYTE      ;T
           POP BC            ;B=BOT
           CP B
           JR Z,WND2         ;OK IF WINDOW TOP=WINDOW BOTTOM (1 LINE HIGH)

           JR NC,WDERR       ;ERROR IF WINDOW TOP WILL BE BELOW BOTTOM

WND2:      LD C,A
           PUSH BC           ;BOT/TOP
           CALL GETBYTE      ;R
           LD A,(WINDMAX+1)  ;MAX RHS MARGIN
           CP C
           JR C,WDERR        ;ERROR IF TOO FAR RIGHT

           PUSH BC
           CALL GETBYTE      ;LHS
           POP HL            ;L=RHS
           CP L
           JR NC,WDERR       ;ERROR IF WINDOW IS ONE CHAR OR LESS WIDE

SETWIND:   LD H,A            ;HL=LHS/RHS
           LD (UWRHS),HL
           POP HL            ;HL=BOT/TOP
           LD (UWTOP),HL
           LD H,L
           LD L,A            ;LHS
           LD (SPOSNU),HL    ;TOP/LHS
           RET

ALLWIND:   CALL CHKEND

           LD HL,(WINDMAX)   ;L=BOT, H=RHS
           XOR A
           LD D,L
           LD E,A            ;DE=BOT/TOP
           PUSH DE
           LD L,H            ;L=RHS
           JR SETWIND

WDERR:     RST &08
           DB 54             ;'Invalid WINDOW'



OUT:       CALL SYNTAX8

           CALL GETBYTE
           PUSH AF
           CALL GETINT
           POP AF
           OUT (C),A
           RET

STOP:      CALL CHKEND

           RST &08
           DB 16             ;'STOP statement'


RANDOMIZE: CALL SYNTAX3      ;NUMBER OR ZERO

           CALL GETINT
           LD A,H
           OR L
           JR NZ,RANDOM1

           LD HL,(FRAMES)    ;RANDOMIZE OR RANDOMIZE 0 USES FRAMES

RANDOM1:   LD (SEED),HL
           RET

                                 ;ON ERROR, FINDER, MNINIT, PALETTE, BLOCKS,
                                 ;KEY, DEVICE, PAUSE, COITEM2
