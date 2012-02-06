;EDITOR.SAM

EDER:      CALL KSCHK        ;Z IF K/S CHANNEL
           JP NZ,ERRCHK

           CALL WARNBZ       ;BUZZ AND RE-PRESENT LINE IF K OR S CHANNEL
           JR EDAG

EDCX:      XOR A
           LD (XPTR+1),A
           LD (ERRNR),A      ;NEEDED BY "INPUT LINE"

EDITOR:    LD HL,(EDITV)
           LD A,H
           OR L
           CALL NZ,HLJUMP

           LD HL,(ERRSP)
           PUSH HL
           CALL POFETCH
           LD (OLDPOS),DE

EDAG:      LD HL,EDER
           PUSH HL
           LD (ERRSP),SP
           CALL AULN         ;AUTOMATIC LINE NUMBER ENTERED INTO LINE IF WANTED

EDLP:      LD HL,EDLP
           PUSH HL
           CALL EDFK         ;GET A KEY, OR ENTER USER-DEFINED KEY TEXT
           PUSH AF
           CALL NOISE
           POP AF
           CP &16
           JR NC,ADCH1       ;ENTER CHARS >=16H

           CP 7
           JR C,ADCH1        ;ENTER CHARS 00-06H

           CP &10
           JR NC,TWOKYS      ;JR IF 10H-15H (INK-OVER)

;DEAL WITH EDITING KEYS 07-0FH

           LD HL,EKPT-7      ;ED KEY PTR TABLE
           LD C,A
           LD E,A
           LD D,0
           ADD HL,DE
           LD E,(HL)
           ADD HL,DE
           PUSH HL
           JP ADDRKC

TWOKYS:    CALL ADCH1        ;INSERT CONTROL CODE
           CALL WAITKEY      ;GET PARAM IN A
           JR ADCH1          ;JR TO INSERT PARAM

;CHANNEL "R" - INSERT CHARS AT KCUR, EXPANDING KEYWORDS. USED BY EDKY

ADDCHAR:   CP &85
           JR NC,ADCH07

           LD HL,FLAGS       ;INSERT 00-85H (INCLUDES CONTROL CODES)
           CP &20
           JR Z,ADCH05

           RES 0,(HL)        ;"LEADING SPACE NEEDED"
           CP ":"
           JR NZ,ADCH1

           LD A,(LSTFT)      ;TEMP LIST FLG
           AND A
           LD A,":"
           JR Z,ADCH1

ADCH05:    SET 0,(HL)        ;NO LEADING SPACE NEEDED IF LAST CHAR=SPACE, OR
                             ;LAST CHAR =":" AND PRETTY LISTING ON.
           JR ADCH1

ADCH07:    LD C,A
           LD A,(INQUFG)
           RRCA
           LD A,C
           JR C,ADCH1       ;INSERT CHARS 85H+ IF TOKENS NOT TO BE EXPANDED

           RST &30
           DW PRGR802-&8000  ;PRINT KEYWORD (85-FE) OR FN (FFXX)

;ENTRY HERE ALLOWS ANYTHING TO BE INSERTED

ADCH1:     LD B,A
           CALL R1OSR        ;ROM1 OFF, URPORT SAVED
           PUSH BC           ;CHAR
           CALL ADDRKC       ;ADDR KCUR
           LD BC,1
           CALL MKRMCH       ;MAKE ROOM FOR ONE CHAR. ALLOW ALL SPACE TO BE USED
           POP AF
           LD (HL),A
           INC HL
           LD (KCUR),HL      ;KEEP KCURP UNCHANGED SO SAME BASE AS ELINE OR WS
           JP POPOUT         ;RESTORE PREV LR AND  URPORT STATUS


;GET A KEY, AND IF IT IS A DEF KEY, ENTER ASSOCIATED TEXT INTO LINE

EDFK:      CALL WAITKEY
           CP 192
           RET C             ;RET IF NOT IN USER-DEFINED KEY RANGE

           CP 202
           JR NC,EDFK1       ;JR IF NOT A KEYPAD KEY

           LD C,A
           LD A,(KPFLG)
           RRA
           LD A,C
           JR NC,EDFK1       ;JR IF NUMBER FLAG NOT SET

           SUB 144           ;192-201 -> "0"-"9"
           RET

EDFK1:     CALL FNDKYD       ;LOOK FOR DEFINITION
           LD A,D            ;A=KEY CODE AGAIN
           RET C             ;RET IF NONE

           PUSH HL           ;DEF. START
           ADD HL,BC
           DEC HL            ;HL=DEF END
           LD A,(HL)
           CP ":"
           JR NZ,EDFK2

           DEC BC            ;LOP OFF LAST CHAR IF IT IS ":"

EDFK2:     PUSH AF           ;SAVE LAST CHAR
           PUSH BC           ;LEN
           CALL ADDRKC
           CALL MAKEROOM     ;OPEN ROOM FOR DEF KEY TEXT
           EX DE,HL          ;DE=ROOM
           POP BC            ;LEN
           POP AF
           POP HL            ;SRC
           PUSH AF
           LDIR
           EX DE,HL
           LD (KCUR),HL      ;CURSOR POSN AFTER DEF KEY TEXT
           CALL NOISE
           POP AF
           POP DE            ;RET ADDR
           JP NZ,EDENT       ;ENTER LINE IF LAST CHAR WASN"T ":"

           RET               ;BACK TO EDLP

;ED KEY PTR TABLE

EKPT:      DB EDKY-EKPT      ;7  EDIT
           DB EDLT-EKPT-1    ;8  LEFT
           DB EDRT-EKPT-2    ;9  RIGHT
           DB EDDN-EKPT-3    ;10 DOWN
           DB EDUP-EKPT-4    ;11 UP
           DB EDDLL-EKPT-5   ;0C DEL LEFT
           DB EDENT-EKPT-6   ;0D ENTER
           DB EDDLR-EKPT-7   ;0E DEL RIGHT
           DB EDKPX-EKPT-8   ;0F KEY PAD TOGGLE

;"PRINT" LINE TO ELINE EDITING BUFFER

EDKY:      LD A,(FLAGX)
           AND &20
           JP NZ,CLEARSP     ;JP IF INPUT - CLEAR INPUT LINE

           CALL EVALLINO     ;SKIP ELINE NUMBER, GET IN BC. CY IF TOO BIG
           JR C,EDKY2

           RST &18
           CP &0D
           JR NZ,EDKY2       ;JR IF LINE NEITHER EMPTY NOR JUST LINE NO.

;FROM EDIT (CMD)

EDKY1:     LD A,B
           OR C
           JR Z,EDKY2

           LD (EPPC),BC      ;E.G. ENTER 123 (EDIT) SETS EPPC TO 123

EDKY2:     CALL CLEARSP      ;CLEAR ELINE OR INPUT LINE
           LD HL,(EPPC)
           CALL FNDLINE
           CALL LNNM         ;GET LINE NUMBER OR ZERO
           LD A,D
           OR E
           RET Z             ;DON"T EDIT LINE ZERO

           LD DE,(CURCHL)
           PUSH DE
           LD A,(EPPC+1)
           PUSH AF
           PUSH HL           ;LSB OF LINE NO
           LD A,&FF
           LD (EPPC+1),A     ;ENSURE NO ">" IS PRINTED
           CALL SETSTRM      ;"R" O/P
           LD HL,LISTFLG
           LD A,(HL)
           LD (LSTFT),A      ;PUT PRTY LISTING STATUS WHERE CHAN "R" CAN SEE IT.
           LD (HL),0         ;PRETTY LISTING OFF - COLONS STAY COLONS!
           EX (SP),HL
           DEC HL            ;PT TO LINE START
           RST &30
           DW OUTLINE
           POP HL
           LD A,(LSTFT)
           LD (HL),A         ;PREV LIST FLAG RESTORED.
           POP AF
           LD (EPPC+1),A
           LD HL,(ELINE)
           LD BC,5
           ADD HL,BC
           LD (KCUR),HL      ;CURSOR ADDR IS AFTER 5-DIGIT LINE NUMBER
           POP HL
           JP CHANFLAG

;CURSOR LEFT (ALSO CALLED BY DELETE LEFT)

EDLT:      CALL SETDE        ;GET DE=START OF LINE (HL=CURSOR ADDR). NC

EDLT2:     DEC HL            ;KCUR MOVES LEFT
           SBC HL,DE
           ADD HL,DE
           RET C             ;RET IF AT LINE START ALREADY (NEW POSN WOULD BE
                             ;FF SAVARS TERMINATOR)
           JR Z,EDRLC        ;JR IF NEW POSN WOULD BE START OF LINE

           DEC HL            ;ELSE LOOK AT PREV. CHAR (*NOT* SAVARS TERM)
           LD A,(HL)
           INC HL
           INC A
           JR Z,EDLT2        ;MOVE AGAIN IF CURSOR WOULD POINT TO FN CODE AFTER
                             ;FF PREFIX

           JR EDRLC          ;CHANGE KCUR UNLESS ALREADY AT LINE START

EDRT:      LD A,(HL)
           INC HL
           CP &0D
           RET Z             ;NO MOVE RIGHT IF AT LINE END

           INC A
           JR Z,EDRT         ;MOVE AGAIN IF HIT FFH FN PREFIX

EDRLC:     LD (KCUR),HL
           RET

EDDN:                        ;A=0A
EDUP:      LD A,C            ;KEY 0B/0A
           LD HL,FLAGX
           BIT 5,(HL)
           JR NZ,EDUD2       ;JR IF INPUT MODE

           CALL ADDRELN
           LD A,(HL)
           CP &0D
           LD A,C
           JP Z,FUPDN        ;JR IF EDIT MODE, ELINE EMPTY - MOVE ">" CURSOR

EDUD2:     LD HL,KPOS+1      ;CURSOR LINE
           CP &0B
           JR Z,EDUD25

           INC (HL)
           INC (HL)

EDUD25:    DEC (HL)          ;CURSOR LINE ADJUSTED UP
           LD DE,CUOP
           CALL KOPSET       ;ALTER CHANNEL K TO SPECIAL O/P
           LD HL,(WORKSP)
           DEC HL
           LD A,(FLAGX)
           AND &20
           JR Z,EDUD3        ;JR WITH HL=END OF ELINE IF EDIT MODE

           LD HL,(WKEND)     ;ELSE GET END OF INPUT LINE

EDUD3:     DEC HL
           LD (KCUR),HL      ;SET KCUR TO END OF LINE IN CASE NO MATCH OCCURS.
           CALL NOISE
           CALL EDPRT        ;PRINT LINE, SETTING KCUR TO MATCH WITH KPOS
           LD DE,(MNOP)      ;FBA7 EQU

KOPSET:    LD HL,(CHANS)
           JR DETOHL

;TOGGLE KEYPAD BIT (FUNCTION/NUMBER PAD)

EDKPX:     LD HL,KPFLG
           INC (HL)
           RET

;EDITOR DELETE RIGHT (KEY 0E)

EDDLR:     LD A,(HL)
           CP &0D
           JR NZ,EDDLC

           RET               ;RET IF HIT LINE END

;EDITOR DELETE LEFT

EDDLL:     CALL EDLT
           RET C             ;RET IF AT START OF LINE

EDDLC:     LD BC,2           ;2 BYTES TO DELETE
           LD A,(HL)
           INC A
           JR Z,EDDL3        ;IF FN, DELETE FN PREFIX AND CODE

;CARRIAGE RETURN USED BY LOCAL!

CARET:     DEC C             ;ELSE DEL 1 BYTE
           DEC HL
           LD A,(HL)         ;CHAR BEFORE CHAR TO DEL. (PERHAPS FF SAVARS TERM.)
           INC HL
           CP &16
           JR NC,EDDL3       ;JR IF >OVER

           CP &10
           JR C,EDDL3        ;JR IF NOT INK-OVER

           INC HL
           LD (KCUR),HL      ;PT KCUR PAST PARAM, SO IT WILL BE DELED NEXT TIME
           DEC HL
           DEC HL            ;DELETE CONTROL CODE NOW, NOT PARAM

EDDL3:     JP RECLAIM2       ;RECLAIM AT (HL)


EDENT:     POP AF            ;JUNK EDLP
           POP AF            ;JUNK WARNBZ

ERRCHK:    POP HL

RESESP:    LD (ERRSP),HL
           LD A,(ERRNR)
           AND A
           RET Z             ;RET IF NO ERRORS

           LD SP,HL
           RET               ;ELSE RET TO ERROR HANDLER

;CONTROL CODE PARAM HANDLING ROUTINES

RESTOP:    LD DE,(OPSTORE)

POCHNG:    LD HL,(CURCHL)

DETOHL:    LD (HL),E
           INC HL
           LD (HL),D
           RET

PRERESTOP: LD DE,CCRESTOP
           LD (TVDATA+1),A
           JR POCHNG

;SPECIAL PRINT O/P ROUTINE TO CHECK IF LS POSN MATCHES DESIRED CURSOR POSN AS
;EACH CHAR IS PRINTED. IF SO, KCUR IS SET TO ADDR OF THE CHAR IN THE LINE.

CUOP:      RST &30
           DW CUOPP-&8000    ;JP ROM1

;WARNING BUZZ - EDITOR ERRORS

WARNBZ:    LD A,(DEVICE)
           DEC A
           JR NZ,ERRCHK      ;REPORT ERROR IF NOT LOWER SCREEN DEVICE

;CALLED BY LINE ENTRY ERRORS

RSPNS:     LD H,7
           XOR A
           LD (ERRNR),A      ;"NO ERROR"
           LD A,(RASP)
           JR NS2

;PIP NOISE

NOISE:     LD HL,250
           LD A,(PIP)

NS2:       LD E,A
           LD D,0

BEEPER:    RST &30
           DW BEEPP2-&8000

;RECLAIM ELINE-WORKSP-1 IF EDITING, OR WORKSP-WKEND-1 IF EDITING

CLEARSP:   CALL SETDE
           JR Z,CLRSP2       ;JR IF EDIT MODE

           LD HL,(WKEND)
           JR CLRSP3

CLRSP2:    LD HL,(WORKSP)
           DEC HL            ;PT TO 0D IN ELINE (NOT DELETED)

CLRSP3:    DEC HL
           CALL RECLAIM1     ;CLEAR ELINE

SETKC:     IN A,(URPORT)

SETKC2:    AND &1F
           LD (KCURP),A
           LD (KCUR),HL
           RET

;GET DE=START OF ELINE IF IN EDIT MODE (Z), ELSE DE PTS TO INPUT LINE IN WKSPACE

SETDE:     PUSH HL
           LD A,(FLAGX)
           AND &20
           JR NZ,SETDE2

           CALL ADDRELN
           CP A

SETDE2:    PUSH AF
           CALL NZ,ADDRWK
           EX DE,HL

;USED BY DEF KEYCODE

PPRET:     POP AF
           POP HL
           RET

;ENTRY: USUALLY, HL PTS TO LINE NUMBER, DE TO LINE NO OF PREVIOUS LINE, AND
;EXITS WITH DE=LINE NUMBER. HOWEVER, IF HL HIT PROG TERMINATOR, PREVIOUS LINE NO
;IS RETURNED, AND IF NO LINES EXIST, DE=0

LNNM:      LD A,(HL)
           INC A
           JR NZ,LNNM2       ;JR IF NOT PROG TERMINATOR

           EX DE,HL          ;LOOK AT PREV LINE.
           LD A,(HL)
           INC A
           LD D,A
           LD E,A
           RET Z             ;DE=0 IF NULL PROGRAM

LNNM2:     LD D,(HL)
           INC HL
           LD E,(HL)
           RET

;USED BY SCROLL/SAVE TO TAPE

GTKBK:     CALL KBFLUSH

;USED BY GET AND CONTROL KEY ENTRY ROUTINE

WKBR:      CALL BRKCR        ;CHECK BREAK, STOP IF SO
           CALL KYIP2
           JR Z,WKBR

           RET

WAITKEY:   LD HL,TVFLAG
           LD A,(HL)
           AND &20
           JR NZ,WTKY2       ;JR IF THE LOWER SCREEN IS GOING TO BE CLEARED

           SET 3,(HL)        ;"LINE TO BE PRINTED TO LS" ON 1ST CALL OF INPUTAD

WTKY2:     CALL INPUTAD      ;CALL I/P OF CURRENT CHANNEL - OFTEN KYIP BELOW
           RET C             ;RET IF GOT A KEY IN A
           JR Z,WTKY2        ;JR IF NO KEY, AND NO ERRORS

           RST &08
           DB 22             ;"END OF FILE"


;KEYBOARD INPUT - KYIP ADDR IS IN CHANNELS THAT ALLOW KB INPUT
;EXIT: CY IF GOT KEY IN A. NC,Z IF NO KEY. NC,NZ IF END OF FILE.

KYIP:      LD A,(TVFLAG)
           AND &08
           CALL NZ,EDPRT     ;PRINT LINE TO LS IF REQUIRED

;USED BY GET

KYIP2:     LD HL,FLAGS
           AND A             ;NC
           BIT 5,(HL)
           RET Z             ;RET IF NO KEY PRESSED

           LD A,(LASTK)
           RES 5,(HL)
           PUSH AF
           INC HL
           BIT 5,(HL)        ;TVFLAG
           CALL NZ,CLSLOWER
           POP AF
           CP &16
           CCF
           RET C             ;ACCEPT ANYTHING >=16H

           CP 6
           JR Z,KYCL         ;JR IF CAPS LOCK

           CP &10
           RET C             ;ACCEPT 00-0FH

           LD (KDATA),A      ;SAVE CONTROL CODE 10H-15H (INK-OVER)
           LD DE,KYPM
           JR KYCZ           ;RETURN WITH CONTROL CODE, AND ALTER I/P SO NEXT
                             ;KEY IS ADJUSTED AND CHECKED FOR RANGE VS CC.
KYPM:      LD HL,FLAGS
           LD A,(HL)
           AND &20
           RET Z             ;RET (NC, Z) IF NO KEY

           LD A,(LASTK)
           RES 5,(HL)        ;"NO KEY"
           SUB &30
           JR C,KYPN         ;ONLY ACCEPT 30H+

           CP 8
           JR NC,KYPN        ;LIMIT TO 0-7

           LD B,A
           LD A,(KDATA)      ;10-15H
           CP &12
           JR C,KYPM6        ;JR IF INK OR PAPER; 0-7 OK

           CP &15
           LD A,B            ;PARAM
           JR Z,KYPM5        ;JR IF OVER

           CP 2
           JR NC,KYPN        ;0/1 ONLY FOR FLASH/BRIGHT/INVERSE

KYPM5:     CP 4
           JR NC,KYPN        ;0-3 ONLY FOR OVER

KYPM6:     LD A,B
           LD DE,(MNIP)      ;RESTORE NORMAL I/P

KYCZ:      SCF
           LD HL,(CHANS)     ;KEY I/P CHANNEL ZAP
           INC HL
           INC HL
           JP DETOHL

KYCL:      LD HL,FLAGS2
           LD A,(HL)
           XOR 8             ;REVERSE CAPS LOCK
           LD (HL),A

           LD HL,TVFLAG
           SET 3,(HL)        ;"COPY LINE TO SCREEN"

KYPN:      CP A              ;NC,Z - NO KEY
           RET

;PRINT EDIT/INPUT LINE TO LOWER SCREEN

EDPRT:     RST &30
           DW EDPTR2-&8000

;FORCE NORMAL OUTPUT. USED BY E.G CURSOR OUTPUT TO ENSURE NORMAL OUTPUT EVEN
;IF PRINTING BETWEEN CONTROL CODE AND PARAMETER.

FONOP:     RST &30
           DW FONOP2-&8000

;AUTOMATIC LINE NUMBER ENTRY

AULN:      LD A,(FLAGX)
           AND &20
           RET NZ            ;RET IF INPUT MODE

           LD A,(AUTOFLG)
           AND A
           RET Z             ;RET IF AUTO OFF

           CALL ADDRELN
           LD A,(HL)
           CP &0D
           RET NZ            ;RET IF LINE NOT EMPTY

           LD HL,(EPPC)
           LD BC,(AUTOSTEP)
           ADD HL,BC
           LD A,H
           CP &FF
           RET Z             ;RET IF LINE NUMBER TOO BIG

           PUSH HL
           LD A,&FF
           CALL SETSTRM      ;CHANNEL "R"
           POP BC
           RST &30
           DW PRNUMB1

STRM0:     XOR A
           JP SETSTRM

;FNDKYD - FIND DEF KEY DATA
;ENTRY: A=KEY CODE (192-255)
;EXIT: HL PTS TO START OF DEFINITION, BC=LEN, D=KEY CODE. CY IF NOT FOUND
;USES AF,BC,D,HL
;DEFINITIONS TERMINATED BY FFH

;ENTRY AT DKTR FINDS POSN OF TERMINATOR,+3

DKTR:      LD A,&FF

FNDKYD:    LD D,A
           LD HL,(DKDEF)

FDKL:      LD A,(HL)         ;CODE
           INC HL
           LD C,(HL)
           INC HL
           LD B,(HL)         ;BC=LEN
           INC HL
           ADD A,1
           RET C             ;RET IF NOT FOUND - (TERMINATOR)

           DEC A
           CP D
           RET Z             ;RET IF FOUND

           ADD HL,BC         ;PT TO NEXT DEFINITION
           JR FDKL


;Z IF K OR S CHANNEL. USED BY EDITOR AND INPUT

KSCHK:     LD A,(CLET)
           CP "K"
           RET Z

           CP "S"
           RET
