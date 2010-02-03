; Glue module for assembling SAM Coupe ROM (using pyz80)

    ; Test code to check ROM with native image (uncomment to activate)
    ;include "compare.asm"

    ; Write the assembled ROM image at &8000
    DUMP &8000

    ; ROM0
    include "MAIN.SAM"       ;MAIN FILE
    include "VARS.SAM"       ;SYSTEM VARIABLES
    include "EDITOR.SAM"     ;BASIC EDITOR
    include "LIST.SAM"       ;AUTOLIST, LIST, SPACAN,
    include "ROLL.SAM"       ;CALCPIX, NEXTUP, NXTDOWN, CLSWIND, EDRS, CRTBF
    include "MAINLP.SAM"     ;MAIN LOOP
    include "MISC1.SAM"      ;PRHSH, STRMINFO, SETSTRM, CHANFLAG,
    include "LOOKVAR.SAM"    ;SETUP VARS
    include "EVAL.SAM"       ;ROM 0 FNS
    include "DO.SAM"         ;DO, LOOP, LOOP IF, EXIT IF, ON, GOSUB,
    include "TADJM.SAM"      ;KEYSCAN, FPSTACK, SETMIN, SEARCH
    include "GRAPH0.SAM"     ;CIRCLE, DRAW
    include "GRAPH1.SAM"     ;DRAW CURVE, PLOT
    include "GRAPH2.SAM"     ;BLITZ, FILL, TRANSCR, CRDFID
    include "GRABPUT.SAM"    ;GRAB, PUT, FARLDIR, STRMOV
    include "ASSIGN.SAM"     ;STKVAR, ASSIGN, SYNTAX1, DIM, SLICER
    include "FN.SAM"         ;DEF FN, FN, COMPILE, DEF PROC, LOCAL, PROC
    include "NPARPRO.SAM"    ;PARAM PROCESSING
    include "MISC2.SAM"      ;BOOT, ERRORS, BUFMV AND STUBS FOR BUFF CMDS,
    include "ENDPRINT.SAM"   ;END PRINT, ANY DE ADDR, PIXADDR, POATTR, NET

    ; Ensure ROM1 code is located in the second 16K block, to fit with paging
    ; assumptions made by both ROMs, and to avoid code straddling the gap
    DUMP &C000

    ; ROM1
    include "MISCX1.SAM"     ;PARAM PROCESSING
    include "MISCX2.SAM"     ;DEF KEYCODE, READ, DEF FN, TOKEN.
    include "FPCMAIN.SAM"    ;CALCVARS, FPCMAIN
    include "TRANSEND.SAM"   ;SIN, COS, EXP, ETC
    include "MULT.SAM"       ;ARITHMETIC
    include "ROM1FNS.SAM"    ;POINT
    include "SCRSEL1.SAM"    ;OPEN/CLOSE SCREEN/PAGES, INTS, BOOTEX, SOUND,
    include "SCRSEL2.SAM"    ; GOTO, CONT, GETTOKEN, MODPT2, AUTO
    include "PRINTFP.SAM"    ;PRINT A NUMBER
    include "TPRINT.SAM"     ;PRINT ASCII AND CR.
    include "TAPEMN.SAM"     ;SAVE, LOAD, MERGE, VERIFY
    include "TAPEX.SAM"      ;TAPE/NET EXECUTIVE ROUTINES
    include "USING.SAM"      ;INSTR, LENGTH, STRING$, XOINTERS
    include "MISC31.SAM"     ;CALL, BORDER, ZOOM ETC,
    include "MISC32.SAM"     ;BEEP
    include "SCRFN.SAM"      ;COPY, SCREEN$, LIST, SPACES, EDPTR2
    include "TEXT.SAM"       ;TABLES, MSGS, KEYWORDS, CMDADDRT, CHARSET
