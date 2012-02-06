; Glue module for assembling SAM Coupe ROM (using pyz80)

    ; Write the assembled ROM image at &8000
    DUMP &8000

    ; ROM0
    include "main.asm"       ;MAIN FILE
    include "vars.asm"       ;SYSTEM VARIABLES
    include "editor.asm"     ;BASIC EDITOR
    include "list.asm"       ;AUTOLIST, LIST, SPACAN,
    include "roll.asm"       ;CALCPIX, NEXTUP, NXTDOWN, CLSWIND, EDRS, CRTBF
    include "mainlp.asm"     ;MAIN LOOP
    include "misc1.asm"      ;PRHSH, STRMINFO, SETSTRM, CHANFLAG,
    include "lookvar.asm"    ;SETUP VARS
    include "eval.asm"       ;ROM 0 FNS
    include "do.asm"         ;DO, LOOP, LOOP IF, EXIT IF, ON, GOSUB,
    include "tadjm.asm"      ;KEYSCAN, FPSTACK, SETMIN, SEARCH
    include "graph0.asm"     ;CIRCLE, DRAW
    include "graph1.asm"     ;DRAW CURVE, PLOT
    include "graph2.asm"     ;BLITZ, FILL, TRANSCR, CRDFID
    include "grabput.asm"    ;GRAB, PUT, FARLDIR, STRMOV
    include "assign.asm"     ;STKVAR, ASSIGN, SYNTAX1, DIM, SLICER
    include "fn.asm"         ;DEF FN, FN, COMPILE, DEF PROC, LOCAL, PROC
    include "nparpro.asm"    ;PARAM PROCESSING
    include "misc2.asm"      ;BOOT, ERRORS, BUFMV AND STUBS FOR BUFF CMDS,
    include "endprint.asm"   ;END PRINT, ANY DE ADDR, PIXADDR, POATTR, NET

    ; Ensure ROM1 code is located in the second 16K block, to fit with paging
    ; assumptions made by both ROMs, and to avoid code straddling the gap
    DUMP &C000

    ; ROM1
    include "miscx1.asm"     ;PARAM PROCESSING
    include "miscx2.asm"     ;DEF KEYCODE, READ, DEF FN, TOKEN.
    include "fpcmain.asm"    ;CALCVARS, FPCMAIN
    include "transend.asm"   ;SIN, COS, EXP, ETC
    include "mult.asm"       ;ARITHMETIC
    include "rom1fns.asm"    ;POINT
    include "scrsel1.asm"    ;OPEN/CLOSE SCREEN/PAGES, INTS, BOOTEX, SOUND,
    include "scrsel2.asm"    ; GOTO, CONT, GETTOKEN, MODPT2, AUTO
    include "printfp.asm"    ;PRINT A NUMBER
    include "tprint.asm"     ;PRINT ASCII AND CR.
    include "tapemn.asm"     ;SAVE, LOAD, MERGE, VERIFY
    include "tapex.asm"      ;TAPE/NET EXECUTIVE ROUTINES
    include "using.asm"      ;INSTR, LENGTH, STRING$, XOINTERS
    include "misc31.asm"     ;CALL, BORDER, ZOOM ETC,
    include "misc32.asm"     ;BEEP
    include "scrfn.asm"      ;COPY, SCREEN$, LIST, SPACES, EDPTR2
    include "text.asm"       ;TABLES, MSGS, KEYWORDS, CMDADDRT, CHARSET
