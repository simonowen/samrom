; Test program to compare assembled ROM with live ROM
;
; NOTE: to simplify the comparison the assembled image is partly overwritten

        org &7000
        dump $
        autoexec

        xor a                 ; mode 1
        call &015a            ; MODET - set screen mode
        ld  a,2               ; main screen
        call &0112            ; SETSTRM - set output stream

        ld  hl,&8000          ; assembled ROM0
        ld  de,&0000          ; original ROM0
lp:     ld  a,(de)
        cp  (hl)
        call nz,mismatch
        inc hl
        inc de
        bit 6,d               ; stop after 16K
        jr  z,lp

        ld  a,2               ; page 2
        out (251),a           ; assembled ROM1 now at &8000

        in  a,(250)
        or  %01000000         ; ROM1 enable
        out (250),a           ; original ROM1 now at &c000

        ld  hl,&8000          ; assembled ROM1
        ld  de,&c000          ; original ROM1
lp2:    ld  a,(de)
        cp  (hl)
        call nz,mismatch
        inc hl
        inc de
        bit 6,d               ; stop after 16K
        jr  nz,lp2

        ld  a,"O"
        rst 16
        ld  a,"K"
        rst 16

        ld  a,1               ; page 1
        out (251),a           ; assembled ROM now between &8000-ffff

        di                    ; interrupts off for LMPR change
        in  a,(250)
        and %10111111         ; ROM1 off
        or  %00100000         ; ROM1 on
        out (250),a
        
        ld  hl,&8000
        ld  de,&0000
        ld  bc,&4000
        ldir                  ; copy assembled ROM0 to &0000

        halt                  ; all done


mismatch:
        push de
        push hl
        ld  a,(hl)
        push af
        ld  a,(de)
        push af
        ld  a,e
        push af
        ld  a,d
        call prhex
        pop af
        call prhex
        ld  a," "
        rst 16
        pop af
        call prhex
        ld  a," "
        rst 16
        pop af
        call prhex
        ld  a,13
        rst 16
        pop hl
        pop de
        ret

prhex:  push af
        rra
        rra
        rra
        rra
        and %00001111
prhexd: add a,"0"
        cp  "9"+1
        jr  c,prleft
        add a,"A"-"0"-10
prleft: rst 16
        pop af
        and %00001111
        add a,"0"
        cp  "9"+1
        jr  c,prright
        add a,"A"-"0"-10
prright:rst 16
        ret

include "samrom.asm"
