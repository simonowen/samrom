ROM=samrom.bin
SYM=samrom.sym

SOURCES=assign.asm do.asm editor.asm endprint.asm eval.asm fn.asm \
 fpcmain.asm grabput.asm graph0.asm graph1.asm graph2.asm list.asm \
 lookvar.asm main.asm mainlp.asm misc1.asm misc2.asm misc31.asm misc32.asm \
 miscx1.asm miscx2.asm mult.asm nparpro.asm printfp.asm roll.asm rom1fns.asm \
 scrfn.asm scrsel1.asm scrsel2.asm tadjm.asm tapemn.asm tapex.asm text.asm \
 tprint.asm transend.asm using.asm vars.asm

.PHONY: clean

$(ROM): samrom.asm $(SOURCES)
	pyz80.py --obj=$(ROM) --exportfile=$(SYM) -o /dev/null samrom.asm

clean:
	rm -f $(ROM) $(SYM)
