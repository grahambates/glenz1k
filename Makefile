program=out/glenz

# Emulator options
MODEL=A500
FASTMEM=0
CHIPMEM=512
SLOWMEM=512

BIN_DIR = ~/amiga/bin

# Binaries
VASM = $(BIN_DIR)/vasmm68k_mot
ZX0 = $(BIN_DIR)/ZX0
MKADF = $(BIN_DIR)/mkadf
FSUAE = /Applications/FS-UAE-3.app/Contents/MacOS/fs-uae
VAMIGA = /Applications/vAmiga.app/Contents/MacOS/vAmiga

# Flags:
VASMFLAGS = -m68000 -x -opt-size -nosym
FSUAEFLAGS = --floppy_drive_0_sounds=off --video_sync=1 --automatic_input_grab=0  --chip_memory=$(CHIPMEM) --fast_memory=$(FASTMEM) --slow_memory=$(SLOWMEM) --amiga_model=$(MODEL) --automatic_input_grab=0

$(program).adf: $(program).bb
	$(info Installing bootblock $<)
	$(MKADF) $< > $@

run: $(program).adf
	$(FSUAE) $(FSUAEFLAGS) $<

out/effect.bin: effect.asm
	$(VASM) $(VASMFLAGS) -Fbin -o $@ $<

out/effect.bin.zx0: out/effect.bin Makefile
	$(ZX0) -f $<

$(program).bb: _main.asm out/effect.bin.zx0
	$(info Assembling bootblock for $<)
	$(VASM) $< $(VASMFLAGS) -Fbin -o $@

clean:
	$(RM) -f out/*
