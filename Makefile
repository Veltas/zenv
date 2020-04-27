.PHONY: all
all: zenv.tap

.PHONY: clean
clean:
	rm -f zenv.bin zenv.tap zenv.lst

#zenv-unpadded.bin: zenv.asm
zenv.bin: $(wildcard *.asm)
	sjasmplus --nologo zenv.asm --raw=$@ --lst=zenv.lst

zenv.tap: zenv.bin
	bin2tap -b -o $@ $<

# ROM-style version?
#zenv.bin: zenv-unpadded.bin
#	head -c $$(( 16384 - $$( wc -c $< | sed 's/ .*//' ) )) /dev/zero | cat $< - > $@
