# ZEnv - Forth for the ZX Spectrum
# Copyright 2021 (C) - Christopher Leonard, MIT Licence
# https://github.com/veltas/zenv

# Makefile

.PHONY: all
all: bin/zenv.tap

.PHONY: clean
clean:
	-rm -r bin

bin:
	mkdir -p $@

#zenv-unpadded.bin: zenv.asm
bin/zenv.bin: $(wildcard src/*.asm) | bin
	sjasmplus --nologo src/zenv.asm --lst=bin/zenv.lst --raw="$@"

bin/zenv.tap: bin/zenv.bin | bin
	bin2tap -b -cb 7 -cp 7 -ci 0 -o $@ $<

bin/zenv.wav: bin/zenv.tap | bin
	tape2wav $< $@

# Stereo version for use with a stereo phone cable on the ZX Spectrum.
bin/stereo.wav: bin/zenv.wav | bin
	ffmpeg -y -i $< -af "aeval=c=stereo:exprs=val(0)|-val(0)" $@

# ROM-style version?
#zenv.bin: zenv-unpadded.bin
#	head -c $$(( 16384 - $$( wc -c $< | sed 's/ .*//' ) )) /dev/zero | cat $< - > $@
