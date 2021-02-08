.PHONY: all
all: zenv.tap

.PHONY: clean
clean:
	rm -f zenv.bin zenv.lst zenv.tap zenv.wav stereo.wav

#zenv-unpadded.bin: zenv.asm
zenv.bin: $(wildcard *.asm)
	sjasmplus --nologo zenv.asm --lst=zenv.lst --raw=zenv.bin

zenv.tap: zenv.bin
	bin2tap -b -cb 7 -cp 7 -ci 0 -o $@ $<

zenv.wav: zenv.tap
	tape2wav $< $@

# Stereo version for use with a stereo phone cable on the ZX Spectrum.
stereo.wav: zenv.wav
	ffmpeg -y -i $< -af "aeval=c=stereo:exprs=val(0)|-val(0)" $@

# ROM-style version?
#zenv.bin: zenv-unpadded.bin
#	head -c $$(( 16384 - $$( wc -c $< | sed 's/ .*//' ) )) /dev/zero | cat $< - > $@
