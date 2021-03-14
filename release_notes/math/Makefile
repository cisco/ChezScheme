include mathfiles

density=-r90x90

.SUFFIXES:
.SUFFIXES: .tex .gif

# translate ps file to ppm, crop to minimum background, and translate ppm
# to gif with white (background) transparent
#
.tex.gif:
	TEXINPUTS=.:../..:$(TEXINPUTS);\
	echo | latex $* &&\
          dvips -f < $*.dvi |\
          gs -q -dNOPAUSE -dSAFER -sDEVICE=ppmraw -sOutputFile=-\
          ${density} - |\
          pnmcrop |\
          ppmtogif -transparent white > $*.gif
	rm -f $*.dvi $*.log $*.aux
	test -f $*.gif && chmod 644 $*.gif

all: ${gifs}

${gifs}: mathmacros

clean: ; rm -f *.gif Make.out
