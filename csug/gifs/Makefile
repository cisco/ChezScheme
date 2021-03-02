gifs = ghostRightarrow.gif

# ch1.gif ch2.gif ch3.gif ch4.gif ch5.gif ch6.gif ch7.gif ch8.gif ch9.gif\
# ch10.gif ch11.gif ch12.gif

density=-r90x90

.SUFFIXES:
.SUFFIXES: .tex .gif .eps

# translate ps file to ppm, crop to minimum background, and translate ppm
# to gif with white (background) transparent
#
.tex.gif:
	echo | latex $* &&\
          dvips -f < $*.dvi |\
          gs -q -dNOPAUSE -dSAFER -sDEVICE=ppmraw -sOutputFile=-\
          ${density} - |\
          pnmcrop |\
          ppmtogif -transparent white > $*.gif
	rm -f $*.dvi $*.log *.aux
	test -f $*.gif && chmod 644 $*.gif

# translate ps file to gif w/o transparent white background
.eps.gif:
	cat $*.eps |\
          gs -q -dNOPAUSE -dSAFER -sDEVICE=ppmraw -sOutputFile=-\
          ${density} - |\
          pnmcrop |\
          ppmtogif > $*.gif
	rm -f $*.dvi $*.log *.aux
	test -f $*.gif && chmod 644 $*.gif

all: ${gifs}

# make ghostRightarrow.gif a completely transparent version of Rightarrow.ps
#
# translate ps to gif as above but w/o making white transparent, map black
# to white, convert to ppm, and convert back to gif with white transparent
#
# could skip intermediate conversion to gif if we could map black to white
# with some ppm tool
#
# it seems like should be able to replace last three steps with
#    giftrans -g '#000000=#ffffff' -t '#ffffff'
# or at least
#    giftrans -g '#000000=#ffffff' | giftrans -t '#ffffff'
# but giftrans changes only the first white color it sees, not all
#
ghostRightarrow.gif: Rightarrow.tex
	echo | latex Rightarrow &&\
          dvips -f < Rightarrow.dvi |\
          gs -q -dNOPAUSE -dSAFER -sDEVICE=ppmraw -sOutputFile=-\
          ${density} - |\
          pnmcrop |\
          ppmtogif |\
          giftrans -g '#000000=#ffffff' |\
          giftopnm |\
          ppmtogif -transparent white > $*.gif
	rm -f Rightarrow.dvi Rightarrow.log Rightarrow.aux
	test -f $*.gif && chmod 644 $*.gif

clean: ; rm -f *.gif Make.out
