# define default document pathname here
# override on command line with 'make x=newdoc'
x = release_notes

distdir=../release/dist$(x)
commondir=$(distdir)/common

# define latex processor: latex or pdflatex
latex = pdflatex

# define stex macro files here
stexmacrofiles =

# list bibliography files here
bib =

# define index if an index is to be generated
# index=yes

include ~/stex/Mf-stex

# define or override suffixes here

# define any additional targets here

install: $x.pdf $x.html
	installsh -m 2755 -d $(distdir)
	installsh -m 2755 -d $(commondir)
	installsh -m 0644 --ifdiff $x.html $x.pdf $(commondir)
	installsh -m 0644 --ifdiff releasenotes.css $(commondir)
	(X=`echo canned/*` ;\
         if [ "$$X" != "canned/*" ] ; then\
           installsh -m 2755 -d $(commondir)/canned ;\
           installsh -m 0644 --ifdiff canned/* $(commondir)/canned ;\
         fi)
	installsh -m 2755 -d $(commondir)/gifs
	installsh -m 0644 --ifdiff gifs/*.gif $(commondir)/gifs
	installsh -m 2755 -d $(commondir)/math
	-/bin/rm -rf $(commondir)/$(mathdir)
	installsh -m 2755 -d $(commondir)/$(mathdir)
	if [ -e $(mathdir)/0.gif ] ; then installsh -m 0644 $(mathdir)/*.gif $(commondir)/$(mathdir) ; fi

# define any dependencies here

$(x).firstrun: macros.tex

# define cleanup targets here:

$(x).clean:

$(x).reallyclean:

$(x).reallyreallyclean:
