VERSION := 9.4.1
WIXEXTENSIONS := -ext WixUIExtension

Chez\ Scheme.msi: product.wixobj a6nt.wixobj examples.wixobj i3nt.wixobj ta6nt.wixobj ti3nt.wixobj
	cmd.exe /c light.bat -nologo $(WIXEXTENSIONS) product.wixobj a6nt.wixobj examples.wixobj i3nt.wixobj ta6nt.wixobj ti3nt.wixobj -sice:ICE64 -out "$@"

%.wixobj: %.wxs
	cmd.exe /c candle.bat -nologo -dVersion=$(VERSION) $(WIXEXTENSIONS) $<

.PHONY: workareas
workareas:
	cd ..; ./configure -m=a6nt; $(MAKE) -C a6nt/c
	cd ..; ./configure -m=i3nt; $(MAKE) -C i3nt/c
	cd ..; ./configure -m=ta6nt; $(MAKE) -C ta6nt/c
	cd ..; ./configure -m=ti3nt; $(MAKE) -C ti3nt/c

.PHONY: clean
clean:
	-rm -f Chez\ Scheme.msi *.wixobj *.wixpdb
