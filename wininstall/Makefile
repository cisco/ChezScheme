VERSION := 9.5.9
WIXEXTENSIONS := -ext WixUIExtension -ext WixBalExtension
export MSYS_NO_PATHCONV=1

ChezScheme.exe: x86/bundle.wixobj ChezScheme32.msi ChezScheme64.msi
	cmd.exe /c light.bat -nologo $(WIXEXTENSIONS) $< -out $@

ChezScheme32.msi: x86/product.wixobj x86/examples.wixobj x86/i3nt.wixobj x86/ti3nt.wixobj x86/vcredist.wixobj x86/ui.wixobj
	cmd.exe /c light.bat -nologo $(WIXEXTENSIONS) $^ -sice:ICE64 -sice:ICE03 -sice:ICE82 -sice:ICE61 -out "$@"

ChezScheme64.msi: x64/product.wixobj x64/a6nt.wixobj x64/examples.wixobj x64/ta6nt.wixobj x64/vcredist.wixobj x64/ui.wixobj
	cmd.exe /c light.bat -nologo $(WIXEXTENSIONS) $^ -sice:ICE64 -sice:ICE03 -sice:ICE82 -sice:ICE61 -out "$@"

x86/%.wixobj: %.wxs
	cmd.exe /c candle.bat -nologo -dVersion=$(VERSION) $(WIXEXTENSIONS) $< -out $@

x64/%.wixobj: %.wxs
	cmd.exe /c candle.bat -arch x64 -nologo -dVersion=$(VERSION) $(WIXEXTENSIONS) $< -out $@

vcredist.wxs: locate-vcredist.bat
	cmd.exe /c locate-vcredist.bat

.PHONY: workareas
workareas:
	cd ..; ./configure -m=a6nt; $(MAKE) -C a6nt
	cd ..; ./configure -m=i3nt; $(MAKE) -C i3nt
	cd ..; ./configure -m=ta6nt; $(MAKE) -C ta6nt
	cd ..; ./configure -m=ti3nt; $(MAKE) -C ti3nt

.PHONY: clean
clean:
	-rm -rf *.msi *.wixpdb x86/ x64/ vcredist.wxs *.wixobj *.exe
