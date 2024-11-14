VERSION := 10.2.0
WIXEXTENSIONS := -ext WixUIExtension -ext WixBalExtension
export MSYS_NO_PATHCONV=1

ChezScheme.exe: x86/bundle.wixobj ChezScheme32.msi ChezScheme64.msi
	cmd.exe /c "light.bat -nologo $(WIXEXTENSIONS) $< -out $@"

ChezScheme32.msi: x86/product.wixobj x86/examples.wixobj x86/i3nt.wixobj x86/ti3nt.wixobj x86/ui.wixobj
	cmd.exe /c "light.bat -nologo $(WIXEXTENSIONS) $^ -sice:ICE64 -sice:ICE03 -sice:ICE82 -sice:ICE61 -out $@"

ChezScheme64.msi: x64/product.wixobj x64/a6nt.wixobj x64/examples.wixobj x64/ta6nt.wixobj x64/ui.wixobj
	cmd.exe /c "light.bat -nologo $(WIXEXTENSIONS) $^ -sice:ICE64 -sice:ICE03 -sice:ICE82 -sice:ICE61 -out $@"

x86/%.wixobj: %.wxs
	cmd.exe /c "candle.bat -nologo -dVersion=$(VERSION) $(WIXEXTENSIONS) $< -out $@"

x64/%.wixobj: %.wxs
	cmd.exe /c "candle.bat -arch x64 -nologo -dVersion=$(VERSION) $(WIXEXTENSIONS) $< -out $@"

# After building a6nt, use it to create boot files for the
# other machine types, since that's faster than going through
# pb every time
.PHONY: workareas
workareas:
	cd ..; cmd.exe /c "build.bat a6nt"
	cd ..; ./zuo.exe . bootquick i3nt
	cd ..; ./zuo.exe . bootquick ta6nt
	cd ..; ./zuo.exe . bootquick ti3nt
	cd ..; cmd.exe /c "build.bat i3nt"
	cd ..; cmd.exe /c "build.bat ta6nt"
	cd ..; cmd.exe /c "build.bat ti3nt"

.PHONY: clean
clean:
	-rm -rf *.msi *.wixpdb x86/ x64/ *.wixobj *.exe
