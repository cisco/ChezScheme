VERSION := 10.5.0
WIXEXTENSIONS := -ext WixUIExtension -ext WixBalExtension
export MSYS_NO_PATHCONV=1

ChezScheme.exe: x86/bundle.wixobj ChezSchemeX86.msi ChezSchemeX64.msi ChezSchemeArm64.msi
	cmd.exe /c "light.bat -nologo $(WIXEXTENSIONS) $< -out $@"

ChezSchemeX86.msi: x86/product.wixobj x86/examples.wixobj x86/i3nt.wixobj x86/ti3nt.wixobj x86/ui.wixobj
	cmd.exe /c "light.bat -nologo $(WIXEXTENSIONS) $^ -sice:ICE64 -sice:ICE03 -sice:ICE82 -sice:ICE61 -out $@"

ChezSchemeX64.msi: x64/product.wixobj x64/a6nt.wixobj x64/examples.wixobj x64/ta6nt.wixobj x64/ui.wixobj
	cmd.exe /c "light.bat -nologo $(WIXEXTENSIONS) $^ -sice:ICE64 -sice:ICE03 -sice:ICE82 -sice:ICE61 -out $@"

ChezSchemeArm64.msi: arm64/product.wixobj arm64/arm64nt.wixobj arm64/examples.wixobj arm64/tarm64nt.wixobj arm64/ui.wixobj
	cmd.exe /c "light.bat -nologo $(WIXEXTENSIONS) $^ -sice:ICE64 -sice:ICE03 -sice:ICE82 -sice:ICE61 -out $@"

x86/%.wixobj: %.wxs
	cmd.exe /c "candle.bat -nologo -dVersion=$(VERSION) $(WIXEXTENSIONS) $< -out $@"

x64/%.wixobj: %.wxs
	cmd.exe /c "candle.bat -arch x64 -nologo -dVersion=$(VERSION) $(WIXEXTENSIONS) $< -out $@"

arm64/%.wixobj: %.wxs
	cmd.exe /c "candle.bat -arch arm64 -nologo -dVersion=$(VERSION) $(WIXEXTENSIONS) $< -out $@"

# After building arm64nt, use it to create boot files for the
# other machine types, since that's faster than going through
# pb every time. This must run on ARM64 Windows, which can also
# run the x86 and x64 builds.
.PHONY: workareas
workareas:
	cd ..; cmd.exe /c "build.bat arm64nt"
	cd ..; ./zuo.exe . bootquick tarm64nt
	cd ..; ./zuo.exe . bootquick a6nt
	cd ..; ./zuo.exe . bootquick ta6nt
	cd ..; ./zuo.exe . bootquick i3nt
	cd ..; ./zuo.exe . bootquick ti3nt
	cd ..; cmd.exe /c "build.bat tarm64nt"
	cd ..; cmd.exe /c "build.bat a6nt"
	cd ..; cmd.exe /c "build.bat ta6nt"
	cd ..; cmd.exe /c "build.bat i3nt"
	cd ..; cmd.exe /c "build.bat ti3nt"

.PHONY: clean
clean:
	-rm -rf *.msi *.wixpdb x86/ x64/ arm64/ *.wixobj *.exe
