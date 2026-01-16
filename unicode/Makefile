Scheme=../bin/zuo .. run

doit:
	$(MAKE) update
	$(MAKE) unicode-char-cases.ss unicode-charinfo.ss

update:
	./get-UNIDATA

unicode-char-cases.ss: extract-char-cases.ss extract-common.ss unicode-data.ss
	echo | $(Scheme) -q extract-char-cases.ss

unicode-charinfo.ss: extract-info.ss extract-common.ss unicode-data.ss
	echo | $(Scheme) -q extract-info.ss

unicode-char-cases.ss: \
 UNIDATA/CaseFolding.txt\
 UNIDATA/CompositionExclusions.txt\
 UNIDATA/DerivedCoreProperties.txt\
 UNIDATA/emoji-data.txt\
 UNIDATA/GraphemeBreakProperty.txt\
 UNIDATA/SpecialCasing.txt\
 UNIDATA/UnicodeData.txt

unicode-charinfo.ss: \
 UNIDATA/UnicodeData.txt\
 UNIDATA/WordBreakProperty.txt\
 UNIDATA/PropList.txt
