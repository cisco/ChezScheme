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
 UNIDATA/CompositionExclusions.txt\
 UNIDATA/UnicodeData.txt\
 UNIDATA/CaseFolding.txt\
 UNIDATA/SpecialCasing.txt\
 UNIDATA/GraphemeBreakProperty.txt

unicode-charinfo.ss: \
 UNIDATA/UnicodeData.txt\
 UNIDATA/WordBreakProperty.txt\
 UNIDATA/PropList.txt\
 UNIDATA/emoji-data.txt
