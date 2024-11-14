# Makefile
# Copyright 1984-2017 Cisco Systems, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

version = 10.2.0
release = 1
m := $(shell find ../bin/* -type d | xargs basename)
arch := $(shell if test "$(m)" == "i3le" ; then echo i686 ; elif test "$(m)" == "a6le" ; then echo x86_64 ; else echo UNKNOWN ; fi)
DOTUSER := $(shell ls -ld . | sed -e 's/[^ ]* *[^ ]* *\([^ ]*\).*/\1/')
DOTGROUP := $(shell ls -ldg . | sed -e 's/[^ ]* *[^ ]* *\([^ ]*\).*/\1/')
TMP := $(shell pwd)/tmp
SPEC = $(TMP)/ChezScheme-$(version)-$(arch)-$(release).spec
RELEASE = csv$(version)
TARBALL = $(RELEASE)-$(m).tar.gz
RPM = ChezScheme-$(version)-$(release).$(arch).rpm


$(RPM): $(TMP)/$(RPM)
	sudo install -m 644 -o $(DOTUSER) -g $(DOTGROUP) $(TMP)/${RPM} .

$(TMP)/$(RPM): $(SPEC) $(TMP)/$(TARBALL)
	sudo setarch $(arch) rpmbuild\
          --target $(arch)\
          --define "_topdir $(TMP)" \
          --define "_srcrpmdir $(TMP)" \
          --define "_rpmdir $(TMP)" \
          --define "_sourcedir $(TMP)" \
          --define "_builddir $(TMP)" \
          --define "_rpmfilename %{NAME}-%{VERSION}-%{RELEASE}.%{ARCH}.rpm" \
          --quiet -ba $(SPEC)

$(SPEC): $(TMP)
	echo 'Summary: Chez Scheme: A high-performance version of Scheme' > $(SPEC)
	echo 'Name: ChezScheme' >> $(SPEC)
	echo 'Version: $(version)' >> $(SPEC)
	echo 'Release: $(release)' >> $(SPEC)
	echo 'Provides: ChezScheme-runtime-{VERSION}' >> $(SPEC)
	echo 'License: Apache 2.0' >> $(SPEC)
	echo 'URL: http://github.com/cisco/chezscheme' >> $(SPEC)
	echo 'Group: Development/Languages' >> $(SPEC)
	echo 'Source0: $(TARBALL)' >> $(SPEC)
	echo 'BuildRoot: %{_tmppath}/%{name}' >> $(SPEC)
	echo '%description' >> $(SPEC)
	echo 'Chez Scheme is a programming language and an implementation of that language,' >> $(SPEC)
	echo 'with supporting tools and documentation.' >> $(SPEC)
	echo '' >> $(SPEC)
	echo '%prep' >> $(SPEC)
	echo '' >> $(SPEC)
	echo '%setup -T -b 0 -n $(RELEASE)' >> $(SPEC)
	echo '' >> $(SPEC)
	echo '#%build' >> $(SPEC)
	echo '' >> $(SPEC)
	echo '%install' >> $(SPEC)
	echo 'make install TempRoot=%{buildroot}' >> $(SPEC)
	echo '' >> $(SPEC)
	echo '%files' >> $(SPEC)
	echo '#%doc NOTICE' >> $(SPEC)
	echo '#%doc LICENSE' >> $(SPEC)
	echo '/usr/lib/$(RELEASE)' >> $(SPEC)
	echo '/usr/bin/petite' >> $(SPEC)
	echo '/usr/bin/scheme' >> $(SPEC)
	echo '/usr/bin/scheme-script' >> $(SPEC)
	echo '/usr/share/man/man1/petite.1.gz' >> $(SPEC)
	echo '/usr/share/man/man1/scheme.1.gz' >> $(SPEC)

$(TMP)/$(TARBALL): $(TMP) ../bintar/$(TARBALL)
	cp ../bintar/$(TARBALL) $(TMP)

$(TMP):
	mkdir $(TMP)

clean:
	rm -rf $(TMP) $(RPM)
