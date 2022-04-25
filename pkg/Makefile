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

m := $(shell find ../bin/* -type d | xargs basename)
version = 9.5.9
release = 1

DOTUSER = $(shell ls -ld . | sed -e 's/[^ ]* *[^ ]* *\([^ ]*\).*/\1/')
DOTGROUP = $(shell ls -ldg . | sed -e 's/[^ ]* *[^ ]* *\([^ ]*\).*/\1/')
BUILDROOT = $(m)$(version)
RELEASE = csv$(version)
TARBALL = $(RELEASE)-$(m).tar.gz
PKG = $(RELEASE)-$(m)-$(release).pkg

PKGCONTENT =\
 $(BUILDROOT)/Resources/en.lproj/Welcome.html\
 $(BUILDROOT)/Resources/en.lproj/License.txt\
 $(BUILDROOT)/Distribution\
 $(BUIDROOT)/Root/bin\
 $(BUILDROOT)/Root/lib\
 $(BUILDROOT)/Root/man

$(PKG): $(BUILDROOT)/$(PKG)
	sudo /usr/bin/productbuild\
          --resources $(BUILDROOT)/Resources\
          --distribution $(BUILDROOT)/Distribution\
          --package-path $(BUILDROOT)\
          $(PKG)
	sudo chown $(DOTUSER):$(DOTGROUP) $(PKG)
	sudo rm -rf $(RELEASE) $(BUILDROOT)

$(BUILDROOT)/$(PKG): $(PKGCONTENT)
	sudo /usr/bin/pkgbuild\
          --root $(BUILDROOT)/Root\
          --identifier chezscheme\
          --version $(version)\
          --install-location /\
          --ownership recommended\
          $(BUILDROOT)/$(PKG)

$(BUILDROOT)/Distribution: $(BUILDROOT)
	echo '<?xml version="1.0" encoding="utf-8" standalone="no"?>' > $(BUILDROOT)/Distribution
	echo '<installer-gui-script minSpecVersion="1">' >> $(BUILDROOT)/Distribution
	echo '    <pkg-ref id="chezscheme">' >> $(BUILDROOT)/Distribution
	echo '        <bundle-version/>' >> $(BUILDROOT)/Distribution
	echo '    </pkg-ref>' >> $(BUILDROOT)/Distribution
	echo '    <title>Chez Scheme</title>' >> $(BUILDROOT)/Distribution
	echo '    <welcome file="Welcome.html"/>' >> $(BUILDROOT)/Distribution
	echo '    <license file="License.txt"/>' >> $(BUILDROOT)/Distribution
	echo '    <options customize="never" rootVolumeOnly="true"/>' >> $(BUILDROOT)/Distribution
	echo '    <options customize="never" require-scripts="false"/>' >> $(BUILDROOT)/Distribution
	echo '    <choices-outline>' >> $(BUILDROOT)/Distribution
	echo '        <line choice="default">' >> $(BUILDROOT)/Distribution
	echo '            <line choice="chezscheme"/>' >> $(BUILDROOT)/Distribution
	echo '        </line>' >> $(BUILDROOT)/Distribution
	echo '    </choices-outline>' >> $(BUILDROOT)/Distribution
	echo '    <choice id="default"/>' >> $(BUILDROOT)/Distribution
	echo '    <choice id="chezscheme" visible="false">' >> $(BUILDROOT)/Distribution
	echo '        <pkg-ref id="chezscheme"/>' >> $(BUILDROOT)/Distribution
	echo '    </choice>' >> $(BUILDROOT)/Distribution
	echo '    <pkg-ref id="chezscheme" version="$(version)" onConclusion="none" installKBytes="1639">#$(PKG)</pkg-ref>' >> $(BUILDROOT)/Distribution
	echo '</installer-gui-script>' >> $(BUILDROOT)/Distribution
	chmod 644 $(BUILDROOT)/Distribution

$(BUILDROOT)/Resources/en.lproj/Welcome.html: $(BUILDROOT)/Resources/en.lproj
	echo '<html>' > $(BUILDROOT)/Resources/en.lproj/Welcome.html
	echo '<h3>Chez Scheme Version $(version)</h3>' >> $(BUILDROOT)/Resources/en.lproj/Welcome.html
	echo '<p>Copyright &copy; 2022 Cisco Systems, Inc.</p>' >> $(BUILDROOT)/Resources/en.lproj/Welcome.html
	echo '<p>Chez Scheme is a programming language and an implementation of that language, with supporting tools and documentation.</p>' >> $(BUILDROOT)/Resources/en.lproj/Welcome.html
	echo '</html>' >> $(BUILDROOT)/Resources/en.lproj/Welcome.html
	chmod 644 $(BUILDROOT)/Resources/en.lproj/Welcome.html

$(BUILDROOT)/Resources/en.lproj/License.txt: $(BUILDROOT)/Resources/en.lproj
	cat ../../NOTICE ../../LICENSE > $(BUILDROOT)/Resources/en.lproj/License.txt
	chmod 644 $(BUILDROOT)/Resources/en.lproj/License.txt

$(BUILDROOT)/Resources/en.lproj: $(BUILDROOT)/Resources
	install -d $(BUILDROOT)/Resources/en.lproj

$(BUILDROOT)/Resources: $(BUILDROOT)
	install -d $(BUILDROOT)/Resources

$(BUIDROOT)/Root/bin $(BUILDROOT)/Root/lib $(BUILDROOT)/Root/man: $(BUILDROOT)/Root $(RELEASE)
	( cd $(RELEASE); sudo make install InstallGroup=wheel TempRoot=../$(BUILDROOT)/Root )

$(BUILDROOT)/Root: $(BUILDROOT)
	install -d $(BUILDROOT)/Root

$(RELEASE): $(BUILDROOT) ../bintar/$(TARBALL)
	tar -xzf ../bintar/$(TARBALL)

$(BUILDROOT):
	install -d $(BUILDROOT)

clean:
	rm -rf $(PKG) $(BUILDROOT) $(RELEASE)
