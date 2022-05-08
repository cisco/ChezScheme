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

version = 9.5.9
m := $(shell find ../bin/* -type d | xargs basename)

R = csv$(version)
TARBALL = $(R)-$(m).tar.gz

CONTENTS=\
  $(R)/LICENSE\
  $(R)/NOTICE\
  $(R)/scheme.1.in\
  $(R)/installsh\
  $(R)/Makefile\
  $(R)/examples\
  $(R)/boot\
  $(R)/bin

$(TARBALL): $(CONTENTS)
	( BROKEN=`find -L $R -type l` ; \
          if test -n "$$BROKEN" ; then \
             echo "Error: missing $(BROKEN)" ; \
             exit 1 ; \
          fi )
	tar -czhf $(TARBALL) $R
	rm -rf $(R) 

$(R)/LICENSE: $(R)
	( cd $(R) ; ln -s ../../../LICENSE . )

$(R)/NOTICE: $(R)
	( cd $(R) ; ln -s ../../../NOTICE . )

$(R)/scheme.1.in: $(R)
	( cd $(R) ; ln -s ../../scheme.1.in . )

$(R)/installsh: $(R)
	( cd $(R) ; ln -s ../../installsh . )

$(R)/Makefile: $(R)
	( cd $(R) ; ln -s ../../Mf-install Makefile )

$(R)/examples: $(R)
	( cd $(R) ; ln -s ../../examples . )

$(R)/boot: $(R)
	mkdir -p $(R)/boot/$(m)
	( cd $(R)/boot/$(m) ; ln -s ../../../../boot/$(m)/{scheme.h,petite.boot,scheme.boot,revision} . )
	case $(m) in \
          *nt) \
            ( cd $R/boot/$(m) ; ln -s ../../../../boot/$(m)/{csv959md.lib,csv959mt.lib,mainmd.obj,mainmt.obj,scheme.res} . ) \
            ;; \
          *) \
            ( cd $R/boot/$(m) ; ln -s ../../../../boot/$(m)/{main.o,kernel.o} . ) \
            ;; \
        esac

$(R)/bin: $(R)
	mkdir -p $(R)/bin/$(m)
	case $(m) in \
          *nt) \
            ( cd $R/bin/$(m) ; ln -s ../../../../bin/$(m)/{scheme.exe,csv959.dll,csv959.lib,vcruntime140.lib} . ) \
            ;; \
          *) \
            ( cd $R/bin/$(m) ; ln -s ../../../../bin/$(m)/scheme . ) \
            ;; \
        esac

$(R):
	mkdir $(R)

clean:
	rm -rf $(R) $(TARBALL)
