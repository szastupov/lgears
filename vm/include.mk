#
# This file is part of lGears scheme system
# Copyright (C) 2009 Stepan Zastupov <redchrom@gmail.com>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General
# Public Licens along with this program, if not; see
# <http://www.gnu.org/licenses>.
#
# The code is full of magic, but does work :)
#
CFLAGS += -pipe
targets := $(libraries) $(programs)

.PHONY: clean echo-deps ctags all

all: $(targets)

.deps/%.dep: %.c
	@mkdir -p .deps
	@set -e; rm -f $@; \
		$(CC) -M $(CFLAGS) $< > $@; \
		sed -i 's,\($*\)\.o[ :]*,\1.o $@ : ,g' $@;

deps := $(foreach o,$(targets:=_obj),$($(o):%.o=.deps/%.dep))

define target_template
$(2): $$($(1)_obj)
trash += $$($(1)_obj)
trash += $(2)
$(1)_install: $(2)
	install -D $$^ $$(DESTDIR)$$(PREFIX)/$(3)/$$^
endef

$(foreach prog,$(programs),$(eval $(call target_template,$(prog),$(prog),bin)))
$(foreach lib,$(libraries),$(eval $(call target_template,$(lib),lib$(lib).so,lib)))

$(programs):
	$(LINK.o) $^ -o $@

%.so:
	$(LINK.o) -shared $^ -o $@

$(libraries): $(libraries:%=lib%.so)

install: $(targets:=_install)
	@echo "Installed"

echo-deps:
	@echo $(deps)

clean:
	rm -f $(trash) $(deps) gmon.out

ctags:
	@ctags *.c *.h

etags:
	@etags *.c *.h

check-syntax:
	$(CC) $(CFLAGS) -fsyntax-only $(CHK_SOURCES)

ifeq ($(findstring clean, $(MAKECMDGOALS)), )
-include $(deps)
endif
