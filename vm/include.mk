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
CFLAGS += -pipe

.PHONY: clean echo-deps ctags all

all: $(targets)

.deps/%.dep: %.c
	@mkdir -p .deps
	@set -e; rm -f $@; \
		$(CC) -M $(CFLAGS) $< > $@; \
		sed -i 's,\($*\)\.o[ :]*,\1.o $@ : ,g' $@;

deps := $(foreach o,$(targets:=_obj),$($(o):%.o=.deps/%.dep))
-include $(deps)

define target_template
$(1): $$($(1)_obj)
all_objs += $$($(1)_obj)
$(1)_install: $(1)
	install -D $$^ $$(DESTDIR)$$(PREFIX)/bin/$$^
endef

$(foreach prog,$(targets),$(eval $(call target_template,$(prog))))

$(targets):
	$(LINK.o) $^ -o $@

install: $(targets:=_install)
	@echo "Installed"

echo-deps:
	@echo $(deps)

clean:
	rm -f $(all_objs) $(deps) $(targets) gmon.out

ctags:
	@ctags *.c *.h

etags:
	@etags *.c *.h

check-syntax:
	$(CC) $(CFLAGS) -fsyntax-only $(CHK_SOURCES)
