PREFIX ?= /usr/local
PROG = lgears

all:
	@$(MAKE) -C vm

clean:
	@$(MAKE) -C vm clean

install:
	@mkdir -p -m755 $(DESTDIR)$(PREFIX)/share/$(PROG)
	@$(MAKE) -C vm install
	@find scheme -iname '*.scm' -type f|cpio -pdu $(DESTDIR)$(PREFIX)/share/$(PROG)

snapshot:
	git archive --format=tar --prefix=lgears/ HEAD|gzip > lgears_git-$(shell date +'%F').tar.gz
