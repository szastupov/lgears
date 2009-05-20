.PHONY: debug release
all: debug

debug:
	@test -f debug/Makefile || (mkdir -p debug && cd debug && cmake -DCMAKE_BUILD_TYPE=Debug ..)
	@$(MAKE) -C debug

release:
	@test -f release/Makefile || (mkdir -p release && cd release && cmake -DCMAKE_BUILD_TYPE=Release ..)
	@$(MAKE) -C release

snapshot:
	git archive --format=tar --prefix=lgears/ HEAD|gzip > lgears_git-$(shell date +'%F').tar.gz
