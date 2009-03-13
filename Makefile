all:
	@make -C vm

clean:
	@make -C vm clean

snapshot:
	git archive --format=tar --prefix=lgears/ HEAD|gzip > lgears_git-$(shell date +'%F').tar.gz
