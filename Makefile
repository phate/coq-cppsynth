build: Makefile.coq
	$(MAKE) -f Makefile.coq

install: Makefile.coq build
	$(MAKE) -f Makefile.coq install

clean: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq Makefile.coq.conf

Makefile.coq:
	coq_makefile -f _CoqProject -o Makefile.coq

.PHONY: build install clean
