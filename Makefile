HOME ?= ~
init.elc: init.el
	rm -vf "$@"
	emacs -Q -l $< --batch -f batch-byte-compile $<
	if emacs -Q --batch -l $@ --eval '(message "foo")' 2>&1 | grep ^Error; then exit 1; fi

install: init.elc init.el | $(HOME)/.emacs.d
	cp -v $^ $(HOME)/.emacs.d/
	[ -r $(HOME)/.emacs.d/custom.el ] || cp -v custom.el $(HOME)/.emacs.d/

$(HOME)/.emacs.d:
	mkdir -p $@

realclean:
	rm -rvf $(HOME)/.emacs.d/elpa init.elc

test: init.elc
	@echo Check init.el, foo.py, foo.yaml, and foo.md buffers
	emacs -Q -l init.elc init.el foo.py foo.yaml foo.md

cleantest: realclean test

.PHONY: install test cleantest
