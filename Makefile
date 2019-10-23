HOME ?= ~
init.elc: init.el
	rm -vf "$@"
	emacs -Q -l $< --batch -f batch-byte-compile $<
	if emacs -Q --batch -l $@ --eval '(message "foo")' 2>&1 | grep ^Error; then exit 1; fi

install: init.elc init.el
	mkdir -p $(HOME)/.emacs.d
	cp -v $^ $(HOME)/.emacs.d/
	[ -r $(HOME)/.emacs.d/custom.el ] || cp -v custom.el $(HOME)/.emacs.d/

realclean:
	rm -rvf $(HOME)/.emacs.d/elpa init.elc

test: init.elc
	emacs -Q -l init.elc init.el

cleantest: realclean test

.PHONY: install test cleantest
