
init-compile: init.elc

init.elc: init.el
	rm -vf "$@"
	emacs -Q -l $< --batch -f batch-byte-compile $<
	if emacs -Q --batch -l $@ --eval '(message "foo")' 2>&1 | grep ^Error; then exit 1; fi

install: init.elc

realclean:
	rm -rvf ~/.emacs.d/elpa init.elc

.PHONY: init-compile install
