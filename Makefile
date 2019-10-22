
init-compile: init.elc

init.elc: init.el
	emacs -Q --batch -f batch-byte-compile $<

install: init.elc

.PHONY: init-compile install
