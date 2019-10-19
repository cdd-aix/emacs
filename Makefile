# Asserting my directory preference.  Fix later
EMACS_DIR := $(HOME)/p/emacs
EMACSD := $(HOME)/.emacs.d
LANG=en_US.UTF-8

EMACS_BATCH = emacs -Q --batch -l init.el
EMACS_BATCH_BYTE_COMPILE = $(EMACS_BATCH) -f batch-byte-compile

init.elc init.log: init.el elpa
	rm -vf $@
	$(EMACS_BATCH_BYTE_COMPILE) $< > init.log 2>&1
	if grep ^Error init.log; then exit 1; fi
elpa: init.el
	rm -vf init.elc
	$(EMACS_BATCH) --eval '(princ package-user-dir)'
realclean:
	rm -rvf elpa
	rm -vf init.elc
install: $(EMACS_DIR)/init.elc  $(EMACS_DIR)/init.el
	mkdir -p "$(EMACSD)"
	ln --symbolic --no-dereference --force --target-directory=$(EMACSD) $^
