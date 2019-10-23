# Asserting my directory preference.  Fix later
EMACS_DIR := $(HOME)/p/emacs
EMACSD := $(HOME)/.emacs.d
LANG=en_US.UTF-8

EMACS_BATCH = emacs -Q --batch -l init.el
EMACS_BATCH_BYTE_COMPILE = $(EMACS_BATCH) -f batch-byte-compile
GREP_BATCH_ERROR = egrep '^(Error|Failed)'

init.elc init.log: init.el elpa/.done
	rm -vf $@
	$(EMACS_BATCH_BYTE_COMPILE) $< 2>&1 | tee init.log
	if $(GREP_BATCH_ERROR) init.log; then rm -vf $@; exit 1; fi
elpa/.done elpa.log: init.el elpa/archives/gnu/archive-contents.signed
# Ugly keyring bootstrap on ubuntu 18.04
# https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
	rm -vf init.elc
	$(EMACS_BATCH) --eval '(princ package-user-dir)' 2>&1 | tee elpa.log
	if $(GREP_BATCH_ERROR) elpa.log; then exit 1; fi
	$(EMACS_BATCH) --eval '(byte-recompile-directory package-user-dir 0 t)'
	touch $@
elpa/archives/gnu/archive-contents.signed: bootstrapkeyring.el bootstrapkeyring.sh
	./bootstrapkeyring.sh "$@"
refresh:
	$(EMACS_BATCH) --eval '(package-refresh-contents)'
realclean:
	rm -rvf elpa
	rm -vf init.elc
install: $(EMACS_DIR)/init.elc  $(EMACS_DIR)/init.el
	mkdir -p "$(EMACSD)"
	ln --symbolic --no-dereference --force --target-directory=$(EMACSD) $^
