# Asserting my directory preference.  Fix later
EMACS_DIR := $(HOME)/p/emacs
EMACSD := $(HOME)/.emacs.d
install: $(EMACS_DIR)/init.elc  $(EMACS_DIR)/init.el
	mkdir -p "$(EMACSD)"
	ln --symbolic --no-dereference --force --target-directory=$(EMACSD) $^
