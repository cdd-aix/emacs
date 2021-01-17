HOME ?= ~
DESTDIR ?= $(PWD)/build
EMACSD = $(DESTDIR)/.emacs.d
REPOLISP = $(EMACSD)/repo-lisp

init.elc: init.el $(EMACSD)/init.el | $(REPOLISP)
	rm -vf $@ $(EMACSD)/$@ $(EMACSD)/$@ || :
	HOME=$(DESTDIR) emacs -Q -l $< --batch -f batch-byte-compile $<
	HOME=$(DESTDIR) emacs -Q --batch -l $@ --eval '(message "foo")' 2>&1 > $@.test
	! grep ^Error $@.test
	rm -f $@.test

.DELETE_ON_ERROR:

$(REPOLISP): $(EMACSD)
	rsync -av $(@F) $<

$(EMACSD):
	mkdir -p $@

INITEL = $(addprefix $(EMACSD)/, init.el init.elc)
install: $(INITEL) # Missing custom.el.  Need better handling
	rsync -av $(EMACSD)/ $(HOME)/.emacs.d

$(EMACSD)/%: % | $(EMACSD)
	cp -pv $< $@

package: clean $(DESTDIR)/emacsd.zip

$(DESTDIR)/emacsd.zip: package.list
	cd $(@D); find .emacs.d package.list -type f | sort | zip -9q $(@F) -@

package.list: $(DESTDIR)/package.list

$(DESTDIR)/package.list: $(INITEL)
	find $(EMACSD) -name \*-pkg.el | awk -F/ '{print $(NF-1)}' | sort > $@

clean:
	rm -rf $(DESTDIR) init.elc

test: $(INITEL)
	@echo Check init.el, foo.py, foo.yaml, and foo.md buffers
	HOME=$(DESTDIR) emacs init.el foo.py foo.yaml foo.md

cleantest: clean test

.PHONY: install test cleantest
