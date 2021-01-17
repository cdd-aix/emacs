HOME ?= ~
DESTDIR ?= $(PWD)/build
EMACSD = $(DESTDIR)/.emacs.d
REPOLISP = $(EMACSD)/repo-lisp

init.elc: init.el | $(REPOLISP)
	rm -vf $@ $(EMACSD)/$@ $(EMACSD)/$@ $(EMACSD)/$< || :
	HOME=$(DESTDIR) emacs -Q -l $< --batch -f batch-byte-compile $<
	HOME=$(DESTDIR) emacs -Q --batch -l $@ --eval '(message "foo")' 2>&1 > $@.test
	! grep ^Error $@.test
	rm -f $@.test

.DELETE_ON_ERROR:

$(REPOLISP): $(EMACSD)
	rsync -av $(@F) $<

$(EMACSD):
	mkdir -p $@

install: $(addprefix $(EMACSD)/, init.el init.elc) # Missing custom.el.  Need better handling
	rsync -av $(EMACSD)/ $(HOME)/.emacs.d

$(EMACSD)/%: % | $(EMACSD)
	cp -pv $< $@

package: init.el
	rm -rvf $(DESTDIR)
	make -B init.elc install
	cd $(HOME); find .emacs.d -type f | sort | zip -9v ../emacsd.zip -@

package.list: realclean init.elc
	find $(HOME)/.emacs.d -name \*-pkg.el | awk -F/ '{print $(NF-1)}' | sort > $@

realclean:
	rm -rvf $(HOME)/.emacs.d/elpa init.elc
	rm -rvf $(DESTDIR)

test: init.elc
	@echo Check init.el, foo.py, foo.yaml, and foo.md buffers
	emacs -Q -l init.elc init.el foo.py foo.yaml foo.md

cleantest: realclean test

.PHONY: install test cleantest
