HOME ?= ~
DESTDIR ?= $(PWD)/build
EMACSD = $(DESTDIR)/.emacs.d
REPOLISP = $(EMACSD)/repo-lisp

init.elc: init.el cache-elpa | $(REPOLISP)
	rsync -a --delete-after $(ELPA) $(EMACSD)
	rm -vf $@ $(EMACSD)/$@ $(EMACSD)/$@ || true
	HOME=$(DESTDIR) emacs -Q -l $< --batch -f batch-byte-compile $< > $@.log || (cat $@.log; exit 1)
	HOME=$(DESTDIR) emacs -Q --batch -l $@ --eval '(message "foo")' 2>&1 > $@.test
	! grep ^Error $@.test
	rm -f $@.test

.DELETE_ON_ERROR:

$(REPOLISP): $(EMACSD)
	rsync -a $(@F) $<

$(EMACSD):
	mkdir -p $@

INITEL = $(addprefix $(EMACSD)/, init.elc init.el)
install: $(INITEL) remove-rubbish
# Missing custom.el.  Need better handling
	rsync -a $(EMACSD)/ $(HOME)/.emacs.d

$(EMACSD)/%: % | $(EMACSD)
	cp -pv $< $@

package: clean $(DESTDIR)/emacsd.zip

$(DESTDIR)/emacsd.zip: Makefile package.list remove-rubbish
	cd $(@D); find .emacs.d package.list -type f | sort | zip -9q $(@F) -@

remove-rubbish: RUBBISH = $(addprefix $(EMACSD)/, *.eld *.sqlite auto-save-list)
remove-rubbish: $(INITEL)
	rm -rvf $(RUBBISH) || true


package.list: $(EMACSD)/package.list

$(EMACSD)/package.list: $(INITEL)
	find $(EMACSD) -name \*-pkg.el | awk -F/ '{print $(NF-1)}' | sort > $@

clean:
	rm -rf $(DESTDIR) init.elc

test: $(INITEL)
	@echo Check init.el, foo.py, foo.yaml, and foo.md buffers
	HOME=$(DESTDIR) emacs init.el foo.py foo.yaml foo.md

cleantest: clean test

CACHEDIR ?= $(PWD)/cache
ELPA = $(CACHEDIR)/.emacs.d/elpa
cache-elpa: $(ELPA)
$(ELPA): init.el
	HOME=$(CACHEDIR) EMACSLOADPATH=$(PWD)/repo-lisp: emacs -Q -l init.el --batch --eval '(message "foo")'

update-cache-elpa: clean-cache-elpa cache-elpa

clean-cache-elpa:
	rm -rf $(CACHEDIR)

.PHONY: install test cleantest
