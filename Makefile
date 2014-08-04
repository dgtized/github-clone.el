EMACS ?= emacs
EMACSFLAGS =
CASK = cask

OBJECTS = github-clone.elc

install:
	$(CASK) install

.PHONY: build
build : install $(OBJECTS)

.PHONY: test
test : build
	$(CASK) exec ert-runner

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf .cask # Clean packages installed for development

%.elc : %.el
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
