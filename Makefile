# You can override this for example using 'EMACS=emacs25 make -e test'. The '-e'
# tells make to give variables taken from the environment precedence over variables
# from makefiles.
EMACS = emacs

.PHONY: all
all:
	cask build

.PHONY: clean
clean:
	cask clean-elc
	rm -rfd ./example-config/straight

.PHONY: test
test:
	$(EMACS) -nw --debug-init -Q \
		--eval '(setq user-emacs-directory "./example-config/")' \
		--load example-config/init.el \
		--visit README.md
