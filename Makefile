PRELUDE_DIRECTORIES=prelude snippets themes personal vendor
EMACS_INSTALL_DIR=$(HOME)/.emacs.d
AQUAMACS_INSTALL_DIR=$(HOME)/Library/Preferences/Aquamacs Emacs

.PHONY: first emacs aquamacs
first:
	@echo 'Please type ...'
	@echo "  \`make emacs' to install to \`$(EMACS_INSTALL_DIR)'"
	@echo "  \`make aquamacs' to install to \`$(AQUAMACS_INSTALL_DIR)'"

emacs:
	install -d "$(EMACS_INSTALL_DIR)"
	install -b -m 644 init.el "$(EMACS_INSTALL_DIR)"
	cp -R $(PRELUDE_DIRECTORIES) "$(EMACS_INSTALL_DIR)"

aquamacs:
	install -d "$(AQUAMACS_INSTALL_DIR)"
	install -b -m 644 init.el "$(AQUAMACS_INSTALL_DIR)/Preferences.el"
	cp -R $(PRELUDE_DIRECTORIES) "$(AQUAMACS_INSTALL_DIR)"
