PRELUDE_DIRECTORIES=prelude snippets themes personal
EMACS_INSTALL_DIR=$(HOME)/.emacs.d
AQUAMACS_INSTALL_DIR=$(HOME)/Library/Preferences/Aquamacs Emacs

.PHONY: emacs aquamacs
emacs:
	install -d "$(EMACS_INSTALL_DIR)"
	install -b -m 644 init.el "$(EMACS_INSTALL_DIR)"
	cp -R $(PRELUDE_DIRECTORIES) "$(EMACS_INSTALL_DIR)"

aquamacs:
	install -d "$(AQUAMACS_INSTALL_DIR)"
	install -b -m 644 init.el "$(AQUAMACS_INSTALL_DIR)/Preferences.el"
	cp -R $(PRELUDE_DIRECTORIES) "$(AQUAMACS_INSTALL_DIR)"
