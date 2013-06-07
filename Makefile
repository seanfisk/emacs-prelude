INSTALL = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -b -m 644
INSTALL_DIRECTORY = $(INSTALL) -d
# INSTALL_RECURSIVE idea stolen from
# <http://lists.freebsd.org/pipermail/freebsd-ports/2007-February/038476.html>
INSTALL_RECURSIVE = $(SHELL) -c 'find "$$1" | cpio -dmpuv "$$2"' --

# Install files recursively, but only install *.el files.
INSTALL_EL_RECURSIVE = $(SHELL) -c 'find "$$1" -name "*.el" | cpio -dmpuv "$$2"' --

# pass prefix on the command-line to change install location
# e.g.,
# make prefix=/my/different/prefix
prefix = $(HOME)/.emacs.d
AQUAMACS_INSTALL_DIR = $(HOME)/Library/Preferences/Aquamacs Emacs

.PHONY : first install install-dirs aquamacs emacs-repo-path
first :
	@echo 'Please type ...'
	@echo "  \`make install' to install to \`$(prefix)'"
	@echo "  \`make aquamacs' to install to \`$(AQUAMACS_INSTALL_DIR)'"
	@echo "  \`make prefix=/my/different/prefix install' to install to a different directory."

install : install-dirs emacs-repo-path
	$(INSTALL_DATA) init.el "$(prefix)"

aquamacs : prefix = $(AQUAMACS_INSTALL_DIR)
aquamacs : install-dirs emacs-repo-path
	$(INSTALL_DATA) init.el "$(prefix)/Preferences.el"

install-dirs :
	$(INSTALL_DIRECTORY) "$(prefix)"
	$(INSTALL_DATA)      prelude-modules.el "$(prefix)"
	$(INSTALL_RECURSIVE) core "$(prefix)"
	$(INSTALL_RECURSIVE) modules "$(prefix)"
	$(INSTALL_RECURSIVE) snippets "$(prefix)"
	$(INSTALL_RECURSIVE) themes "$(prefix)"
	$(INSTALL_RECURSIVE) personal "$(prefix)"
# Use EL_RECURSIVE so we don't install the git repository for
# use-package.
	$(INSTALL_EL_RECURSIVE) vendor "$(prefix)"

# Write this repository's path to a file in $(prefix) so that the
# repository can be opened directly from Emacs.
emacs-repo-path :
	pwd > "$(prefix)/emacs-repo-path"
