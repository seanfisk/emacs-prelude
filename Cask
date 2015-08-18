;;; Package archives
;;
;; When sourcing MELPA, its versions will take precedence over all of
;; MELPA Stable's. Unfortunately, there are some packages that are not
;; available on MELPA Stable, and also not available through fetchers
;; (e.g., packages on the wiki). There is currently no way through
;; Cask to pin a package to a specific source. For now, we're just
;; installing everything from MELPA, even though we'd like to install
;; with preference from MELPA Stable.
;;
;; Hopefully source-based selection will be available soon:
;; https://github.com/cask/cask/issues/212
;;
;; Order appears not to matter for this list. Packages are chosen by
;; the highest version.
(source gnu) ; for let-alist and csv-mode
(source melpa)
(source org)



;;; Prelude packages
;;
;; These packages are required because they are used for Prelude. This
;; list is copied from 'core/prelude-packages.el'.
(depends-on "ace-window") ; chosen over `switch-window' and `buffer-move'
(depends-on "avy") ; chosen over `ace-jump-mode'
(depends-on "anzu")
(depends-on "browse-kill-ring")
(depends-on "dash")
(depends-on "discover-my-major")
(depends-on "diff-hl")
(depends-on "diminish")
(depends-on "easy-kill")
(depends-on "epl")
(depends-on "expand-region")
(depends-on "flycheck")
(depends-on "gist")
(depends-on "git-timemachine")
(depends-on "gitconfig-mode")
(depends-on "gitignore-mode")
(depends-on "god-mode")
(depends-on "grizzl")
(depends-on "guru-mode")
(depends-on "ov")
(depends-on "projectile")
(depends-on "magit")
(depends-on "move-text")
(depends-on "operate-on-number")
(depends-on "smartparens")
(depends-on "smartrep")
(depends-on "smart-mode-line") ; chosen over `powerline'
(depends-on "undo-tree")
(depends-on "volatile-highlights")
(depends-on "zenburn-theme")
(depends-on "zop-to-char")
(depends-on "exec-path-from-shell") ; From 'core/prelude-osx.el'
(depends-on "vkill") ; From 'core/prelude-osx.el'
;; Now Prelude packages required by Prelude modules that we are using.
(depends-on "rainbow-mode") ; css, emacs-lisp
(depends-on "elisp-slime-nav") ; emacs-lisp
(depends-on "haskell-mode") ; haskell
(depends-on "helm") ; helm
(depends-on "helm-projectile") ; helm
;; helm-descbinds on MELPA-stable is old, pull directly from Git.
;;(depends-on "helm-descbinds" :git "https://github.com/emacs-helm/helm-descbinds.git") ; helm-everywhere
(depends-on "helm-descbinds")
(depends-on "helm-ag") ; helm-everywhere
(depends-on "js2-mode") ; js
(depends-on "json-mode") ;js
(depends-on "key-chord") ; key-chord
(depends-on "rainbow-delimiters") ; lisp
(depends-on "ruby-tools") ; ruby
(depends-on "inf-ruby") ; ruby
;;(depends-on "yari" :git "https://github.com/hron/yari.el.git") ; ruby
(depends-on "yari")
(depends-on "geiser") ; scheme
(depends-on "scss-mode") ; scss
(depends-on "web-mode") ; web
(depends-on "yaml-mode") ; yaml



;;; My required packages
(depends-on "auctex")
(depends-on "bind-key")
(depends-on "comment-dwim-2")
(depends-on "cmake-mode")
(depends-on "csv-mode")
(depends-on "cython-mode")
(depends-on "dash-at-point")
(depends-on "dtrt-indent")
(depends-on "editorconfig")
(depends-on "ein")
(depends-on "f")
(depends-on "fill-column-indicator")
;; Out of the box, flyspell slows down editing. That's the last thing
;; I need. flyspell-lazy runs flyspell only when idle, preventing lag.
(depends-on "flyspell-lazy")
;; Chosen over https://github.com/wasamasa/form-feed#alternatives
;; pp-c-l's header is kind of ugly, and page-break-lines displays in
;; such a way that the display wraps, which isn't right. form-feed
;; does not display correctly in terminal, but looks great in
;; graphical, which is an OK compromise.
(depends-on "form-feed")
(depends-on "git-timemachine")
(depends-on "goto-last-change")
(depends-on "graphviz-dot-mode")
(depends-on "header2")
(depends-on "highlight-indentation")
(depends-on "highlight-symbol")
(depends-on "inflections")
(depends-on "jump-char")
(depends-on "markdown-mode")
(depends-on "misc-cmds")
(depends-on "multiple-cursors")
;; nyan-mode can't install because it's missing a package version.
;; (depends-on nyan-mode)
(depends-on "paredit")
(depends-on "s")
;; The difference between `smooth-scroll' and `smooth-scrolling' is
;; this: `smooth-scroll' changes "Page Up" and "Page Down" to show all
;; the area scrolled when using those operations. I don't like this.
;; `smooth-scrolling' prevents the cursor from hitting the bottom or
;; top of the screen when using next and previous line. This is what I
;; want, because it allows viewing the context of the line with the
;; cursor.
(depends-on "smooth-scrolling")
(depends-on "undo-tree")
(depends-on "unfill")
(depends-on "use-package")
(depends-on "whole-line-or-region")
(depends-on "yasnippet")
