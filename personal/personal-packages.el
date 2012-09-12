;; Add Marmalade <http://marmalade-repo.org/>

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; The decision here is whether to use packages from MELPA or from
;; Marmalade. I'd like to use MELPA exclusively because I believe it's
;; the future of Emacs packaging. However, MELPA does not currently
;; support stable versions of packages. It's a planned feature, but
;; currently not supported. Therefore, as long as stable packages are
;; not supported in MELPA, I'm going to prefer Marmalade packages over
;; MELPA whenever possible. MELPA packages will be specifically
;; included whenever needed.
(require 'melpa)
(setq package-archive-include-alist
      '(("melpa"
         ;; Prelude packages
         helm
         helm-projectile
         melpa
         rainbow-mode
         yasnippet                 ; the Marmalde version is quite old
         ;; Prelude MELPA packages
         prelude-c
         prelude-clojure
         prelude-coffee
         prelude-common-lisp
         prelude-css
         prelude-emacs-lisp
         prelude-erlang
         prelude-haskell
         prelude-js
         prelude-latex
         prelude-lisp
         prelude-mediawiki
         prelude-perl
         prelude-programming
         prelude-python
         prelude-ruby
         prelude-scheme
         prelude-scss
         prelude-xml
         ;; my MELPA packages
         edit-server
         js2-mode
         js2-refactor
         flymake-python-pyflakes
         )))

;; (package-refresh-contents)
;; (package-initialize)

;; Add my own packages
(require 'prelude-packages)
(setq prelude-packages
      (append '(
                ;; fillcode
                ;; nxhtml
                auto-complete
                auto-complete-clang
                buffer-move
                cmake-mode
                dtrt-indent
                ecb
                edit-server             ; MELPA
                fill-column-indicator
                flymake-cursor
                flymake-python-pyflakes ; MELPA
                flymake-ruby
                flymake-shell
                goto-last-change
                header2
                highlight-symbol
                ido-ubiquitous
                js2-refactor             ; MELPA
                js2-mode                ; MELPA
                mo-git-blame
                rvm
                smex
                smooth-scroll
                ;; solarized theme that does not depend on color-theme
                solarized-theme
                switch-window
                undo-tree
                whole-line-or-region
                zencoding-mode
                ) prelude-packages))

;; Install all of the packages
(prelude-install-packages)

;;; buffer-move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; goto-last-change
;; when using AZERTY keyboard, consider C-x C-_
(global-set-key (kbd "C-x C-/") 'goto-last-change)

;; smex
(smex-initialize)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key [remap execute-extended-command] 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; solarized-theme
(load-theme 'solarized-dark)

;; whitespace-mode
;; having whitespace mode on bothers me
(add-hook 'prelude-prog-mode-hook 'prelude-turn-off-whitespace t)

(provide 'personal-packages)
