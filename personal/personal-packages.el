;;; personal-packages.el --- Personal list of package.el packages.
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: local
;; Compatibility: GNU Emacs: 24.x, Aquamacs: 3.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; List of packages to install using package.el from ELPA, Marmalade,
;; and MELPA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

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
         flymake-python-pyflakes
         js2-mode
         js2-refactor
         smart-tabs-mode
         )))

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
                js2-mode                 ; MELPA
                mo-git-blame
                smart-tabs-mode         ; MELPA
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
(define-key global-map (kbd "<C-S-up>")     'buf-move-up)
(define-key global-map (kbd "<C-S-down>")   'buf-move-down)
(define-key global-map (kbd "<C-S-left>")   'buf-move-left)
(define-key global-map (kbd "<C-S-right>")  'buf-move-right)

;; flymake-python-pyflakes
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; flymake-shell
(add-hook 'sh-set-shell-hook 'flymake-shell-load)

;; goto-last-change
;; when using AZERTY keyboard, consider C-x C-_
(define-key global-map (kbd "C-x C-/") 'goto-last-change)

;; smex
(smex-initialize)
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)
(define-key global-map [remap execute-extended-command] 'smex)
(define-key global-map (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(define-key global-map (kbd "C-c C-c M-x") 'execute-extended-command)

;; whitespace-mode
;; don't visualize newlines (with a $)
(require 'whitespace)
(setq whitespace-style (remove 'newline-mark whitespace-style))

;; dired-x
;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

(provide 'personal-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-packages.el ends here
