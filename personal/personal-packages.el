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

(require 'package)
;; Add Marmalade <http://marmalade-repo.org/>
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Add org ELPA <http://orgmode.org/elpa.html>
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; The decision here is whether to use packages from MELPA or from
;; Marmalade. I'd like to use MELPA exclusively because I believe it's
;; the future of Emacs packaging. However, MELPA does not currently
;; support stable versions of packages. It's a planned feature, but
;; currently not supported. Therefore, as long as stable packages are
;; not supported in MELPA, I'm going to prefer Marmalade packages over
;; MELPA whenever possible. MELPA packages will be specifically
;; included whenever needed.
(require 'melpa)
(setq package-archive-enable-alist
      '(("melpa"
         ;; Prelude packages
         helm
         helm-projectile
         jump-char                      ; Marmalade version is old
         melpa
         multiple-cursors           ; experimental, so stay up to date
         rainbow-mode
         yaml-mode                ; the Marmalade version is quite old
         yasnippet                ; the Marmalade version is quite old
         ;; My MELPA packages
         auto-complete-clang
         edit-server
         ;; elpy ;; MELPA elpy (submitted by me) is now broken by some recent changes
         ;; smart-tabs-mode
         )))

;; use-package <https://github.com/jwiegley/use-package> is a great
;; way to manage the config. See the Github README or the commentary
;; in the file for more documentation. Undocumented but in the code is
;; the use of the `:ensure' plist key to automatically install through
;; ELPA. Also, take note of the `bind-key' utility.
(require 'use-package)

;; Start by installing diminish for use with use-package.
(use-package diminish
  :ensure t)

;;; auto-complete
(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :init (global-auto-complete-mode +1)
  :config (progn
            (defun personal-auto-complete-mode-setup ()
              (local-set-key (kbd "M-/") 'auto-complete))
            (add-hook 'auto-complete-mode-hook
                      'personal-auto-complete-mode-setup)))

;;; auto-complete-clang
(use-package auto-complete-clang
  :ensure t
  :config (progn
            (defun personal-ac-cc-mode-setup ()
              (setq ac-sources
                    (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
            (add-hook 'c-mode-common-hook 'personal-ac-cc-mode-setup)))

;;; buffer-move
(use-package buffer-move
  :ensure t
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right)
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(use-package dtrt-indent
  :ensure t
  :defer t)

;; Haven't taken the time to learn this yet.
;;(use-package ecb)

;; Haven't used this in a while.
;; (use-package edit-server
;;   :ensure t)

;;; elpy
;; For elpy to work correctly, the following packages need to be
;; installed. They should be installed in the user site directory for
;; the system Python. If using a virtualenv, install to the virtualenv
;; and have elpy restart in that virtualenv. Don't include --user for
;; a virtualenv.
;;
;;     pip install [--user] elpy rope jedi pyflakes pep8
;;
(use-package elpy
  :ensure t
  :init (elpy-enable)
  :config (progn
            (setq python-check-command "flake8")
            (elpy-clean-modeline)))

;;; fill-column-indicator
;; Specifically *don't* set `fci-fill-column' (the column at which the
;; line is shown). `fci-mode' will then default to using the value of
;; `fill-column'.
(use-package fill-column-indicator
  :ensure t
  :config (add-hook 'prog-mode-hook 'turn-on-fci-mode))

;;; No ELPA repository for this yet.
;;(use-package fillcode)

;;; flymake-cursor
;; (eval-after-load 'flymake '(require 'flymake-cursor))

;;; flymake-shell
;; (add-hook 'sh-set-shell-hook 'flymake-shell-load)

;; Out of the box, flyspell slows down editing. That's the last thing
;; I need. flyspell-lazy runs flyspell only when idle, preventing lag.
(use-package flyspell-lazy
  :ensure t
  :init (flyspell-lazy-mode +1))

;;; goto-last-change
;; when using AZERTY keyboard, consider C-x C-_
(use-package goto-last-change
  :ensure t
  :bind ("C-x C-/" . goto-last-change))
;;(define-key global-map (kbd "C-x C-/") 'goto-last-change)

(use-package header2
  :ensure t
  :defer t)

;;; highlight-symbol
;; I don't really want to enable highlight symbol mode globally, but
;; it appears the easiest solution at the moment.
(use-package highlight-symbol
             :ensure t
             :bind (("C-c C-n" . highlight-symbol-next)
                    ("C-c C-p" . highlight-symbol-prev))
             :init (highlight-symbol-mode +1))

(use-package ido-ubiquitous
  :ensure t
  :init (progn
          ;; If we have ido-ubiquitous, there's no need for icomplete.
          (icomplete-mode -1)
          (ido-ubiquitous-mode +1)))

;;; jump-char
(use-package jump-char
  :ensure t
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

(use-package maxframe
  :ensure t
  :config (add-hook 'window-setup-hook 'maximize-frame t))

(use-package mo-git-blame
  :ensure t
  :defer t)

;;; multiple-cursors
(use-package multiple-cursors
             :ensure t
             :bind (("C-S-c C-S-c" . mc/edit-lines)
                    ("C->" . mc/mark-next-like-this)
                    ("C-<" . mc/mark-previous-like-this)
                    ("C-c C-<" . mc/mark-all-like-this-dwim)))

;; No ELPA package for this yet.
;;(use-package nxhtml)

(use-package org
  :ensure t
  :defer t)

;;; smart-tabs-mode
;; Use the convenience function to load these automatically.
;; (smart-tabs-insinuate 'c)

;;; smex
(use-package smex
  :ensure t
  :bind (("C-x C-m" . smex)
         ("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; This is your old M-x.
         ("C-c C-c M-x" . execute-extended-command))
  :init (smex-initialize))

;;; smooth-scrolling
;; The difference between `smooth-scroll' and `smooth-scrolling' is
;; this: `smooth-scroll' changes "Page Up" and "Page Down" to show all
;; the area scrolled when using those operations. I don't like
;; this. `smooth-scrolling' prevents the cursor from hitting the
;; bottom or top of the screen when using next and previous line. This
;; is what I want, because it allows viewing the context of the line
;; with the cursor.
(use-package smooth-scrolling
  :ensure t)

;;; switch-window
;; This package has no autoloads, so it doesn't "do" anything when
;; it's just installed through ELPA. It needs to be required, at which
;; point it takes over the `C-x o' binding.
;;(require 'switch-window)
(use-package switch-window
  :ensure t)

;;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode +1))

;;; whole-line-or-region
(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode
  :init (whole-line-or-region-mode +1))

;;; dired-x
;; C-x C-j opens dired with the cursor right on the file you're
;; editing.
(use-package dired-x
  :defer t)



;;; Include vendorized code.
;; Even if these files have autoload cookies, the autoload files are
;; probably not generated. I believe use-package calls autoload on
;; commands that are specified with :bind. However, for packages
;; without bindings, probably the best idea is to use the :commands
;; keyword of use-package. Find a better solution later.

;;; comment-or-uncomment-line-or-region
;; I'd like this set to `C-;' or `C-c C-c'. Right now flyspell is
;; overriding the first one. See
;; <http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs>
;; for a way to fix it.
;;
;; This should also be in prog-mode map instead of the global map.
;;
;; This is overriding `comment-dwim' right now. The only thing that
;; `comment-dwim' does that `comment-or-uncomment-region-or-line' does
;; is add trailing comments, which I use sparingly anyway. And this
;; function also works on lines when no region is activated. However,
;; I don't like overriding this key binding, so consider changing it
;; later.
(use-package comment-or-uncomment-region-or-line
  :bind ("M-;" . comment-or-uncomment-region-or-line))

(use-package cython-mode
  :commands cython-mode)

(use-package ido-find-tagged-file
  :bind ("C-x p" . ido-find-tagged-file))

(use-package json-format
  :commands json-format)

(use-package misc-cmds
  :bind (("C-a" . beginning-or-indentation)
         ("C-e" . end-of-line+)))

(use-package open-next-line
  :bind (("C-o" . open-next-line)
         ("M-o" . open-previous-line)))

(use-package plist)

(use-package url-insert-contents-at-point
  :commands url-insert-contents-at-point)

;;; Configuration for packages included by Prelude.

(use-package flyspell
  :defer t
  :diminish flyspell-mode)

;;; guru-mode
;; Turn off guru mode. I'll be a guru when I want to be.
(setq prelude-guru nil)

(use-package prelude-mode
  :defer t
  :diminish prelude-mode)

(use-package projectile
  :defer t
  :diminish projectile-mode)

(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode)

(use-package volatile-highlights
  :defer t
  :diminish volatile-highlights-mode)

;;; whitespace
;; Enable whitespace-mode, since it's disabled by default. The default
;; visualizations for whitespace mode are now pretty sane, but we want
;; to tweak them a bit.
(setq prelude-whitespace t)
;; The following code setting variables should be able to go in a
;; :config or :init block. But since whitespace copies from variables
;; when it is initialized, they have to be set BEFORE it is loaded.

;; Annoyingly, Prelude sets the whitespace column limit to 80. Set
;; back to nil to just inherit from `fill-column'. We are not really
;; using this, but just do it anyway.
(setq whitespace-line-column nil)
;; fill-column-indicator can be used to indicate long lines, so
;; lines-tail is not needed.
(setq whitespace-style '(face tabs empty trailing))
(use-package whitespace
  :defer t
  :diminish whitespace-mode)

(provide 'personal-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-packages.el ends here
