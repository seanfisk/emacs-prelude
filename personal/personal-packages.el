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
;;
;; The following code doesn't work. If this were used, it would need
;; to be executed in the init.el file _before_ prelude-packages is
;; required. That's because prelude adds melpa and installs a bunch of
;; packages before this file gets executed.
;;
;; (require 'melpa)
;; (setq package-archive-enable-alist
;;       '(("melpa"
;;          ;; Prelude packages
;;          helm
;;          helm-projectile
;;          jump-char                      ; Marmalade version is old
;;          melpa
;;          multiple-cursors           ; experimental, so stay up to date
;;          rainbow-mode
;;          yaml-mode                ; the Marmalade version is quite old
;;          yasnippet                ; the Marmalade version is quite old
;;          ;; My MELPA packages
;;          auto-complete-clang
;;          edit-server
;;          ;; elpy ;; MELPA elpy (submitted by me) is now broken by some recent changes
;;          ;; smart-tabs-mode
;;          )))

;; use-package <https://github.com/jwiegley/use-package> is a great
;; way to manage the config. See the Github README or the commentary
;; in the file for more documentation. Undocumented but in the code is
;; the use of the `:ensure' plist key to automatically install through
;; ELPA. Also, take note of the `bind-key' utility.
(require 'use-package)

;; Start by installing diminish for use with use-package.
(use-package diminish
  :ensure t)

;;; This package is used by projectile. However, projectile uses the
;;; variable `ack-and-a-half-arguments', which results in error if the
;;; package loading is deferred by autoloads. Therefore, we just need
;;; to load it now.
(use-package ack-and-a-half
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
  :defer t
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

(use-package cmake-mode
  :ensure t)

;; Automatic installation always fails, and I barely use it, so remove
;; it for now.
;; (use-package dtrt-indent
;;   :ensure t
;;   :defer t)

;; Haven't taken the time to learn this yet.
;;(use-package ecb)

(use-package editorconfig
  :ensure t)

(use-package edit-server
  :ensure t
  :init (edit-server-start)
  ;; Finish the edit-server buffer when we press the key for `kill-this-buffer'.
  :config (define-key edit-server-edit-mode-map [remap kill-this-buffer] 'edit-server-done))

(use-package ein
  :ensure t)

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
  :init (elpy-enable))

;;; fill-column-indicator
(use-package fill-column-indicator
  :ensure t)

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

(use-package graphviz-dot-mode
  :ensure t)

(use-package header2
  :ensure t
  :defer t)

(use-package highlight-indentation
  :defer t
  :diminish highlight-indentation-mode)

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
  :defer t
  :init (progn
          ;; If we have ido-ubiquitous, there's no need for icomplete.
          (icomplete-mode -1)
          (ido-ubiquitous-mode +1)))

(use-package jump-char
  :ensure t
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

(use-package mo-git-blame
  :ensure t
  :defer t)

;;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this-dwim)))

;; No ELPA package for this yet.
;;(use-package nxhtml)

(use-package nyan-mode
  :ensure t
  :init (progn
            ;; This is short enough that it will still look OK with
            ;; two vertical windows on my Macbook Pro.
            (setq nyan-bar-length 15)
            (defun nyanimate ()
              "Toggle nyan-cat's wavy animation."
              (interactive)
              (if nyan-animate-nyancat
                  (nyan-stop-animation)
                (nyan-start-animation))
              (setq nyan-wavy-trail nyan-animate-nyancat))
            ))

(use-package org
  :ensure t
  :defer t)

(use-package paredit
  :ensure t
  :defer t
  :diminish paredit-mode)

(use-package powerline
  :ensure t
  :init (progn
          (defun powerline-sean-theme ()
            "Set up mode line with nyan-mode."
            ;; This is mostly a copy of `powerline-default-theme' with
            ;; nyan-mode added.
            (interactive)
            (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list
                                ;; output read-only (%), modified (*), unmodified (-)
                                (powerline-raw "%*" nil 'l)
                                (powerline-buffer-size nil 'l)
                                (powerline-raw mode-line-mule-info nil 'l)
                                (powerline-buffer-id nil 'l)
                                (when (and (boundp 'which-func-mode) which-func-mode)
                                  (powerline-raw which-func-format nil 'l))
                                (powerline-raw " ")

                                (funcall separator-left mode-line face1)

                                (when (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object face1 'l))
                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                (powerline-minor-modes face1 'l)
                                (powerline-narrow face1 'l)
                                (powerline-raw " " face1)

                                (funcall separator-left face1 face2)

                                (powerline-vc face2 'r)))
                          (rhs (list
                                (powerline-raw global-mode-string face2 'r)

                                (funcall separator-right face2 face1)

                                ;; line number
                                (powerline-raw "%4l" face1 'l)
                                (powerline-raw ":" face1 'l)
                                ;; column number
                                (powerline-raw "%3c" face1 'r)

                                (funcall separator-right face1 mode-line)

                                ;; Look here for hints:
                                ;; <http://nyan-mode.buildsomethingamazing.com/#sec-4>
                                ;; <https://github.com/bodil/emacs.d/blob/master/bodil-theme.el#L188>
                                (powerline-raw " ")
                                (powerline-raw (nyan-create))
                                ;; (powerline-raw "%6p" nil 'r)
                                ;; (powerline-hud face2 face1)
                                )))
                     (concat
                      (powerline-render lhs)

                      (powerline-fill face2 (powerline-width rhs))

                      (powerline-render rhs)))))))
          ;; Run this after everything initializes or else it will
          ;; look wonky.
          (add-hook 'after-init-hook 'powerline-sean-theme t)
	  ))

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

(use-package unfill
  :defer t
  :ensure t)

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode
  :init (whole-line-or-region-mode +1))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode +1))

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

(use-package tty-format) ; Dependency of e-sink
(use-package e-sink
  :commands e-sink-start)

(use-package ido-find-tagged-file
  :bind ("C-x p" . ido-find-tagged-file))

(use-package json-format
  :commands json-format)

(use-package misc-cmds
  :bind (("C-e" . end-of-line+)
         ;; This doesn't quite do what I want.
         ;;("C-a" . beginning-or-indentation)
         ))

(use-package plist)

;; (use-package real-auto-save
;;   :init (add-hook 'prog-mode-hook 'turn-on-real-auto-save))

(use-package toggle-plural
  :bind ("C-c C-s" . toggle-plural-at-point))

(use-package unfill-region-or-paragraph
  :bind ("M-Q" . unfill-region-or-paragraph))

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
(setq whitespace-style '(face empty trailing))
(use-package whitespace
  :defer t
  :diminish whitespace-mode)

;; For loading gettext's po-mode on Mac OS X, installed with Homebrew
(add-to-list 'load-path "/usr/local/opt/gettext/share/emacs/site-lisp")
(load "start-po" t) ;; the t prevents this command from erroring

;; Add George Brandl's improvements to po-mode, which include an autowrap.
;; (eval-after-load 'po-mode '(load "gb-po-mode"))

(provide 'personal-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-packages.el ends here
