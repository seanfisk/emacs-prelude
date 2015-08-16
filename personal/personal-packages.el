;;; personal-packages.el --- Third-party package configurations
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: local
;; Compatibility: GNU Emacs: 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Configure packages installed with Cask.
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

;; use-package <https://github.com/jwiegley/use-package> is a great
;; way to manage the configuration of third-party packages. See the
;; Github README or the commentary in the file for more documentation.
(eval-when-compile
  (require 'use-package))
(require 'diminish)

;;; auto-complete
;; (use-package auto-complete
;;   :ensure t
;;   :diminish auto-complete-mode
;;   :init (global-auto-complete-mode +1)
;;   :config (progn
;;             (defun personal-auto-complete-mode-setup ()
;;               (local-set-key (kbd "M-/") 'auto-complete))
;;             (add-hook 'auto-complete-mode-hook
;;                       'personal-auto-complete-mode-setup)))

;;; auto-complete-clang
;; (use-package auto-complete-clang
;;   :ensure t
;;   :defer t
;;   :config (progn
;;             (defun personal-ac-cc-mode-setup ()
;;               (setq ac-sources
;;                     (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
;;             (add-hook 'c-mode-common-hook 'personal-ac-cc-mode-setup)))

;;; buffer-move
(use-package buffer-move
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right)
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

;;; elpy
;; For elpy to work correctly, the following packages need to be
;; installed. They should be installed in the user site directory for
;; the system Python. If using a virtualenv, install to the virtualenv
;; and have elpy restart in that virtualenv. Don't include --user for
;; a virtualenv.
;;
;;     pip install [--user] elpy rope jedi pyflakes pep8
;;
;; (use-package elpy
;;   :ensure t
;;   :init (elpy-enable))

;; Out of the box, flyspell slows down editing. That's the last thing
;; I need. flyspell-lazy runs flyspell only when idle, preventing lag.
(use-package flyspell-lazy
  :config (flyspell-lazy-mode +1))

;;; goto-last-change
(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))

(use-package highlight-indentation
  :config (highlight-indentation-mode +1)
  :diminish highlight-indentation-mode)

;;; highlight-symbol
;; I don't really want to enable highlight symbol mode globally, but
;; it appears the easiest solution at the moment.
(use-package highlight-symbol
  :bind (("C-c C-n" . highlight-symbol-next)
         ("C-c C-p" . highlight-symbol-prev))
  :config (highlight-symbol-mode +1))

(use-package jump-char
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

;;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-c m" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this-dwim)))

;; (use-package nyan-mode
;;   :config (progn
;;             ;; This is short enough that it will still look OK with
;;             ;; two vertical windows on my Macbook Pro.
;;             (setq nyan-bar-length 15)
;;             (defun nyanimate ()
;;               "Toggle nyan-cat's wavy animation."
;;               (interactive)
;;               (if nyan-animate-nyancat
;;                   (nyan-stop-animation)
;;                 (nyan-start-animation))
;;               (setq nyan-wavy-trail nyan-animate-nyancat))
;;             ))

(use-package paredit
  :diminish paredit-mode)

;; (use-package powerline
;;   :config (progn
;;           (defun powerline-sean-theme ()
;;             "Set up mode line with nyan-mode."
;;             ;; This is mostly a copy of `powerline-default-theme' with
;;             ;; nyan-mode added.
;;             (interactive)
;;             (setq-default mode-line-format
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (mode-line (if active 'mode-line 'mode-line-inactive))
;;                           (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                           (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                           (separator-left (intern (format "powerline-%s-%s"
;;                                                           powerline-default-separator
;;                                                           (car powerline-default-separator-dir))))
;;                           (separator-right (intern (format "powerline-%s-%s"
;;                                                            powerline-default-separator
;;                                                            (cdr powerline-default-separator-dir))))
;;                           (lhs (list
;;                                 ;; output read-only (%), modified (*), unmodified (-)
;;                                 (powerline-raw "%*" nil 'l)
;;                                 (powerline-buffer-size nil 'l)
;;                                 (powerline-raw mode-line-mule-info nil 'l)
;;                                 (powerline-buffer-id nil 'l)
;;                                 (when (and (boundp 'which-func-mode) which-func-mode)
;;                                   (powerline-raw which-func-format nil 'l))
;;                                 (powerline-raw " ")

;;                                 (funcall separator-left mode-line face1)

;;                                 (when (boundp 'erc-modified-channels-object)
;;                                   (powerline-raw erc-modified-channels-object face1 'l))
;;                                 (powerline-major-mode face1 'l)
;;                                 (powerline-process face1)
;;                                 (powerline-minor-modes face1 'l)
;;                                 (powerline-narrow face1 'l)
;;                                 (powerline-raw " " face1)

;;                                 (funcall separator-left face1 face2)

;;                                 (powerline-vc face2 'r)))
;;                           (rhs (list
;;                                 (powerline-raw global-mode-string face2 'r)

;;                                 (funcall separator-right face2 face1)

;;                                 ;; line number
;;                                 (powerline-raw "%4l" face1 'l)
;;                                 (powerline-raw ":" face1 'l)
;;                                 ;; column number
;;                                 (powerline-raw "%3c" face1 'r)

;;                                 (funcall separator-right face1 mode-line)

;;                                 ;; Look here for hints:
;;                                 ;; <http://nyan-mode.buildsomethingamazing.com/#sec-4>
;;                                 ;; <https://github.com/bodil/emacs.d/blob/master/bodil-theme.el#L188>
;;                                 (powerline-raw " ")
;;                                 (powerline-raw (nyan-create))
;;                                 ;; (powerline-raw "%6p" nil 'r)
;;                                 ;; (powerline-hud face2 face1)
;;                                 )))
;;                      (concat
;;                       (powerline-render lhs)

;;                       (powerline-fill face2 (powerline-width rhs))

;;                       (powerline-render rhs)))))))
;;           ;; Run this after everything initializes or else it will
;;           ;; look wonky.
;;           (add-hook 'after-init-hook 'powerline-sean-theme t)
;; 	  ))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode +1))

(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config (whole-line-or-region-mode +1))

(use-package yasnippet
  :config (yas-global-mode +1))



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

(use-package misc-cmds
  :bind ("C-e" . end-of-line+))

(use-package plist)

(use-package toggle-plural
  :bind ("C-c C-s" . toggle-plural-at-point))

(use-package unfill-region-or-paragraph
  :bind ("M-Q" . unfill-region-or-paragraph))

(use-package url-insert-contents-at-point
  :commands url-insert-contents-at-point)



;;; Configuration for packages included by Prelude.

(use-package flyspell
  :diminish flyspell-mode)

;; Turn off guru mode. I'll be a guru when I want to be.
(setq prelude-guru nil)

(use-package prelude-mode
  :diminish prelude-mode)

(use-package projectile
  :diminish projectile-mode)

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package volatile-highlights
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
  :diminish whitespace-mode)

;; Add George Brandl's improvements to po-mode, which include an autowrap.
;; (eval-after-load 'po-mode '(load "gb-po-mode"))

(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'personal-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-packages.el ends here
