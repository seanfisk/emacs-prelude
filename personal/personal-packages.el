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
(setq package-archive-enable-alist
      '(("melpa"
         ;; Prelude packages
         helm
         helm-projectile
         melpa
         multiple-cursors           ; experimental, so stay up to date
         rainbow-mode
         yaml-mode                 ; the Marmalade version is quite old
         yasnippet                 ; the Marmalade version is quite old
         ;; My MELPA packages
         auto-complete-clang
         edit-server
         elpy
         ;; smart-tabs-mode
         )))

;; Add my own packages.
(require 'prelude-packages)
(setq prelude-packages
      (append '(
                ;; ecb
                ;; fillcode
                ;; flymake-ruby
                ;; nxhtml
                ;; smart-tabs-mode         ; MELPA
                auto-complete
                auto-complete-clang     ; MELPA
                buffer-move
                dtrt-indent
                edit-server             ; MELPA
                elpy
                fill-column-indicator
                flymake-cursor
                flymake-shell
                flyspell-lazy
                goto-last-change
                header2
                highlight-symbol
                ido-ubiquitous
                jump-char
                mo-git-blame
                multiple-cursors        ; MELPA
                smex
                smooth-scrolling
                switch-window
                undo-tree
                whole-line-or-region
                zencoding-mode
                ) prelude-packages))

;; Install all of the packages
(prelude-install-packages)

;;; auto-complete
;; No autoloads.
(require 'auto-complete)
(defun personal-auto-complete-mode-setup ()
  (local-set-key (kbd "M-/") 'auto-complete))
(add-hook 'auto-complete-mode-hook 'personal-auto-complete-mode-setup)
(global-auto-complete-mode +1)

;;; auto-complete-clang
(defun personal-ac-cc-mode-setup ()
  (require 'auto-complete-clang)
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'personal-ac-cc-mode-setup)

;;; buffer-move
(autoload 'buf-move-up "buffer-move" nil t)
(autoload 'buf-move-down "buffer-move" nil t)
(autoload 'buf-move-left "buffer-move" nil t)
(autoload 'buf-move-right "buffer-move" nil t)
(define-key global-map (kbd "<C-S-up>") 'buf-move-up)
(define-key global-map (kbd "<C-S-down>") 'buf-move-down)
(define-key global-map (kbd "<C-S-left>") 'buf-move-left)
(define-key global-map (kbd "<C-S-right>") 'buf-move-right)

;;; elpy
;; For elpy to work correctly, the following packages need to be
;; installed to the _system_ Python. They can be installed in the user
;; site directory, though.
;;
;;     pip install --user git+https://github.com/pinard/Pymacs.git \
;;         rope ropemode ropemacs flake8
;;
(setq python-check-command "flake8")
(elpy-enable)

;;; fill-column-indicator
;; Specifically *don't* set `fci-fill-column' (the column at which the
;; line is shown). `fci-mode' will then default to using the value of
;; `fill-column'.
(add-hook 'prog-mode-hook 'turn-on-fci-mode)

;;; flymake-cursor
(eval-after-load 'flymake '(require 'flymake-cursor))

;;; flymake-shell
(add-hook 'sh-set-shell-hook 'flymake-shell-load)

;;; flyspell
;; Out of the box, it slows down editing. That's the last thing I
;; need. flyspell-lazy runs flyspell only when idle, preventing lag.
(flyspell-lazy-mode +1)

;;; goto-last-change
;; when using AZERTY keyboard, consider C-x C-_
(define-key global-map (kbd "C-x C-/") 'goto-last-change)

;;; jump-char
(define-key global-map (kbd "M-j") 'jump-char-forward)
(define-key global-map (kbd "M-S-j") 'jump-char-backward)

;;; highlight-symbol
(defun personal-highlight-symbol-setup ()
  (highlight-symbol-mode +1)
  (local-set-key (kbd "M-n") 'highlight-symbol-next)
  (local-set-key (kbd "M-p") 'highlight-symbol-prev))
(add-hook 'prog-mode-hook 'personal-highlight-symbol-setup)

;;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)

;;; smart-tabs-mode
;; Use the convenience function to load these automatically.
;; (smart-tabs-insinuate 'c)

;;; smex
(smex-initialize)
(define-key global-map (kbd "C-x C-m") 'smex)
(define-key global-map (kbd "M-x") 'smex)
(define-key global-map (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(define-key global-map (kbd "C-c C-c M-x") 'execute-extended-command)

;;; smooth-scrolling
;; The difference between `smooth-scroll' and `smooth-scrolling' is
;; this: `smooth-scroll' changes "Page Up" and "Page Down" to show all
;; the area scrolled when using those operations. I don't like
;; this. `smooth-scrolling' prevents the cursor from hitting the
;; bottom or top of the screen when using next and previous line. This
;; is what I want, because it allows viewing the context of the line
;; with the cursor.

;;; switch-window
;; This package has no autoloads, so it doesn't "do" anything when
;; it's just installed through ELPA. It needs to be required, at which
;; point it takes over the `C-x o' binding.
(require 'switch-window)

;;; whitespace-mode
;; Enable whitespace-mode, since it's disabled by default. The default
;; visualizations for whitespace mode are now pretty sane, so no need
;; to change them.
(setq prelude-whitespace t)

;;; undo-tree
(global-undo-tree-mode +1)

;;; whole-line-or-region
;; Hopefully this turns it on globally.
(whole-line-or-region-mode +1)

;;; dired-x
;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; Require vendorized code.
(require 'plist)
(require 'json-format)
(require 'cython-mode)
(require 'open-next-line)
(require 'url-insert-contents-at-point)
(require 'comment-or-uncomment-region-or-line)

;;; comment-or-uncomment-line-or-region
;; This should be set to `C-;' or `C-c C-c'. Right now flyspell is
;; overiding the first one. See
;; <http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs>
;; for a way to fix it.
;;
;; This should also be in prog-mode map instead of the global map.
(define-key global-map (kbd "C-c C-;") 'comment-or-uncomment-region-or-line)

(provide 'personal-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-packages.el ends here
