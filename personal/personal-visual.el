;;; personal-visual.el --- Visual configuration
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
;; Configure visual appearance of the editor.
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

;; Turn on CUA selection mode for enhanced rectangle support.
;; <C-RET> to start a rectangular selection.
(cua-selection-mode +1)

;; There are many ways to do line wrap. Personally, I like the
;; default, which adds a bent arrow to the fringe (or backslash to the
;; end in terminal mode). However, I also like word wrapping. Enable
;; it now.
(setq-default word-wrap t)

;; based on <http://emacswiki.org/emacs/SetFonts#toc9>
(require 'dash)
(defun font-candidate (&rest fonts)
  "Return the first available font from FONTS."
  (--first (find-font (font-spec :name it)) fonts))

(if (display-graphic-p)
    (progn
      ;; Choose fonts
      ;; On my Mac, the font size seems small, so make it bigger.
      (if (eq system-type 'darwin)
          (set-face-attribute 'default nil :height 240))
      (set-face-attribute 'default nil :family
                          (font-candidate "Inconsolata" "Consolas" "Monospace"))
      ;; Set theme.
      ;; (load-theme 'solarized-dark t)
      )
  (disable-theme 'zenburn))

;; Easy-on-the-eyes flymake
(require 'flymake)

(set-face-attribute 'flymake-errline nil :underline "red")
(set-face-attribute 'flymake-warnline nil :underline "yellow")

;; Cursor
(blink-cursor-mode -1)
(setq-default x-stretch-cursor t)
(setq-default cursor-type 'box)

;;; Themes
;; For some reason Prelude provides a themes directory, but apparently
;; doesn't add it to the themes path. Let's do it ourselves.
(defvar prelude-themes-dir (expand-file-name  "themes" prelude-dir)
  "This directory houses all of the user themes.")
(add-to-list 'custom-theme-load-path prelude-themes-dir)

;; Maximize new frames when they are created. We previously used
;; maxframe for this: https://github.com/rmm5t/maxframe.el
(defun personal-maximize-frame (frame)
  "Maximize FRAME within the screen."
  (interactive)
  ;; Adapted from `toggle-frame-maximized'.
  (modify-frame-parameters frame '((fullscreen . maximized))))

;; Maximize initial frame.
(personal-maximize-frame nil)
;; Maximize all new frames as well.
(add-hook 'after-make-frame-functions 'personal-maximize-frame t)

(provide 'personal-visual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-visual.el ends here
