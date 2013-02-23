;;; personal-visual.el --- Visual customizations
;;
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: local
;; Compatibility: GNU Emacs: 24.x, Aquamacs: 3.x
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

;; based on <http://emacswiki.org/emacs/SetFonts#toc9>
(require 'dash)
(defun font-candidate (&rest fonts)
  "Return the first available font."
  (--first (find-font (font-spec :name it)) fonts))

;; choose fonts
(if (display-graphic-p)
    (progn
      ;; Font size
      ;; On my Mac, the font size seems small, so make it bigger.
      (set-face-attribute 'default nil :height
                          (if (eq system-type 'darwin) 240 140))
      (set-face-attribute 'default nil :family
                          (font-candidate "Inconsolata" "Monospace")))
  (disable-theme 'zenburn)
  (load-theme 'solarized-dark t))

;; easy-on-the-eyes flymake
(require 'flymake)

(set-face-attribute 'flymake-errline nil :underline "red")
(set-face-attribute 'flymake-warnline nil :underline "yellow")

;; cursor
(blink-cursor-mode -1)
(setq-default x-stretch-cursor t)
(setq-default cursor-type 'box)

;;; Themes
;; For some reason Prelude provides a themes directory, but apparently
;; doesn't add it to the themes path. Let's do it ourselves.
(defvar prelude-themes-dir (expand-file-name  "themes" prelude-dir)
  "This directory houses all of the user themes.")
(add-to-list 'custom-theme-load-path prelude-themes-dir)

(provide 'personal-visual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-visual.el ends here
