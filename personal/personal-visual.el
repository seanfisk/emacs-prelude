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
(require 'cl)
(defun font-candidate (&rest fonts)
  "Return the first available font."
  (find-if (lambda (font-name) (find-font (font-spec :name font-name))) fonts))

;; choose fonts
(when (display-graphic-p)
  ;; font size
  (if (featurep 'aquamacs)
      ;; on my Mac, the font size seems small, so make it bigger
      (set-face-attribute 'default nil :height 180)
    (set-face-attribute 'default nil :height 140))
  (set-face-attribute 'default nil :family
                      (font-candidate "Inconsolata" "Monospace")))

;; easy-on-the-eyes flymake
(require 'flymake)
(set-face-attribute 'flymake-errline nil :underline "red")
(set-face-attribute 'flymake-warnline nil :underline "yellow")

;; cursor
(blink-cursor-mode -1)
(setq-default x-stretch-cursor t)
(setq-default cursor-type 'box)

(provide 'personal-visual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-visual.el ends here
