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

;; choose fonts
(if (window-system)
    ;; font size
    (set-face-attribute 'default nil :height 180)
    (condition-case nil
        ;; use Inconsolata if we have it
        (set-face-attribute 'default nil :family "Inconsolata")
      (error
       ;; otherwise default to Monospace
       (set-face-attribute 'default nil :family "Monospace"))))

;; easy-on-the-eyes flymake
(require 'flymake)
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(provide 'personal-visual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal-visual.el ends here
